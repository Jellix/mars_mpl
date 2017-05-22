--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fixed                     Luebeck            --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
-- __________________________________________________________________ --

with Ada.Strings.Fixed;
with Ada.Text_IO;

with Glib.Properties;

with Gtk.Style_Context;

with Pango.Enums;

package body Gtk.Cell_Renderer_Fixed is

   package GDouble_IO is new Ada.Text_IO.Float_IO (GDouble);

   Renderer_Type : GType := GType_Invalid;
   Value_ID      : constant Glib.Properties.Creation.Property_Id := 1;
   After_ID      : constant Glib.Properties.Creation.Property_Id := 2;
   Empty_ID      : constant Glib.Properties.Creation.Property_Id := 3;

   procedure Class_Init (Class : GObject_Class);
   pragma Convention (C, Class_Init);

   procedure Class_Init (Class : GObject_Class) is
   begin
      Gtk.Cell_Renderer.Abstract_Renderer.Base_Class_Init (Class);
      Gtk.Missed.Class_Install_Property
        (Class,
         Value_ID,
         Glib.Properties.Creation.Gnew_Double
           (Name    => "value",
            Nick    => "value",
            Blurb   => "fixed point number",
            Minimum => Gdouble'First,
            Maximum => Gdouble'Last,
            Default => 0.0));
      Gtk.Missed.Class_Install_Property
        (Class,
         After_ID,
         Glib.Properties.Creation.Gnew_Uint
           (Name    => "after",
            Nick    => "aft",
            Blurb   => "digits after decimal point",
            Minimum => 0,
            Maximum => Gdouble'Digits,
            Default => 0));
      Gtk.Missed.Class_Install_Property
        (Class,
         Empty_ID,
         Glib.Properties.Creation.Gnew_Boolean
           (Name    => "empty",
            Nick    => "empty",
            Blurb   => "leave the cell empty if set true",
            Default => False));
   end Class_Init;

   procedure Editing_Done
     (Editor : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Cell   : Gtk_Cell_Renderer_Fixed) is
   begin
      if Cell.all.Focus_Out.Id /= Gtk.Handlers.Null_Handler_Id then
         Gtk.Handlers.Disconnect (Editor, Cell.all.Focus_Out);
         Cell.all.Focus_Out.Id := Gtk.Handlers.Null_Handler_Id;
      end if;
      Cell.all.Value :=
        Gdouble'Value
          (Ada.Strings.Fixed.Trim
             (Gtk.GEntry.Get_Text (Editor), Ada.Strings.Both));
      Stop_Editing (Cell, False);
      Commit (Cell);
   exception
      when others =>
         Stop_Editing (Cell, True);
   end Editing_Done;

   overriding procedure Finalize
     (Cell : not null access Gtk_Cell_Renderer_Fixed_Record)
   is
      use type Pango.Layout.Pango_Layout;
   begin
      if Cell.all.Text /= null then
         Pango.Layout.Unref (Cell.all.Text);
      end if;
      Gtk.Cell_Renderer.Abstract_Renderer.Finalize
        (Gtk.Cell_Renderer.Abstract_Renderer.Gtk_Abstract_Renderer_Record
           (Cell.all)'Access);
   end Finalize;

   function Focus_Out
     (Editor : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Cell   : Gtk_Cell_Renderer_Fixed) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Editing_Done (Editor, Cell);
      return False;
   end Focus_Out;

   overriding function Get_Aligned_Area
     (Cell      : not null access Gtk_Cell_Renderer_Fixed_Record;
      Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags     : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle)
      return Gdk.Rectangle.Gdk_Rectangle
   is
      pragma Unreferenced (Flags);
      Area   : constant Gdk.Rectangle.Gdk_Rectangle :=
               Cell.all.Get_Size (Widget, Cell_Area);
      Result : Gdk.Rectangle.Gdk_Rectangle;
   begin
      Result.X :=
        (Cell_Area.X
         +  Gint (Get_X_Pad (Cell))
         +  Area.X
         +  (Cell.all.Max_Offset - Cell.all.Left_Width));
      Result.Y :=
        (Cell_Area.Y
         +  Gint (Get_Y_Pad (Cell))
         +  Area.Y);
      Result.Width :=
         Gint'Min
          (Cell_Area.X - Result.X + Cell_Area.Width,
           Area.Width);
      Result.Height :=
         Gint'Min
          (Cell_Area.Y - Result.Y + Cell_Area.Height,
           Area.Height);
      return Result;
   end Get_Aligned_Area;

   overriding procedure Get_Property
     (Cell          : not null access Gtk_Cell_Renderer_Fixed_Record;
      Param_ID      : Glib.Properties.Creation.Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Property_Spec);
   begin
      case Param_ID is
         when Value_ID =>
            Glib.Values.Init (Value, GType_Double);
            Glib.Values.Set_Double (Value, Cell.all.Value);
         when After_ID =>
            Glib.Values.Init (Value, GType_Uint);
            Glib.Values.Set_Uint (Value, Guint (Cell.all.After));
         when Empty_ID =>
            Glib.Values.Init (Value, GType_Boolean);
            Glib.Values.Set_Boolean (Value, Cell.all.Empty);
         when others =>
            Glib.Values.Init (Value, GType_String);
            Glib.Values.Set_String (Value, "unknown");
      end case;
   end Get_Property;
--
-- Update -- The widget associated with the renderer
--
--    Cell   - The renderer
--    Widget - The widget it is used at
--
-- This  procedure  is  used  upon  each  call to either to render or to
-- evaluate  the geometry of a cell. The renderer has no data associated
-- with  any  concrete  cell of the tree view. It is called at random to
-- indicate all of them.
--
   procedure Update
     (Cell   : in out Gtk_Cell_Renderer_Fixed_Record'Class;
      Widget : in out Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Text      : String (1 .. 40);
      Start_Pos : Integer := Text'Last + 1;
      Point_Pos : Integer := Text'Last + 1;
      Right     : constant Gint := Cell.Width - Cell.Max_Offset;
      Line      : Gint;

      use type Pango.Layout.Pango_Layout;
   begin
      if Cell.Text = null then
         Cell.Text := Widget.Create_Pango_Layout;
      end if;
      if Cell.Empty then
         Text := (others => ' ');
      else
         GDouble_IO.Put (Text, Cell.Value, Cell.After, 0);
      end if;
      for Index in reverse Text'Range loop
         -- Find the beginning of the number in the output string
         if ' ' = Text (Index) then
            Start_Pos := Index + 1;
            exit;
         end if;
      end loop;
      for Index in Start_Pos .. Text'Last loop
         -- Find the position of the decimal point in the output
         if '.' = Text (Index) then
            Point_Pos := Index;
            exit;
         end if;
      end loop;
      Cell.Text.all.Set_Text (Text (Start_Pos .. Text'Last));
      Cell.Text.all.Get_Pixel_Size (Cell.Width, Cell.Height);
      if Point_Pos <= Text'Last then
         Cell.Text.all.Index_To_Line_X
           (Gint (Point_Pos - Start_Pos),
            False,
            Line,
            Cell.Left_Width);
         Cell.Left_Width := Pango.Enums.To_Pixels (Cell.Left_Width);
      else
         Cell.Left_Width := Cell.Width;
      end if;
      Cell.Max_Offset := Gint'Max (Cell.Left_Width, Cell.Max_Offset);
      Cell.Width :=
         (Cell.Max_Offset
          +  Gint'Max (Right, Cell.Width - Cell.Left_Width));
   end Update;

   overriding function Get_Size
     (Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
      Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gdk.Rectangle.Gdk_Rectangle is
   begin
      Update (Cell.all, Widget.all);
      return
        (X      => 0,
         Y      => 0,
         Width  => Cell.all.Width,
         Height => Cell.all.Height);
   end Get_Size;

   overriding function Get_Size
     (Cell      : not null access Gtk_Cell_Renderer_Fixed_Record;
      Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Cell_Area : Gdk.Rectangle.Gdk_Rectangle)
      return Gdk.Rectangle.Gdk_Rectangle is
   begin
      Update (Cell.all, Widget.all);
      return
        (X      =>
           Gint
             (Get_X_Align (Cell) * Gfloat (Cell_Area.Width  - Cell.all.Width)),
         Y      =>
           Gint
             (Get_Y_Align (Cell) * Gfloat (Cell_Area.Height - Cell.all.Height)),
         Width  => Cell.all.Width,
         Height => Cell.all.Height);
   end Get_Size;

   function Get_Type return Gtk_Type is
   begin
      if Renderer_Type = GType_Invalid then
         Renderer_Type :=
           Gtk.Cell_Renderer.Abstract_Renderer.Register
             ("GtkCellRendererFixed", Class_Init'Access);
      end if;
      return Renderer_Type;
   end Get_Type;

   procedure Gtk_New
     (Cell  : out Gtk_Cell_Renderer_Fixed;
      After : Natural := 0) is
   begin
      Cell := new Gtk_Cell_Renderer_Fixed_Record;
      Initialize (Cell, After);
   end Gtk_New;

   procedure Initialize
     (Cell  : not null access Gtk_Cell_Renderer_Fixed_Record'Class;
      After : Natural) is
   begin
      Gtk.Cell_Renderer.Abstract_Renderer.Initialize (Cell, Get_Type);
      Cell.all.After := After;
   end Initialize;

   overriding procedure Render
     (Cell            : not null access Gtk_Cell_Renderer_Fixed_Record;
      Context         : Cairo.Cairo_Context;
      Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State)
   is
      pragma Unreferenced (Background_Area);
      pragma Unreferenced (Flags);
      Area  : constant Gdk.Rectangle.Gdk_Rectangle :=
              Cell.all.Get_Size (Widget, Cell_Area);
      Style : constant Gtk.Style_Context.Gtk_Style_Context :=
                Gtk.Style_Context.Get_Style_Context (Widget);
   begin
      Cairo.Save (Context);
      Cairo.Rectangle
        (Context,
         Gdouble (Cell_Area.X),
         Gdouble (Cell_Area.Y),
         Gdouble (Cell_Area.Width),
         Gdouble (Cell_Area.Height));
      Cairo.Clip (Context);
      Gtk.Style_Context.Render_Layout
        (Style,
         Context,
         Gdouble
           (Cell_Area.X
            +  Gint (Get_X_Pad (Cell))
            +  Area.X
            +  (Cell.all.Max_Offset - Cell.all.Left_Width)),
         Gdouble
           (Cell_Area.Y
            +  Gint (Get_Y_Pad (Cell))
            +  Area.Y),
         Cell.all.Text);
      Cairo.Restore (Context);
   end Render;

   overriding procedure Set_Property
     (Cell          : not null access Gtk_Cell_Renderer_Fixed_Record;
      Param_ID      : Glib.Properties.Creation.Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Property_Spec);
   begin
      case Param_ID is
         when Value_ID =>
            if not Cell.all.Empty then
               Cell.all.Value := Glib.Values.Get_Double (Value);
            end if;
         when After_ID =>
            Cell.all.After := Integer (Glib.Values.Get_Uint (Value));
         when Empty_ID =>
            Cell.all.Empty := Glib.Values.Get_Boolean (Value);
         when others =>
            null;
      end case;
   end Set_Property;

   overriding function Start_Editing
     (Cell            : not null access Gtk_Cell_Renderer_Fixed_Record;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Path            : UTF8_String;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk.Cell_Renderer.Gtk_Cell_Renderer_State)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Background_Area);
      pragma Unreferenced (Cell_Area);
      pragma Unreferenced (Event);
      pragma Unreferenced (Flags);
      pragma Unreferenced (Path);
      pragma Unreferenced (Widget);
      Editor    : Gtk.GEntry.Gtk_Entry;
      Text      : String (1 .. 40);
      Start_Pos : Integer := Text'Last + 1;
   begin
      GDouble_IO.Put (Text, Cell.all.Value, Cell.all.After, 0);
      for Index in reverse Text'Range loop
         -- Find the beginning of the number in the output string
         if ' ' = Text (Index) then
            Start_Pos := Index + 1;
            exit;
         end if;
      end loop;
      Gtk.GEntry.Gtk_New (Editor);
      Glib.Properties.Set_Property (Editor,
                                    Glib.Properties.Build ("xalign"),
                                    Get_X_Align (Cell));
      Glib.Properties.Set_Property (Editor,
                                    Glib.Properties.Build ("has-frame"),
                                    False);
      Editor.all.Set_Text (Text (Start_Pos .. Text'Last));
      Editor.all.Select_Region (0, -1);
      Entry_Callbacks.Connect
        (Editor,
         "editing_done",
         Entry_Callbacks.To_Marshaller (Editing_Done'Access),
         Cell.all'Access);
      Cell.all.Focus_Out :=
         Entry_Return_Callbacks.Connect
          (Editor,
           "focus_out_event",
           Entry_Return_Callbacks.To_Marshaller (Focus_Out'Access),
           Cell.all'Access);
      Editor.all.Show;
      return Editor.all'Access;
   end Start_Editing;

end Gtk.Cell_Renderer_Fixed;
