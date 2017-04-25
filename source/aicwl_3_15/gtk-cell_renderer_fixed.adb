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
--____________________________________________________________________--

with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with GLib.Properties;    use GLib.Properties;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Style;          use Gtk.Style;
with Gtk.Style_Context;  use Gtk.Style_Context;
with Pango.Cairo;        use Pango.Cairo;
with Pango.Enums;        use Pango.Enums;
with Pango.Font;         use Pango.Font;

with Ada.Text_IO;

package body Gtk.Cell_Renderer_Fixed is
   use Gtk.Missed;
   use Gtk.Widget;

   package GDouble_IO is new Ada.Text_IO.Float_IO (GDouble);
   use GDouble_IO;

   Renderer_Type : GType := GType_Invalid;
   Value_ID      : constant Property_ID := 1;
   After_ID      : constant Property_ID := 2;
   Empty_ID      : constant Property_ID := 3;

   procedure Class_Init (Class : GObject_Class);
   pragma Convention (C, Class_Init);

   procedure Class_Init (Class : GObject_Class) is
      use Gtk.Cell_Renderer.Abstract_Renderer;
   begin
      Base_Class_Init (Class);
      Class_Install_Property
      (  Class,
         Value_ID,
         Gnew_Double
         (  Name    => "value",
            Nick    => "value",
            Blurb   => "fixed point number",
            Minimum => GDouble'First,
            Maximum => GDouble'Last,
            Default => 0.0
      )  );
      Class_Install_Property
      (  Class,
         After_ID,
         Gnew_UInt
         (  Name    => "after",
            Nick    => "aft",
            Blurb   => "digits after decimal point",
            Minimum => 0,
            Maximum => GDouble'Digits,
            Default => 0
      )  );
      Class_Install_Property
      (  Class,
         Empty_ID,
         Gnew_Boolean
         (  Name    => "empty",
            Nick    => "empty",
            Blurb   => "leave the cell empty if set true",
            Default => False
      )  );
   end Class_Init;

   procedure Editing_Done
             (  Editor : access Gtk_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Fixed
             )  is
   begin
      if Cell.Focus_Out.Id /= Null_Handler_Id then
         Disconnect (Editor, Cell.Focus_Out);
         Cell.Focus_Out.Id := Null_Handler_Id;
      end if;
      Cell.Value :=
         GDouble'Value (Trim (Get_Text (Editor), Ada.Strings.Both));
      Stop_Editing (Cell, False);
      Commit (Cell);
   exception
      when others =>
         Stop_Editing (Cell, True);
   end Editing_Done;

   procedure Finalize
             (  Cell : not null access Gtk_Cell_Renderer_Fixed_Record
             )  is
      use Gtk.Cell_Renderer.Abstract_Renderer;
   begin
      if Cell.Text /= null then
         Unref (Cell.Text);
      end if;
      Finalize (Gtk_Abstract_Renderer_Record (Cell.all)'Access);
   end Finalize;

   function Focus_Out
            (  Editor : access Gtk_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Fixed
            )  return Boolean is
   begin
      Editing_Done (Editor, Cell);
      return False;
   end Focus_Out;

   function Get_Aligned_Area
            (  Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Flags  : Gtk_Cell_Renderer_State;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle is
      Area   : constant Gdk_Rectangle :=
               Cell.Get_Size (Widget, Cell_Area);
      Result : Gdk_Rectangle;
   begin
      Result.X :=
         (  Cell_Area.X
         +  GInt (Get_X_Pad (Cell))
         +  Area.X
         +  (Cell.Max_Offset - Cell.Left_Width)
         );
      Result.Y :=
         (  Cell_Area.Y
         +  GInt (Get_Y_Pad (Cell))
         +  Area.Y
         );
      Result.Width :=
         GInt'Min
         (  Cell_Area.X - Result.X + Cell_Area.Width,
            Area.Width
         );
      Result.Height :=
         GInt'Min
         (  Cell_Area.Y - Result.Y + Cell_Area.Height,
            Area.Height
         );
      return Result;
   end Get_Aligned_Area;

   procedure Get_Property
             (  Cell : not null access Gtk_Cell_Renderer_Fixed_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Value_ID =>
            Init (Value, GType_Double);
            Set_Double (Value, Cell.Value);
         when After_ID =>
            Init (Value, GType_UInt);
            Set_UInt (Value, GUInt (Cell.After));
         when Empty_ID =>
            Init (Value, GType_Boolean);
            Set_Boolean (Value, Cell.Empty);
         when others =>
            Init (Value, GType_String);
            Set_String (Value, "unknown");
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
             (  Cell   : in out Gtk_Cell_Renderer_Fixed_Record'Class;
                Widget : in out Gtk.Widget.Gtk_Widget_Record'Class
             )  is
      Text      : String (1..40);
      Start_Pos : Integer := Text'Last + 1;
      Point_Pos : Integer := Text'Last + 1;
      Right     : constant GInt := Cell.Width - Cell.Max_Offset;
      Line      : GInt;
   begin
      if Cell.Text = null then
         Cell.Text := Widget.Create_Pango_Layout;
      end if;
      if Cell.Empty then
         Text := (others => ' ');
      else
         Put (Text, Cell.Value, Cell.After, 0);
      end if;
      for Index in reverse Text'Range loop
         -- Find the beginning of the number in the output string
         if ' ' = Text (Index) then
            Start_Pos := Index + 1;
            exit;
         end if;
      end loop;
      for Index in Start_Pos..Text'Last loop
         -- Find the position of the decimal point in the output
         if '.' = Text (Index) then
            Point_Pos := Index;
            exit;
         end if;
      end loop;
      Cell.Text.Set_Text (Text (Start_Pos..Text'Last));
      Cell.Text.Get_Pixel_Size (Cell.Width, Cell.Height);
      if Point_Pos <= Text'Last then
         Cell.Text.Index_To_Line_X
         (  GInt (Point_Pos - Start_Pos),
            False,
            Line,
            Cell.Left_Width
         );
         Cell.Left_Width := To_Pixels (Cell.Left_Width);
      else
         Cell.Left_Width := Cell.Width;
      end if;
      Cell.Max_Offset := GInt'Max (Cell.Left_Width, Cell.Max_Offset);
      Cell.Width :=
         (  Cell.Max_Offset
         +  GInt'Max (Right, Cell.Width - Cell.Left_Width)
         );
   end Update;

   function Get_Size
            (  Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Gdk_Rectangle is
   begin
      Update (Cell.all, Widget.all);
      return
      (  X      => 0,
         Y      => 0,
         Width  => Cell.Width,
         Height => Cell.Height
      );
   end Get_Size;

   function Get_Size
            (  Cell      : not null access Gtk_Cell_Renderer_Fixed_Record;
               Widget    : not null access Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle is
   begin
      Update (Cell.all, Widget.all);
      return
      (  X      => GInt
                   (  Get_X_Align (Cell)
                   *  GFloat (Cell_Area.Width  - Cell.Width)
                   ),
         Y      => GInt
                   (  Get_Y_Align (Cell)
                   *  GFloat (Cell_Area.Height - Cell.Height)
                   ),
         Width  => Cell.Width,
         Height => Cell.Height
      );
   end Get_Size;

   function Get_Type return Gtk_Type is
      use Gtk.Cell_Renderer.Abstract_Renderer;
   begin
     if Renderer_Type = GType_Invalid then
        Renderer_Type :=
           Register ("GtkCellRendererFixed", Class_Init'Access);
     end if;
     return Renderer_Type;
   end Get_Type;

   procedure Gtk_New
             (  Cell  : out Gtk_Cell_Renderer_Fixed;
                After : Natural := 0
             )  is
   begin
      Cell := new Gtk_Cell_Renderer_Fixed_Record;
      Initialize (Cell, After);
   end Gtk_New;

   procedure Initialize
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fixed_Record'Class;
                After : Natural
             )  is
      use Gtk.Cell_Renderer.Abstract_Renderer;
   begin
      Initialize (Cell, Get_Type);
      Cell.After := After;
   end Initialize;

   procedure Render
             (  Cell    : not null access Gtk_Cell_Renderer_Fixed_Record;
                Context : Cairo_Context;
                Widget  : not null access Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             )  is
      Area  : constant Gdk_Rectangle :=
              Cell.Get_Size (Widget, Cell_Area);
      Style : constant Gtk_Style_Context := Get_Style_Context (Widget);
   begin
      Save (Context);
      Rectangle
      (  Context,
         GDouble (Cell_Area.X),
         GDouble (Cell_Area.Y),
         GDouble (Cell_Area.Width),
         GDouble (Cell_Area.Height)
      );
      Clip (Context);
      Render_Layout
      (  Style,
         Context,
         GDouble
         (  Cell_Area.X
         +  GInt (Get_X_Pad (Cell))
         +  Area.X
         +  (Cell.Max_Offset - Cell.Left_Width)
         ),
         GDouble
         (  Cell_Area.Y
         +  GInt (Get_Y_Pad (Cell))
         +  Area.Y
         ),
         Cell.Text
      );
      Restore (Context);
   end Render;

   procedure Set_Property
             (  Cell : not null access Gtk_Cell_Renderer_Fixed_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Value_ID =>
            if not Cell.Empty then
               Cell.Value := Get_Double (Value);
            end if;
         when After_ID =>
            Cell.After := Integer (Get_UInt (Value));
         when Empty_ID =>
            Cell.Empty := Get_Boolean (Value);
         when others =>
            null;
      end case;
   end Set_Property;

   function Start_Editing
            (  Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
               Event  : Gdk_Event;
               Widget : not null access Gtk_Widget_Record'Class;
               Path            : UTF8_String;
               Background_Area : Gdk_Rectangle;
               Cell_Area       : Gdk_Rectangle;
               Flags           : Gtk_Cell_Renderer_State
            )  return Gtk_Widget is
      Editor    : Gtk_Entry;
      Text      : String (1..40);
      Start_Pos : Integer := Text'Last + 1;
   begin
      Put (Text, Cell.Value, Cell.After, 0);
      for Index in reverse Text'Range loop
         -- Find the beginning of the number in the output string
         if ' ' = Text (Index) then
            Start_Pos := Index + 1;
            exit;
         end if;
      end loop;
      Gtk_New (Editor);
      Set_Property (Editor, Build ("xalign"), Get_X_Align (Cell));
      Set_Property (Editor, Build ("has-frame"), False);
      Editor.Set_Text (Text (Start_Pos..Text'Last));
      Editor.Select_Region (0, -1);
      Entry_Callbacks.Connect
      (  Editor,
         "editing_done",
         Entry_Callbacks.To_Marshaller (Editing_Done'Access),
         Cell.all'Access
      );
      Cell.Focus_Out :=
         Entry_Return_Callbacks.Connect
         (  Editor,
            "focus_out_event",
            Entry_Return_Callbacks.To_Marshaller (Focus_Out'Access),
            Cell.all'Access
         );
      Editor.Show;
      return Editor.all'Access;
   end Start_Editing;

end Gtk.Cell_Renderer_Fixed;
