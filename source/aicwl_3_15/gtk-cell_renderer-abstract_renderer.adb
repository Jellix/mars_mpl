--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer.Abstract_Renderer         Luebeck            --
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

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Glib.Messages;

with Gtk.Arguments;
with Gtk.Cell_Editable;
with Gtk.Missed;

with Gtkada.Bindings;

with Interfaces.C;
with Interfaces.C.Strings;

package body Gtk.Cell_Renderer.Abstract_Renderer is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Cell_Renderer.Abstract_Renderer." & Name;
   end Where;

   type GtkCellRendererClass;
   type GtkCellRendererClass_Ptr is access all GtkCellRendererClass;
   pragma Convention (C, GtkCellRendererClass_Ptr);

   type C_Dispose is access procedure (Object : System.Address);
   pragma Convention (C, C_Dispose);

   type C_Finalize is access procedure (Object : System.Address);
   pragma Convention (C, C_Finalize);

   type C_Activate is access function
     (Cell            : System.Address;
      Event           : Gdk_Event;
      Widget          : System.Address;
      Path            : Interfaces.C.Strings.chars_ptr;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return Gboolean;
   pragma Convention (C, C_Activate);

   type C_Editing_Canceled is access procedure (Cell : System.Address);
   pragma Convention (C, C_Editing_Canceled);

   type C_Editing_Started is access procedure
     (Cell     : System.Address;
      Editable : System.Address;
      Path     : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, C_Editing_Started);

   type C_Get_Aligned_Area is access procedure
     (Cell           : System.Address;
      Widget         : System.Address;
      Flags          : Gtk_Cell_Renderer_State;
      Cell_Rectangle : access Gdk_Rectangle;
      Aligned_Area   : access Gdk_Rectangle);
   pragma Convention (C, C_Get_Aligned_Area);

   type C_Get_Preferred_Width is access procedure
     (Cell         : System.Address;
      Widget       : System.Address;
      Minimum_Size : in out Gint;
      Natural_Size : in out Gint);
   pragma Convention (C, C_Get_Preferred_Width);

   type C_Get_Preferred_Height_For_Width is access procedure
     (Cell           : System.Address;
      Widget         : System.Address;
      Width          : Gint;
      Minimum_Height : in out Gint;
      Natural_Height : in out Gint);
   pragma Convention (C, C_Get_Preferred_Height_For_Width);

   type C_Get_Preferred_Height is access procedure
     (Cell         : System.Address;
      Widget       : System.Address;
      Minimum_Size : in out Gint;
      Natural_Size : in out Gint);
   pragma Convention (C, C_Get_Preferred_Height);

   type C_Get_Preferred_Width_For_Height is access procedure
     (Cell          : System.Address;
      Widget        : System.Address;
      Height        : Gint;
      Minimum_Width : in out Gint;
      Natural_Width : in out Gint);
   pragma Convention (C, C_Get_Preferred_Width_For_Height);

   type C_Get_Property is access procedure
     (Object    : System.Address;
      Param_ID  : Glib.Properties.Creation.Property_Id;
      Value     : in out Glib.Values.GValue;
      Param     : Param_Spec);
   pragma Convention (C, C_Get_Property);

   type C_Get_Request_Mode is access function
     (Cell : System.Address) return Gtk_Size_Request_Mode;
   pragma Convention (C, C_Get_Request_Mode);

   type Gdk_Rectangle_Ptr is access all Gdk_Rectangle;
   pragma Convention (C, Gdk_Rectangle_Ptr);

   type GInt_Ptr is access all Gint;
   pragma Convention (C, GInt_Ptr);

   type C_Get_Size is access procedure
     (Cell      : System.Address;
      Widget    : System.Address;
      Cell_Area : Gdk_Rectangle_Ptr;
      X_Offset  : GInt_Ptr;
      Y_Offset  : GInt_Ptr;
      Width     : GInt_Ptr;
      Height    : GInt_Ptr);
   pragma Convention (C, C_Get_Size);

   type C_Render is access procedure
     (Cell            : System.Address;
      Context         : System.Address;
      Widget          : System.Address;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State);
   pragma Convention (C, C_Render);

   type C_Set_Property is access procedure
     (Object   : System.Address;
      Param_ID : Glib.Properties.Creation.Property_Id;
      Value    : Glib.Values.GValue;
      Param    : Param_Spec);
   pragma Convention (C, C_Set_Property);

   type C_Start_Editing is access function
     (Object          : System.Address;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : System.Address;
      Path            : Interfaces.C.Strings.chars_ptr;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return System.Address;
   pragma Convention (C, C_Start_Editing);

   type GTypeQuery is
      record
         Type_Of       : GType;
         Type_Name     : Interfaces.C.Strings.chars_ptr;
         Class_Size    : Guint;
         Instance_Size : Guint;
      end record;
   pragma Convention (C, GTypeQuery);

   procedure Type_Query (Type_Of : GType; Query : out GTypeQuery);
   pragma Import (C, Type_Query, "g_type_query");

   type Dummy is array (1 .. 6) of System.Address;
   pragma Convention (C, Dummy);
   type GtkCellRendererClass is record
      G_Type                         : GType;      -- GTypeClass

      Construct_Properties           : System.Address;    -- GObjectClass
      Constructor                    : System.Address;
      Set_Property                   : C_Set_Property;
      Get_Property                   : C_Get_Property;
      Dispose                        : C_Dispose;
      Finalize                       : C_Finalize;
      Dispatch_Properties_Changed    : System.Address;
      Notify                         : System.Address;
      Constructed                    : System.Address;
      Flags                          : Gsize;
      P_Dummy                        : Dummy;
      -- GtkCellRendererClass
      Get_Request_Mode               : C_Get_Request_Mode;
      Get_Preferred_Width            : C_Get_Preferred_Width;
      Get_Preferred_Height_For_Width : C_Get_Preferred_Height_For_Width;
      Get_Preferred_Height           : C_Get_Preferred_Height;
      Get_Preferred_Width_For_Height : C_Get_Preferred_Width_For_Height;
      Get_Aligned_Area               : C_Get_Aligned_Area;
      Get_Size                       : C_Get_Size;
      Render                         : C_Render;
      Activate                       : C_Activate;
      Start_Editing                  : C_Start_Editing;
      Editing_Canceled               : C_Editing_Canceled;
      Editing_Started                : C_Editing_Started;
      Priv                           : System.Address;
      Gtk_Reserved_1                 : System.Address;
      Gtk_Reserved_2                 : System.Address;
      Gtk_Reserved_3                 : System.Address;
   end record;
   pragma Convention (C, GtkCellRendererClass);

   function To_Ptr is
     new Ada.Unchecked_Conversion
       (  GObject_Class,
          GtkCellRendererClass_Ptr
         );

   type Gtk_Abstract_Renderer is
     access all Gtk_Abstract_Renderer_Record'Class;

   GtkAda_String       : constant String := "_GtkAda" & ASCII.NUL;
   GtkAda_String_Quark : Glib.GQuark := Glib.Unknown_Quark;

   function To_Ada (Object : System.Address) return Gtk_Abstract_Renderer
   is
      function Internal (Object : System.Address; Quark : GQuark)
                         return System.Address;
      pragma Import (C, Internal, "g_object_get_qdata");
      function To_Object is
        new Ada.Unchecked_Conversion (System.Address, Gtk_Abstract_Renderer);

      use type System.Address;
   begin
      if Object = System.Null_Address then
         return null;
      end if;
      if GtkAda_String_Quark = Unknown_Quark then
         GtkAda_String_Quark := Quark_From_String (GtkAda_String);
      end if;
      return To_Object (Internal (Object, GtkAda_String_Quark));
   end To_Ada;

   procedure Free is
     new Ada.Unchecked_Deallocation (Cell_Path, Cell_Path_Ptr);

   Parent_Class : GtkCellRendererClass_Ptr := null;

   type GType_Info is record
      Class_Size     : Guint16;
      Base_Init      : System.Address;
      Base_Finalize  : System.Address;
      Class_Init     : C_Class_Init;
      Class_Finalize : System.Address;
      Class_Data     : System.Address;
      Instance_Size  : Guint16;
      Preallocs      : Guint16;
      Instance_Init  : System.Address;
      Value_Table    : System.Address;
   end record;
   pragma Convention (C, GType_Info);

   procedure Marsh_Gtk_Abstract_Renderer_Void
     (Closure         : Gtkada.Bindings.GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Abstract_Renderer_Void);

   function On_Activate
     (Cell            : System.Address;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : System.Address;
      Path            : Interfaces.C.Strings.chars_ptr;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return Gboolean;
   pragma Convention (C, On_Activate);

   procedure On_Delete (Object : System.Address);
   pragma Convention (C, On_Delete);

   --     procedure On_Editing_Canceled (Cell : System.Address);
   --     pragma Convention (C, On_Editing_Canceled);
   --
   --     procedure On_Editing_Started
   --               (  Cell     : System.Address;
   --                  Editable : System.Address;
   --                  Path     : Interfaces.C.Strings.Chars_Ptr
   --               );
   --     pragma Convention (C, On_Editing_Started);

   procedure On_Get_Aligned_Area
     (Cell           : System.Address;
      Widget         : System.Address;
      Flags          : Gtk_Cell_Renderer_State;
      Cell_Rectangle : access Gdk_Rectangle;
      Aligned_Area   : access Gdk_Rectangle);
   pragma Convention (C, On_Get_Aligned_Area);

   procedure On_Get_Preferred_Width
     (Cell         : System.Address;
      Widget       : System.Address;
      Minimum_Size : in out Gint;
      Natural_Size : in out Gint);
   pragma Convention (C, On_Get_Preferred_Width);

   procedure On_Get_Preferred_Height_For_Width
     (Cell           : System.Address;
      Widget         : System.Address;
      Width          : Gint;
      Minimum_Height : in out Gint;
      Natural_Height : in out Gint);
   pragma Convention (C, On_Get_Preferred_Height_For_Width);

   procedure On_Get_Preferred_Height
     (Cell         : System.Address;
      Widget       : System.Address;
      Minimum_Size : in out Gint;
      Natural_Size : in out Gint);
   pragma Convention (C, On_Get_Preferred_Height);

   procedure On_Get_Preferred_Width_For_Height
     (Cell          : System.Address;
      Widget        : System.Address;
      Height        : Gint;
      Minimum_Width : in out Gint;
      Natural_Width : in out Gint);
   pragma Convention (C, On_Get_Preferred_Width_For_Height);

   procedure On_Get_Property
     (Cell      : System.Address;
      Param_ID  : Glib.Properties.Creation.Property_Id;
      Value     : in out Glib.Values.GValue;
      Param     : Param_Spec);
   pragma Convention (C, On_Get_Property);

   function On_Get_Request_Mode
     (Cell : System.Address) return Gtk_Size_Request_Mode;
   pragma Convention (C, On_Get_Request_Mode);

   procedure On_Get_Size
     (Cell      : System.Address;
      Widget    : System.Address;
      Cell_Area : Gdk_Rectangle_Ptr;
      X_Offset  : GInt_Ptr;
      Y_Offset  : GInt_Ptr;
      Width     : GInt_Ptr;
      Height    : GInt_Ptr);
   pragma Convention (C, On_Get_Size);

   procedure On_Render
     (Cell            : System.Address;
      Context         : System.Address;
      Widget          : System.Address;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State);
   pragma Convention (C, On_Render);

   procedure On_Set_Property
     (Cell     : System.Address;
      Param_ID : Glib.Properties.Creation.Property_Id;
      Value    : Glib.Values.GValue;
      Param    : Param_Spec);
   pragma Convention (C, On_Set_Property);

   function On_Start_Editing
     (Cell            : System.Address;
      Event           : Gdk_Event;
      Widget          : System.Address;
      Path            : Interfaces.C.Strings.chars_ptr;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return System.Address;
   pragma Convention (C, On_Start_Editing);

   overriding function Activate
     (  Cell          : not null access Gtk_Abstract_Renderer_Record;
        Event           : Gdk_Event;
        Widget          : not null access Gtk_Widget_Record'Class;
        Path            : UTF8_String;
        Background_Area : Gdk_Rectangle;
        Cell_Area       : Gdk_Rectangle;
        Flags           : Gtk_Cell_Renderer_State
       )  return Boolean is
   begin
      return False;
   end Activate;

   overriding procedure Adjust  (Path : in out Cell_Path_Ref) is
   begin
      Path.Ref := null;
   end Adjust;

   procedure Base_Class_Init (Class : GObject_Class) is
      function Class_Peek_Parent (Class : GtkCellRendererClass_Ptr)
                                  return GtkCellRendererClass_Ptr;
      pragma Import (C, Class_Peek_Parent, "g_type_class_peek_parent");
      This : constant GtkCellRendererClass_Ptr := To_Ptr (Class);
   begin
      if Parent_Class = null then
         Parent_Class := Class_Peek_Parent (This);
      end if;
      This.Get_Property         := On_Get_Property'Access;
      This.Set_Property         := On_Set_Property'Access;
      This.Finalize             := On_Delete'Access;
      This.Get_Request_Mode     := On_Get_Request_Mode'Access;
      This.Get_Preferred_Width  := On_Get_Preferred_Width'Access;
      This.Get_Preferred_Height := On_Get_Preferred_Height'Access;
      This.Get_Aligned_Area     := On_Get_Aligned_Area'Access;
      This.Get_Size             := On_Get_Size'Access;
      This.Render               := On_Render'Access;
      This.Activate             := On_Activate'Access;
      This.Start_Editing        := On_Start_Editing'Access;
      --        This.Editing_Started      := On_Editing_Started'Access;
      --        This.Editing_Canceled     := On_Editing_Canceled'Access;
      This.Get_Preferred_Height_For_Width :=
        On_Get_Preferred_Height_For_Width'Access;
      This.Get_Preferred_Width_For_Height :=
        On_Get_Preferred_Width_For_Height'Access;
   end Base_Class_Init;

   procedure Cancel
     (Cell : not null access Gtk_Abstract_Renderer_Record)
   is
      procedure Internal (Object : System.Address; Name : String);
      pragma Import (C, Internal, "ada_g_signal_emit_by_name");
   begin
      Internal (Get_Object (Cell), "editing-canceled" & ASCII.NUL);
   end Cancel;

   procedure Commit
     (Cell : not null access Gtk_Abstract_Renderer_Record)
   is
      procedure Internal (Object : System.Address; Name : String);
      pragma Import (C, Internal, "ada_g_signal_emit_by_name");
   begin
      Internal (Get_Object (Cell), "commit" & ASCII.NUL);
   end Commit;

   overriding procedure Finalize (Path : in out Cell_Path_Ref) is
   begin
      Free (Path.Ref);
   end Finalize;

   procedure Finalize
     (Cell : not null access Gtk_Abstract_Renderer_Record) is
   begin
      null;
   end Finalize;

   function Get_Mode
     (Cell : not null access Gtk_Abstract_Renderer_Record)
      return Gtk_Cell_Renderer_Mode is
   begin
      return
        Gtk_Cell_Renderer_Mode_Properties.Get_Property
          (Cell,
           Gtk_Cell_Renderer_Mode_Properties.Property_RO (Mode_Property));
   end Get_Mode;

   function Get_Path
     (  Cell : not null access Gtk_Abstract_Renderer_Record
       )  return String is
   begin
      if Cell.Path.Ref = null then
         return "";
      else
         return Cell.Path.Ref.Text (1 .. Cell.Path.Ref.Length);
      end if;
   end Get_Path;

   overriding procedure Get_Preferred_Height
     (  Cell         : not null access Gtk_Abstract_Renderer_Record;
        Widget         : not null access Gtk_Widget_Record'Class;
        Minimum_Height : out Gint;
        Natural_Height : out Gint
       )  is
      Area : constant Gdk_Rectangle :=
               Get_Size
                 (  Gtk_Abstract_Renderer_Record'Class (Cell.all)'Access,
                    Widget
                   );
   begin
      Minimum_Height := Area.Height;
      Natural_Height := Area.Height;
   end Get_Preferred_Height;

   overriding procedure Get_Preferred_Height_For_Width
     (  Cell         : not null access Gtk_Abstract_Renderer_Record;
        Widget         : not null access Gtk_Widget_Record'Class;
        Width          : Gint;
        Minimum_Height : out Gint;
        Natural_Height : out Gint
       )  is
      Area : constant Gdk_Rectangle :=
               Get_Size
                 (  Gtk_Abstract_Renderer_Record'Class (Cell.all)'Access,
                    Widget
                   );
   begin
      Minimum_Height := Area.Height;
      Natural_Height := Area.Height;
   end Get_Preferred_Height_For_Width;

   overriding procedure Get_Preferred_Width_For_Height
     (  Cell        : not null access Gtk_Abstract_Renderer_Record;
        Widget        : not null access Gtk_Widget_Record'Class;
        Height        : Gint;
        Minimum_Width : out Gint;
        Natural_Width : out Gint
       )  is
      Area : constant Gdk_Rectangle :=
               Get_Size
                 (  Gtk_Abstract_Renderer_Record'Class (Cell.all)'Access,
                    Widget
                   );
   begin
      Minimum_Width := Area.Width;
      Natural_Width := Area.Width;
   end Get_Preferred_Width_For_Height;

   overriding procedure Get_Preferred_Width
     (  Cell        : not null access Gtk_Abstract_Renderer_Record;
        Widget        : not null access Gtk_Widget_Record'Class;
        Minimum_Width : out Gint;
        Natural_Width : out Gint
       )  is
      Area : constant Gdk_Rectangle :=
               Get_Size
                 (  Gtk_Abstract_Renderer_Record'Class (Cell.all)'Access,
                    Widget
                   );
   begin
      Minimum_Width := Area.Width;
      Natural_Width := Area.Width;
   end Get_Preferred_Width;

   procedure Get_Property
     (Cell          : not null access Gtk_Abstract_Renderer_Record;
      Param_ID      : Glib.Properties.Creation.Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec) is
   begin
      Parent_Class.Get_Property
        (  Get_Object (Cell),
           Param_ID,
           Value,
           Property_Spec
          );
   end Get_Property;

   overriding function Get_Request_Mode
     (  Cell : not null access Gtk_Abstract_Renderer_Record
       )  return Gtk_Size_Request_Mode is
   begin
      return Constant_Size;
   end Get_Request_Mode;

   function Get_X_Align
     (  Cell : not null access Gtk_Abstract_Renderer_Record
       )  return Gfloat is
      X, Y : Gfloat;
   begin
      Cell.Get_Alignment (X, Y);
      return X;
   end Get_X_Align;

   function Get_X_Pad
     (  Cell : not null access Gtk_Abstract_Renderer_Record
       )  return Guint is
      X, Y : Gint;
   begin
      Cell.Get_Padding (X, Y);
      return Guint (X);
   end Get_X_Pad;

   function Get_Y_Align
     (  Cell : not null access Gtk_Abstract_Renderer_Record
       )  return Gfloat is
      X, Y : Gfloat;
   begin
      Cell.Get_Alignment (X, Y);
      return Y;
   end Get_Y_Align;

   function Get_Y_Pad
     (  Cell : not null access Gtk_Abstract_Renderer_Record
       )  return Guint is
      X, Y : Gint;
   begin
      Cell.Get_Padding (X, Y);
      return Guint (Y);
   end Get_Y_Pad;

   procedure Initialize
     (  Cell    : not null access
          Gtk_Abstract_Renderer_Record'Class;
        Type_Of : GType
       )  is
      function Object_New (Typ : GType) return System.Address;
      pragma Import (C, Object_New, "ada_g_object_new");
   begin
      Set_Object (Cell, Object_New (Type_Of));
   end Initialize;

   procedure Marsh_Gtk_Abstract_Renderer_Void
     (Closure         : Gtkada.Bindings.GClosure;
      Return_Value    : Glib.Values.GValue;
      N_Params        : Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : System.Address)
   is
      function From_Address is
        new Ada.Unchecked_Conversion (System.Address, Commit_Callback);
   begin
      From_Address (Gtkada.Bindings.Get_Callback (Closure))
        (Gtk_Abstract_Renderer (Gtk.Arguments.Unchecked_To_Object (Params, 0)));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Marsh_Gtk_Abstract_Renderer_Void"));
   end Marsh_Gtk_Abstract_Renderer_Void;

   function On_Activate
     (Cell            : System.Address;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : System.Address;
      Path            : Interfaces.C.Strings.chars_ptr;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return Gboolean
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if This /= null then
         declare
            Renderer : Gtk_Abstract_Renderer_Record'Class renames
                         This.all;
         begin
            if Renderer.Get_Mode = Cell_Renderer_Mode_Activatable then
               if Path = Interfaces.C.Strings.Null_Ptr then
                  Set (Renderer.Path, "");
               else
                  Set (Renderer.Path, Interfaces.C.Strings.Value (Path));
               end if;
               if Renderer.Activate
                 (Event,
                  Convert (Widget),
                  Get_Path (This),
                  Background_Area.all,
                  Cell_Area.all,
                  Flags)
               then
                  return 1;
               end if;
            end if;
         end;
      end if;
      return 0;
   end On_Activate;

   procedure On_Commit
     (  Cell    : not null access Gtk_Abstract_Renderer_Record;
        Handler : not null Commit_Callback;
        After   : Boolean := False
       )  is
      function To_Address is
        new Ada.Unchecked_Conversion (Commit_Callback, System.Address);
   begin
      Gtkada.Bindings.Unchecked_Do_Signal_Connect
        (Object     => Cell,
         C_Name     => "commit" & Character'Val (0),
         Marshaller => Marsh_Gtk_Abstract_Renderer_Void'Access,
         Handler    => To_Address (Handler), --  Set in the closure
         After      => After);
   end On_Commit;

   procedure On_Delete (Object : System.Address) is
      This : constant Gtk_Abstract_Renderer := To_Ada (Object);
   begin
      if This /= null then
         This.Finalize;
      end if;
      if Parent_Class.Finalize /= null then
         Parent_Class.Finalize (Object);
      end if;
   end On_Delete;

   procedure On_Get_Aligned_Area
     (Cell           : System.Address;
      Widget         : System.Address;
      Flags          : Gtk_Cell_Renderer_State;
      Cell_Rectangle : access Gdk_Rectangle;
      Aligned_Area   : access Gdk_Rectangle)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         Aligned_Area.all :=
           This.Get_Aligned_Area
             (  Convert (Widget),
                Flags,
                Cell_Rectangle.all
               );
      end if;
   end On_Get_Aligned_Area;

   procedure On_Get_Preferred_Height
     (Cell         : System.Address;
      Widget       : System.Address;
      Minimum_Size : in out Gint;
      Natural_Size : in out Gint)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         This.Get_Preferred_Height
           (  Convert (Widget),
              Minimum_Size,
              Natural_Size
             );
      end if;
   end On_Get_Preferred_Height;

   procedure On_Get_Preferred_Height_For_Width
     (Cell           : System.Address;
      Widget         : System.Address;
      Width          : Gint;
      Minimum_Height : in out Gint;
      Natural_Height : in out Gint)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         This.Get_Preferred_Height_For_Width
           (Convert (Widget),
            Width,
            Minimum_Height,
            Natural_Height);
      end if;
   end On_Get_Preferred_Height_For_Width;

   procedure On_Get_Preferred_Width
     (Cell         : System.Address;
      Widget       : System.Address;
      Minimum_Size : in out Gint;
      Natural_Size : in out Gint)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         This.Get_Preferred_Width
           (  Convert (Widget),
              Minimum_Size,
              Natural_Size
             );
      end if;
   end On_Get_Preferred_Width;

   procedure On_Get_Preferred_Width_For_Height
     (Cell          : System.Address;
      Widget        : System.Address;
      Height        : Gint;
      Minimum_Width : in out Gint;
      Natural_Width : in out Gint)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         This.Get_Preferred_Width_For_Height
           (  Convert (Widget),
              Height,
              Minimum_Width,
              Natural_Width
             );
      end if;
   end On_Get_Preferred_Width_For_Height;

   procedure On_Get_Property
     (Cell     : System.Address;
      Param_ID : Glib.Properties.Creation.Property_Id;
      Value    : in out Glib.Values.GValue;
      Param    : Param_Spec)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         This.Get_Property (Param_ID, Value, Param);
      end if;
   end On_Get_Property;

   function On_Get_Request_Mode
     (Cell : System.Address) return Gtk_Size_Request_Mode
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         return This.Get_Request_Mode;
      end if;
      return Constant_Size;
   end On_Get_Request_Mode;

   procedure On_Get_Size
     (Cell      : System.Address;
      Widget    : System.Address;
      Cell_Area : Gdk_Rectangle_Ptr;
      X_Offset  : GInt_Ptr;
      Y_Offset  : GInt_Ptr;
      Width     : GInt_Ptr;
      Height    : GInt_Ptr)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         declare
            Area : Gdk_Rectangle;
         begin
            if Cell_Area = null then
               Area := This.Get_Size (Convert (Widget));
            else
               Area := This.Get_Size (Convert (Widget), Cell_Area.all);
            end if;
            if X_Offset /= null then
               X_Offset.all := Area.X;
            end if;
            if Y_Offset /= null then
               Y_Offset.all := Area.Y;
            end if;
            if Width /= null then
               Width.all := Area.Width;
            end if;
            if Height /= null then
               Height.all := Area.Height;
            end if;
         end;
      end if;
   end On_Get_Size;

   procedure On_Render
     (Cell            : System.Address;
      Context         : System.Address;
      Widget          : System.Address;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State)
   is
      function To_Context is
        new Ada.Unchecked_Conversion (System.Address, Cairo_Context);
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         This.Render
           (  To_Context (Context),
              Convert (Widget),
              Background_Area.all,
              Cell_Area.all,
              Flags
             );
      end if;
   end On_Render;

   procedure On_Set_Property
     (Cell     : System.Address;
      Param_ID : Glib.Properties.Creation.Property_Id;
      Value    : Glib.Values.GValue;
      Param    : Param_Spec)
   is
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);
   begin
      if This /= null then
         This.Set_Property (Param_ID, Value, Param);
      end if;
   end On_Set_Property;

   function On_Start_Editing
     (Cell            : System.Address;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : System.Address;
      Path            : Interfaces.C.Strings.chars_ptr;
      Background_Area : access Gdk_Rectangle;
      Cell_Area       : access Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return System.Address
   is
      function Internal (Object : System.Address; Interface_Type : GType)
                         return System.Address;
      pragma Import (C, Internal, "g_type_check_instance_cast");
      This : constant Gtk_Abstract_Renderer := To_Ada (Cell);

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if This /= null then
         if Path = Interfaces.C.Strings.Null_Ptr then
            Set (This.Path, "");
         else
            Set (This.Path, Interfaces.C.Strings.Value (Path));
         end if;
         declare
            Editor   : Gtk_Widget renames
                         This.Start_Editing
                           (Event,
                            Convert (Widget),
                            This.Get_Path,
                            Background_Area.all,
                            Cell_Area.all,
                            Flags);
            Editable : System.Address;
         begin
            if Editor /= null then
               --
               -- Conversion  to the interface.  When failed  the result
               -- is System.Null_Address
               --
               Editable :=
                 Internal
                   (  Get_Object (Editor),
                      Gtk.Cell_Editable.Get_Type
                     );
               return Editable;
            end if;
         end;
      end if;
      return System.Null_Address;
   end On_Start_Editing;

   function Register
     (  Name : String;
        Init : not null C_Class_Init := Base_Class_Init'Access
       )  return GType is
   begin
      return Register_Type (Name => Name, Init => Init);
   end Register;

   function Register_Type
     (  Name : String;
        Size : Positive := GtkCellRenderer'Size;
        Init : not null C_Class_Init := Base_Class_Init'Access
       )  return GType is
      function Cell_Renderer_Type return GType;
      pragma Import
        (  C,
           Cell_Renderer_Type,
           "gtk_cell_renderer_get_type"
          );
      function Register_Static
        (Parent_Type : GType;
         Type_Name   : Interfaces.C.char_array;
         Type_Info   : access GType_Info;
         Type_Flags  : Guint) return Glib.GType;
      pragma Import (C, Register_Static, "g_type_register_static");
      function Signal_NewV
        (Name         : Interfaces.C.char_array;
         IType        : GType;
         Signal_Flags : Guint;
         Closure      : System.Address;
         Accumulator  : System.Address;
         Accu_Data    : System.Address;
         C_Marshaller : System.Address;
         Return_Type  : GType;
         N_Params     : Guint;
         Params       : System.Address) return Guint;
      pragma Import (C, Signal_NewV, "g_signal_newv");
      procedure Marshaller
        (Closure         : System.Address;
         Return_Value    : access Glib.Values.GValue;
         N_Param_Values  : Guint;
         Param_Values    : access Glib.Values.GValue;
         Invocation_Hint : System.Address;
         Marshal_Data    : System.Address);
      pragma Import (C, Marshaller, "g_cclosure_marshal_VOID__VOID");

      Result    : GType;
      Info      : GTypeQuery;
      Commit_ID : Guint;
      Parent    : GObject_Class :=
                    Gtk.Missed.Class_From_Type (Cell_Renderer_Type);
   begin
      Type_Query (Cell_Renderer_Type, Info);
      if Guint (Size / Interfaces.C.char'Size) < Info.Instance_Size then
         raise Constraint_Error with
           "GTK object instance of '"
           & Name
           & "' is smaller than parent '"
           & Interfaces.C.Strings.Value (Info.Type_Name)
           & "' "
           & Integer'Image (Size / Interfaces.C.char'Size)
           & " <"
           & Guint'Image (Info.Instance_Size);
      end if;
      Result :=
        Register_Static
          (Parent_Type => Cell_Renderer_Type,
           Type_Name   => Interfaces.C.To_C (Name),
           Type_Flags  => 0,
           Type_Info   =>
              new GType_Info'
             (Class_Size     =>
                    Guint16'Max
                (GtkCellRendererClass'Size / Interfaces.C.char'Size,
                 Guint16 (Info.Class_Size)),
              Base_Init      => System.Null_Address,
              Base_Finalize  => System.Null_Address,
              Class_Init     => Init,
              Class_Finalize => System.Null_Address,
              Class_Data     => System.Null_Address,
              Preallocs      => 0,
              Instance_Init  => System.Null_Address,
              Value_Table    => System.Null_Address,
              Instance_Size  =>
                Guint16 (Size / Interfaces.C.char'Size)));
      Commit_ID :=
        Signal_NewV
          (Name         => Interfaces.C.To_C ("commit"),
           IType        => Result,
           Signal_Flags => 2, -- G_SIGNAL_RUN_LAST
           Closure      => System.Null_Address,
           Accumulator  => System.Null_Address,
           Accu_Data    => System.Null_Address,
           C_Marshaller => Marshaller'Address,
           Return_Type  => GType_None,
           N_Params     => 0,
           Params       => System.Null_Address);
      return Result;
   end Register_Type;

   procedure Set_Mode
     (Cell : not null access Gtk_Abstract_Renderer_Record;
      Mode : Gtk_Cell_Renderer_Mode) is
   begin
      Gtk_Cell_Renderer_Mode_Properties.Set_Property
        (  Cell,
           Gtk_Cell_Renderer_Mode_Properties.Property (Mode_Property),
           Mode
          );
   end Set_Mode;

   procedure Set (Path : in out Cell_Path_Ref; Text : String) is
   begin
      if Path.Ref = null then
         if Text'Length > 0 then
            Path.Ref := new Cell_Path'(Text'Length, Text'Length, Text);
         end if;
      else
         if Path.Ref.Size < Text'Length then
            Free (Path.Ref);
            Path.Ref := new Cell_Path'(Text'Length, Text'Length, Text);
         else
            Path.Ref.Text (1 .. Text'Length) := Text;
            Path.Ref.Length := Text'Length;
         end if;
      end if;
   end Set;

   procedure Set_Property
     (Cell          : not null access Gtk_Abstract_Renderer_Record;
      Param_ID      : Glib.Properties.Creation.Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec) is
   begin
      Parent_Class.Set_Property
        (  Get_Object (Cell),
           Param_ID,
           Value,
           Property_Spec
          );
   end Set_Property;

   function Start_Editing
     (Cell          : not null access Gtk_Abstract_Renderer_Record;
      Event           : Gdk_Event;
      Widget          : not null access Gtk_Widget_Record'Class;
      Path            : UTF8_String;
      Background_Area : Gdk_Rectangle;
      Cell_Area       : Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State) return Gtk_Widget is
   begin
      return null;
   end Start_Editing;

   overriding procedure Stop_Editing
     (Cell    : not null access Gtk_Abstract_Renderer_Record;
      Cancelled : Boolean)
   is
      procedure Internal (Cell : System.Address; Cancelled : Gboolean);
      pragma Import (C, Internal, "gtk_cell_renderer_stop_editing");
   begin
      if Cancelled then
         Internal (Get_Object (Cell), 1);
         Cancel (Cell);
      else
         Internal (Get_Object (Cell), 0);
      end if;
   end Stop_Editing;

end Gtk.Cell_Renderer.Abstract_Renderer;
