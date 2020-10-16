with Cairo;
with Gdk.Color;
with Gtk.Colors;
with Gtk.Layered.Cache;
with Gtk.Layered.Rectangular_Background;

package Gtk.Gauge.Dot_Matrix is

   --
   -- Class_Name - Of the widget
   --
   Class_Name : constant String := "GtkGaugeDotMatrix";

   Max_Columns : constant := 4096;
   Max_Rows    : constant := 4096;

   --
   type    Col_Count is           range 0 .. Max_Columns;
   subtype Col_Index is Col_Count range 1 .. Col_Count'Last;

   type    Row_Count is           range 0 .. Max_Rows;
   subtype Row_Index is Row_Count range 1 .. Row_Count'Last;

   --
   -- Gtk_Gauge_Dot_Matrix -- Dot Matrix Display
   --
   type Gtk_Gauge_Dot_Matrix_Record is
     new Gtk.Layered.Gtk_Layered_Record with private;
   type Gtk_Gauge_Dot_Matrix is
     access all Gtk_Gauge_Dot_Matrix_Record'Class;

   --
   -- Get_Cache -- The display's caching layer
   --
   --    This - The widget
   --
   -- If the widget is extended, static things which do not change with the
   -- widget state should be placed below the caching layer for performance
   -- reasons.
   --
   -- Returns :
   --
   --    The cache layer of the widget
   --
   function Get_Cache (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                       return not null access Gtk.Layered.Cache.Cache_Layer;

   --
   -- Get_Type -- The type of the widget
   --
   -- Returns :
   --
   --    The GTK type of the widget
   --
   function Get_Type_1 return Glib.GType; --  GNAT fix "not subtype conformant"

   --
   -- Gtk_New -- Widget construction
   --
   --    This          - The result
   --    Columns       - The number of columns in the dot matrix
   --    Rows          - The number of rows in the dot matrix
   --    BG_Color      - The color of the background
   --    On_Color      - The LED's color when on
   --    Off_Color     - The LED's color when off
   --
   procedure Gtk_New
     (This      :    out Gtk_Gauge_Dot_Matrix;
      Columns   : in     Col_Index;
      Rows      : in     Row_Index;
      BG_Color  : in     Gdk.Color.Gdk_Color := Gtk.Colors.Light_Grey;
      On_Color  : in     Gdk.Color.Gdk_Color := Gtk.Colors.Black;
      Off_Color : in     Gdk.Color.Gdk_Color := Gtk.Colors.White);

   --
   -- Initialize -- The widget initialization
   --
   --    This          - The widget to initialize
   --    Columns       - The number of columns in the dot matrix
   --    Rows          - The number of rows in the dot matrix
   --    BG_Color      - The color of the background
   --    On_Color      - The LED's color when on
   --    Off_Color     - The LED's color when off
   --
   procedure Initialize
     (This          : not null access Gtk_Gauge_Dot_Matrix_Record'Class;
      Columns       : in              Col_Index;
      Rows          : in              Row_Index;
      BG_Color      : in              Gdk.Color.Gdk_Color;
      On_Color      : in              Gdk.Color.Gdk_Color;
      Off_Color     : in              Gdk.Color.Gdk_Color);

   --
   -- Get_Background -- The display's background
   --
   --    This - The widget
   --
   -- Returns :
   --
   --    The background layer
   --
   function Get_Background (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                            return not null access
     Gtk.Layered.Rectangular_Background.Rectangular_Background_Layer;

   --
   -- Get_Off_Color -- The color of turned off LEDs
   --
   --    This - The widget
   --
   -- Returns :
   --
   --    The color
   --
   function Get_Off_Color (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                           return Gdk.Color.Gdk_Color;

   --
   -- Get_On_Color -- The color of turned on LEDs
   --
   --    This - The widget
   --
   -- Returns :
   --
   --    The color
   --
   function Get_On_Color (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                          return Gdk.Color.Gdk_Color;

   --
   -- Get_State -- The LEDs state
   --
   --    This - The widget
   --
   -- Returns :
   --
   --    The current state of the widget
   --
   function Get_State (This   : not null access Gtk_Gauge_Dot_Matrix_Record;
                       Column : in              Col_Index;
                       Row    : in              Row_Index) return Boolean;

   --
   -- Set_Colors -- Change the colors
   --
   --    This      - The widget
   --    On_Color  - The LED's color when on
   --    Off_Color - The LED's color when off
   --
   -- Note that changing state does not refresh the widget. The operation is
   -- task safe.
   --
   procedure Set_Colors
     (This      : not null access Gtk_Gauge_Dot_Matrix_Record;
      On_Color  : in              Gdk.Color.Gdk_Color;
      Off_Color : in              Gdk.Color.Gdk_Color);

   --
   -- Set_State -- Change a LED's status
   --
   --    This   - The widget
   --    State  - To set
   --
   -- Note that changing  state does not refresh the widget.  The operation
   -- is task safe.
   --
   procedure Set_State (This   : not null access Gtk_Gauge_Dot_Matrix_Record;
                        Column : in              Col_Index;
                        Row    : in              Row_Index;
                        State  : in              Boolean);

   overriding procedure Refresh
     (This    : not null access Gtk_Gauge_Dot_Matrix_Record;
      Context : in              Cairo.Cairo_Context);

   overriding procedure Finalize (This : in out Gtk_Gauge_Dot_Matrix_Record);

private

   type State_Array is array (Col_Index range <>,
                              Row_Index range <>) of Boolean
     with Atomic_Components => True;
   type States is access State_Array;

   type Dotted_Matrix_Array is array (Col_Index range <>,
                                      Row_Index range <>) of
     access Gtk.Layered.Abstract_Layer'Class;
   type Dotted_Matrix is access Dotted_Matrix_Array;

   type Gtk_Gauge_Dot_Matrix_Record is
     new Gtk.Layered.Gtk_Layered_Record with
      record
         Background : access Gtk.Layered.Rectangular_Background.Rectangular_Background_Layer;
         Cache      : access Gtk.Layered.Cache.Cache_Layer;
         Dots       : Dotted_Matrix;
         State      : States;
         Changed    : Boolean;
         On         : Gdk.Color.Gdk_Color;
         Off        : Gdk.Color.Gdk_Color;
         pragma Atomic (Changed);
      end record;

   procedure Update_State (This : not null access Gtk_Gauge_Dot_Matrix_Record);

end Gtk.Gauge.Dot_Matrix;
