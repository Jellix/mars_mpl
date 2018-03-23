with Cairo;
with Gdk.Color;
with Gtk.Enums;
with Gtk.Layered.Cache;
with Gtk.Layered.Rectangular_Background;
with Gtk.Missed;

package Gtk.Gauge.Dot_Matrix is

   --
   -- Class_Name - Of the widget
   --
   Class_Name : constant String := "GtkGaugeDotMatrix";

   --
   type Row_Index is new Glib.Gint range 1 .. Glib.Gint'Last;

   type    Col_Count is new Glib.Gint range 0 .. Glib.Gint'Last;
   subtype Col_Index is Col_Count     range 1 .. Col_Count'Last;

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
   function Get_Type return GType;

   --
   -- Gtk_New -- Widget construction
   --
   --    This          - The result
   --    On_Color      - The LED's color when on
   --    Off_Color     - The LED's color when off
   --    Border_Shadow - Border shadow type
   --
   procedure Gtk_New
     (This          :    out Gtk_Gauge_Dot_Matrix;
      Columns       : in     Col_Index;
      Rows          : in     Row_Index;
      On_Color      : in     Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Off_Color     : in     Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Border_Shadow : in     Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_In);

   --
   -- Initialize -- The widget initialization
   --
   --    This          - The widget to initialize
   --    On_Color      - The LED's color when on
   --    Off_Color     - The LED's color when off
   --    Border_Shadow - Border shadow type
   --
   procedure Initialize
     (This          : not null access Gtk_Gauge_Dot_Matrix_Record'Class;
      Columns       : in              Col_Index;
      Rows          : in              Row_Index;
      On_Color      : in              Gdk.Color.Gdk_Color;
      Off_Color     : in              Gdk.Color.Gdk_Color;
      Border_Shadow : in              Gtk.Enums.Gtk_Shadow_Type);

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
   function Get_State
     (This   : not null access Gtk_Gauge_Dot_Matrix_Record;
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
      On_Color  : Gdk.Color.Gdk_Color;
      Off_Color : Gdk.Color.Gdk_Color);

   --
   -- Set_State -- Change a LED's status
   --
   --    This   - The widget
   --    State  - To set
   --
   -- Note that changing  state does not refresh the widget.  The operation
   -- is task safe.
   --
   procedure Set_State
     (This   : not null access Gtk_Gauge_Dot_Matrix_Record;
      Column : in              Col_Index;
      Row    : in              Row_Index;
      State  : in              Boolean);

   overriding procedure Refresh
     (This    : not null access Gtk_Gauge_Dot_Matrix_Record;
      Context : in              Cairo.Cairo_Context);

   overriding procedure Finalize
     (This : in out Gtk_Gauge_Dot_Matrix_Record);

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
         Toggled    : Boolean := False;
         On         : Gdk.Color.Gdk_Color;
         Off        : Gdk.Color.Gdk_Color;
         pragma Atomic (Toggled);
      end record;

   procedure Update_State (This : not null access Gtk_Gauge_Dot_Matrix_Record);

end Gtk.Gauge.Dot_Matrix;
