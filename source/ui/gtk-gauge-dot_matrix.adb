with Ada.Unchecked_Deallocation;
with Cairo.Ellipses;
with Gdk.Color.IHLS;
with Glib.Object.Checked_Destroy;
with Glib.Values;
with Gtk.Layered.Arc;

package body Gtk.Gauge.Dot_Matrix is

   use type Gdk.Color.IHLS.Gdk_Luminance;
   use type Cairo.Ellipses.Ellipse_Parameters;

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Dotted_Matrix_Array,
                                     Name   => Dotted_Matrix);

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => State_Array,
                                     Name   => States);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Corner : constant := 1.0 / 10.0;

   protected Lock is
      function On_Color (LED : Gtk_Gauge_Dot_Matrix_Record'Class)
                         return Gdk.Color.Gdk_Color;

      function Off_Color (LED : Gtk_Gauge_Dot_Matrix_Record'Class)
                          return Gdk.Color.Gdk_Color;

      procedure Set
        (LED : in out Gtk_Gauge_Dot_Matrix_Record'Class;
         On  : Gdk.Color.Gdk_Color;
         Off : Gdk.Color.Gdk_Color);
   end Lock;

   protected body Lock  is
      function Off_Color (LED : Gtk_Gauge_Dot_Matrix_Record'Class)
                          return Gdk.Color.Gdk_Color is
      begin
         return LED.Off;
      end Off_Color;

      function On_Color (LED : Gtk_Gauge_Dot_Matrix_Record'Class)
                         return Gdk.Color.Gdk_Color is
      begin
         return LED.On;
      end On_Color;

      procedure Set (LED : in out Gtk_Gauge_Dot_Matrix_Record'Class;
                     On  : in     Gdk.Color.Gdk_Color;
                     Off : in     Gdk.Color.Gdk_Color)
      is
         use type Gdk.Color.Gdk_Color;
      begin
         if LED.On /= On or else LED.Off /= Off then
            LED.Toggled := True;
            LED.On  := On;
            LED.Off := Off;
         end if;
      end Set;
   end Lock;

   overriding procedure Finalize
     (This : in out Gtk_Gauge_Dot_Matrix_Record) is
   begin
      if This.Dots /= null then
         Free (This.Dots);
      end if;

      if This.State /= null then
         Free (This.State);
      end if;

      Gtk.Layered.Finalize (Widget => Gtk.Layered.Gtk_Layered_Record (This));
   end Finalize;

   function Get_Background (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                            return not null access
     Gtk.Layered.Rectangular_Background.Rectangular_Background_Layer
   is
   begin
      return This.all.Background;
   end Get_Background;

   function Get_Cache (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                       return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return This.all.Cache;
   end Get_Cache;

   function Get_Off_Color (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                           return Gdk.Color.Gdk_Color is
   begin
      return Lock.Off_Color (This.all);
   end Get_Off_Color;

   function Get_On_Color (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                          return Gdk.Color.Gdk_Color is
   begin
      return Lock.On_Color (This.all);
   end Get_On_Color;

   function Get_State (This   : not null access Gtk_Gauge_Dot_Matrix_Record;
                       Column : in              Col_Index;
                       Row    : in              Row_Index) return Boolean is
   begin
      return This.all.State.all (Column, Row);
   end Get_State;

   function Get_Type return GType is
   begin
      if
        Initialize_Class_Record
          (Ancestor     => Gtk.Layered.Get_Type,
           Class_Record => Class_Record'Access,
           Type_Name    => Class_Name)
      then
         null;
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New
     (This          :    out Gtk_Gauge_Dot_Matrix;
      Columns       : in     Col_Index;
      Rows          : in     Row_Index;
      On_Color      : in     Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Off_Color     : in     Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Border_Shadow : in     Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_In) is
   begin
      This := new Gtk_Gauge_Dot_Matrix_Record;
      Initialize (This          => This,
                  Rows          => Rows,
                  Columns       => Columns,
                  On_Color      => On_Color,
                  Off_Color     => Off_Color,
                  Border_Shadow => Border_Shadow);
   exception
      when others =>
         Glib.Object.Checked_Destroy (This);
         This := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (This          : not null access Gtk_Gauge_Dot_Matrix_Record'Class;
      Columns       : in              Col_Index;
      Rows          : in              Row_Index;
      On_Color      : in              Gdk.Color.Gdk_Color;
      Off_Color     : in              Gdk.Color.Gdk_Color;
      Border_Shadow : in              Gtk.Enums.Gtk_Shadow_Type) is
   begin
      G_New (This, Get_Type);
      Gtk.Layered.Initialize (This);
      Lock.Set (This.all, On_Color, Off_Color);

      if This.all.Background /= null then
         This.all.Background.all.Finalize;
      end if;

      This.all.Background :=
        Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
          (Under         => This,
           Height        => 1.0,
           Width         => 1.0,
           Center        => (0.0, 0.0),
           Corner_Radius => Corner,
           Color         => Off_Color,
           Border_Width  => (case Border_Shadow is
                                when Gtk.Enums.Shadow_None       => 0.0,
                                when Gtk.Enums.Shadow_Etched_In |
                                     Gtk.Enums.Shadow_Etched_Out => 0.025,
                                when Gtk.Enums.Shadow_In |
                                     Gtk.Enums.Shadow_Out        => 0.05),
           Border_Depth  => (case Border_Shadow is
                                when Gtk.Enums.Shadow_None       => 0.0,
                                when Gtk.Enums.Shadow_Etched_In |
                                     Gtk.Enums.Shadow_Etched_Out => 0.075,
                                when Gtk.Enums.Shadow_In |
                                     Gtk.Enums.Shadow_Out        => 0.15),
           Border_Shadow => Border_Shadow,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      This.all.Set_Aspect_Ratio
        (Aspect_Ratio => Glib.Gdouble (Columns) / Glib.Gdouble (Rows));

      if This.all.State /= null then
         Free (This.all.State);
      end if;

      This.all.State := new State_Array (1 .. Columns, 1 .. Rows);

      if This.all.Dots /= null then
         Free (This.all.Dots);
      end if;

      This.all.Dots := new Dotted_Matrix_Array (1 .. Columns, 1 .. Rows);

      for X in This.all.Dots.all'Range (1) loop
         for Y in This.all.Dots.all'Range (2) loop
            pragma Compile_Time_Warning (True, "Ellipse parameters here.");
            declare
               X_Pos : constant Glib.Gdouble :=
                         Glib.Gdouble (X) / Glib.Gdouble (Columns + 1);
               Y_Pos : constant Glib.Gdouble :=
                         Glib.Gdouble (Y) / Glib.Gdouble (Rows + 1);
            begin
               This.all.Dots.all (X, Y) :=
                 Gtk.Layered.Arc.Add_Arc
                   (Under         => This.all.Background.all.Get_Foreground,
                    Ellipse       =>
                      ((Cairo.Ellipses.Unit_Circle / Glib.Gdouble (Columns)) +
                       (X_Pos - 0.5, Y_Pos - 0.5)),
                    Width         => 1.0,
                    Color         => On_Color,
                    Scaled        => True,
                    Widened       => False);
            end;
         end loop;
      end loop;

      This.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (This.all.Background.all.Get_Foreground);

      This.all.Update_State;
   end Initialize;

   overriding procedure Refresh
     (This    : not null access Gtk_Gauge_Dot_Matrix_Record;
      Context : in              Cairo.Cairo_Context) is
   begin
      if This.all.Toggled then
         This.all.Update_State;
         This.all.Toggled := False;
      end if;

      Gtk.Layered.Gtk_Layered_Record (This.all).Refresh (Context);
   end Refresh;

   procedure Set_Colors
     (This      : not null access Gtk_Gauge_Dot_Matrix_Record;
      On_Color  : in              Gdk.Color.Gdk_Color;
      Off_Color : in              Gdk.Color.Gdk_Color) is
   begin
      Lock.Set (This.all, On_Color, Off_Color);
   end Set_Colors;

   procedure Set_State (This   : not null access Gtk_Gauge_Dot_Matrix_Record;
                        Column : in              Col_Index;
                        Row    : in              Row_Index;
                        State  : in              Boolean) is
   begin
      if This.all.State.all (Column, Row) /= State then
         This.all.State.all (Column, Row) := State;
         This.all.Toggled := True;
      end if;
   end Set_State;

   procedure Update_State
     (This : not null access Gtk_Gauge_Dot_Matrix_Record) is
   begin
      This.all.Background.all.Set
        (Height         => This.all.Background.all.Get_Height,
         Width          => This.all.Background.all.Get_Width,
         Center         => This.all.Background.all.Get_Center,
         Rotation_Angle => This.all.Background.all.Get_Rotation_Angle,
         Corner_Radius  => This.all.Background.all.Get_Corner_Radius,
         Border_Width   => This.all.Background.all.Get_Border_Width,
         Border_Depth   => This.all.Background.all.Get_Border_Depth,
         Border_Color   => This.all.Background.all.Get_Border_Color,
         Border_Shadow  => This.all.Background.all.Get_Border_Shadow,
         Color          => Lock.Off_Color (This.all));

      Set_Arc_Color :
      declare
         Value     : Glib.Values.GValue;
         On_Color  : constant Gdk.Color.Gdk_Color := Lock.On_Color (This.all);
         Off_Color : constant Gdk.Color.Gdk_Color := Lock.Off_Color (This.all);
      begin
         for Column in This.all.Dots.all'Range (1) loop
            for Row in This.all.Dots.all'Range (2) loop
               Gdk.Color.Set_Value
                 (Value => Value,
                  Val   => (if This.all.State.all (Column, Row)
                            then On_Color
                            else Off_Color));
               This.all.Dots.all (Column, Row).all.Set_Property_Value
                 (Property => 11, -- Property_Line_Color,
                  Value    => Value);
            end loop;
         end loop;
      end Set_Arc_Color;
   end Update_State;

end Gtk.Gauge.Dot_Matrix;
