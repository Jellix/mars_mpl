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
            LED.On  := On;
            LED.Off := Off;
            LED.Toggled := True;
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
      On_Color      : in     Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Off_Color     : in     Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0)) is
   begin
      This := new Gtk_Gauge_Dot_Matrix_Record;
      Initialize (This      => This,
                  Rows      => Rows,
                  Columns   => Columns,
                  On_Color  => On_Color,
                  Off_Color => Off_Color);
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
      Off_Color     : in              Gdk.Color.Gdk_Color) is
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
           Corner_Radius => 1.0 / 100.0,
           Color         => Off_Color,
           Border_Width  => 0.0,
           Border_Depth  => 0.0,
           Border_Shadow => Gtk.Enums.Shadow_None,
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
         for X in This.all.Dots.all'Range (1) loop
            for Y in This.all.Dots.all'Range (2) loop
               This.all.Dots.all (X, Y).all.Finalize;
            end loop;
         end loop;

         Free (This.all.Dots);
      end if;

      This.all.Dots := new Dotted_Matrix_Array (1 .. Columns, 1 .. Rows);

      declare
         Col_Double : constant Glib.Gdouble := Glib.Gdouble (Columns);
         Row_Double : constant Glib.Gdouble := Glib.Gdouble (Rows);
         Curvature  : constant Glib.Gdouble :=
                        This.all.Get_Aspect_Ratio / 2.0 * Col_Double;
      begin
         for X in This.all.Dots.all'Range (1) loop
            declare
               X_Pos : constant Glib.Gdouble :=
                         (Glib.Gdouble (X) / (Col_Double + 1.0)) - 0.5;
            begin
               for Y in This.all.Dots.all'Range (2) loop
                  declare
                     Y_Pos : constant Glib.Gdouble :=
                               ((Glib.Gdouble (Y) / (Row_Double + 1.0)) - 0.5) / This.all.Get_Aspect_Ratio;
                     E     : constant Cairo.Ellipses.Ellipse_Parameters :=
                               Cairo.Ellipses.Ellipse_Parameters'
                                 (Center          => (X_Pos, Y_Pos),
                                  Major_Curvature => Curvature,
                                  Minor_Radius    => 1.0 / Curvature,
                                  Angle           => 0.0);
                  begin
                     This.all.State.all (X, Y) := False;
                     This.all.Dots.all (X, Y) :=
                       Gtk.Layered.Arc.Add_Arc
                         (Under         => This.all.Background.all.Get_Foreground,
                          Ellipse       => E,
                          Width         => 1.0 / Curvature,
                          Color         => Off_Color,
                          Scaled        => True,
                          Widened       => True);
                  end;
               end loop;
            end;
         end loop;
      end;

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
