with Ada.Unchecked_Deallocation;
with Cairo.Ellipses;
with Glib.Object.Checked_Destroy;
with Glib.Values;
with Gtk.Enums;
with Gtk.Layered.Arc;

package body Gtk.Gauge.Dot_Matrix is

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Dotted_Matrix_Array,
                                     Name   => Dotted_Matrix);

   procedure Free is new Ada.Unchecked_Deallocation (Object => State_Array,
                                                     Name   => States);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   protected Lock is
      function On_Color (DM : in Gtk_Gauge_Dot_Matrix_Record'Class)
                         return Gdk.Color.Gdk_Color;

      function Off_Color (DM : in Gtk_Gauge_Dot_Matrix_Record'Class)
                          return Gdk.Color.Gdk_Color;

      procedure Set (DM  : in out Gtk_Gauge_Dot_Matrix_Record'Class;
                     On  : in     Gdk.Color.Gdk_Color;
                     Off : in     Gdk.Color.Gdk_Color);
   end Lock;

   protected body Lock  is
      function Off_Color (DM : in Gtk_Gauge_Dot_Matrix_Record'Class)
                          return Gdk.Color.Gdk_Color is
      begin
         return DM.Off;
      end Off_Color;

      function On_Color (DM : in Gtk_Gauge_Dot_Matrix_Record'Class)
                         return Gdk.Color.Gdk_Color is
      begin
         return DM.On;
      end On_Color;

      procedure Set (DM  : in out Gtk_Gauge_Dot_Matrix_Record'Class;
                     On  : in     Gdk.Color.Gdk_Color;
                     Off : in     Gdk.Color.Gdk_Color)
      is
         use type Gdk.Color.Gdk_Color;
      begin
         if DM.On /= On or else DM.Off /= Off then
            DM.On      := On;
            DM.Off     := Off;
            DM.Changed := True;
         end if;
      end Set;
   end Lock;

   overriding procedure Finalize (This : in out Gtk_Gauge_Dot_Matrix_Record) is
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
     (This      :    out Gtk_Gauge_Dot_Matrix;
      Columns   : in     Col_Index;
      Rows      : in     Row_Index;
      BG_Color  : in     Gdk.Color.Gdk_Color := Gtk.Colors.Light_Grey;
      On_Color  : in     Gdk.Color.Gdk_Color := Gtk.Colors.Black;
      Off_Color : in     Gdk.Color.Gdk_Color := Gtk.Colors.White) is
   begin
      This := new Gtk_Gauge_Dot_Matrix_Record;
      Initialize (This      => This,
                  Rows      => Rows,
                  Columns   => Columns,
                  BG_Color  => BG_Color,
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
      BG_Color      : in              Gdk.Color.Gdk_Color;
      On_Color      : in              Gdk.Color.Gdk_Color;
      Off_Color     : in              Gdk.Color.Gdk_Color)
   is
      Col_Double : constant Glib.Gdouble := Glib.Gdouble (Columns);
      Row_Double : constant Glib.Gdouble := Glib.Gdouble (Rows);
   begin
      G_New (Object => This,
             Typ    => Get_Type);
      Gtk.Layered.Initialize (This);
      Lock.Set (DM  => This.all,
                On  => On_Color,
                Off => Off_Color);

      if This.all.Background /= null then
         This.all.Background.all.Finalize;
      end if;

      This.all.Background :=
        Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
          (Under         => This,
           Height        => 1.0,
           Width         => 1.0,
           Center        => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                        Y => 0.0),
           Corner_Radius => 1.0 / 100.0,
           Color         => BG_Color,
           Border_Width  => 0.0,
           Border_Depth  => 0.0,
           Border_Shadow => Gtk.Enums.Shadow_None,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      This.all.Set_Aspect_Ratio (Aspect_Ratio => Col_Double / Row_Double);

      if This.all.State /= null then
         Free (This.all.State);
      end if;

      This.all.State := new State_Array (1 .. Columns, 1 .. Rows);

      if This.all.Dots /= null then
         Finalize_Column :
         for X in This.all.Dots.all'Range (1) loop
            Finalize_Row :
            for Y in This.all.Dots.all'Range (2) loop
               This.all.Dots.all (X, Y).all.Finalize;
            end loop Finalize_Row;
         end loop Finalize_Column;

         Free (This.all.Dots);
      end if;

      This.all.Dots := new Dotted_Matrix_Array (1 .. Columns, 1 .. Rows);

      Setup_Dot_Matrix :
      declare
         Curvature : constant Glib.Gdouble := 5.0 * Col_Double;
      begin
         Column_Loop :
         for X in This.all.Dots.all'Range (1) loop
            Set_Column :
            declare
               X_Pos : constant Glib.Gdouble :=
                         (Glib.Gdouble (X) / (Col_Double + 1.0)) - 0.5;
            begin
               Row_Loop :
               for Y in This.all.Dots.all'Range (2) loop
                  This.all.State.all (X, Y) := False;

                  Set_Row :
                  declare
                     Y_Pos : constant Glib.Gdouble :=
                               ((Glib.Gdouble (Y) / (Row_Double + 1.0)) - 0.5) / This.all.Get_Aspect_Ratio;
                     E     : constant Cairo.Ellipses.Ellipse_Parameters :=
                               Cairo.Ellipses.Ellipse_Parameters'
                                 (Center          =>
                                    Cairo.Ellipses.Cairo_Tuple'(X => X_Pos,
                                                                Y => Y_Pos),
                                  Major_Curvature => Curvature,
                                  Minor_Radius    => 1.0 / Curvature,
                                  Angle           => 0.0);
                  begin
                     This.all.Dots.all (X, Y) :=
                       Gtk.Layered.Arc.Add_Arc
                         (Under         => This.all.Background.all.Get_Foreground,
                          Ellipse       => E,
                          Width         => 2.0 / Curvature,
                          Color         => Off_Color,
                          Scaled        => True,
                          Widened       => True);
                  end Set_Row;
               end loop Row_Loop;
            end Set_Column;
         end loop Column_Loop;
      end Setup_Dot_Matrix;

      This.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (This.all.Background.all.Get_Foreground);

      This.all.Update_State;
      This.all.Changed := False;
   end Initialize;

   overriding procedure Refresh
     (This    : not null access Gtk_Gauge_Dot_Matrix_Record;
      Context : in              Cairo.Cairo_Context) is
   begin
      if This.all.Changed then
         This.all.Changed := False;
         This.all.Update_State;
      end if;

      Gtk.Layered.Gtk_Layered_Record (This.all).Refresh (Context);
   end Refresh;

   procedure Set_Colors
     (This      : not null access Gtk_Gauge_Dot_Matrix_Record;
      On_Color  : in              Gdk.Color.Gdk_Color;
      Off_Color : in              Gdk.Color.Gdk_Color) is
   begin
      Lock.Set (DM  => This.all,
                On  => On_Color,
                Off => Off_Color);
   end Set_Colors;

   procedure Set_State (This   : not null access Gtk_Gauge_Dot_Matrix_Record;
                        Column : in              Col_Index;
                        Row    : in              Row_Index;
                        State  : in              Boolean) is
   begin
      if This.all.State.all (Column, Row) xor State then
         This.all.State.all (Column, Row) := State;
         This.all.Changed := True;
      end if;
   exception
      when Constraint_Error => null;
   end Set_State;

   procedure Update_State (This : not null access Gtk_Gauge_Dot_Matrix_Record)
   is
      On_Color  : constant Gdk.Color.Gdk_Color := Lock.On_Color (This.all);
      Off_Color : constant Gdk.Color.Gdk_Color := Lock.Off_Color (This.all);
      Value     : Glib.Values.GValue;
   begin
      Column_Loop :
      for Column in This.all.Dots.all'Range (1) loop
         Row_Loop :
         for Row in This.all.Dots.all'Range (2) loop
            Gdk.Color.Set_Value
              (Value => Value,
               Val   => (if This.all.State.all (Column, Row)
                         then On_Color
                         else Off_Color));
            This.all.Dots.all (Column, Row).all.Set_Property_Value
              (Property => 11, -- Property_Line_Color,
               Value    => Value);
         end loop Row_Loop;
      end loop Column_Loop;
   end Update_State;

end Gtk.Gauge.Dot_Matrix;
