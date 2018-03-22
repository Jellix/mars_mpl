with Cairo.Ellipses;
with Gdk.Color.IHLS;
with Glib.Object.Checked_Destroy;

package body Gtk.Gauge.Dot_Matrix is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   use type Gdk.Color.IHLS.Gdk_Luminance;

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

   function Get_State (This : not null access Gtk_Gauge_Dot_Matrix_Record)
                       return Boolean
   is
   begin
      return This.all.State;
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
      On_Color      : in     Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (0.0, 1.0, 0.0);
      Off_Color     : in     Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (0.5, 0.5, 0.5);
      Border_Shadow : in     Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_In) is
   begin
      This := new Gtk_Gauge_Dot_Matrix_Record;
      Initialize (This, On_Color, Off_Color, Border_Shadow);
   exception
      when others =>
         Glib.Object.Checked_Destroy (This);
         This := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (This          : not null access Gtk_Gauge_Dot_Matrix_Record'Class;
      On_Color      : in              Gdk.Color.Gdk_Color;
      Off_Color     : in              Gdk.Color.Gdk_Color;
      Border_Shadow : in              Gtk.Enums.Gtk_Shadow_Type) is
   begin
      G_New (This, Get_Type);
      Gtk.Layered.Initialize (This);
      Lock.Set (This.all, On_Color, Off_Color);
      case Border_Shadow is
         when Gtk.Enums.Shadow_None =>
            This.all.Background :=
              Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
                (Under         => This,
                 Height        => 1.0,
                 Width         => 1.0,
                 Center        => (0.0, 0.0),
                 Corner_Radius => Corner,
                 Color         => Off_Color,
                 Border_Width  => 0.0,
                 Border_Depth  => 0.0,
                 Border_Shadow => Border_Shadow,
                 Deepened      => True,
                 Widened       => True,
                 Scaled        => True);
         when Gtk.Enums.Shadow_Etched_In | Gtk.Enums.Shadow_Etched_Out =>
            This.all.Background :=
              Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
                (Under         => This,
                 Height        => 1.0,
                 Width         => 1.0,
                 Center        => (0.0, 0.0),
                 Corner_Radius => Corner,
                 Color         => Off_Color,
                 Border_Width  => 0.025,
                 Border_Depth  => 0.075,
                 Border_Shadow => Border_Shadow,
                 Deepened      => True,
                 Widened       => True,
                 Scaled        => True);
         when Gtk.Enums.Shadow_In | Gtk.Enums.Shadow_Out =>
            This.all.Background :=
              Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
                (Under         => This,
                 Height        => 1.0,
                 Width         => 1.0,
                 Center        => (0.0, 0.0),
                 Corner_Radius => Corner,
                 Color         => Off_Color,
                 Border_Width  => 0.05,
                 Border_Depth  => 0.15,
                 Border_Shadow => Border_Shadow,
                 Deepened      => True,
                 Widened       => True,
                 Scaled        => True);
      end case;

      pragma Compile_Time_Warning (True, "Ellipse parameters here.");
      This.all.Dot :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => This,
           Outer         =>
             Cairo.Ellipses.Ellipse_Parameters'(Center          => (0.0, 0.0),
                                                Major_Curvature => 5.0,
                                                Minor_Radius    => 0.2,
                                                Angle           => 0.0),
           Center        => Cairo.Ellipses.Cairo_Tuple'(X => 0.5,
                                                        Y => 0.5),
           Color         => On_Color,
           Border_Width  => 0.05,
           Border_Depth  => 0.15,
           Border_Shadow => Border_Shadow,
           Deepened      => False,
           Scaled        => True,
           Widened       => False);

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

   procedure Set_State
     (This  : not null access Gtk_Gauge_Dot_Matrix_Record;
      State : in              Boolean) is
   begin
      if This.all.State /= State then
         This.all.State   := State;
         This.all.Toggled := True;
      end if;
   end Set_State;

   procedure Update_State (This : not null access Gtk_Gauge_Dot_Matrix_Record)
   is
      Color : Gdk.Color.Gdk_Color;
   begin
      if This.all.State then
         Color := Lock.On_Color (This.all);
      else
         Color := Lock.Off_Color (This.all);
      end if;

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

      This.all.Dot.all.Set
        (Outer          => This.all.Dot.all.Get_Outer,
         Inner          => This.all.Dot.all.Get_Inner,
         From           => This.all.Dot.all.Get_From,
         Length         => This.all.Dot.all.Get_Length,
         Color          => Color,
         Border_Width   => This.all.Dot.all.Get_Border_Width,
         Border_Depth   => This.all.Dot.all.Get_Border_Depth,
         Border_Color   => This.all.Dot.all.Get_Border_Color,
         Border_Shadow  => This.all.Dot.all.Get_Border_Shadow);
   end Update_State;

end Gtk.Gauge.Dot_Matrix;
