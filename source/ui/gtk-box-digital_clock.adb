with Gtk.Enums;
with Gtk.Frame;
with Gtk.Gauge.LED_Rectangular;

package body Gtk.Box.Digital_Clock is

   use type Glib.Gdouble;

   function Gtk_New
     (Label     : in Glib.UTF8_String;
      On_Color  : in Gdk.Color.Gdk_Color;
      Off_Color : in Gdk.Color.Gdk_Color) return Gtk_Box_Digital_Clock
   is
      Clk_Box   : constant Gtk_Box_Digital_Clock :=
                    new Gtk_Box_Digital_Clock_Record;
      Clk_Frame : constant Gtk.Frame.Gtk_Frame   :=
                    Gtk.Frame.Gtk_Frame_New (Label => Label);
      The_Grid  : constant Gtk.Grid.Gtk_Grid     := Gtk.Grid.Gtk_Grid_New;
   begin
      Initialize (Box         => Clk_Box,
                  Orientation => Gtk.Enums.Orientation_Vertical,
                  Spacing     => 0);
      Clk_Box.all.Grid := The_Grid;

      Clk_Box.all.Pack_Start (Child => Clk_Frame,
                              Expand => False);
      Clk_Box.all.Pack_End (Child  => Gtk_Hbox_New,
                            Expand => True);
      Clk_Frame.all.Add (Widget => The_Grid);
      The_Grid.all.Set_Size_Request (165, 35);
      The_Grid.all.Set_Column_Homogeneous (Homogeneous => True);
      The_Grid.all.Set_Column_Spacing (Spacing => 0);
      The_Grid.all.Set_Row_Homogeneous (Homogeneous => True);
      The_Grid.all.Set_Row_Spacing (Spacing => 0);

      for X in Glib.Gint range 0 .. 32 loop
         The_Grid.all.Insert_Row (Position => X);
      end loop;

      for Y in Glib.Gint range 0 .. 6 loop
         The_Grid.all.Insert_Column (Position => Y);
      end loop;

      for X in Glib.Gint range 0 .. 32 loop
         for Y in Glib.Gint range 0 .. 6 loop
            The_Grid.all.Attach (Child => New_LED (On_Color  => On_Color,
                                                   Off_Color => Off_Color),
                                 Left  => X,
                                 Top   => Y);
         end loop;
      end loop;

      Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
        (The_Grid.all.Get_Child_At (12, 2)).all.Set_State (State => True);
      Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
        (The_Grid.all.Get_Child_At (12, 4)).all.Set_State (State => True);
      Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
        (The_Grid.all.Get_Child_At (26, 6)).all.Set_State (State => True);

      Clk_Box.all.Set_Time (0.0);
      return Clk_Box;
   end Gtk_New;

   function New_LED (On_Color  : in Gdk.Color.Gdk_Color;
                     Off_Color : in Gdk.Color.Gdk_Color) return not null
     Gtk.Widget.Gtk_Widget
   is
      LED : Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;
   begin
      Gtk.Gauge.LED_Rectangular.Gtk_New (Widget    => LED,
                                         On_Color  => On_Color,
                                         Off_Color => Off_Color);
      return Gtk.Widget.Gtk_Widget (LED);
   end New_LED;

   procedure Set_Time (Clock : in Gtk_Box_Digital_Clock_Record;
                       Time  : in Duration)
   is
      The_Digits : array (Digit_Index) of Valid_Digits;
      Temp       : Glib.Gdouble := Glib.Gdouble (Time);
   begin
      The_Digits (1) := Natural (Glib.Gdouble'Floor (Temp / 600.0));
      Temp := Temp - 600.0 * Glib.Gdouble (The_Digits (1));

      The_Digits (2) := Natural (Glib.Gdouble'Floor (Temp / 60.0));
      Temp := Temp - 60.0 * Glib.Gdouble (The_Digits (2));

      The_Digits (3) := Natural (Glib.Gdouble'Floor (Temp / 10.0));
      Temp := Temp - 10.0 * Glib.Gdouble (The_Digits (3));

      The_Digits (4) := Natural (Glib.Gdouble'Floor (Temp / 1.0));
      Temp := Temp - 1.0 * Glib.Gdouble (The_Digits (4));

      The_Digits (5) := Natural (Glib.Gdouble'Floor (Temp * 10.0));

      for Digit in Digit_Index'Range loop
         Clock.Write_Digit (Digit => Digit,
                            Num   => The_Digits (Digit));
      end loop;
   end Set_Time;

   procedure Write_Digit (This  : in Gtk_Box_Digital_Clock_Record;
                          Digit : in Digit_Index;
                          Num   : in Valid_Digits)
   is
      Digit_Layout : Single_Digit_7_X_5 renames Digit_Lookup (Num);
      use type Glib.Gint;
   begin
      for X in Digit_Layout'Range (2) loop
         for Y in Digit_Layout'Range (1) loop
            Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
              (This.Grid.all.Get_Child_At (Left => Digit_Offset (Digit) + X,
                                           Top  => Y)).all.
              Set_State (State => Digit_Layout (Y, X));
         end loop;
      end loop;
   end Write_Digit;

end Gtk.Box.Digital_Clock;
