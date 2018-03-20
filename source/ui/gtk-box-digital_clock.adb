with Gtk.Enums;
with Gtk.Frame;

package body Gtk.Box.Digital_Clock is

   use type Glib.Gdouble;

   subtype Single_Digit_7_X_5 is LED_Assignment (0 .. 4, 0 .. 6);
   type Char_Def_String is
     new String (1 .. Single_Digit_7_X_5'Length (X_Dimension) * Single_Digit_7_X_5'Length (Y_Dimension));

   function "+" (S : in Char_Def_String) return Single_Digit_7_X_5;
   function "+" (S : in Char_Def_String) return Single_Digit_7_X_5 is
      Result : Single_Digit_7_X_5;
      X_Width : constant := Result'Length (X_Dimension);
   begin
      for Y in Result'Range (Y_Dimension) loop
         declare
            X_Offset : constant Glib.Gint := Glib.Gint (S'First) + Y * X_Width;
         begin
            for X in Result'Range (X_Dimension) loop
               Result (X, Y) := S (Positive (X_Offset + X)) /= ' ';
            end loop;
         end;
      end loop;

      return Result;
   end "+";

   LED_0 : constant Single_Digit_7_X_5 := +(" XXX " &
                                            "X   X" &
                                            "X   X" &
                                            "X   X" &
                                            "X   X" &
                                            "X   X" &
                                            " XXX ");

   LED_1 : constant Single_Digit_7_X_5 := +("  X  " &
                                            " XX  " &
                                            "  X  " &
                                            "  X  " &
                                            "  X  " &
                                            "  X  " &
                                            " XXX ");

   LED_2 : constant Single_Digit_7_X_5 := +(" XXX " &
                                            "X   X" &
                                            "    X" &
                                            "  XX " &
                                            " X   " &
                                            "X    " &
                                            "XXXXX");

   LED_3 : constant Single_Digit_7_X_5 := +(" XXX " &
                                            "X   X" &
                                            "    X" &
                                            "  XX " &
                                            "    X" &
                                            "X   X" &
                                            " XXX ");

   LED_4 : constant Single_Digit_7_X_5 := +("   X " &
                                            "  XX " &
                                            " X X " &
                                            "X  X " &
                                            "XXXXX" &
                                            "   X " &
                                            "   X ");

   LED_5 : constant Single_Digit_7_X_5 := +("XXXXX" &
                                            "X    " &
                                            "XXXX " &
                                            "    X" &
                                            "    X" &
                                            "X   X" &
                                            " XXX ");

   LED_6 : constant Single_Digit_7_X_5 := +("  XX " &
                                            " X   " &
                                            "X    " &
                                            "XXXX " &
                                            "X   X" &
                                            "X   X" &
                                            " XXX ");

   LED_7 : constant Single_Digit_7_X_5 := +("XXXXX" &
                                            "    X" &
                                            "   X " &
                                            "  X  " &
                                            " X   " &
                                            " X   " &
                                            " X   ");

   LED_8 : constant Single_Digit_7_X_5 := +(" XXX " &
                                            "X   X" &
                                            "X   X" &
                                            " XXX " &
                                            "X   X" &
                                            "X   X" &
                                            " XXX ");

   LED_9 : constant Single_Digit_7_X_5 := +(" XXX " &
                                            "X   X" &
                                            "X   X" &
                                            " XXXX" &
                                            "    X" &
                                            "   X " &
                                            " XX  ");

   type LED_Lookup is array (Valid_Digits) of Single_Digit_7_X_5;

   Digit_Lookup : constant LED_Lookup := (0 => LED_0,
                                          1 => LED_1,
                                          2 => LED_2,
                                          3 => LED_3,
                                          4 => LED_4,
                                          5 => LED_5,
                                          6 => LED_6,
                                          7 => LED_7,
                                          8 => LED_8,
                                          9 => LED_9);

   function Gtk_New
     (Label     : in Glib.UTF8_String;
      On_Color  : in Gdk.Color.Gdk_Color;
      Off_Color : in Gdk.Color.Gdk_Color) return Gtk_Box_Digital_Clock
   is
      The_Clock : constant Gtk_Box_Digital_Clock :=
                    new Gtk_Box_Digital_Clock_Record;
   begin
      Initialize (This      => The_Clock.all,
                  Label     => Label,
                  On_Color  => On_Color,
                  Off_Color => Off_Color);
      return The_Clock;
   end Gtk_New;

   procedure Initialize (This      : in out Gtk_Box_Digital_Clock_Record;
                         Label     : in Glib.UTF8_String;
                         On_Color  : in Gdk.Color.Gdk_Color;
                         Off_Color : in Gdk.Color.Gdk_Color)
   is
      Clk_Frame : constant Gtk.Frame.Gtk_Frame   :=
                    Gtk.Frame.Gtk_Frame_New (Label => Label);
      The_Grid  : constant Gtk.Grid.Gtk_Grid     := Gtk.Grid.Gtk_Grid_New;
   begin
      Gtk.Box.Initialize (Box         => This'Access,
                          Orientation => Gtk.Enums.Orientation_Vertical,
                          Spacing     => 0);
      This.Grid := The_Grid;

      This.Pack_Start (Child => Clk_Frame,
                       Expand => False);
      This.Pack_End (Child  => Gtk_Hbox_New,
                     Expand => True);
      Clk_Frame.all.Add (Widget => The_Grid);
      The_Grid.all.Set_Size_Request
        (Width  => This.LED_Matrix'Length (X_Dimension) * 5,
         Height => This.LED_Matrix'Length (Y_Dimension) * 5);
      The_Grid.all.Set_Column_Homogeneous (Homogeneous => True);
      The_Grid.all.Set_Column_Spacing (Spacing => 0);
      The_Grid.all.Set_Row_Homogeneous (Homogeneous => True);
      The_Grid.all.Set_Row_Spacing (Spacing => 0);

      for X in This.LED_Matrix'Range (X_Dimension) loop
         The_Grid.all.Insert_Column (Position => X);
      end loop;

      for Y in This.LED_Matrix'Range (Y_Dimension) loop
         The_Grid.all.Insert_Row (Position => Y);
      end loop;

      for X in This.LED_Matrix'Range (X_Dimension) loop
         for Y in This.LED_Matrix'Range (Y_Dimension) loop
            This.LED_Matrix (X, Y) := New_LED (On_Color  => On_Color,
                                               Off_Color => Off_Color);
            The_Grid.all.Attach (Child => This.LED_Matrix (X, Y),
                                 Left  => X,
                                 Top   => Y);
         end loop;
      end loop;

      This.LED_Matrix (12, 2).all.Set_State (State => True);
      This.LED_Matrix (12, 4).all.Set_State (State => True);
      This.LED_Matrix (26, 6).all.Set_State (State => True);

      --  Force digits to all zero, so we have a defined state.
      for Digit in Digit_Index'Range loop
         This.Write_Digit (Digit => Digit,
                           Num   => 0,
                           Force => True);
      end loop;
   end Initialize;

   function New_LED (On_Color  : in Gdk.Color.Gdk_Color;
                     Off_Color : in Gdk.Color.Gdk_Color) return not null
     Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
   is
      LED : Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;
   begin
      Gtk.Gauge.LED_Rectangular.Gtk_New (Widget    => LED,
                                         On_Color  => On_Color,
                                         Off_Color => Off_Color);
      return LED;
   end New_LED;

   procedure Set_Time (This : in out Gtk_Box_Digital_Clock_Record;
                       Time : in     Duration)
   is
      Temp       : constant Glib.Gdouble := Glib.Gdouble (Time);
      The_Digits : constant array (Digit_Index) of Valid_Digits :=
                     (Natural (Glib.Gdouble'Floor (Temp / 600.0)) mod  6,
                      Natural (Glib.Gdouble'Floor (Temp /  60.0)) mod 10,
                      Natural (Glib.Gdouble'Floor (Temp /  10.0)) mod  6,
                      Natural (Glib.Gdouble'Floor (Temp /   1.0)) mod 10,
                      Natural (Glib.Gdouble'Floor (Temp *  10.0)) mod 10);
   begin
      for Digit in Digit_Index'Range loop
         This.Write_Digit (Digit => Digit,
                           Num   => The_Digits (Digit));
      end loop;
   end Set_Time;

   procedure Write_Digit (This  : in out Gtk_Box_Digital_Clock_Record;
                          Digit : in     Digit_Index;
                          Num   : in     Valid_Digits;
                          Force : in     Boolean := False)
   is
      Old_Layout   : Single_Digit_7_X_5 renames
                       Digit_Lookup (This.Old_Digits (Digit));
      Digit_Layout : Single_Digit_7_X_5 renames
                       Digit_Lookup (Num);
      use type Glib.Gint;
   begin
      --  Speed optimization 1: Only change LED state if the digit to be shown
      --  is different from the previously displayed one.
      if Force or else This.Old_Digits (Digit) /= Num then
         for X in Digit_Layout'Range (X_Dimension) loop
            Set_Column :
            declare
               Column : constant Glib.Gint := Digit_Offset (Digit) + X;
            begin
               for Y in Digit_Layout'Range (Y_Dimension) loop
                  --  Speed optimization 2: Only change state of the LED if it's
                  --  different from the previously shown digit.
                  if Force or else Old_Layout (X, Y) /= Digit_Layout (X, Y) then
                     This.LED_Matrix (Column, Y).all.Set_State
                       (State => Digit_Layout (X, Y));
                  end if;
               end loop;
            end Set_Column;
         end loop;

         This.Old_Digits (Digit) := Num;
      end if;
   end Write_Digit;

end Gtk.Box.Digital_Clock;
