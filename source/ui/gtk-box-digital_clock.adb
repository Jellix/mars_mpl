with Gtk.Enums;
with Gtk.Frame;

package body Gtk.Box.Digital_Clock is

   use type Glib.Gdouble;

   Char_Width  : constant := 5;
   Char_Height : constant := 7;

   subtype Char_Width_Index  is Natural range 0 .. Char_Width  - 1;
   subtype Char_Height_Index is Natural range 0 .. Char_Height - 1;

   subtype Char_7_X_5 is LED_Assignment (Char_Width_Index'Range,
                                         Char_Height_Index'Range);
   type Char_Def_String is array (1 .. Char_Width * Char_Height) of Character;

   function "+" (S : in Char_Def_String) return Char_7_X_5;
   function "+" (S : in Char_Def_String) return Char_7_X_5
   is
      Result  : Char_7_X_5;
      X_Width : constant := Result'Length (X_Dimension);
   begin
      Column_Loop :
      for Y in Result'Range (Y_Dimension) loop
         Convert_Row :
         declare
            X_Offset : constant Positive := S'First + Y * X_Width;
         begin
            Row_Loop :
            for X in Result'Range (X_Dimension) loop
               Result (X, Y) := S (X_Offset + X) /= ' ';
            end loop Row_Loop;
         end Convert_Row;
      end loop Column_Loop;

      return Result;
   end "+";

   LED_0 : constant Char_7_X_5 := +(" XXX " &
                                    "X   X" &
                                    "X   X" &
                                    "X   X" &
                                    "X   X" &
                                    "X   X" &
                                    " XXX ");

   LED_1 : constant Char_7_X_5 := +("  X  " &
                                    " XX  " &
                                    "  X  " &
                                    "  X  " &
                                    "  X  " &
                                    "  X  " &
                                    " XXX ");

   LED_2 : constant Char_7_X_5 := +(" XXX " &
                                    "X   X" &
                                    "    X" &
                                    "  XX " &
                                    " X   " &
                                    "X    " &
                                    "XXXXX");

   LED_3 : constant Char_7_X_5 := +(" XXX " &
                                    "X   X" &
                                    "    X" &
                                    "  XX " &
                                    "    X" &
                                    "X   X" &
                                    " XXX ");

   LED_4 : constant Char_7_X_5 := +("   X " &
                                    "  XX " &
                                    " X X " &
                                    "X  X " &
                                    "XXXXX" &
                                    "   X " &
                                    "   X ");

   LED_5 : constant Char_7_X_5 := +("XXXXX" &
                                    "X    " &
                                    "XXXX " &
                                    "    X" &
                                    "    X" &
                                    "X   X" &
                                    " XXX ");

   LED_6 : constant Char_7_X_5 := +("  XX " &
                                    " X   " &
                                    "X    " &
                                    "XXXX " &
                                    "X   X" &
                                    "X   X" &
                                    " XXX ");

   LED_7 : constant Char_7_X_5 := +("XXXXX" &
                                    "    X" &
                                    "   X " &
                                    "  X  " &
                                    " X   " &
                                    " X   " &
                                    " X   ");

   LED_8 : constant Char_7_X_5 := +(" XXX " &
                                    "X   X" &
                                    "X   X" &
                                    " XXX " &
                                    "X   X" &
                                    "X   X" &
                                    " XXX ");

   LED_9 : constant Char_7_X_5 := +(" XXX " &
                                    "X   X" &
                                    "X   X" &
                                    " XXXX" &
                                    "    X" &
                                    "   X " &
                                    " XX  ");

   type LED_Lookup is array (Valid_Digits) of Char_7_X_5;

   Digit_Lookup : constant LED_Lookup := LED_Lookup'(0 => LED_0,
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
      The_Grid.all.Set_Column_Homogeneous (Homogeneous => False);
      The_Grid.all.Set_Column_Spacing (Spacing => 0);
      The_Grid.all.Set_Row_Homogeneous (Homogeneous => False);
      The_Grid.all.Set_Row_Spacing (Spacing => 0);

      for X in This.LED_Matrix'Range (X_Dimension) loop
         The_Grid.all.Insert_Column (Position => Glib.Gint (X));
      end loop;

      for Y in This.LED_Matrix'Range (Y_Dimension) loop
         The_Grid.all.Insert_Row (Position => Glib.Gint (Y));
      end loop;

      Column_Loop :
      for X in This.LED_Matrix'Range (X_Dimension) loop
         Row_Loop :
         for Y in This.LED_Matrix'Range (Y_Dimension) loop
            This.LED_Matrix (X, Y) := New_LED (On_Color  => On_Color,
                                               Off_Color => Off_Color,
                                               Size      => 7);
            The_Grid.all.Attach (Child => This.LED_Matrix (X, Y),
                                 Left  => Glib.Gint (X),
                                 Top   => Glib.Gint (Y));
         end loop Row_Loop;
      end loop Column_Loop;

      --  Colon between hour and minutes.
      This.LED_Matrix (1 * Char_Width + 1, 2).all.Set_State (State => True);
      This.LED_Matrix (1 * Char_Width + 1, 4).all.Set_State (State => True);

      --  Colon between minutes and seconds.
      This.LED_Matrix (3 * Char_Width + 5, 2).all.Set_State (State => True);
      This.LED_Matrix (3 * Char_Width + 5, 4).all.Set_State (State => True);

      --  Decimal point between seconds and tenth seconds.
      This.LED_Matrix (5 * Char_Width + 9, 6).all.Set_State (State => True);

      --  Force digits to all zero, so we have a defined state.
      for Digit in Digit_Index'Range loop
         This.Old_Digits (Digit) := 9; --  Preset a different digit than the one
                                       --  we're writing to defeat the draw
                                       --  optimization.
         This.Write_Digit (Digit => Digit,
                           Num   => 0);
      end loop;
   end Initialize;

   function New_LED (On_Color  : in Gdk.Color.Gdk_Color;
                     Off_Color : in Gdk.Color.Gdk_Color;
                     Size      : in Glib.Gint) return not null
     Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
   is
      LED : Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;
   begin
      Gtk.Gauge.LED_Rectangular.Gtk_New (Widget    => LED,
                                         On_Color  => On_Color,
                                         Off_Color => Off_Color);
      LED.all.Set_Size_Request (Width  => Size,
                                Height => Size);

      return LED;
   end New_LED;

   procedure Set_Time (This : in out Gtk_Box_Digital_Clock_Record;
                       Time : in     Duration)
   is
      Temp       : constant Glib.Gdouble := Glib.Gdouble (Time);
      The_Digits : constant Digits_Set
        := Digits_Set'
             (H_1    => Natural (Glib.Gdouble'Floor (Temp / 3600.0)) mod 10,
              M_10   => Natural (Glib.Gdouble'Floor (Temp /  600.0)) mod  6,
              M_1    => Natural (Glib.Gdouble'Floor (Temp /   60.0)) mod 10,
              S_10   => Natural (Glib.Gdouble'Floor (Temp /   10.0)) mod  6,
              S_1    => Natural (Glib.Gdouble'Floor (Temp /    1.0)) mod 10,
              S_10th => Natural (Glib.Gdouble'Floor (Temp *   10.0)) mod 10);
   begin
      for Digit in Digit_Index'Range loop
         This.Write_Digit (Digit => Digit,
                           Num   => The_Digits (Digit));
      end loop;
   end Set_Time;

   procedure Write_Digit (This  : in out Gtk_Box_Digital_Clock_Record;
                          Digit : in     Digit_Index;
                          Num   : in     Valid_Digits)
   is
      Digit_Layout : Char_7_X_5 renames Digit_Lookup (Num);
      use type Glib.Gint;
   begin
      --  Speed optimization 1: Only change LED state if the digit to be shown
      --  is different from the previously displayed one.
      if This.Old_Digits (Digit) /= Num then
         Column_Loop :
         for X in Digit_Layout'Range (X_Dimension) loop
            Set_Column :
            declare
               Column : constant Natural := Digit_Offset (Digit) + X;
            begin
               Row_Loop :
               for Y in Digit_Layout'Range (Y_Dimension) loop
                  --  No speed optmization here. LED.Set_State already takes
                  --  care of state changes. It is believed (*not* measured)
                  --  that additional, prior checks only slow us down.
                  This.LED_Matrix (Column, Y).all.Set_State
                    (State => Digit_Layout (X, Y));
               end loop Row_Loop;
            end Set_Column;
         end loop Column_Loop;

         This.Old_Digits (Digit) := Num;
      end if;
   end Write_Digit;

end Gtk.Box.Digital_Clock;
