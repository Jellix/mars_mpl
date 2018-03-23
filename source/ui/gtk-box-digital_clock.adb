with Gtk.Enums;
with Gtk.Frame;

package body Gtk.Box.Digital_Clock is

   use type Glib.Gdouble;
   use type Gtk.Gauge.Dot_Matrix.Col_Index;
   use type Gtk.Gauge.Dot_Matrix.Row_Index;

   Char_Width  : constant := 5;
   Char_Height : constant := 7;

   subtype Char_Width_Index is
     Gtk.Gauge.Dot_Matrix.Col_Index range 1 .. Char_Width;

   subtype Char_Height_Index is
     Gtk.Gauge.Dot_Matrix.Row_Index range 1 .. Char_Height;

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
            X_Offset : constant Positive :=
                         S'First +
                           Natural ((Y - Result'First (Y_Dimension)) * X_Width);
         begin
            Row_Loop :
            for X in Result'Range (X_Dimension) loop
               Result (X, Y) :=
                 S (X_Offset + Natural (X - Result'First (X_Dimension))) /= ' ';
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
   begin
      Gtk.Box.Initialize (Box         => This'Access,
                          Orientation => Gtk.Enums.Orientation_Vertical,
                          Spacing     => 0);
      Gtk.Gauge.Dot_Matrix.Gtk_New (This      => This.LED_Matrix,
                                    Columns   => Matrix_Width'Last,
                                    Rows      => Matrix_Height'Last,
                                    On_Color  => On_Color,
                                    Off_Color => Off_Color);

      This.LED_Matrix.all.Set_Size_Request
        (Width  => Glib.Gint (Matrix_Width'Last * 5),
         Height => Glib.Gint (Matrix_Height'Last * 5));

      This.Pack_Start (Child  => Clk_Frame,
                       Expand => True);
      Clk_Frame.all.Add (Widget => This.LED_Matrix);

      --  Colon between hour and minutes.
      This.LED_Matrix.all.Set_State (Column => 1 * Char_Width + 1,
                                     Row    => 2,
                                     State  => True);
      This.LED_Matrix.all.Set_State (Column => 1 * Char_Width + 1,
                                     Row    => 4,
                                     State  => True);

      --  Colon between minutes and seconds.
      This.LED_Matrix.all.Set_State (Column => 3 * Char_Width + 5,
                                     Row    => 2,
                                     State  => True);
      This.LED_Matrix.all.Set_State (Column => 3 * Char_Width + 5,
                                     Row    => 4,
                                     State  => True);

      --  Decimal point between seconds and tenth seconds.
      This.LED_Matrix.all.Set_State (Column => 5 * Char_Width + 9,
                                     Row    => 6,
                                     State  => True);

      --  Force digits to all zero, so we have a defined state.
      for Digit in Digit_Index'Range loop
         This.Old_Digits (Digit) := 9; --  Preset a different digit than the one
                                       --  we're writing to defeat the draw
                                       --  optimization.
         This.Write_Digit (Digit => Digit,
                           Num   => 0);
      end loop;
   end Initialize;

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
               Column : constant Gtk.Gauge.Dot_Matrix.Col_Index :=
                          Digit_Offset (Digit) + X;
            begin
               Row_Loop :
               for Y in Digit_Layout'Range (Y_Dimension) loop
                  --  No speed optmization here. LED.Set_State already takes
                  --  care of state changes. It is believed (*not* measured)
                  --  that additional, prior checks only slow us down.
                  This.LED_Matrix.all.Set_State (Column => Column,
                                                 Row    => Y,
                                                 State  => Digit_Layout (X, Y));
               end loop Row_Loop;
            end Set_Column;
         end loop Column_Loop;

         This.Old_Digits (Digit) := Num;
      end if;
   end Write_Digit;

end Gtk.Box.Digital_Clock;
