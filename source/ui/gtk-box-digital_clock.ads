with Gdk.Color;
with Glib;
with Gtk.Box;
with Gtk.Gauge.LED_Rectangular;
with Gtk.Grid;

package Gtk.Box.Digital_Clock is

   type Gtk_Box_Digital_Clock_Record is new Gtk_Box_Record with
     private;

   type Gtk_Box_Digital_Clock is access all Gtk_Box_Digital_Clock_Record'Class;

   function Gtk_New
     (Label     : in Glib.UTF8_String;
      On_Color  : in Gdk.Color.Gdk_Color;
      Off_Color : in Gdk.Color.Gdk_Color) return Gtk_Box_Digital_Clock;

   procedure Initialize (This      : in out Gtk_Box_Digital_Clock_Record;
                         Label     : in Glib.UTF8_String;
                         On_Color  : in Gdk.Color.Gdk_Color;
                         Off_Color : in Gdk.Color.Gdk_Color);

   procedure Set_Time (This : in out Gtk_Box_Digital_Clock_Record;
                       Time : in     Duration);

private

   type    Digit_Index  is (H_1, M_10, M_1, S_10, S_1, S_10th); -- H:MM:SS.T
   subtype Valid_Digits is Natural range 0 .. 9;

   type Digits_Set is array (Digit_Index) of Valid_Digits;

   type LED_Assignment is array (Natural range <>,
                                 Natural range <>) of Boolean
     with Pack => True;
   X_Dimension : constant := 1;
   Y_Dimension : constant := 2;

   subtype Matrix_Width  is Natural range 0 .. 40;
   subtype Matrix_Height is Natural range 0 .. 6;

   type Dot_Matrix is array (Natural range <>,
                             Natural range <>) of
     Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;

   type Gtk_Box_Digital_Clock_Record is new Gtk_Box_Record with
      record
         Grid       : Gtk.Grid.Gtk_Grid;
         Old_Digits : Digits_Set;
         LED_Matrix : Dot_Matrix (Matrix_Width'Range,
                                  Matrix_Height'Range);
      end record;

   procedure Write_Digit (This  : in out Gtk_Box_Digital_Clock_Record;
                          Digit : in     Digit_Index;
                          Num   : in     Valid_Digits);

   function New_LED (On_Color  : in Gdk.Color.Gdk_Color;
                     Off_Color : in Gdk.Color.Gdk_Color;
                     Size      : in Glib.Gint) return not null
     Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;

   type Offset_List is array (Digit_Index) of Natural;

   -- 0         1         2         3         4
   -- 01234567890123456789012345678901234567890
   -- .....   ..... .....   ..... .....   .....
   -- .....   ..... .....   ..... .....   .....
   -- ..... . ..... ..... . ..... .....   .....
   -- .....   ..... .....   ..... .....   .....
   -- ..... . ..... ..... . ..... .....   .....
   -- .....   ..... .....   ..... .....   .....
   -- .....   ..... .....   ..... ..... . .....
   Digit_Offset : constant Offset_List := Offset_List'(H_1    => 0,
                                                       M_10   => 8,
                                                       M_1    => 14,
                                                       S_10   => 22,
                                                       S_1    => 28,
                                                       S_10th => 36);

end Gtk.Box.Digital_Clock;
