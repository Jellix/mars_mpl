with Gdk.Color;
with Glib;
with Gtk.Box;
with Gtk.Gauge.Dot_Matrix;

package Gtk.Box.Digital_Clock is

   type Gtk_Box_Digital_Clock_Record is new Gtk_Box_Record with
     private;

   type Gtk_Box_Digital_Clock is access all Gtk_Box_Digital_Clock_Record'Class;

   function Gtk_New
     (Label     : in Glib.UTF8_String;
      BG_Color  : in Gdk.Color.Gdk_Color;
      On_Color  : in Gdk.Color.Gdk_Color;
      Off_Color : in Gdk.Color.Gdk_Color) return Gtk_Box_Digital_Clock;

   procedure Initialize (This      : in out Gtk_Box_Digital_Clock_Record;
                         Label     : in Glib.UTF8_String;
                         BG_Color  : in Gdk.Color.Gdk_Color;
                         On_Color  : in Gdk.Color.Gdk_Color;
                         Off_Color : in Gdk.Color.Gdk_Color);

   procedure Set_Time (This : in out Gtk_Box_Digital_Clock_Record;
                       Time : in     Duration);

private

   type    Digit_Index  is (H_1, M_10, M_1, S_10, S_1, S_10th); -- H:MM:SS.T
   subtype Valid_Digits is Natural range 0 .. 9;

   type Digits_Set is array (Digit_Index) of Valid_Digits;

   type LED_Assignment is
     array (Gtk.Gauge.Dot_Matrix.Col_Index range <>,
            Gtk.Gauge.Dot_Matrix.Row_Index range <>) of Boolean
     with Pack => True;

   X_Dimension : constant := 1;
   Y_Dimension : constant := 2;

   type Gtk_Box_Digital_Clock_Record is new Gtk_Box_Record with
      record
         Old_Digits : Digits_Set;
         LED_Matrix : Gtk.Gauge.Dot_Matrix.Gtk_Gauge_Dot_Matrix;
      end record;

   procedure Write_Digit (This  : in out Gtk_Box_Digital_Clock_Record;
                          Digit : in     Digit_Index;
                          Num   : in     Valid_Digits);

   type Offset_List is array (Digit_Index) of Gtk.Gauge.Dot_Matrix.Col_Count;

   -- \X 00000000001111111111222222222233333333334444444
   -- Y\ 01234567890123456789012345678901234567890123456
   -- 0  .....     ..... .....       ..... .....   .....
   -- 1  .....     ..... .....       ..... .....   .....
   -- 2  .....     ..... .....       ..... .....   .....
   -- 3  ..... o   ..... .....       ..... .....   .....
   -- 4  ..... oo  ..... ..... oo o  ..... .....   .....
   -- 5  ..... o o ..... ..... o o o ..... .....   .....
   -- 6  ..... o o ..... ..... o o o ..... ..... o .....
   subtype Matrix_Width  is Gtk.Gauge.Dot_Matrix.Col_Index range 1 .. 47;
   subtype Matrix_Height is Gtk.Gauge.Dot_Matrix.Row_Index range 1 ..  7;

   Digit_Offset : constant Offset_List := Offset_List'(H_1    => 0,
                                                       M_10   => 10,
                                                       M_1    => 16,
                                                       S_10   => 28,
                                                       S_1    => 34,
                                                       S_10th => 42);

end Gtk.Box.Digital_Clock;
