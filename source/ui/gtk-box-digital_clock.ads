with Gdk.Color;
with Glib;
with Gtk.Box;
with Gtk.Grid;

package Gtk.Box.Digital_Clock is

   type Gtk_Box_Digital_Clock_Record is new Gtk_Box_Record with
     private;

   type Gtk_Box_Digital_Clock is access all Gtk_Box_Digital_Clock_Record'Class;

   function Gtk_New
     (Label     : in Glib.UTF8_String;
      On_Color  : in Gdk.Color.Gdk_Color;
      Off_Color : in Gdk.Color.Gdk_Color) return Gtk_Box_Digital_Clock;

   procedure Set_Time (Clock : in Gtk_Box_Digital_Clock_Record;
                       Time  : in Duration);

private

   type Digit_Index is range 1 .. 5;
   subtype Valid_Digits is Natural range 0 .. 9;

   type LED_Assignment is array (Glib.Gint range <>,
                                 Glib.Gint range <>) of Boolean
     with Pack => True;

   type Gtk_Box_Digital_Clock_Record is new Gtk_Box_Record with
      record
         Grid : Gtk.Grid.Gtk_Grid;
      end record;

   procedure Write_Digit (This  : in Gtk_Box_Digital_Clock_Record;
                          Digit : in Digit_Index;
                          Num   : in Valid_Digits);

   function New_LED (On_Color  : in Gdk.Color.Gdk_Color;
                     Off_Color : in Gdk.Color.Gdk_Color) return not null
     Gtk.Widget.Gtk_Widget;

   X : constant Boolean := True;
   O : constant Boolean := False;

   subtype Single_Digit_7_X_5 is LED_Assignment (0 .. 6, 0 .. 4);

   LED_0 : constant Single_Digit_7_X_5 := ((O, X, X, X, O),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (O, X, X, X, O));

   LED_1 : constant Single_Digit_7_X_5 := ((O, O, X, O, O),
                                           (O, X, X, O, O),
                                           (O, O, X, O, O),
                                           (O, O, X, O, O),
                                           (O, O, X, O, O),
                                           (O, O, X, O, O),
                                           (O, X, X, X, O));

   LED_2 : constant Single_Digit_7_X_5 := ((O, X, X, X, O),
                                           (X, O, O, O, X),
                                           (O, O, O, O, X),
                                           (O, O, X, X, O),
                                           (O, X, O, O, O),
                                           (X, O, O, O, O),
                                           (X, X, X, X, X));

   LED_3 : constant Single_Digit_7_X_5 := ((O, X, X, X, O),
                                           (X, O, O, O, X),
                                           (O, O, O, O, X),
                                           (O, X, X, X, O),
                                           (O, O, O, O, X),
                                           (X, O, O, O, X),
                                           (O, X, X, X, O));

   LED_4 : constant Single_Digit_7_X_5 := ((O, O, O, X, O),
                                           (O, O, X, X, O),
                                           (O, X, O, X, O),
                                           (X, O, O, X, O),
                                           (X, X, X, X, X),
                                           (O, O, O, X, O),
                                           (O, O, O, X, O));

   LED_5 : constant Single_Digit_7_X_5 := ((X, X, X, X, X),
                                           (X, O, O, O, O),
                                           (X, X, X, X, O),
                                           (O, O, O, O, X),
                                           (O, O, O, O, X),
                                           (X, O, O, O, X),
                                           (O, X, X, X, O));

   LED_6 : constant Single_Digit_7_X_5 := ((O, O, X, X, O),
                                           (O, X, O, O, O),
                                           (X, O, O, O, O),
                                           (X, X, X, X, O),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (O, X, X, X, O));

   LED_7 : constant Single_Digit_7_X_5 := ((X, X, X, X, X),
                                           (O, O, O, O, X),
                                           (O, O, O, X, O),
                                           (O, O, X, O, O),
                                           (O, X, O, O, O),
                                           (O, X, O, O, O),
                                           (O, X, O, O, O));

   LED_8 : constant Single_Digit_7_X_5 := ((O, X, X, X, O),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (O, X, X, X, O),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (O, X, X, X, O));

   LED_9 : constant Single_Digit_7_X_5 := ((O, X, X, X, O),
                                           (X, O, O, O, X),
                                           (X, O, O, O, X),
                                           (O, X, X, X, X),
                                           (O, O, O, O, X),
                                           (O, O, O, X, O),
                                           (O, X, X, O, O));

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

   Digit_Offset : constant array (Digit_Index) of Glib.Gint :=
                    (0, 6, 14, 20, 28);

end Gtk.Box.Digital_Clock;
