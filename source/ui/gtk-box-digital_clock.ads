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

   type    Digit_Index  is range 1 .. 5;
   subtype Valid_Digits is Natural range 0 .. 9;

   type Digits_Set is array (Digit_Index) of Valid_Digits;

   type LED_Assignment is array (Glib.Gint range <>,
                                 Glib.Gint range <>) of Boolean
     with Pack => True;

   type Dot_Matrix is array (Glib.Gint range <>,
                             Glib.Gint range <>) of
     Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;

   type Gtk_Box_Digital_Clock_Record is new Gtk_Box_Record with
      record
         Grid       : Gtk.Grid.Gtk_Grid;
         Old_Digits : Digits_Set;
         LED_Matrix : Dot_Matrix (0 .. 32, 0 .. 6);
      end record;

   procedure Write_Digit (This  : in out Gtk_Box_Digital_Clock_Record;
                          Digit : in     Digit_Index;
                          Num   : in     Valid_Digits;
                          Force : in     Boolean := False);

   function New_LED (On_Color  : in Gdk.Color.Gdk_Color;
                     Off_Color : in Gdk.Color.Gdk_Color) return not null
     Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;

   Digit_Offset : constant array (Digit_Index) of Glib.Gint :=
                    (0, 6, 14, 20, 28);

end Gtk.Box.Digital_Clock;
