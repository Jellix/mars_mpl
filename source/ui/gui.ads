with GNAT.OS_Lib;
with Gtk.Gauge.Altimeter;
with Gtk.Gauge.LED_Round;
with Gtk.Gauge.Round_270;
with Gtk.GEntry;
with Gtk.Meter.Angular_90;
with Gtk.Oscilloscope;
with Gtk.Widget;
with Gtk.Window;
with Shared_Types;

package GUI is

   procedure Run;

private

   use type GNAT.OS_Lib.Process_Id;

   Aborted : Boolean                := False;
   SIM_Pid : GNAT.OS_Lib.Process_Id := GNAT.OS_Lib.Invalid_Pid;

   function Simulator_Running return Boolean;
   function Simulator_Running return Boolean is
     (SIM_Pid /= GNAT.OS_Lib.Invalid_Pid);

   type Leg_Switches is
     array (Shared_Types.Legs_Index) of Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;

   type Dynamic_Elements is
      record
         Leg_Led      : Leg_Switches;
         Thruster_Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
         Altitude     : Gtk.GEntry.Gtk_Entry;
         Velocity     : Gtk.GEntry.Gtk_Entry;
         Fuel         : Gtk.GEntry.Gtk_Entry;
      end record;

   type Legs_Channels is array (Shared_Types.Legs_Index) of
     Gtk.Oscilloscope.Channel_Number;

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Start_Button      : access Gtk.Widget.Gtk_Widget_Record'Class;
         Abort_Button      : access Gtk.Widget.Gtk_Widget_Record'Class;
         Elements          : Dynamic_Elements;
         Oscilloscope      : Gtk.Oscilloscope.Gtk_Oscilloscope;
         Altitude_Channel  : Gtk.Oscilloscope.Channel_Number;
         Velocity_Channel  : Gtk.Oscilloscope.Channel_Number;
         Touchdown_Channel : Legs_Channels;
         Thruster_Channel  : Gtk.Oscilloscope.Channel_Number;
         Tachometer        : Gtk.Gauge.Round_270.Gtk_Gauge_Round_270;
         Altimeter         : Gtk.Gauge.Altimeter.Gtk_Gauge_Altimeter;
         Fuel_Scale        : Gtk.Meter.Angular_90.Gtk_Meter_Angular_90;
      end record;
   type Main_Window is access all Main_Window_Record'Class;

end GUI;
