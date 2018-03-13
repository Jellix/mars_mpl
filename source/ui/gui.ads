with Global;
with GNAT.Expect;
with Gtk.Frame.Log_Viewer;
with Gtk.Gauge.Altimeter;
with Gtk.Gauge.LED_Round;
with Gtk.Gauge.Round_270;
with Gtk.GEntry;
with Gtk.Meter.Angular_90;
with Gtk.Oscilloscope;
with Gtk.Widget;
with Gtk.Window;
with Shared_Types.IO;

package GUI is

   procedure Run;

private

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

   type Plot_Elements is
      record
         Oscilloscope      : Gtk.Oscilloscope.Gtk_Oscilloscope;
         Altitude_Channel  : Gtk.Oscilloscope.Channel_Number;
         Fuel_Channel      : Gtk.Oscilloscope.Channel_Number;
         Thruster_Channel  : Gtk.Oscilloscope.Channel_Number;
         Touchdown_Channel : Legs_Channels;
         Velocity_Channel  : Gtk.Oscilloscope.Channel_Number;
      end record;

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Start_Button      : access Gtk.Widget.Gtk_Widget_Record'Class;
         Abort_Button      : access Gtk.Widget.Gtk_Widget_Record'Class;
         Elements          : Dynamic_Elements;
         Plot              : Plot_Elements;
         Tachometer        : Gtk.Gauge.Round_270.Gtk_Gauge_Round_270;
         Altimeter         : Gtk.Gauge.Altimeter.Gtk_Gauge_Altimeter;
         Fuel_Scale        : Gtk.Meter.Angular_90.Gtk_Meter_Angular_90;
         SIMon_Says        : Gtk.Frame.Log_Viewer.Gtk_Frame_Log_Viewer;
         Aborted           : Boolean;
         SIM_Process       : GNAT.Expect.Process_Descriptor_Access;
   end record;
   type Main_Window is access all Main_Window_Record'Class;

   procedure Quit_GUI (Win : not null access Main_Window_Record);

   function Simulator_Running (Win : in Main_Window_Record) return Boolean;

   package Log is new Global.Log (Unit_Name => "GUI");

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   function Image is new
     Shared_Types.IO.Generic_Image (T    => Shared_Types.Acceleration,
                                    Unit => "m/s²");

   function Image is new
     Shared_Types.IO.Generic_Image (T    => Shared_Types.Altitude,
                                    Unit => "m");

   function Image is new
     Shared_Types.IO.Generic_Image (T    => Shared_Types.Flow_Rate,
                                    Unit => "kg/s");

   function Image is new
     Shared_Types.IO.Generic_Image (T    => Shared_Types.Fuel_Mass,
                                    Unit => "kg");

   function Image is new
     Shared_Types.IO.Generic_Image (T    => Shared_Types.On_Time,
                                    Unit => "ms");

   function Image is new
     Shared_Types.IO.Generic_Image (T    => Shared_Types.Vehicle_Mass,
                                    Unit => "kg");

   function Image is new
     Shared_Types.IO.Generic_Image (T    => Shared_Types.Velocity,
                                    Unit => "m/s");

   pragma Warnings (On, "instance does not use primitive operation ""*""");

end GUI;
