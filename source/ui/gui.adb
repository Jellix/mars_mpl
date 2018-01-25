with Ada.Real_Time;

with GNAT.OS_Lib;
with GNATCOLL.Traces;

with Global;
with GUI_Callbacks;
with Shared_Sensor_Data;
with Shared_Types.IO;

with Cairo;

with Gdk.Color;
with Glib;

with Gtk.Box;
with Gtk.Enums.String_Lists;
with Gtk.Gauge.Altimeter;
with Gtk.Gauge.LED_Round;
with Gtk.Gauge.Round_270;
with Gtk.GEntry;
with Gtk.Main;
with Gtk.Meter.Angular_90;
with Gtk.Missed;
with Gtk.Oscilloscope;
with Gtk.Widget;
with Gtk.Window;

with Pango.Cairo.Fonts;

procedure GUI is

   Logger : constant GNATCOLL.Traces.Trace_Handle
     := GNATCOLL.Traces.Create (Unit_Name => "GUI",
                                Default   => GNATCOLL.Traces.On,
                                Stream    => Global.Standard_Error);

   use type Ada.Real_Time.Time;
   use type GNAT.OS_Lib.Process_Id;
   use type Shared_Types.Altitude;
   use type Shared_Types.Velocity;
   use type Glib.Gdouble;
   use type Gtk.Enums.String_Lists.Controlled_String_List;
   use type Shared_Types.Leg_State;
   use type Shared_Types.State;

   Label_Font        : constant Pango.Cairo.Fonts.Pango_Cairo_Font :=
                         Pango.Cairo.Fonts.Create_Toy
                           (Family => "arial",
                            Slant  => Cairo.Cairo_Font_Slant_Normal,
                            Weight => Cairo.Cairo_Font_Weight_Bold);

   Label_Font_Italic : constant Pango.Cairo.Fonts.Pango_Cairo_Font :=
                         Pango.Cairo.Fonts.Create_Toy
                           (Family => "arial",
                            Slant  => Cairo.Cairo_Font_Slant_Italic,
                            Weight => Cairo.Cairo_Font_Weight_Bold);

   package Colors is

      subtype Color is Gdk.Color.Gdk_Color;

      function RGB (Red   : Glib.Gdouble;
                    Green : Glib.Gdouble;
                    Blue  : Glib.Gdouble) return Color renames Gtk.Missed.RGB;

      Black        : constant Color := RGB (0.0, 0.0, 0.0);
      Blue         : constant Color := RGB (0.0, 0.0, 1.0);
      Light_Yellow : constant Color := RGB (1.0, 1.0, 0.5);
      Green        : constant Color := RGB (0.0, 1.0, 0.0);
      Grey         : constant Color := RGB (0.5, 0.5, 0.5);
      Purple       : constant Color := RGB (1.0, 0.5, 0.5);
      Red          : constant Color := RGB (1.0, 0.0, 0.0);
      White        : constant Color := RGB (1.0, 1.0, 1.0);

   end Colors;

   type Scaling is
      record
         Texts  : access Gtk.Enums.String_Lists.Controlled_String_List;
         Factor : Glib.Gdouble;
      end record;

   Altitude_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
           ("0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"),
         Factor => 10000.0);
   Fuel_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
           ("0" / "20" / "40" / "60" / "80"),
         Factor => 80.0);
   Velocity_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
             ("0" / "20" / "40" / "60" / "80" / "100" / "120" / "140" / "160"),
         Factor => 160.0);

   Update_Interval : constant Ada.Real_Time.Time_Span :=
                       Ada.Real_Time.Milliseconds (1);
   -- GUI update frequency if no events happen.

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

   --  Prototypes
   function Create_Altitude_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_Button_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_Fuel_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_Sensor_Signals_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_Timeline_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_Velocity_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   --  Stubs
   function Create_Altitude_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   function Create_Button_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   function Create_Fuel_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   function Create_Sensor_Signals_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   function Create_Timeline_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   function Create_Velocity_Frame
     (Window : in out Main_Window_Record'Class) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   --
   procedure Feed_Values (Win          : in Main_Window_Record'Class;
                          Update_State : in Shared_Sensor_Data.State);
   procedure Feed_Values (Win          : in Main_Window_Record'Class;
                          Update_State : in Shared_Sensor_Data.State) is
      DE : Dynamic_Elements renames Win.Elements;
   begin
      --  LEDs
      for Leg in DE.Leg_Led'Range loop
         DE.Leg_Led (Leg).all.Set_State
           (State => Update_State.Legs (Leg) = Shared_Types.Touched_Down);
         DE.Leg_Led (Leg).all.Queue_Draw;
      end loop;
      DE.Thruster_Led.all.Set_State
        (State => Update_State.Thruster = Shared_Types.Enabled);
      DE.Thruster_Led.all.Queue_Draw;

      -- Altitude
      DE.Altitude.all.Set_Text
        (Text => Shared_Types.IO.Image (Value => Update_State.Altitude));
      Win.Altimeter.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Altitude) / Altitude_Scale.Factor);
      Win.Altimeter.all.Queue_Draw;

      -- Velocity
      DE.Velocity.all.Set_Text
        (Text => Shared_Types.IO.Image (Value => Update_State.Velocity));
      Win.Tachometer.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Velocity) / Velocity_Scale.Factor);
      Win.Tachometer.all.Queue_Draw;

      --  Fuel
      DE.Fuel.all.Set_Text
        (Text => Shared_Types.IO.Image (Value => Update_State.Fuel));
      Win.Fuel_Scale.all.Set_Value
        (Value => Glib.Gdouble (Update_State.Fuel) / Fuel_Scale.Factor);
      Win.Fuel_Scale.all.Queue_Draw;

      --  Data plots
      declare
         Plotter : Gtk.Oscilloscope.Gtk_Oscilloscope_Record renames
                     Gtk.Oscilloscope.Gtk_Oscilloscope_Record
                       (Win.Oscilloscope.all);
         Time_Stamp : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      begin
         -- Data plot
         Plotter.Feed
           (Channel => Win.Altitude_Channel,
            V       => Glib.Gdouble (Update_State.Altitude),
            T       => Time_Stamp);
         Plotter.Feed
           (Channel => Win.Velocity_Channel,
            V       => Glib.Gdouble (Update_State.Velocity),
            T       => Time_Stamp);

         for Leg in Shared_Types.Legs_Index loop
            declare
               Offset : constant Glib.Gdouble :=
                          0.3 * Glib.Gdouble (Shared_Types.Legs_Index'Pos (Leg));
               Active : constant Glib.Gdouble :=
                          0.2 * Glib.Gdouble (Boolean'Pos (Update_State.Legs (Leg) = Shared_Types.Touched_Down));
            begin
               Plotter.Feed
                 (Channel => Win.Touchdown_Channel (Leg),
                  V       => Offset + Active,
                  T       => Time_Stamp);
            end;
         end loop;

         Plotter.Feed
           (Channel => Win.Thruster_Channel,
            V       =>
              Glib.Gdouble (Shared_Types.State'Pos (Update_State.Thruster)),
            T       => Time_Stamp);
      end;
   end Feed_Values;

   procedure Initialize (Window : in out Main_Window_Record'Class);
   procedure Initialize (Window : in out Main_Window_Record'Class)
   is
   begin
      Window.Initialize (The_Type => Gtk.Enums.Window_Toplevel);
      Window.Set_Title (Title => "Mars MPL simulation");

      declare
         Box : constant Gtk.Box.Gtk_Box :=
                 Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                       Spacing     => 0);
      begin
         Window.Add (Widget => Box);
         Box.all.Pack_Start (Child =>
                               Create_Sensor_Signals_Frame (Window => Window));
         Box.all.Pack_Start (Child => Create_Timeline_Frame (Window => Window));
         Box.all.Pack_Start (Child => Create_Button_Frame (Window => Window));
      end;
   end Initialize;

   Win          : Main_Window;
   Update_State : Shared_Sensor_Data.State;
begin
   GUI_Callbacks.Aborted := False;

   Gtk.Main.Init;
   Win := new Main_Window_Record;
   Initialize (Window => Win.all);

   GUI_Callbacks.Windows_CB.Connect
     (Widget => Win.all'Unrestricted_Access,
      Name   => "destroy",
      Cb     => GUI_Callbacks.Exit_Main'Access);

   Win.all.Show_All;

   declare
      Next_Update : Ada.Real_Time.Time := Global.Start_Time;
      Last_Update : Ada.Real_Time.Time := Global.Start_Time;
   begin
      Main_Loop :
      loop
         delay until Next_Update;
         Next_Update := Next_Update + Update_Interval;
         Update_State := Shared_Sensor_Data.Current_State.Get;

         if not Update_State.Terminated then
            Feed_Values (Win          => Win.all,
                         Update_State => Update_State);
            Last_Update := Ada.Real_Time.Clock;
         else
            Win.all.Oscilloscope.all.Set_Time
              (Sweeper => Gtk.Oscilloscope.Lower,
               Stamp   => Last_Update);
         end if;

         Gtk.Widget.Set_Sensitive
           (Widget    => Win.all.Start_Button,
            Sensitive => GUI_Callbacks.SIM_Pid = GNAT.OS_Lib.Invalid_Pid);

         while Gtk.Main.Events_Pending loop
            if Gtk.Main.Main_Iteration_Do (Blocking => False) then
               null;
            end if;
         end loop;

         exit Main_Loop when GUI_Callbacks.Aborted;
      end loop Main_Loop;
   end;
exception
   when E : others =>
      Logger.all.Trace (E => E);
end GUI;
