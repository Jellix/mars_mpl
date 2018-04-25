with Ada.Real_Time;
with Cairo;
with Glib;
with GNAT.Regpat;
with Gtk.Box;
with Gtk.Dialog;
with Gtk.Enums.String_Lists;
with Gtk.Label;
with Gtk.Layered.Waveform;
with Gtk.Main;
with Gtk.Message_Dialog;
with Gtk.Missed;
with Gtk.Widget;
with GUI.Callbacks;
with Pango.Cairo.Fonts;
with Rocket_Science;
with Shared_Parameters.Default;
with Shared_Parameters.Read;
with Shared_Parameters.Write;
with Shared_Sensor_Data;

package body GUI is

   use type Ada.Real_Time.Time;
   use type Glib.Gdouble;
   use type Gtk.Dialog.Gtk_Response_Type;
   use type Gtk.Enums.String_Lists.Controlled_String_List;
   use type Shared_Types.Altitude;
   use type Shared_Types.Leg_State;
   use type Shared_Types.Mass;
   use type Shared_Types.Velocity;

   Label_Font        : constant Pango.Cairo.Fonts.Pango_Cairo_Font :=
                         Pango.Cairo.Fonts.Create_Toy
                           (Family => "arial",
                            Slant  => Cairo.Cairo_Font_Slant_Normal,
                            Weight => Cairo.Cairo_Font_Weight_Bold);

   type Scaling is
      record
         Texts  : not null access Gtk.Enums.String_Lists.Controlled_String_List;
         Factor : Glib.Gdouble;
      end record;

   Altitude_Scale   : constant Scaling
     := Scaling'(Texts  =>
                   new Gtk.Enums.String_Lists.Controlled_String_List'
                     ("0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"),
                 Factor => 10000.0);
   Delta_V_Scale    : constant Scaling
     := Scaling'(Texts =>
                   new Gtk.Enums.String_Lists.Controlled_String_List'
                     ("0" / "100" / "200" / "300" / "400" / "500"),
                 Factor => 500.0);
   Drag_Scale       : constant Scaling
     := Scaling'(Texts  =>
                    new Gtk.Enums.String_Lists.Controlled_String_List'
                      ("0" / "20" / "40" / "60" / "80" / "100"),
                 Factor => 100.0);
   Fuel_Scale       : constant Scaling
     := Scaling'(Texts  =>
                   new Gtk.Enums.String_Lists.Controlled_String_List'
                     ("0" / "20" / "40" / "60" / "80"),
                 Factor => 80.0);
   Velocity_Scale   : constant Scaling
     := Scaling'(Texts  =>
                   new Gtk.Enums.String_Lists.Controlled_String_List'
                     ("0" / "100" / "200" / "300" / "400" / "500"),
                 Factor => 500.0);

   Update_Interval  : constant Ada.Real_Time.Time_Span :=
                        Ada.Real_Time.Milliseconds (10);
   -- GUI update frequency, 100/s ought to be enough.

   --  Callbacks for entry fields
   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   function Set_Dry_Mass is new Callbacks.Set_Spin_Button_Value
     (T     => Shared_Types.Vehicle_Mass,
      Read  => Shared_Parameters.Read.Dry_Mass,
      Write => Shared_Parameters.Write.Dry_Mass,
      Name  => "spacecraft dry mass");

   procedure Reset_Dry_Mass is new Callbacks.Reset_Value
     (T          => Shared_Types.Vehicle_Mass,
      Write      => Shared_Parameters.Write.Dry_Mass,
      Default    => Shared_Parameters.Default.Dry_Mass,
      Text_Entry => Dry_Mass);

   function Set_Exhaust_Velocity is new Callbacks.Set_Spin_Button_Value
     (T     => Shared_Types.Velocity,
      Read  => Shared_Parameters.Read.Exhaust_Velocity,
      Write => Shared_Parameters.Write.Exhaust_Velocity,
      Name  => "exhaust velocity");

   procedure Reset_Exhaust_Velocity is new Callbacks.Reset_Value
     (T          => Shared_Types.Velocity,
      Write      => Shared_Parameters.Write.Exhaust_Velocity,
      Default    => Shared_Parameters.Default.Exhaust_Velocity,
      Text_Entry => Exhaust_Velocity);

   function Set_Fuel_Flow_Rate is new Callbacks.Set_Spin_Button_Value
     (T     => Shared_Types.Flow_Rate,
      Read  => Shared_Parameters.Read.Fuel_Flow_Rate,
      Write => Shared_Parameters.Write.Fuel_Flow_Rate,
      Name  => "fuel flow rate");

   procedure Reset_Fuel_Flow_Rate is new Callbacks.Reset_Value
     (T          => Shared_Types.Flow_Rate,
      Write      => Shared_Parameters.Write.Fuel_Flow_Rate,
      Default    => Shared_Parameters.Default.Fuel_Flow_Rate,
      Text_Entry => Fuel_Flow_Rate);

   function Set_Initial_Altitude is new Callbacks.Set_Spin_Button_Value
     (T     => Shared_Types.Altitude,
      Read  => Shared_Parameters.Read.Initial_Altitude,
      Write => Shared_Parameters.Write.Initial_Altitude,
      Name  => "initial altitude");

   procedure Reset_Initial_Altitude is new Callbacks.Reset_Value
     (T          => Shared_Types.Altitude,
      Write      => Shared_Parameters.Write.Initial_Altitude,
      Default    => Shared_Parameters.Default.Initial_Altitude,
      Text_Entry => Initial_Altitude);

   function Set_Initial_Fuel_Mass is new Callbacks.Set_Spin_Button_Value
     (T     => Shared_Types.Fuel_Mass,
      Read  => Shared_Parameters.Read.Initial_Fuel_Mass,
      Write => Shared_Parameters.Write.Initial_Fuel_Mass,
      Name  => "initial fuel mass");

   procedure Reset_Initial_Fuel_Mass is new Callbacks.Reset_Value
     (T          => Shared_Types.Fuel_Mass,
      Write      => Shared_Parameters.Write.Initial_Fuel_Mass,
      Default    => Shared_Parameters.Default.Initial_Fuel_Mass,
      Text_Entry => Initial_Fuel_Mass);

   function Set_Initial_Velocity is new Callbacks.Set_Spin_Button_Value
     (T     => Shared_Types.Velocity,
      Read  => Shared_Parameters.Read.Initial_Velocity,
      Write => Shared_Parameters.Write.Initial_Velocity,
      Name  => "initial velocity");

   procedure Reset_Initial_Velocity is new Callbacks.Reset_Value
     (T          => Shared_Types.Velocity,
      Write      => Shared_Parameters.Write.Initial_Velocity,
      Default    => Shared_Parameters.Default.Initial_Velocity,
      Text_Entry => Initial_Velocity);

   function Set_Shortest_On_Time is new Callbacks.Set_Spin_Button_Value
     (T     => Shared_Types.On_Time,
      Read  => Shared_Parameters.Read.Shortest_On_Time,
      Write => Shared_Parameters.Write.Shortest_On_Time,
      Name  => "Shortest on-time");

   procedure Reset_Shortest_On_Time is new Callbacks.Reset_Value
     (T          => Shared_Types.On_Time,
      Write      => Shared_Parameters.Write.Shortest_On_Time,
      Default    => Shared_Parameters.Default.Shortest_On_Time,
      Text_Entry => Shortest_On_Time);

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   --  Prototypes
   function Create_Sensor_Signals_Frame
     (Window     : in out Main_Window_Record;
      Gauge_Size : in     Glib.Gint) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_Simulation_Frame
     (Window : in out Main_Window_Record) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_Timeline_Frame
     (Window : in out Main_Window_Record) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   --  Stubs
   function Create_Sensor_Signals_Frame
     (Window     : in out Main_Window_Record;
      Gauge_Size : in     Glib.Gint) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   function Create_Simulation_Frame
     (Window : in out Main_Window_Record) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   function Create_Timeline_Frame
     (Window : in out Main_Window_Record) return not null access
     Gtk.Widget.Gtk_Widget_Record'Class is separate;

   --
   procedure Feed_Values (Win          : in Main_Window_Record;
                          Update_State : in Shared_Sensor_Data.State);
   procedure Feed_Values (Win          : in Main_Window_Record;
                          Update_State : in Shared_Sensor_Data.State)
   is
      DE : Dynamic_Elements renames Win.Elements;
      function Touched_Down (Leg : in Shared_Types.Legs_Index) return Boolean is
         (Update_State.Legs (Leg) = Shared_Types.Touched_Down);
   begin
      --  LEDs
      for Leg in DE.Leg_Led'Range loop
         DE.Leg_Led (Leg).all.Set_State
           (State => Update_State.Legs (Leg) = Shared_Types.Touched_Down);
         DE.Leg_Led (Leg).all.Queue_Draw;
      end loop;
      DE.Thruster_Led.all.Set_State
        (State => Update_State.Thruster_Enabled);
      DE.Thruster_Led.all.Queue_Draw;

      -- Altitude
      DE.Altitude.all.Set_Text (Text => Image (Value => Update_State.Altitude));
      Win.Altimeter.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Altitude) / Altitude_Scale.Factor);
      Win.Altimeter.all.Queue_Draw;

      -- Velocity
      DE.Velocity.all.Set_Text (Text => Image (Value => Update_State.Velocity));
      Win.Tachometer.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Velocity) / Velocity_Scale.Factor);
      Win.Tachometer.all.Queue_Draw;

      --  Delta_V
      declare
         Dry_Mass         : constant Shared_Types.Mass :=
                              Shared_Types.Mass
                                (Shared_Parameters.Read.Dry_Mass);
         -- FIXME: Due to heatshield jettison this is not constant anymore, so
         --        it should become part of the Update_State.
         Current_Wet_Mass : constant Shared_Types.Mass :=
                              Dry_Mass + Shared_Types.Mass (Update_State.Fuel);
         Max_Delta_V      : constant Shared_Types.Velocity :=
                              Rocket_Science.Delta_V
                                (Initial_Wet_Mass => Current_Wet_Mass,
                                 Current_Wet_Mass => Dry_Mass,
                                 Exhaust_Velocity =>
                                   Shared_Parameters.Read.Exhaust_Velocity);
      begin
         DE.Delta_V.all.Set_Text (Text => Image (Value => Max_Delta_V));
         Win.Delta_V_Scale.all.Set_Value
           (Value => Glib.Gdouble (Max_Delta_V) / Delta_V_Scale.Factor);
         Win.Delta_V_Scale.all.Queue_Draw;
      end;

      --  Drag
      DE.Drag.all.Set_Text (Text => Image (Value => Update_State.Drag));
      Win.Drag_Scale.all.Set_Value
        (Value => Glib.Gdouble (Update_State.Drag) / Drag_Scale.Factor);
      Win.Drag_Scale.all.Queue_Draw;

      --  Fuel
      DE.Fuel.all.Set_Text (Text => Image (Value => Update_State.Fuel));
      Win.Fuel_Scale.all.Set_Value
        (Value => Glib.Gdouble (Update_State.Fuel) / Fuel_Scale.Factor);
      Win.Fuel_Scale.all.Queue_Draw;

      Feed_Data_Plots :
      declare
         Time_Stamp : constant Glib.Gdouble :=
                        Glib.Gdouble (Update_State.Time_Stamp);
      begin
         -- Data plot
         Win.Plot.Altitude_Plot.all.Feed
           (Channel => Win.Plot.Altitude_Channel,
            T       => Time_Stamp,
            V       => Glib.Gdouble (Update_State.Altitude));
         Win.Plot.Drag_Plot.all.Feed
           (Channel => Win.Plot.Drag_Channel,
            T       => Time_Stamp,
            V       => Glib.Gdouble (Update_State.Drag));
         Win.Plot.Fuel_Plot.all.Feed
           (Channel => Win.Plot.Fuel_Channel,
            T       => Time_Stamp,
            V       => Glib.Gdouble (Update_State.Fuel));
         Win.Plot.Velocity_Plot.all.Feed
           (Channel => Win.Plot.Velocity_Channel,
            T       => Time_Stamp,
            V       => Glib.Gdouble (Update_State.Velocity));

         for Leg in Shared_Types.Legs_Index loop
            Feed_Leg_Plot :
            declare
               Offset : constant Glib.Gdouble :=
                          0.3 *
                            Glib.Gdouble (Shared_Types.Legs_Index'Pos (Leg));
               Active : constant Glib.Gdouble :=
                          0.2 * Glib.Gdouble (Boolean'Pos (Touched_Down (Leg)));
            begin
               Win.Plot.Discretes_Plot.all.Feed
                 (Channel => Win.Plot.Touchdown_Channel (Leg),
                  T       => Time_Stamp,
                  V       => Offset + Active);
            end Feed_Leg_Plot;
         end loop;

         Win.Plot.Discretes_Plot.all.Feed
           (Channel => Win.Plot.Thruster_Channel,
            T       => Time_Stamp,
            V       =>
              Glib.Gdouble (Boolean'Pos (Update_State.Thruster_Enabled)));
      end Feed_Data_Plots;

      --  Advance mission clock.
      Win.Mission_Clock.all.Set_Time (Time => Update_State.Time_Stamp);
      Win.Mission_Clock.all.Queue_Draw;
   end Feed_Values;

   procedure Initialize (Window : in out Main_Window_Record);
   procedure Initialize (Window : in out Main_Window_Record) is
   begin
      Window.Initialize (The_Type => Gtk.Enums.Window_Toplevel);
      Window.Set_Title (Title => "Mars MPL simulation");

      Window.Aborted := False;
      Window.SIM_Process := new GNAT.Expect.Process_Descriptor;

      Add_Widgets_To_Box :
      declare
         Box : constant not null Gtk.Box.Gtk_Box :=
                 Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                       Spacing     => 0);
      begin
         Window.Add (Widget => Box);
         Box.all.Pack_Start
           (Child  => Window.Create_Sensor_Signals_Frame (Gauge_Size => 6),
            Expand => True);
         Box.all.Pack_Start (Child  => Window.Create_Timeline_Frame,
                             Expand => False);
         Box.all.Pack_Start (Child  => Window.Create_Simulation_Frame,
                             Expand => False);
      end Add_Widgets_To_Box;
   end Initialize;

   procedure Quit_GUI (Win : not null access Main_Window_Record) is
      Do_Abort : Boolean := not Win.all.Aborted;
   begin
      if Win.all.Simulator_Running and then Do_Abort then
         User_Confirm :
         declare
            Confirm : constant not null Gtk.Message_Dialog.Gtk_Message_Dialog :=
                        Gtk.Message_Dialog.Gtk_Message_Dialog_New
                          (Parent   => Win,
                           Flags    => Gtk.Dialog.Modal,
                           The_Type => Gtk.Message_Dialog.Message_Question,
                           Buttons  => Gtk.Message_Dialog.Buttons_Yes_No,
                           Message  =>
                             "Simulator seems still running, quit anyway?");
         begin
            Do_Abort := Confirm.all.Run = Gtk.Dialog.Gtk_Response_Yes;
            Confirm.all.Destroy;
         end User_Confirm;
      end if;

      if Do_Abort then
         Log.Trace (Message => "Quitting GUI...");
         Win.all.Aborted := True;
      end if;
   end Quit_GUI;

   --
   procedure Reset_Timeline (Plot : in Plot_Elements) is
      procedure Erase (Scope   : in Gtk.Oscilloscope.Gtk_Oscilloscope_Record'Class;
                       Channel : in Gtk.Oscilloscope.Channel_Number);
      procedure Erase (Scope   : in Gtk.Oscilloscope.Gtk_Oscilloscope_Record'Class;
                       Channel : in Gtk.Oscilloscope.Channel_Number) is
      begin
         Scope.Get_Buffer (Channel => Channel).all.Erase;
      end Erase;
   begin
      Plot.Altitude_Plot.all.Get_Sweeper
        (Sweeper => Gtk.Oscilloscope.Lower).all.Configure
          (Value          => 0.0,
           Lower          => 0.0,
           Upper          => 10.0,
           Step_Increment => 1.0,
           Page_Increment => 5.0,
           Page_Size      => 10.0);
      Plot.Altitude_Plot.all.Set_Time
        (Sweeper => Gtk.Oscilloscope.Lower,
         Stamp   =>
           Ada.Real_Time.Time'(Gtk.Layered.Waveform.To_Time (Value => 10.0)));

      --  Clear Altitude plot
      Erase (Scope   => Plot.Altitude_Plot.all,
             Channel => Plot.Altitude_Channel);

      --  Clear Drag plot.
      Erase (Scope   => Plot.Drag_Plot.all,
             Channel => Plot.Drag_Channel);

      --  Clear Discretes plots.
      Erase (Scope   => Plot.Discretes_Plot.all,
             Channel => Plot.Thruster_Channel);
      for T in Plot.Touchdown_Channel'Range loop
         Erase (Scope   => Plot.Discretes_Plot.all,
                Channel => Plot.Touchdown_Channel (T));
      end loop;

      --  Clear fuel plot.
      Erase (Scope   => Plot.Fuel_Plot.all,
             Channel => Plot.Fuel_Channel);

      --  Clear velocity plot.
      Erase (Scope   => Plot.Velocity_Plot.all,
             Channel => Plot.Velocity_Channel);
   end Reset_Timeline;

   procedure Run is
      Win          : aliased Main_Window_Record;
      Update_State : Shared_Sensor_Data.State :=
                       Shared_Sensor_Data.Current_State.Get;
   begin
      Gtk.Main.Disable_Setlocale; --  Default (English?) always.
      Gtk.Main.Init;
      Initialize (Window => Win);
      Win.On_Delete_Event (Call  => Callbacks.Exit_Main'Access,
                           After => True);
      Win.Set_Default_Size (Width  => 1024,
                            Height => 768);

      Win.Show_All;

      Main_Block :
      declare
         Next_Update : Ada.Real_Time.Time := Global.Start_Time;
         Last_Change : Duration           := -1.0;
      begin
         Last_Change := Update_State.Time_Stamp;

         Main_Loop :
         loop
            delay until Next_Update;
            Next_Update := Next_Update + Update_Interval;

            Update_State := Shared_Sensor_Data.Current_State.Get;

            if Last_Change /= Update_State.Time_Stamp then
               --  Update time stamp control.
               Last_Change := Update_State.Time_Stamp;

               --  Feed new values.
               Win.Feed_Values (Update_State => Update_State);

               --  Update right margin of sweeper.
               Win.Plot.Altitude_Plot.all.Set_Time
                 (Sweeper => Gtk.Oscilloscope.Lower,
                  Stamp   =>
                    Ada.Real_Time.Time'
                      (Gtk.Layered.Waveform.To_Time
                        (Value => Glib.Gdouble (Last_Change))));
            end if;

            Check_SIM_State :
            declare
               SIM_Active : constant Boolean := Win.Simulator_Running;
               --  This call is rather expensive, so only do it once per check.
            begin
               Win.Buttons (Start_Button).all.Set_Sensitive
                 (Sensitive => not SIM_Active);

               --  There might be a simulator running, but as we don't know its
               --  Process_Id at this point, we can't send it a kill signal
               --  anyway, thus there's no point in enabling the Abort button.
               Win.Buttons (Abort_Button).all.Set_Sensitive
                 (Sensitive => SIM_Active);
            end Check_SIM_State;

            --  Update SIM log view.
            Win.SIMon_Says.all.Update;

            Handle_Gtk_Events :
            while Gtk.Main.Events_Pending loop
               if Gtk.Main.Main_Iteration then
                  null;
               end if;
            end loop Handle_Gtk_Events;

            exit Main_Loop when Win.Aborted;
         end loop Main_Loop;
      end Main_Block;
   exception
      when E : others =>
         Log.Trace (E => E);
   end Run;

   function Simulator_Running (Win : in Main_Window_Record) return Boolean
   is
      Match_Result : GNAT.Expect.Expect_Match;
      Result       : Boolean;
   begin
      Handle_Exceptions :
      begin
         GNAT.Expect.Expect (Descriptor  => Win.SIM_Process.all,
                             Result      => Match_Result,
                             Regexp      => GNAT.Regpat.Never_Match,
                             Timeout     => 0,
                             Full_Buffer => False);
         Result := True;
      exception
         when GNAT.Expect.Invalid_Process
            | GNAT.Expect.Process_Died =>
            Result := False;
      end Handle_Exceptions;

      return Result;
   end Simulator_Running;

end GUI;
