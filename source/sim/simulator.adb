--
--  EDL sequence: https://mars.jpl.nasa.gov/msp98/lander/edl.html
--  Data is a bit different from the actual investigation report, though.
--
--  Sequence overview:
--
--  | Time    | Height  | Speed      | Action
--  +---------+---------+------------+------------------------------------------
--  |         |         | 6800   m/s | Start of EDL sequence
--  | -15 min | 4600 km | 5700   m/s | Guidance system initialization
--  | -12 min | 3000 km | 5900   m/s | Turn to entry attitude
--  | -10 min | 2300 km | 6200   m/s | Cruise ring separation
--  |  -5 min |  125 km | 6900   m/s | Atmospheric entry
--  |  -2 min | 8800  m |  490   m/s | Parachute deployment
--  | -110  s | 7500  m |  250   m/s | Heatshield jettison
--  |  -50  s | 2500  m |   85   m/s | Radar ground acquisition (altitude mode)
--  |  -36  s | 1400  m |   80   m/s | Radar ground acquisition (doppler/speed
--  |         |         |            | and direction mode)
--  |  -35  s | 1300  m |   80   m/s | Lander separation/powered descent
--  |    0  s |    0  m |    2.5 m/s | Touchdown
--
--  https://mars.nasa.gov/msp98/lander/fact.html
--
--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Altimeter;
with Configuration.Task_Offsets;
with Global;
with Landing_Legs;
with Shared_Parameters.Read;
with Shared_Sensor_Data;
with Shared_Types.IO;
with Thrusters;
with Touchdown_Monitor;

procedure Simulator is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Altitude;
   use type Shared_Types.Velocity;
   use type Touchdown_Monitor.Run_State;

   type EDL_Phase is (EDL_Started,
                      Guidance_System_Initialized,
                      Turned_To_Entry_Attitude,
                      Cruise_Ring_Separated,
                      Atmosphere_Entered,
                      Parachute_Deployed,
                      Heatshield_Jettisoned,
                      Legs_Deployed,
                      Lander_Separated,
                      Powered_Descent);

   Altitude_For_Guidance_System_Initialization : constant := 4_600_000.0;
   Altitude_For_Turn_To_Entry_Attitude         : constant := 3_000_000.0;
   Altitude_For_Cruise_Ring_Separation         : constant := 2_300_000.0;
   Altitude_For_Atmospheric_Entry              : constant :=   125_000.0;
   Altitude_For_Parachute_Deployment           : constant :=     8_800.0;
   Altitude_For_Heatshield_Jettison            : constant :=     7_500.0;
   Altitude_For_Leg_Deployment                 : constant :=     2_500.0;
   Altitude_For_Lander_Separation              : constant :=     1_300.0;
   Altitude_For_Arming_Touchdown_Monitor       : constant :=        40.0;

   Cycle : constant Ada.Real_Time.Time_Span :=
             Ada.Real_Time.Milliseconds (MS => 10);

   procedure Update_Shared_Data;
   procedure Update_Shared_Data is
      Offset      : constant Duration :=
                      Ada.Real_Time.To_Duration
                        (TS => Ada.Real_Time.Clock - Global.Start_Time);
      Altitude    : constant Shared_Types.Altitude     := Altimeter.Current_Altitude;
      Drag        : constant Shared_Types.Acceleration := Altimeter.Current_Drag;
      Dry_Mass    : constant Shared_Types.Vehicle_Mass := Altimeter.Current_Dry_Mass;
      Fuel        : constant Shared_Types.Fuel_Mass    := Thrusters.Current_Fuel_Mass;
      All_Legs    : Shared_Types.All_Legs_State;
      Temperature : constant Shared_Types.Kelvin       := Altimeter.Current_Surface_Temperature;
      Thrust_On   : constant Boolean                   := Thrusters.Is_Enabled;
      Velocity    : constant Shared_Types.Velocity     := Altimeter.Current_Velocity;
   begin
      Landing_Legs.Read_State (State => All_Legs);
      Shared_Sensor_Data.Current_State.Set
        (New_Value =>
           Shared_Sensor_Data.State'(Altitude         => Altitude,
                                     Drag             => Drag,
                                     Dry_Mass         => Dry_Mass,
                                     Fuel             => Fuel,
                                     Legs             => All_Legs,
                                     Temperature      => Temperature,
                                     Thruster_Enabled => Thrust_On,
                                     Time_Stamp       => Offset,
                                     Velocity         => Velocity));
   end Update_Shared_Data;

   Target_Landing_Velocity : constant Shared_Types.Velocity :=
                               Shared_Parameters.Read.Target_Landing_Velocity;
   Safe_Landing_Velocity   : constant Shared_Types.Velocity :=
                               Shared_Parameters.Read.Safe_Landing_Velocity;

   package Log is new Global.Log (Unit_Name => "SIM");
begin
   Touchdown_Monitor.Start;
   Log.Trace (Message => "Touchdown monitors started.");

   Main_Block :
   declare
      Next_Cycle : Ada.Real_Time.Time :=
                     Global.Start_Time + Configuration.Task_Offsets.SIM_Task;

      Current_Phase      : EDL_Phase             := EDL_Started;
      Monitor_Enabled    : Boolean               := False;
      Current_Altitude   : Shared_Types.Altitude := Altimeter.Current_Altitude;
      Powered_Descent_At : Ada.Real_Time.Time    := Ada.Real_Time.Time_Last;

      Current_Velocity   : Shared_Types.Velocity;
   begin
      while Current_Altitude > 0.0 loop
         delay until Next_Cycle;

         Current_Altitude := Altimeter.Current_Altitude;
         Current_Velocity := Altimeter.Current_Velocity;

         --  | -15 min | 4600 km | 5700   m/s | Guidance system initialization
         if
           Current_Phase = EDL_Started and then
           Current_Altitude <= Altitude_For_Guidance_System_Initialization
         then
            Current_Phase := Guidance_System_Initialized;
            Log.Trace (Message => "Guidance system initialized.");
         end if;

         --  | -12 min | 3000 km | 5900   m/s | Turn to entry attitude
         if
           Current_Phase = Guidance_System_Initialized and then
           Current_Altitude <= Altitude_For_Turn_To_Entry_Attitude
         then
            Current_Phase := Turned_To_Entry_Attitude;
            Log.Trace (Message => "Turn to entry attitude.");
         end if;

         --  | -10 min | 2300 km | 6200   m/s | Cruise ring separation
         if
           Current_Phase = Turned_To_Entry_Attitude and then
           Current_Altitude <= Altitude_For_Cruise_Ring_Separation
         then
            Current_Phase := Cruise_Ring_Separated;
            Altimeter.Separate_Cruise_Stage;
            Log.Trace (Message => "Cruise ring and probes separated.");
         end if;

         --  |  -5 min |  125 km | 6900   m/s | Atmospheric entry
         if
           Current_Phase = Cruise_Ring_Separated and then
           Current_Altitude <= Altitude_For_Atmospheric_Entry
         then
            Current_Phase := Atmosphere_Entered;
            Altimeter.Enter_Atmosphere;
            Log.Trace (Message => "Entered atmosphere.");
         end if;

         --  |  -2 min | 8800  m |  490   m/s | Parachute deployment
         if
           Current_Phase = Atmosphere_Entered and then
           Current_Altitude <= Altitude_For_Parachute_Deployment
         then
            Current_Phase := Parachute_Deployed;
            Altimeter.Deploy_Parachute;
            Log.Trace (Message => "Parachute deployed.");
         end if;

         --  | -110  s | 7500  m |  250   m/s | Heatshield jettison
         if
           Current_Phase = Parachute_Deployed and then
           Current_Altitude <= Altitude_For_Heatshield_Jettison
         then
            Current_Phase := Heatshield_Jettisoned;
            Altimeter.Jettison_Heatshield;
            Log.Trace (Message => "Heatshield jettisoned.");
         end if;

         --  EDL sequence:
         --    [...] the lander legs will be deployed; 1.5 seconds after that,
         --    the landing radar will be activated. The radar will be able to
         --    gauge the spacecraft's altitude about 44 seconds after it is
         --    turned on, at an altitude of about 2.5 kilometers [...] above the
         --    surface.
         if
           Current_Phase = Heatshield_Jettisoned and then
           Current_Altitude <= Altitude_For_Leg_Deployment
         then
            Current_Phase := Legs_Deployed;
            Log.Trace (Message => "Deploying landing legs...");
            Landing_Legs.Deploy;
         end if;

         --  Entering powered descent.
         --
         --  EDL sequence:
         --    [...] when the spacecraft is traveling at about 80 m/s [...] some
         --    1.4 kilometers [...] above the surface, the [...] descent engines
         --    will be turned on one-half second later [...]
         if
           Current_Phase = Legs_Deployed and then
           Current_Altitude <= Altitude_For_Lander_Separation
         then
            Current_Phase := Lander_Separated;
            Altimeter.Separate_Lander;
            Powered_Descent_At := Next_Cycle + Ada.Real_Time.Milliseconds (500);
            Log.Trace (Message => "Lander separated.");
         end if;

         if
           Current_Phase = Lander_Separated and then
           Next_Cycle >= Powered_Descent_At
         then
            Current_Phase := Powered_Descent;
            Log.Trace (Message => "Entered powered descent flight mode.");
         end if;

         --  EDL sequence:
         --    Once the spacecraft reaches either an altitude of 12 meters [...]
         --    or a velocity of 2.4 meters per second [...], the lander will
         --    drop straight down at a constant speed. The descent engines will
         --    be turned off when touchdown is detected by sensors in the
         --    footpads.
         if Current_Phase = Powered_Descent then
            Approach_Landing_Velocity :
            declare
               --  We want a steadily decelerating descent until drop distance,
               --  then a constant velocity at the target landing velocity.

               Drop_Distance   : constant := 12.0;
               --  Distance from ground when we drop straight down at constant
               --  velocity.

               Velocity_Factor : constant
                 := 80.0 / (Altitude_For_Lander_Separation - Drop_Distance);
               --  At 1300 m, we expect to be at ~80 m/s, so use this as a
               --  factor to derive a target velocity from the current altitude
               --  until we match the target landing velocity at Drop_Distance.

               Corrected_Altitude : constant Shared_Types.Altitude :=
                                      Shared_Types.Altitude'Max
                                        (0.0, Current_Altitude - Drop_Distance);
               --  Altitude until constant drop speed.

               Current_Target_Velocity : constant Shared_Types.Velocity
                 := Shared_Types.Velocity (Corrected_Altitude * Velocity_Factor)
                                           + Target_Landing_Velocity;
            begin
               if Current_Velocity < Current_Target_Velocity then
                  Thrusters.Disable;
               else
                  Thrusters.Enable;
               end if;
            end Approach_Landing_Velocity;
         end if;

         if
           not Monitor_Enabled and then
           Current_Altitude <= Altitude_For_Arming_Touchdown_Monitor
         then
            Log.Trace (Message => "Enabling touchdown monitors...");
            Touchdown_Monitor.Enable;
            Monitor_Enabled := True;
         end if;

         Update_Shared_Data;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;
   end Main_Block;

   delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);

   Check_Touchdown :
   declare
      Touchdown_Velocity : constant Shared_Types.Velocity :=
                             Altimeter.Current_Velocity;
   begin
      if Touchdown_Velocity > Safe_Landing_Velocity then
         Log.Trace (Message => "MISSION FAILURE: MPL crashed on surface!");
      else
         Log.Trace (Message => "MISSION SUCCESS: MPL touched down safely.");
      end if;
   end Check_Touchdown;

   Update_Shared_Data;

   Altimeter.Shutdown;
   Touchdown_Monitor.Shutdown;
   Landing_Legs.Shutdown;
   Thrusters.Shutdown;

   Wait_For_Monitors :
   declare
      All_Monitors_Dead : Boolean := False;
   begin
      while not All_Monitors_Dead loop
         All_Monitors_Dead :=
           (for all Leg in Shared_Types.Legs_Index'Range =>
              Touchdown_Monitor.Current_State (Leg => Leg) =
                Touchdown_Monitor.Terminated);

         if All_Monitors_Dead then
            Log.Trace (Message => "All touchdown monitors finished.");
            Update_Shared_Data;
         end if;
      end loop;
   end Wait_For_Monitors;

   Output_Statistics :
   declare
      function Image is new Shared_Types.IO.Generic_Image (T    => Duration,
                                                           Unit => "s");
   begin
      Log.Trace (Message => "Thrusters have been fired"
                 & Thrusters.Max_Burn_Cycles'Image (Thrusters.Burn_Cycles)
                 & " times, and burned for a total of "
                 & Image (Value     => Thrusters.Burn_Time,
                          With_Unit => True)
                 & ".");
   end Output_Statistics;

   --  Give the data generating task time to terminate.
   delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
   Update_Shared_Data;

   Log.Trace (Message => "Simulation finished.");
exception
   when E : others =>
      Log.Trace (E => E);
end Simulator;
