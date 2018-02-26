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

   Monitor_Enabled   : Boolean := False;
   Cycle             : constant Ada.Real_Time.Time_Span :=
                         Ada.Real_Time.Milliseconds (MS => 10);

   procedure Update_Shared_Data (Terminated : in Boolean := False);

   procedure Update_Shared_Data (Terminated : in Boolean := False)
   is
      Offset    : constant Duration :=
                    Ada.Real_Time.To_Duration
                      (TS => Ada.Real_Time.Clock - Global.Start_Time);
      Thrust_On : constant Boolean                := Thrusters.Is_Enabled;
      Altitude  : constant Shared_Types.Altitude  := Altimeter.Current_Altitude;
      Velocity  : constant Shared_Types.Velocity  := Altimeter.Current_Velocity;
      Fuel      : constant Shared_Types.Fuel_Mass := Thrusters.Current_Fuel_Mass;
      All_Legs  : Shared_Types.All_Legs_State;
   begin
      Landing_Legs.Read_State (State => All_Legs);
      Shared_Sensor_Data.Current_State.Set
        (Data =>
           Shared_Sensor_Data.State'(Legs             => All_Legs,
                                     Thruster_Enabled => Thrust_On,
                                     Altitude         => Altitude,
                                     Velocity         => Velocity,
                                     Fuel             => Fuel,
                                     Time_Stamp       => Offset,
                                     Terminated       => Terminated));
   end Update_Shared_Data;

   Target_Landing_Velocity : constant Shared_Types.Velocity :=
                               Shared_Parameters.Read.Target_Landing_Velocity;
   Safe_Landing_Velocity   : constant Shared_Types.Velocity :=
                               Shared_Parameters.Read.Safe_Landing_Velocity;

   package Log is new Global.Log (Unit_Name => "SIM");
begin
   Log.Trace (Message => "Starting touchdown monitors...");
   Touchdown_Monitor.Start;

   declare
      Next_Cycle       : Ada.Real_Time.Time
        := Global.Start_Time + Configuration.Task_Offsets.SIM_Task;

      Current_Altitude : Shared_Types.Altitude := Altimeter.Current_Altitude;
      Legs_Deployed    : Boolean               := False;
      Powered_Descent  : Boolean               := False;
      Current_Velocity : Shared_Types.Velocity;
   begin
      while Current_Altitude > 0.0 loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;

         Current_Altitude := Altimeter.Current_Altitude;
         Current_Velocity := Altimeter.Current_Velocity;

         --  EDL sequence:
         --    [...] the lander legs will be deployed; 1.5 seconds after that,
         --    the landing radar will be activated. The radar will be able to
         --    gauge the spacecraft's altitude about 44 seconds after it is
         --    turned on, at an altitude of about 2.5 kilometers [...] above the
         --    surface.
         if not Legs_Deployed and then Current_Altitude <= 2500.0 then
            Landing_Legs.Deploy;
            Legs_Deployed := True;
         end if;

         --  Entering powered descent.
         --
         --  EDL sequence:
         --    [...] when the spacecraft is traveling at about 80 m/s [...] some
         --    1.4 kilometers [...] above the surface, the [...] descent engines
         --    will be turned on one-half second later [...]
         if not Powered_Descent and then Current_Altitude <= 1300.0 then
            Thrusters.Enable;
            Powered_Descent := True;
            Log.Trace (Message => "Entered powered descent flight mode.");
         end if;

         --  EDL sequence:
         --    Once the spacecraft reaches either an altitude of 12 meters [...]
         --    or a velocity of 2.4 meters per second [...], the lander will
         --    drop straight down at a constant speed. The descent engines will
         --    be turned off when touchdown is detected by sensors in the
         --    footpads.
         if Powered_Descent then
            if Current_Velocity < Target_Landing_Velocity then
               Thrusters.Disable;
            elsif
              Current_Velocity > Shared_Types.Velocity (Current_Altitude * 0.2)
            then
               Thrusters.Enable;
            end if;
         end if;

         if not Monitor_Enabled and then Current_Altitude <= 40.0 then
            Log.Trace (Message => "Enabling touchdown monitors...");
            Touchdown_Monitor.Enable;
            Monitor_Enabled := True;
         end if;

         Update_Shared_Data;
      end loop;
   end;

   delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);

   declare
      Touchdown_Velocity : constant Shared_Types.Velocity :=
                             Altimeter.Current_Velocity;
   begin
      if Touchdown_Velocity > Safe_Landing_Velocity then
         Log.Trace (Message => "MISSION FAILURE: MPL crashed on surface!");
      else
         Log.Trace (Message => "MISSION SUCCESS: MPL touched down safely.");
      end if;
   end;

   Update_Shared_Data;

   Altimeter.Shutdown;
   Touchdown_Monitor.Shutdown;
   Landing_Legs.Shutdown;
   Thrusters.Shutdown;

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
   end;

   declare
      function Image is new Shared_Types.IO.Generic_Image (T    => Duration,
                                                           Unit => "s");
   begin
      Log.Trace (Message => "Thrusters burned for a total of "
                 & Image (Value     => Thrusters.Burn_Time,
                          With_Unit => True)
                 & ".");
   end;

   --  Give the data generating task time to terminate.
   delay until Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100);
   Update_Shared_Data (Terminated => True);

   Log.Trace (Message => "Simulation finished.");
exception
   when E : others =>
      Log.Trace (E => E);
end Simulator;
