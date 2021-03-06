with Ada.Real_Time;
with Altimeter;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Shared_Parameters.Read;
with Shared_Types;
with Thrusters;

package body Engines is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Meter;
   use type Shared_Types.Meter_Per_Second;

   Target_Landing_Velocity : constant Shared_Types.Meter_Per_Second :=
                               Shared_Parameters.Read.Target_Landing_Velocity;

   Aborted : Boolean := False
     with Atomic => True;
   pragma Warnings (Off,
                    Aborted,
                    Reason => "Do not warn about atomic synchronization");

   Descent_Started : Boolean := False
     with Atomic => True;
   pragma Warnings (Off,
                    Descent_Started,
                    Reason => "Do not warn about atomic synchronization");

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   procedure Start_Descent is
   begin
      Descent_Started := True;
   end Start_Descent;

   task Engine_Control;

   task body Engine_Control is
      Next_Cycle : Ada.Real_Time.Time :=
                     Global.Start_Time + Configuration.Task_Offsets.Engine_Task;
   begin
      Log.Trace ("Engine control task started.");

      Main_Loop :
      while not Aborted loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Configuration.Cycle_Times.Engine_Task;

         if Descent_Started then
            --  EDL sequence:
            --    Once the spacecraft reaches either an altitude of 12 meters
            --    [...] or a velocity of 2.4 meters per second [...], the lander
            --    will drop straight down at a constant speed. The descent
            --    engines will be turned off when touchdown is detected by
            --    sensors in the footpads.
            Approach_Landing_Velocity :
            declare
               Current_Altitude : constant Shared_Types.Meter
                 := Altimeter.Current_Altitude;

               Current_Velocity : constant Shared_Types.Meter_Per_Second
                 := Altimeter.Current_Velocity;

               --  We want a steadily decelerating descent until drop distance,
               --  then a constant velocity at the target landing velocity.

               Drop_Distance   : constant := 12.0;
               --  Distance from ground when we drop straight down at constant
               --  velocity.

               Velocity_Factor : constant Shared_Types.Scalar :=
                                   Shared_Types.Scalar (80.0 / (1300.0 - Drop_Distance));
               --  At 1300 m, we expect to be at ~80 m/s, so use this as a
               --  factor to derive a target velocity from the current altitude
               --  until we match the target landing velocity at Drop_Distance.

               Corrected_Altitude : constant Shared_Types.Meter :=
                                      Shared_Types.Meter'Max
                                        (0.0, Current_Altitude - Drop_Distance);
               --  Altitude until constant drop speed.

               Current_Target_Velocity : constant Shared_Types.Meter_Per_Second
                 := Shared_Types.Meter_Per_Second (Corrected_Altitude * Velocity_Factor)
                                           + Target_Landing_Velocity;
            begin
               if Current_Velocity > Current_Target_Velocity then
                  Thrusters.Enable;
               else
                  Thrusters.Disable;
               end if;
            end Approach_Landing_Velocity;
         end if;
      end loop Main_Loop;

      Log.Trace ("Engine control task terminated.");
   exception
      when E : others =>
         Log.Trace (E => E);
   end Engine_Control;

end Engines;
