with Ada.Real_Time;
with Altimeter;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Shared_Parameters.Read;
with Shared_Types;
with Thrusters;

package body Engines is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Altitude;
   use type Shared_Types.Velocity;

   Target_Landing_Velocity : constant Shared_Types.Velocity :=
                               Shared_Parameters.Read.Target_Landing_Velocity;

   Aborted : Boolean := False
     with Atomic => True;

   Descent_Started : Boolean := False
     with Atomic => True;

   pragma Warnings (Off, "atomic synchronization set");

   function Is_Aborted return Boolean is
     (Aborted)
   with Inline            => True,
        Volatile_Function => True;

   function Is_Descent_Started return Boolean is
     (Descent_Started)
   with Inline            => True,
        Volatile_Function => True;

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   procedure Start_Descent is
   begin
      Descent_Started := True;
   end Start_Descent;

   pragma Warnings (On, "atomic synchronization set");

   task Engine_Control;

   task body Engine_Control is
      Next_Cycle : Ada.Real_Time.Time :=
                     Global.Start_Time + Configuration.Task_Offsets.Engine_Task;
   begin
      Log.Trace ("Engine control task started.");

      Main_Loop :
      while not Is_Aborted loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Configuration.Cycle_Times.Engine_Task;

         if Is_Descent_Started then
            --  EDL sequence:
            --    Once the spacecraft reaches either an altitude of 12 meters
            --    [...] or a velocity of 2.4 meters per second [...], the lander
            --    will drop straight down at a constant speed. The descent
            --    engines will be turned off when touchdown is detected by
            --    sensors in the footpads.
            Approach_Landing_Velocity :
            declare
               Current_Altitude : constant Shared_Types.Altitude
                 := Altimeter.Current_Altitude;

               Current_Velocity : constant Shared_Types.Velocity
                 := Altimeter.Current_Velocity;

               --  We want a steadily decelerating descent until drop distance,
               --  then a constant velocity at the target landing velocity.

               Drop_Distance   : constant := 12.0;
               --  Distance from ground when we drop straight down at constant
               --  velocity.

               Velocity_Factor : constant := 80.0 / (1300.0 - Drop_Distance);
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
