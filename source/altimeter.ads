--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Global;
with Shared_Types;

--  @summary
--  The (radar) altimeter simulation.
--
--  @description
--  Simulates the rate of change of altitude depending on current thruster
--  acceleration and local gravity.
--  It uses a cyclic task to sample the current thruster state and discretely
--  calculates the change in altitude and velocity until (simulated) ground
--  contact.
package Altimeter is

   Cycle : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);
   -- Sample cycle of simulated height sensor.

   function Current_Altitude return Shared_Types.Altitude
     with Volatile_Function;
   --  Provides the current altitude.
   --  @return The current altitude.

   function Current_Velocity return Shared_Types.Velocity
     with Volatile_Function;
   --  Provides the current velocity.
   --  @return The current velocity.

private

   package Log is new Global.Log (Unit_Name => "ATM");
   --  Logger package instance for Altimeter.

end Altimeter;
