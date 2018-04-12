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

   function Current_Altitude return Shared_Types.Altitude
     with Volatile_Function;
   --  Provides the current altitude.
   --  @return The current altitude.

   function Current_Drag return Shared_Types.Acceleration
     with Volatile_Function;
   --  Provides the current drag acting on the space craft.
   --  @return The current drag.

   function Current_Velocity return Shared_Types.Velocity
     with Volatile_Function;
   --  Provides the current velocity.
   --  @return The current velocity.

   procedure Lander_Separation (Separated_At : in Ada.Real_Time.Time);
   --  Tells the altimeter simulation that and when the lander has been
   --  separated and the powered descent phase started (where we actively
   --  monitor thruster and fuel state).
   --  @param Separated_At The time at which the lander stage has been
   --         separated.

   procedure Shutdown;
   --  Signals the altitude control monitor to terminate.

private

   package Log is new Global.Log (Unit_Name => "ATM");
   --  Logger package instance for Altimeter.

end Altimeter;
