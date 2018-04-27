--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

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

   function Current_Dry_Mass return Shared_Types.Vehicle_Mass with
     Volatile_Function;
   --  Provides the current dry mass of the space craft.
   --  @return The current dry mass.

   function Current_Velocity return Shared_Types.Velocity
     with Volatile_Function;
   --  Provides the current velocity.
   --  @return The current velocity.

   procedure Separate_Cruise_Stage;
   --  Tells the altimeter simulation that the cruise ring and the impact probes
   --  have been separated.

   procedure Enter_Atmosphere;
   --  Tells the altimeter simulation that the upper atmosphere has been
   --  entered.

   procedure Deploy_Parachute;
   --  Tells the altimeter simulation that the parachute has been deployed.

   procedure Jettison_Heatshield;
   --  Tells the altimeter simulation that the heatshield has been jettisoned.

   procedure Separate_Lander;
   --  Tells the altimeter simulation that the lander has been separated and the
   --  powered descent phase started (where we actively monitor thruster and
   --  fuel state).

   procedure Shutdown;
   --  Signals the altitude control monitor to terminate.

private

   package Log is new Global.Log (Unit_Name => "ATM");
   --  Logger package instance for Altimeter.

end Altimeter;
