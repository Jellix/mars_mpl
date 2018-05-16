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

   function Current_Attitude return Shared_Types.Degree
     with Volatile_Function => True;
   --  Provides the current space craft's attitude.
   --  @return The current attitude.

   function Current_Altitude return Shared_Types.Meter
     with Volatile_Function => True;
   --  Provides the current altitude.
   --  @return The current altitude.

   function Current_Drag return Shared_Types.Meter_Per_Square_Second
     with Volatile_Function => True;
   --  Provides the current drag acting on the space craft.
   --  @return The current drag.

   function Current_Dry_Mass return Shared_Types.Vehicle_Mass with
     Volatile_Function => True;
   --  Provides the current dry mass of the space craft.
   --  @return The current dry mass.

   function Current_Core_Temperature return Shared_Types.Kelvin with
     Volatile_Function => True;
   --  Provides the current core temperature of the space craft.
   --  @return The current core temperature.

   function Current_Surface_Temperature return Shared_Types.Kelvin with
     Volatile_Function => True;
   --  Provides the current surface temperature of the space craft's heatshield.
   --  @return The current surface temperature.

   function Current_Velocity return Shared_Types.Meter_Per_Second
     with Volatile_Function => True;
   --  Provides the current velocity.
   --  @return The current velocity.

   procedure Separate_Cruise_Stage;
   --  Tells the altimeter simulation that lander and cruise stage have been
   --  separated.
   --  Changes the dry mass of the vehicle.

   procedure Enter_Atmosphere;
   --  Tells the altimeter simulation that the upper atmosphere has been
   --  entered.
   --  Changes the drag coefficient

   procedure Deploy_Parachute;
   --  Tells the altimeter simulation that the parachute has been deployed.
   --  Changed the drag coefficient.

   procedure Jettison_Heatshield;
   --  Tells the altimeter simulation that the heatshield has been jettisoned.
   --  Changes the dry mass of the vehicle.

   procedure Separate_Lander;
   --  Tells the altimeter simulation that the lander has been separated.
   --  Changes the drag coefficent.

   procedure Shutdown;
   --  Signals the altitude control monitor to terminate.

private

   package Log is new Global.Log (Unit_Name => "ATM");
   --  Logger package instance for Altimeter.

end Altimeter;
