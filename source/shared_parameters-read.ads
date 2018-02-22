--  @summary
--  Provides read access to all shared parameters.
--
--  @description
--  Provides read access for each of the shared parameters.
package Shared_Parameters.Read is

   function Fuel_Flow_Rate return Shared_Types.Fuel_Mass
     with Volatile_Function;
   --  Parametrized fuel flow rate.
   --  @return The set fuel flow rate.

   function Initial_Altitude return Shared_Types.Altitude
     with Volatile_Function;
   --  Parametrized initial altitude.
   --  This is the altitide at which the simulation starts.
   --  @return The set initial altitude.

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass
     with Volatile_Function;
   --  Parametrized initial fuel mass.
   --  @return The set initial fuel mass.

   function Initial_Velocity return Shared_Types.Velocity
     with Volatile_Function;
   --  Parametrized initial velocity.
   --  This is the velocity at which the simulation starts.
   --  @return The set initial velocity.

   function Safe_Landing_Velocity return Shared_Types.Velocity
     with Volatile_Function;
   --  Parametrized safe landing velocity.
   --  This is the velocity at which a touchdown is deemed survivable.
   --  @return The set safe landing velocity.

   function Target_Landing_Velocity return Shared_Types.Velocity
     with Volatile_Function;
   --  Parametrized target landing velocity.
   --  This is the target velocity for a touchdown (it should be slightly
   --  smaller than Safe_Landing_Velocity).
   --  @return The set target landing velocity.

   function TDM_Bug_Enabled return Boolean
     with Volatile_Function;
   --  Indicates if the TDM bug shall be enabled or not in the simulation.
   --  @return The state of the bug switch.

   function Thruster_Acceleration return Shared_Types.Acceleration
     with Volatile_Function;
   --  Parametrized thruster acceleration.
   --  This is the acceleration the thruster applies to the spacecraft when the
   --  thruster is enabled. The value shuld be negative, as it is an
   --  acceleration in the opposite direction of gravity.
   --  @return The set thruster acceleration.

private

   function Initial_Altitude return Shared_Types.Altitude is
     (Shared_Initial_Altitude);

   function Initial_Velocity return Shared_Types.Velocity is
     (Shared_Initial_Velocity);

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Shared_Initial_Fuel_Mass);

   function Fuel_Flow_Rate return Shared_Types.Fuel_Mass is
     (Shared_Fuel_Flow_Rate);

   function Safe_Landing_Velocity return Shared_Types.Velocity is
     (Shared_Safe_Landing_Velocity);

   function Target_Landing_Velocity return Shared_Types.Velocity is
     (Shared_Target_Landing_Velocity);

   function TDM_Bug_Enabled return Boolean is
     (Shared_Bug_Enabled);

   function Thruster_Acceleration return Shared_Types.Acceleration is
     (Shared_Thruster_Acceleration);

end Shared_Parameters.Read;
