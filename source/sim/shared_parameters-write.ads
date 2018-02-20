--  @summary
--  Provides write access to the shared parameters.
--
--  @description
--  Provides subroutines to change parametrization prior to simulation start.
package Shared_Parameters.Write is

   procedure Fuel_Flow_Rate (Value : in Shared_Types.Fuel_Mass);
   --  Set a new value for the fuel flow rate.
   --  @param Value The new fuel flow rate to be set.

   procedure Initial_Altitude (Value : in Shared_Types.Altitude);
   --  Set a new value for the initial altitude.
   --  @param Value The new initial altitude to be set.

   procedure Initial_Velocity (Value : in Shared_Types.Velocity);
   --  Set a new value for the initial velocity.
   --  @param Value The new initial velocity to be set.

   procedure Initial_Fuel_Mass (Value : in Shared_Types.Fuel_Mass);
   --  Set a new value for the initial fuel mass.
   --  @param Value The new initial fuel mass to be set.

   procedure Thruster_Acceleration (Value : in Shared_Types.Acceleration);
   --  Set a new value for the thruster acceleration.
   --  @param Value The new thruster acceleration to be set. The value shuld be
   --  negative, as it is an acceleration in the opposite direction of gravity.

end Shared_Parameters.Write;
