--  @summary
--  Provides write access to the shared parameters.
--
--  @description
--  Provides subroutines to change parametrization prior to simulation start.
package Shared_Parameters.Write is

   procedure Dry_Mass (Value : in Shared_Types.Fuel_Mass);
   --  Set a new value for the space craft dry mass.
   --  @param Value The new dry mass to be set.

   procedure Exhaust_Velocity (Value : in Shared_Types.Velocity);
   --  Set a new value for the thruster exhaust velocity.
   --  @param Value The new thruster exhaust velocity to be set.

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

   procedure TDM_Bug_Enabled (Value : in Boolean);
   --  Set a new value for the choice of enabling the bug in the touchdown
   --  monitor.
   --  @param Value The new value to be set.

private

   --  The No_Inline pragmas are there to ensure that calls to change shared
   --  parameters are never inlined. Otherwise they may not work as expected.
   pragma No_Inline (Dry_Mass);
   pragma No_Inline (Exhaust_Velocity);
   pragma No_Inline (Fuel_Flow_Rate);
   pragma No_Inline (Initial_Altitude);
   pragma No_Inline (Initial_Velocity);
   pragma No_Inline (Initial_Fuel_Mass);
   pragma No_Inline (TDM_Bug_Enabled);

end Shared_Parameters.Write;
