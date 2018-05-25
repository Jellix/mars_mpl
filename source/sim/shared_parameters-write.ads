--  @summary
--  Provides write access to the shared parameters.
--
--  @description
--  Provides subroutines to change parametrization prior to simulation start.
package Shared_Parameters.Write is

   procedure Dry_Mass (Value : in Shared_Types.Vehicle_Mass)
     with Inline => False;
   --  Set a new value for the space craft dry mass.
   --  @param Value The new dry mass to be set.

   procedure Exhaust_Velocity (Value : in Shared_Types.Meter_Per_Second)
     with Inline => False;
   --  Set a new value for the thruster exhaust velocity.
   --  @param Value The new thruster exhaust velocity to be set.

   procedure Fuel_Flow_Rate (Value : in Shared_Types.Kilogram_Per_Second)
     with Inline => False;
   --  Set a new value for the fuel flow rate.
   --  @param Value The new fuel flow rate to be set.

   procedure Initial_Altitude (Value : in Shared_Types.Meter)
     with Inline => False;
   --  Set a new value for the initial altitude.
   --  @param Value The new initial altitude to be set.

   procedure Initial_Attitude (Value : in Shared_Types.Degree)
     with Inline => False;
   --  Set a new value for the initial attitude.
   --  @param Value The new entry attitude to be set.

   procedure Initial_Fuel_Mass (Value : in Shared_Types.Fuel_Mass)
     with Inline => False;
   --  Set a new value for the initial fuel mass.
   --  @param Value The new initial fuel mass to be set.

   procedure Initial_Velocity (Value : in Shared_Types.Meter_Per_Second)
     with Inline => False;
   --  Set a new value for the initial velocity.
   --  @param Value The new initial velocity to be set.

   procedure Shortest_On_Time (Value : in Shared_Types.On_Time)
     with Inline => False;
   --  Set a new value for the shortest on-time.
   --  @param Value The new shortest on-time to be set.

   procedure TDM_Bug_Enabled (Value : in Boolean)
     with Inline => False;
   --  Set a new value for the choice of enabling the bug in the touchdown
   --  monitor.
   --  @param Value The new value to be set.

end Shared_Parameters.Write;
