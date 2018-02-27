--  @summary
--  Provides read access to all shared parameters.
--
--  @description
--  Provides read access for each of the shared parameters.
package Shared_Parameters.Read is

   function Dry_Mass return Shared_Types.Fuel_Mass
     with Volatile_Function;
   --  Parametrized space craft mass (landing parts).
   --  The mass of the landing parts of the space craft excluding fuel (hence
   --  dry mass).
   --  @return The set dry mass.

   function Exhaust_Velocity return Shared_Types.Velocity
     with Volatile_Function;
   --  Parametrized thruster exhaust velocity.
   --  This is the exhaust velocity of fuel through the thruster when the
   --  thrusters are enabled.
   --  @return The set exhaust velocity.

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

private

   function Dry_Mass return Shared_Types.Fuel_Mass is
     (Shared_Dry_Mass);

   function Exhaust_Velocity return Shared_Types.Velocity is
     (Shared_Exhaust_Velocity);

   function Fuel_Flow_Rate return Shared_Types.Fuel_Mass is
     (Shared_Fuel_Flow_Rate);

   function Initial_Altitude return Shared_Types.Altitude is
     (Shared_Initial_Altitude);

   function Initial_Velocity return Shared_Types.Velocity is
     (Shared_Initial_Velocity);

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Shared_Initial_Fuel_Mass);

   function Safe_Landing_Velocity return Shared_Types.Velocity is
     (Shared_Safe_Landing_Velocity);

   function Target_Landing_Velocity return Shared_Types.Velocity is
     (Shared_Target_Landing_Velocity);

   function TDM_Bug_Enabled return Boolean is
     (Shared_Bug_Enabled);

   --  The No_Inline pragmas are there to ensure that calls to retrieve shared
   --  parameters are never inlined (even though they are implemented as
   --  expression functions). Otherwise they may not work as expected.
   pragma No_Inline (Dry_Mass);
   pragma No_Inline (Exhaust_Velocity);
   pragma No_Inline (Fuel_Flow_Rate);
   pragma No_Inline (Initial_Altitude);
   pragma No_Inline (Initial_Velocity);
   pragma No_Inline (Initial_Fuel_Mass);
   pragma No_Inline (Safe_Landing_Velocity);
   pragma No_Inline (Target_Landing_Velocity);
   pragma No_Inline (TDM_Bug_Enabled);

end Shared_Parameters.Read;
