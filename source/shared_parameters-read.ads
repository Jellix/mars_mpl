--  @summary
--  Provides read access to all shared parameters.
--
--  @description
--  Provides read access for each of the shared parameters.
package Shared_Parameters.Read is

   function Dry_Mass return Shared_Types.Vehicle_Mass
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized space craft mass (landing parts).
   --  The mass of the landing parts of the space craft excluding fuel (hence
   --  dry mass).
   --  @return The set dry mass.

   function Exhaust_Velocity return Shared_Types.Meter_Per_Second
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized thruster exhaust velocity.
   --  This is the exhaust velocity of fuel through the thruster when the
   --  thrusters are enabled.
   --  @return The set exhaust velocity.

   function Fuel_Flow_Rate return Shared_Types.Kilogram_Per_Second
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized fuel flow rate.
   --  @return The set fuel flow rate.

   function Initial_Altitude return Shared_Types.Meter
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized initial altitude.
   --  This is the altitide at which the simulation starts.
   --  @return The set initial altitude.

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized initial fuel mass.
   --  @return The set initial fuel mass.

   function Initial_Velocity return Shared_Types.Meter_Per_Second
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized initial velocity.
   --  This is the velocity at which the simulation starts.
   --  @return The set initial velocity.

   function Safe_Landing_Velocity return Shared_Types.Meter_Per_Second
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized safe landing velocity.
   --  This is the velocity at which a touchdown is deemed survivable.
   --  @return The set safe landing velocity.

   function Shortest_On_Time return Shared_Types.On_Time
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized shortest on-time for thrusters.
   --  This is the amount of milliseconds a thruster remains active after it has
   --  been fired.
   --  @return The shortest on-time for thrusters.

   function Target_Landing_Velocity return Shared_Types.Meter_Per_Second
     with
       Inline            => False,
       Volatile_Function => True;
   --  Parametrized target landing velocity.
   --  This is the target velocity for a touchdown (it should be slightly
   --  smaller than Safe_Landing_Velocity).
   --  @return The set target landing velocity.

   function TDM_Bug_Enabled return Boolean
     with
       Inline            => False,
       Volatile_Function => True;
   --  Indicates if the TDM bug shall be enabled or not in the simulation.
   --  @return The state of the bug switch.

private

   function Dry_Mass return Shared_Types.Vehicle_Mass is
     (Shared_Dry_Mass.Get);

   function Exhaust_Velocity return Shared_Types.Meter_Per_Second is
     (Shared_Exhaust_Velocity.Get);

   function Fuel_Flow_Rate return Shared_Types.Kilogram_Per_Second is
     (Shared_Fuel_Flow_Rate.Get);

   function Initial_Altitude return Shared_Types.Meter is
     (Shared_Initial_Altitude.Get);

   function Initial_Velocity return Shared_Types.Meter_Per_Second is
     (Shared_Initial_Velocity.Get);

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Shared_Initial_Fuel_Mass.Get);

   function Safe_Landing_Velocity return Shared_Types.Meter_Per_Second is
     (Shared_Safe_Landing_Velocity.Get);

   function Shortest_On_Time return Shared_Types.On_Time is
      (Shared_Shortest_On_Time.Get);

   function Target_Landing_Velocity return Shared_Types.Meter_Per_Second is
     (Shared_Target_Landing_Velocity.Get);

   function TDM_Bug_Enabled return Boolean is
     (Shared_Bug_Enabled.Get);

end Shared_Parameters.Read;
