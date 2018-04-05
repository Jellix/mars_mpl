package Shared_Parameters.Default is

   function Dry_Mass return Shared_Types.Vehicle_Mass
     with Inline => True;
   --  Parametrized space craft mass (landing parts).
   --  The mass of the landing parts of the space craft excluding fuel (hence
   --  dry mass).
   --  @return The default dry mass.

   function Exhaust_Velocity return Shared_Types.Velocity
     with Inline => True;
   --  Parametrized thruster exhaust velocity.
   --  This is the exhaust velocity of fuel through the thruster when the
   --  thrusters are enabled.
   --  @return The default exhaust velocity.

   function Fuel_Flow_Rate return Shared_Types.Flow_Rate
     with Inline => True;
   --  Parametrized fuel flow rate.
   --  @return The default fuel flow rate.

   function Initial_Altitude return Shared_Types.Altitude
     with Inline => True;
   --  Parametrized initial altitude.
   --  This is the altitide at which the simulation starts.
   --  @return The default initial altitude.

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass
     with Inline => True;
   --  Parametrized initial fuel mass.
   --  @return The default initial fuel mass.

   function Initial_Velocity return Shared_Types.Velocity
     with Inline => True;
   --  Parametrized initial velocity.
   --  This is the velocity at which the simulation starts.
   --  @return The default initial velocity.

   function Safe_Landing_Velocity return Shared_Types.Velocity
     with Inline => True;
   --  Parametrized safe landing velocity.
   --  This is the velocity at which a touchdown is deemed survivable.
   --  @return The default safe landing velocity.

   function Shortest_On_Time return Shared_Types.On_Time
     with Inline => True;
   --  Parametrized shortest on-time for thrusters.
   --  This is the amount of milliseconds a thruster remains active after it has
   --  been fired.
   --  @return The default shortest on-time for thrusters.

   function Target_Landing_Velocity return Shared_Types.Velocity
     with Inline => True;
   --  Parametrized target landing velocity.
   --  This is the target velocity for a touchdown (it should be slightly
   --  smaller than Safe_Landing_Velocity).
   --  @return The default target landing velocity.

   function TDM_Bug_Enabled return Boolean
     with Inline => True;
   --  Indicates if the TDM bug shall be enabled or not in the simulation.
   --  @return The default state of the bug switch.

private

   function Dry_Mass return Shared_Types.Vehicle_Mass is
     (Dry_Mass_Store.Default_Value);

   function Exhaust_Velocity return Shared_Types.Velocity is
     (Exhaust_Velocity_Store.Default_Value);

   function Fuel_Flow_Rate return Shared_Types.Flow_Rate is
     (Fuel_Flow_Rate_Store.Default_Value);

   function Initial_Altitude return Shared_Types.Altitude is
     (Initial_Altitude_Store.Default_Value);

   function Initial_Velocity return Shared_Types.Velocity is
     (Initial_Velocity_Store.Default_Value);

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Initial_Fuel_Mass_Store.Default_Value);

   function Safe_Landing_Velocity return Shared_Types.Velocity is
     (Safe_Landing_Velocity_Store.Default_Value);

   function Shortest_On_Time return Shared_Types.On_Time is
     (Shortest_On_Time_Store.Default_Value);

   function Target_Landing_Velocity return Shared_Types.Velocity is
     (Target_Landing_Velocity_Store.Default_Value);

   function TDM_Bug_Enabled return Boolean is
     (Bug_Enabled_Store.Default_Value);

end Shared_Parameters.Default;
