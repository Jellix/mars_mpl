package Shared_Types
  with Pure => True
is

   --  Altimeter
   Altitude_Resolution : constant := 1.0 / 2.0 ** 9;
   type Altitude is delta Altitude_Resolution range 0.0 .. 2.0 ** 23 - Altitude_Resolution
     with Size => 32;

   Velocity_Resolution : constant := 1.0 / 2.0 ** 18;
   type Velocity is delta Velocity_Resolution range -2.0 ** 10 .. 2.0 ** 13 - Velocity_Resolution
     with Size => 32;

   --  Landing Legs
   type Legs_Index is (LL_000, LL_120, LL_240);
   --  Three landing legs at 0, 120, and 240 degree.

   type Leg_State  is (In_Flight, Touched_Down);
   --  Hall sensor reading from I/O card.

   type All_Legs_State is array (Legs_Index) of Leg_State;

   --  Thrusters
   type State is (Disabled, Enabled);

   --  Engine
   Fuel_Resolution : constant := 1.0 / 2.0 ** 25;
   type Fuel_Mass is delta Fuel_Resolution range 0.0 .. 2.0 ** 7 - Fuel_Resolution
     with Size => 32;

end Shared_Types;
