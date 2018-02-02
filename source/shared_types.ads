--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

package Shared_Types
  with Pure         => True,
       Remote_Types => True
is

   --  Altimeter
   Altitude_Resolution : constant := 1.0 / 2.0 ** 9;
   type Altitude is delta Altitude_Resolution range
     0.0 .. 2.0 ** 23 - Altitude_Resolution
       with Size => 32;

   Velocity_Resolution : constant := 1.0 / 2.0 ** 18;
   type Velocity is delta Velocity_Resolution range
     -2.0 ** 10 .. 2.0 ** 13 - Velocity_Resolution
       with Size => 32;

   --  Landing Legs
   type Legs_Index is (LL_000, LL_120, LL_240);
   --  Three landing legs at 0, 120, and 240 degree.

   type Leg_State  is (In_Flight, Touched_Down);
   --  Hall sensor reading from I/O card.

   type All_Legs_State is array (Legs_Index) of Leg_State;

   --  Thrusters
   Acceleration_Resolution : constant := 1.0 / 2 ** 12;
   type Acceleration is delta Acceleration_Resolution range
     -2.0 ** 10 .. 2.0 ** 10 - Acceleration_Resolution
       with Size => 32;

   function "*" (A : in Acceleration;
                 T : in Duration) return Velocity
     with Inline => True;

   function "*" (V : in Velocity;
                 T : in Duration) return Altitude
     with Inline => True;

   function "*" (V     : in Velocity;
                 Scale : in Float) return Velocity
     with Inline => True;

   type State is (Disabled, Enabled);

   --  Engine
   Fuel_Resolution : constant := 1.0 / 2.0 ** 25;
   type Fuel_Mass is delta Fuel_Resolution range
     0.0 .. 2.0 ** 7 - Fuel_Resolution
       with Size => 32;

private

   function "*" (A : in Acceleration;
                 T : in Duration) return Velocity is
     (Velocity (A * Acceleration (T)));

   function "*" (V : in Velocity;
                 T : in Duration) return Altitude is
     (Altitude (V * Velocity (T)));

   function "*" (V     : in Velocity;
                 Scale : in Float) return Velocity is
     (Velocity (Float (V) * Scale));

end Shared_Types;
