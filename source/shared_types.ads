--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

package Shared_Types
  with Pure         => True,
       Remote_Types => True
is

   --  Landing Legs
   type Legs_Index is (LL_000, LL_120, LL_240);
   --  Three landing legs at 0, 120, and 240 degree.

   type Leg_State  is (In_Flight, Touched_Down);
   --  Hall sensor reading from I/O card.

   type All_Legs_State is array (Legs_Index) of Leg_State;

   package Meter is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 25;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 64;

      type T is delta R range F .. L - R with Size => S;
   end Meter;

   --  Altimeter
   type Altitude is new Meter.T;

   package Meter_Per_Second is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 15;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 64;

      type T is delta R range F .. L - R with Size => S;
   end Meter_Per_Second;

   type Velocity is new Meter_Per_Second.T;

   package Meter_Per_Square_Second is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 10;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 32;

      type T is delta R range F .. L - R with Size => S;
   end Meter_Per_Square_Second;

   --  Thrusters
   type Acceleration is new Meter_Per_Square_Second.T;

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
   package Kilogram is
      F : constant := 0.0;
      L : constant := 2.0 ** 7;
      R : constant := 1.0 / 2.0 ** 25;
      S : constant := 32;

      type T is delta R range F .. L - R with Size => S;
   end Kilogram;

   --  Engine
   type Fuel_Mass is new Kilogram.T;

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
