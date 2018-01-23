-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Planets.Parameters;

package Altimeter is

   Altitude_Resolution : constant := 1.0 / 2.0 ** 9;
   type Altitude is delta Altitude_Resolution range 0.0 .. 2.0 ** 23 - Altitude_Resolution
     with Size => 32;

   Velocity_Resolution : constant := 1.0 / 2.0 ** 18;
   type Velocity is delta Velocity_Resolution range -2.0 ** 10 .. 2.0 ** 13 - Velocity_Resolution
     with Size => 32;

   -- Sample cycle of simulated height sensor.
   Cycle         : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.Milliseconds (10);

   Gravity                 : constant Float    := Planets.Parameters.Gravity (Of_Planet => Planets.Mars); -- m/s**2
   Initial_Velocity        : constant Velocity :=    80.000; -- m/s
   Safe_Landing_Velocity   : constant Velocity :=     2.500; -- m/s
   Initial_Altitude        : constant Altitude := 3_500.000; -- m

   function Current_Altitude return Altitude
     with Volatile_Function;

   function Current_Velocity return Velocity
     with Volatile_Function;

   function Image (A : Altitude) return String;
   function Image (V : Velocity) return String;

end Altimeter;
