-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;

package Altimeter is

   type Altitude is delta 1.0 / 2.0 ** 24 range        0.0 .. 200_000.0;
   type Velocity is delta 1.0 / 2.0 ** 24 range -100_000.0 .. 100_000.0;

   -- Sample cycle of simulated height sensor.
   Cycle         : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.Milliseconds (10);

   Acceleration     : constant          :=     3.8; -- m/s**2
   Initial_Velocity : constant Velocity :=   178.0; -- m/s
   Initial_Altitude : constant Altitude := 2_500.0; -- m

   function Current_Altitude return Altitude
     with Volatile_Function;

   function Current_Velocity return Velocity
     with Volatile_Function;

   function Image (A : Altitude) return String;
   function Image (V : Velocity) return String;

end Altimeter;
