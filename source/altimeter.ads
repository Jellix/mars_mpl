-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;

package Altimeter is

   type Height   is delta 1.0 / 2.0 ** 9 range        0.0 .. 200_000.0;
   type Velocity is delta 1.0 / 2.0 ** 9 range -100_000.0 .. 100_000.0;

   -- Sample cycle of simulated height sensor.
   Cycle         : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.Milliseconds (10);

   Acceleration  : constant          := 3.8; -- m/s**2
   Base_Velocity : constant Velocity := -120.0;
   Base_Height   : constant Height   := 2_000.0;

   procedure Current_Height (H : out Height);
   procedure Current_Velocity (V : out Velocity);
   function Image (H : Height) return String;
   function Image (V : Velocity) return String;

end Altimeter;
