with Ada.Real_Time;

package Altimeter with SPARK_Mode is

   type Height   is delta 0.125 range 0.0 .. 200_000.0;
   type Velocity is delta 0.125 range 0.0 .. 100_000.0;

   -- Sample cycle of simulated height sensor.
   Cycle         : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.Milliseconds (10);

   Acceleration  : constant          := 3.8; -- m/s**2
   Base_Velocity : constant Velocity := 0.0;
   Base_Height   : constant Height   := 2_000.0;

   function Current_Height return Height;
   function Current_Velocity return Velocity;
   function Image (H : Height) return String;
   function Image (V : Velocity) return String;

end Altimeter;
