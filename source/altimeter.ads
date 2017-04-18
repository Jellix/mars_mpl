with Ada.Real_Time;

package Altimeter is

   -- Sample cycle of simulated height sensor.
   Cycle : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);

   type Height_Above_Ground is range 0 .. 200_000;

   function Current_Height return Height_Above_Ground;
   function Image (H : Height_Above_Ground) return String;

end Altimeter;
