with Shared_Types;

package Rocket_Science
  with Pure => True
is

   --  Tsiolkovsky rocket equation.
   --  Returns the delta V for given fuel masses and exhaust velocity.
   function Delta_V
     (Initial_Wet_Mass : in Shared_Types.Kilogram;
      Current_Wet_Mass : in Shared_Types.Kilogram;
      Exhaust_Velocity : in Shared_Types.Meter_Per_Second)
      return Shared_Types.Meter_Per_Second;

   --  Drag equations...
   --  See https://www.grc.nasa.gov/www/k-12/airplane/termv.html

   --  Net (downward) force F = D - W (whereas D = drag, W = weight)
   --  W = m * g (mass * gravitational acceleration)
   --  D = Cd * r * V² * A / 2 (whereas Cd = drag coefficient, r = gas density, V = velocity, A = surface area).
   --
   --  Combining (Cd * r * A) into a single drag constant C, we arrive at:
   --  D = C * V² / 2
   --  F = D - W
   --  a = F / m
   function Drag (Current_Wet_Mass : in Shared_Types.Kilogram;
                  Velocity         : in Shared_Types.Meter_Per_Second;
                  Drag_Constant    : in Shared_Types.Scalar)
                  return Shared_Types.Meter_Per_Square_Second;

end Rocket_Science;
