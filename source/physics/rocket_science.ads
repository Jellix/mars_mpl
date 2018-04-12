with Shared_Types;

package Rocket_Science is

   --  Tsiolkovsky rocket equation.
   --  Returns the delta V for given fuel masses and exhaust velocity.
   function Delta_V
     (Initial_Wet_Mass : in Shared_Types.Mass;
      Current_Wet_Mass : in Shared_Types.Mass;
      Exhaust_Velocity : in Shared_Types.Velocity) return Shared_Types.Velocity;

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
   function Drag (Current_Wet_Mass : in Shared_Types.Mass;
                  Gravitation      : in Shared_Types.Acceleration;
                  Velocity         : in Shared_Types.Velocity;
                  Drag_Constant    : in Float) return Shared_Types.Acceleration;

end Rocket_Science;
