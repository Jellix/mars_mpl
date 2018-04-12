with Ada.Numerics.Elementary_Functions;

package body Rocket_Science is

   use type Shared_Types.Velocity;

   function Delta_V
     (Initial_Wet_Mass : in Shared_Types.Mass;
      Current_Wet_Mass : in Shared_Types.Mass;
      Exhaust_Velocity : in Shared_Types.Velocity) return Shared_Types.Velocity
   is
      function Ln (X : in Float) return Float renames
        Ada.Numerics.Elementary_Functions.Log;
   begin
      return
        Exhaust_Velocity *
          Ln (X => Float (Initial_Wet_Mass / Current_Wet_Mass));
   end Delta_V;

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
                  Drag_Constant    : in Float) return Shared_Types.Acceleration
   is
      Upwards_Drag    : Float;
      Downwards_Force : Float;
   begin
      Upwards_Drag    := Drag_Constant * Float (Velocity) * Float (Velocity) / 2.0;
      Downwards_Force := Upwards_Drag - Float (Current_Wet_Mass * Gravitation);

      return Shared_Types.Acceleration (Downwards_Force / Float (Current_Wet_Mass));
   end Drag;

end Rocket_Science;
