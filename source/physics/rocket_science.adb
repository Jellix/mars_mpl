with Scalar_Elementary_Functions;

package body Rocket_Science is

   use type Shared_Types.Kilogram;
   use type Shared_Types.Meter_Per_Second;
   use type Shared_Types.Scalar;

   function Delta_V
     (Initial_Wet_Mass : in Shared_Types.Kilogram;
      Current_Wet_Mass : in Shared_Types.Kilogram;
      Exhaust_Velocity : in Shared_Types.Meter_Per_Second)
      return Shared_Types.Meter_Per_Second
   is
      function Ln (X : in Shared_Types.Scalar) return Shared_Types.Scalar renames
        Scalar_Elementary_Functions.Log;
   begin
      return Exhaust_Velocity * Ln (X => Initial_Wet_Mass / Current_Wet_Mass);
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
   function Drag (Current_Wet_Mass : in Shared_Types.Kilogram;
                  Velocity         : in Shared_Types.Meter_Per_Second;
                  Drag_Constant    : in Shared_Types.Scalar)
                  return Shared_Types.Meter_Per_Square_Second
   is
      Upwards_Drag : Shared_Types.Scalar;
   begin
      Upwards_Drag :=
        Drag_Constant * Shared_Types.Scalar (Velocity) * Shared_Types.Scalar (Velocity) / 2.0;

      return
        Shared_Types.Meter_Per_Square_Second
          (Upwards_Drag / Shared_Types.Scalar (Current_Wet_Mass));
   end Drag;

end Rocket_Science;
