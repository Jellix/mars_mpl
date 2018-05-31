package body Planets.Equations is

   use type Shared_Types.Scalar;

   function Gravity (Planet   : in Parameters.Property;
                     Altitude : in Shared_Types.Meter)
                     return Shared_Types.Meter_Per_Square_Second
   is
      M : constant Shared_Types.Scalar := Planet.Estimated_Mass;
      R : constant Shared_Types.Scalar :=
            Planet.Mean_Radius + Shared_Types.Scalar (Altitude);
   begin
      return Shared_Types.Meter_Per_Square_Second (Big_G * M / (R * R));
   end Gravity;

end Planets.Equations;
