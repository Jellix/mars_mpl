with Shared_Types;
with Planets.Parameters;

package Planets.Equations is

   function Gravity (Planet   : in Planet_Name;
                     Altitude : in Shared_Types.Meter)
                     return Shared_Types.Meter_Per_Square_Second;

private

   Big_G : constant Shared_Types.Scalar := 6.67408E-11;
   -- gravitational constant

   use type Shared_Types.Scalar;

   function Gravity (Planet   : in Planet_Name;
                     Altitude : in Shared_Types.Meter)
                     return Shared_Types.Meter_Per_Square_Second is
     (Shared_Types.Meter_Per_Square_Second
          (Big_G * Parameters.Mass (Planet)
             / (Parameters.Radius (Planet) + Shared_Types.Scalar (Altitude)) ** 2));

end Planets.Equations;
