-- @summary
-- Provides planet related equations
--
-- @description
-- Child package of Planets, providing various equations related to them.
with Planets.Parameters;
with Shared_Types;

package Planets.Equations
   with Pure => True
is

   function Gravity (Planet   : in Parameters.Property;
                     Altitude : in Shared_Types.Meter)
                     return Shared_Types.Meter_Per_Square_Second;

end Planets.Equations;
