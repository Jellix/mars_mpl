-- @summary
-- Provides planet specific parameters
--
-- @description
-- Child package of Planets, providing various physical parameters of them.
package Planets.Parameters
  with Pure => True
is

   type Parameter is array (Planet_Name'Range) of Float;

   Gravity : constant Parameter;
   --  The (average) gravity of the given planet in m/s².

   Mass : constant Parameter;
   --  The (estimated) mass of the given planet in kg.

   Radius : constant Parameter;
   --  The (mean) radius of the given planet in m.

private

   Gravity : constant Parameter := (Mercury =>  3.700,
                                    Venus   =>  8.870,
                                    Earth   =>  9.807,
                                    Mars    =>  3.711,
                                    Jupiter => 24.790,
                                    Saturn  => 10.440,
                                    Uranus  =>  8.690,
                                    Neptune => 11.150);

   Mass : constant Parameter := (Mercury => 3.285E23,
                                 Venus   => 4.867E24,
                                 Earth   => 5.972E24,
                                 Mars    => 6.390E23,
                                 Jupiter => 1.898E27,
                                 Saturn  => 5.683E26,
                                 Uranus  => 8.681E25,
                                 Neptune => 1.024E26);

   Radius : constant Parameter := (Mercury =>  2_439_700.0,
                                   Venus   =>  6_051_800.0,
                                   Earth   =>  6_371_000.0,
                                   Mars    =>  3_389_500.0,
                                   Jupiter => 69_911_000.0,
                                   Saturn  => 58_232_000.0,
                                   Uranus  => 25_362_000.0,
                                   Neptune => 24_622_000.0);

end Planets.Parameters;
