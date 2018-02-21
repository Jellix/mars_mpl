with Planets.Parameters;

package Parametrization is

   Gravity : constant Float
     := Planets.Parameters.Gravity (Of_Planet => Planets.Mars);
   -- Gravity of the Mars in m/s².

end Parametrization;
