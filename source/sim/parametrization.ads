with Planets.Parameters;

package Parametrization is

   Gravity                 : constant Float
     := Planets.Parameters.Gravity (Of_Planet => Planets.Mars); -- m/s**2

end Parametrization;
