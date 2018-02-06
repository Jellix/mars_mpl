with Planets.Parameters;
with Shared_Types;

package Parametrization is

   Gravity                 : constant Float
     := Planets.Parameters.Gravity (Of_Planet => Planets.Mars); -- m/s**2

   Thruster_Acceleration   : constant Shared_Types.Acceleration :=
                             Shared_Types.Acceleration
                               (Float'(-6.0) *
                                Planets.Parameters.Gravity
                                  (Of_Planet => Planets.Mars)); -- m/s**2
   --  Acceleration of space craft when thrusters are enabled.

end Parametrization;
