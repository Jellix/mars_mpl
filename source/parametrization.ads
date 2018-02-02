with Planets.Parameters;
with Shared_Types;

package Parametrization is

   Gravity               : constant Float
     := Planets.Parameters.Gravity (Of_Planet => Planets.Mars); -- m/s**2

   Initial_Velocity      : constant Shared_Types.Velocity :=    80.000; -- m/s
   Safe_Landing_Velocity : constant Shared_Types.Velocity :=     2.500; -- m/s
   Initial_Altitude      : constant Shared_Types.Altitude := 3_500.000; -- m

   Initial_Fuel_Mass     : constant Shared_Types.Fuel_Mass := 64.0; -- kg

   Fuel_Flow_Rate        : constant Shared_Types.Fuel_Mass
     := 0.044_999_986_886_978_149_414_062_500; -- equals ~4.5 kg/s

   Thruster_Acceleration : constant Shared_Types.Acceleration :=
                             Shared_Types.Acceleration
                               (-6.0 *
                                Planets.Parameters.Gravity
                                  (Of_Planet => Planets.Mars)); -- m/s**2
   --  Acceleration of space craft when thrusters are enabled.

end Parametrization;
