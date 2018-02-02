with Planets.Parameters;
with Shared_Types;

package Parametrization is

   use type Shared_Types.Velocity;

   Gravity                 : constant Float
     := Planets.Parameters.Gravity (Of_Planet => Planets.Mars); -- m/s**2

   Initial_Velocity        : constant Shared_Types.Velocity  :=    80.0; -- m/s
   Safe_Landing_Velocity   : constant Shared_Types.Velocity  :=     2.5; -- m/s
   Target_Landing_Velocity : constant Shared_Types.Velocity  :=
                               Safe_Landing_Velocity * 0.95;

   Initial_Altitude        : constant Shared_Types.Altitude  := 3_500.0; -- m
   Initial_Fuel_Mass       : constant Shared_Types.Fuel_Mass :=    64.0; -- kg

   Fuel_Flow_Rate          : constant Shared_Types.Fuel_Mass
     := 0.044_999_986_886_978_149_414_062_500; -- equals ~4.5 kg/s

   Thruster_Acceleration   : constant Shared_Types.Acceleration :=
                             Shared_Types.Acceleration
                               (Float'(-6.0) *
                                Planets.Parameters.Gravity
                                  (Of_Planet => Planets.Mars)); -- m/s**2
   --  Acceleration of space craft when thrusters are enabled.

end Parametrization;
