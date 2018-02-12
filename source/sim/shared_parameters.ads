with Shared_Types;

package Shared_Parameters with
  Shared_Passive => True
is

   use type Shared_Types.Acceleration;

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   -- Altitude at which the simulation starts.
   Initial_Altitude        : Shared_Types.Altitude  := 3_500.000; -- m

   --  Initial velocity at simulation start.
   Initial_Velocity        : Shared_Types.Velocity  :=    80.000; -- m/s

   --  The velocity considered safe for landing.
   Safe_Landing_Velocity   : Shared_Types.Velocity  :=     2.500; -- m/s

   --  Target landing velocity for thruster control.
   Target_Landing_Velocity : Shared_Types.Velocity  :=     2.375; -- m/s

   --  Initial amount of fuel on spacecraft.
   Initial_Fuel_Mass       : Shared_Types.Fuel_Mass :=    64.000; -- kg

   --  Fuel flow rate when Thrusters are (fully) enabled.
   Fuel_Flow_Rate          : Shared_Types.Fuel_Mass
     := 0.044_999_986_886_978_149_414_062_500; -- equals ~4.5 kg/s

   --  Acceleration of space craft when thrusters are enabled.
   Thruster_Acceleration   : Shared_Types.Acceleration
     := -22.265_999_794_006_347_656_250; -- equals -22.26 m/s² (6 g(Mars))

   pragma Warnings (On, "instance does not use primitive operation ""*""");

end Shared_Parameters;
