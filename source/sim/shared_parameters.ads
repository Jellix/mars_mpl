with Shared_Types;

package Shared_Parameters with
  Shared_Passive => True
is
   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   Initial_Altitude        : Shared_Types.Altitude  := 3_500.000; -- m

   Initial_Velocity        : Shared_Types.Velocity  :=    80.000; -- m/s
   Safe_Landing_Velocity   : Shared_Types.Velocity  :=     2.500; -- m/s
   Target_Landing_Velocity : Shared_Types.Velocity  :=     2.375; -- m/s

   Initial_Fuel_Mass       : Shared_Types.Fuel_Mass :=    64.000; -- kg
   Fuel_Flow_Rate          : Shared_Types.Fuel_Mass
     := 0.044_999_986_886_978_149_414_062_500; -- equals ~4.5 kg/s

   pragma Warnings (On, "instance does not use primitive operation ""*""");

end Shared_Parameters;
