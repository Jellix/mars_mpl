with Shared_Types;

package Shared_Parameters with
  Shared_Passive => True
is
   pragma Elaborate_Body (Shared_Parameters);

   use type Shared_Types.Acceleration;

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   -- Altitude at which the simulation starts.
   Initial_Altitude        : Shared_Types.Altitude  := 3_500.000; -- m

   function Read_Initial_Altitude return Shared_Types.Altitude is
     (Initial_Altitude);

   procedure Write_Initial_Altitude (Value : in Shared_Types.Altitude);

   --  Initial velocity at simulation start.
   Initial_Velocity        : Shared_Types.Velocity  :=    80.000; -- m/s

   function Read_Initial_Velocity return Shared_Types.Velocity is
     (Initial_Velocity);

   procedure Write_Initial_Velocity (Value : in Shared_Types.Velocity);

   --  The velocity considered safe for landing.
   Safe_Landing_Velocity   : Shared_Types.Velocity  :=     2.500; -- m/s

   --  Target landing velocity for thruster control.
   Target_Landing_Velocity : Shared_Types.Velocity  :=     2.375; -- m/s

   --  Initial amount of fuel on spacecraft.
   Initial_Fuel_Mass       : Shared_Types.Fuel_Mass :=    64.000; -- kg

   function Read_Initial_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Initial_Fuel_Mass);

   procedure Write_Initial_Fuel_Mass (Value : in Shared_Types.Fuel_Mass);

   --  Fuel flow rate when Thrusters are (fully) enabled.
   Fuel_Flow_Rate          : Shared_Types.Fuel_Mass :=     4.500; -- kg/s

   function Read_Fuel_Flow_Rate return Shared_Types.Fuel_Mass is
     (Fuel_Flow_Rate);

   procedure Write_Fuel_Flow_Rate (Value : in Shared_Types.Fuel_Mass);

   --  Acceleration of space craft when thrusters are enabled.
   Thruster_Acceleration   : Shared_Types.Acceleration
     := -22.265_999_794_006_347_656_250; -- equals -22.26 m/s² (6 g(Mars))

   function Read_Thruster_Acceleration return Shared_Types.Acceleration is
     (Thruster_Acceleration);

   procedure Write_Thruster_Acceleration (Value : in Shared_Types.Acceleration);

   pragma Warnings (On, "instance does not use primitive operation ""*""");

end Shared_Parameters;
