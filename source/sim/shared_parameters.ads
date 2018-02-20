with Shared_Types;

-- @summary
-- All shared parameters.
--
-- @description
-- Stores all shared parameters. This is a Shared_Passive package, thus the
-- values stored within here are shared between all partitions.
-- There is no public interface, for read and write access to the parameters,
-- see the corresponding child packages.
package Shared_Parameters with
  Shared_Passive => True
is

private

   use type Shared_Types.Acceleration;

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   --  The velocity considered safe for landing.
   Shared_Safe_Landing_Velocity   : Shared_Types.Velocity  :=     2.500; -- m/s

   --  Target landing velocity for thruster control.
   Shared_Target_Landing_Velocity : Shared_Types.Velocity  :=     2.375; -- m/s

   -- Altitude at which the simulation starts.
   Shared_Initial_Altitude        : Shared_Types.Altitude  := 3_500.000; -- m

   --  Initial velocity at simulation start.
   Shared_Initial_Velocity        : Shared_Types.Velocity  :=    80.000; -- m/s

   --  Initial amount of fuel on spacecraft.
   Shared_Initial_Fuel_Mass       : Shared_Types.Fuel_Mass :=    64.000; -- kg

   --  Fuel flow rate when Thrusters are (fully) enabled.
   Shared_Fuel_Flow_Rate          : Shared_Types.Fuel_Mass :=     4.500; -- kg/s

   --  Acceleration of space craft when thrusters are enabled.
   Shared_Thruster_Acceleration   : Shared_Types.Acceleration
     := -22.265_999_794_006_347_656_250; -- equals -22.26 m/s² (6 g(Mars))

   pragma Warnings (On, "instance does not use primitive operation ""*""");

end Shared_Parameters;
