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

   Shared_Bug_Enabled : Boolean := False;
   --  Indicates if the original Mars MPL implementation fault in the touchdown
   --  monitor shall be simulated (=True) or not (=False).

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   Shared_Safe_Landing_Velocity : Shared_Types.Velocity := 2.500;
   --  The velocity considered safe for landing in m/s.

   Shared_Target_Landing_Velocity : Shared_Types.Velocity := 2.375;
   --  Target landing velocity for thruster control in m/s.

   Shared_Initial_Altitude : Shared_Types.Altitude := 3_500.000;
   -- Altitude at which the simulation starts in m.

   Shared_Initial_Velocity : Shared_Types.Velocity := 80.000;
   --  Initial velocity at simulation start in m/s.

   Shared_Initial_Fuel_Mass : Shared_Types.Fuel_Mass := 64.000;
   --  Initial amount of fuel on spacecraft in kg.

   Shared_Fuel_Flow_Rate : Shared_Types.Fuel_Mass := 4.500;
   --  Fuel flow rate when Thrusters are (fully) enabled in kg/s.

   Shared_Thruster_Acceleration   : Shared_Types.Acceleration
     := -22.265_999_794_006_347_656_250;
   --  Acceleration of space craft when thrusters are enabled in m/s².

   pragma Warnings (On, "instance does not use primitive operation ""*""");

end Shared_Parameters;
