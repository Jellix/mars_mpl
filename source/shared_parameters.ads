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

   Shared_Dry_Mass : Shared_Types.Fuel_Mass := 290.0;
   --  Dry mass of space craft after heat shield and cruise stage separation in
   --  kg
   --
   --  https://nssdc.gsfc.nasa.gov/nmc/masterCatalog.do?sc=1999-001A:
   --
   --  The launch mass of the spacecraft is approximately 583 kg, including
   --  64 kg of fuel, an 82 kg cruise stage, a 140 kg aeroshell/heatshield, and
   --  the two 3.5 kg microprobes.

   Shared_Exhaust_Velocity : Shared_Types.Velocity := 2300.0;
   --  Exhaust velocity of fuel when thruster are enabled in m/s.
   --  Hydrazine engine is rated as having a specific impulse of 230 - 240 s,
   --  which converted to a mass based result in an effective exhaust velocity
   --  of ~2300 m/s.

   Shared_Fuel_Flow_Rate : Shared_Types.Fuel_Mass := 1.500;
   --  Fuel flow rate when Thrusters are (fully) enabled in kg/s.

   Shared_Initial_Altitude : Shared_Types.Altitude := 3_500.000;
   -- Altitude at which the simulation starts in m.

   Shared_Initial_Velocity : Shared_Types.Velocity := 80.000;
   --  Initial velocity at simulation start in m/s.

   Shared_Initial_Fuel_Mass : Shared_Types.Fuel_Mass := 64.000;
   --  Initial amount of fuel on spacecraft in kg.

   Shared_Safe_Landing_Velocity : Shared_Types.Velocity := 2.500;
   --  The velocity considered safe for landing in m/s.

   Shared_Target_Landing_Velocity : Shared_Types.Velocity := 2.375;
   --  Target landing velocity for thruster control in m/s.

   pragma Warnings (On, "instance does not use primitive operation ""*""");

end Shared_Parameters;
