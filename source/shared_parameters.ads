with Shared_Types;
with Task_Safe_Store;

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

   pragma Annotate (GnatCheck,
                    Exempt_On,
                    "Global_Variables",
                    "These shared variables are intentional");

   package Bug_Enabled_Store is new Task_Safe_Store ("+" => "or",
                                                     "-" => "xor",
                                                     Stored_Type   => Boolean,
                                                     Initial_Value => False);
   Shared_Bug_Enabled : Bug_Enabled_Store.Shelf;
   --  Indicates if the original Mars MPL implementation fault in the touchdown
   --  monitor shall be simulated (=True) or not (=False).

   package Dry_Mass_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Vehicle_Mass,
                          Initial_Value => 290.0);
   Shared_Dry_Mass : Dry_Mass_Store.Shelf;
   --  Dry mass of space craft after heat shield and cruise stage separation in
   --  kg
   --
   --  https://nssdc.gsfc.nasa.gov/nmc/masterCatalog.do?sc=1999-001A:
   --
   --  The launch mass of the spacecraft is approximately 583 kg, including
   --  64 kg of fuel, an 82 kg cruise stage, a 140 kg aeroshell/heatshield, and
   --  the two 3.5 kg microprobes.

   package Exhaust_Velocity_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Meter_Per_Second,
                          Initial_Value => 2300.0);
   Shared_Exhaust_Velocity : Exhaust_Velocity_Store.Shelf;
   --  Exhaust velocity of fuel when thruster are enabled in m/s.
   --  Hydrazine engine is rated as having a specific impulse of 230 - 240 s,
   --  which converted to a mass based result in an effective exhaust velocity
   --  of ~2300 m/s.

   package Fuel_Flow_Rate_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Kilogram_Per_Second,
                          Initial_Value => 1.500);
   Shared_Fuel_Flow_Rate : Fuel_Flow_Rate_Store.Shelf;
   --  Fuel flow rate when Thrusters are (fully) enabled in kg/s.

   package Initial_Attitude_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Degree,
                          Initial_Value => 0.0);
   Shared_Initial_Attitude : Initial_Attitude_Store.Shelf;
   -- Attitude of space craft when simulation starts in degree.

   package Initial_Altitude_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Meter,
                          Initial_Value => 125_000.0);
   Shared_Initial_Altitude : Initial_Altitude_Store.Shelf;
   -- Altitude at which the simulation starts in m.

   package Initial_Velocity_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Meter_Per_Second,
                          Initial_Value => 6900.000);
   Shared_Initial_Velocity : Initial_Velocity_Store.Shelf;
   --  Initial velocity at simulation start in m/s.

   package Initial_Fuel_Mass_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Fuel_Mass,
                          Initial_Value => 64.000);
   Shared_Initial_Fuel_Mass : Initial_Fuel_Mass_Store.Shelf;
   --  Initial amount of fuel on spacecraft in kg.

   package Safe_Landing_Velocity_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Meter_Per_Second,
                          Initial_Value => 2.500);
   Shared_Safe_Landing_Velocity : Safe_Landing_Velocity_Store.Shelf;
   --  The velocity considered safe for landing in m/s.

   package Shortest_On_Time_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.On_Time,
                          Initial_Value => 16.0);
   Shared_Shortest_On_Time : Shortest_On_Time_Store.Shelf;
   --  Shortest possible on-time for thruster in ms.

   package Target_Landing_Velocity_Store is
     new Task_Safe_Store ("+"           => Shared_Types."+",
                          "-"           => Shared_Types."-",
                          Stored_Type   => Shared_Types.Meter_Per_Second,
                          Initial_Value => 2.375);
   Shared_Target_Landing_Velocity : Target_Landing_Velocity_Store.Shelf;
   --  Target landing velocity for thruster control in m/s.

   pragma Annotate (GnatCheck,
                    Exempt_Off,
                    "Global_Variables");

end Shared_Parameters;
