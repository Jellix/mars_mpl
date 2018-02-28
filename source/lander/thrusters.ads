--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Global;
with Shared_Types;

--  @summary
--  Provides simulated thruster control, thruster state, and fuel level tracker.
--
--  @description
--  Depending on given thruster commands and resulting thruster state and the
--  parametrized fuel flow rate, the simulated total burn time and remaining
--  fuel mass is calculated.
--  It uses a cyclic task to keep track of the duration, the thruster was on and
--  estimates the fuel consumption over time. Once the fuel tank is empty, the
--  thrusters are signalled to keep disabled, regardless of thruster commands.
package Thrusters is

   function Burn_Time return Duration with
     Volatile_Function;
   --  Total burn time of thrusters.
   --  @return The total thruster burn time since start.

   function Current_Fuel_Mass return Shared_Types.Fuel_Mass
     with Volatile_Function;
   --  The remaining fuel.
   --  @return The remaining fuel mass.

   procedure Enable;
   --  Enable thruster.
   --
   --  Thrusters are not enabled if there is no more an fuel left to burn.

   procedure Disable;
   --  Disable thrusters.

   function Is_Enabled return Boolean
     with Volatile_Function;
   --  Tells if the thruster is currently enabled.
   --  @return True if the thruster is currently enabled, False otherwise.

   function Is_Disabled return Boolean
     with Volatile_Function;
   --  Tells if the thruster is currently disabled.
   --  @return True if the thruster is currently disabled, False otherwise.

   procedure Shutdown (Source : in Shared_Types.Legs_Index);
   --  Shutdown thrusters terminally due to touchdown signal from the given leg.
   --  @param Source The landing leg which detected the touchdown signal.

   procedure Shutdown;
   --  Terminates the fuel monitor task.

private

   package Log is new Global.Log (Unit_Name => "THR");
   --  Logger package instance for Thruster.

end Thrusters;
