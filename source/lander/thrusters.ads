--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Global;
with Shared_Types;

--  @summary
--  Provides simulated thruster control.
--
--  @description
--  Keeps track of thruster commands and provides the resulting thruster state.
package Thrusters is

   procedure Enable;
   --  Enable thruster.
   --
   --  Thrusters are not enabled if an Out_Of_Fuel signal has been emitted in
   --  the past.

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

   procedure Out_Of_Fuel;
   --  Disables thrusters terminally as they ran out of fuel. Further Enable
   --  commands are being ignored.

   procedure Shutdown (Source : Shared_Types.Legs_Index);
   --  Shutdown thrusters terminally due to touchdown signal from the given leg.
   --  @param Source The landing leg which detected the touchdown signal.

private

   package Log is new Global.Log (Unit_Name => "THR");
   --  Logger package instance for Thruster.

end Thrusters;
