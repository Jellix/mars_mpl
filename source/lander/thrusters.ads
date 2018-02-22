--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Global;
with Shared_Types;

--  @summary
--  Provides simulated thruster control.
--
--  @description
--  Keeps track of thrister commands and provides the resulting thruster state.
package Thrusters is

   procedure Enable;
   --  Enable thruster. Thrusters are not enabled if an Out_Of_Fuel signal has
   --  been emitted in the past.

   procedure Disable;
   --  Disable thrusters.

   procedure Out_Of_Fuel;
   --  Disabled thruster terminally as they ran out of fuel. Further Enable
   --  commands are being ignored.

   procedure Shutdown (Source : Shared_Types.Legs_Index);
   --  Shutdown thrusters unconditionally due touchdown signal from the given
   --  leg.
   --  @param Source The landing leg which detected the touchdown signal.

   function Current_State return Shared_Types.State
     with Volatile_Function;
   --  Current state of thruster.
   --  @return Current thruster state.

private

   package Log is new Global.Log (Unit_Name => "THR");
   --  Logger package instance for Thruster.

end Thrusters;
