--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Global;
with Shared_Types;

package Thrusters is

   procedure Enable;
   procedure Disable;     --  Disable thrusters due to safe landing velocity.
   procedure Out_Of_Fuel; --  Disabled thrusters as they ran out of fuel.

   procedure Shutdown (Source : Shared_Types.Legs_Index);

   function Current_State return Shared_Types.State
     with Volatile_Function;

private

   procedure Trace is new Global.Trace (Unit_Name => "THR");

end Thrusters;
