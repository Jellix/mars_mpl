--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Global;
with Shared_Types;

package Engine is

   Cycle : constant Ada.Real_Time.Time_Span :=
             Ada.Real_Time.Milliseconds (10);

   function Remaining_Fuel return Shared_Types.Fuel_Mass;

   procedure Shutdown;

private

   package Log is new Global.Log (Unit_Name => "ENG");

end Engine;
