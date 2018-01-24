--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Shared_Types;

package Touchdown_Monitor is

   Cycle : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);
   -- Monitoring cycle

   type Health_State is (Unknown, Good, Bad);
   type Run_State    is (Not_Started, Started, Enabled, Terminated);

   procedure Start;
   procedure Enable;
   procedure Shutdown;
   function Current_State (Leg : Shared_Types.Legs_Index) return Run_State;

end Touchdown_Monitor;
