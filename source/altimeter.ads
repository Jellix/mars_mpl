--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Global;
with Shared_Types;

package Altimeter is

   -- Sample cycle of simulated height sensor.
   Cycle         : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.Milliseconds (10);

   function Current_Altitude return Shared_Types.Altitude
     with Volatile_Function;

   function Current_Velocity return Shared_Types.Velocity
     with Volatile_Function;

private

   package Log is new Global.Log (Unit_Name => "ATM");

end Altimeter;
