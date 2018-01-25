--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;

with Shared_Types;

package Engine is

   Flow_Rate : constant Shared_Types.Fuel_Mass
     := 0.044_999_986_886_978_149_414_062_500; -- equals ~4.5 kg/s

   Cycle : constant Ada.Real_Time.Time_Span :=
             Ada.Real_Time.Milliseconds (10);

   function Remaining_Fuel return Shared_Types.Fuel_Mass;

   procedure Shutdown;

end Engine;
