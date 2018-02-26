with Ada.Real_Time;
with Shared_Types;

--  @summary
--  Provides task interval (cycle) times for the several tasks.
--
--  @description
--  All tasks currently run in 10 ms cycles.
package Configuration.Cycle_Times is

   Altitude_Task : Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.Milliseconds (MS => 10);

   Fuel_Monitor : Ada.Real_Time.Time_Span :=
                    Ada.Real_Time.Milliseconds (MS => 10);

   type TDM_Cycles is
     array (Shared_Types.Legs_Index) of Ada.Real_Time.Time_Span;

   TD_Monitor : TDM_Cycles
     := (Shared_Types.LL_000 => Ada.Real_Time.Milliseconds (MS => 10),
         Shared_Types.LL_120 => Ada.Real_Time.Milliseconds (MS => 10),
         Shared_Types.LL_240 => Ada.Real_Time.Milliseconds (MS => 10));

end Configuration.Cycle_Times;
