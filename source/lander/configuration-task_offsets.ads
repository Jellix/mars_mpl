with Shared_Types;

--  @summary
--  Provides offsets into activation times for the several tasks.
--
--  @description
--  All tasks are activated at Global.Start_Time and run in 10 ms cycles. To
--  distribute the load, these offsets are added to the activation time, so
--  tasks run interleaved.
package Configuration.Task_Offsets is

   Altitude_Task : constant Ada.Real_Time.Time_Span := Microseconds (0);
   --  Altitude task runs at exactly activation time.

   Fuel_Monitor  : constant Ada.Real_Time.Time_Span := Microseconds (1500);
   --  The engine task runs with an offset of 2 millisecond.

   type TDM_Offsets is
     array (Shared_Types.Legs_Index) of Ada.Real_Time.Time_Span;

   TD_Monitor : constant TDM_Offsets :=
                  TDM_Offsets'(Shared_Types.LL_000 => Microseconds (3000),
                               Shared_Types.LL_120 => Microseconds (4500),
                               Shared_Types.LL_240 => Microseconds (6000));
   --  The touchdown monitors run with an offset of 3, 4.5, and 6 milliseconds.

   SIM_Task : constant Ada.Real_Time.Time_Span := Microseconds (7500);
   --  The simulator task runs with an offset of 7.5 milliseconds.

end Configuration.Task_Offsets;
