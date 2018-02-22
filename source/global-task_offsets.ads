--  @summary
--  Provides offsets into activation times for the several tasks.
--
--  @description
--  All tasks are activated at Global.Start_Time and run in 10 ms cycles. To
--  distribute the load, these offsets are added to the activation time, so
--  tasks run interleaved.
package Global.Task_Offsets is

   Altitude_Task : Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (0);
   --  Altitude task runs at exactly activation time.

   Engine_Task   : Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (2500);
   --  The engine task runs with an offset of 2.5 millisecond.

   TD_Monitor    : Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (5000);
   --  The touchdown monitor runs with an offset of 5 milliseconds.

   SIM_Task      : Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (7500);
   --  The simulator task runs with an offset of 7.5 milliseconds.

end Global.Task_Offsets;
