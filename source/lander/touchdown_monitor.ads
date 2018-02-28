--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Global;
with Shared_Types;

--  @summary
--  The touchdown monitor.
--
--  @description
--  Uses a task to constantly monitor the landing legs' states and shut down the
--  thruster if a touchdown from at least one of the landing legs has been
--  detected.
package Touchdown_Monitor is

   type Health_State is (Unknown, Good, Bad);
   --  General health state of the landing leg hall sensor. Signals from landing
   --  legs previously marked as bad are ignored.
   --  @value Unknown Initial state until the monitoring is actually enabled.
   --  @value Good    Signal of leg seems fine.
   --  @value Bad     Signal of leg considered faulty.

   type Run_State is (Not_Started, Started, Enabled, Terminated);
   --  State of touchdown monitor task.
   --  @value Not_Started Monitor has not been started yet.
   --  @value Started     Monitor has been started, leg signals are monitored,
   --                     but ignored, except for the first transition where the
   --                     health status is sampled.
   --  @value Enabled     Any touchdown signal from one of the legs shall
   --                     disabled the thrusters now.
   --  @value Terminated  Monitor has been terminated.

   procedure Start;
   --  Start touchdown monitoring.

   procedure Enable;
   --  Enable the touchdown logic.

   procedure Shutdown;
   --  Terminate the touchdown monitor.

   function Current_State (Leg : in Shared_Types.Legs_Index) return Run_State
     with Volatile_Function;
   --  Return the running state of the monitoring task for the given landing
   --  leg.
   --  @param Leg The leg designation for which the current running status shall
   --             be returned.
   --  @return The running state of the task monitoring the given Leg.

private

   package Log is new Global.Log (Unit_Name => "TDM");
   --  Logger package instance for Touchdown Monitor.

end Touchdown_Monitor;
