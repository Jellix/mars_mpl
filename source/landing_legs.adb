with Ada.Exceptions;
with Ada.Real_Time.Timing_Events;

with Global;

package body Landing_Legs is

   package Real_Time     renames Ada.Real_Time;
   package Timing_Events renames Ada.Real_Time.Timing_Events;

   use type Real_Time.Time;

   type Task_State is (Running, Deployed, Touched_Down, Terminated);

   protected Task_Control is
      procedure Trigger_Deploy;
      procedure Trigger_Touchdown;
      procedure Trigger_Shutdown;
      procedure Trigger_Timeout (Event : in out Timing_Events.Timing_Event);
      entry Wait_For_Event (Old_State : out Task_State;
                            New_State : out Task_State);
   private
      Event_Triggered : Boolean    := False;
      Previous_State  : Task_State := Running;
      State           : Task_State := Running;
   end Task_Control;

   type All_Legs_State_Atomic is array (Legs_Index) of Leg_State
     with Atomic_Components;

   Legs_State : All_Legs_State_Atomic;

   Timed_Trigger : Timing_Events.Timing_Event;

   protected body Task_Control is

      entry Wait_For_Event (Old_State : out Task_State;
                            New_State : out Task_State)
        when Event_Triggered is
      begin
         Old_State       := Previous_State;
         New_State       := State;
         Previous_State  := State;
         Event_Triggered := False;
      end Wait_For_Event;

      procedure Trigger_Deploy is
      begin
         State           := Deployed;
         Event_Triggered := True;
      end Trigger_Deploy;

      procedure Trigger_Shutdown is
      begin
         State           := Terminated;
         Event_Triggered := True;
      end Trigger_Shutdown;

      procedure Trigger_Timeout (Event : in out Timing_Events.Timing_Event)
      is
         pragma Unreferenced (Event);
      begin
         Event_Triggered := True;
      end Trigger_Timeout;

      procedure Trigger_Touchdown is
      begin
         State           := Touched_Down;
         Event_Triggered := True;
      end Trigger_Touchdown;
   end Task_Control;

   procedure Deploy is
   begin
      Task_Control.Trigger_Deploy;
   end Deploy;

   procedure Read_State (Index : in     Legs_Index;
                         State :    out Leg_State) is
   begin
      State := Legs_State (Index);
   end Read_State;

   procedure Read_State (State : out All_Legs_State) is
   begin
      for The_Leg in Legs_Index'Range loop
         State (The_Leg) := Legs_State (The_Leg);
      end loop;
   end Read_State;

   procedure Shutdown is
   begin
      Task_Control.Trigger_Shutdown;
   end Shutdown;

   procedure Touchdown is
   begin
      Task_Control.Trigger_Touchdown;
   end Touchdown;

   task Simulate_Landing_Legs;

   package Sensor_Glitch is

      subtype Glitch_Delay    is Integer range 100 .. 500;
      -- Number of milliseconds the glitch might be delayed since deploy.

      subtype Glitch_Duration is Integer range 1 .. 37;
      -- Number of milliseconds for sensor glitch.

      procedure Initialize;

      procedure Activate_Glitch;

   end Sensor_Glitch;

   package body Sensor_Glitch is separate;

   task body Simulate_Landing_Legs is
      Previous_State : Task_State := Running;
      Current_State  : Task_State := Running;
   begin
      Sensor_Glitch.Initialize;

      Timing_Events.Set_Handler (Event   => Timed_Trigger,
                                 At_Time => Global.Start_Time,
                                 Handler => Task_Control.Trigger_Timeout'Access);

      while Current_State /= Terminated loop
         Task_Control.Wait_For_Event (Old_State => Previous_State,
                                      New_State => Current_State);

         if Previous_State /= Current_State then
            case Current_State is

               when Deployed             =>
                  Global.Log (Message => "Landing legs deployed.");
                  Sensor_Glitch.Activate_Glitch;

               when Touched_Down         =>
                  Legs_State (Legs_Index'Range) := (others => Touched_Down);

               when Running | Terminated =>
                  null;
            end case;
         end if;
      end loop;
   exception
      when E : others =>
         Global.Log (Ada.Exceptions.Exception_Information (E));
   end Simulate_Landing_Legs;

end Landing_Legs;
