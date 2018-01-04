with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time.Timing_Events;

with Global;

package body Landing_Legs is

   package Real_Time     renames Ada.Real_Time;
   package Timing_Events renames Ada.Real_Time.Timing_Events;

   use type Real_Time.Time;

   type All_Legs_State_Atomic is array (Legs_Index) of Leg_State
     with Atomic_Components;
   Leg : All_Legs_State_Atomic;

   package Random_Leg is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Legs_Index);
   package Random_Time is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Sensor_Glitch);

   task Simulate_Landing_Legs;

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

   protected body Task_Control is
      entry Wait_For_Event (Old_State : out Task_State;
                            New_State : out Task_State) when Event_Triggered
      is
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

      procedure Trigger_Timeout (Event : in out Timing_Events.Timing_Event) is
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

   Timed_Trigger : Timing_Events.Timing_Event;

   function Handler return Timing_Events.Timing_Event_Handler is
      (Task_Control.Trigger_Timeout'Access);

   task body Simulate_Landing_Legs is
      Legs_G : Random_Leg.Generator;
      Time_G : Random_Time.Generator;

      Num_Spurious_Signals : Natural    := 3;
      Previous_State       : Task_State := Running;
      Current_State        : Task_State := Running;
   begin
      Random_Leg.Reset (Gen => Legs_G);
      Random_Time.Reset (Gen => Time_G);

      Timing_Events.Set_Handler (Event   => Timed_Trigger,
                                 At_Time => Global.Start_Time,
                                 Handler => Handler);

      while Current_State /= Terminated loop
         Task_Control.Wait_For_Event (Old_State => Previous_State,
                                      New_State => Current_State);

         if Previous_State /= Current_State then
            case Current_State is
               when Deployed =>
                  Global.Log (Message => "Landing legs deployed.");
               when Touched_Down =>
                  Leg (Legs_Index'Range) := (others => Touched_Down);
               when Running | Terminated =>
                  null;
            end case;
         end if;

         case Current_State is
            when Deployed =>
               if Num_Spurious_Signals > 0 then
                  declare
                     Selected_Leg : constant Legs_Index :=
                       Random_Leg.Random (Gen => Legs_G);
                     MS_Triggered : constant Sensor_Glitch :=
                       Random_Time.Random (Gen => Time_G);
                  begin
                     Global.Log
                       (Message =>
                          "Landing leg " &
                          Legs_Index'Image (Selected_Leg) &
                          " triggered for" &
                          Sensor_Glitch'Image (MS_Triggered) &
                          " ms.");
                     Leg (Selected_Leg) := Touched_Down;
                     delay until
                       Real_Time.Clock +
                         Real_Time.Milliseconds (MS => MS_Triggered);
                     Leg (Selected_Leg) := In_Flight;
                  end;

                  Num_Spurious_Signals := Num_Spurious_Signals - 1;

                  Timing_Events.Set_Handler
                    (Event   => Timed_Trigger,
                     At_Time =>
                       Real_Time.Clock + Real_Time.Milliseconds (MS => 100),
                     Handler => Handler);
               end if;

            when Running | Touched_Down | Terminated =>
               null;
         end case;
      end loop;
   exception
      when E : others =>
         Global.Log (Ada.Exceptions.Exception_Information (E));
   end Simulate_Landing_Legs;

   procedure Deploy is
   begin
      Task_Control.Trigger_Deploy;
   end Deploy;

   procedure Read_State (Index : in     Legs_Index;
                         State :    out Leg_State) is
   begin
      State := Leg (Index);
   end Read_State;

   procedure Read_State (State : out All_Legs_State) is
   begin
      for I in Legs_Index'Range loop
         State (I) := Leg (I);
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

end Landing_Legs;
