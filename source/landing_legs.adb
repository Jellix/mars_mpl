--pragma Profile (Ravenscar);
--pragma Partition_Elaboration_Policy (Sequential);

with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Real_Time.Timing_Events;

with Global;

package body Landing_Legs with
     Spark_Mode => Off is

   use type Ada.Real_Time.Time;

   Leg : array (Legs_Index) of Leg_State with
      Atomic_Components;

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
      procedure Trigger_Timeout (Event : in out Ada.Real_Time.Timing_Events.Timing_Event);
      entry Wait_For_Event (Old_State : out Task_State;
                            New_State : out Task_State);
   private
      Event_Triggered : Boolean    := False;
      Previous_State  : Task_State := Running;
      State           : Task_State := Running;
   end Task_Control;

   protected body Task_Control is
      entry Wait_For_Event (Old_State : out Task_State;
                            New_State : out Task_State) when Event_Triggered is
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

      procedure Trigger_Touchdown is
      begin
         State           := Touched_Down;
         Event_Triggered := True;
      end Trigger_Touchdown;

      procedure Trigger_Shutdown is
      begin
         State           := Terminated;
         Event_Triggered := True;
      end Trigger_Shutdown;

      procedure Trigger_Timeout
        (Event : in out Ada.Real_Time.Timing_Events.Timing_Event)
      is
         pragma Unreferenced (Event);
      begin
         Event_Triggered := True;
      end Trigger_Timeout;
   end Task_Control;

   task body Simulate_Landing_Legs is
      Legs_G : Random_Leg.Generator;
      Time_G : Random_Time.Generator;

      Num_Spurious_Signals : Natural    := 3;
      Previous_State       : Task_State := Running;
      Current_State        : Task_State := Running;
      TE                   : Ada.Real_Time.Timing_Events.Timing_Event;
   begin
      Random_Leg.Reset (Gen => Legs_G);
      Random_Time.Reset (Gen => Time_G);

      Ada.Real_Time.Timing_Events.Set_Handler (Event   => TE,
                                               At_Time => Global.Start_Time,
                                               Handler => Task_Control.Trigger_Timeout'Access);

      while Current_State /= Terminated loop
         Task_Control.Wait_For_Event (Previous_State, Current_State);

         if Previous_State /= Current_State then
            case Current_State is
               when Deployed =>
                  Global.Log ("Landing legs deployed.");
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
                       Random_Time.Random (Time_G);
                  begin
                     Global.Log
                       ("Landing leg " &
                        Legs_Index'Image (Selected_Leg) &
                        " triggered for" &
                        Sensor_Glitch'Image (MS_Triggered) &
                        " ms.");
                     Leg (Selected_Leg) := Touched_Down;
                     delay until Ada.Real_Time.Clock +
                       Ada.Real_Time.Milliseconds (MS_Triggered);
                     Leg (Selected_Leg) := In_Flight;
                  end;

                  Num_Spurious_Signals := Num_Spurious_Signals - 1;

                  Ada.Real_Time.Timing_Events.Set_Handler
                    (Event   => TE,
                     At_Time => Ada.Real_Time.Clock + Ada.Real_Time.Milliseconds (100),
                     Handler => Task_Control.Trigger_Timeout'Access);
               end if;

            when Running | Touched_Down | Terminated =>
               null;
         end case;
      end loop;
   end Simulate_Landing_Legs;

   function Read_State
     (Index : in Legs_Index) return Leg_State is
     (Leg (Index)) with
      Spark_Mode => Off;

   procedure Deploy is
   begin
      Task_Control.Trigger_Deploy;
   end Deploy;

   procedure Touchdown is
   begin
      Task_Control.Trigger_Touchdown;
   end Touchdown;

   procedure Shutdown is
   begin
      Task_Control.Trigger_Shutdown;
   end Shutdown;

end Landing_Legs;
