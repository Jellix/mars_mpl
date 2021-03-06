with Ada.Real_Time;
with Task_Safe_Store;

package body Landing_Legs is

   use type Ada.Real_Time.Time;

   type Task_State is (Running, Deployed, Touched_Down, Terminated);
   subtype Events is Task_State range Deployed .. Terminated;

   pragma Warnings (Off, "formal parameter ""L"" is not referenced");
   pragma Warnings (Off, "formal parameter ""R"" is not referenced");
   function "+" (L, R : in Shared_Types.Leg_State)
                 return Shared_Types.Leg_State is
     (raise Program_Error);

   function "-" (L, R : in Shared_Types.Leg_State)
                 return Shared_Types.Leg_State is
     (raise Program_Error);
   pragma Warnings (On, "formal parameter ""L"" is not referenced");
   pragma Warnings (On, "formal parameter ""R"" is not referenced");

   package Leg_Store is new Task_Safe_Store
     (Stored_Type   => Shared_Types.Leg_State,
      Initial_Value => Shared_Types.In_Flight);

   type All_Legs_State is array (Shared_Types.Legs_Index) of Leg_Store.Shelf;

   Legs_State : All_Legs_State;

   protected Task_Control is
      entry Wait_For_Event (Old_State : out Task_State;
                            New_State : out Task_State);
      procedure Emit_Event (Event : in Events);
   private
      Event_Triggered : Boolean    := False;
      Previous_State  : Task_State := Running;
      State           : Task_State := Running;
   end Task_Control;

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

      procedure Emit_Event (Event : in Events) is
      begin
         State           := Event;
         Event_Triggered := True;
      end Emit_Event;

   end Task_Control;

   procedure Deploy is
   begin
      Task_Control.Emit_Event (Event => Deployed);
   end Deploy;

   procedure Read_State (Index : in     Shared_Types.Legs_Index;
                         State :    out Shared_Types.Leg_State) is
   begin
      State := Legs_State (Index).Get;
   end Read_State;

   procedure Read_State (State : out Shared_Types.All_Legs_State) is
   begin
      for I in Shared_Types.Legs_Index loop
         State (I) := Legs_State (I).Get;
      end loop;
   end Read_State;

   procedure Shutdown is
   begin
      Task_Control.Emit_Event (Event => Terminated);
   end Shutdown;

   procedure Touchdown is
   begin
      Task_Control.Emit_Event (Event => Touched_Down);
   end Touchdown;

   task Simulate_Landing_Legs;

   package Sensor_Glitch is

      subtype Glitch_Delay    is Integer range 100 .. 500;
      -- Number of milliseconds the glitch might be delayed since deploy.

      subtype Glitch_Duration is Integer range 5 .. 33;
      -- Number of milliseconds for sensor glitch
      -- (Transient Signal During Deployment).

      procedure Initialize;

      procedure Activate_Glitch;

   end Sensor_Glitch;

   package body Sensor_Glitch is separate;

   task body Simulate_Landing_Legs is
      Previous_State : Task_State := Running;
      Current_State  : Task_State := Running;
   begin
      Sensor_Glitch.Initialize;

      Main_Loop :
      while Current_State /= Terminated loop
         Task_Control.Wait_For_Event (Old_State => Previous_State,
                                      New_State => Current_State);

         if Previous_State /= Current_State then
            case Current_State is

               when Deployed             =>
                  Log.Trace (Message => "Landing legs deployed.");
                  Sensor_Glitch.Activate_Glitch;

               when Touched_Down         =>
                  Set_Legs_Touchdown :
                  for I in Shared_Types.Legs_Index'Range loop
                     Legs_State (I).Set
                       (New_Value => Shared_Types.Touched_Down);
                  end loop Set_Legs_Touchdown;

               when Running | Terminated =>
                  null;
            end case;
         end if;
      end loop Main_Loop;
   exception
      when E : others =>
         Log.Trace (E => E);
   end Simulate_Landing_Legs;

end Landing_Legs;
