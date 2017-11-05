with Global;
with Thrusters;

package body Touchdown_Monitor is

   use type Ada.Real_Time.Time, Landing_Legs.Leg_State, Landing_Legs.Legs_Index;

   type Leg_Indicator is
      record
         State  : Landing_Legs.Leg_State;
         Health : Health_State;
      end record;

   protected type Task_Control is
      procedure TC_Start;
      procedure TC_Enable;
      procedure TC_Shutdown;
      function TC_State return Run_State;
   private
      State : Run_State := Not_Started;
   end Task_Control;

   protected Retrieve_Controller is
      procedure Get_Leg (L : out Landing_Legs.Legs_Index);
   private
      Current_Leg : Landing_Legs.Legs_Index := Landing_Legs.Legs_Index'First;
      Out_Of_Legs : Boolean := False;
   end Retrieve_Controller;

   protected body Retrieve_Controller is
      procedure Get_Leg (L : out Landing_Legs.Legs_Index) is
      begin
         if Out_Of_Legs then
            raise Program_Error;
         end if;

         L := Current_Leg;

         if Current_Leg /= Landing_Legs.Legs_Index'Last then
            Current_Leg := Landing_Legs.Legs_Index'Succ (Current_Leg);
         else
            Out_Of_Legs := True;
         end if;
      end Get_Leg;
   end Retrieve_Controller;

   protected body Task_Control is
      procedure TC_Enable is
      begin
         State := Enabled;
      end TC_Enable;

      procedure TC_Shutdown is
      begin
         State := Terminated;
      end TC_Shutdown;

      procedure TC_Start is
      begin
         State := Started;
      end TC_Start;

      function TC_State return Run_State is (State);
   end Task_Control;

   task type Touchdown_Monitor_Execute;
   Legs_Task    : array (Landing_Legs.Legs_Index) of Touchdown_Monitor_Execute;
   pragma Unreferenced (Legs_Task);
   Legs_Control : array (Landing_Legs.Legs_Index) of Task_Control;

   task body Touchdown_Monitor_Execute is
      Leg               : Landing_Legs.Legs_Index;
      Indicator         : Leg_Indicator;
      Event_Enabled     : Boolean;
      Last_Indicator    : Landing_Legs.Leg_State;
      Current_Indicator : Landing_Legs.Leg_State;
      Old_Run_State     : Run_State;
      Current_Run_State : Run_State;
      Next_Cycle        : Ada.Real_Time.Time;
   begin
      --  Initialize local state.
      Indicator         := Leg_Indicator'(State  => Landing_Legs.In_Flight,
                                          Health => Unknown);
      Event_Enabled     := False;
      Last_Indicator    := Landing_Legs.In_Flight;
      Current_Indicator := Landing_Legs.In_Flight;
      Old_Run_State     := Not_Started;
      Current_Run_State := Not_Started;
      Next_Cycle        := Global.Start_Time;

      Retrieve_Controller.Get_Leg (L => Leg);

      while Current_Run_State /= Terminated loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;

         Old_Run_State     := Current_Run_State;
         Current_Run_State := Legs_Control (Leg).TC_State;

         if Old_Run_State /= Current_Run_State then
            case Current_Run_State is
               when Started =>
                  Global.Log (Message =>
                                 "Monitoring for leg " &
                                 Landing_Legs.Legs_Index'Image (Leg) &
                                 " started.");
                  Indicator     :=
                    Leg_Indicator'(State  => Landing_Legs.In_Flight,
                                   Health => Good);
               when Enabled =>
                  Global.Log (Message =>
                                 "Monitoring for leg " &
                                 Landing_Legs.Legs_Index'Image (Leg) &
                                 " enabled.");
                  if
                    Last_Indicator    = Landing_Legs.Touched_Down and then
                    Current_Indicator = Landing_Legs.Touched_Down
                  then
                     Indicator.Health := Bad;
                  else
                     Event_Enabled := True;
                  end if;
               when Not_Started | Terminated =>
                  null;
            end case;
         end if;

         if Current_Run_State /= Terminated then
            Last_Indicator := Current_Indicator;

            begin
               Landing_Legs.Read_State (Index => Leg,
                                        State => Current_Indicator);
            exception
               when Landing_Legs.IO_Error =>
                  Indicator.Health := Bad;
            end;

            -- Bug lies here. While we certainly want to read the state of
            -- the leg in each cycle (for a) health monitoring and
            -- b) constant task workload, we should not enable the setting of
            -- the actual result.
            -- Set indicator state only once.
            if
              -- Event_Enabled and
              Last_Indicator    = Landing_Legs.Touched_Down and then
              Current_Indicator = Landing_Legs.Touched_Down
            then
               Indicator.State := Landing_Legs.Touched_Down;
            end if;

            if
              Event_Enabled and then
              Indicator = (State  => Landing_Legs.Touched_Down,
                           Health => Good)
            then
               Thrusters.Disable (Source => Leg);
               Legs_Control (Leg).TC_Shutdown;
               Event_Enabled := False;
            end if;
         end if;
      end loop;
   end Touchdown_Monitor_Execute;

   function Current_State (Leg : Landing_Legs.Legs_Index) return Run_State is
      New_State : Run_State;
   begin
      New_State := Legs_Control (Leg).TC_State;
      return New_State;
   end Current_State;

   procedure Enable is
   begin
      for Leg of Legs_Control loop
         Leg.TC_Enable;
      end loop;
   end Enable;

   procedure Shutdown is
   begin
      for Leg of Legs_Control loop
         Leg.TC_Shutdown;
      end loop;
   end Shutdown;

   procedure Start is
   begin
      for Leg of Legs_Control loop
         Leg.TC_Start;
      end loop;
   end Start;

end Touchdown_Monitor;
