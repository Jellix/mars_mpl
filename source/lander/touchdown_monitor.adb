with Configuration.Task_Offsets;
with Landing_Legs;
with Shared_Parameters.Read;
with Thrusters;

package body Touchdown_Monitor is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Leg_State;

   TDM_Bug_Enabled : constant Boolean := Shared_Parameters.Read.TDM_Bug_Enabled;

   type Leg_Indicator is
      record
         State  : Shared_Types.Leg_State;
         Health : Health_State;
      end record;
   Default_Indicator : constant Leg_Indicator
     := (State  => Shared_Types.In_Flight,
         Health => Unknown);

   protected type Task_Control is
      procedure TC_Start;
      procedure TC_Enable;
      procedure TC_Shutdown;
      function TC_State return Run_State;
   private
      State : Run_State := Not_Started;
   end Task_Control;

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
   Legs_Task    : array (Shared_Types.Legs_Index) of Touchdown_Monitor_Execute;
   pragma Unreferenced (Legs_Task);

   Legs_Control : array (Shared_Types.Legs_Index) of Task_Control;

   Assign_Leg : Landing_Legs.Leg_Iterator;

   task body Touchdown_Monitor_Execute is
      Indicator         : Leg_Indicator          := Default_Indicator;
      Event_Enabled     : Boolean                := False;
      Last_Indicator    : Shared_Types.Leg_State := Shared_Types.In_Flight;
      Current_Indicator : Shared_Types.Leg_State := Shared_Types.In_Flight;
      Old_Run_State     : Run_State              := Not_Started;
      Current_Run_State : Run_State              := Not_Started;
      Leg               : Shared_Types.Legs_Index;
      Next_Cycle        : Ada.Real_Time.Time;
   begin
      --  Initialize local state.
      Assign_Leg.Next (The_Leg => Leg);
      Next_Cycle
        := Global.Start_Time + Configuration.Task_Offsets.TD_Monitor (Leg);

      declare
         Legs_Monitoring : constant String
           := "Monitoring for leg " & Shared_Types.Legs_Index'Image (Leg);
      begin
         while Current_Run_State /= Terminated loop
            delay until Next_Cycle;
            Next_Cycle := Next_Cycle + Cycle;

            Old_Run_State     := Current_Run_State;
            Current_Run_State := Legs_Control (Leg).TC_State;

            if Old_Run_State /= Current_Run_State then
               case Current_Run_State is
                  when Started =>
                     Log.Trace (Message => Legs_Monitoring & " started.");

                     Indicator :=
                       Leg_Indicator'(State  => Shared_Types.In_Flight,
                                      Health => Good);

                  when Enabled =>
                     Log.Trace (Message => Legs_Monitoring & " enabled.");

                     if
                       Last_Indicator    = Shared_Types.Touched_Down and then
                       Current_Indicator = Shared_Types.Touched_Down
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
                 (TDM_Bug_Enabled or else Event_Enabled)
                 -- Bug is here.
                 and then
                   Last_Indicator    = Shared_Types.Touched_Down and then
                   Current_Indicator = Shared_Types.Touched_Down
               then
                  Indicator.State := Shared_Types.Touched_Down;
               end if;

               if
                 Event_Enabled and then
                 Indicator = (State  => Shared_Types.Touched_Down,
                              Health => Good)
               then
                  Thrusters.Shutdown (Source => Leg);
                  Legs_Control (Leg).TC_Shutdown;
                  Event_Enabled := False;
               end if;
            end if;
         end loop;
      end;
   exception
      when E : others =>
         Log.Trace (E => E);
   end Touchdown_Monitor_Execute;

   function Current_State (Leg : Shared_Types.Legs_Index) return Run_State is
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
