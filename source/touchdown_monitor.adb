with Ada.Text_IO;
with Thrusters;

package body Touchdown_Monitor is

   use type Ada.Real_Time.Time, Landing_Legs.Leg_State;

   type Leg_Indicator is
      record
         State  : Landing_Legs.Leg_State;
         Health : Health_State;
      end record;

   task type Touchdown_Monitor_Execute is
      entry Start (Leg_Index : Landing_Legs.Legs_Index);
      entry Enable;
      entry Shutdown;
      entry State (Current : out Run_State);
   end Touchdown_Monitor_Execute;

   task body Touchdown_Monitor_Execute is
      Indicator         : Leg_Indicator;
      Monitor_State     : Run_State;
      Event_Enabled     : Boolean;
      Leg               : Landing_Legs.Legs_Index;
      Last_Indicator    : Landing_Legs.Leg_State := Landing_Legs.In_Flight;
      Current_Indicator : Landing_Legs.Leg_State := Landing_Legs.In_Flight;

      Is_Finished : Boolean := False;
      Next_Cycle  : Ada.Real_Time.Time;
   begin
      --  Initialize local state.
      Monitor_State := Not_Started;
      Indicator     := (State => Landing_Legs.In_Flight, Health => Unknown);
      Event_Enabled := False;

      while not Is_Finished loop
         select
            accept State (Current : out Run_State) do
               Current := Monitor_State;
            end State;
         or
            accept Shutdown do
               Is_Finished := True;
            end Shutdown;
         or
            when Monitor_State = Not_Started =>
               accept Start (Leg_Index : Landing_Legs.Legs_Index) do
                  Leg := Leg_Index;
               end Start;

               Ada.Text_IO.Put_Line ("Monitoring for leg"
                                     & Landing_Legs.Legs_Index'Image (Leg)
                                     & " started.");

               Monitor_State := Started;
               Indicator     := (State  => Landing_Legs.In_Flight,
                                 Health => Good);
               Event_Enabled := False;

               Next_Cycle := Ada.Real_Time.Clock;
         or
            when Monitor_State = Started =>
               accept Enable;

               Ada.Text_IO.Put_Line ("Monitoring for leg"
                                     & Landing_Legs.Legs_Index'Image (Leg)
                                     & " enabled.");

               if
                     Last_Indicator    = Landing_Legs.Touched_Down
                 and Current_Indicator = Landing_Legs.Touched_Down
               then
                  Indicator.Health := Bad;
               else
                  Event_Enabled := True;
               end if;
         or
            when Monitor_State = Started =>
               delay until Next_Cycle;

               Next_Cycle := Next_Cycle + Cycle;

               Last_Indicator := Current_Indicator;

               -- Bug lies here. While we certainly want to read the state of
               -- the leg in each cycle (for a) health monitoring and
               -- b) constant task workload, we should not enable the setting of
               -- the actual result.
               begin
                  Current_Indicator := Landing_Legs.Read_State (Index => Leg);
               exception
                  when Landing_Legs.IO_Error =>
                     Indicator.Health := Bad;
               end;

               -- Set indicator state only once.
               if
                 Event_Enabled and
                 Last_Indicator    = Landing_Legs.Touched_Down and
                 Current_Indicator = Landing_Legs.Touched_Down
               then
                  Indicator.State := Landing_Legs.Touched_Down;
               end if;

               if
                     Indicator = (State  => Landing_Legs.Touched_Down,
                                  Health => Good)
                 and Event_Enabled
               then
                  Thrusters.Disable (Source => Leg);
                  Monitor_State := Not_Started;
                  Event_Enabled := False;
               end if;
         end select;
      end loop;
   end Touchdown_Monitor_Execute;

   Legs : array (Landing_Legs.Legs_Index) of Touchdown_Monitor_Execute;

   procedure Start is
   begin
      for Leg in Legs'Range loop
         Legs (Leg).Start (Leg);
      end loop;
   end Start;

   procedure Enable is
   begin
      for Leg of Legs loop
         Leg.Enable;
      end loop;
   end Enable;

   procedure Shutdown is
   begin
      for Leg of Legs loop
         Leg.Shutdown;
      end loop;
   end Shutdown;

   function Current_State (Leg : Landing_Legs.Legs_Index) return Run_State is
     New_State : Run_State;
   begin
      Legs (Leg).State (New_State);
      return New_State;
   end Current_State;

end Touchdown_Monitor;
