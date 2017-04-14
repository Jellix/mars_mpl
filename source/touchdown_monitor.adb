with Ada.Real_Time;
with Ada.Text_IO;
with Thrusters;

package body Touchdown_Monitor is

   use type Ada.Real_Time.Time;

   Cycle : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);

   type Leg_Indicator is
      record
         State  : Boolean;
         Health : Health_State;
      end record;

   task type Touchdown_Monitor_Execute is
      entry Start (Leg_Index : Landing_Legs.Legs_Index);
      entry Enable;
      entry State (Current : out Run_State);
   end Touchdown_Monitor_Execute;

   task body Touchdown_Monitor_Execute is
      Indicator         : Leg_Indicator;
      Monitor_State     : Run_State;
      Event_Enabled     : Boolean;
      Leg               : Landing_Legs.Legs_Index;
      Last_Indicator    : Boolean := False;
      Current_Indicator : Boolean := False;

      Next_Cycle : Ada.Real_Time.Time;
   begin
      --  Initialize local state.
      Monitor_State := Not_Started;
      Indicator     := (State => False, Health => Unknown);
      Event_Enabled := False;

      loop
         select
            accept State (Current : out Run_State) do
               Current := Monitor_State;
            end State;
         or
            when Monitor_State = Not_Started =>
               accept Start (Leg_Index : Landing_Legs.Legs_Index) do
                  Leg := Leg_Index;
               end Start;

               Ada.Text_IO.Put_Line ("Monitoring for leg" & Landing_Legs.Legs_Index'Image (Leg) & " started.");

               Monitor_State := Started;
               Indicator     := (State => False, Health => Good);
               Event_Enabled := False;

               Next_Cycle := Ada.Real_Time.Clock;
         or
            when Monitor_State = Started =>
               accept Enable;

               Ada.Text_IO.Put_Line ("Monitoring for leg" & Landing_Legs.Legs_Index'Image (Leg) & " enabled.");

               if Last_Indicator and Current_Indicator then
                  Indicator.Health := Bad;
               else
                  Event_Enabled := True;
               end if;
         or
            when Monitor_State = Started =>
               delay until Next_Cycle;

               Next_Cycle := Next_Cycle + Cycle;

               Last_Indicator := Current_Indicator;

               if Event_Enabled then
                  begin
                     Current_Indicator := Landing_Legs.Read_State (Index => Leg);
                  exception
                     when Landing_Legs.IO_Error =>
                        Indicator.Health := Bad;
                  end;
               else
                  Current_Indicator := False;
               end if;

               -- Set indicator state to True only once.
               if Last_Indicator and Current_Indicator then
                  Indicator.State := True;
               end if;

               if Indicator = (State => True, Health => Good) and Event_Enabled then
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

   function Current_State (Leg : Landing_Legs.Legs_Index) return Run_State is
     New_State : Run_State;
   begin
      Legs (Leg).State (New_State);
      return New_State;
   end Current_State;

end Touchdown_Monitor;
