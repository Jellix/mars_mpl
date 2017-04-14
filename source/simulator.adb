with Ada.Text_IO;
with Landing_Legs;
with Touchdown_Monitor;

procedure Simulator is
   All_Monitors_Dead : Boolean := False;
   use type Touchdown_Monitor.Run_State;
begin
   Ada.Text_IO.Put_Line ("Starting monitors...");
   Touchdown_Monitor.Start;

   Ada.Text_IO.Put_Line ("Enabling monitors...");
   Touchdown_Monitor.Enable;

   while not All_Monitors_Dead loop
      All_Monitors_Dead := True;

      for Leg in Landing_Legs.Legs_Index'Range loop
         if Touchdown_Monitor.Current_State (Leg) = Touchdown_Monitor.Started then
            All_Monitors_Dead := False;
         end if;
      end loop;
   end loop;
end Simulator;
