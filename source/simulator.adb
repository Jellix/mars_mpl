with Ada.Real_Time;
with Ada.Text_IO;

with Altimeter;
with Landing_Legs;
with Touchdown_Monitor;

procedure Simulator is
   All_Monitors_Dead    : Boolean := False;
   Legs_Deployed        : Boolean := False;
   Monitor_Enabled      : Boolean := False;
   Last_Reported_Height : Altimeter.Height_Above_Ground :=
                            Altimeter.Height_Above_Ground'Last;
   Cycle                : constant Ada.Real_Time.Time_Span :=
                            Ada.Real_Time.Milliseconds (100);
   Next_Cycle           : Ada.Real_Time.Time;

   use type Ada.Real_Time.Time;
   use type Altimeter.Height_Above_Ground;
   use type Touchdown_Monitor.Run_State;
begin
   Ada.Text_IO.Put_Line ("Starting monitors...");
   Touchdown_Monitor.Start;

   Next_Cycle := Ada.Real_Time.Clock + Cycle;

   while Altimeter.Current_Height > 0 loop
      declare
         Current_Height : constant Altimeter.Height_Above_Ground :=
                            Altimeter.Current_Height;
      begin
         if not Legs_Deployed and Current_Height <= 1500 then
            Ada.Text_IO.Put_Line ("Landing legs deployed.");
            Legs_Deployed := True;
         end if;

         if not Monitor_Enabled and Current_Height <= 40 then
            Ada.Text_IO.Put_Line ("Enabling monitors...");
            Touchdown_Monitor.Enable;
            Monitor_Enabled := True;
         end if;

         if abs (Last_Reported_Height - Current_Height) > 10 then
            Ada.Text_IO.Put_Line ("Height above surface:" &
                                    Altimeter.Image (Current_Height) & ".");
            Last_Reported_Height := Current_Height;
         end if;
      end;

      delay until Next_Cycle;
      Next_Cycle := Next_Cycle + Cycle;
   end loop;

   All_Monitors_Dead := False;

   while not All_Monitors_Dead loop
      All_Monitors_Dead :=
        (for all Leg in Landing_Legs.Legs_Index'Range =>
           Touchdown_Monitor.Current_State (Leg) /= Touchdown_Monitor.Started);

      if All_Monitors_Dead then
         Ada.Text_IO.Put_Line ("All monitors finished.");
         Touchdown_Monitor.Shutdown;
         Landing_Legs.Shutdown;
      end if;
   end loop;
end Simulator;
