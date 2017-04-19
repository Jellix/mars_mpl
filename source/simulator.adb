--pragma Profile (Ravenscar);
--pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;

with Altimeter;
with Global;
with Landing_Legs;
with Touchdown_Monitor;

procedure Simulator is
   All_Monitors_Dead      : Boolean := False;
   Legs_Deployed          : Boolean := False;
   Monitor_Enabled        : Boolean := False;
   Last_Reported_Height   : Altimeter.Height   := Altimeter.Height'Last;
   Last_Reported_Velocity : Altimeter.Velocity := Altimeter.Velocity'Last;
   Cycle                  : constant Ada.Real_Time.Time_Span :=
                            Ada.Real_Time.Milliseconds (100);
   Next_Cycle             : Ada.Real_Time.Time;

   use type Ada.Real_Time.Time;
   use type Altimeter.Height;
   use type Altimeter.Velocity;
   use type Touchdown_Monitor.Run_State;
begin
   Global.Log ("Starting monitors...");
   Touchdown_Monitor.Start;

   Next_Cycle := Global.Start_Time + Cycle;

   while Altimeter.Current_Height > 0.0 loop
      declare
         Current_Height   : constant Altimeter.Height   :=
                              Altimeter.Current_Height;
         Current_Velocity : constant Altimeter.Velocity :=
                              Altimeter.Current_Velocity;
      begin
         if not Legs_Deployed and Current_Height <= 1500.0 then
            Landing_Legs.Deploy;
            Legs_Deployed := True;
         end if;

         if not Monitor_Enabled and Current_Height <= 40.0 then
            Global.Log ("Enabling monitors...");
            Touchdown_Monitor.Enable;
            Monitor_Enabled := True;
         end if;

         if abs (Last_Reported_Height - Current_Height) > 10.0 then
            Global.Log ("Height above surface:" &
                          Altimeter.Image (Current_Height) & ".");
            Last_Reported_Height := Current_Height;
         end if;

         if abs (Last_Reported_Velocity - Current_Velocity) > 10.0 then
            Global.Log ("Velocity:" &
                          Altimeter.Image (Current_Velocity) & ".");
            Last_Reported_Velocity := Current_Velocity;
         end if;
      end;

      delay until Next_Cycle;
      Next_Cycle := Next_Cycle + Cycle;
   end loop;

   Landing_Legs.Touchdown;

   All_Monitors_Dead := False;

   while not All_Monitors_Dead loop
      All_Monitors_Dead :=
        (for all Leg in Landing_Legs.Legs_Index'Range =>
           Touchdown_Monitor.Current_State (Leg) /= Touchdown_Monitor.Started);

      if All_Monitors_Dead then
         Global.Log ("All monitors finished.");
         Touchdown_Monitor.Shutdown;
         Landing_Legs.Shutdown;
      end if;
   end loop;
end Simulator;
