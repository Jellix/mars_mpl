-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Altimeter;
with Global;
with GUI;
with Landing_Legs;
with Thrusters;
with Touchdown_Monitor;

procedure Simulator is
   All_Monitors_Dead : Boolean := False;
   Legs_Deployed     : Boolean := False;
   Monitor_Enabled   : Boolean := False;
   Cycle             : constant Ada.Real_Time.Time_Span :=
                         Ada.Real_Time.Milliseconds (MS => 10);
   Next_Cycle        : Ada.Real_Time.Time;

   use type Ada.Real_Time.Time;
   use type Altimeter.Height;
   use type Altimeter.Velocity;
   use type Touchdown_Monitor.Run_State;

   procedure Update_GUI;
   procedure Update_GUI is
      All_Legs : Landing_Legs.All_Legs_State;
      Thruster : Boolean;
      Height   : Altimeter.Height;
      Velocity : Altimeter.Velocity;
   begin
      Altimeter.Current_Height (H => Height);
      Altimeter.Current_Velocity (V => Velocity);
      Landing_Legs.Read_State (State => All_Legs);
      Thrusters.Get_State (Disabled => Thruster);
      GUI.Update (New_State => GUI.State'(Legs     => All_Legs,
                                          Thruster => not Thruster,
                                          Height   => Height,
                                          Velocity => Velocity));
   end Update_GUI;

begin
   Global.Log (Message => "Starting monitors...");
   Touchdown_Monitor.Start;

   Next_Cycle := Global.Start_Time + Cycle;

   declare
      Current_Height   : Altimeter.Height;
      Current_Velocity : Altimeter.Velocity;
   begin
      Altimeter.Current_Height (H => Current_Height);
      Altimeter.Current_Velocity (V => Current_Velocity);

      while Current_Height > 1.0 loop
         Altimeter.Current_Height (H => Current_Height);
         Altimeter.Current_Velocity (V => Current_Velocity);

         if not Legs_Deployed and then Current_Height <= 1500.0 then
            Landing_Legs.Deploy;
            Legs_Deployed := True;
         end if;

         if not Monitor_Enabled and then Current_Height <= 40.0 then
            Global.Log (Message => "Enabling monitors...");
            Touchdown_Monitor.Enable;
            Monitor_Enabled := True;
         end if;

         Update_GUI;

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;
   end;

   Update_GUI;

   Landing_Legs.Touchdown;

   Update_GUI;

   All_Monitors_Dead := False;

   while not All_Monitors_Dead loop
      All_Monitors_Dead :=
        (for all Leg in Landing_Legs.Legs_Index'Range =>
           Touchdown_Monitor.Current_State (Leg => Leg) =
             Touchdown_Monitor.Terminated);

      if All_Monitors_Dead then
         Global.Log (Message => "All monitors finished.");
         Touchdown_Monitor.Shutdown;
         Landing_Legs.Shutdown;
      end if;
   end loop;

   Global.Log (Message => "Simulation terminated.");

   loop
      Update_GUI;
   end loop;
end Simulator;
