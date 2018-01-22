--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Altimeter;
with Engine;
with Global;
with GUI;
with Landing_Legs;
with Thrusters;
with Touchdown_Monitor;

procedure Simulator is
   All_Monitors_Dead : Boolean := False;
   Monitor_Enabled   : Boolean := False;
   Cycle             : constant Ada.Real_Time.Time_Span :=
                         Ada.Real_Time.Milliseconds (MS => 10);
   Next_Cycle        : Ada.Real_Time.Time;

   procedure Update_GUI (Terminated : in Boolean := False);

   procedure Update_GUI (Terminated : in Boolean := False)
   is
      All_Legs : Landing_Legs.All_Legs_State;
      Thruster : constant Thrusters.State    := Thrusters.Current_State;
      Altitude : constant Altimeter.Altitude := Altimeter.Current_Altitude;
      Velocity : constant Altimeter.Velocity := Altimeter.Current_Velocity;
      Fuel     : constant Engine.Fuel_Mass   := Engine.Remaining_Fuel;
   begin
      Landing_Legs.Read_State (State => All_Legs);

      GUI.Update (New_State => GUI.State'(Legs       => All_Legs,
                                          Thruster   => Thruster,
                                          Altitude   => Altitude,
                                          Velocity   => Velocity,
                                          Fuel       => Fuel,
                                          Terminated => Terminated));
   end Update_GUI;

   use type Ada.Real_Time.Time;
   use type Altimeter.Altitude;
   use type Altimeter.Velocity;
   use type Thrusters.State;
   use type Touchdown_Monitor.Run_State;
begin
   Global.Log (Message => "Starting touchdown monitors...");
   Touchdown_Monitor.Start;

   Next_Cycle := Global.Start_Time + Cycle;

   declare
      Current_Altitude : Altimeter.Altitude := Altimeter.Current_Altitude;
      Current_Velocity : Altimeter.Velocity := Altimeter.Current_Velocity;
      Legs_Deployed    : Boolean := False;
      Powered_Descent  : Boolean := False;
   begin
      while Current_Altitude > 0.0 loop
         Current_Altitude := Altimeter.Current_Altitude;
         Current_Velocity := Altimeter.Current_Velocity;

         if not Legs_Deployed and then Current_Altitude <= 1500.0 then
            Landing_Legs.Deploy;
            Legs_Deployed := True;
         end if;

         --  Entering powered descent.
         if not Powered_Descent and then Current_Altitude <= 1000.0 then
            Thrusters.Enable;
            Powered_Descent := True;
            Global.Log (Message => "Entered powered descent flight mode.");
         end if;

         if Powered_Descent then
            if Current_Velocity < Altimeter.Safe_Landing_Velocity * 0.6 then
               Thrusters.Disable;
            elsif Current_Velocity > Altimeter.Safe_Landing_Velocity * 0.8 then
               Thrusters.Enable;
            end if;
         end if;

         if not Monitor_Enabled and then Current_Altitude <= 40.0 then
            Global.Log (Message => "Enabling touchdown monitors...");
            Touchdown_Monitor.Enable;
            Monitor_Enabled := True;
         end if;

         Update_GUI;

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;
   end;

   declare
      Touchdown_Velocity : constant Altimeter.Velocity :=
                             Altimeter.Current_Velocity;
   begin
      if Touchdown_Velocity > Altimeter.Safe_Landing_Velocity then
         Global.Log (Message => "MISSION FAILURE: MPL crashed on surface!");
      else
         Global.Log (Message => "MISSION SUCCESS: MPL touched down safely.");
      end if;
   end;

   Update_GUI;

   All_Monitors_Dead := False;

   while not All_Monitors_Dead loop
      All_Monitors_Dead :=
        (for all Leg in Landing_Legs.Legs_Index'Range =>
           Touchdown_Monitor.Current_State (Leg => Leg) =
             Touchdown_Monitor.Terminated);

      if All_Monitors_Dead then
         Global.Log (Message => "All touchdown monitors finished.");
         Update_GUI;
         Touchdown_Monitor.Shutdown;
         Landing_Legs.Shutdown;
         Engine.Shutdown;
      end if;
   end loop;

   Global.Log (Message => "Simulation finished.");

   loop
      Update_GUI (Terminated => True);

      exit when GUI.Aborted;
   end loop;
end Simulator;
