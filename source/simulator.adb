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
   use type Altimeter.Altitude;
   use type Altimeter.Velocity;
   use type Touchdown_Monitor.Run_State;

   procedure Update_GUI
     (Terminated : in Boolean := False;
      Time_Stamp : in Ada.Real_Time.Time := Global.Start_Time);

   procedure Update_GUI
     (Terminated : in Boolean := False;
      Time_Stamp : in Ada.Real_Time.Time := Global.Start_Time)
   is
      All_Legs : Landing_Legs.All_Legs_State;
      Thruster : constant Thrusters.State    := Thrusters.Current_State;
      Altitude : constant Altimeter.Altitude := Altimeter.Current_Altitude;
      Velocity : constant Altimeter.Velocity := Altimeter.Current_Velocity;
   begin
      Landing_Legs.Read_State (State => All_Legs);

      GUI.Update (New_State => GUI.State'(Legs       => All_Legs,
                                          Thruster   => Thruster,
                                          Altitude   => Altitude,
                                          Velocity   => Velocity,
                                          Terminated => Terminated,
                                          Time_Stamp => Time_Stamp));
   end Update_GUI;

begin
   Global.Log (Message => "Starting monitors...");
   Touchdown_Monitor.Start;

   Next_Cycle := Global.Start_Time + Cycle;

   declare
      Current_Altitude : Altimeter.Altitude := Altimeter.Current_Altitude;
   begin
      while Current_Altitude > 0.0 loop
         Current_Altitude := Altimeter.Current_Altitude;

         if not Legs_Deployed and then Current_Altitude <= 1500.0 then
            Landing_Legs.Deploy;
            Legs_Deployed := True;
         end if;

         if not Monitor_Enabled and then Current_Altitude <= 40.0 then
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

   All_Monitors_Dead := False;

   while not All_Monitors_Dead loop
      All_Monitors_Dead :=
        (for all Leg in Landing_Legs.Legs_Index'Range =>
           Touchdown_Monitor.Current_State (Leg => Leg) =
             Touchdown_Monitor.Terminated);

      if All_Monitors_Dead then
         Global.Log (Message => "All monitors finished.");
         Update_GUI;
         Touchdown_Monitor.Shutdown;
         Landing_Legs.Shutdown;
      end if;
   end loop;

   Global.Log (Message => "Simulation terminated.");

   declare
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      loop
         Update_GUI (Terminated => True,
                     Time_Stamp => Now);
      end loop;
   end;
end Simulator;
