with Ada.Real_Time;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Landing_Legs;
with Planets.Parameters;
with Shared_Parameters.Read;
with Rocket_Science;
with Task_Safe_Store;
with Thrusters;

package body Altimeter is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Acceleration;
   use type Shared_Types.Altitude;
   use type Shared_Types.Mass;
   use type Shared_Types.Velocity;

   Gravity             : constant Shared_Types.Acceleration
     := Shared_Types.Acceleration (Planets.Parameters.Gravity (Planets.Mars));

   Initial_Velocity    : constant Shared_Types.Velocity
     := Shared_Parameters.Read.Initial_Velocity;

   Initial_Lander_Mass : constant Shared_Types.Vehicle_Mass
     := Shared_Parameters.Read.Dry_Mass;

   Heatshield_Mass     : constant Shared_Types.Mass := 140.0;
   Cruise_Stage_Mass   : constant Shared_Types.Mass :=  82.0;

   --  Relevant spacecraft masses during EDL.
   Dry_Mass_Before_EDL                    : constant Shared_Types.Mass
     := Cruise_Stage_Mass + Heatshield_Mass + Initial_Lander_Mass;
   Dry_Mass_After_Cruise_Stage_Separation : constant Shared_Types.Mass
     := Heatshield_Mass + Initial_Lander_Mass;
   Dry_Mass_After_Heatshield_Separation   : constant Shared_Types.Mass
     := Shared_Types.Mass (Initial_Lander_Mass);

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Altimeter_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Altitude,
                      Initial_Value => Shared_Parameters.Read.Initial_Altitude);
   Altimeter_State : Altimeter_Store.Shelf;

   package Drag_Coefficient_Store is
      new Task_Safe_Store (Stored_Type   => Float,
                           Initial_Value => 0.0);
   Drag_Coefficient : Drag_Coefficient_Store.Shelf;

   package Drag_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Acceleration,
                      Initial_Value => 0.0);
   Drag_State : Drag_Store.Shelf;

   package Spacecraft_Mass_Store is
     new Task_Safe_Store (Stored_Type   => Shared_Types.Mass,
                          Initial_Value => Dry_Mass_Before_EDL);
   Spacecraft_Dry_Mass : Spacecraft_Mass_Store.Shelf;

   package Velocity_Store  is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Velocity,
                      Initial_Value => Shared_Parameters.Read.Initial_Velocity);
   Velocity_State : Velocity_Store.Shelf;

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   function Current_Altitude return Shared_Types.Altitude is
     (Altimeter_State.Get);

   function Current_Drag return Shared_Types.Acceleration is
      (Drag_State.Get);

   function Current_Dry_Mass return Shared_Types.Vehicle_Mass is
     (Shared_Types.Vehicle_Mass (Spacecraft_Dry_Mass.Get));

   function Current_Velocity return Shared_Types.Velocity is
     (Velocity_State.Get);

   procedure Deploy_Parachute is
   begin
      Drag_Coefficient.Set (New_Value => 0.410);
   end Deploy_Parachute;

   procedure Enter_Atmosphere is
   begin
      Drag_Coefficient.Set (New_Value => 0.026);
   end Enter_Atmosphere;

   procedure Jettison_Heatshield is
   begin
      Spacecraft_Dry_Mass.Set
        (New_Value => Dry_Mass_After_Heatshield_Separation);
   end Jettison_Heatshield;

   procedure Separate_Cruise_Stage is
   begin
      Spacecraft_Dry_Mass.Set
        (New_Value => Dry_Mass_After_Cruise_Stage_Separation);
   end Separate_Cruise_Stage;

   procedure Separate_Lander is
   begin
      Drag_Coefficient.Set (New_Value => 0.100);
   end Separate_Lander;

   Aborted : Boolean := False
     with Atomic;
   pragma Warnings (Off,
                    Aborted,
                    Reason => "Do not warn about atomic synchronization");

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   task Radar_Simulator;

   task body Radar_Simulator is
      T             : constant Duration
        := Ada.Real_Time.To_Duration (Configuration.Cycle_Times.Altitude_Task);
      --  Duration of a single task cycle.

      Next_Cycle    : Ada.Real_Time.Time
        := Global.Start_Time + Configuration.Task_Offsets.Altitude_Task;
      Altitude_Now  : Shared_Types.Altitude := Altimeter_State.Get;
      Velocity_Now  : Shared_Types.Velocity := Velocity_State.Get;
      Drag_Delta_V  : Shared_Types.Velocity := 0.0; --  accumulated delta v due to drag effects.
   begin
      Log.Trace (Message => "Altitude control monitor started.");

      while not Aborted and then Altitude_Now > 0.0 loop
         delay until Next_Cycle;

         --  Calculate change in altitude according to current velocity.
         Calculate_Delta_A :
         declare
            Delta_A : constant Shared_Types.Altitude := Velocity_Now * T;
         begin
            Altitude_Now :=
              Altitude_Now - Shared_Types.Altitude'Min (Altitude_Now, Delta_A);
            Altimeter_State.Set (New_Value => Altitude_Now);
         end Calculate_Delta_A;

         Calculate_Drag :
         declare
            Drag_Constant : constant Float                     :=
                              Drag_Coefficient.Get;
            Fuel_Mass     : constant Shared_Types.Fuel_Mass    :=
                              Thrusters.Current_Fuel_Mass;
            Current_Mass  : constant Shared_Types.Mass         :=
                              Spacecraft_Dry_Mass.Get + Fuel_Mass;
            Drag_Now      : constant Shared_Types.Acceleration :=
                              Rocket_Science.Drag
                                (Current_Wet_Mass => Current_Mass,
                                 Velocity         => Velocity_Now,
                                 Drag_Constant    => Drag_Constant);
         begin
            Drag_State.Set (New_Value => Drag_Now);
            Drag_Delta_V := Drag_Delta_V + (Drag_Now * T);
         end Calculate_Drag;

         Calculate_Delta_V :
         declare
            Descent_Time : constant Duration :=
                             Ada.Real_Time.To_Duration
                               (Next_Cycle - Global.Start_Time);
            Delta_V      : constant Shared_Types.Velocity :=
                             Gravity * Descent_Time;
         begin
            Velocity_Now :=
              Initial_Velocity + Delta_V - Thrusters.Delta_V - Drag_Delta_V;
         end Calculate_Delta_V;

         Velocity_State.Set (New_Value => Velocity_Now);

         if Altitude_Now = 0.0 then
            Landing_Legs.Touchdown;
            Log.Trace (Message => "Touchdown triggered due to ground zero.");
         end if;

         Next_Cycle := Next_Cycle + Configuration.Cycle_Times.Altitude_Task;
      end loop;

      Log.Trace (Message => "Altitude control monitor finished.");
   exception
      when E : others =>
         Log.Trace (E => E);
   end Radar_Simulator;

end Altimeter;
