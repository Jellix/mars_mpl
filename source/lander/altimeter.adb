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

   Gravity : constant Shared_Types.Acceleration
     := Shared_Types.Acceleration (Planets.Parameters.Gravity (Planets.Mars));

   Initial_Velocity : constant Shared_Types.Velocity
     := Shared_Parameters.Read.Initial_Velocity;

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Altimeter_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Altitude,
                      Initial_Value => Shared_Parameters.Read.Initial_Altitude);
   Altimeter_State : Altimeter_Store.Shelf;

   package Drag_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Acceleration,
                      Initial_Value => 0.0);
   Drag_State : Drag_Store.Shelf;

   package Velocity_Store  is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Velocity,
                      Initial_Value => Shared_Parameters.Read.Initial_Velocity);
   Velocity_State : Velocity_Store.Shelf;

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   function Current_Altitude return Shared_Types.Altitude is
     (Altimeter_State.Get);

   function Current_Drag return Shared_Types.Acceleration is
      (Drag_State.Get);

   function Current_Velocity return Shared_Types.Velocity is
     (Velocity_State.Get);

   type Descent_Phase is (Start,
                          -- ... some more, not supported/relevant
                          Parachute_Deployed,
                          Heatshield_Jettisoned,
                          Lander_Separation);

   protected Descent_State is
      procedure Set (New_Phase : in Descent_Phase;
                     Change_At : in Ada.Real_Time.Time);
      function Get return Descent_Phase;
      function Separation_Time return Ada.Real_Time.Time;
   private
      Phase        : Descent_Phase      := Start;
      Separated_At : Ada.Real_Time.Time := Global.Start_Time;
   end Descent_State;

   protected body Descent_State is

      function Get return Descent_Phase is
        (Phase);

      function Separation_Time return Ada.Real_Time.Time is
        (Separated_At);

      procedure Set (New_Phase : in Descent_Phase;
                     Change_At : in Ada.Real_Time.Time) is
      begin
         if Phase /= New_Phase then
            if New_Phase = Lander_Separation then
               Separated_At := Change_At;
            end if;

            Phase := New_Phase;
         end if;
      end Set;

   end Descent_State;

   procedure Heatshield (Jettisoned_At : in Ada.Real_Time.Time) is
   begin
      Descent_State.Set (New_Phase => Heatshield_Jettisoned,
                         Change_At => Jettisoned_At);
   end Heatshield;

   procedure Lander (Separated_At : in Ada.Real_Time.Time) is
   begin
      Descent_State.Set (New_Phase => Lander_Separation,
                         Change_At => Separated_At);
   end Lander;

   procedure Parachute (Deployed_At : in Ada.Real_Time.Time) is
   begin
      Descent_State.Set (New_Phase => Parachute_Deployed,
                         Change_At => Deployed_At);
   end Parachute;

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

   type Drag_Lookup is array (Descent_Phase) of Float;
   type Mass_Lookup is array (Descent_Phase) of Shared_Types.Mass;

   Drag_Table : constant Drag_Lookup := (Start                 => 0.015,
                                         Parachute_Deployed    => 0.410,
                                         Heatshield_Jettisoned => 0.410,
                                         Lander_Separation     => 0.100);

   Mass_Table : constant Mass_Lookup := (Start .. Parachute_Deployed => 140.0,
                                         Heatshield_Jettisoned       =>   0.0,
                                         Lander_Separation           =>   0.0);

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
            Current_Phase : constant Descent_Phase     := Descent_State.Get;
            Drag_Constant : constant Float             :=
                              Drag_Table (Current_Phase);
            Dry_Mass      : constant Shared_Types.Mass :=
                              Shared_Types.Mass (Shared_Parameters.Read.Dry_Mass) + Mass_Table (Current_Phase);
            Fuel_Mass     : constant Shared_Types.Mass :=
                              Shared_Types.Mass (Thrusters.Current_Fuel_Mass);
            Drag_Now      : constant Shared_Types.Acceleration :=
                              Rocket_Science.Drag
                                (Current_Wet_Mass => Dry_Mass + Fuel_Mass,
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
