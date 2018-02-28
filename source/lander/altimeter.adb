with Ada.Numerics.Elementary_Functions;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Landing_Legs;
with Planets.Parameters;
with Shared_Parameters.Read;
with Task_Safe_Store;
with Thrusters;

package body Altimeter is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Altitude;
   use type Shared_Types.Mass;
   use type Shared_Types.Velocity;

   Gravity : constant Shared_Types.Acceleration
     := Shared_Types.Acceleration (Planets.Parameters.Gravity (Planets.Mars));

   Initial_Fuel_Mass : constant Shared_Types.Fuel_Mass
     := Shared_Parameters.Read.Initial_Fuel_Mass;

   Initial_Velocity : constant Shared_Types.Velocity
     := Shared_Parameters.Read.Initial_Velocity;

   Exhaust_Velocity : constant Shared_Types.Velocity
     := Shared_Parameters.Read.Exhaust_Velocity;

   Dry_Mass : constant Shared_Types.Mass := Shared_Parameters.Read.Dry_Mass;

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Altimeter_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Altitude,
                      Initial_Value => Shared_Parameters.Read.Initial_Altitude);
   Altimeter_State : Altimeter_Store.Shelf;

   package Velocity_Store  is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Velocity,
                      Initial_Value => Shared_Parameters.Read.Initial_Velocity);
   Velocity_State : Velocity_Store.Shelf;

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   function Current_Altitude return Shared_Types.Altitude is
     (Altimeter_State.Get);

   function Current_Velocity return Shared_Types.Velocity is
     (Velocity_State.Get);

   function Ln (X : in Float) return Float renames
     Ada.Numerics.Elementary_Functions.Log;

   type Descent_Phase is (Start,
                          -- ... some more, not supported/relevant
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

   procedure Lander_Separation (Separated_At : in Ada.Real_Time.Time) is
   begin
      Descent_State.Set (New_Phase => Lander_Separation,
                         Change_At => Separated_At);
   end Lander_Separation;

   Aborted : Boolean := False
     with Atomic;

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   task Radar_Simulator;

   task body Radar_Simulator is
      Next_Cycle    : Ada.Real_Time.Time
        := Global.Start_Time + Configuration.Task_Offsets.Altitude_Task;

      Altitude_Now  : Shared_Types.Altitude := Altimeter_State.Get;
      Velocity_Now  : Shared_Types.Velocity := Velocity_State.Get;

      --  The following parameters remain constant during the task's lifetime.

      T             : constant Duration
        := Ada.Real_Time.To_Duration (Configuration.Cycle_Times.Altitude_Task);
      --  Duration of a single task cycle.
      M0            : constant Float
        := Float (Dry_Mass + Shared_Types.Mass (Initial_Fuel_Mass));
      --  Initial space craft wet mass.
      Current_Phase : Descent_Phase := Descent_State.Get;
      --  Current descent phase.
   begin
      Log.Trace (Message => "Altitude control monitor started.");

      while not Aborted and then Altitude_Now > 0.0 loop
         delay until Next_Cycle;

         Current_Phase := Descent_State.Get;

         --  Calculate change in altitude according to current velocity.
         Calculate_Delta_A :
         declare
            Delta_A : constant Shared_Types.Altitude := Velocity_Now * T;
         begin
            Altitude_Now :=
              Altitude_Now - Shared_Types.Altitude'Min (Altitude_Now, Delta_A);
            Altimeter_State.Set (New_Value => Altitude_Now);
         end Calculate_Delta_A;

         if Current_Phase = Lander_Separation then
            Calculate_Delta_V :
            declare
               Descent_Time : constant Duration :=
                                Ada.Real_Time.To_Duration
                                  (Next_Cycle - Descent_State.Separation_Time);
               M1           : constant Float
                 := Float (Dry_Mass
                           + Shared_Types.Mass (Thrusters.Current_Fuel_Mass));
               Delta_V      : constant Shared_Types.Velocity
                 := (Gravity * Descent_Time) - Exhaust_Velocity * Ln (X => M0 / M1);
            begin
               Velocity_Now :=
                 Shared_Types.Velocity'Max (0.0, Initial_Velocity + Delta_V);
               Velocity_State.Set (New_Value => Velocity_Now);
            end Calculate_Delta_V;
         end if;

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
