with Ada.Real_Time;
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
   use type Shared_Types.Velocity;

   Thruster_Accel : constant Shared_Types.Acceleration
     := Shared_Parameters.Read.Thruster_Acceleration;

   Gravity        : constant Shared_Types.Acceleration
     := Shared_Types.Acceleration (Planets.Parameters.Gravity (Planets.Mars));

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

   Aborted : Boolean := False
     with Atomic;

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   task Radar_Simulator;

   task body Radar_Simulator is
      Next_Cycle   : Ada.Real_Time.Time
        := Global.Start_Time + Configuration.Task_Offsets.Altitude_Task;

      Altitude_Now : Shared_Types.Altitude := Altimeter_State.Get;
      Velocity_Now : Shared_Types.Velocity := Velocity_State.Get;

      --  The following parameters remain constant during the task's lifetime.
      T            : constant Duration
        := Ada.Real_Time.To_Duration (Configuration.Cycle_Times.Altitude_Task);
      Free_Fall    : constant Shared_Types.Velocity := Gravity * T;
      Thrusted     : constant Shared_Types.Velocity := Thruster_Accel * T;
   begin
      Log.Trace (Message => "Altitude control monitor started.");

      while not Aborted and then Altitude_Now > 0.0 loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Configuration.Cycle_Times.Altitude_Task;

         declare
            Delta_V : constant Shared_Types.Velocity :=
                        (if Thrusters.Is_Disabled
                         then Free_Fall
                         else Thrusted);
            Delta_A : constant Shared_Types.Altitude := Velocity_Now * T;
         begin
            Altitude_Now :=
              Altitude_Now - Shared_Types.Altitude'Min (Altitude_Now, Delta_A);
            Altimeter_State.Set (New_Value => Altitude_Now);

            Velocity_Now :=
              Shared_Types.Velocity'Max (0.0, Velocity_Now + Delta_V);
            Velocity_State.Set (New_Value => Velocity_Now);
         end;

         if Altitude_Now = 0.0 then
            Landing_Legs.Touchdown;
            Log.Trace (Message => "Touchdown triggered due to ground zero.");
         end if;
      end loop;

      Log.Trace (Message => "Altitude control monitor finished.");
   exception
      when E : others =>
         Log.Trace (E => E);
   end Radar_Simulator;

end Altimeter;
