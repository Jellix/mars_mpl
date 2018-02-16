with Landing_Legs;
with Parametrization;
with Shared_Parameters.Read;
with Task_Safe_Store;
with Thrusters;

package body Altimeter is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Altitude;
   use type Shared_Types.State;
   use type Shared_Types.Velocity;

   Thruster_Acceleration : constant Shared_Types.Acceleration
     := Shared_Parameters.Read.Thruster_Acceleration;

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Altimeter_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Altitude,
                      Initial_Value => Shared_Parameters.Read.Initial_Altitude);

   package Velocity_Store  is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Velocity,
                      Initial_Value => Shared_Parameters.Read.Initial_Velocity);

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   task Radar_Simulator;

   task body Radar_Simulator is
      Next_Cycle   : Ada.Real_Time.Time    := Global.Start_Time;
      Altitude_Now : Shared_Types.Altitude := Altimeter_Store.Get;
      Velocity_Now : Shared_Types.Velocity := Velocity_Store.Get;
   begin
      Log.Trace (Message => "Altitude control monitor started.");

      while Altitude_Now > 0.0 loop
         declare
            T       : constant Duration := Ada.Real_Time.To_Duration (Cycle);
            Delta_V : constant Shared_Types.Velocity :=
                        (if Thrusters.Current_State = Shared_Types.Disabled
                         then Shared_Types.Velocity (Parametrization.Gravity * Float (T))
                         else Thruster_Acceleration * T);
            Delta_A : constant Shared_Types.Altitude := Velocity_Now * T;
         begin
            Altitude_Now :=
              Altitude_Now - Shared_Types.Altitude'Min (Altitude_Now, Delta_A);
            Altimeter_Store.Set (New_Value => Altitude_Now);

            Velocity_Now :=
              Shared_Types.Velocity'Max (0.0, Velocity_Now + Delta_V);
            Velocity_Store.Set (New_Value => Velocity_Now);
         end;

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;

      Landing_Legs.Touchdown;
      Log.Trace (Message => "Altitude control monitor finished.");
   exception
      when E : others =>
         Log.Trace (E => E);
   end Radar_Simulator;

   function Current_Altitude return Shared_Types.Altitude
     renames Altimeter_Store.Get;

   function Current_Velocity return Shared_Types.Velocity
     renames Velocity_Store.Get;

end Altimeter;
