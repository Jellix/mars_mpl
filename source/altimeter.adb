with GNATCOLL.Traces;

with Global;
with Landing_Legs;
with Parametrization;
with Task_Safe_Store;
with Thrusters;

package body Altimeter is

   Logger : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create (Unit_Name => "ATM",
                                      Default   => GNATCOLL.Traces.On,
                                      Stream    => Global.Standard_Error);

   use type Ada.Real_Time.Time;
   use type Shared_Types.Altitude;
   use type Shared_Types.State;
   use type Shared_Types.Velocity;

   package Altimeter_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Altitude,
                      Initial_Value => Parametrization.Initial_Altitude);

   package Velocity_Store  is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Velocity,
                      Initial_Value => Parametrization.Initial_Velocity);

   task Radar_Simulator;

   task body Radar_Simulator is
      Next_Cycle   : Ada.Real_Time.Time    := Global.Start_Time;
      Altitude_Now : Shared_Types.Altitude := Altimeter_Store.Get;
      Velocity_Now : Shared_Types.Velocity := Velocity_Store.Get;
   begin
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image & "] Altitude control monitor started.");

      while Altitude_Now > 0.0 loop
         declare
            T            : constant Float :=
                             Float (Ada.Real_Time.To_Duration (Cycle));
            Acceleration : constant Shared_Types.Velocity
              := (if Thrusters.Current_State = Shared_Types.Disabled
                  then Shared_Types.Velocity (Parametrization.Gravity * T)
                  else Shared_Types.Velocity
                         (Parametrization.Thruster_Acceleration * T));
            Distance     : constant Shared_Types.Altitude :=
                             Shared_Types.Altitude (Float (Velocity_Now) * T);
         begin
            Altimeter_Store.Set
              (New_Value =>
                 Altitude_Now
               - Shared_Types.Altitude'Min (Altitude_Now, Distance));
            Velocity_Store.Set
              (New_Value =>
                 Shared_Types.Velocity'Max (0.0, Velocity_Now + Acceleration));

            Altitude_Now := Altimeter_Store.Get;
            Velocity_Now := Velocity_Store.Get;
         end;

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;

      Landing_Legs.Touchdown;
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image & "] Altitude control monitor finished.");
   exception
      when E : others =>
         Logger.all.Trace (E => E);
   end Radar_Simulator;

   function Current_Altitude return Shared_Types.Altitude
     renames Altimeter_Store.Get;

   function Current_Velocity return Shared_Types.Velocity
     renames Velocity_Store.Get;

end Altimeter;
