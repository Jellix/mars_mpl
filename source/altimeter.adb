with Ada.Exceptions;

with Global;
with Landing_Legs;
with Task_Safe_Store;
with Thrusters;

package body Altimeter is

   Module : constant String := "ALTIMETER";

   use type Ada.Real_Time.Time;
   use type Thrusters.State;

   package Altimeter_Store is new
     Task_Safe_Store (Stored_Type   => Altitude,
                      Initial_Value => Initial_Altitude);

   package Velocity_Store  is new
     Task_Safe_Store (Stored_Type   => Velocity,
                      Initial_Value => Initial_Velocity);

   task Radar_Simulator;

   task body Radar_Simulator is
      Next_Cycle   : Ada.Real_Time.Time := Global.Start_Time;
      Altitude_Now : Altitude           := Altimeter_Store.Get;
      Velocity_Now : Velocity           := Velocity_Store.Get;
   begin
      Global.Log (Module  => Module,
                  Message => "Altitude control monitor started.");

      while Altitude_Now > 0.0 loop
         declare
            T            : constant Float :=
                             Float (Ada.Real_Time.To_Duration (Cycle));
            Acceleration : constant Velocity :=
                             (if Thrusters.Current_State = Thrusters.Disabled
                              then Velocity (Gravity * T)
                              else Velocity (Thrusters.Acceleration * T));
            Distance     : constant Altitude :=
                             Altitude (Float (Velocity_Now) * T);
         begin
            Altimeter_Store.Set
              (New_Value =>
                 Altitude_Now - Altitude'Min (Altitude_Now, Distance));
            Velocity_Store.Set
              (New_Value => Velocity'Max (0.0, Velocity_Now + Acceleration));

            Altitude_Now := Altimeter_Store.Get;
            Velocity_Now := Velocity_Store.Get;
         end;

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;

      Landing_Legs.Touchdown;
      Global.Log (Module  => Module,
                  Message => "Altitude control monitor finished.");
   exception
      when E : others =>
         Global.Log (Module  => Module,
                     Message => Ada.Exceptions.Exception_Information (E));
   end Radar_Simulator;

   function Current_Altitude return Altitude renames Altimeter_Store.Get;

   function Current_Velocity return Velocity renames Velocity_Store.Get;

   function Image (A : Altitude) return String is
     (Altitude'Image (A) & " m");

   function Image (V : Velocity) return String is
     (Velocity'Image (V) & " m/s (" & Velocity'Image (V * 3.6) & " km/h)");

end Altimeter;
