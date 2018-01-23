with Ada.Strings.Fixed;
with Ada.Text_IO;
with GNATCOLL.Traces;

with Global;
with Landing_Legs;
with Task_Safe_Store;
with Thrusters;

package body Altimeter is

   Logger : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create (Unit_Name => "ATM",
                                      Default   => GNATCOLL.Traces.On,
                                      Stream    => Global.Standard_Error);

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
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image & "] Altitude control monitor started.");

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
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image & "] Altitude control monitor finished.");
   exception
      when E : others =>
         Logger.all.Trace (E => E);
   end Radar_Simulator;

   function Current_Altitude return Altitude renames Altimeter_Store.Get;

   function Current_Velocity return Velocity renames Velocity_Store.Get;

   package Altitude_IO is new Ada.Text_IO.Fixed_IO (Num => Altitude);
   package Velocity_IO is new Ada.Text_IO.Fixed_IO (Num => Velocity);

   function Image (A : Altitude) return String is
      Result : String := "XXXXXXX.XXX";
   begin
      Altitude_IO.Put (To   => Result,
                       Item => A,
                       Aft  => 3,
                       Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left) & " m";
   end Image;

   function Image (V : Velocity) return String is
      Result     : String := "-XXXX.XXX";
      Result_KMH : String := "-XXXX.XXX";
   begin
      Velocity_IO.Put (To   => Result,
                       Item => V,
                       Aft  => 3,
                       Exp  => 0);
      Velocity_IO.Put (To   => Result_KMH,
                       Item => V * 3.6,
                       Aft  => 3,
                       Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left)
        & " m/s ("
        & Ada.Strings.Fixed.Trim (Source => Result_KMH,
                                  Side   => Ada.Strings.Left)
        & " km/h)";
   end Image;

end Altimeter;
