with Ada.Strings.Fixed;
with Ada.Text_IO;

with Global;
with Thrusters;
with Task_Safe_Store;

package body Engine is

   use type Ada.Real_Time.Time;
   use type Thrusters.State;

   package Fuel_Store is new Task_Safe_Store (Stored_Type   => Fuel_Mass,
                                              Initial_Value => 64.0);

   package Fuel_IO is new Ada.Text_IO.Fixed_IO (Num => Fuel_Mass);

   function Image (Value : in Fuel_Mass) return String is
      Result : String := "XXX.XXX";
   begin
      Fuel_IO.Put (To   => Result,
                   Item => Value,
                   Aft  => 3,
                   Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left) & " kg";
   end Image;

   function Remaining_Fuel return Fuel_Mass is
   begin
      return Fuel_Store.Get;
   end Remaining_Fuel;

   task Engine_Task;

   task body Engine_Task is
      Next_Cycle   : Ada.Real_Time.Time := Global.Start_Time;
      Current_Fuel : Fuel_Mass;
   begin
      loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;

         Current_Fuel := Fuel_Store.Get;

         if Thrusters.Current_State = Thrusters.Enabled then
            if Current_Fuel > Flow_Rate then
               Current_Fuel := Current_Fuel - Flow_Rate;
            else
               Current_Fuel := 0.0;
            end if;

            if Current_Fuel = 0.0 then
               Thrusters.Out_Of_Fuel;
            end if;
         end if;

         Fuel_Store.Set (New_Value => Current_Fuel);
      end loop;
   end Engine_Task;

   procedure Shutdown is
   begin
      abort Engine_Task;
   end Shutdown;

end Engine;
