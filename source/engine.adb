with Global.Task_Offsets;
with Shared_Parameters.Read;
with Task_Safe_Store;
with Thrusters;

package body Engine is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Fuel_Mass;
   use type Shared_Types.State;

   Fuel_Flow_Rate : constant Shared_Types.Fuel_Mass
     := Shared_Parameters.Read.Fuel_Flow_Rate;
   --  Read parametrization only once at startup.

   package Fuel_Store is new Task_Safe_Store
     (Stored_Type   => Shared_Types.Fuel_Mass,
      Initial_Value => Shared_Parameters.Read.Initial_Fuel_Mass);

   Fuel_State : Fuel_Store.Shelf;

   task Engine_Task;

   task body Engine_Task is
      Next_Cycle   : Ada.Real_Time.Time
        := Global.Start_Time + Global.Task_Offsets.Engine_Task;

      Current_Fuel : Shared_Types.Fuel_Mass := Fuel_State.Get;
      Fuel_Used    : constant Shared_Types.Fuel_Mass
        := Fuel_Flow_Rate / Duration'(1.0 / Ada.Real_Time.To_Duration (Cycle));
   begin
      loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;

         if Thrusters.Current_State = Shared_Types.Enabled then
            if Current_Fuel > Fuel_Used then
               Current_Fuel := Current_Fuel - Fuel_Used;
            else
               Current_Fuel := 0.0;
            end if;

            if Current_Fuel = 0.0 then
               Thrusters.Out_Of_Fuel;
            end if;
         end if;

         Fuel_State.Set (New_Value => Current_Fuel);
      end loop;
   exception
      when E : others =>
         Log.Trace (E => E);
   end Engine_Task;

   function Current_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Fuel_State.Get);

   procedure Shutdown is
   begin
      abort Engine_Task;
   end Shutdown;

end Engine;
