with GNATCOLL.Traces;

with Global;
with Thrusters;
with Task_Safe_Store;

package body Engine is

   Logger : constant GNATCOLL.Traces.Trace_Handle :=
              GNATCOLL.Traces.Create (Unit_Name => "ENG",
                                      Default   => GNATCOLL.Traces.On,
                                      Stream    => Global.Standard_Error);

   use type Ada.Real_Time.Time;
   use type Shared_Types.Fuel_Mass;
   use type Shared_Types.State;

   package Fuel_Store is
     new Task_Safe_Store (Stored_Type   => Shared_Types.Fuel_Mass,
                          Initial_Value => 64.0);

   function Remaining_Fuel return Shared_Types.Fuel_Mass is
   begin
      return Fuel_Store.Get;
   end Remaining_Fuel;

   task Engine_Task;

   task body Engine_Task is
      Next_Cycle   : Ada.Real_Time.Time := Global.Start_Time;
      Current_Fuel : Shared_Types.Fuel_Mass;
   begin
      loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;

         Current_Fuel := Fuel_Store.Get;

         if Thrusters.Current_State = Shared_Types.Enabled then
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
   exception
      when E : others =>
         Logger.all.Trace (E => E);
   end Engine_Task;

   procedure Shutdown is
   begin
      abort Engine_Task;
   end Shutdown;

end Engine;
