with Ada.Real_Time;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Shared_Parameters.Read;
with Task_Safe_Store;

package body Thrusters is

   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;
   use type Shared_Types.Fuel_Mass;

   type State is (Disabled, Enabled);
   --  State of thruster.
   --  @value Disabled Thruster disabled, no upwards acceleration.
   --  @value Enabled  Thruster enabled, upwards acceleration accordingly.

   Fuel_Flow_Rate : constant Shared_Types.Flow_Rate
     := Shared_Parameters.Read.Fuel_Flow_Rate;
   --  Parametrized Fuel_Flow_Rate, read once at startup.

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Fuel_Store is new Task_Safe_Store
     (Stored_Type   => Shared_Types.Fuel_Mass,
      Initial_Value => Shared_Parameters.Read.Initial_Fuel_Mass);

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   Fuel_State : Fuel_Store.Shelf;
   Aborted    : Boolean := False
     with Atomic => True;

   type Valve_Info is
      record
         Is_Open    : Boolean;
         Open_Since : Ada.Real_Time.Time;
         Total_Open : Ada.Real_Time.Time_Span;
      end record
     with
       Dynamic_Predicate =>
         (Is_Open = False or else Open_Since > Global.Start_Time);

   protected Engine_State is
      function Get_Total return Duration;
      function Get return State;
      procedure Set (Value : in State);
      procedure No_More_Fuel;
   private
      Thruster_State  : State      := Disabled;
      Fuel_Tank_Empty : Boolean    := False;
      Valve_State     : Valve_Info :=
                          (Is_Open    => False,
                           Open_Since => Global.Start_Time,
                           Total_Open => Ada.Real_Time.Time_Span_Zero);
   end Engine_State;

   protected body Engine_State is

      function Get return State is
        (Thruster_State);

      function Get_Total return Duration is
      begin
         if Valve_State.Is_Open then
            declare
               Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            begin
               --  Thruster is currently on, recalculate fuel consumption.
               return
                 Ada.Real_Time.To_Duration
                   (TS =>
                      Valve_State.Total_Open + (Now - Valve_State.Open_Since));
            end;
         else
            --  Thruster is currently off, return recorded fuel consumption.
            return Ada.Real_Time.To_Duration (TS => Valve_State.Total_Open);
         end if;
      end Get_Total;

      procedure No_More_Fuel is
      begin
         Fuel_Tank_Empty := True;
         Set (Value => Disabled);
      end No_More_Fuel;

      procedure Set (Value : in State) is
      begin
         if not Fuel_Tank_Empty or else Value = Disabled then
            declare
               Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            begin
               Thruster_State := Value;

               case Value is
                  when Disabled =>
                     --  Ignore off commands if thruster is not active.
                     if Valve_State.Is_Open then
                        Valve_State.Is_Open    := False;
                        Valve_State.Total_Open :=
                          Valve_State.Total_Open +
                            (Now - Valve_State.Open_Since);
                     end if;

                  when Enabled  =>
                     --  Ignore On commands if thruster is already active.
                     if not Valve_State.Is_Open then
                        Valve_State.Is_Open    := True;
                        Valve_State.Open_Since := Now;
                     end if;
               end case;
            end;
         end if;
      end Set;

   end Engine_State;

   function Burn_Time return Duration is
     (Engine_State.Get_Total);

   function Current_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Fuel_State.Get);

   procedure Disable is
   begin
      Engine_State.Set (Value => Disabled);
   end Disable;

   procedure Enable is
   begin
      Engine_State.Set (Value => Enabled);
   end Enable;

   function Is_Disabled return Boolean is
     (Engine_State.Get = Disabled);

   function Is_Enabled return Boolean is
     (Engine_State.Get = Enabled);

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   procedure Shutdown (Source : in Shared_Types.Legs_Index) is
   begin
      Engine_State.No_More_Fuel;
      Log.Trace (Message =>
                   "Thrusters have been disabled due to signal from leg "
                 & Shared_Types.Legs_Index'Image (Source) & ".");
   end Shutdown;

   task Fuel_Monitor;

   task body Fuel_Monitor is
      Next_Cycle   : Ada.Real_Time.Time
        := Global.Start_Time + Configuration.Task_Offsets.Fuel_Monitor;
      Initial_Fuel : constant Shared_Types.Fuel_Mass := Fuel_State.Get;
      Current_Fuel : Shared_Types.Fuel_Mass := Initial_Fuel;
   begin
      Log.Trace (Message => "Fuel monitor started.");

      while not Aborted and then Current_Fuel > 0.0 loop
         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Configuration.Cycle_Times.Fuel_Monitor;

         declare
            Valve_Open_Time : constant Duration := Engine_State.Get_Total;
            Fuel_Used       : constant Shared_Types.Fuel_Mass
              := Shared_Types.Fuel_Mass'Min (Initial_Fuel,
                                             Fuel_Flow_Rate * Valve_Open_Time);
         begin
            Current_Fuel := Initial_Fuel - Fuel_Used;

            if Current_Fuel = 0.0 then
               Engine_State.No_More_Fuel;
               Log.Trace (Message => "Ran out of fuel, terminating...");
            end if;
         end;

         Fuel_State.Set (New_Value => Current_Fuel);
      end loop;

      Log.Trace (Message => "Fuel monitor finished.");
   exception
      when E : others =>
         Log.Trace (E => E);
   end Fuel_Monitor;

end Thrusters;
