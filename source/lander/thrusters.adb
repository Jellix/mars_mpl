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

   Fuel_Flow_Rate : constant Shared_Types.Fuel_Mass
     := Shared_Parameters.Read.Fuel_Flow_Rate;
   --  Read parametrization only once at startup.

   package Fuel_Store is new Task_Safe_Store
     (Stored_Type   => Shared_Types.Fuel_Mass,
      Initial_Value => Shared_Parameters.Read.Initial_Fuel_Mass);

   Fuel_State : Fuel_Store.Shelf;
   Aborted    : Boolean := False
     with Atomic => True;

   protected Fuel_Flow_Tracker is

      procedure Get_Activation_Time (Activation_Time : out Duration);

      procedure On (Activation_Time : in Ada.Real_Time.Time);

      procedure Off (Deactivation_Time : in Ada.Real_Time.Time);

   private

      Last_On     : Ada.Real_Time.Time      := Global.Start_Time;
      Active_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;

   end Fuel_Flow_Tracker;

   protected Thruster_State is
      function Get return State;
      procedure Set (Value : in State);
      procedure No_More_Fuel;
   private
      Current_Thruster_State : State   := Disabled;
      Fuel_Tank_Empty        : Boolean := False;
   end Thruster_State;

   protected body Fuel_Flow_Tracker is

      procedure Get_Activation_Time (Activation_Time : out Duration) is
      begin
         if Last_On /= Global.Start_Time then
            --  Thruster is currently on, recalculate fuel consumption.
            Activation_Time :=
              Ada.Real_Time.To_Duration
                (TS => Active_Time + (Ada.Real_Time.Clock - Last_On));
         else
            --  Thruster is currently off, return recorded fuel consumption.
            Activation_Time := Ada.Real_Time.To_Duration (TS => Active_Time);
         end if;
      end Get_Activation_Time;

      procedure Off (Deactivation_Time : in Ada.Real_Time.Time) is
      begin
         --  Ignore off commands if thruster is not active.
         if Last_On /= Global.Start_Time then
            Active_Time := Active_Time + (Deactivation_Time - Last_On);
            Last_On := Global.Start_Time;
         end if;
      end Off;

      procedure On (Activation_Time : in Ada.Real_Time.Time) is
      begin
         --  Ignore On commands if thruster is already active.
         if Last_On = Global.Start_Time then
            Last_On := Activation_Time;
         end if;
      end On;

   end Fuel_Flow_Tracker;

   protected body Thruster_State is

      function Get return State is
        (Current_Thruster_State);

      procedure No_More_Fuel is
      begin
         Fuel_Tank_Empty := True;
         Current_Thruster_State := Disabled;
         Fuel_Flow_Tracker.Off (Deactivation_Time => Ada.Real_Time.Clock);
      end No_More_Fuel;

      procedure Set (Value : in State) is
         Time_Stamp : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
      begin
         if not Fuel_Tank_Empty or else Value = Disabled then
            Current_Thruster_State := Value;

            case Value is

               when Disabled =>
                  Fuel_Flow_Tracker.Off (Deactivation_Time => Time_Stamp);

               when Enabled  =>
                  Fuel_Flow_Tracker.On (Activation_Time => Time_Stamp);
            end case;
         end if;
      end Set;

   end Thruster_State;

   function Burn_Time return Duration is
      Result : Duration;
   begin
      Fuel_Flow_Tracker.Get_Activation_Time (Activation_Time => Result);
      return Result;
   end Burn_Time;

   function Current_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Fuel_State.Get);

   procedure Disable is
   begin
      Thruster_State.Set (Value => Disabled);
   end Disable;

   procedure Enable is
   begin
      Thruster_State.Set (Value => Enabled);
   end Enable;

   function Is_Disabled return Boolean is
     (Thruster_State.Get = Disabled);

   function Is_Enabled return Boolean is
     (Thruster_State.Get = Enabled);

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   procedure Shutdown (Source : in Shared_Types.Legs_Index) is
   begin
      Thruster_State.No_More_Fuel;
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
            Valve_Open_Time : Duration;
            Fuel_Used       : Shared_Types.Fuel_Mass;
         begin
            Fuel_Flow_Tracker.Get_Activation_Time
              (Activation_Time => Valve_Open_Time);
            Fuel_Used :=
              Shared_Types.Fuel_Mass'Min (Initial_Fuel,
                                          Fuel_Flow_Rate * Valve_Open_Time);
            Current_Fuel := Initial_Fuel - Fuel_Used;

            if Current_Fuel = 0.0 then
               Thruster_State.No_More_Fuel;
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
