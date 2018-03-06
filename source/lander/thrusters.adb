with Ada.Numerics.Elementary_Functions;
with Ada.Real_Time;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Shared_Parameters.Read;
with Task_Safe_Store;

package body Thrusters is

   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;
   use type Shared_Types.Fuel_Mass;
   use type Shared_Types.Mass;
   use type Shared_Types.Velocity;

   type State is (Disabled, Enabled);
   --  State of thruster.
   --  @value Disabled Thruster disabled, no upwards acceleration.
   --  @value Enabled  Thruster enabled, upwards acceleration accordingly.

   Dry_Mass : constant Shared_Types.Vehicle_Mass
     := Shared_Parameters.Read.Dry_Mass;
   --  Parametrized dry mass of the space craft.

   Exhaust_Velocity : constant Shared_Types.Velocity
     := Shared_Parameters.Read.Exhaust_Velocity;
   --  Parametrized effective exhaust velocity, read once at startup.

   Fuel_Flow_Rate : constant Shared_Types.Flow_Rate
     := Shared_Parameters.Read.Fuel_Flow_Rate;
   --  Parametrized Fuel_Flow_Rate, read once at startup.

   Initial_Fuel_Mass : constant Shared_Types.Fuel_Mass
     := Shared_Parameters.Read.Initial_Fuel_Mass;
   --  Parametrized initial fuel mass, read once at startup.

   Initial_Wet_Mass : constant Float
     := Float (Shared_Types.Mass'Base (Dry_Mass) +
                 Shared_Types.Mass'Base (Initial_Fuel_Mass));

   Min_Pulse_Time : constant Ada.Real_Time.Time_Span :=
                      Ada.Real_Time.Milliseconds (MS => 250);

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
         (not Is_Open or else Open_Since > Global.Start_Time);

   protected Engine_State is
      function Get_Total return Duration;
      function Get return State;
      procedure Set (Value : in State);
      procedure No_More_Fuel;
   private
      Thruster_State  : State      := Disabled;
      Fuel_Tank_Empty : Boolean    := False;
      Valve_State     : Valve_Info
        := Valve_Info'(Is_Open    => False,
                       Open_Since => Global.Start_Time,
                       Total_Open => Ada.Real_Time.Time_Span_Zero);
   end Engine_State;

   task Valve_Control;

   protected Valve_Timing_Control is
      entry Wait_For_Off (At_Time : out Ada.Real_Time.Time);
      procedure Schedule_Off (At_Time : Ada.Real_Time.Time);
      procedure Cancel;
      function Is_Cancelled return Boolean;
      procedure Wake_Up;
   private
      Turn_Off_At : Ada.Real_Time.Time := Global.Start_Time;
      Cancelled   : Boolean            := True;
      Scheduled   : Boolean            := False;
   end Valve_Timing_Control;

   protected body Valve_Timing_Control is
      entry Wait_For_Off (At_Time : out Ada.Real_Time.Time) when Scheduled is
      begin
         At_Time := Turn_Off_At;
      end Wait_For_Off;

      procedure Cancel is
      begin
         Cancelled := True;
         Scheduled := False; --  Close barrier.
      end Cancel;

      function Is_Cancelled return Boolean is
        (Cancelled);

      procedure Schedule_Off (At_Time : Ada.Real_Time.Time) is
      begin
         Turn_Off_At := At_Time;
         Cancelled   := False;
         Scheduled   := True;
         --  trigger waiting task to turn off the valve at given time.
      end Schedule_Off;

      procedure Wake_Up is
      begin
         Turn_Off_At := Global.Start_Time; --  Immediate trigger
         Cancelled   := True; --  Don't do anything.
         Scheduled   := True; --  Open barrier for waiting tasks.
      end Wake_Up;
   end Valve_Timing_Control;

   protected body Engine_State is

      function Get return State is
        (Thruster_State);

      function Get_Total return Duration is
         Result : Ada.Real_Time.Time_Span;
      begin
         if Valve_State.Is_Open then
            --  Thruster is currently on.
            Recalculate_Fuel_Consumption :
            declare
               Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            begin
               Result :=
                 Valve_State.Total_Open + (Now - Valve_State.Open_Since);
            end Recalculate_Fuel_Consumption;
         else
            --  Thruster is currently off, return recorded fuel consumption.
            Result := Valve_State.Total_Open;
         end if;

         return Ada.Real_Time.To_Duration (Result);
      end Get_Total;

      procedure No_More_Fuel is
      begin
         Fuel_Tank_Empty := True;
         Set (Value => Disabled);
         --  FIXME: This off command should be immediate.
      end No_More_Fuel;

      procedure Set (Value : in State) is
      begin
         if not Fuel_Tank_Empty or else Value = Disabled then
            Recalculate_Valve_State :
            declare
               Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            begin
               Thruster_State := Value;

               if Value = Disabled then
                  --  Ignore off commands if thruster is not active.
                  if Valve_State.Is_Open then
                     declare
                        Off_Time : constant Ada.Real_Time.Time :=
                                     Valve_State.Open_Since + Min_Pulse_Time;
                     begin
                        if Off_Time > Now then
                           Valve_Timing_Control.Schedule_Off
                             (At_Time => Off_Time);
                        else
                           --  Cancel scheduled off commands.
                           Valve_Timing_Control.Cancel;

                           Valve_State.Is_Open    := False;
                           Valve_State.Total_Open :=
                             Valve_State.Total_Open +
                               (Now - Valve_State.Open_Since);
                        end if;
                     end;
                  end if;
               else
                  --  Ignore On commands if thruster is already active.
                  if not Valve_State.Is_Open then
                     --  Cancel pending off commands.
                     Valve_Timing_Control.Cancel;

                     Valve_State.Is_Open    := True;
                     Valve_State.Open_Since := Now;
                  end if;
               end if;
            end Recalculate_Valve_State;
         end if;
      end Set;

   end Engine_State;

   function Burn_Time return Duration is
     (Engine_State.Get_Total);

   function Current_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Fuel_State.Get);

   function Delta_V return Shared_Types.Velocity
   is
      function Ln (X : in Float) return Float renames
        Ada.Numerics.Elementary_Functions.Log;

      Current_Wet_Mass : constant Float
        := Float (Shared_Types.Mass'Base (Dry_Mass) +
                    Shared_Types.Mass'Base (Fuel_State.Get));
   begin
      return Exhaust_Velocity * Ln (X => Initial_Wet_Mass / Current_Wet_Mass);
   end Delta_V;

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
      Valve_Timing_Control.Wake_Up;
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

         Recalculate_Fuel_Level :
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
         end Recalculate_Fuel_Level;

         Fuel_State.Set (New_Value => Current_Fuel);
      end loop;

      Log.Trace (Message => "Fuel monitor finished.");
   exception
      when E : others =>
         Log.Trace (E => E);
   end Fuel_Monitor;

   task body Valve_Control is
      At_Time : Ada.Real_Time.Time;
   begin
      while not Aborted loop
         Valve_Timing_Control.Wait_For_Off (At_Time => At_Time);
         delay until At_Time;

         if not Valve_Timing_Control.Is_Cancelled then
            --  Off command was not cancelled yet.
            Engine_State.Set (Value => Disabled);
         end if;
      end loop;
   end Valve_Control;

end Thrusters;
