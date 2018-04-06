with Ada.Real_Time;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Rocket_Science;
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

   Initial_Wet_Mass : constant Shared_Types.Mass
     := Shared_Types.Mass (Dry_Mass) + Shared_Types.Mass (Initial_Fuel_Mass);

   Shortest_On_Time : constant Ada.Real_Time.Time_Span :=
                        Ada.Real_Time.To_Time_Span
                          (D =>
                            Shared_Types.To_Duration
                              (Shared_Parameters.Read.Shortest_On_Time));
   --  Shortest on-time for thruster.

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Fuel_Store is new Task_Safe_Store
     (Stored_Type   => Shared_Types.Fuel_Mass,
      Initial_Value => Shared_Parameters.Read.Initial_Fuel_Mass);

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   Fuel_State : Fuel_Store.Shelf;
   Aborted    : Boolean := False
     with Atomic => True;
   pragma Warnings (Off,
                    Aborted,
                    Reason => "Do not warn about atomic synchronization");

   type Valve_Info is
      record
         Fuel_Flow  : State;                   --  Current state of fuel flow.
         Flow_Since : Ada.Real_Time.Time;      --  Time at which the flow was enabled.
         Flow_Total : Ada.Real_Time.Time_Span; --  Accumulated flow duration.
      end record
     with
       Dynamic_Predicate =>
         (Fuel_Flow = Disabled or else Flow_Since > Global.Start_Time);

   protected Engine_State is
      function Get_Total return Duration;
      function Get return State;
      procedure Set (Value : in State);
      procedure No_More_Fuel;
   private
      Fuel_Tank_Empty : Boolean    := False;
      Valve_State     : Valve_Info
        := Valve_Info'(Fuel_Flow  => Disabled,
                       Flow_Since => Global.Start_Time,
                       Flow_Total => Ada.Real_Time.Time_Span_Zero);
   end Engine_State;

   package Valve_Timing is

      procedure Do_Schedule (At_Time : in Ada.Real_Time.Time);
      procedure Do_Cancel;
      procedure Do_Wake_Up;

   end Valve_Timing;

   package body Valve_Timing is separate;

   protected body Engine_State is

      function Get return State is
        (if Fuel_Tank_Empty
           then Disabled
           else Valve_State.Fuel_Flow);

      function Get_Total return Duration is
         Result : Ada.Real_Time.Time_Span;
      begin
         if Valve_State.Fuel_Flow = Enabled then
            --  Thruster is currently on.
            Recalculate_Fuel_Consumption :
            declare
               Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
            begin
               Result :=
                 Valve_State.Flow_Total + (Now - Valve_State.Flow_Since);
            end Recalculate_Fuel_Consumption;
         else
            --  Thruster is currently off, return recorded fuel consumption.
            Result := Valve_State.Flow_Total;
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
               if Valve_State.Fuel_Flow /= Value then
                  --  Changes to the valve state are only done if it is not in
                  --  the requested state yet.
                  if Value = Disabled then
                     Handle_Shortest_On_Time :
                     declare
                        Off_Time : constant Ada.Real_Time.Time :=
                                     Valve_State.Flow_Since + Shortest_On_Time;
                     begin
                        if Off_Time > Now then
                           Valve_Timing.Do_Schedule (At_Time => Off_Time);
                        else
                           --  Cancel scheduled off commands.
                           Valve_Timing.Do_Cancel;

                           Valve_State.Fuel_Flow  := Disabled;
                           Valve_State.Flow_Total :=
                             Valve_State.Flow_Total +
                               (Now - Valve_State.Flow_Since);
                        end if;
                     end Handle_Shortest_On_Time;
                  else
                     --  Cancel pending off commands.
                     Valve_Timing.Do_Cancel;

                     Valve_State.Fuel_Flow  := Enabled;
                     Valve_State.Flow_Since := Now;
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

   function Delta_V return Shared_Types.Velocity is
   begin
      return
        Rocket_Science.Delta_V
          (Initial_Wet_Mass => Initial_Wet_Mass,
           Current_Wet_Mass =>
             Shared_Types.Mass (Dry_Mass) + Shared_Types.Mass (Fuel_State.Get),
           Exhaust_Velocity => Exhaust_Velocity);
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
      Valve_Timing.Do_Wake_Up;
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

end Thrusters;
