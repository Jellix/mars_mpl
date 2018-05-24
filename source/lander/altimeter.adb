with Ada.Real_Time;
with Configuration.Cycle_Times;
with Configuration.Task_Offsets;
with Landing_Legs;
with Planets.Parameters;
with Shared_Parameters.Read;
with Rocket_Science;
with Scalar_Elementary_Functions;
with Task_Safe_Store;
with Thrusters;

package body Altimeter is

   use type Ada.Real_Time.Time;
   use type Shared_Types.Degree;
   use type Shared_Types.Kelvin;
   use type Shared_Types.Kilogram;
   use type Shared_Types.Meter;
   use type Shared_Types.Meter_Per_Second;
   use type Shared_Types.Meter_Per_Square_Second;
   use type Shared_Types.Scalar;

   Gravity             : constant Shared_Types.Meter_Per_Square_Second :=
                           Shared_Types.Meter_Per_Square_Second
                             (Planets.Parameters.Gravity (Planets.Mars));

   Initial_Velocity    : constant Shared_Types.Meter_Per_Second
     := Shared_Parameters.Read.Initial_Velocity;

   Initial_Lander_Mass : constant Shared_Types.Vehicle_Mass
     := Shared_Parameters.Read.Dry_Mass;

   Upper_Atmosphere    : constant Shared_Types.Meter := 125_000.0;

   Heatshield_Mass     : constant Shared_Types.Kilogram := 140.0;
   Cruise_Stage_Mass   : constant Shared_Types.Kilogram :=  82.0;

   Heat_Capacity       : constant := Shared_Types.Mass.T (Heatshield_Mass) * 15.4; -- 13.6, 15.4, 17.1
   Heat_Flow           : constant := 0.0000325; -- 0.0000100, 0.0000325, 0.0000550

   --  Relevant spacecraft masses during EDL.
   Dry_Mass_Before_EDL : constant Shared_Types.Kilogram
     := Cruise_Stage_Mass + Heatshield_Mass + Initial_Lander_Mass;

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Attitude_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Degree,
                      Initial_Value => Shared_Parameters.Read.Initial_Attitude);
   Attitude_State : Attitude_Store.Shelf;

   package Altitude_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Meter,
                      Initial_Value => Shared_Parameters.Read.Initial_Altitude);
   Altitude_State : Altitude_Store.Shelf;

   package Drag_Coefficient_Store is
      new Task_Safe_Store (Stored_Type   => Shared_Types.Scalar,
                           Initial_Value => 0.0);
   Drag_Coefficient : Drag_Coefficient_Store.Shelf;

   package Drag_Store is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Meter_Per_Square_Second,
                      Initial_Value => 0.0);
   Drag_State : Drag_Store.Shelf;

   package Spacecraft_Mass_Store is
     new Task_Safe_Store (Stored_Type   => Shared_Types.Kilogram,
                          Initial_Value => Dry_Mass_Before_EDL);
   Spacecraft_Dry_Mass : Spacecraft_Mass_Store.Shelf;

   package Temperature_Store is
     new Task_Safe_Store (Stored_Type   => Shared_Types.Kelvin,
                          Initial_Value => 3.1999969482421875);
   Core_Temperature    : Temperature_Store.Shelf;
   Surface_Temperature : Temperature_Store.Shelf;

   package Velocity_Store  is new
     Task_Safe_Store (Stored_Type   => Shared_Types.Meter_Per_Second,
                      Initial_Value => Shared_Parameters.Read.Initial_Velocity);
   Velocity_State : Velocity_Store.Shelf;

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   function Current_Attitude return Shared_Types.Degree is
      (Attitude_State.Get);

   function Current_Altitude return Shared_Types.Meter is
     (Altitude_State.Get);

   function Current_Core_Temperature return Shared_Types.Kelvin is
      (Core_Temperature.Get);

   function Current_Drag return Shared_Types.Meter_Per_Square_Second is
      (Drag_State.Get);

   function Current_Dry_Mass return Shared_Types.Vehicle_Mass is
     (Shared_Types.Vehicle_Mass (Spacecraft_Dry_Mass.Get));

   function Current_Surface_Temperature return Shared_Types.Kelvin is
      (Surface_Temperature.Get);

   function Current_Velocity return Shared_Types.Meter_Per_Second is
     (Velocity_State.Get);

   procedure Deploy_Parachute is
   begin
      Drag_Coefficient.Set (New_Value => 0.732); -- 0.725, 0.732, 0.738
   end Deploy_Parachute;

   procedure Enter_Atmosphere is
   begin
      Drag_Coefficient.Set (New_Value => 0.0813); -- 0.0750, 0.0813, 0.0875
   end Enter_Atmosphere;

   procedure Jettison_Heatshield is
   begin
      Spacecraft_Dry_Mass.Subtract (X => Heatshield_Mass);
      Drag_Coefficient.Set (New_Value => 0.457); -- 0.450, 0.457, 0.463
      Core_Temperature.Set (New_Value => 293.0);
      Surface_Temperature.Set (New_Value => 293.0);
   end Jettison_Heatshield;

   procedure Separate_Cruise_Stage is
   begin
      Spacecraft_Dry_Mass.Subtract (X => Cruise_Stage_Mass);
   end Separate_Cruise_Stage;

   procedure Separate_Lander is
   begin
      Drag_Coefficient.Set (New_Value => 0.457); -- 0.450, 0.457, 0.463
   end Separate_Lander;

   Aborted : Boolean := False
     with Atomic;
   pragma Warnings (Off,
                    Aborted,
                    Reason => "Do not warn about atomic synchronization");

   procedure Shutdown is
   begin
      Aborted := True;
   end Shutdown;

   task Radar_Simulator;

   task body Radar_Simulator is
      T              : constant Duration
        := Ada.Real_Time.To_Duration (Configuration.Cycle_Times.Altitude_Task);
      --  Duration of a single task cycle.

      Next_Cycle     : Ada.Real_Time.Time
        := Global.Start_Time + Configuration.Task_Offsets.Altitude_Task;
      Altitude_Now   : Shared_Types.Meter            := Altitude_State.Get;
      Velocity_Now   : Shared_Types.Meter_Per_Second := Velocity_State.Get;
      Drag_Delta_V   : Shared_Types.Meter_Per_Second := 0.0; --  accumulated delta v due to drag effects.
      Grav_Delta_V   : Shared_Types.Meter_Per_Second := 0.0; --  accumulated delta v due to gravity
      Attitude_Now   : Shared_Types.Degree;

      function Cos (X     : in Shared_Types.Scalar;
                    Cycle : in Shared_Types.Scalar := 360.0) return Shared_Types.Scalar
                    renames Scalar_Elementary_Functions.Cos;
      function Sin (X     : in Shared_Types.Scalar;
                    Cycle : in Shared_Types.Scalar := 360.0) return Shared_Types.Scalar
                    renames Scalar_Elementary_Functions.Sin;
      function Arctan (Y     : in Shared_Types.Scalar;
                       X     : in Shared_Types.Scalar;
                       Cycle : in Shared_Types.Scalar := 360.0) return Shared_Types.Scalar
                       renames Scalar_Elementary_Functions.Arctan;

      Delta_Vertical   : Shared_Types.Meter;
      Delta_Horizontal : Shared_Types.Meter;
   begin
      Log.Trace (Message => "Altitude control monitor started.");

      while not Aborted and then Altitude_Now > 0.0 loop
         delay until Next_Cycle;

         Attitude_Now := Attitude_State.Get;

         declare
            Attitude_Sin : constant Shared_Types.Scalar :=
                             Sin (X => Shared_Types.Scalar (Attitude_Now));
            Attitude_Cos : constant Shared_Types.Scalar :=
                             Cos (X => Shared_Types.Scalar (Attitude_Now));
         begin
            --  Calculate change in velocity according to gravity effects.
            Calculate_Delta_V :
            declare
               Grav_DV_Cycle : constant Shared_Types.Meter_Per_Second :=
                                 Gravity * T * Attitude_Sin;
            begin
               Grav_Delta_V := Grav_Delta_V + Grav_DV_Cycle;
            end Calculate_Delta_V;

            --  Calculate change in altitude according to current velocity.
            Calculate_Delta_A :
            declare
               Delta_A : constant Shared_Types.Meter :=
                           Velocity_Now * T * Attitude_Sin;
               --  Attitude 0 means parallel to ground, so no change in height.
            begin
               Altitude_Now :=
                 Altitude_Now - Shared_Types.Meter'Min (Altitude_Now, Delta_A);
               Altitude_State.Set (New_Value => Altitude_Now);
            end Calculate_Delta_A;

            Calculate_Drag :
            declare
               --  Retrieve dynamic parameters.
               Drag_Constant : constant Shared_Types.Scalar :=
                                 Drag_Coefficient.Get;
               Fuel_Mass     : constant Shared_Types.Fuel_Mass :=
                                 Thrusters.Current_Fuel_Mass;
               Current_Mass  : constant Shared_Types.Kilogram :=
                                 Spacecraft_Dry_Mass.Get + Fuel_Mass;

               --  Drag calculation including depth in atmosphere, velocity loss
               --  and corresponding energy dissipation due to the drag (aka.
               --  aero braking).
               --  Air density is a normalized value denoting the inverse of the
               --  entry depth and is used to alter the drag coefficient
               --  depending on the current altitude.
               Entry_Depth   : constant Shared_Types.Scalar :=
                                 Shared_Types.Scalar'Min
                                   (1.0, Altitude_Now / Upper_Atmosphere);
               Air_Density   : constant Shared_Types.Scalar :=
                                 1.0 -
                                   Scalar_Elementary_Functions.Sqrt
                                     (Entry_Depth);
               -- Assume an inverse quadratic relationship (gravity).

               Drag_Now      : constant Shared_Types.Meter_Per_Square_Second :=
                                 Rocket_Science.Drag
                                   (Current_Wet_Mass => Current_Mass,
                                    Velocity         => Velocity_Now,
                                    Drag_Constant    =>
                                      Drag_Constant * Air_Density);
               Drag_DV_Cycle : constant Shared_Types.Meter_Per_Second :=
                                 Drag_Now * T;
               V_Squared     : constant Shared_Types.Scalar :=
                                 Shared_Types.Scalar (Drag_DV_Cycle) *
                                 Shared_Types.Scalar (Drag_DV_Cycle);
               E_Kin         : constant Shared_Types.Scalar :=
                                 Shared_Types.Scalar (Current_Mass) *
                                 V_Squared / 2.0;
            begin
               Drag_State.Set (New_Value => Drag_Now);
               Drag_Delta_V := Drag_Delta_V + Drag_DV_Cycle;
               Surface_Temperature.Add
                 (X => Shared_Types.Kelvin (E_Kin / Heat_Capacity));

               Calculate_Heat_Flow :
               declare
                  Surface  : constant Shared_Types.Kelvin :=
                               Surface_Temperature.Get;
                  Core     : constant Shared_Types.Kelvin :=
                               Core_Temperature.Get;
                  Gradient : constant Shared_Types.Kelvin := Surface - Core;
                  Delta_T  : constant Shared_Types.Kelvin :=
                               Gradient * Heat_Flow;
               begin
                  Core_Temperature.Add (X => Delta_T);
                  Surface_Temperature.Add (X => -Delta_T);
               end Calculate_Heat_Flow;
            end Calculate_Drag;

            --  Update attitude according to current velocity vector.
            Delta_Vertical   :=
              Gravity * T * T + Velocity_Now * T * Attitude_Sin;
            Delta_Horizontal := Velocity_Now * T * Attitude_Cos;
         end;

         Attitude_Now :=
           Shared_Types.Degree
             (Arctan (Y => Shared_Types.Scalar (Delta_Vertical),
                      X => Shared_Types.Scalar (Delta_Horizontal)));

         Attitude_State.Set (New_Value => Attitude_Now);

         --  Sum all delta Vs according to their sign.
         Velocity_Now :=
           Initial_Velocity +
             (Grav_Delta_V - Thrusters.Delta_V - Drag_Delta_V);

         Velocity_State.Set (New_Value => Velocity_Now);

         if Altitude_Now = 0.0 then
            Landing_Legs.Touchdown;
            Log.Trace (Message => "Touchdown triggered due to ground zero.");
         end if;

         Next_Cycle := Next_Cycle + Configuration.Cycle_Times.Altitude_Task;
      end loop;

      Log.Trace (Message => "Altitude control monitor finished.");
   exception
      when E : others =>
         Log.Trace (E => E);
   end Radar_Simulator;

end Altimeter;
