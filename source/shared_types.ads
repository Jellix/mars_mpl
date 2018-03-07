--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

--  @summary
--  Provides all type shared between simulator application and the actual GUI.
--
--  @description
--  The package is declared with Remote_Types aspect to ensure proper sharing of
--  these types.
package Shared_Types
  with Pure         => True,
       Remote_Types => True
is

   type Legs_Index is (LL_000, LL_120, LL_240);
   --  Landing legs.
   --  There are three landing legs at 0, 120, and 240 degree.
   --  @value LL_000 The landing leg at 0 degree.
   --  @value LL_120 The landing leg at 120 degree.
   --  @value LL_240 The landing leg at 240 degree.

   type Leg_State is (In_Flight, Touched_Down);
   --  Hall sensor reading from I/O card.
   --
   --  @value In_Flight    No touchdown detected by the hall sensor.
   --  @value Touched_Down Touchdown detected by the hall sensor.

   type All_Legs_State is array (Legs_Index) of Leg_State;
   --  Bundles the state of all legs into a single object.

   --  @summary
   --  Provides a length type.
   package Meter is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 25;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 64;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a length.
   end Meter;

   type Altitude is new Meter.T;
   --  Altitude expressed as m.

   --  @summary
   --  Provides a speed/velocity type.
   package Meter_Per_Second is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 15;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 64;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a speed.
   end Meter_Per_Second;

   type Velocity is new Meter_Per_Second.T;
   --  Velocity expressed in m/s.

   --  @summary
   --  Provides an acceleration type.
   package Meter_Per_Square_Second is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 10;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of an acceleration.
   end Meter_Per_Square_Second;

   type Acceleration is new Meter_Per_Square_Second.T;
   --  (Thruster) acceleration expressed in m/s².

   function "*" (A : in Acceleration;
                 T : in Duration) return Velocity
     with Inline => True;
   --  Multiplies an acceleration with a duration to calculate the according
   --  delta velocity gained by that acceleration.
   --  @param A The acceleration being applied.
   --  @param T The length of time the acceleration is being applied.
   --  @return The resulting delta velocity.

   function "*" (V : in Velocity;
                 T : in Duration) return Altitude
     with Inline => True;
   --  Multiplies a velocity with a duration to calculate the distance being
   --  covered during that time at the given speed.
   --  @param V The velocity.
   --  @param T The length of time.
   --  @return The resulting distance (here, expressed as altitude).

   function "*" (V     : in Velocity;
                 Scale : in Float) return Velocity
     with Inline => True;
   --  Multiplies a velocity with a scale factor.
   --  @param V     The velocity.
   --  @param Scale The scaling factor.
   --  @return The new velocity V'Old scaled by Scale.

   --  @summary
   --  Provides a mass type.
   package Kilogram is
      F : constant := 0.0;
      L : constant := 2.0 ** 9;
      R : constant := 1.0 / 2.0 ** 23;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a mass.
   end Kilogram;

   type Mass is new Kilogram.T;
   --  Mass expressed in kg.

   type Vehicle_Mass is new Mass range       10.0 .. Mass'Last;
   type Fuel_Mass    is new Mass range Mass'First ..     100.0;
   type Flow_Rate    is new Mass range Mass'First ..      10.0;

   function "*" (Left  : in Flow_Rate;
                 Right : in Duration) return Fuel_Mass
     with Inline => True;
   --  Multiplies a flow rate with a duration to calculate the resulting mass.
   --  @param Left The flow rate.
   --  @param Right The duration.
   --  @return The total mass flown during given duration at given flow rate.

   --  @summary Provides a distinct time type.
   package Milliseconds is
      F : constant := 0.0;
      L : constant := 2.0 ** 22;
      R : constant := 1.0 / 2.0 ** 10;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a time in ms.
   end Milliseconds;

   type On_Time is new Milliseconds.T range 0.0 .. 2000.0;
   --  Shortest on-time in milliseconds.

   function To_Duration (T : in On_Time) return Duration;

private

   function "*" (A : in Acceleration;
                 T : in Duration) return Velocity is
     (Velocity (Acceleration'Base (A) * Acceleration'Base (T)));

   function "*" (Left  : in Flow_Rate;
                 Right : in Duration) return Fuel_Mass is
     (Fuel_Mass (Flow_Rate'Base (Left) * Flow_Rate'Base (Right)));

   function "*" (V : in Velocity;
                 T : in Duration) return Altitude is
     (Altitude (Velocity'Base (V) * Velocity'Base (T)));

   function "*" (V     : in Velocity;
                 Scale : in Float) return Velocity is
     (Velocity (Float (V) * Scale));

   function To_Duration (T : in On_Time) return Duration is
      (Duration (T / 1000.0));

end Shared_Types;
