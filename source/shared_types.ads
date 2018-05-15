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
   --  Provides an angle type.
   package Angle is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 10;
      R : constant := 1.0 / 2.0 ** 21;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of an angle.
   end Angle;

   type Degree is new Angle.T;

   --  @summary
   --  Provides a length type.
   package Distance is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 25;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 64;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a length.
   end Distance;

   type Meter is new Distance.T;

   --  @summary
   --  Provides a speed/velocity type.
   package Speed is
      F : constant := -2.0 ** 10;
      L : constant := 2.0 ** 15;
      R : constant := 1.0 / 2.0 ** 20;
      S : constant := 64;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a speed.
   end Speed;

   type Meter_Per_Second is new Speed.T;

   function "*" (V : in Meter_Per_Second;
                 T : in Duration) return Meter
     with Inline => True;
   --  Multiplies a velocity with a duration to calculate the distance being
   --  covered during that time at the given speed.
   --  @param V The velocity.
   --  @param T The length of time.
   --  @return The resulting distance (here, expressed as altitude).

   function "*" (V     : in Meter_Per_Second;
                 Scale : in Float) return Meter_Per_Second
     with Inline => True;
   --  Multiplies a velocity with a scale factor.
   --  @param V     The velocity.
   --  @param Scale The scaling factor.
   --  @return The new velocity V'Old scaled by Scale.

   --  @summary
   --  Provides an acceleration type.
   package Acceleration is
      F : constant := -2.0 ** 32;
      L : constant := 2.0 ** 32;
      R : constant := 1.0 / 2.0 ** 31;
      S : constant := 64;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of an acceleration.
   end Acceleration;

   type Meter_Per_Square_Second is new Acceleration.T;
   --  (Thruster) acceleration expressed in m/s².

   function "*" (A : in Meter_Per_Square_Second;
                 T : in Duration) return Meter_Per_Second
     with Inline => True;
   --  Multiplies an acceleration with a duration to calculate the according
   --  delta velocity gained by that acceleration.
   --  @param A The acceleration being applied.
   --  @param T The length of time the acceleration is being applied.
   --  @return The resulting delta velocity.

   --  @summary
   --  Provides a mass type.
   package Mass is
      F : constant := 0.0;
      L : constant := 2.0 ** 10;
      R : constant := 1.0 / 2.0 ** 22;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a mass.
   end Mass;

   type Kilogram is new Mass.T;
   --  Mass expressed in kg.

   type Vehicle_Mass is new Kilogram range           10.0 .. Kilogram'Last;
   type Fuel_Mass    is new Kilogram range Kilogram'First .. 100.0;
   type Flow_Rate    is new Kilogram range Kilogram'First ..  10.0;

   function "+" (Left  : in Kilogram;
                 Right : in Fuel_Mass) return Kilogram
     with Inline => True;

   function "+" (Left  : in Fuel_Mass;
                 Right : in Kilogram) return Kilogram
     with Inline => True;

   function "+" (Left  : in Kilogram;
                 Right : in Vehicle_Mass) return Kilogram
     with Inline => True;

   function "+" (Left  : in Vehicle_Mass;
                 Right : in Kilogram) return Kilogram
     with Inline => True;

   function "+" (Left  : in Vehicle_Mass;
                 Right : in Fuel_Mass) return Kilogram
     with Inline => True;

   function "+" (Left  : in Fuel_Mass;
                 Right : in Vehicle_Mass) return Kilogram
     with Inline => True;

   function "*" (Left  : in Flow_Rate;
                 Right : in Duration) return Fuel_Mass
     with Inline => True;
   --  Multiplies a flow rate with a duration to calculate the resulting mass.
   --  @param Left The flow rate.
   --  @param Right The duration.
   --  @return The total mass flown during given duration at given flow rate.

   --  @summary Provides a distinct time type.
   package Time is
      F : constant := 0.0;
      L : constant := 2.0 ** 22;
      R : constant := 1.0 / 2.0 ** 10;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of a time in ms.
   end Time;

   type Milliseconds is new Time.T;

   type On_Time is new Milliseconds range 0.0 .. 2000.0;
   --  Shortest on-time in milliseconds.

   function To_Duration (T : in On_Time) return Duration;

   package Temperature is
      F : constant := -2.0 ** 31; --  We need temperature differences as well.
      L : constant := 2.0 ** 31;
      R : constant := 1.0 / 2.0 ** 32;
      S : constant := 64;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
   end Temperature;

   type Kelvin is new Temperature.T;

private

   function "*" (V : in Meter_Per_Second;
                 T : in Duration) return Meter is
     (Meter (Meter_Per_Second'Base (V) * Meter_Per_Second'Base (T)));

   function "*" (V     : in Meter_Per_Second;
                 Scale : in Float) return Meter_Per_Second is
     (Meter_Per_Second (Float (V) * Scale));

   function "*" (A : in Meter_Per_Square_Second;
                 T : in Duration) return Meter_Per_Second is
     (Meter_Per_Second (Meter_Per_Square_Second'Base (A) * Meter_Per_Square_Second'Base (T)));

   function "+" (Left  : in Kilogram;
                 Right : in Fuel_Mass) return Kilogram is
     (Left + Kilogram'Base (Right));

   function "+" (Left  : in Fuel_Mass;
                 Right : in Kilogram) return Kilogram is
     (Kilogram'Base (Left) + Right);

   function "+" (Left  : in Kilogram;
                 Right : in Vehicle_Mass) return Kilogram is
     (Left + Kilogram'Base (Right));

   function "+" (Left  : in Vehicle_Mass;
                 Right : in Kilogram) return Kilogram is
     (Kilogram'Base (Left) + Right);

   function "+" (Left  : in Vehicle_Mass;
                 Right : in Fuel_Mass) return Kilogram is
     (Kilogram'Base (Left) + Kilogram'Base (Right));

   function "+" (Left  : in Fuel_Mass;
                 Right : in Vehicle_Mass) return Kilogram is
     (Kilogram'Base (Left) + Kilogram'Base (Right));

   function "*" (Left  : in Flow_Rate;
                 Right : in Duration) return Fuel_Mass is
     (Fuel_Mass (Flow_Rate'Base (Left) * Flow_Rate'Base (Right)));

   function To_Duration (T : in On_Time) return Duration is
      (Duration (T / 1000.0));

end Shared_Types;
