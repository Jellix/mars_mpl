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

   type Scalar is new Long_Float; -- aka. digits 15

   --  @summary
   --  Provides an angle type.
   package Angle is
      F : constant := -2.0 ** 9;
      L : constant := 2.0 ** 9;
      R : constant := 1.0 / 2.0 ** 22;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
      --  The fixed point representation of an angle.
   end Angle;

   type Degree is new Angle.T;

   function "*" (Left  : in Degree;
                 Right : in Degree) return Degree is abstract;

   function "/" (Left  : in Degree;
                 Right : in Degree) return Degree is abstract;

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

   function "*" (Left  : in Meter;
                 Right : in Meter) return Meter is abstract;

   function "*" (Left  : in Meter;
                 Right : in Scalar) return Meter
     with Inline => True;

   function "/" (Left  : in Meter;
                 Right : in Meter) return Meter is abstract;

   function "/" (Left  : in Meter;
                 Right : in Meter) return Scalar
     with Inline => True;

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

   function "*" (Left  : in Meter_Per_Second;
                 Right : in Meter_Per_Second) return Meter_Per_Second
                 is abstract;

   function "/" (Left  : in Meter_Per_Second;
                 Right : in Meter_Per_Second) return Meter_Per_Second
                 is abstract;

   function "*" (V : in Meter_Per_Second;
                 T : in Duration) return Meter
     with Inline => True;
   --  Multiplies a velocity with a duration to calculate the distance being
   --  covered during that time at the given speed.
   --  @param V The velocity.
   --  @param T The length of time.
   --  @return The resulting distance (here, expressed as altitude).

   function "*" (V     : in Meter_Per_Second;
                 Scale : in Scalar) return Meter_Per_Second
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

   function "*" (Left  : in Meter_Per_Square_Second;
                 Right : in Meter_Per_Square_Second)
                 return Meter_Per_Square_Second is abstract;

   function "/" (Left  : in Meter_Per_Square_Second;
                 Right : in Meter_Per_Square_Second)
                 return Meter_Per_Square_Second is abstract;

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

   function "*" (Left  : in Kilogram;
                 Right : in Kilogram) return Kilogram is abstract;

   function "*" (Left  : in Kilogram;
                 Right : in Scalar) return Kilogram
     with Inline => True;

   function "/" (Left  : in Kilogram;
                 Right : in Kilogram) return Kilogram is abstract;

   function "/" (Left  : in Kilogram;
                 Right : in Kilogram) return Scalar
     with Inline => True;

   type Vehicle_Mass is new Kilogram range           10.0 .. Kilogram'Last;
   type Fuel_Mass    is new Kilogram range Kilogram'First .. 100.0;

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

   package Mass_Flow is
      F : constant := 0.0;
      L : constant := 2.0 ** 4;
      R : constant := 1.0 / 2.0 ** 28;
      S : constant := 32;

      type T is delta R range F .. L - R with
        Size  => S,
        Small => R;
   end Mass_Flow;

   type Kilogram_Per_Second is new Mass_Flow.T;

   function "*" (Left  : in Kilogram_Per_Second;
                 Right : in Kilogram_Per_Second) return Kilogram_Per_Second
                 is abstract;

   function "*" (Left  : in Kilogram_Per_Second;
                 Right : in Duration) return Fuel_Mass
     with Inline => True;
   --  Multiplies a flow rate with a duration to calculate the resulting mass.
   --  @param Left The flow rate.
   --  @param Right The duration.
   --  @return The total mass flown during given duration at given flow rate.

   function "/" (Left  : in Kilogram_Per_Second;
                 Right : in Kilogram_Per_Second) return Kilogram_Per_Second
                 is abstract;

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

   function "*" (Left  : in Milliseconds;
                 Right : in Milliseconds) return Milliseconds is abstract;

   function "/" (Left  : in Milliseconds;
                 Right : in Milliseconds) return Milliseconds is abstract;

   type On_Time is new Milliseconds range 0.0 .. 2000.0;
   --  Shortest on-time in milliseconds.

   function To_Duration (T : in On_Time) return Duration
     with Inline => True;

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

   function "*" (Left  : in Kelvin;
                 Right : in Kelvin) return Kelvin is abstract;

   function "*" (Left  : in Kelvin;
                 Right : in Scalar) return Kelvin
     with Inline => True;

   function "/" (Left  : in Kelvin;
                 Right : in Kelvin) return Kelvin is abstract;

private

   --  Meter
   function "*" (Left  : in Meter;
                 Right : in Scalar) return Meter is
      (Meter (Scalar (Left) * Right));

   function "/" (Left  : in Meter;
                 Right : in Meter) return Scalar is
     (Scalar (Distance.T (Left) / Distance.T (Right)));

   --  Meter_Per_Second
   function "*" (V : in Meter_Per_Second;
                 T : in Duration) return Meter is
     (Meter (Scalar (V) * Scalar (T)));

   function "*" (V     : in Meter_Per_Second;
                 Scale : in Scalar) return Meter_Per_Second is
     (Meter_Per_Second (Scalar (V) * Scale));

   --  Meter_Per_Square_Second
   function "*" (A : in Meter_Per_Square_Second;
                 T : in Duration) return Meter_Per_Second is
     (Meter_Per_Second (Scalar (A) * Scalar (T)));

   --  Kilogram
   function "*" (Left  : in Kilogram;
                 Right : in Scalar) return Kilogram is
      (Kilogram (Scalar (Left) * Right));

   function "/" (Left  : in Kilogram;
                 Right : in Kilogram) return Scalar is
      (Scalar (Mass.T (Left) / Mass.T (Right)));

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

   --  Kilogram_Per_Second
   function "*" (Left  : in Kilogram_Per_Second;
                 Right : in Duration) return Fuel_Mass is
     (Fuel_Mass (Scalar (Left) * Scalar (Right)));

   --  On_Time
   function To_Duration (T : in On_Time) return Duration is
      (Duration (Time.T (T) / 1000.0));

   --  Kelvin
   function "*" (Left  : in Kelvin;
                 Right : in Scalar) return Kelvin is
      (Kelvin (Scalar (Left) * Right));

end Shared_Types;
