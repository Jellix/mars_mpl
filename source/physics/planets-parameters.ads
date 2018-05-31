-- @summary
-- Provides planet specific parameters
--
-- @description
-- Child package of Planets, providing various physical parameters of them.
with Shared_Types;

package Planets.Parameters
  with Pure => True
is

   type Property is tagged private;

   function Average_Gravity (This : in Property) return Shared_Types.Scalar;
   function Estimated_Mass  (This : in Property) return Shared_Types.Scalar;
   function Mean_Radius     (This : in Property) return Shared_Types.Scalar;

   function Properties_Of (Planet : in Planet_Name) return Property;

private

   type Property is tagged
      record
         Average_Gravity : Shared_Types.Scalar;
         --  The (average) gravity of the given planet in m/s².
         Estimated_Mass  : Shared_Types.Scalar;
         --  The (estimated) mass of the given planet in kg.
         Mean_Radius     : Shared_Types.Scalar;
         --  The (mean) radius of the given planet in m.
      end record;

   function Average_Gravity (This : in Property) return Shared_Types.Scalar is
     (This.Average_Gravity);

   function Estimated_Mass (This : in Property) return Shared_Types.Scalar is
     (This.Estimated_Mass);

   function Mean_Radius (This : in Property) return Shared_Types.Scalar is
     (This.Mean_Radius);

   type Properties_List is array (Planet_Name'Range) of Property;

   Properties : constant Properties_List
     := Properties_List'(Mercury => Property'(Average_Gravity => 3.700,
                                              Estimated_Mass  => 3.285E23,
                                              Mean_Radius     => 2_439_700.0),
                         Venus   => Property'(Average_Gravity => 8.870,
                                              Estimated_Mass  => 4.867E24,
                                              Mean_Radius     => 6_051_800.0),
                         Earth   => Property'(Average_Gravity => 9.807,
                                              Estimated_Mass  => 5.972E24,
                                              Mean_Radius     => 6_371_000.0),
                         Mars    => Property'(Average_Gravity => 3.711,
                                              Estimated_Mass  => 6.390E23,
                                              Mean_Radius     => 3_389_500.0),
                         Jupiter => Property'(Average_Gravity => 24.790,
                                              Estimated_Mass  => 1.898E27,
                                              Mean_Radius     => 69_911_000.0),
                         Saturn  => Property'(Average_Gravity => 10.440,
                                              Estimated_Mass  => 5.683E26,
                                              Mean_Radius     => 58_232_000.0),
                         Uranus  => Property'(Average_Gravity => 8.690,
                                              Estimated_Mass  => 8.681E25,
                                              Mean_Radius     => 25_362_000.0),
                         Neptune => Property'(Average_Gravity => 11.150,
                                              Estimated_Mass  => 1.024E26,
                                              Mean_Radius     => 24_622_000.0));

   function Properties_Of (Planet : in Planet_Name) return Property is
     (Properties (Planet));

end Planets.Parameters;
