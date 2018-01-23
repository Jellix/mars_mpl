with Ada.Real_Time;

package Engine is

   Fuel_Resolution : constant := 1.0 / 2.0 ** 25;
   type Fuel_Mass is delta Fuel_Resolution range 0.0 .. 2.0 ** 7 - Fuel_Resolution
     with Size => 32;

   Flow_Rate : constant Fuel_Mass := 0.044_999_986_886_978_149_414_062_500; -- equals ~4.5 kg/s

   Cycle : constant Ada.Real_Time.Time_Span :=
             Ada.Real_Time.Milliseconds (10);

   function Remaining_Fuel return Fuel_Mass;

   function Image (Value : in Fuel_Mass) return String;

   procedure Shutdown;

end Engine;
