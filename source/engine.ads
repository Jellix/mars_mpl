with Ada.Real_Time;

package Engine is

   type Fuel_Mass is delta 1.0 / 2 ** 24 range 0.0 .. 10_000.0;

   Cycle : constant Ada.Real_Time.Time_Span :=
             Ada.Real_Time.Milliseconds (10);

   function Remaining_Fuel return Fuel_Mass;

   function Image (Value : in Fuel_Mass) return String;

   procedure Shutdown;

end Engine;
