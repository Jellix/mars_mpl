with Ada.Numerics.Elementary_Functions;

package body Rocket_Science is

   use type Shared_Types.Velocity;

   function Delta_V
     (Initial_Wet_Mass : in Shared_Types.Mass;
      Current_Wet_Mass : in Shared_Types.Mass;
      Exhaust_Velocity : in Shared_Types.Velocity) return Shared_Types.Velocity
   is
      function Ln (X : in Float) return Float renames
        Ada.Numerics.Elementary_Functions.Log;
   begin
      return
        Exhaust_Velocity *
          Ln (X => Float (Initial_Wet_Mass / Current_Wet_Mass));
   end Delta_V;

end Rocket_Science;
