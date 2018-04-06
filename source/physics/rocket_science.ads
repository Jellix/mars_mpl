with Shared_Types;

package Rocket_Science is

   --  Tsiolkovsky rocket equation.
   --  Returns the delta V for given fuel masses and exhaust velocity.
   function Delta_V
     (Initial_Wet_Mass : in Shared_Types.Mass;
      Current_Wet_Mass : in Shared_Types.Mass;
      Exhaust_Velocity : in Shared_Types.Velocity) return Shared_Types.Velocity;

end Rocket_Science;
