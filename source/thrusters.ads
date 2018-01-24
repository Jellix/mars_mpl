-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Planets.Parameters;
with Shared_Types;

package Thrusters is

   Acceleration : constant Float
     := -6.0 * Planets.Parameters.Gravity (Of_Planet => Planets.Mars); -- m/s**2
   --  Acceleration of space craft when thrusters are enabled.

   procedure Enable;
   procedure Disable;     --  Disable thrusters due to safe landing velocity.
   procedure Out_Of_Fuel; --  Disabled thrusters as they ran out of fuel.

   procedure Shutdown (Source : Shared_Types.Legs_Index);

   function Current_State return Shared_Types.State
     with Volatile_Function;

end Thrusters;
