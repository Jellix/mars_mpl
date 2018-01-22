-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Landing_Legs;

package Thrusters is

   Acceleration : constant := -25.600; -- m/s**2
   --  Acceleration of space craft when thrusters are enabled.

   type State is (Disabled, Enabled);

   procedure Enable;
   procedure Disable (Source : Landing_Legs.Legs_Index);
   procedure Disable;     --  Disable thrusters due to safe landing velocity.
   procedure Out_Of_Fuel; --  Disabled thrusters as they ran out of fuel.

   function Current_State return State
     with Volatile_Function;

end Thrusters;
