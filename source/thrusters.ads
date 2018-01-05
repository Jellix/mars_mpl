-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Landing_Legs;

package Thrusters is

   type State is (Disabled, Enabled);

   procedure Disable (Source : Landing_Legs.Legs_Index);

   function Current_State return State
     with Volatile_Function;

end Thrusters;
