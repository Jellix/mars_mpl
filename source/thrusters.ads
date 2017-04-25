-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Landing_Legs;

package Thrusters is

   procedure Disable (Source : Landing_Legs.Legs_Index);
   procedure Get_State (Disabled : out Boolean);

end Thrusters;
