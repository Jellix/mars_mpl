with Landing_Legs;

package Thrusters with SPARK_Mode is

   procedure Disable (Source : Landing_Legs.Legs_Index);

   function Disabled return Boolean;

end Thrusters;
