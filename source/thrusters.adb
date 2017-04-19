--pragma Profile (Ravenscar);
--pragma Partition_Elaboration_Policy (Sequential);

with Altimeter;
with Global;

package body Thrusters with SPARK_Mode is

   protected Thruster is
      procedure Disable (Old_Disabled : out Boolean);
      function Is_Disabled return Boolean;
   private
      Disabled : Boolean := False;
   end Thruster;

   protected body Thruster is
      procedure Disable (Old_Disabled : out Boolean) is
      begin
         Old_Disabled := Disabled;
         Disabled     := True;
      end Disable;

      function Is_Disabled return Boolean is
      begin
         return Disabled;
      end Is_Disabled;
   end Thruster;

   procedure Disable (Source : in Landing_Legs.Legs_Index) is
      Old_Disabled : Boolean;
   begin
      Thruster.Disable (Old_Disabled => Old_Disabled);

      if Old_Disabled then
         Global.Log ("Signal from leg " & Landing_Legs.Legs_Index'Image (Source) & ".");
      else
         Global.Log ("Thrusters have been disabled due to signal from leg " &
                       Landing_Legs.Legs_Index'Image (Source) & ", at height" &
                       Altimeter.Image (Altimeter.Current_Height) & ".");
      end if;
   end Disable;

   function Disabled return Boolean with SPARK_Mode => Off is
   begin
      return Thruster.Is_Disabled;
   end Disabled;

end Thrusters;
