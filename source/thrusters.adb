with Altimeter;
with Global;

package body Thrusters is

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
         Global.Log (Message =>
                       "Signal from leg " &
                       Landing_Legs.Legs_Index'Image (Source) & ".");
      else
         declare
            Current_Altitude : Altimeter.Altitude;
         begin
            Altimeter.Current_Altitude (A => Current_Altitude);
            Global.Log
              (Message =>
                 "Thrusters have been disabled due to signal from leg " &
                 Landing_Legs.Legs_Index'Image (Source) & ", at height" &
                 Altimeter.Image (A => Current_Altitude) & ".");
         end;
      end if;
   end Disable;

   procedure Get_State (Disabled : out Boolean) is
   begin
      Disabled := Thruster.Is_Disabled;
   end Get_State;

end Thrusters;
