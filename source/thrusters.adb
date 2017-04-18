with Ada.Text_IO;

package body Thrusters is

   procedure Disable (Source : in Landing_Legs.Legs_Index) is
   begin
      Ada.Text_IO.Put_Line
        ("Thrusters have been disabled due to signal from leg"
         & Landing_Legs.Legs_Index'Image (Source)
         & ". We're in free fall.");
   end Disable;

end Thrusters;
