with Global;

package body Thrusters is

   protected Thruster is
      function Get return State;
      procedure Set (New_State : in     State;
                     Old_State :    out State);
   private
      Current_Thruster_State : State := Enabled;
   end Thruster;

   protected body Thruster is
      function Get return State is
      begin
         return Current_Thruster_State;
      end Get;

      procedure Set (New_State : in     State;
                     Old_State :    out State) is
      begin
         Old_State              := Current_Thruster_State;
         Current_Thruster_State := New_State;
      end Set;
   end Thruster;

   procedure Disable (Source : in Landing_Legs.Legs_Index) is
      Old_State : State;
   begin
      Thruster.Set (New_State => Disabled,
                    Old_State => Old_State);

      if Old_State = Disabled then
         Global.Log (Message =>
                       "Signal from leg " &
                       Landing_Legs.Legs_Index'Image (Source) & ".");
      else
         Global.Log
           (Message =>
              "Thrusters have been disabled due to signal from leg " &
              Landing_Legs.Legs_Index'Image (Source) & ".");
      end if;
   end Disable;

   function Current_State return State is (Thruster.Get);

end Thrusters;
