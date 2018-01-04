with Ada.Real_Time;

with Altimeter;
with Landing_Legs;

package GUI is

   type State is
      record
         Legs       : Landing_Legs.All_Legs_State;
         Thruster   : Boolean;
         Altitude   : Altimeter.Altitude;
         Velocity   : Altimeter.Velocity;
         Terminated : Boolean;
         Time_Stamp : Ada.Real_Time.Time;
      end record;

   procedure Update (New_State : State);

end GUI;
