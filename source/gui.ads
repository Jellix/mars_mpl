with Altimeter;
with Landing_Legs;

package GUI is

   type State is
      record
         Legs     : Landing_Legs.All_Legs_State;
         Thruster : Boolean;
         Height   : Altimeter.Height;
         Velocity : Altimeter.Velocity;
      end record;

   procedure Update (New_State : State);

end GUI;
