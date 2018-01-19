with Altimeter;
with Engine;
with Landing_Legs;
with Thrusters;

package GUI is

   type State is
      record
         Legs       : Landing_Legs.All_Legs_State;
         Thruster   : Thrusters.State;
         Altitude   : Altimeter.Altitude;
         Velocity   : Altimeter.Velocity;
         Fuel       : Engine.Fuel_Mass;
         Terminated : Boolean;
      end record;

   procedure Update (New_State : State);

   Aborted : Boolean := False;
   pragma Atomic (Aborted);

end GUI;
