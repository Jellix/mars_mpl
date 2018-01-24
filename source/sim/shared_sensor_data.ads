with Shared_Types;

package Shared_Sensor_Data with
  Shared_Passive => True
is

   type State is
      record
         Legs       : Shared_Types.All_Legs_State;
         Thruster   : Shared_Types.State;
         Altitude   : Shared_Types.Altitude;
         Velocity   : Shared_Types.Velocity;
         Fuel       : Shared_Types.Fuel_Mass;
         Terminated : Boolean;
      end record;

   Data        : State;
   Bug_Enabled : Boolean := False;

end Shared_Sensor_Data;
