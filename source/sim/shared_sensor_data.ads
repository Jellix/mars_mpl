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
         Time_Stamp : Duration;
         Terminated : Boolean;
      end record;

   protected Current_State is
      procedure Set (Data : in State);
      function  Get return State;
   private
      The_Blob : State :=
                   State'(Legs       => (others => Shared_Types.In_Flight),
                          Thruster   => Shared_Types.Disabled,
                          Altitude   => 0.0,
                          Velocity   => 0.0,
                          Fuel       => 0.0,
                          Time_Stamp => 0.0,
                          Terminated => False);
   end Current_State;

   Bug_Enabled : Boolean := False;

end Shared_Sensor_Data;
