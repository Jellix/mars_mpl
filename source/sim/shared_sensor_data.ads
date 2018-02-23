with Shared_Types;

--  @summary
--  Provides the shared data from the sensors (i.e. monitoring and control
--  tasks).
--
--  @description
--  Shared_Passive package to provide sensor data across all partitions.
package Shared_Sensor_Data with
  Shared_Passive => True
is

   type State is
      record
         Legs             : Shared_Types.All_Legs_State;
         Thruster_Enabled : Boolean;
         Altitude         : Shared_Types.Altitude;
         Velocity         : Shared_Types.Velocity;
         Fuel             : Shared_Types.Fuel_Mass;
         Time_Stamp       : Duration;
         Terminated       : Boolean;
      end record;
   --  The full space craft sensor state.
   --  @field Legs       State of all landing legs.
   --  @field Thruster   State of thruster.
   --  @field Altitude   Current altitude of space craft.
   --  @field Velocity   Current velocity of space craft.
   --  @field Fuel       Fuel mass left in tank.
   --  @field Time_Stamp (Relative) time since simulation start. Denotes the
   --                    (rough) sampling time of all of the above.
   --  @field Terminated Indicates that the simulation has been finished and no
   --                    more updates will be incoming.

   protected Current_State is

      procedure Set (Data : in State);
      --  Sets a new state.
      --  @param Data The new set of sensor data to be stored.

      function Get return State;
      --  Retrieves the latest state.
      --  @return Latest state of space craft.

   private
      The_Blob : State
        := State'(Legs             => (others => Shared_Types.In_Flight),
                  Thruster_Enabled => False,
                  Altitude         => 0.0,
                  Velocity         => 0.0,
                  Fuel             => 0.0,
                  Time_Stamp       => 0.0,
                  Terminated       => False);
   end Current_State;
   --  To ensure atsk safe access, the actual shared sensor data is wrapped in
   --  this protected object.

end Shared_Sensor_Data;
