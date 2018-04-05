with Shared_Types;
with Task_Safe_Store;

--  @summary
--  Provides the shared data from the sensors (i.e. monitoring and control
--  tasks).
--
--  @description
--  Shared_Passive package to provide sensor data across all partitions.
package Shared_Sensor_Data with
  Shared_Passive => True
is

   pragma Annotate (GNATcheck,
                    Exempt_On,
                    "Visible_Components",
                    "Intentionally visible components for easier access.");

   type State is
      record
         Legs             : Shared_Types.All_Legs_State;
         Thruster_Enabled : Boolean;
         Altitude         : Shared_Types.Altitude;
         Velocity         : Shared_Types.Velocity;
         Fuel             : Shared_Types.Fuel_Mass;
         Time_Stamp       : Duration;
      end record;
   --  The full space craft sensor state.
   --  @field Legs             State of all landing legs.
   --  @field Thruster_Enabled State of thruster.
   --  @field Altitude         Current altitude of space craft.
   --  @field Velocity         Current velocity of space craft.
   --  @field Fuel             Fuel mass left in tank.
   --  @field Time_Stamp       (Relative) time since simulation start. Denotes
   --                          the (rough) sampling time of all of the above.
   --  @field Terminated       Indicates that the simulation has been finished
   --                          and no more updates will be incoming.

   pragma Annotate (GNATcheck, Exempt_Off, "Visible_Components");

   package State_Store is new Task_Safe_Store
     (Stored_Type   => State,
      Initial_Value => State'(Legs             =>
                                Shared_Types.All_Legs_State'
                                  (others => Shared_Types.In_Flight),
                              Thruster_Enabled => False,
                              Altitude         => 0.0,
                              Velocity         => 0.0,
                              Fuel             => 0.0,
                              Time_Stamp       => 0.0));

   Current_State : State_Store.Shelf;

end Shared_Sensor_Data;
