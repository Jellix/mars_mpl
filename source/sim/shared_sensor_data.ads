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
         Altitude         : Shared_Types.Altitude;
         Drag             : Shared_Types.Acceleration;
         Dry_Mass         : Shared_Types.Vehicle_Mass;
         Fuel             : Shared_Types.Fuel_Mass;
         Legs             : Shared_Types.All_Legs_State;
         Temperature      : Shared_Types.Kelvin;
         Thruster_Enabled : Boolean;
         Time_Stamp       : Duration;
         Velocity         : Shared_Types.Velocity;
      end record;
   --  The full space craft sensor state.
   --  @field Altitude         Current altitude of space craft.
   --  @field Drag             Currently experienced acceleration due to drag.
   --  @field Dry_Mass         The current dry mass of the spacecraft.
   --  @field Fuel             Fuel mass left in tank.
   --  @field Legs             State of all landing legs.
   --  #field Temperature      Surface temperature.
   --  @field Thruster_Enabled State of thruster.
   --  @field Time_Stamp       (Relative) time since simulation start. Denotes
   --                          the (rough) sampling time of all of the above.
   --  @field Velocity         Current velocity of space craft.

   pragma Annotate (GNATcheck, Exempt_Off, "Visible_Components");

   package State_Store is new Task_Safe_Store
     (Stored_Type   => State,
      Initial_Value =>
        State'(Altitude         => 0.0,
               Drag             => 0.0,
               Dry_Mass         => Shared_Types.Vehicle_Mass'First,
               Fuel             => 0.0,
               Legs             =>
                 Shared_Types.All_Legs_State'
                   (others => Shared_Types.In_Flight),
               Temperature      => 3.0,
               Thruster_Enabled => False,
               Time_Stamp       => 0.0,
               Velocity         => 0.0));

   pragma Annotate (GNATcheck,
                    Exempt_On,
                    "Global_Variables",
                    "Protected type, outside access is properly restricted.");

   Current_State : State_Store.Shelf;

   pragma Annotate (GNATcheck,
                    Exempt_Off,
                    "Global_Variables");

end Shared_Sensor_Data;
