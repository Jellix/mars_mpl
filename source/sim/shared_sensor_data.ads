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
         Attitude            : Shared_Types.Degree;
         Altitude            : Shared_Types.Meter;
         Core_Temperature    : Shared_Types.Kelvin;
         Drag                : Shared_Types.Meter_Per_Square_Second;
         Dry_Mass            : Shared_Types.Vehicle_Mass;
         Fuel                : Shared_Types.Fuel_Mass;
         Legs                : Shared_Types.All_Legs_State;
         Surface_Temperature : Shared_Types.Kelvin;
         Thruster_Enabled    : Boolean;
         Time_Stamp          : Duration;
         Velocity            : Shared_Types.Meter_Per_Second;
      end record;
   --  The full space craft sensor state.
   --  @field Attitude            Orientation of space craft relative to ground.
   --  @field Altitude            Current altitude of space craft.
   --  @field Core_Temperature    Core (inner hull) temperature.
   --  @field Drag                Currently experienced acceleration due to
   --                             drag.
   --  @field Dry_Mass            The current dry mass of the spacecraft.
   --  @field Fuel                Fuel mass left in tank.
   --  @field Legs                State of all landing legs.
   --  #field Surface_Temperature Surface (outer hull) temperature.
   --  @field Thruster_Enabled    State of thruster.
   --  @field Time_Stamp          (Relative) time since simulation start.
   --                             Denotes the (rough) sampling time of all other
   --                             fields.
   --  @field Velocity            Current velocity of space craft. Direction is
   --                             denoted by the Attitude field.

   pragma Annotate (GNATcheck, Exempt_Off, "Visible_Components");

   pragma Warnings (Off, "formal parameter """);
   function "+" (L, R : in State) return State is
     (raise Program_Error);

   function "-" (L, R : in State) return State is
     (raise Program_Error);
   pragma Warnings (On, "formal parameter """);

   package State_Store is new Task_Safe_Store
     (Stored_Type   => State,
      Initial_Value =>
         State'(Attitude            => 0.0,
                Altitude            => 0.0,
                Core_Temperature    => 0.0,
                Drag                => 0.0,
                Dry_Mass            => Shared_Types.Vehicle_Mass'First,
                Fuel                => 0.0,
                Legs                =>
                   Shared_Types.All_Legs_State'
                  (others => Shared_Types.In_Flight),
                Surface_Temperature => 0.0,
                Thruster_Enabled    => False,
                Time_Stamp          => 0.0,
                Velocity            => 0.0));

   pragma Annotate (GNATcheck,
                    Exempt_On,
                    "Global_Variables",
                    "Protected type, outside access is properly restricted.");

   Current_State : State_Store.Shelf;

   pragma Annotate (GNATcheck,
                    Exempt_Off,
                    "Global_Variables");

end Shared_Sensor_Data;
