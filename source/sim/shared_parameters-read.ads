package Shared_Parameters.Read is

   function Fuel_Flow_Rate return Shared_Types.Fuel_Mass
     with Volatile_Function;

   function Initial_Altitude return Shared_Types.Altitude
     with Volatile_Function;

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass
     with Volatile_Function;

   function Initial_Velocity return Shared_Types.Velocity
     with Volatile_Function;

   function Safe_Landing_Velocity return Shared_Types.Velocity
     with Volatile_Function;

   function Target_Landing_Velocity return Shared_Types.Velocity
     with Volatile_Function;

   function Thruster_Acceleration return Shared_Types.Acceleration
     with Volatile_Function;

private

   function Initial_Altitude return Shared_Types.Altitude is
     (Shared_Initial_Altitude);

   function Initial_Velocity return Shared_Types.Velocity is
     (Shared_Initial_Velocity);

   function Initial_Fuel_Mass return Shared_Types.Fuel_Mass is
     (Shared_Initial_Fuel_Mass);

   function Fuel_Flow_Rate return Shared_Types.Fuel_Mass is
     (Shared_Fuel_Flow_Rate);

   function Safe_Landing_Velocity return Shared_Types.Velocity is
     (Shared_Safe_Landing_Velocity);

   function Target_Landing_Velocity return Shared_Types.Velocity is
     (Shared_Target_Landing_Velocity);

   function Thruster_Acceleration return Shared_Types.Acceleration is
     (Shared_Thruster_Acceleration);

end Shared_Parameters.Read;
