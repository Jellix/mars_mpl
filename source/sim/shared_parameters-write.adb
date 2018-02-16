package body Shared_Parameters.Write is

   procedure Fuel_Flow_Rate (Value : in Shared_Types.Fuel_Mass) is
   begin
      Shared_Fuel_Flow_Rate := Value;
   end Fuel_Flow_Rate;

   procedure Initial_Altitude (Value : in Shared_Types.Altitude) is
   begin
      Shared_Initial_Altitude := Value;
   end Initial_Altitude;

   procedure Initial_Fuel_Mass (Value : in Shared_Types.Fuel_Mass) is
   begin
      Shared_Initial_Fuel_Mass := Value;
   end Initial_Fuel_Mass;

   procedure Initial_Velocity (Value : in Shared_Types.Velocity) is
   begin
      Shared_Initial_Velocity := Value;
   end Initial_Velocity;

   procedure Thruster_Acceleration (Value : in Shared_Types.Acceleration) is
   begin
      Shared_Thruster_Acceleration := Value;
   end Thruster_Acceleration;

end Shared_Parameters.Write;
