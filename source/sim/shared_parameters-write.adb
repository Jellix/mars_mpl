package body Shared_Parameters.Write is

   procedure Dry_Mass (Value : in Shared_Types.Fuel_Mass) is
   begin
      Shared_Dry_Mass := Value;
   end Dry_Mass;

   procedure Exhaust_Velocity (Value : in Shared_Types.Velocity) is
   begin
      Shared_Exhaust_Velocity := Value;
   end Exhaust_Velocity;

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

   procedure TDM_Bug_Enabled (Value : in Boolean) is
   begin
      Shared_Bug_Enabled := Value;
   end TDM_Bug_Enabled;

end Shared_Parameters.Write;
