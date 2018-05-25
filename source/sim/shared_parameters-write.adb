package body Shared_Parameters.Write is

   procedure Dry_Mass (Value : in Shared_Types.Vehicle_Mass) is
   begin
      Shared_Dry_Mass.Set (New_Value => Value);
   end Dry_Mass;

   procedure Exhaust_Velocity (Value : in Shared_Types.Meter_Per_Second) is
   begin
      Shared_Exhaust_Velocity.Set (New_Value => Value);
   end Exhaust_Velocity;

   procedure Fuel_Flow_Rate (Value : in Shared_Types.Kilogram_Per_Second) is
   begin
      Shared_Fuel_Flow_Rate.Set (New_Value => Value);
   end Fuel_Flow_Rate;

   procedure Initial_Altitude (Value : in Shared_Types.Meter) is
   begin
      Shared_Initial_Altitude.Set (New_Value => Value);
   end Initial_Altitude;

   procedure Initial_Attitude (Value : in Shared_Types.Degree) is
   begin
      Shared_Initial_Attitude.Set (New_Value => Value);
   end Initial_Attitude;

   procedure Initial_Fuel_Mass (Value : in Shared_Types.Fuel_Mass) is
   begin
      Shared_Initial_Fuel_Mass.Set (New_Value => Value);
   end Initial_Fuel_Mass;

   procedure Initial_Velocity (Value : in Shared_Types.Meter_Per_Second) is
   begin
      Shared_Initial_Velocity.Set (New_Value => Value);
   end Initial_Velocity;

   procedure Shortest_On_Time (Value : in Shared_Types.On_Time) is
   begin
      Shared_Shortest_On_Time.Set (New_Value => Value);
   end Shortest_On_Time;

   procedure TDM_Bug_Enabled (Value : in Boolean) is
   begin
      Shared_Bug_Enabled.Set (New_Value => Value);
   end TDM_Bug_Enabled;

end Shared_Parameters.Write;
