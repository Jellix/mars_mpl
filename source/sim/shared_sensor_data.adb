package body Shared_Sensor_Data is

   protected body Current_State is

      function Get return State is
      begin
         return The_Blob;
      end Get;

      procedure Set (Data : in State) is
      begin
         The_Blob := Data;
      end Set;

   end Current_State;

end Shared_Sensor_Data;
