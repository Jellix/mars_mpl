package Landing_Legs is

   type Legs_Index is range 1 .. 3;

   IO_Error : exception;

   subtype Sensor_Glitch is Integer range 1 .. 37; -- Number of milliseconds for sensor glitch.

   function Read_State (Index : in Legs_Index) return Boolean;

end Landing_Legs;
