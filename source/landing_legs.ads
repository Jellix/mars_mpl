package Landing_Legs is

   type Legs_Index is range 1 .. 3;
   type Leg_State  is (In_Flight, Touched_Down);

   IO_Error : exception;

   subtype Sensor_Glitch is Integer range 1 .. 37;
   -- Number of milliseconds for sensor glitch.

   function Read_State (Index : in Legs_Index) return Leg_State;
   procedure Shutdown;

end Landing_Legs;
