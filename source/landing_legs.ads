package Landing_Legs with SPARK_Mode is

   type Legs_Index is (North, East, South, West);
   type Leg_State  is (In_Flight, Touched_Down);

   IO_Error : exception;

   subtype Sensor_Glitch is Integer range 1 .. 37;
   -- Number of milliseconds for sensor glitch.

   procedure Deploy;
   procedure Touchdown;
   function Read_State (Index : in Legs_Index) return Leg_State;
   procedure Shutdown;

end Landing_Legs;
