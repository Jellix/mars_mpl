-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

package Landing_Legs is

   type Legs_Index is (North, East, South, West);
   type Leg_State  is (In_Flight, Touched_Down);
   type All_Legs_State is array (Legs_Index) of Leg_State;

   IO_Error : exception;

   subtype Sensor_Glitch is Integer range 1 .. 37;
   -- Number of milliseconds for sensor glitch.

   procedure Deploy;
   procedure Touchdown;
   procedure Read_State (Index : in     Legs_Index;
                         State :    out Leg_State);
   procedure Read_State (State : out All_Legs_State);
   procedure Shutdown;

end Landing_Legs;
