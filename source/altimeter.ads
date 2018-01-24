-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Planets.Parameters;
with Shared_Types;

package Altimeter is

   -- Sample cycle of simulated height sensor.
   Cycle         : constant Ada.Real_Time.Time_Span :=
                     Ada.Real_Time.Milliseconds (10);

   Gravity                 : constant Float
     := Planets.Parameters.Gravity (Of_Planet => Planets.Mars); -- m/s**2
   Initial_Velocity        : constant Shared_Types.Velocity :=    80.000; -- m/s
   Safe_Landing_Velocity   : constant Shared_Types.Velocity :=     2.500; -- m/s
   Initial_Altitude        : constant Shared_Types.Altitude := 3_500.000; -- m

   function Current_Altitude return Shared_Types.Altitude
     with Volatile_Function;

   function Current_Velocity return Shared_Types.Velocity
     with Volatile_Function;

end Altimeter;
