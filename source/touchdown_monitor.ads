with Ada.Real_Time;
with Landing_Legs;

package Touchdown_Monitor with SPARK_Mode is

   Cycle : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);
   -- Monitoring cycle

   type Health_State is (Unknown, Good, Bad);
   type Run_State    is (Not_Started, Started);

   procedure Start;
   procedure Enable;
   procedure Shutdown;
   function Current_State (Leg : Landing_Legs.Legs_Index) return Run_State;

end Touchdown_Monitor;
