with Landing_Legs;

package Touchdown_Monitor is

   type Health_State is (Unknown, Good, Bad);
   type Run_State    is (Not_Started, Started);

   procedure Start;
   procedure Enable;
   function Current_State (Leg : Landing_Legs.Legs_Index) return Run_State;

end Touchdown_Monitor;
