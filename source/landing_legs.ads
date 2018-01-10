-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

package Landing_Legs is

   type Legs_Index is (One, Two, Three);
   type Leg_State  is (In_Flight, Touched_Down);
   type All_Legs_State is array (Legs_Index) of Leg_State;

   IO_Error : exception;

   procedure Deploy;
   procedure Touchdown;
   procedure Read_State (Index : in     Legs_Index;
                         State :    out Leg_State);
   procedure Read_State (State : out All_Legs_State);
   procedure Shutdown;

   protected type Leg_Iterator is
      entry Next (The_Leg : out Legs_Index);
   private
      Current_Leg    : Legs_Index := Legs_Index'First;
      Legs_Available : Boolean    := True;
   end Leg_Iterator;

end Landing_Legs;
