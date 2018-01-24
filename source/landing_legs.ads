-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Shared_Types;

package Landing_Legs is

   IO_Error : exception;

   procedure Deploy;
   procedure Touchdown;
   procedure Read_State (Index : in     Shared_Types.Legs_Index;
                         State :    out Shared_Types.Leg_State);
   procedure Read_State (State : out Shared_Types.All_Legs_State);
   procedure Shutdown;

   protected type Leg_Iterator is
      entry Next (The_Leg : out Shared_Types.Legs_Index);
   private
      Current_Leg    : Shared_Types.Legs_Index := Shared_Types.Legs_Index'First;
      Legs_Available : Boolean                 := True;
   end Leg_Iterator;

end Landing_Legs;
