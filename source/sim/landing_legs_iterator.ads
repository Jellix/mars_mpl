with Shared_Types;

package Landing_Legs_Iterator with
  Pure => True
is

   protected type Leg_Iterator is
      entry Next (The_Leg : out Shared_Types.Legs_Index);
      --  Provides the next available leg.
      --
      --  Please note that this subroutine blocks if there is no free landing
      --  leg index available anymore.
      --
      --  @param The_Leg The index of the next available leg.
   private
      Current_Leg    : Shared_Types.Legs_Index := Shared_Types.Legs_Index'First;
      Legs_Available : Boolean                 := True;
   end Leg_Iterator;
   --  Provides a task safe way to assign a certain leg to a task without the
   --  need to use discriminants.

end Landing_Legs_Iterator;
