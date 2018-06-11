package body Landing_Legs_Iterator is

   use type Shared_Types.Legs_Index;

   protected body Leg_Iterator is

      entry Next (The_Leg : out Shared_Types.Legs_Index) when Legs_Available is
      begin
         The_Leg := Current_Leg;

         if Current_Leg = Shared_Types.Legs_Index'Last then
            Legs_Available := False;
         else
            Current_Leg := Shared_Types.Legs_Index'Succ (Current_Leg);
         end if;
      end Next;

   end Leg_Iterator;

end Landing_Legs_Iterator;
