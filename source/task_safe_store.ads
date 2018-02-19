--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);
generic
   type Stored_Type is private;
   Initial_Value : in Stored_Type;
package Task_Safe_Store is
   pragma Pure;

   protected type Shelf is
      procedure Set (New_Value : Stored_Type);
      function Get return Stored_Type;
   private
      Value : Stored_Type := Initial_Value;
   end Shelf;

end Task_Safe_Store;
