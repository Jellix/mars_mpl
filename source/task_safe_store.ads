--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

generic
   type Stored_Type is private;
   --  The type of value to be stored.

   Initial_Value : in Stored_Type;
   --  The initial value.

   --  @summary
   --  Provides a generic for storing and retrieving values in a task safe
   --  manner.
   --
   --  @description
   --  Provides a task safe protected type to store and retrieve values of a
   --  given type.
package Task_Safe_Store is
   pragma Pure;

   function Default_Value return Stored_Type
     with Inline => True;
   --  @return The Initial_Value given in instantation.

   protected type Shelf is

      procedure Set (New_Value : in Stored_Type);
      --  Sets a new value.
      --  @param New_Value The new value to be set.

      function Get return Stored_Type;
      --  Retrieves the last set value.
      --  @return The stored value.

   private

      Value : Stored_Type := Initial_Value;
      --  The value stored internally.

   end Shelf;
   --  The actual protected type storing the value.

end Task_Safe_Store;
