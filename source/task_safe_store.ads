--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

generic
   type Stored_Type is private;
   --  The type of value to be stored.

   with function "+" (Left  : in Stored_Type;
                      Right : in Stored_Type) return Stored_Type is <>;
   with function "-" (Left  : in Stored_Type;
                      Right : in Stored_Type) return Stored_Type is <>;

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

      procedure Add (X : in Stored_Type);
      --  Adds X to the currently stored value.
      --  @param X The value to be added.

      procedure Subtract (X : in Stored_Type);
      --  Subtracts X from the currently stored value.
      --  @param X The value to be subtracted.

   private

      Value : Stored_Type := Initial_Value;
      --  The value stored internally.

   end Shelf;
   --  The actual protected type storing the value.

end Task_Safe_Store;
