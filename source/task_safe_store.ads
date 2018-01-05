generic
   type Stored_Type is private;
   Initial_Value : in Stored_Type;

package Task_Safe_Store is

   procedure Set (New_Value : Stored_Type);

   function Get return Stored_Type;

end Task_Safe_Store;
