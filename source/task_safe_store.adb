package body Task_Safe_Store is

   function Default_Value return Stored_Type is
     (Initial_Value);

   protected body Shelf is

      procedure Add (X : in Stored_Type) is
      begin
         Value := Value + X;
      end Add;

      function Get return Stored_Type is
      begin
         return Value;
      end Get;

      procedure Set (New_Value : in Stored_Type) is
      begin
         Value := New_Value;
      end Set;

   end Shelf;

end Task_Safe_Store;
