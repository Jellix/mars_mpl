package body Task_Safe_Store is

   protected body Shelf is

      function Get return Stored_Type is
      begin
         return Value;
      end Get;

      procedure Set (New_Value : Stored_Type) is
      begin
         Value := New_Value;
      end Set;

   end Shelf;

end Task_Safe_Store;
