package body Task_Safe_Store is

   protected Protected_Store is
      procedure Set_Value (New_Value : Stored_Type);
      function Get_Value return Stored_Type;
   private
      Value : Stored_Type := Initial_Value;
   end Protected_Store;

   protected body Protected_Store is
      function Get_Value return Stored_Type is
      begin
         return Value;
      end Get_Value;

      procedure Set_Value (New_Value : Stored_Type) is
      begin
         Value := New_Value;
      end Set_Value;
   end Protected_Store;

   function Get return Stored_Type is
   begin
      return Protected_Store.Get_Value;
   end Get;

   procedure Set (New_Value : Stored_Type) is
   begin
      Protected_Store.Set_Value (New_Value => New_Value);
   end Set;

end Task_Safe_Store;
