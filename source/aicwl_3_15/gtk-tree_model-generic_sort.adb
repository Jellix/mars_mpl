--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Generic_Sort                 Luebeck            --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Unchecked_Conversion;

package body Gtk.Tree_Model.Generic_Sort is

   procedure Ref (Object : System.Address);
   pragma Import (C, Ref, "gtk_object_ref");
   procedure Unref (Object : System.Address);
   pragma Import (C, Unref, "gtk_object_unref");

   function C_Sort
            (  Model     : System.Address;
               A         : access Gtk_Tree_Iter;
               B         : access Gtk_Tree_Iter;
               User_Data : System.Address
            )  return GInt;
   pragma Convention (C, C_Sort);

   procedure Clear_Cache
             (  Store : not null access Gtk_Tree_Model_Sort_Record
             )  is
      procedure Internal (Model : System.Address);
      pragma Import (C, Internal, "gtk_tree_model_sort_clear_cache");
   begin
      Internal (Get_Object (Store));
   end Clear_Cache;

   function Compare
            (  Store  : not null access Gtk_Tree_Model_Sort_Record;
               Left   : Gtk_Tree_Iter;
               Right  : Gtk_Tree_Iter
            )  return Gtk.Missed.Row_Order is
   begin
      return Gtk.Missed.Equal;
   end Compare;

   function Convert_Child_Iter_To_Iter
            (  Store : not null access Gtk_Tree_Model_Sort_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
      procedure Internal
                (  Model       : System.Address;
                   Sorted_Iter : access Gtk_Tree_Iter;
                   Child_Iter  : access Gtk_Tree_Iter
                );
      pragma Import
             (  C,
                Internal,
                "gtk_tree_model_sort_convert_child_iter_to_iter"
             );
   begin
      if Iter = Null_Iter then
         return Null_Iter;
      end if;
      declare
         Child_Iter  : aliased Gtk_Tree_Iter := Iter;
         Sorted_Iter : aliased Gtk_Tree_Iter;
      begin
         Internal
         (  Get_Object (Store),
            Sorted_Iter'Access,
            Child_Iter'Access
         );
         return Sorted_Iter;
      end;
   end Convert_Child_Iter_To_Iter;

   procedure Convert_Child_Path_To_Path
             (  Store : not null access Gtk_Tree_Model_Sort_Record;
                Path  : Gtk_Tree_Path
             )  is
      procedure Internal
                (  Model : System.Address;
                   Path  : System.Address
                );
      pragma Import
             (  C,
                Internal,
                "gtk_tree_model_sort_convert_child_path_to_path"
             );
   begin
      Internal (Get_Object (Store), Get_Object (Path));
   end Convert_Child_Path_To_Path;

   function Convert_Iter_To_Child_Iter
            (  Store : not null access Gtk_Tree_Model_Sort_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
      procedure Internal
                (  Model       : System.Address;
                   Child_Iter  : access Gtk_Tree_Iter;
                   Sorted_Iter : access Gtk_Tree_Iter
                );
      pragma Import
             (  C,
                Internal,
                "gtk_tree_model_sort_convert_iter_to_child_iter"
             );
   begin
      if Iter = Null_Iter then
         return Null_Iter;
      end if;
      declare
         Child_Iter  : aliased Gtk_Tree_Iter;
         Sorted_Iter : aliased Gtk_Tree_Iter := Iter;
      begin
         Internal
         (  Get_Object (Store),
            Child_Iter'Access,
            Sorted_Iter'Access
         );
         return Child_Iter;
      end;
   end Convert_Iter_To_Child_Iter;

   procedure Convert_Path_To_Child_Path
             (  Store : not null access Gtk_Tree_Model_Sort_Record;
                Path  : Gtk_Tree_Path
             )  is
      procedure Internal
                (  Model : System.Address;
                   Path  : System.Address
                );
      pragma Import
             (  C,
                Internal,
                "gtk_tree_model_sort_convert_path_to_child_path"
             );
   begin
      Internal (Get_Object (Store), Get_Object (Path));
   end Convert_Path_To_Child_Path;

   function Get_Model
            (  Store : not null access Gtk_Tree_Model_Sort_Record
            )  return Tree_Model is
   begin
      return Store.Model;
   end Get_Model;

   procedure Gtk_New
             (  Store : out Gtk_Tree_Model_Sort;
                Model : not null access Tree_Model_Record'Class
             )  is
   begin
      Store := new Gtk_Tree_Model_Sort_Record;
      Initialize (Store, Model);
   exception
      when others =>
         Store.Unref;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Store : not null access
                        Gtk_Tree_Model_Sort_Record'Class;
                Model : not null access Tree_Model_Record'Class
             )  is
      function Internal (Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tree_model_sort_new_with_model");
   begin
      Set_Object (Store, Internal (Get_Object (Model)));
      Store.Model := Model.all'Access;
   end Initialize;

   function Iter_Is_Valid
            (  Store : not null access Gtk_Tree_Model_Sort_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
      function Internal
               (  Model : System.Address;
                  Iter  : access Gtk_Tree_Iter
               )  return GBoolean;
      pragma Import (C, Internal, "gtk_tree_model_sort_iter_is_valid");
   begin
      if Iter = Null_Iter then
         return False;
      end if;
      declare
         Local_Iter : aliased Gtk_Tree_Iter := Iter;
      begin
         if Internal (Get_Object (Store), Local_Iter'Access) = 0 then
            return False;
         else
            return True;
         end if;
      end;
   end Iter_Is_Valid;

   function C_Sort
            (  Model     : System.Address;
               A         : access Gtk_Tree_Iter;
               B         : access Gtk_Tree_Iter;
               User_Data : System.Address
            )  return GInt is
      function To_Store is
         new Ada.Unchecked_Conversion
             (  System.Address,
                Gtk_Tree_Model_Sort
             );
      Store : constant Gtk_Tree_Model_Sort := To_Store (User_Data);
   begin
      case Compare (Store, A.all, B.all) is
         when Gtk.Missed.Before => return -1;
         when Gtk.Missed.Equal  => return  0;
         when Gtk.Missed.After  => return  1;
      end case;
   end C_Sort;

   procedure Get_Sort_Column_ID
             (  Store  : not null access Gtk_Tree_Model_Sort_Record;
                Column : out GInt;
                Order  : out Gtk_Sort_Type
             )  is
      function Internal
               (  Model  : System.Address;
                  Column : access GInt;
                  Order  : access Gtk_Sort_Type
               )  return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_tree_sortable_get_sort_column_id"
             );
      Result       : GBoolean;
      Local_Column : aliased GInt;
      Local_Order  : aliased Gtk_Sort_Type;
   begin
      Result :=
         Internal
         (  Get_Object (Store),
            Local_Column'Access,
            Local_Order'Access
         );
      Column := Local_Column;
      Order  := Local_Order;
   end Get_Sort_Column_ID;

   function Has_Default_Sort_Func
            (  Store : not null access Gtk_Tree_Model_Sort_Record
            )  return Boolean is
      function Internal (Model : System.Address) return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_tree_sortable_has_default_sort_func"
             );
   begin
      return 0 /= Internal (Get_Object (Store));
   end Has_Default_Sort_Func;

   procedure Set_Sort_Column_ID
             (  Store  : not null access Gtk_Tree_Model_Sort_Record;
                Column : GInt;
                Order  : Gtk_Sort_Type
             )  is
      procedure Internal
                (  Model  : System.Address;
                   Column : GInt;
                   Order  : Gtk_Sort_Type
                );
      pragma Import
             (  C,
                Internal,
                "gtk_tree_sortable_set_sort_column_id"
             );
   begin
      Internal (Get_Object (Store), Column, Order);
   end Set_Sort_Column_ID;

   procedure Set_Sort_Func
             (  Store  : not null access
                         Gtk_Tree_Model_Sort_Record'Class;
                Column : GInt
             )  is
      procedure Internal
                (  Model     : System.Address;
                   Column    : GInt;
                   Sort_Func : System.Address;
                   User_Data : System.Address;
                   Destroy   : System.Address
                );
      pragma Import (C, Internal, "gtk_tree_sortable_set_sort_func");
   begin
      Internal
      (  Get_Object (Store),
         Column,
         C_Sort'Address,
         Store.all'Address,
         System.Null_Address
      );
   end Set_Sort_Func;

   procedure Set_Sort_Func
             (  Store : not null access Gtk_Tree_Model_Sort_Record'Class
             )  is
      procedure Internal
                (  Model     : System.Address;
                   Sort_Func : System.Address;
                   User_Data : System.Address;
                   Destroy   : System.Address
                );
      pragma Import
             (  C,
                Internal,
                "gtk_tree_sortable_set_default_sort_func"
             );
   begin
      Internal
      (  Get_Object (Store),
         C_Sort'Address,
         Store.all'Address,
         System.Null_Address
      );
   end Set_Sort_Func;

   procedure Sort_Column_Changed
             (  Store : not null access Gtk_Tree_Model_Sort_Record'Class
             )  is
      procedure Internal (Model : System.Address);
      pragma Import
             (  C,
                Internal,
                "gtk_tree_sortable_sort_column_changed"
             );
   begin
      Internal (Get_Object (Store));
   end Sort_Column_Changed;

end Gtk.Tree_Model.Generic_Sort;
