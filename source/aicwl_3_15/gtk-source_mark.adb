--                                                                    --
--  package Gtk.Source_Mark         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2009       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Interfaces.C;          use Interfaces.C;

with Ada.Unchecked_Deallocation;

package body Gtk.Source_Mark is

   function Convert (Mark : Address) return Gtk_Source_Mark is
      Stub : Gtk_Source_Mark_Record;
   begin
      return Gtk_Source_Mark (Get_User_Data (Mark, Stub));
   end Convert;

   function Convert (Mark : Gtk_Source_Mark) return Address is
   begin
      if Mark = null then
         return Null_Address;
      else
         return Get_Object (Mark);
      end if;
   end Convert;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Gtk_Source_Mark_Record'Class,
             Gtk_Source_Mark
          );

   function Get_Category
            (  Mark : not null access Gtk_Source_Mark_Record
            )  return UTF8_String is
      function Internal (Object : Address) return Chars_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_get_category"
             );
      Result : constant Chars_Ptr := Internal (Get_Object (Mark));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Category;

   procedure Gtk_New
             (  Mark     : out Gtk_Source_Mark;
                Name     : UTF8_String;
                Category : UTF8_String
             )  is
   begin
      Mark := new Gtk_Source_Mark_Record;
      Initialize (Mark, Name, Category);
   exception
      when others =>
         Free (Mark);
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Mark     : out Gtk_Source_Mark;
                Category : UTF8_String
             )  is
   begin
      Mark := new Gtk_Source_Mark_Record;
      Initialize (Mark, Category);
   exception
      when others =>
         Free (Mark);
         raise;
   end Gtk_New;

   procedure Initialize
             (  Mark     : not null access Gtk_Source_Mark_Record'Class;
                Name     : UTF8_String;
                Category : UTF8_String
             )  is
      function Internal (Name, Category : Char_Array) return Address;
      pragma Import (C, Internal, "gtk_source_mark_new");
   begin
     Set_Object (Mark, Internal (To_C (Name), To_C (Category)));
   end Initialize;

   procedure Initialize
             (  Mark     : not null access Gtk_Source_Mark_Record'Class;
                Category : UTF8_String
             )  is
      function Internal (Name : Address; Category : Char_Array)
         return Address;
      pragma Import (C, Internal, "gtk_source_mark_new");
   begin
     Set_Object (Mark, Internal (Null_Address, To_C (Category)));
   end Initialize;

   function Next
            (  Mark     : not null access Gtk_Source_Mark_Record;
               Category : UTF8_String
            )  return Gtk_Source_Mark is
      function Internal (Mark : Address; Category : Char_Array)
         return Address;
      pragma Import (C, Internal, "gtk_source_mark_next");
      Stub : Gtk_Source_Mark_Record;
   begin
      return
         Gtk_Source_Mark
         (  Get_User_Data_Fast
            (  Internal
               (  Get_Object (Mark),
                  To_C (Category)
               ),
               Stub
         )  );
   end Next;

   function Next
            (  Mark : not null access Gtk_Source_Mark_Record
            )  return Gtk_Source_Mark is
      function Internal
               (  Mark     : Address;
                  Category : Address := Null_Address
               )  return Address;
      pragma Import (C, Internal, "gtk_source_mark_next");
      Stub : Gtk_Source_Mark_Record;
   begin
      return
         Gtk_Source_Mark
         (  Get_User_Data_Fast (Internal (Get_Object (Mark)), Stub)
         );
   end Next;

   function Prev
            (  Mark     : not null access Gtk_Source_Mark_Record;
               Category : UTF8_String
            )  return Gtk_Source_Mark is
      function Internal (Mark : Address; Category : Char_Array)
         return Address;
      pragma Import (C, Internal, "gtk_source_mark_prev");
      Stub : Gtk_Source_Mark_Record;
   begin
      return
         Gtk_Source_Mark
         (  Get_User_Data_Fast
            (  Internal
               (  Get_Object (Mark),
                  To_C (Category)
               ),
               Stub
         )  );
   end Prev;

   function Prev
            (  Mark : not null access Gtk_Source_Mark_Record
            )  return Gtk_Source_Mark is
      function Internal
               (  Mark     : Address;
                  Category : Address := Null_Address
               )  return Address;
      pragma Import (C, Internal, "gtk_source_mark_prev");
      Stub : Gtk_Source_Mark_Record;
   begin
      return
         Gtk_Source_Mark
         (  Get_User_Data_Fast (Internal (Get_Object (Mark)), Stub)
         );
   end Prev;

end Gtk.Source_Mark;
