--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_Language_Manager                 Luebeck            --
--  Implementation                                 Summer, 2009       --
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

with GLib.Chars_Ptr_Vectors;  use GLib.Chars_Ptr_Vectors;
with Interfaces.C;            use Interfaces.C;
with System;                  use System;

with Ada.Unchecked_Deallocation;

package body Gtk.Source_Language_Manager is

   function Get_Default return Gtk_Source_Language_Manager is
      function Internal return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_get_default"
             );
      Stub : Gtk_Source_Language_Manager_Record;
   begin
      return
         Gtk_Source_Language_Manager
         (  Get_User_Data_Fast (Internal, Stub)
         );
   end Get_Default;

   function Get_Language
            (  Manager  : not null access
                          Gtk_Source_Language_Manager_Record;
               Language : UTF8_String
            )  return Gtk_Source_Language is
      function Internal (Manager : Address; ID : Char_Array)
         return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_get_language"
             );
      Stub : Gtk_Source_Language_Record;
   begin
      return
         Gtk_Source_Language
         (  Get_User_Data_Fast
            (  Internal (Get_Object (Manager), To_C (Language)),
               Stub
         )  );
   end Get_Language;

   function Get_Language_IDs
            (  Manager : not null access
                         Gtk_Source_Language_Manager_Record
            )  return Chars_Ptr_Array is
      function Internal (Manager : Address) return Chars_Ptr_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_get_language_ids"
             );
   begin
      return Convert (Internal (Get_Object (Manager)));
   end Get_Language_IDs;

   function Get_Search_Path
            (  Manager : not null access
                         Gtk_Source_Language_Manager_Record
            )  return Chars_Ptr_Array is
      function Internal (Manager : Address) return Chars_Ptr_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_get_search_path"
             );
   begin
      return Convert (Internal (Get_Object (Manager)));
   end Get_Search_Path;

   procedure Gtk_New (Manager : out Gtk_Source_Language_Manager) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Gtk_Source_Language_Manager_Record'Class,
                Gtk_Source_Language_Manager
             );
   begin
      Manager := new Gtk_Source_Language_Manager_Record;
      Gtk.Source_Language_Manager.Initialize (Manager);
   exception
      when others =>
         Free (Manager);
         raise;
   end Gtk_New;

   function Guess_Language
            (  Manager      : not null access
                              Gtk_Source_Language_Manager_Record;
               File_Name    : UTF8_String;
               Content_Type : UTF8_String
            )  return Gtk_Source_Language is
      function Internal
               (  Manager      : Address;
                  File_Name    : Char_Array;
                  Content_Type : Char_Array
               )  return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_guess_language"
             );
      Stub : Gtk_Source_Language_Record;
   begin
      return
         Gtk_Source_Language
         (  Get_User_Data_Fast
            (  Internal
               (  Get_Object (Manager),
                  To_C (File_Name),
                  To_C (Content_Type)
               ),
               Stub
         )  );
   end Guess_Language;

   function Guess_Language_By_File_Name
            (  Manager   : not null access
                           Gtk_Source_Language_Manager_Record;
               File_Name : UTF8_String
            )  return Gtk_Source_Language is
      function Internal
               (  Manager      : Address;
                  File_Name    : Char_Array;
                  Content_Type : Address := Null_Address
               )  return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_guess_language"
             );
      Stub : Gtk_Source_Language_Record;
   begin
      return
         Gtk_Source_Language
         (  Get_User_Data_Fast
            (  Internal (Get_Object (Manager), To_C (File_Name)),
               Stub
         )  );
   end Guess_Language_By_File_Name;

   function Guess_Language_By_Content
            (  Manager      : not null access
                              Gtk_Source_Language_Manager_Record;
               Content_Type : UTF8_String
            )  return Gtk_Source_Language is
      function Internal
               (  Manager      : Address;
                  File_Name    : Address;
                  Content_Type : Char_Array
               )  return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_guess_language"
             );
      Stub : Gtk_Source_Language_Record;
   begin
      return
         Gtk_Source_Language
         (  Get_User_Data_Fast
            (  Internal
               (  Get_Object (Manager),
                  Null_Address,
                  To_C (Content_Type)
               ),
               Stub
         )  );
   end Guess_Language_By_Content;

   procedure Initialize
             (  Manager : not null access
                          Gtk_Source_Language_Manager_Record'Class
             )  is
      function Internal return Address;
      pragma Import (C, Internal, "gtk_source_language_manager_new");
   begin
     Set_Object (Manager, Internal);
   end Initialize;

   procedure Set_Search_Path
             (  Manager : not null access
                          Gtk_Source_Language_Manager_Record;
                Dirs    : Chars_Ptr_Array
             )  is
      procedure Internal (Manager : Address; Dirs : Chars_Ptr_Array);
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_set_search_path"
             );
      List : Chars_Ptr_Array (Dirs'First..Dirs'Last + 1);
   begin
      List (Dirs'First..Dirs'Last) := Dirs;
      List (List'Last) := Null_Ptr;  -- NUL at the end of Dirs
      Internal (Get_Object (Manager), List);
   end Set_Search_Path;

   procedure Set_Search_Path
             (  Manager : not null access
                          Gtk_Source_Language_Manager_Record
             )  is
      procedure Internal
                (  Manager : Address;
                   Dirs    : Address := Null_Address
                );
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_manager_set_search_path"
             );
   begin
      Internal (Get_Object (Manager));
   end Set_Search_Path;

end Gtk.Source_Language_Manager;
