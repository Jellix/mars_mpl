--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_Language_Manager                 Luebeck            --
--  Interface                                      Summer, 2009       --
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

with Gtk.Source_Language;   use Gtk.Source_Language;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package Gtk.Source_Language_Manager is
--
-- Gtk_Source_Language_Manager_Record -- Language manager
--
   type Gtk_Source_Language_Manager_Record is
      new GObject_Record with private;
   type Gtk_Source_Language_Manager is
      access all Gtk_Source_Language_Manager_Record'Class;
--
-- Get_Default -- Get default language manage
--
-- The returned object is owned and shall not be used with Unref
--
-- Returns :
--
--    The default manager
--
   function Get_Default return Gtk_Source_Language_Manager;
--
-- Get_Language -- Get language by ID
--
--    Manager  - The language manager
--    Language - The language ID
--
-- The returned object is owned and shall not be used in Unref
--
-- Returns :
--
--    The language or null
--
   function Get_Language
            (  Manager  : not null access
                          Gtk_Source_Language_Manager_Record;
               Language : UTF8_String
            )  return Gtk_Source_Language;
--
-- Get_Language_IDs -- Get the ids of the available languages
--
--    Manager - The language manager
--
-- This  function  returns  an  array  containing  a  list  of  language
-- identifiers. The array elements are owned by the language manager and
-- must not be modified.
--
-- Returns :
--
--    Array of ponters to C-strings
--
   function Get_Language_IDs
            (  Manager : not null access
                         Gtk_Source_Language_Manager_Record
            )  return Chars_Ptr_Array;

--
-- Get_Search_Path -- Get current search path
--
--    Manager - The language manager
--
-- This function returns an array containing a list  of  language  files
-- directories. The array elements are owned  by  language  manager  and
-- must not be modified.
--
-- Returns :
--
--    Array of ponters to path items (C-strings)
--
   function Get_Search_Path
            (  Manager : not null access
                         Gtk_Source_Language_Manager_Record
            )  return Chars_Ptr_Array;
--
-- Gtk_New -- New object creation
--
--    Manager - The result
--
   procedure Gtk_New (Manager : out Gtk_Source_Language_Manager);
--
-- Guess_Language -- Guess language by file name and or content
--
--    Manager      - The language manager
--    File_Name    - The file name
--    Content_Type - The content type
--
-- The  returned  object is owned and shall not be used in Unref. Either
-- File_Name or Content_Type can be omitted  in  the  function  versions
-- named Guess_Language_By_File_Name and Guess_Language_By_Content.
--
-- Returns :
--
--    The language or null
--
   function Guess_Language
            (  Manager      : not null access
                              Gtk_Source_Language_Manager_Record;
               File_Name    : UTF8_String;
               Content_Type : UTF8_String
            )  return Gtk_Source_Language;
   function Guess_Language_By_File_Name
            (  Manager      : not null access
                              Gtk_Source_Language_Manager_Record;
               File_Name    : UTF8_String
            )  return Gtk_Source_Language;
   function Guess_Language_By_Content
            (  Manager      : not null access
                              Gtk_Source_Language_Manager_Record;
               Content_Type : UTF8_String
            )  return Gtk_Source_Language;
--
-- Initialize -- To be called by any derived object
--
--    Manager - The language manager
--
   procedure Initialize
             (  Manager : not null access
                          Gtk_Source_Language_Manager_Record'Class
             );
--
-- Set_Search_Path
--
--    Manager - The language manager
--  [ Dirs ]  - Array of pointers to C-strings of path items
--
-- When Dirs is absent the search path is set to default. At the  moment
-- this function can be called only before the language files are loaded
-- for the first time. In practice to set a custom  search  path  for  a
-- Gtk_Source_Language_Manager, you have to  call  this  function  right
-- after creating it.
--
   procedure Set_Search_Path
             (  Manager : not null access
                          Gtk_Source_Language_Manager_Record;
                Dirs    : Chars_Ptr_Array
             );
   procedure Set_Search_Path
             (  Manager : not null access
                          Gtk_Source_Language_Manager_Record
             );
private
   type Gtk_Source_Language_Manager_Record is
      new GObject_Record with null record;

end Gtk.Source_Language_Manager;
