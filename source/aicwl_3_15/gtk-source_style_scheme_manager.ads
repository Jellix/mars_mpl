--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_Style_Scheme_Manager             Luebeck            --
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

with Gtk.Source_Style_Scheme;  use Gtk.Source_Style_Scheme;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

package Gtk.Source_Style_Scheme_Manager is
--
-- Gtk_Source_Style_Scheme_Manager_Record -- Manager of style schemes
--
   type Gtk_Source_Style_Scheme_Manager_Record is
      new GObject_Record with private;
   type Gtk_Source_Style_Scheme_Manager is
      access all Gtk_Source_Style_Scheme_Manager_Record'Class;
--
-- Get_Default -- Get default manager
--
-- This  function  does not create a new object, so its result shall not
-- be used with Unref
--
-- Returns :
--
--    The default manager
--
   function Get_Default return Gtk_Source_Style_Scheme_Manager;
--
-- Append_Search_Path -- Append an item to the path
--
--    Manager - The manager
--    Path    - A directory or a filename
--
-- This procedure appends path to the  list  of  directories  where  the
-- manager looks for style scheme files
--
   procedure Append_Search_Path
             (  Manager : not null access
                          Gtk_Source_Style_Scheme_Manager_Record;
                Path    : UTF8_String
             );
--
-- Force_Rescan -- Erase the cache
--
--    Manager - The manager
--
-- Mark  any  currently  cached  information  about  the available style
-- schemes as invalid. All the available style schemes will be  reloaded
-- next time the manager is accessed.
--
   procedure Force_Rescan
             (  Manager : not null access
                          Gtk_Source_Style_Scheme_Manager_Record
             );
--
-- Get_Scheme -- Get scheme style by ID
--
--    Manager - The manager
--    Scheme  - Style scheme ID to find
--
-- The returned value is owned by manager and must not be unref'ed.
--
-- Returns :
--
--    The scheme or null
--
   function Get_Scheme
            (  Manager : not null access
                         Gtk_Source_Style_Scheme_Manager_Record;
               Scheme  : UTF8_String
            )  return Gtk_Source_Style_Scheme;
--
-- Get_Scheme_IDs -- Erase the cache
--
--    Manager - The manager
--
-- This  function  returns  an array of string containing the ids of the
-- available style schemes or Null_Ptr if no style scheme is  available.
-- The array elements are owned by the manager and must not be freed.
--
-- Returns :
--
--    The array of pointers to the identifiers
--
   function Get_Scheme_IDs
            (  Manager : not null access
                         Gtk_Source_Style_Scheme_Manager_Record
            )  return Chars_Ptr_Array;
--
-- Get_Search_Path -- Get the current search path for the manager
--
--    Manager - The manager
--
-- This  function  returns an array of string containing the path items.
-- The array elements are owned by the manager and must not be freed.
--
-- Returns :
--
--    The array of pointers to the path items
--
   function Get_Search_Path
            (  Manager : not null access
                         Gtk_Source_Style_Scheme_Manager_Record
            )  return Chars_Ptr_Array;
--
-- Gtk_New -- Creation of a new manager object
--
--    Manager - The result
--
   procedure Gtk_New (Manager : out Gtk_Source_Style_Scheme_Manager);
--
-- Initialize -- To be called by any derived object
--
--    Manager - The result
--
   procedure Initialize
             (  Manager : not null access
                          Gtk_Source_Style_Scheme_Manager_Record'Class
             );
--
-- Prepend_Search_Path -- Prepend an item to the path
--
--    Manager - The manager
--    Path    - A directory or a filename
--
-- This  procedure  prepends  path  to the list of directories where the
-- manager looks for style scheme files
--
   procedure Prepend_Search_Path
             (  Manager : not null access
                          Gtk_Source_Style_Scheme_Manager_Record;
                Path    : UTF8_String
             );
--
-- Set_Search_Path -- Set the search path
--
--    Manager - The manager
--  [ Dirs ]   - Array of pointers to C-strings (path items)
--
-- This  procedure  sets the list of directories where the manager looks
-- for style scheme files. When Dirs is absent the search path is  reset
-- to default.

   procedure Set_Search_Path
             (  Manager : not null access
                          Gtk_Source_Style_Scheme_Manager_Record;
                Dirs    : Chars_Ptr_Array
             );
   procedure Set_Search_Path
             (  Manager : not null access
                          Gtk_Source_Style_Scheme_Manager_Record
             );
private
   type Gtk_Source_Style_Scheme_Manager_Record is
      new GObject_Record with null record;

end Gtk.Source_Style_Scheme_Manager;
