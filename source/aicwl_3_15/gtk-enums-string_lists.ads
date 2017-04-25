--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Enums.String_Lists                     Spring, 2007       --
--  Interface                                                                  --
--                                Last revision :  10:00 13 Oct 2007  --
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
--
--  This  package  is  provided  to  ease  handling  of  lists of static
--  strings. When a String_List.GList expression is expected at  Foo  it
--  can be acheved as:
--
--     Foo (+ "first string" / "second string" / third string");
--
-- The list will be created and removed after completion of Foo.

with Ada.Finalization;

package Gtk.Enums.String_Lists is
--
-- Controlled_String_List -- The controlled wrapper around GList
--
   type Controlled_String_List (<>) is limited private;
--
-- Get_GList -- Conversion to GList
--
--    List - The controlled list
--
-- Returns :
--
--    The GList of
--
   function Get_GList (List : Controlled_String_List)
      return String_List.GList;
--
-- + -- Conversion to GList
--
--    List - The controlled list
--
-- Returns :
--
--    The GList of
--
   function "+" (List : Controlled_String_List)
      return String_List.GList renames Get_GList;
--
-- / -- List construction
--
--    Left  - A controlled list or string
--    Right - A string to add
--
-- Returns :
--
--    The concatenation of
--
   function "/" (Left, Right : UTF8_String)
      return Controlled_String_List;
   function "/" (Left : Controlled_String_List; Right : UTF8_String)
      return Controlled_String_List;

private
   type String_List_Body is record
      Use_Count : Natural := 1;
      List      : String_List.GList;
   end record;
   type String_List_Body_Ptr is access String_List_Body;

   type Controlled_String_List is
      new Ada.Finalization.Controlled with
   record
      Ptr : String_List_Body_Ptr;
   end record;
   procedure Adjust (List : in out Controlled_String_List);
   procedure Finalize (List : in out Controlled_String_List);

end Gtk.Enums.String_Lists;
