--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Enums.String_Lists.Wildcards           Winter, 2007       --
--  Interface                                                         --
--                                Last revision :  19:52 12 Jan 2008  --
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
--  This package is provides wildcard patterns. A pattern is a  list  of
--  alternatives.  An  empty list matches anything. Otherwise it matches
--  only if at least one alternative does. An  alternative  can  contain
--  wildcard characters as described in Strings_Edit.UTF8.Wildcards.
--
package Gtk.Enums.String_Lists.Wildcards is

   Any : constant String_List.GList := String_List.Null_List;
--
-- Match -- A text against a pattern
--
--    Text    - To match
--    Pattern - The pattern
--
-- The pattern can be specified either as a list of  UTF8_String  or  as
-- controlled wrapper around it.
--
-- Returns :
--
--    True if Pattern matches Text
--
   function Match
            (  Text    : UTF8_String;
               Pattern : Controlled_String_List
            )  return Boolean;
   function Match
            (  Text    : UTF8_String;
               Pattern : String_List.GList
            )  return Boolean;

end Gtk.Enums.String_Lists.Wildcards;
