--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Enums.String_Lists                     Spring, 2007       --
--  Implementation                                                    --
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

with Ada.Unchecked_Deallocation;

package body Gtk.Enums.String_Lists is

   use String_List;

   procedure Adjust (List : in out Controlled_String_List) is
   begin
      if List.Ptr /= null then
         List.Ptr.Use_Count := List.Ptr.Use_Count + 1;
      end if;
   end Adjust;

   procedure Finalize (List : in out Controlled_String_List) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  String_List_Body,
                String_List_Body_Ptr
             );
   begin
      if List.Ptr /= null then
         if List.Ptr.Use_Count = 0 then
            raise Program_Error;
         elsif List.Ptr.Use_Count = 1 then
            Free_String_List (List.Ptr.List);
            Free (List.Ptr);
         else
            List.Ptr.Use_Count := List.Ptr.Use_Count - 1;
            List.Ptr := null;
         end if;
      end if;
   end Finalize;

   function Get_GList (List : Controlled_String_List)
      return String_List.GList is
   begin
      return List.Ptr.List;
   end Get_GList;

   function "/" (Left : Controlled_String_List; Right : UTF8_String)
      return Controlled_String_List is
   begin
      Left.Ptr.Use_Count := Left.Ptr.Use_Count + 1;
      Append (Left.Ptr.List, Right);
      return (Ada.Finalization.Controlled with Left.Ptr);
   end "/";

   function "/" (Left, Right : UTF8_String)
      return Controlled_String_List is
      Ptr : constant String_List_Body_Ptr := new String_List_Body;
   begin
      Append (Ptr.List, Left);
      Append (Ptr.List, Right);
      return (Ada.Finalization.Controlled with Ptr);
   end "/";

end Gtk.Enums.String_Lists;
