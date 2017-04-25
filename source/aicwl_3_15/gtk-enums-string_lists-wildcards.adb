--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Enums.String_Lists.Wildcards           Winter, 2007       --
--  Implementation                                                    --
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

with Strings_Edit.UTF8.Wildcards;  use Strings_Edit.UTF8.Wildcards;

package body Gtk.Enums.String_Lists.Wildcards is

   function Match (Text : UTF8_String; Pattern : String_List.GList)
      return Boolean is
      use String_List;
   begin
      if Pattern = Null_List then
         return True;
      end if;
      declare
         Alternative : String_List.GList := Pattern;
      begin
         loop
            if Match (Text, Get_Data (Alternative)) then
               return True;
            end if;
            Alternative := Next (Alternative);
            if Alternative = Null_List then
               return False;
            end if;
         end loop;
      end;
   end Match;

   function Match (Text : UTF8_String; Pattern : Controlled_String_List)
      return Boolean is
   begin
      return Match (Text, +Pattern);
   end Match;

end Gtk.Enums.String_Lists.Wildcards;
