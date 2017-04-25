--                                                                    --
--  package GIO.GVolume             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Summer, 2010       --
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

with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package body GIO.Volume is

   function Convert (Volume  : GVolume ) return Address is
   begin
      return Get_Object (Volume);
   end Convert;

   function Convert (Pointer : Address) return GVolume is
      Stub : GVolume_Record;
   begin
      return GVolume  (Get_User_Data (Pointer, Stub));
   end Convert;

   function Get_Icon
            (  Volume : not null access GVolume_Record
            )  return GObject is
      function Internal (Volume : Address) return Address;
      pragma Import (C, Internal, "g_volume_get_icon");
   begin
      return Convert (Internal (Get_Object (Volume)));
   end Get_Icon;

   function Get_Name
            (  Volume : not null access GVolume_Record
            )  return UTF8_String is
      function Internal (Volume : Address) return Chars_Ptr;
      pragma Import (C, Internal, "g_volume_get_name");
      Ptr : Chars_Ptr := Internal (Get_Object (Volume));
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         declare
            Result : constant UTF8_String := Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end Get_Name;

   function Get_Volume
            (  Mount : not null access GMount_Record'Class
            )  return GVolume is
      function Internal (Volume : Address) return Address;
      pragma Import (C, Internal, "g_mount_get_volume");
   begin
      return Convert (Internal (Get_Object (Mount)));
   end Get_Volume;

   function Should_Automount
            (  Volume : not null access GVolume_Record
            )  return Boolean is
      function Internal (Volume : Address) return GBoolean;
      pragma Import (C, Internal, "g_volume_should_automount");
   begin
      return 0 /= Internal (Get_Object (Volume));
   end Should_Automount;

end GIO.Volume;
