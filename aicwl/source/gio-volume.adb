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
-- __________________________________________________________________ --

with Interfaces.C.Strings;

package body GIO.Volume is

   function Convert (Volume  : GVolume) return System.Address is
   begin
      return Glib.Object.Get_Object (Volume);
   end Convert;

   function Convert (Pointer : System.Address) return GVolume is
      Stub : GVolume_Record;
   begin
      return GVolume  (Glib.Object.Get_User_Data (Pointer, Stub));
   end Convert;

   function Get_Icon
     (Volume : not null access GVolume_Record) return Glib.Object.GObject
   is
      function Internal (Volume : System.Address) return System.Address;
      pragma Import (C, Internal, "g_volume_get_icon");
   begin
      return Glib.Object.Convert (Internal (Glib.Object.Get_Object (Volume)));
   end Get_Icon;

   function Get_Name
     (Volume : not null access GVolume_Record) return Glib.UTF8_String
   is
      function Internal (Volume : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_volume_get_name");
      Ptr : Interfaces.C.Strings.chars_ptr := Internal (Glib.Object.Get_Object (Volume));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
            Result : constant Glib.UTF8_String :=
                       Interfaces.C.Strings.Value (Ptr);
         begin
            Interfaces.C.Strings.Free (Ptr);
            return Result;
         end;
      end if;
   end Get_Name;

   function Get_Volume
     (Mount : not null access GIO.Mount.GMount_Record'Class) return GVolume
   is
      function Internal (Volume : System.Address) return System.Address;
      pragma Import (C, Internal, "g_mount_get_volume");
   begin
      return Convert (Internal (Glib.Object.Get_Object (Mount)));
   end Get_Volume;

   function Should_Automount
     (Volume : not null access GVolume_Record) return Boolean
   is
      function Internal (Volume : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_volume_should_automount");

      use type Glib.Gboolean;
   begin
      return 0 /= Internal (Glib.Object.Get_Object (Volume));
   end Should_Automount;

end GIO.Volume;
