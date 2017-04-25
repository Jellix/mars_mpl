--                                                                    --
--  package GIO.Mount               Copyright (c)  Dmitry A. Kazakov  --
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

package body GIO.Mount is

   function Get_Root (Mount : Address) return Address;
   pragma Import (C, Get_Root, "g_mount_get_root");

   procedure Unref (File : Address);
   pragma Import (C, Unref, "g_object_unref");

   function Can_Eject
            (  Mount : not null access GMount_Record
            )  return Boolean is
      function Internal (Mount : Address) return GBoolean;
      pragma Import (C, Internal, "g_mount_can_eject");
   begin
      return 0 /= Internal (Get_Object (Mount));
   end Can_Eject;

   function Can_Unmount
            (  Mount : not null access GMount_Record
            )  return Boolean is
      function Internal (Mount : Address) return GBoolean;
      pragma Import (C, Internal, "g_mount_can_unmount");
   begin
      return 0 /= Internal (Get_Object (Mount));
   end Can_Unmount;

   function Convert (Mount : GMount) return Address is
   begin
      return Get_Object (Mount);
   end Convert;

   function Convert (Pointer : Address) return GMount is
      Stub : GMount_Record;
   begin
      return GMount (Get_User_Data (Pointer, Stub));
   end Convert;

   function Get_Name
            (  Mount : not null access GMount_Record
            )  return UTF8_String is
      function Internal (Mount : Address) return Chars_Ptr;
      pragma Import (C, Internal, "g_mount_get_name");
      Ptr : Chars_Ptr := Internal (Get_Object (Mount));
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

   function Get_Icon
            (  Mount : not null access GMount_Record
            )  return GObject is
      function Internal (Mount : Address) return Address;
      pragma Import (C, Internal, "g_mount_get_icon");
   begin
      return Convert (Internal (Get_Object (Mount)));
   end Get_Icon;

   function Get_Root
            (  Mount : not null access GMount_Record
            )  return UTF8_String is
      File : constant Address := Get_Root (Get_Object (Mount));
   begin
      if File = Null_Address then
         return "";
      else
         declare
            function Internal (File : Address) return Chars_Ptr;
            pragma Import (C, Internal, "g_file_get_path");
            Ptr : Chars_Ptr := Internal (File);
         begin
            if Ptr = Null_Ptr then
               Unref (File);
               return "";
            else
                declare
                   Result : constant UTF8_String := Value (Ptr);
                begin
                   Unref (File);
                   Free (Ptr);
                   return Result;
                end;
            end if;
         end;
      end if;
   end Get_Root;

   function Get_UUID
            (  Mount : not null access GMount_Record
            )  return UTF8_String is
      function Internal (Mount : Address) return Chars_Ptr;
      pragma Import (C, Internal, "g_mount_get_uuid");
      Ptr : Chars_Ptr := Internal (Get_Object (Mount));
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
   end Get_UUID;

----
---- Query_Exists -- Check if the mount exists
----
----    Mount - To check for existence
----
---- Returns :
----
----    True if mount exists
----
--   function Query_Exists (Mount : access GMount_Record) return Boolean;
--
--   function Query_Exists (Mount : access GMount_Record)
--      return Boolean is
--      File : Address := Get_Root (Get_Object (Mount));
--   begin
--      if File = Null_Address then
--         return False;
--      else
--         declare
--            function Internal
--                     (  File        : Address;
--                        Cancellable : Address := Null_Address
--                     )  return GBoolean;
--            pragma Import (C, Internal, "g_file_query_exists");
--            Result : GBoolean := Internal (File);
--         begin
--            Unref (File);
--            return Result /= 0;
--         end;
--      end if;
--   end Query_Exists;

end GIO.Mount;
