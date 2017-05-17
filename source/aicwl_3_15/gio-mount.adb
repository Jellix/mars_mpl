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
-- __________________________________________________________________ --

with Interfaces.C.Strings;

package body GIO.Mount is

   function Get_Root (Mount : System.Address) return System.Address;
   pragma Import (C, Get_Root, "g_mount_get_root");

   procedure Unref (File : System.Address);
   pragma Import (C, Unref, "g_object_unref");

   function Can_Eject
     (Mount : not null access GMount_Record) return Boolean
   is
      function Internal (Mount : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_can_eject");

      use type Glib.Gboolean;
   begin
      return 0 /= Internal (Glib.Object.Get_Object (Mount));
   end Can_Eject;

   function Can_Unmount
     (Mount : not null access GMount_Record) return Boolean
   is
      function Internal (Mount : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "g_mount_can_unmount");

      use type Glib.Gboolean;
   begin
      return 0 /= Internal (Glib.Object.Get_Object (Mount));
   end Can_Unmount;

   function Convert (Mount : GMount) return System.Address is
   begin
      return Glib.Object.Get_Object (Mount);
   end Convert;

   function Convert (Pointer : System.Address) return GMount is
      Stub : GMount_Record;
   begin
      return GMount (Glib.Object.Get_User_Data (Pointer, Stub));
   end Convert;

   function Get_Name
     (Mount : not null access GMount_Record) return Glib.UTF8_String
   is
      function Internal (Mount : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_mount_get_name");
      Ptr : Interfaces.C.Strings.chars_ptr :=
              Internal (Glib.Object.Get_Object (Mount));

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

   function Get_Icon
     (Mount : not null access GMount_Record) return Glib.Object.GObject
   is
      function Internal (Mount : System.Address) return System.Address;
      pragma Import (C, Internal, "g_mount_get_icon");
   begin
      return Glib.Object.Convert (Internal (Glib.Object.Get_Object (Mount)));
   end Get_Icon;

   function Get_Root
     (Mount : not null access GMount_Record) return Glib.UTF8_String
   is
      File : constant System.Address :=
               Get_Root (Glib.Object.Get_Object (Mount));

      use type System.Address;
   begin
      if File = System.Null_Address then
         return "";
      else
         declare
            function Internal (File : System.Address)
                               return Interfaces.C.Strings.chars_ptr;
            pragma Import (C, Internal, "g_file_get_path");
            Ptr : Interfaces.C.Strings.chars_ptr := Internal (File);

            use type Interfaces.C.Strings.chars_ptr;
         begin
            if Ptr = Interfaces.C.Strings.Null_Ptr then
               Unref (File);
               return "";
            else
               declare
                  Result : constant Glib.UTF8_String :=
                             Interfaces.C.Strings.Value (Ptr);
               begin
                  Unref (File);
                  Interfaces.C.Strings.Free (Ptr);
                  return Result;
               end;
            end if;
         end;
      end if;
   end Get_Root;

   function Get_UUID
     (Mount : not null access GMount_Record) return Glib.UTF8_String
   is
      function Internal (Mount : System.Address)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_mount_get_uuid");
      Ptr : Interfaces.C.Strings.chars_ptr :=
              Internal (Glib.Object.Get_Object (Mount));

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
--      File : System.Address := Get_Root (Glib.Object.Get_Object (Mount));
--   begin
--      if File = Null_Address then
--         return False;
--      else
--         declare
--            function Internal
--                     (  File        : System.Address;
--                        Cancellable : System.Address := Null_Address
--                     )  return Glib.GBoolean;
--            pragma Import (C, Internal, "g_file_query_exists");
--            Result : Glib.GBoolean := Internal (File);
--         begin
--            Unref (File);
--            return Result /= 0;
--         end;
--      end if;
--   end Query_Exists;

end GIO.Mount;
