--                                                                    --
--  package GIO.Volume_Monitor      Copyright (c)  Dmitry A. Kazakov  --
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
with System;                use System;

package body GIO.Volume_Monitor is

   function Get return GVolume_Monitor is
      function Internal return Address;
      pragma Import (C, Internal, "g_volume_monitor_get");
      Stub : GVolume_Monitor_Record;
   begin
      return GVolume_Monitor (Get_User_Data (Internal, Stub));
   end Get;

   function Get_Connected_Drives
            (  Monitor : not null access GVolume_Monitor_Record
            )  return Drive_List.GList is
      function Internal (Monitor : Address) return Address;
      pragma Import
             (  C,
                Internal,
                "g_volume_monitor_get_connected_drives"
             );
      List : Drive_List.Glist;
   begin
      Drive_List.Set_Object (List, Internal (Get_Object (Monitor)));
      return List;
   end Get_Connected_Drives;

   function Get_Mounts
            (  Monitor : not null access GVolume_Monitor_Record
            )  return Mount_List.GList is
      function Internal (Monitor : Address) return Address;
      pragma Import
             (  C,
                Internal,
                "g_volume_monitor_get_mounts"
             );
      List : Mount_List.Glist;
   begin
      Mount_List.Set_Object (List, Internal (Get_Object (Monitor)));
      return List;
   end Get_Mounts;

   function Get_Mount_For_UUID
            (  Monitor : not null access GVolume_Monitor_Record;
               UUID    : UTF8_String
            )  return GMount is
      function Internal
               (  Monitor : Address;
                  UUID    : Char_Array
               )  return Address;
      pragma Import
             (  C,
                Internal,
                "g_volume_monitor_get_mount_for_uuid"
             );
      Ptr : constant Address :=
            Internal (Get_Object (Monitor), To_C (UUID));
   begin
      if Ptr = Null_Address then
         return null;
      else
         return Convert (Ptr);
      end if;
   end Get_Mount_For_UUID;

   function Get_Volumes
            (  Monitor : not null access GVolume_Monitor_Record
            )  return Volume_List.GList is
      function Internal (Monitor : Address) return Address;
      pragma Import
             (  C,
                Internal,
                "g_volume_monitor_get_volumes"
             );
      List : Volume_List.Glist;
   begin
      Volume_List.Set_Object (List, Internal (Get_Object (Monitor)));
      return List;
   end Get_Volumes;

   function Get_Volume_For_UUID
            (  Monitor : not null access GVolume_Monitor_Record;
               UUID    : UTF8_String
            )  return GVolume is
      function Internal
               (  Monitor : Address;
                  UUID    : Char_Array
               )  return Address;
      pragma Import
             (  C,
                Internal,
                "g_volume_monitor_get_volume_for_uuid"
             );
      Ptr : constant Address :=
            Internal (Get_Object (Monitor), To_C (UUID));
   begin
      if Ptr = Null_Address then
         return null;
      else
         return Convert (Ptr);
      end if;
   end Get_Volume_For_UUID;

end GIO.Volume_Monitor;
