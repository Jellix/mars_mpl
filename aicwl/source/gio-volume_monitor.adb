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
-- __________________________________________________________________ --

with Interfaces.C;
with System;

package body GIO.Volume_Monitor is

   function Get return GVolume_Monitor is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_volume_monitor_get");
      Stub : GVolume_Monitor_Record;
   begin
      return GVolume_Monitor (Glib.Object.Get_User_Data (Internal, Stub));
   end Get;

   function Get_Connected_Drives
     (Monitor : not null access GVolume_Monitor_Record)
      return GIO.Drive.Drive_List.Glist
   is
      function Internal (Monitor : System.Address) return System.Address;
      pragma Import (C, Internal, "g_volume_monitor_get_connected_drives");
      List : GIO.Drive.Drive_List.Glist;
   begin
      GIO.Drive.Drive_List.Set_Object
        (List,
         Internal (Glib.Object.Get_Object (Monitor)));
      return List;
   end Get_Connected_Drives;

   function Get_Mounts
     (Monitor : not null access GVolume_Monitor_Record)
      return GIO.Mount.Mount_List.Glist
   is
      function Internal (Monitor : System.Address) return System.Address;
      pragma Import (C, Internal, "g_volume_monitor_get_mounts");
      List : GIO.Mount.Mount_List.Glist;
   begin
      GIO.Mount.Mount_List.Set_Object
        (List,
         Internal (Glib.Object.Get_Object (Monitor)));
      return List;
   end Get_Mounts;

   function Get_Mount_For_UUID
     (Monitor : not null access GVolume_Monitor_Record;
      UUID    : Glib.UTF8_String) return GIO.Mount.GMount
   is
      function Internal
        (Monitor : System.Address;
         UUID    : Interfaces.C.char_array) return System.Address;
      pragma Import (C, Internal, "g_volume_monitor_get_mount_for_uuid");
      Ptr : constant System.Address :=
              Internal (Glib.Object.Get_Object (Monitor),
                        Interfaces.C.To_C (UUID));

      use type System.Address;
   begin
      if Ptr = System.Null_Address then
         return null;
      else
         return GIO.Mount.Convert (Ptr);
      end if;
   end Get_Mount_For_UUID;

   function Get_Volumes
     (Monitor : not null access GVolume_Monitor_Record)
      return GIO.Volume.Volume_List.Glist
   is
      function Internal (Monitor : System.Address) return System.Address;
      pragma Import (C, Internal, "g_volume_monitor_get_volumes");
      List : GIO.Volume.Volume_List.Glist;
   begin
      GIO.Volume.Volume_List.Set_Object
        (List,
         Internal (Glib.Object.Get_Object (Monitor)));
      return List;
   end Get_Volumes;

   function Get_Volume_For_UUID
     (Monitor : not null access GVolume_Monitor_Record;
      UUID    : Glib.UTF8_String) return GIO.Volume.GVolume
   is
      function Internal
        (Monitor : System.Address;
         UUID    : Interfaces.C.char_array) return System.Address;
      pragma Import (C, Internal, "g_volume_monitor_get_volume_for_uuid");
      Ptr : constant System.Address :=
              Internal (Glib.Object.Get_Object (Monitor),
                        Interfaces.C.To_C (UUID));

      use type System.Address;
   begin
      if Ptr = System.Null_Address then
         return null;
      else
         return GIO.Volume.Convert (Ptr);
      end if;
   end Get_Volume_For_UUID;

end GIO.Volume_Monitor;
