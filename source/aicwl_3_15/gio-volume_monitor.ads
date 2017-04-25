--                                                                    --
--  package GIO.Volume_Monitor      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Summer, 2010       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with GIO.Drive;    use GIO.Drive;
with GIO.Mount;    use GIO.Mount;
with GIO.Volume;   use GIO.Volume;
with GLib;         use GLib;
with GLib.Object;  use GLib.Object;

package GIO.Volume_Monitor is

   type GVolume_Monitor_Record is new GObject_Record with null record;
   type GVolume_Monitor is access all GVolume_Monitor_Record'Class;
--
-- Get -- Get GIO monitor
--
-- The result must released using Unref.
--
-- Returns :
--
--    The monitor
--
   function Get return GVolume_Monitor;
--
-- Get_Connected_Drives -- Drives connected
--
--    Monitor - The volume monitor
--
-- The  elements  of  result  must released using Unref. The list itself
-- must be freed using Free.
--
-- Returns :
--
--    A list of drives connected to the system
--
   function Get_Connected_Drives
            (  Monitor : not null access GVolume_Monitor_Record
            )  return Drive_List.GList;
--
-- Get_Mounts -- Gets a list of the mounts on the system
--
--    Monitor - The volume monitor
--
-- The  elements  of  result  must released using Unref. The list itself
-- must be freed using Free.
--
-- Returns :
--
--    A list of mounts
--
   function Get_Mounts
            (  Monitor : not null access GVolume_Monitor_Record
            )  return Mount_List.GList;
--
-- Get_Mount_For_UUID -- Finds a mount object by its UUID
--
--    Monitor - The volume monitor
--
-- When not null, free the returned object with Unref.
--
-- Returns :
--
--    The mount our null
--
   function Get_Mount_For_UUID
            (  Monitor : not null access GVolume_Monitor_Record;
               UUID    : UTF8_String
            )  return GMount;
--
-- Get_Volumes -- Gets a list of the volumes on the system
--
--    Monitor - The volume monitor
--
-- The  elements  of  result  must released using Unref. The list itself
-- must be freed using Free.
--
-- Returns :
--
--    A list of volumes
--
   function Get_Volumes
            (  Monitor : not null access GVolume_Monitor_Record
            )  return Volume_List.GList;
--
-- Get_Volume_For_UUID -- Finds a volume object by its UUID
--
--    Monitor - The volume monitor
--
-- When not null, free the returned object with Unref.
--
-- Returns :
--
--    The mount our null
--
   function Get_Volume_For_UUID
            (  Monitor : not null access GVolume_Monitor_Record;
               UUID    : UTF8_String
            )  return GVolume;

end GIO.Volume_Monitor;
