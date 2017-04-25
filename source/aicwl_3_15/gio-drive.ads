--                                                                    --
--  package GIO.Drive               Copyright (c)  Dmitry A. Kazakov  --
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
--
-- GDrive  represent  a piece of hardware connected to the machine. It's
-- generally only  created  for  removable  hardware  or  hardware  with
-- removable media.
--
-- GDrive  is  a  container class for GVolume objects that stem from the
-- same piece of media. As such,  GDrive  abstracts  a  drive  with  (or
-- without) removable media and provides operations for querying whether
-- media  is  available, determing whether media change is automatically
-- detected and ejecting the media.
--
-- If  the  GDrive  reports that media isn't automatically detected, one
-- can  poll for media; typically one should not do this periodically as
-- a  poll  for media operation is potententially expensive and may spin
-- up the drive creating noise.
--
-- GDrive  supports  starting  and  stopping  drives with authentication
-- support  for the former. This can be used to support a diverse set of
-- use cases including connecting/disconnecting iSCSI devices,  powering
-- down  external  disk  enclosures  and  starting/stopping   multi-disk
-- devices such as RAID devices. Note  that  the  actual  semantics  and
-- side-effects of starting/stopping a  GDrive  may  vary  according  to
-- implementation.  To  choose the correct verbs in e.g. a file manager,
-- use g_drive_get_start_stop_type().
--
with GIO.Mount;    use GIO.Mount;
with GIO.Volume;   use GIO.Volume;
with GLib;         use GLib;
with GLib.Object;  use GLib.Object;
with System;       use System;

with Glib.Glist;

package GIO.Drive is

   type GDrive_Record is new GObject_Record with null record;
   type GDrive is access all GDrive_Record'Class;
--
-- Can_Eject -- Checks if drive can be ejected
--
--    Drive - A drive
--
-- Returns :
--
--    True if drive can be ejected
--
   function Can_Eject
            (  Drive : not null access GDrive_Record
            )  return Boolean;
--
-- Get_Drive -- Get drive of a volume or mount
--
--    Volume / Mount - To get the drive of
--
-- The returned object should be unreffed  with  Unref  when  no  longer
-- needed.
--
--    The drive or null
--
   function Get_Drive
            (  Volume : not null access GVolume_Record'Class
            )  return GDrive;
   function Get_Drive
            (  Mount : not null access GMount_Record'Class
            )  return GDrive;
--
-- Get_Icon -- Get icon of a drive
--
--    Drive - To get the icon of
--
-- The returned object should be unreffed  with  Unref  when  no  longer
-- needed.
--
-- Returns :
--
--    The icon or null
--
   function Get_Icon
            (  Drive : not null access GDrive_Record
            )  return GObject;
--
-- Get_Name -- Get name of a drive
--
--    Drive - To get the name of
--
-- Returns :
--
--    The name
--
   function Get_Name
            (  Drive : not null access GDrive_Record
            )  return UTF8_String;
--
-- Get_Volumes -- Gets a list of mountable volumes
--
--    Drive - A drive
--
-- The  elements  of  result  must released using Unref. The list itself
-- must be freed using Free.
--
-- Returns :
--
--    A list of volumes
--
   function Get_Volumes
            (  Drive : not null access GDrive_Record
            )  return Volume_List.GList;
--
-- Has_Media -- Check
--
--    Drive - A drive
--
-- Returns :
--
--    True if Drive has media
--
   function Has_Media
            (  Drive : not null access GDrive_Record
            )  return Boolean;
--
-- Has_Volumes -- Check
--
--    Drive - A drive
--
-- Returns :
--
--    True if Drive contains volumes
--
   function Has_Volumes
            (  Drive : not null access GDrive_Record
            )  return Boolean;
--
-- Is_Media_Check_Automatic -- Check
--
--    Drive - A drive
--
-- Returns :
--
--    True if Drive is capable of automatically detecting media changes
--
   function Is_Media_Check_Automatic
            (  Drive : not null access GDrive_Record
            )  return Boolean;
--
-- Is_Media_Removable -- Check
--
--    Drive - A drive
--
-- Returns :
--
--    True if the drive supports removable media
--
   function Is_Media_Removable
            (  Drive : not null access GDrive_Record
            )  return Boolean;

   function Convert (Mount  : GDrive) return Address;
   function Convert (Pointer : Address) return GDrive;
   package Drive_List is new Glib.Glist.Generic_List (GDrive);

end GIO.Drive;
