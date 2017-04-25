--                                                                    --
--  package GIO.Mount               Copyright (c)  Dmitry A. Kazakov  --
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
--  The interface represents user-visible mounts.
--
with GLib;             use GLib;
with GLib.Object;      use GLib.Object;
with Glib.Properties;  use Glib.Properties;
with System;           use System;

with Glib.Glist;

package GIO.Mount is

   type GMount_Record is new GObject_Record with null record;
   type GMount is access all GMount_Record'Class;
--
-- Can_Eject -- Checks if mount can be ejected
--
--    Mount - A mount
--
-- Returns :
--
--    True if Mount can be ejected
--
   function Can_Eject
            (  Mount : not null access GMount_Record
            )  return Boolean;
--
-- Can_Unmount -- Checks if mount can be mounted
--
--    Mount - A mount
--
-- Returns :
--
--    True if mount can be unmounted
--
   function Can_Unmount
            (  Mount : not null access GMount_Record
            )  return Boolean;
--
-- Get_Icon -- Get icon of a mount
--
--    Mount - To get the icon of
--
-- The returned object should be unreffed  with  Unref  when  no  longer
-- needed.
--
-- Returns :
--
--    The icon or null
--
   function Get_Icon
            (  Mount : not null access GMount_Record
            )  return GObject;
--
-- Get_Name -- Get name of a mount
--
--    Mount - To get the name of
--
-- Returns :
--
--    The name
--
   function Get_Name
            (  Mount : not null access GMount_Record
            )  return UTF8_String;
--
-- Get_Root -- Get root path of a mount
--
--    Mount - To get the root of
--
-- Returns :
--
--    The local path of the mount root or an empty string
--
   function Get_Root
            (  Mount : not null access GMount_Record
            )  return UTF8_String;
--
-- Get_UUID -- Get UUID for a mount
--
--    Mount - To get the UUID of
--
-- This function sets the UUID for the mount. The reference is typically
-- based on the file system UUID for the mount in question and should be
-- considered an opaque string.
--
-- Returns :
--
--    The UUID or empty string if not available
--
   function Get_UUID
            (  Mount : not null access GMount_Record
            )  return UTF8_String;

   function Convert (Mount : GMount) return Address;
   function Convert (Pointer : Address) return GMount;
   package Mount_List is new Glib.Glist.Generic_List (GMount);

end GIO.Mount;
