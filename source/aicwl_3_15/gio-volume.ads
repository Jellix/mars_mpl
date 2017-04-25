--                                                                    --
--  package GIO.Volume              Copyright (c)  Dmitry A. Kazakov  --
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
--  The GVolume interface represents user-visible objects  that  can  be
--  mounted.
--
with GIO.Mount;    use GIO.Mount;
with GLib;         use GLib;
with GLib.Object;  use GLib.Object;
with System;       use System;

with Glib.Glist;

package GIO.Volume is

   type GVolume_Record is new GObject_Record with null record;
   type GVolume is access all GVolume_Record'Class;
--
-- Get_Icon -- Get icon of a volume
--
--    Volume - To get the icon of
--
-- The returned object should be unreffed  with  Unref  when  no  longer
-- needed.
--
-- Returns :
--
--    The icon or null
--
   function Get_Icon
            (  Volume : not null access GVolume_Record
            )  return GObject;
--
-- Get_Name -- Get name of a volume
--
--    Volume - To get the name of
--
-- Returns :
--
--    The name
--
   function Get_Name
            (  Volume : not null access GVolume_Record
            )  return UTF8_String;
--
-- Get_Volume -- Get volume of a mount
--
--    Mount - To get the drive of
--
-- The returned object should be unreffed  with  Unref  when  no  longer
-- needed.
--
--    The volume or null
--
   function Get_Volume
            (  Mount : not null access GMount_Record'Class
            )  return GVolume;
--
-- Should_Automount -- Checks if volume can be mounted
--
--    Volume - To check
--
-- Returns :
--
--    True if volume should be automatically mounted.
--
   function Should_Automount
            (  Volume : not null access GVolume_Record
            )  return Boolean;

   function Convert (Volume  : GVolume) return Address;
   function Convert (Pointer : Address) return GVolume;
   package Volume_List is new Glib.Glist.Generic_List (GVolume);

end GIO.Volume;
