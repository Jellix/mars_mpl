--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Object.Checked_Destroy                 Luebeck            --
--  Interface                                      Autumn, 2011       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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

with Gtk.Widget;

--
-- Checked_Destroy -- A safe variant of Destroy
--
--    Object - The widget to destroy
--
-- This procedure is  a safe variant  of Destroy,  which  checks  if the
-- reference  to  the widget is  floating.  The procedure  is void  when
-- Object is not bound to any C object.
--
procedure Glib.Object.Checked_Destroy
  (Object : not null access Gtk.Widget.Gtk_Widget_Record'Class);
