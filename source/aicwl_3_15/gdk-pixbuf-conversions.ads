--                                                                    --
--  package Gdk.Pixbuf.From_Address Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Summer, 2010       --
--                                                                    --
--                                Last revision :  19:03 27 May 2009  --
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
-- GtkAda  2.14.2  introduced  a  backward  incompatibility  by   making
-- Gdk_Pixbuf a tagged type rather than plain pointer.  This package  is
-- provided for backward compatibility of the GtkAda Contributions.
--
-- !!WARNING!! Use only with GtkAda >= 2.14.2
--
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with GLib.Object;  use GLib.Object;
with System;       use System;

with Glib.Properties;
with Gtk.Cell_Renderer_Pixbuf;

package Gdk.Pixbuf.Conversions is
--
-- From_Address -- Ada object creation
--
--    Object - Obtained from a GDK call
--
-- Returns :
--
--    The Ada object
--
   function From_Address (Object : Address) return Gdk_Pixbuf
      renames Convert;
--
-- To_Address -- Getting C object address
--
--    Object - An Ada object
--
-- Returns :
--
--    The C object's address
--
   function To_Address
            (  Object : access GObject_Record'Class
            )  return Address renames GLib.Object.Get_Object;
--
-- Set_Pixbuf_Property -- Set pixbuf property
--
   procedure Set_Pixbuf_Property
             (  Object : access GObject_Record'Class;
                Name   : Glib.Properties.Property_Object :=
                            Gtk.Cell_Renderer_Pixbuf.Pixbuf_Property;
                Value  : access GObject_Record'Class
             )  renames Glib.Properties.Set_Property;

   subtype To_Value is Gdk_Pixbuf;

end Gdk.Pixbuf.Conversions;
