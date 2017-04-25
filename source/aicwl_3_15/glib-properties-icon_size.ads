--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Properties.Icon_Size                   Luebeck            --
--  Instantiation                                  Spring, 2007       --
--                                                                    --
--                                Last revision :  09:12 29 Jun 2008  --
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
--  This   package   declares  an  Ada  and  a  GTK+  enumeration  types
--  corresponding  to  Gtk_Icon_Size.  For  the  latter  properties  are
--  defined by instantiation of Generic_Enumeration_Property.
--
with GLib.Generic_Properties;
with Gtk.Enums;

package GLib.Properties.Icon_Size is
   --
   -- Since  GtkAda  2.10 Gtk_Icon_Size_Enum is not an enumeration type.
   -- Therefore we have to declare an enumeration type here.
   --
   type Gtk_Icon_Size_Enum is
        (  Invalid,
           Menu,
           Small_Toolbar,
           Large_Toolbar,
           Button,
           DND,
           Dialog
        );
   for Gtk_Icon_Size_Enum use
       (  Invalid       => Gtk.Enums.Icon_Size_Invalid,
          Menu          => Gtk.Enums.Icon_Size_Menu,
          Small_Toolbar => Gtk.Enums.Icon_Size_Small_Toolbar,
          Large_Toolbar => Gtk.Enums.Icon_Size_Large_Toolbar,
          Button        => Gtk.Enums.Icon_Size_Button,
          DND           => Gtk.Enums.Icon_Size_DND,
          Dialog        => Gtk.Enums.Icon_Size_Dialog
       );
   for Gtk_Icon_Size_Enum'Size use GInt'Size;
   package Property is
      new GLib.Generic_Properties.Generic_Enumeration_Property
          (  "icon-size",
             Gtk_Icon_Size_Enum
          );
end GLib.Properties.Icon_Size;
