--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Properties.Relief_Style                Luebeck            --
--  Instantiation                                  Spring, 2009       --
--                                                                    --
--                                Last revision :  17:01 20 Apr 2010  --
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
--  corresponding to Gtk_Relief_Style. For  the  latter  properties  are
--  defined by instantiation of Generic_Enumeration_Property.
--
with GLib.Generic_Properties;
with Gtk.Enums;

package GLib.Properties.Relief_Style is
   package Property is
      new GLib.Generic_Properties.Generic_Enumeration_Property
          (  "relief-style",
             Gtk.Enums.Gtk_Relief_Style
          );
end GLib.Properties.Relief_Style;
