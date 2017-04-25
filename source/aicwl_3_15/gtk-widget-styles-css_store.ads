--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Widget.Styles.CSS_Store                 Luebeck            --
--  Interface                                      Summer, 2013       --
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
--____________________________________________________________________--

with Ada.Text_IO;  use Ada.Text_IO;

package Gtk.Widget.Styles.CSS_Store is
--
-- Put_Styles -- Write widget properties into a resource file
--
--    File    - The file to write
--    Widget  - The widget
--    Recurse - The children of containers
--
-- This procedure enumerates the style properties of Widget  and  stores
-- them  into  a File in the format used for GTK+ CSS style sheet.  When
-- File is omitted, the standard output is used instead.  The  parameter
-- Recurse specifies if the children of the container widgets have to be
-- taped as well.  In this case the resources of the children are placed
-- into the file before the resources of the containers of.
--
   procedure Put_CSS_Styles
             (  File    : File_Type;
                Widget  : not null access Gtk_Widget_Record'Class;
                Recurse : Boolean := True
             );
   procedure Put_CSS_Styles
             (  Widget  : not null access Gtk_Widget_Record'Class;
                Recurse : Boolean := True
             );
end Gtk.Widget.Styles.CSS_Store;
