--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_Mark_Attributes                  Luebeck            --
--  Interface                                      Summer, 2013       --
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

with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.RGBA;         use Gdk.RGBA;
with Gtk.Source_Mark;  use Gtk.Source_Mark;
with Gtk.Widget;       use Gtk.Widget;

with Glib.G_Icon;

package Gtk.Source_Mark_Attributes is
--
-- Gtk_Source_Mark_Record -- Mark object for Gtk_Source_Buffer_Record
--
   type Gtk_Source_Mark_Atributes_Record is
      new GObject_Record with private;
   type Gtk_Source_Mark_Atributes is
      access all Gtk_Source_Mark_Atributes_Record'Class;
--
-- Get_Background -- Of a mark
--
--    Attributes - Source mark attributes
--
-- Returns :
--
--    The background color
--
   function Get_Background
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return Gdk_RGBA;
--
-- Get_GIcon -- Of a mark
--
--    Attributes - Source mark attributes
--
-- Returns :
--
--    The the icon
--
   function Get_GIcon
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return Glib.G_Icon.G_Icon;
--
-- Get_Icon_Name -- Of a mark
--
--    Attributes - Source mark attributes
--
-- Returns :
--
--    The the icon name or an empty string
--
   function Get_Icon_Name
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return UTF8_String;
--
-- Get_Pxibuf -- Of a mark
--
--    Attributes - Source mark attributes
--
-- Returns :
--
--    The pixbuf or null
--
   function Get_Pxibuf
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return Gdk_Pixbuf;
--
-- Get_Stock_ID -- Of a mark
--
--    Attributes - Source mark attributes
--
-- Returns :
--
--    The stock-ID of the icon or an empty string
--
   function Get_Stock_ID
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return UTF8_String;
--
-- Get_Tooltip_Markup -- Of a mark
--
--    Attributes - Source mark attributes
--    Mark       - Source mark
--
-- Returns :
--
--    The tooltip text (may contain markup)
--
   function Get_Tooltip_Markup
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record;
               Mark       : not null access Gtk_Source_Mark_Record
            )  return UTF8_String;
--
-- Get_Tooltip_Text -- Of a mark
--
--    Attributes - Source mark attributes
--    Mark       - Source mark
--
-- Returns :
--
--    The tooltip text
--
   function Get_Tooltip_Text
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record;
               Mark       : not null access Gtk_Source_Mark_Record
            )  return UTF8_String;
--
-- Gtk_New -- Object creation
--
--    Attributes - Source mark attributes
--
   procedure Gtk_New (Attributes : out Gtk_Source_Mark_Atributes);
--
-- Initialize -- To be called by any derived object
--
--    Attributes - Source mark attributes
--
   procedure Initialize
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record'Class
             );
--
-- Render_Icon -- Of a mark
--
--    Attributes - Source mark attributes
--    Widget     - The widget
--    Size       - The icon size
--
-- Returns :
--
--    The pixbuf of the rendered icon (owned, do not Unref)
--
   function Render_Icon
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record;
               Widget     : not null access Gtk_Widget_Record'Class;
               Size       : GInt
            )  return Gdk_Pixbuf;
--
-- Set_Background -- Of a mark
--
--    Attributes - Source mark attributes
--    Background - The color to set
--
   procedure Set_Background
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Background : Gdk_RGBA
             );
--
-- Set_GIcon -- Of a mark
--
--    Attributes - Source mark attributes
--  [ Icon ]     - The icon to set
--
   procedure Set_GIcon
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Icon       : Glib.G_Icon.G_Icon
             );
--
-- Set_Icon_Name -- Of a mark
--
--    Attributes - Source mark attributes
--    Icon_Name  - The icon name to set
--
   procedure Set_Icon_Name
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Icon_Name  : UTF8_String
             );
--
-- Set_Pixbuf -- Of a mark
--
--    Attributes - Source mark attributes
--    Pixbuf     - The icon to set
--
   procedure Set_Pixbuf
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Pixbuf     : not null access Gdk_Pixbuf_Record'Class
             );
--
-- Set_Stock_ID -- Of a mark
--
--    Attributes - Source mark attributes
--    Stock_ID   - The stock icon to set
--
   procedure Set_Stock_ID
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Stock_ID   : UTF8_String
             );
private
   type Gtk_Source_Mark_Atributes_Record is
      new GObject_Record with null record;

end Gtk.Source_Mark_Attributes;
