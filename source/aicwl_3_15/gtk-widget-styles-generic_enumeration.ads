--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Widget.Styles.                         Luebeck            --
--         Generic_Enumeration                     Summer, 2006       --
--  Interface                                                         --
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
--
--  This generic package is used for dealing with  style  properties  of
--  enumerations. Such properties can be specified in the form:
--
--     style "..."
--     {
--        <class_name>::<property_name> = <property_value>
--
--  For example:
--
--     My_Widget::My_Property = My_Property_Value
--
--  Differently to the standard parser, one  used  in  this  package  is
--  case-insensitive.  When  an  enumeration style property is specified
--  for a class using the standard parser, that is, when  such  property
--  is installed using Install_Style_Property, then its values has
--  to be capitalized.
--
--  The  package  is  generic.  Its  formal  parameter is an instance of
--  GLib.Generic_Properties.Generic_Enumeration_Property. Technically it
--  should  be  its  child,  but that is impossible, because the generic
--  parent were a nested package in that case.
--
with GLib.Generic_Properties;

generic
   with package Enumeration_Property is
      new GLib.Generic_Properties.Generic_Enumeration_Property (<>);
package Gtk.Widget.Styles.Generic_Enumeration is
   use Enumeration_Property;
--
-- Style_Get -- Get style enumeration property
--
--    Widget        - To get a style property of
--    Property_Name - The name of the style property
--
-- This function is tailored for enumeration properties installed  using
-- Install_Style from this package.
--
-- Returns :
--
--    The value of the property
--
   function Style_Get
            (  Widget        : access Gtk_Widget_Record'Class;
               Property_Name : String
            )  return Enumeration;
--
-- Install_Style -- Install an enumeration property
--
--    Class     - The class
--    Enum_Spec - A  property  obtained  by  Gnew_Enum  of  the  package
--                Enumeration_Property
--
   procedure Install_Style
             (  Class     : GObject_Class;
                Enum_Spec : Param_Spec
             );
private
--
-- Enum_Property_Parser -- Custom parser
--
   function Enum_Property_Parser
            (  PSpec          : Param_Spec;
               RC_String      : GString;
               Property_Value : access GValue
            )  return GBoolean;
   pragma Convention (C, Enum_Property_Parser);

   Parser : constant Gtk_RC_Property_Parser :=
               Enum_Property_Parser'Access;

end Gtk.Widget.Styles.Generic_Enumeration;
