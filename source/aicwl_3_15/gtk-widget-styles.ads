--                                                                    --
--  package Gtk.Widget.Styles       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2006       --
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
--
--  This package contains GTK+ subroutines to  deal  with  widget  style
--  properties.
--
--  1. Installing style properties. The properties are  installed  on  a
--     class of widgets. For this the parent class of the widget has  to
--     be extended. This need to be done once  before  creation  of  the
--     first  widget  from  the  class. The proper place to do it is the
--     Initialize   procedure.   The   package   body   of   the  widget
--     implementation could contain:
--
--     My_Class_Record : Ada_GObject_Class;
--
--     procedure Initialize
--               (  Widget : not null access My_Widget_Record'Class
--               )  is
--        ...
--        To_Install : constant Boolean :=
--                     Class_Ref (Class_Record.The_Type) = Null_GObject_Class;
--     begin
--        ...Initialize (Widget); -- Parent's initialization
--        Initialize_Class_Record
--        (  Widget,
--           Null_Array,
--           My_Class_Record,
--           "MyWidgetClass"
--        );
--        if To_Install then
--           Install_Style_Property
--           (  My_Class_Ref (Class_Record.The_Type),
--              Gnew_String
--              (  Name    => "my-widget-property",
--                 Nick    => "Text",
--                 Blurb   => "The text my widget needs",
--                 Default => "Default text"
--           )  );
--           ... -- Other style properties
--        end if;
--        ... -- Continue initialization
--     end Initialize;
--
--  2. Querying style  properties.  Widget  style  properties  might  be
--     undefined while widget initialization.  In  particular  it  means
--     that  querying  style  properties  from  the Initialize procedure
--     might  fail  (to a default property value). To avoid this problem
--     you can catch "style-set" event and  query  properties  from  the
--     handler of. For example:
--
--     package Handlers is
--        new Gtk.Handlers.Callback (My_Widget_Record);
--
--     procedure Style_Updated
--               (  Widget : access My_Widget_Record'Class
--               )  is
--        Value : ... := Style_Get (Widget, "my-widget-property");
--     begin
--        ...
--     end "style-set";
--
--     procedure Initialize
--               (  Widget : not null access My_Widget_Record'Class
--               )  is
--     begin
--         ... -- Necessary initialization
--         Handlers.Connect
--         (  Widget,
--            "style-updated",
--            Handlers.To_Marshaller ("style-set"'Access)
--        );
--     end Initialize;
--
--  3. Types of properties. The procedure Style_Get_Property  when  used
--     with  a  custom  type  will  probably not work. To avoid this one
--     should use Install_Style_Property_Parser instead  of  plain
--     Install_Style_Property. The parser then should take care of
--     parsing  the string containing the property in textual form. This
--     feature is used in the child generic package Generic_Enumeration,
--     which allows case-insensitive matching of enumeration values. The
--     parser shall return the parsed value in exactly the type  of  the
--     requested  value. That should circumvent value transformation and
--     thus deliver it back to the caller of Style_Get_Property.
--
with Gdk.Color;             use Gdk.Color;
with GLib.Values;           use GLib.Values;
with Gtk.Widget;            use Gtk.Widget;
with Interfaces.C.Strings;  use Interfaces.C.Strings;

package Gtk.Widget.Styles is
--
-- Param_Spec_Array -- Array of parameter specifications
--
   type Param_Spec_Array is array (Positive range <>) of Param_Spec;
--
-- GString -- Parser's state
--
--    Str           - Nul-terminated string to parse
--    Len           - The length of
--    Allocated_Len - The size of the memory allocated for (max length)
--
   type GString is record
      Str           : Chars_Ptr;
      Len           : GSize;
      Allocated_Len : GSize;
   end record;
   pragma Convention (C, GString);
--
-- Gtk_RC_Property_Parser -- Property parser
--
--    PSpec          - The property specification
--    RC_String      - The property value (unparsed)
--    Property_Value - The propery value to set
--
-- The  parser  scans RC_String.Str and sets Property_Value according to
-- the value detected. To call Init is the parser's responsibility. When
-- it does so and sets a  value  there,  it  returns  1.  Otherwise,  it
-- returns 0.
--
   type Gtk_RC_Property_Parser is access function
        (  PSpec          : Param_Spec;
           RC_String      : GString;
           Property_Value : access GValue
        )  return GBoolean;
   pragma Convention (C, Gtk_RC_Property_Parser);
--
-- Class_Install_Style_Property_Parser -- Add a custom style property
--
--    Class  - The class
--    PSpec  - The property specification
--    Parser - To parse the property
--
-- This procedure installs a style property for which  a  custom  parser
-- will be used.
--
   procedure Class_Install_Style_Property_Parser
             (  Class  : GObject_Class;
                PSpec  : Param_Spec;
                Parser : Gtk_RC_Property_Parser
             );
--
-- Class_List_Style_Properties -- Enumerate style properties
--
--    Class - The class
--
-- This procedure enumerates style properties of a widget's class.
--
-- Returns :
--
--    The array of properties
--
   function Class_List_Style_Properties
            (  Class : GObject_Class
            )  return Param_Spec_Array;
--
-- Get_Path -- Get widget path
--
--    Widget  - To get the path of
--    Reverse - Flag, if the path should be reversed
--
-- The widget path includes the name of the widget set using Set_Name.
--
-- Returns :
--
--    The widget path
--
   function Get_Path
            (  Widget   : not null access Gtk_Widget_Record'Class;
               Reversed : Boolean := False
            )  return UTF8_String;
--
-- Get_Class_Path -- Get widget class path
--
--    Widget  - To get the path of
--    Reverse - Flag, if the path should be reversed
--
-- This is same as Get_Path, but  ignores  Set_Name  and  returns  class
-- names in the path instead of widget names.
--
-- Returns :
--
--    The widget class path
--
   function Get_Class_Path
            (  Widget   : not null access Gtk_Widget_Record'Class;
               Reversed : Boolean := False
            )  return UTF8_String;
--
-- Style_Get -- Get a color from the widget style
--
--    Widget        - To request string from
--    Property_Name - The property name
--    Default       - The default value
--
-- Color style properties are kept as boxed types. It means,  that  they
-- don't have  defaults.  Therefore  this  function  has  an  additional
-- parameter Default.
--
-- Returns :
--
--    The value
--
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String;
               Default       : Gdk_Color
            )  return Gdk_Color;
--
-- Style_Get -- Get a property from the widget style
--
--    Widget        - To request string from
--    Property_Name - The property name
--
-- Returns :
--
--    The value
--
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Boolean;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GChar;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GUChar;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GInt;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GUInt;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GLong;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GULong;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GFloat;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GDouble;
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return UTF8_String;
--
-- Style_Get -- Get widget style by its name
--
--    Widget        - To request string from
--    Property_Name - The property name
--
-- The  result is GValue initialized by the style value. When the widget
-- does not have the style the result has the type GType_None. The value
-- must be freed using Unset when no more used.
--
-- Returns :
--
--    The style value
--
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GValue;
--
-- Generic_Style_Get -- Get style property
--
--    Widget        - To get a style property of
--    Property_Name - The name of the style property
--
-- This  generic  function can be instantiated to get a more comfortable
-- wrapper around Style_Get_Property. The generic parameters are:
--
--    GTK_Type - The GTK type of the property (a function to get it)
--    Ada_Type - The type of the property
--    Get      - The function that gets an Ada_Type value from GValue
--
-- Note  that  the  GTK_Type parameter is a function rather than a plain
-- GType  value.  The reason for this is that the value might be unknown
-- until run-time. This is the case for many  GtkAda  types,  which  are
-- constructed  at run-time. For such types an attempt to get the actual
-- value for GTK_Type will cause creation of a GTK type where it  cannot
-- be  created. That would result in a CRITICAL GTK+ error.
--
-- Returns :
--
--    The value of the property
--
   generic
      with function GTK_Type return GType;
      type Ada_Type (<>) is private;
      with function Get (Value : GValue) return Ada_Type;
   function Generic_Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : String
            )  return Ada_Type;
private
   pragma Import
          (  C,
             Class_Install_Style_Property_Parser,
             "gtk_widget_class_install_style_property_parser"
          );
end Gtk.Widget.Styles;
