--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Thermo_Symmetric                  Luebeck            --
--  Interface                                      Summer, 2012       --
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

with Gdk.Color;                    use Gdk.Color;
with Gtk.Adjustment;               use Gtk.Adjustment;
with Gtk.Enums.String_Lists;       use Gtk.Enums.String_Lists;
with Gtk.Layered;                  use Gtk.Layered;
with Gtk.Layered.Bar;              use Gtk.Layered.Bar;
with Gtk.Layered.Cache;            use Gtk.Layered.Cache;
with Gtk.Layered.Flat_Annotation;  use Gtk.Layered.Flat_Annotation;
with Gtk.Layered.Label;            use Gtk.Layered.Label;
with Gtk.Layered.Line;             use Gtk.Layered.Line;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Widget;                   use Gtk.Widget;

with Gtk.Enums;
with Gtk.Layered.Flat_Scale;

with Gtk.Layered.Rectangular_Background;
use  Gtk.Layered.Rectangular_Background;

package Gtk.Meter.Thermo_Symmetric is
--
-- Class_Name - Of the widget
--
   Class_Name : constant String := "GtkMeterThermoSymmetric";
   Celsius    : constant String := Character'Val (16#C2#) &
                                   Character'Val (16#B0#) & 'C';
--
-- Gtk_Meter_Thermo_Symmetric_Record -- Thermometer with symmetric
--                                      annotation
--
   type Gtk_Meter_Thermo_Symmetric_Record is
      new Gtk_Layered_Record with private;
   type Gtk_Meter_Thermo_Symmetric is
      access all Gtk_Meter_Thermo_Symmetric_Record'Class;
--
-- Get_Type -- The type of the widget
--
-- Returns :
--
--    The GTK type of the widget
--
   function Get_Type return GType;
--
-- Gtk_New -- Widget construction
--
--    Widget      - The result
--    Texts       - The texts to be placed near major ticks of the scale
--  [ Delimiter ] - The delimiter character used in Texts
--    Adjustment  - The adjustment object to indicate
--    Sectors     - The number of intervals between major ticks
--    Left_Label  - The left label text
--    Right_Label - The right label text
--    Color       - The bar color
--
-- Normally there should be Sector + 1 texts in the  list  Texts.  Extra
-- texts are ignored. Missing texts are shown empty.
--
   procedure Gtk_New
             (  Widget      : out Gtk_Meter_Thermo_Symmetric;
                Texts       : Gtk.Enums.String_List.GList;
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 10;
                Left_Label  : String         := Celsius;
                Right_Label : String         := Celsius;
                Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0)
             );
   procedure Gtk_New
             (  Widget      : out Gtk_Meter_Thermo_Symmetric;
                Texts       : Controlled_String_List;
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 10;
                Left_Label  : String         := Celsius;
                Right_Label : String         := Celsius;
                Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0)
             );
   procedure Gtk_New
             (  Widget      : out Gtk_Meter_Thermo_Symmetric;
                Texts       : UTF8_String;
                Delimiter   : Character      := ' ';
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 10;
                Left_Label  : String         := Celsius;
                Right_Label : String         := Celsius;
                Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0)
             );
--
-- Initialize -- The widget initialization
--
--    Widget      - The widget to initialize
--    Texts       - The texts to be placed near major ticks of the scale
--  [ Delimiter ] - The delimiter character used in Texts
--    Adjustment  - The adjustment object to indicate
--    Sectors     - The number of intervals between major ticks
--    Left_Label  - The left label text
--    Right_Label - The right label text
--    Color       - The bar color
--
-- When  a  widget  type  is  derived from this one, it has to call this
-- procedure from its version of Initialize.
--
   procedure Initialize
             (  Widget      : not null access
                              Gtk_Meter_Thermo_Symmetric_Record'Class;
                Texts       : Gtk.Enums.String_List.GList;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive;
                Left_Label  : String;
                Right_Label : String;
                Color       : Gdk_Color
             );
   procedure Initialize
             (  Widget      : not null access
                              Gtk_Meter_Thermo_Symmetric_Record'Class;
                Texts       : Controlled_String_List;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive;
                Left_Label  : String;
                Right_Label : String;
                Color       : Gdk_Color
             );
   procedure Initialize
             (  Widget      : not null access
                              Gtk_Meter_Thermo_Symmetric_Record'Class;
                Texts       : UTF8_String;
                Delimiter   : Character;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive;
                Left_Label  : String;
                Right_Label : String;
                Color       : Gdk_Color
             );
--
-- Get_Background -- The thermometer's background
--
--    Widget - The widget
--
-- Returns :
--
--    The background layer
--
   function Get_Background
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return not null access Rectangular_Background_Layer;
--
-- Get_Bar -- The themperature bar
--
--    Widget - The widget
--
-- Returns :
--
--    The bar layer of the widget
--
   function Get_Bar
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return not null access Bar_Layer;
--
-- Get_Bar_Color -- The themperature bar color
--
--    Widget - The widget
--
-- Returns :
--
--    The color of the bar
--
   function Get_Bar_Color
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return Gdk_Color;
--
-- Get_Cache -- The thermometer's caching layer
--
--    Widget - The widget
--
-- If the widget is extended, static things which do not change with the
-- widget state should be placed below the caching layer for performance
-- reasons.
--
-- Returns :
--
--    The cache layer of the widget
--
   function Get_Cache
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return not null access Cache_Layer;
--
-- Get_Left_Annotation -- The thermometer's annotation on the left
--
--    Widget - The widget
--
-- Returns :
--
--    The annotation layer
--
   function Get_Left_Annotation
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return not null access Flat_Annotation_Layer;
--
-- Get_Left_Label -- The thermometer label on the left
--
--    Widget - The widget
--
-- Returns :
--
--    The label layer of the widget
--
   function Get_Left_Label
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return not null access Label_Layer;
--
-- Get_Right_Annotation -- The thermometer's annotation on the right
--
--    Widget - The widget
--
-- Returns :
--
--    The annotation layer
--
   function Get_Right_Annotation
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return not null access Flat_Annotation_Layer;
--
-- Get_Right_Label -- The thermometer label on the left
--
--    Widget - The widget
--
-- Returns :
--
--    The label layer of the widget
--
   function Get_Right_Label
            (  Widget : not null access
                        Gtk_Meter_Thermo_Symmetric_Record
            )  return not null access Label_Layer;
--
-- Set_Bar_Color -- Set the themperature bar color
--
--    Widget - The widget
--    Color  - The color to set
--
-- Returns :
--
--    The color of the bar
--
   procedure Set_Bar_Color
             (  Widget : not null access
                         Gtk_Meter_Thermo_Symmetric_Record;
                Color  : Gdk_Color
             );
--
-- Set_Value -- Change the value indicated by the thermometer
--
--    Widget - The widget
--    Value  - The value in the range From .. From + Length
--
-- When  the value is out of range it is saturated to the nearest bound.
-- Note  that  procedure does not emit any events, if the widget need to
-- be redrawn the event "draw" should be emitted.
--
   procedure Set_Value
             (  Widget : not null access
                         Gtk_Meter_Thermo_Symmetric_Record;
                Value  : GDouble
             );

   overriding
      procedure Style_Changed
                (  Widget : not null access
                            Gtk_Meter_Thermo_Symmetric_Record
                );

private
   use Gtk.Layered.Cache;
   use Gtk.Layered.Flat_Scale;

   type Temperature_Scale is record
      Minor_Ticks  : access Flat_Scale_Layer;
      Middle_Ticks : access Flat_Scale_Layer;
      Major_Ticks  : access Flat_Scale_Layer;
      Annotation   : access Flat_Annotation_Layer;
      Label        : access Label_Layer;
   end record;

   type Gtk_Meter_Thermo_Symmetric_Record is
      new Gtk_Layered_Record with
   record
      Sectors      : Positive := 10;
      Background   : access Rectangular_Background_Layer;
      Cache        : access Cache_Layer;
      Left_Scale   : Temperature_Scale;
      Right_Scale  : Temperature_Scale;
      Bar          : access Bar_Layer;
      Bulb         : access Line_Layer;
      Reflection   : access Line_Layer;
      Stem         : access Line_Layer;
   end record;

end Gtk.Meter.Thermo_Symmetric;
