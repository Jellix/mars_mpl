--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Round_254                         Luebeck            --
--  Interface                                      Winter, 2010       --
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

with Cairo;                   use Cairo;
with Gdk.Color;               use Gdk.Color;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Enums.String_Lists;  use Gtk.Enums.String_Lists;
with Gtk.Layered;             use Gtk.Layered;
with Gtk.Layered.Needle;      use Gtk.Layered.Needle;
with Gtk.Layered.Cache;       use Gtk.Layered.Cache;
with Gtk.Widget;              use Gtk.Widget;

with Gtk.Enums;
with Gtk.Layered.Cap;
with Gtk.Layered.Elliptic_Scale;

with Gtk.Layered.Elliptic_Annotation;
use  Gtk.Layered.Elliptic_Annotation;

with Gtk.Layered.Elliptic_Background;
use  Gtk.Layered.Elliptic_Background;

package Gtk.Gauge.Round_254 is
--
-- Class_Name - Of the widget
--
   Class_Name : constant String := "GtkGaugeRound254";
--
-- Gtk_Gauge_Round_254 -- Round 254 degrees gauge
--
   type Gtk_Gauge_Round_254_Record is
      new Gtk_Layered_Record with private;
   type Gtk_Gauge_Round_254 is
      access all Gtk_Gauge_Round_254_Record'Class;
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
--    Major_Texts - The texts to be placed near major ticks of the scale
--    Minor_Texts - The texts to be placed near minor ticks of the scale
--  [ Delimiter ] - The delimiter character used in Texts
--    Adjustment  - The adjustment object to indicate
--    Sectors     - The number of intervals between major ticks
--
-- Normally there should be Sector + 1 texts in the  list  Texts.  Extra
-- texts are ignored. Missing texts are shown empty.
--
   procedure Gtk_New
             (  Widget      : out Gtk_Gauge_Round_254;
                Major_Texts : Gtk.Enums.String_List.GList;
                Minor_Texts : Gtk.Enums.String_List.GList;
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 17
             );
   procedure Gtk_New
             (  Widget      : out Gtk_Gauge_Round_254;
                Major_Texts : Controlled_String_List;
                Minor_Texts : Controlled_String_List;
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 17
             );
   procedure Gtk_New
             (  Widget      : out Gtk_Gauge_Round_254;
                Major_Texts : UTF8_String;
                Minor_Texts : UTF8_String;
                Delimiter   : Character      := ' ';
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 17
             );
--
-- Initialize -- The widget initialization
--
--    Widget      - The widget to initialize
--    Major_Texts - The texts to be placed near major ticks of the scale
--    Minor_Texts - The texts to be placed near minor ticks of the scale
--  [ Delimiter ] - The delimiter character used in Texts
--    Adjustment  - The adjustment object to indicate
--    Sectors     - The number of intervals between major ticks
--
-- When  a  widget  type  is  derived from this one, it has to call this
-- procedure from its version of Initialize.
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record'Class;
                Major_Texts : Gtk.Enums.String_List.GList;
                Minor_Texts : Gtk.Enums.String_List.GList;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive
             );
   procedure Initialize
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record'Class;
                Major_Texts : Controlled_String_List;
                Minor_Texts : Controlled_String_List;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive
             );
   procedure Initialize
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record'Class;
                Major_Texts : UTF8_String;
                Minor_Texts : UTF8_String;
                Delimiter   : Character;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive
             );
--
-- Get_Needle -- The gauge needle
--
--    Widget - The widget
--
-- Returns :
--
--    The needle layer of the widget
--
   function Get_Needle
            (  Widget : not null access
                        Gtk_Gauge_Round_254_Record
            )  return not null access Needle_Layer;
--
-- Get_Background -- The gauge's background
--
--    Widget - The widget
--
-- Returns :
--
--    The background layer
--
   function Get_Background
            (  Widget : not null access Gtk_Gauge_Round_254_Record
            )  return not null access Elliptic_Background_Layer;
--
-- Get_Cache -- The gauge caching layer
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
            (  Widget : not null access Gtk_Gauge_Round_254_Record
            )  return not null access Cache_Layer;
--
-- Get_Major_Annotation -- The gauge major ticks annotation
--
--    Widget - The widget
--
-- Returns :
--
--    The annotation layer
--
   function Get_Major_Annotation
            (  Widget : not null access
                        Gtk_Gauge_Round_254_Record
            )  return not null access Elliptic_Annotation_Layer;
--
-- Get_Minor_Annotation -- The gauge minor ticks annotation
--
--    Widget - The widget
--
-- Returns :
--
--    The annotation layer
--
   function Get_Minor_Annotation
            (  Widget : not null access
                        Gtk_Gauge_Round_254_Record
            )  return not null access Elliptic_Annotation_Layer;
--
-- Set_Value -- Change the value indicated by the gauge
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
                         Gtk_Gauge_Round_254_Record;
                Value  : GDouble
             );

   overriding
      procedure Style_Changed
                (  Widget : not null access
                            Gtk_Gauge_Round_254_Record
                );

private
   use Gtk.Layered.Cap;
   use Gtk.Layered.Cache;
   use Gtk.Layered.Elliptic_Scale;

   type Gtk_Gauge_Round_254_Record is
      new Gtk_Layered_Record with
   record
      Sectors          : Positive := 17;
      Background       : access Elliptic_Background_Layer;
      Cache            : access Cache_Layer;
      Pin              : access Cap_Layer;
      Minor_Ticks      : access Elliptic_Scale_Layer;
      Major_Ticks      : access Elliptic_Scale_Layer;
      Major_Annotation : access Elliptic_Annotation_Layer;
      Minor_Annotation : access Elliptic_Annotation_Layer;
      Needle           : access Needle_Layer;
   end record;
end Gtk.Gauge.Round_254;
