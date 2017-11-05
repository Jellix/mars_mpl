--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.LED_Round                         Luebeck            --
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
-- __________________________________________________________________ --

with Cairo;              use Cairo;
with Gdk.Color;          use Gdk.Color;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Layered;        use Gtk.Layered;
with Gtk.Layered.Cache;  use Gtk.Layered.Cache;
with Gtk.Layered.Line;   use Gtk.Layered.Line;
with Gtk.Missed;         use Gtk.Missed;
with Gtk.Widget;         use Gtk.Widget;

with Gtk.Layered.Elliptic_Background;
use  Gtk.Layered.Elliptic_Background;

package Gtk.Gauge.LED_Round is

   pragma Warnings (Off, "declaration hides ""Widget""");

   --
   -- Class_Name - Of the widget
   --
   Class_Name : constant String := "GtkGaugeLEDRound";

   --
   -- Gtk_Gauge_LED_Round -- Round LED
   --
   type Gtk_Gauge_LED_Round_Record is
     new Gtk_Layered_Record with private;
   type Gtk_Gauge_LED_Round is
     access all Gtk_Gauge_LED_Round_Record'Class;

   --
   -- Get_Background -- The LED's background
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The background layer
   --
   function Get_Background
     (Widget : not null access Gtk_Gauge_LED_Round_Record)
      return not null access Elliptic_Background_Layer;

   --
   -- Get_Cache -- The LED's caching layer
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
   function Get_Cache (Widget : not null access Gtk_Gauge_LED_Round_Record)
                       return not null access Cache_Layer;

   --
   -- Get_Off_Color -- The color of turned off LED
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The color
   --
   function Get_Off_Color (Widget : not null access Gtk_Gauge_LED_Round_Record)
                           return Gdk_Color;

   --
   -- Get_On_Color -- The color of turned on LED
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The color
   --
   function Get_On_Color (Widget : not null access Gtk_Gauge_LED_Round_Record)
                          return Gdk_Color;

   --
   -- Get_State -- The LED's state
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The needle layer of the widget
   --
   function Get_State (Widget : not null access Gtk_Gauge_LED_Round_Record)
                       return Boolean;

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
   --    Widget        - The result
   --    On_Color      - The LED's color when on
   --    Off_Color     - The LED's color when off
   --    Border_Shadow - Border shadow type
   --
   procedure Gtk_New (Widget        : out Gtk_Gauge_LED_Round;
                      On_Color      : Gdk_Color := RGB (0.0, 1.0, 0.0);
                      Off_Color     : Gdk_Color := RGB (0.5, 0.5, 0.5);
                      Border_Shadow : Gtk_Shadow_Type := Shadow_In);

   --
   -- Initialize -- The widget initialization
   --
   --    Widget        - The widget to initialize
   --    On_Color      - The LED's color when on
   --    Off_Color     - The LED's color when off
   --    Border_Shadow - Border shadow type
   --
   procedure Initialize
     (Widget        : not null access Gtk_Gauge_LED_Round_Record'Class;
      On_Color      : Gdk_Color;
      Off_Color     : Gdk_Color;
      Border_Shadow : Gtk_Shadow_Type);

   --
   -- Set_Colors -- Change the colors
   --
   --    Widget    - The widget
   --    On_Color  - The LED's color when on
   --    Off_Color - The LED's color when off
   --
   -- Note that changing  state does not refresh the widget.  The operation
   -- is task safe.
   --
   procedure Set_Colors (Widget    : not null access Gtk_Gauge_LED_Round_Record;
                         On_Color  : Gdk_Color;
                         Off_Color : Gdk_Color);

   --
   -- Set_State -- Change the LED status
   --
   --    Widget - The widget
   --    State  - To set
   --
   -- Note that changing  state does not refresh the widget.  The operation
   -- is task safe.
   --
   procedure Set_State (Widget : not null access Gtk_Gauge_LED_Round_Record;
                        State  : Boolean);

   overriding procedure Refresh
     (Widget  : not null access Gtk_Gauge_LED_Round_Record;
      Context : Cairo_Context);

private

   type Gtk_Gauge_LED_Round_Record is
     new Gtk_Layered_Record with
      record
         Background : access Elliptic_Background_Layer;
         Cache      : access Cache_Layer;
         Reflection : access Line_Layer;
         Shadow     : access Line_Layer;
         State      : Boolean := False;
         Toggled    : Boolean := False;
         On         : Gdk_Color;
         Off        : Gdk_Color;
         pragma Atomic (State);
         pragma Atomic (Toggled);
      end record;

   procedure Update_State (Widget : not null access Gtk_Gauge_LED_Round_Record);

   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Gauge.LED_Round;
