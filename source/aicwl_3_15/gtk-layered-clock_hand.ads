--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Clock_Hand                      Luebeck            --
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
-- __________________________________________________________________ --

with Ada.Numerics;
with Gtk.Handlers;
with Gtk.Missed;

package Gtk.Layered.Clock_Hand is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Center""");

   --
   -- Clock_Hand_Layer -- A clock hand
   --
   type Clock_Hand_Layer (<>) is
     new Abstract_Layer
     and Gauge_Needle
     and Scalable_Layer with private;

   --
   -- Add_Clock_Hand -- Add a clock hand
   --
   --    Under         - The layer or widget where to place the arc under
   --    Center        - The needle rotation center
   --    From          - The angle (position) of the lowest value
   --    Length        - The angular length of the values range
   --    Tip_Length    - The distance to the center (can be negative)
   --    Tip_Width     - The width at the needle tip
   --    Tip_Cap       - The style of needle ending
   --    Rear_Length   - The distance to the center (can be negative)
   --    Rear_Width    - The width at the needle tip
   --    Rear_Cap      - The style of needle ending
   --    Bulb_Radius   - The radius of the bulb
   --    Bulb_Position - The location of the bulb (from the center)
   --    Bulb_Width    - The buld line width
   --    Color         - The needle color
   --    Adjustment    - The value source
   --    Scaled        - The layer is scaled together with the widget
   --
   -- When Length is positive the needle moves clockwise, otherwise it  does
   -- counterclockwise.  When  Adjustment is not null the needle moves each
   -- time the adjustment is changed. Note that it also redraws the layered
   -- widget  it  belongs  to. For complex widgets it is not recommended to
   -- use  adjustment  and  event  controlled  layered   widgets.   As   an
   -- alternative  consider  using  Set_Value  instead  and  redrawing  the
   -- layered widget periodically independently on the value state.  When
   -- Scaled is true the needle is scaled  to fit the  parent  widget.  The
   -- scaling of needle is performed as follows:
   --
   -- (o)  The center's X is multiplied by the widget's width and placed in
   --      the coorinate system centered in the widget's center;
   -- (o)  The  center's  Y is multiplied by the widget's height and placed
   --      in the coorinate system centered in the widget's center;
   -- (o)  The  needle's length and width are multiplied the minimum of the
   --      widget's height and length.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Clock_Hand
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      From          : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 4.0;
      Length        : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 2.0;
      Tip_Length    : Gdouble                                           := 20.0;
      Tip_Width     : Gdouble                                           := 2.0;
      Tip_Cap       : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length   : Gdouble                                           := 3.0;
      Rear_Width    : Gdouble                                           := 3.0;
      Rear_Cap      : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Bulb_Position : Gdouble                                           := 13.0;
      Bulb_Radius   : Gdouble                                           := 5.0;
      Bulb_Width    : Gdouble                                           := 2.0;
      Color         : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment    : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled        : Boolean                                           := False);

   function Add_Clock_Hand
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      From          : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 4.0;
      Length        : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 2.0;
      Tip_Length    : Gdouble                                           := 20.0;
      Tip_Width     : Gdouble                                           := 2.0;
      Tip_Cap       : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length   : Gdouble                                           := 3.0;
      Rear_Width    : Gdouble                                           := 3.0;
      Rear_Cap      : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Bulb_Position : Gdouble                                           := 13.0;
      Bulb_Radius   : Gdouble                                           := 5.0;
      Bulb_Width    : Gdouble                                           := 2.0;
      Color         : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment    : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled        : Boolean                                           := False)
      return not null access Clock_Hand_Layer;

   --
   -- Get_Bulb_Position -- The position of the bulb
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The position of the bulb
   --
   function Get_Bulb_Position (Layer : Clock_Hand_Layer)
                               return Gdouble;

   --
   -- Get_Bulb_Radius -- The bulb radius
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The radius
   --
   function Get_Bulb_Radius (Layer : Clock_Hand_Layer) return Gdouble;

   --
   -- Get_Bulb_Width -- The bulb line width
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The radius
   --
   function Get_Bulb_Width (Layer : Clock_Hand_Layer) return Gdouble;

   --
   -- Get_Center -- Center of the needle
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The rotation center of the needle
   --
   function Get_Center (Layer : Clock_Hand_Layer)
                        return Cairo.Ellipses.Cairo_Tuple;

   --
   -- Get_Color -- The needle color
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The needle's color
   --
   function Get_Color (Layer : Clock_Hand_Layer) return Gdk.Color.Gdk_Color;

   --
   -- Get_From -- The angle (position) of the lowest value
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The angle
   --
   function Get_From (Layer : Clock_Hand_Layer) return Gdouble;

   --
   -- Get_Length -- The angular length of the needle positions
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The angle
   --
   function Get_Length (Layer : Clock_Hand_Layer) return Gdouble;

   --
   -- Get_Rear -- The parameters of the needle's rear end
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The parameters of the needle's rear end
   --
   function Get_Rear (Layer : Clock_Hand_Layer) return End_Parameters;

   --
   -- Get_Tip -- The parameters of the needle's tip
   --
   --    Layer - The needle
   --
   -- Returns :
   --
   --    The parameters of the needle's tip
   --
   function Get_Tip (Layer : Clock_Hand_Layer) return End_Parameters;

   --
   -- Set -- Parameters of the arc
   --
   --    Layer         - The needle
   --    Center        - The needle rotation center
   --    From          - The angle (position) of the lowest value
   --    Length        - The angular length of the values range
   --    Tip           - The needle's tip parameters
   --    Rear          - The needle's rear end parameters
   --    Bulb_Radius   - The radius of the bulb
   --    Bulb_Position - The location of the bulb (from the center)
   --    Bulb_Width    - The buld line width
   --    Color         - The needle color
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Set
     (Layer         : in out Clock_Hand_Layer;
      Center        : Cairo.Ellipses.Cairo_Tuple;
      From          : Gdouble;
      Length        : Gdouble;
      Tip           : End_Parameters;
      Rear          : End_Parameters;
      Bulb_Position : Gdouble;
      Bulb_Radius   : Gdouble;
      Bulb_Width    : Gdouble;
      Color         : Gdk.Color.Gdk_Color);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Clock_Hand_Layer;

   overriding procedure Draw
     (Layer   : in out Clock_Hand_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle);

   overriding function Get_Adjustment
     (Layer : Clock_Hand_Layer) return Gtk.Adjustment.Gtk_Adjustment;

   overriding function Get_Properties_Number
     (Layer : Clock_Hand_Layer) return Natural;

   overriding function Get_Property_Specification
     (Layer    : Clock_Hand_Layer;
      Property : Positive) return Param_Spec;

   overriding function Get_Property_Value
     (Layer    : Clock_Hand_Layer;
      Property : Positive) return Glib.Values.GValue;

   overriding function Get_Scaled (Layer : Clock_Hand_Layer) return Boolean;

   overriding procedure Finalize (Layer : in out Clock_Hand_Layer);

   overriding function Get_Value (Layer : Clock_Hand_Layer) return Gdouble;

   overriding function Is_Updated (Layer : Clock_Hand_Layer) return Boolean;

   overriding procedure Move
     (Layer  : in out Clock_Hand_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple);

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Clock_Hand_Layer);

   overriding procedure Set_Property_Value
     (Layer    : in out Clock_Hand_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue);

   overriding procedure Scale
     (Layer  : in out Clock_Hand_Layer;
      Factor : Gdouble);

   overriding procedure Set_Scaled
     (Layer  : in out Clock_Hand_Layer;
      Scaled : Boolean);

   overriding procedure Set_Value
     (Layer : in out Clock_Hand_Layer;
      Value : Gdouble);

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Clock_Hand_Layer);

private

   type Clock_Hand_Layer is
     new Abstract_Layer
     and Gauge_Needle
     and Scalable_Layer with
      record
         Center        : Cairo.Ellipses.Cairo_Tuple;
         From          : Gdouble;
         Length        : Gdouble;
         Value         : Gdouble := 0.0;
         Tip           : End_Parameters;
         Rear          : End_Parameters;
         Bulb_Position : Gdouble;
         Bulb_Radius   : Gdouble;
         Bulb_Width    : Gdouble;
         Color         : Gdk.Color.Gdk_Color;
         Adjustment    : Gtk.Adjustment.Gtk_Adjustment;
         Changed       : Gtk.Handlers.Handler_Id;
         Value_Changed : Gtk.Handlers.Handler_Id;
         Scaled        : Boolean := False;
         Updated       : Boolean := True;
         pragma Atomic (Value);
         pragma Atomic (Updated);
      end record;

   pragma Warnings (On, "declaration hides ""Center""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Layered.Clock_Hand;
