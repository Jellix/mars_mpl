--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Flat_Needle                     Luebeck            --
--  Interface                                      Winter, 2011       --
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

with Gtk.Handlers;  use Gtk.Handlers;

package Gtk.Layered.Flat_Needle is
--
-- Flat_Needle_Layer -- A needle  both ends of  which  are moving  along
--                      parallel lines
--
   type Flat_Needle_Layer (<>) is
      new Abstract_Layer
      and Gauge_Needle
      and Scalable_Layer with private;
--
-- Add_Flat_Needle -- Add needle
--
--    Under       - The layer or widget where to place the arc under
--    From        - The position of the lowest value
--  [ Angle, Length | To ] - The angle and length or the end point
--    Tip_Length  - The distance to the center (can be negative)
--    Tip_Width   - The width at the needle tip
--    Tip_Cap     - The style of needle ending
--    Rear_Length - The distance to the center (can be negative)
--    Rear_Width  - The width at the needle tip
--    Rear_Cap    - The style of needle ending
--    Color       - The needle color
--    Adjustment  - The value source
--    Scaled      - The layer is scaled together with the parent widget
--
-- When Adjustment is not null the needle moves each time the adjustment
-- is changed. Note that it also redraws the layered widget  it  belongs
-- to. For complex widgets it is not recommended to use  adjustment  and
-- event controlled layered widgets. As an  alternative  consider  using
-- Set_Value instead  and  redrawing  the  layered  widget  periodically
-- independently on the value state. When Scaled is true the  needle  is
-- scaled to fit the parent widget. The scaling of needle  is  performed
-- as follows:
--
-- (o)  The points coordinates are multiplied  by the widget's  size and
--      placed in the coorinate system centered in the widget's center;
-- (o)  Length is multiplied by  the widget's size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Flat_Needle
             (  Under       : not null access Layer_Location'Class;
                From        : Cairo_Tuple    := (0.0, 0.0);
                Angle       : Gdouble        := 0.0;
                Length      : Gdouble        := 1.0;
                Tip_Length  : Gdouble        := 20.0;
                Tip_Width   : Gdouble        := 2.0;
                Tip_Cap     : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
                Rear_Length : Gdouble        := 3.0;
                Rear_Width  : Gdouble        := 3.0;
                Rear_Cap    : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
                Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0);
                Adjustment  : access Gtk_Adjustment_Record'Class :=
                                     null;
                Scaled      : Boolean        := False
             );
   function Add_Flat_Needle
            (  Under       : not null access Layer_Location'Class;
               From        : Cairo_Tuple    := (0.0, 0.0);
               Angle       : Gdouble        := 0.0;
               Length      : Gdouble        := 1.0;
               Tip_Length  : Gdouble        := 20.0;
               Tip_Width   : Gdouble        := 2.0;
               Tip_Cap     : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
               Rear_Length : Gdouble        := 3.0;
               Rear_Width  : Gdouble        := 3.0;
               Rear_Cap    : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
               Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0);
               Adjustment  : access Gtk_Adjustment_Record'Class := null;
               Scaled      : Boolean        := False
            )  return not null access Flat_Needle_Layer;
   procedure Add_Flat_Needle
             (  Under       : not null access Layer_Location'Class;
                From        : Cairo_Tuple    := (0.0, 0.0);
                To          : Cairo_Tuple    := (0.0, 1.0);
                Tip_Length  : Gdouble        := 20.0;
                Tip_Width   : Gdouble        := 2.0;
                Tip_Cap     : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
                Rear_Length : Gdouble        := 3.0;
                Rear_Width  : Gdouble        := 3.0;
                Rear_Cap    : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
                Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0);
                Adjustment  : access Gtk_Adjustment_Record'Class :=
                                     null;
                Scaled      : Boolean        := False
             );
   function Add_Flat_Needle
            (  Under       : not null access Layer_Location'Class;
               From        : Cairo_Tuple    := (0.0, 0.0);
               To          : Cairo_Tuple    := (0.0, 1.0);
               Tip_Length  : Gdouble        := 20.0;
               Tip_Width   : Gdouble        := 2.0;
               Tip_Cap     : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
               Rear_Length : Gdouble        := 3.0;
               Rear_Width  : Gdouble        := 3.0;
               Rear_Cap    : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
               Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0);
               Adjustment  : access Gtk_Adjustment_Record'Class := null;
               Scaled      : Boolean        := False
            )  return not null access Flat_Needle_Layer;
--
-- Get_Angle -- The needle path angle
--
--    Layer - The line layer
--
-- Returns :
--
--    The line angle along which the needle moves
--
   function Get_Angle (Layer : Flat_Needle_Layer) return Gdouble;
--
-- Get_Color -- The needle color
--
--    Layer - The needle
--
-- Returns :
--
--    The needle's color
--
   function Get_Color (Layer : Flat_Needle_Layer) return Gdk_Color;
--
-- Get_From -- The position of the lowest value
--
--    Layer - The needle
--
-- Returns :
--
--    The position of the lowest value
--
   function Get_From (Layer : Flat_Needle_Layer) return Cairo_Tuple;
--
-- Get_Length -- The length of the needle positions
--
--    Layer - The needle
--
-- Returns :
--
--    The angle
--
   function Get_Length (Layer : Flat_Needle_Layer) return Gdouble;
--
-- Get_Rear -- The parameters of the needle's rear end
--
--    Layer - The needle
--
-- Returns :
--
--    The parameters of the needle's rear end
--
   function Get_Rear (Layer : Flat_Needle_Layer) return End_Parameters;
--
-- Get_Tip -- The parameters of the needle's tip
--
--    Layer - The needle
--
-- Returns :
--
--    The parameters of the needle's tip
--
   function Get_Tip (Layer : Flat_Needle_Layer) return End_Parameters;
--
-- Get_To -- The position of the highest value
--
--    Layer - The needle
--
-- Returns :
--
--    The position of the highest value
--
   function Get_To (Layer : Flat_Needle_Layer) return Cairo_Tuple;
--
-- Set -- Parameters of the needle
--
--    Layer - The needle
--    From  - The position of the lowest value
--  [ Angle, Length | To ] - The angle and length or the end point
--    Tip   - The needle's tip parameters
--    Rear  - The needle's rear end parameters
--    Color - The needle color
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer  : in out Flat_Needle_Layer;
                From   : Cairo_Tuple;
                Angle  : Gdouble;
                Length : Gdouble;
                Tip    : End_Parameters;
                Rear   : End_Parameters;
                Color  : Gdk_Color
             );
   procedure Set
             (  Layer : in out Flat_Needle_Layer;
                From  : Cairo_Tuple;
                To    : Cairo_Tuple;
                Tip   : End_Parameters;
                Rear  : End_Parameters;
                Color : Gdk_Color
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Flat_Needle_Layer;
   overriding
      function Get_Adjustment (Layer : Flat_Needle_Layer)
         return Gtk_Adjustment;
   overriding
      procedure Draw
                (  Layer   : in out Flat_Needle_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding procedure Finalize (Layer : in out Flat_Needle_Layer);
   overriding
      function Get_Properties_Number
               (  Layer : Flat_Needle_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Flat_Needle_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Flat_Needle_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Flat_Needle_Layer) return Boolean;
   overriding
      function Get_Value (Layer : Flat_Needle_Layer) return Gdouble;
   overriding
      function Is_Updated (Layer : Flat_Needle_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Flat_Needle_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Flat_Needle_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Flat_Needle_Layer;
                   Factor : Gdouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Flat_Needle_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Flat_Needle_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Value
                (  Layer : in out Flat_Needle_Layer;
                   Value : Gdouble
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Flat_Needle_Layer
                );
private
   type Flat_Needle_Layer is
      new Abstract_Layer and Gauge_Needle and Scalable_Layer with
   record
      From          : Cairo_Tuple;
      To            : Cairo_Tuple;
      Angle         : Gdouble := 0.0;
      Value         : Gdouble := 0.0;
      Tip           : End_Parameters;
      Rear          : End_Parameters;
      Color         : Gdk_Color;
      Adjustment    : Gtk_Adjustment;
      Changed       : Handler_Id;
      Value_Changed : Handler_Id;
      Scaled        : Boolean := False;
      Updated       : Boolean := True;
      pragma Atomic (Value);
      pragma Atomic (Updated);
   end record;

end Gtk.Layered.Flat_Needle;
