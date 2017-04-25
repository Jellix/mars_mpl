--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Scale                  Luebeck            --
--  Interface                                      Autumn, 2010       --
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

with Ada.Numerics;  use Ada.Numerics;

package Gtk.Layered.Elliptic_Scale is
--
-- Elliptic_Scale_Layer -- Layer showing ticks of a gauge. The ticks are
--                         drawn from  inner to outer bounding ellipses.
-- Both ellipses have the same center. The ticks start at the from angle
-- and  end  at  the  to  angle.  The ticks are numbered 1,2,3 until the
-- skipped tick, which is not drawn. The next tick has again the  number
-- 1. The first tick at the from angle has the specified number.
--
   type Elliptic_Scale_Layer (<>) is
      new Abstract_Layer
      and Scalable_Layer
      and Widened_Layer with private;
--
-- Add_Elliptic_Scale -- Add scale ticks
--
--    Under    - The layer or widget where to place the scale under
--    Step     - The tick step (angle)
--    First    - The position of the first tick
--    Skipped  - The position of skipped ticks
--    Outer    - The outer ellipse bounding the scale ticks
--    Inner    - The inner ellipse bounding the scale ticks
--    From     - The angle where the bounding ellipse arc begins
--    Length   - The angular length of the arc
--    Width    - The tick line width
--    Color    - The tick line color
--    Line_Cap - Line cap style
--    Scaled   - The layer is scaled together with the parent widget
--    Widened  - The layer's line is widened together with the widget
--
-- The  center  coordinates  of  the  inner  ellipse  are  ignored.  The
-- parameter  Length  specifies  the  angular  length of the scale. When
-- length  is  positive  the  scale  goes  clockwise.  Otherwise it does
-- counterclockwise. When Scaled is true the scale's arc  is  scaled  to
-- fit the parent widget. The scaling is performed as follows:
--
-- (o)  The arc center's X is multiplied by the widget's size and placed
--      in the coorinate system centered in the widget's center;
-- (o)  The arc center's Y is multiplied by the widget's size and placed
--      in the coorinate system centered in the widget's center;
-- (o)  The arc major axis curvature is divided by the widget's size;
-- (o)  The arc minor axis radius is multiplied by the widget's size.
--
-- When the parameter Widened is true the line width  is scaled together
-- with the widget.
--
   procedure Add_Elliptic_Scale
             (  Under    : not null access Layer_Location'Class;
                Step     : GDouble;
                First    : Tick_Number        := Tick_Number'Last;
                Skipped  : Tick_Number        := Tick_Number'Last;
                Outer    : Ellipse_Parameters := Unit_Circle;
                Inner    : Ellipse_Parameters := Unit_Circle / 2.0;
                From     : GDouble            := 0.0;
                Length   : GDouble            := 2.0 * Pi;
                Width    : GDouble            := 1.0;
                Color    : Gdk_Color          := RGB (0.0, 0.0, 0.0);
                Line_Cap : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
                Scaled   : Boolean            := False;
                Widened  : Boolean            := False
             );
   function Add_Elliptic_Scale
            (  Under    : not null access Layer_Location'Class;
               Step     : GDouble;
               First    : Tick_Number        := Tick_Number'Last;
               Skipped  : Tick_Number        := Tick_Number'Last;
               Outer    : Ellipse_Parameters := Unit_Circle;
               Inner    : Ellipse_Parameters := Unit_Circle / 2.0;
               From     : GDouble            := 0.0;
               Length   : GDouble            := 2.0 * Pi;
               Width    : GDouble            := 1.0;
               Color    : Gdk_Color          := RGB (0.0, 0.0, 0.0);
               Line_Cap : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
               Scaled   : Boolean            := False;
               Widened  : Boolean            := False
            )  return not null access Elliptic_Scale_Layer;
--
-- Get_From -- Angle of the scale beginning
--
--    Layer - The scale layer
--
-- Returns :
--
--    The angle
--
   function Get_From (Layer : Elliptic_Scale_Layer) return GDouble;
--
-- Get_Inner -- The parameters of the inner ellipse bounding ticks
--
--    Layer - The scale layer
--
-- Returns :
--
--    The parameters of the elliptic arc
--
   function Get_Inner (Layer : Elliptic_Scale_Layer)
      return Ellipse_Parameters;
--
-- Get_Length -- Angular length of the scale
--
--    Layer - The scale layer
--
-- Returns :
--
--    The angle
--
   function Get_Length (Layer : Elliptic_Scale_Layer) return GDouble;
--
-- Get_Line -- Ticks line parameters
--
--    Layer - The scale layer
--
-- Returns :
--
--    The line parameters
--
   function Get_Line (Layer : Elliptic_Scale_Layer)
      return Line_Parameters;
--
-- Get_Outer -- The parameters of the outer ellipse bounding ticks
--
--    Layer - The scale layer
--
-- Returns :
--
--    The parameters of the elliptic arc
--
   function Get_Outer (Layer : Elliptic_Scale_Layer)
      return Ellipse_Parameters;
--
-- Get_Ticks -- The parameters of the scale
--
--    Layer - The scale layer
--
-- Returns :
--
--    The parameters of the scale
--
   function Get_Ticks (Layer : Elliptic_Scale_Layer)
      return Tick_Parameters;
--
-- Set -- Parameters of the scale
--
--    Layer  - The scale layer
--    Outer  - The outer ellipse bound of the layer
--    Inner  - The inner ellipse bound of the layer
--    Line   - The tick lines parameters
--    Ticks  - The parameters of the scale ticks
--    From   - The angle where the elliptic arcs begins
--    Length - The angular length of the arcs
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer  : in out Elliptic_Scale_Layer;
                Outer  : Ellipse_Parameters;
                Inner  : Ellipse_Parameters;
                Line   : Line_Parameters;
                Ticks  : Tick_Parameters;
                From   : GDouble;
                Length : GDouble
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Elliptic_Scale_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Elliptic_Scale_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      function Get_Properties_Number
               (  Layer : Elliptic_Scale_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Elliptic_Scale_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Elliptic_Scale_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Elliptic_Scale_Layer) return Boolean;
   overriding
      function Get_Widened
               (  Layer : Elliptic_Scale_Layer
               )  return Boolean;
   overriding
      function Is_Updated (Layer : Elliptic_Scale_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Elliptic_Scale_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Elliptic_Scale_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Elliptic_Scale_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Elliptic_Scale_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Elliptic_Scale_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Widened
                (  Layer   : in out Elliptic_Scale_Layer;
                   Widened : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Elliptic_Scale_Layer
                );
private
   type Elliptic_Scale_Layer is
      new Abstract_Layer and Scalable_Layer and Widened_Layer with
   record
      Outer   : Ellipse_Parameters;
      Inner   : Ellipse_Parameters;
      Ticks   : Tick_Parameters;
      Line    : Line_Parameters;
      From    : GDouble;
      Length  : GDouble;
      Scaled  : Boolean := False;
      Widened : Boolean := False;
      Updated : Boolean := True;
   end record;

end Gtk.Layered.Elliptic_Scale;
