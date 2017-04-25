--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Flat_Scale                      Luebeck            --
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

package Gtk.Layered.Flat_Scale is
--
-- Flat_Scale_Layer -- Layer showing ticks of a flat  gauge.  The  ticks
--                     are   drawn   from  starting  from  the  position
-- specified  by  the  parameter  From  along  the  line  which angle is
-- specified   by   the   parameter   Rotation_Angle.  Ticks  are  drawn
-- perpendicular to the line. The ticks start at the from angle and  end
-- at the to angle. The ticks are numbered 1,2,3 until the skipped tick,
-- which is not drawn. The next tick has again the number 1.  The  first
-- tick at the from angle has the specified number.
--
   type Flat_Scale_Layer (<>) is
      new Abstract_Layer
      and Scalable_Layer
      and Widened_Layer with private;
--
-- Add_Flat_Scale -- Add scale ticks
--
--    Under    - The layer or widget where to place it under
--    Step     - The tick step
--    First    - The position of the first tick
--    Skipped  - The position of skipped ticks
--    From     - Coordinates of the first tick
--    Length   - The scale length
--    Breadth  - The tick length
--    Angle    - The angle of the scale (perpendicular to ticks)
--    Width    - The tick line width
--    Color    - The tick line color
--    Line_Cap - Line cap style
--    Scaled   - The layer is scaled together with the parent
--    Widened  - The layer's line is widened together with the widget
--
-- The layer scaling is performed as follows:
--
-- (o)  The first tick position X is multiplied by the widget's size and
--      placed in the coordinate system centered in the widget's center;
-- (o)  The first tick position Y is multiplied by the widget's size and
--      placed in the coordinate system centered in the widget's center;
-- (o)  Length, width, step are multiplied by the widget's size.
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Flat_Scale
             (  Under    : not null access Layer_Location'Class;
                Step     : GDouble;
                First    : Tick_Number    := Tick_Number'Last;
                Skipped  : Tick_Number    := Tick_Number'Last;
                From     : Cairo_Tuple    := (0.0, 0.0);
                Length   : GDouble        := 1.0;
                Breadth  : GDouble        := 1.0;
                Angle    : GDouble        := 0.0;
                Width    : GDouble        := 1.0;
                Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
                Line_Cap : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
                Scaled   : Boolean        := False;
                Widened  : Boolean        := False
             );
   function Add_Flat_Scale
            (  Under    : not null access Layer_Location'Class;
               Step     : GDouble;
               First    : Tick_Number    := Tick_Number'Last;
               Skipped  : Tick_Number    := Tick_Number'Last;
               From     : Cairo_Tuple    := (0.0, 0.0);
               Length   : GDouble        := 1.0;
               Breadth  : GDouble        := 1.0;
               Angle    : GDouble        := 0.0;
               Width    : GDouble        := 1.0;
               Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
               Line_Cap : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
               Scaled   : Boolean        := False;
               Widened  : Boolean        := False
            )  return not null access Flat_Scale_Layer;
--
-- Get_Angle -- The rotation angle
--
--    Layer - The scale layer
--
-- Returns :
--
--    The scale angle
--
   function Get_Angle (Layer : Flat_Scale_Layer) return GDouble;
--
-- Get_Breadth -- The tick length
--
--    Layer  - The scale layer
--
-- Returns :
--
--    The tick length
--
   function Get_Breadth (Layer : Flat_Scale_Layer) return GDouble;
--
-- Get_From -- Position of the scale beginning
--
--    Layer - The scale layer
--
-- Returns :
--
--    The first tick position
--
   function Get_From (Layer : Flat_Scale_Layer) return Cairo_Tuple;
--
-- Get_Length -- The length of the scale
--
--    Layer - The scale layer
--
-- Returns :
--
--    The scale length
--
   function Get_Length (Layer : Flat_Scale_Layer) return GDouble;
--
-- Get_Line -- Ticks line parameters
--
--    Layer - The scale layer
--
-- Returns :
--
--    The line parameters
--
   function Get_Line (Layer : Flat_Scale_Layer) return Line_Parameters;
--
-- Get_Ticks -- The parameters of the scale
--
--    Layer - The scale layer
--
-- Returns :
--
--    The parameters of the scale
--
   function Get_Ticks (Layer : Flat_Scale_Layer)
      return Tick_Parameters;
--
-- Set -- Parameters of the scale
--
--    Layer   - The scale layer
--    Line    - The tick lines parameters
--    Ticks   - The parameters of the scale ticks
--    From    - Coordinates of the first tick
--    Length  - The scale length
--    Breadth - The tick length
--    Angle   - The angle of the scale (perpendicular to ticks)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer   : in out Flat_Scale_Layer;
                Line    : Line_Parameters;
                Ticks   : Tick_Parameters;
                From    : Cairo_Tuple;
                Length  : GDouble;
                Breadth : GDouble;
                Angle   : GDouble
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Flat_Scale_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Flat_Scale_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      function Get_Scaled (Layer : Flat_Scale_Layer) return Boolean;
   overriding
      function Get_Properties_Number
               (  Layer : Flat_Scale_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Flat_Scale_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Flat_Scale_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Widened (Layer : Flat_Scale_Layer) return Boolean;
   overriding
      function Is_Updated (Layer : Flat_Scale_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Flat_Scale_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Flat_Scale_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Flat_Scale_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Flat_Scale_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Flat_Scale_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Widened
                (  Layer   : in out Flat_Scale_Layer;
                   Widened : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Flat_Scale_Layer
                );
private
   type Flat_Scale_Layer is
      new Abstract_Layer and Scalable_Layer and Widened_Layer with
   record
      Ticks   : Tick_Parameters;
      Line    : Line_Parameters;
      From    : Cairo_Tuple;
      Length  : GDouble;
      Breadth : GDouble;
      Angle   : GDouble;
      Scaled  : Boolean := False;
      Widened : Boolean := False;
      Updated : Boolean := True;
   end record;

end Gtk.Layered.Flat_Scale;
