--                                                                    --
--  package Gtk.Layered.Line        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2011       --
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

package Gtk.Layered.Line is
--
-- Line_Layer -- A layer shaped as a line
--
   type Line_Layer (<>) is
      new Abstract_Layer
      and Scalable_Layer
      and Widened_Layer with private;
--
-- Add_Line -- Add line
--
--    Under    - The layer or widget where to place the line under
--    From     - The point where the line begins
--  [ Angle, Length | To ] - The line angle and length or end point
--    Width    - The line width
--    Color    - The line color
--    Line_Cap - The way lines end
--    Scaled   - The layer is scaled together with the parent widget
--    Widened  - The layer's line is widened together with the widget
--
-- The scaling is performed as follows:
--
-- (o)  The point  coordinates are multiplied  by the widget's  size and
--      placed in the coorinate system centered in the widget's center;
-- (o)  Length is multiplied by the widget's size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Line
             (  Under    : not null access Layer_Location'Class;
                From     : Cairo_Tuple    := (0.0, 0.0);
                Angle    : Gdouble        := 0.0;
                Length   : Gdouble        := 1.0;
                Width    : Gdouble        := 1.0;
                Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
                Line_Cap : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
                Scaled   : Boolean        := False;
                Widened  : Boolean        := False
             );
   procedure Add_Line
             (  Under    : not null access Layer_Location'Class;
                From     : Cairo_Tuple    := (0.0, 0.0);
                To       : Cairo_Tuple    := (0.0, 1.0);
                Width    : Gdouble        := 1.0;
                Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
                Line_Cap : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
                Scaled   : Boolean        := False;
                Widened  : Boolean        := False
             );
   function Add_Line
            (  Under    : not null access Layer_Location'Class;
               From     : Cairo_Tuple    := (0.0, 0.0);
               Angle    : Gdouble        := 0.0;
               Length   : Gdouble        := 1.0;
               Width    : Gdouble        := 1.0;
               Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
               Line_Cap : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
               Scaled   : Boolean        := False;
               Widened  : Boolean        := False
            )  return not null access Line_Layer;
   function Add_Line
            (  Under    : not null access Layer_Location'Class;
               From     : Cairo_Tuple    := (0.0, 0.0);
               To       : Cairo_Tuple    := (0.0, 1.0);
               Width    : Gdouble        := 1.0;
               Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
               Line_Cap : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
               Scaled   : Boolean        := False;
               Widened  : Boolean        := False
            )  return not null access Line_Layer;
--
-- Get_Angle -- The line angle
--
--    Layer - The line layer
--
-- Returns :
--
--    The line angle
--
   function Get_Angle (Layer : Line_Layer) return Gdouble;
--
-- Get_From -- The point where the line starts
--
--    Layer - The line layer
--
-- Returns :
--
--    The line starting point
--
   function Get_From (Layer : Line_Layer) return Cairo_Tuple;
--
-- Get_Length -- The line length
--
--    Layer - The line layer
--
-- Returns :
--
--    The line length
--
   function Get_Length (Layer : Line_Layer) return Gdouble;
--
-- Get_Line -- Line's parameters
--
--    Layer - The line layer
--
-- Returns :
--
--    The Line's line parameters
--
   function Get_Line (Layer : Line_Layer) return Line_Parameters;
--
-- Get_To -- The point where the line ends
--
--    Layer - The line layer
--
-- Returns :
--
--    The line end point
--
   function Get_To (Layer : Line_Layer) return Cairo_Tuple;
--
-- Set -- Parameters of the line
--
--    Layer - The line layer
--    From  - The point where the line begins
--  [ Angle, Length | To ] - The line angle and length or end point
--    Line  - The line parameters
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer  : in out Line_Layer;
                From   : Cairo_Tuple;
                Angle  : Gdouble;
                Length : Gdouble;
                Line   : Line_Parameters
             );
   procedure Set
             (  Layer : in out Line_Layer;
                From  : Cairo_Tuple;
                To    : Cairo_Tuple;
                Line  : Line_Parameters
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Line_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Line_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      function Get_Properties_Number
               (  Layer : Line_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Line_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Line_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Line_Layer) return Boolean;
   overriding
      function Get_Widened (Layer : Line_Layer) return Boolean;
   overriding function Is_Updated (Layer : Line_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Line_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Line_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Line_Layer;
                   Factor : Gdouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Line_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Line_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Widened
                (  Layer   : in out Line_Layer;
                   Widened : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Line_Layer
                );
private
   type Line_Layer is
      new Abstract_Layer and Scalable_Layer and Widened_Layer with
   record
      From    : Cairo_Tuple;
      To      : Cairo_Tuple;
      Line    : Line_Parameters;
      Scaled  : Boolean := False;
      Widened : Boolean := False;
      Updated : Boolean := True;
   end record;

end Gtk.Layered.Line;
