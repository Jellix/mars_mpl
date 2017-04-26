--                                                                    --
--  package Gtk.Layered.Rectangle   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2011       --
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

package Gtk.Layered.Rectangle is
--
-- Rectangle_Layer -- A layer shaped as a rectangle
--
   type Rectangle_Layer (<>) is
      new Abstract_Layer
      and Scalable_Layer
      and Widened_Layer with private;
--
-- Add_Rectangle -- Add rectangle
--
--    Under      - The layer or widget to place the rectangle under
--    Box        - The rectangle's box
--    Color      - The fill color
--    Line_Width - Line width
--    Opacity    - Filling opacity
--    Scaled     - The layer is scaled together with the parent widget
--    Widened    - The layer's line is widened together with the widget
--
-- When Opacity is 0 the rectangle is not filled. When Line_Width is  0,
-- no line is drawn. The scaling is performed as follows:
--
-- (o)  The horizontal  rectangle  coordinates  are  multiplied  by  the
--      widget  width  then horizontal coordinate of the widget's center
--      is added;
-- (o)  The vertical rectangle coordinates are multiplied by the  widget
--      width then vertical coordinate of the widget's center is added.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Rectangle
             (  Under      : not null access Layer_Location'Class;
                Box        : Cairo_Box;
                Color      : Gdk_Color    := RGB (0.0, 0.0, 0.0);
                Line_Width : Gdouble      := 0.0;
                Opacity    : Fill_Opacity := 1.0;
                Scaled     : Boolean      := False;
                Widened    : Boolean      := False
             );
   function Add_Rectangle
            (  Under      : not null access Layer_Location'Class;
               Box        : Cairo_Box;
               Color      : Gdk_Color    := RGB (0.0, 0.0, 0.0);
               Line_Width : Gdouble      := 0.0;
               Opacity    : Fill_Opacity := 1.0;
               Scaled     : Boolean      := False;
               Widened    : Boolean      := False
            )  return not null access Rectangle_Layer;
--
-- Get_Box -- The rectangle box
--
--    Layer - The rectangle layer
--
-- Returns :
--
--    The box
--
   function Get_Box (Layer : Rectangle_Layer) return Cairo_Box;
--
-- Get_Color -- The rectangle color
--
--    Layer - The rectangle layer
--
-- Returns :
--
--    The color
--
   function Get_Color (Layer : Rectangle_Layer) return Gdk_Color;
--
-- Get_Line_Width -- The rectangle color
--
--    Layer - The rectangle layer
--
-- Returns :
--
--    The line width
--
   function Get_Line_Width (Layer : Rectangle_Layer) return Gdouble;
--
-- Get_Opacity -- The filling opacity
--
--    Layer - The rectangle layer
--
-- Returns :
--
--    The opacity (0..1)
--
   function Get_Opacity (Layer : Rectangle_Layer) return Fill_Opacity;
--
-- Set -- Parameters of the rectangle
--
--    Layer      - The rectangle layer
--    Box        - The rectangle's box
--    Color      - The fill color
--    Line_Width - Line width
--    Opacity    - Filling opacity
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer      : in out Rectangle_Layer;
                Box        : Cairo_Box;
                Color      : Gdk_Color;
                Line_Width : Gdouble;
                Opacity    : Fill_Opacity
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Rectangle_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Rectangle_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      function Get_Properties_Number
               (  Layer : Rectangle_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Rectangle_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Rectangle_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Rectangle_Layer) return Boolean;
   overriding
      function Get_Widened (Layer : Rectangle_Layer) return Boolean;
   overriding function Is_Updated (Layer : Rectangle_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Rectangle_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Rectangle_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Rectangle_Layer;
                   Factor : Gdouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Rectangle_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Rectangle_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Widened
                (  Layer   : in out Rectangle_Layer;
                   Widened : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Rectangle_Layer
                );
private
   type Rectangle_Layer is
      new Abstract_Layer and Scalable_Layer and Widened_Layer with
   record
      Box     : Cairo_Box;
      Color   : Gdk_Color;
      Width   : Gdouble;
      Opacity : Fill_Opacity;
      Widened : Boolean := False;
      Scaled  : Boolean := False;
      Updated : Boolean := True;
   end record;

end Gtk.Layered.Rectangle;
