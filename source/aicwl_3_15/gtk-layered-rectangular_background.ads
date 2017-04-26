--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Rectangular_Background          Luebeck            --
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

with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Layered.Abstract_Bordered;  use Gtk.Layered.Abstract_Bordered;

package Gtk.Layered.Rectangular_Background is
--
-- Rectangular_Background_Layer -- A filled rectangle
--
   type Rectangular_Background_Layer (<>) is
      new Abstract_Bordered_Layer with private;
--
-- Add_Rectangular_Background -- Add a filled rectangle
--
--    Under          - The layer or widget where to place it under
--    Height         - Of the rectangle
--    Width          - Of the rectangle
--    Center         - The location of the rectangle's center
--    Rotation_Angle - The angle to the width axis of the rectangle
--    Corner_Radius  - The radius of the circles rounding the corners
--    Color          - The background color
--    Border_Width   - Border width
--    Border_Depth   - Border depth
--    Border_Color   - The border color
--    Border_Shadow  - The border shape
--    Deepened       - The border depth is increased with the parent
--    Scaled         - The layer is scaled together with the parent
--    Widened        - The border line is widened with the parent
--
-- The procedure adds an rectangular background and foreground above at.
-- The layers above, visually nested in the background should be  placed
-- above the background and below the foreground. When  Scaled  is  true
-- the background is scaled to fit the parent widget. The scaling of the
-- rectangle is performed as follows:
--
-- (o)  The  center's X is multiplied by the widget's size and placed in
--      the coorinate system centered in the widget's center;
-- (o)  The  center's Y is multiplied by the widget's size and placed in
--      the coorinate system centered in the widget's center;
-- (o)  The  length, width and corner radius are multiplied the widget's
--      size.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Rectangular_Background
             (  Under          : not null access Layer_Location'Class;
                Height         : Gdouble     := 1.0;
                Width          : Gdouble     := 1.0;
                Center         : Cairo_Tuple := (0.0, 0.0);
                Rotation_Angle : Gdouble     := 0.0;
                Corner_Radius  : Gdouble     := 0.0;
                Color          : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Border_Width   : Gdouble           := 0.0;
                Border_Depth   : Gdouble           := 1.0;
                Border_Color   : Border_Color_Type := Default_Color;
                Border_Shadow  : Gtk_Shadow_Type   := Shadow_In;
                Deepened       : Boolean           := False;
                Scaled         : Boolean           := False;
                Widened        : Boolean           := False
             );
   function Add_Rectangular_Background
            (  Under          : not null access Layer_Location'Class;
               Height         : Gdouble     := 1.0;
               Width          : Gdouble     := 1.0;
               Center         : Cairo_Tuple := (0.0, 0.0);
               Rotation_Angle : Gdouble     := 0.0;
               Corner_Radius  : Gdouble     := 0.0;
               Color          : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Border_Width   : Gdouble           := 0.0;
               Border_Depth   : Gdouble           := 1.0;
               Border_Color   : Border_Color_Type := Default_Color;
               Border_Shadow  : Gtk_Shadow_Type   := Shadow_In;
               Deepened       : Boolean           := False;
               Scaled         : Boolean           := False;
               Widened        : Boolean           := False
            )  return not null access Rectangular_Background_Layer;
--
-- Get_Center -- Of the rectangle
--
--    Layer - The background layer
--
-- Returns :
--
--    The center coordinates
--
   function Get_Center (Layer : Rectangular_Background_Layer)
      return Cairo_Tuple;
--
-- Get_Color -- The text color
--
--    Layer - The background layer
--
-- Returns :
--
--    The text color
--
   function Get_Color (Layer : Rectangular_Background_Layer)
      return Gdk_Color;
--
-- Get_Corner_Radius -- The radius of the corners
--
--    Layer - The background layer
--
-- Returns :
--
--    The corner's radius
--
   function Get_Corner_Radius (Layer : Rectangular_Background_Layer)
      return GDouble;
--
-- Get_Height -- The height of the rectangle
--
--    Layer - The background layer
--
-- Returns :
--
--    The rectangle's height
--
   function Get_Height (Layer : Rectangular_Background_Layer)
      return GDouble;
--
-- Get_Rotation_Angle -- The rotation angle
--
--    Layer - The background layer
--
-- Returns :
--
--    The angle
--
   function Get_Rotation_Angle (Layer : Rectangular_Background_Layer)
      return GDouble;
--
-- Get_Width -- The width of the rectangle
--
--    Layer - The background layer
--
-- Returns :
--
--    The rectangle's width
--
   function Get_Width (Layer : Rectangular_Background_Layer)
      return GDouble;
--
-- Set -- Parameters of the background
--
--    Layer          - The background layer
--    Height         - Of the rectangle
--    Width          - Of the rectangle
--    Center         - The location of the rectangle's center
--    Rotation_Angle - The angle to the width axis of the rectangle
--    Corner_Radius  - The radius of the circles rounding the corners
--    Color          - The background color
--    Border_Width   - Border width
--    Border_Depth   - Border depth
--    Border_Color   - The border color
--    Border_Shadow  - The border shape
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer          : in out Rectangular_Background_Layer;
                Height         : GDouble;
                Width          : GDouble;
                Center         : Cairo_Tuple;
                Rotation_Angle : GDouble;
                Corner_Radius  : GDouble;
                Color          : Gdk_Color;
                Border_Width   : GDouble;
                Border_Depth   : GDouble;
                Border_Color   : Border_Color_Type;
                Border_Shadow  : Gtk_Shadow_Type
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Rectangular_Background_Layer;
   overriding
      procedure Draw_Contents
                (  Layer   : in out Rectangular_Background_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      function Get_Properties_Number
               (  Layer : Rectangular_Background_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Rectangular_Background_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Rectangular_Background_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      procedure Move
                (  Layer  : in out Rectangular_Background_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Rectangular_Background_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Rectangular_Background_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Contents_Path
                (  Layer   : in out Rectangular_Background_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Rectangular_Background_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Rectangular_Background_Layer
                );
private
   type Rectangular_Background_Layer is
      new Abstract_Bordered_Layer with
   record
      Height : GDouble;
      Width  : GDouble;
      Center : Cairo_Tuple;
      Angle  : GDouble;
      Radius : GDouble;
      Color  : Gdk_Color;
   end record;

end Gtk.Layered.Rectangular_Background;
