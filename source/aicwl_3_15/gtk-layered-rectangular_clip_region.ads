--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.                                Luebeck            --
--        Rectangular_Clip_Region                  Winter, 2010       --
--  Interface                                                         --
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

package Gtk.Layered.Rectangular_Clip_Region is
--
-- Rectangular_Clip_On_Layer -- A clipping rectangle activated
--
   type Rectangular_Clip_Region_On_Layer (<>) is
      new Abstract_Layer and Scalable_Layer with private;
--
-- Rectangular_Clip_Off_Layer -- A clipping rectangle deactivated
--
   type Rectangular_Clip_Region_Off_Layer (<>) is
      new Abstract_Layer with private;
--
-- Add_Rectangular_Clip_Region -- Add a clipping reagion
--
--    Under          - The layer or widget where to place it under
--    Height         - Of the rectangle
--    Width          - Of the rectangle
--    Center         - The location of the rectangle's center
--    Rotation_Angle - The angle to the width axis of the rectangle
--    Corner_Radius  - The radius of the circles rounding the corners
--    Scaled         - The  layer  is scaled  together  with  the parent
--                     widget
--
-- The operation adds two layers one  above  another.  The  layer  below
-- clips  a  rectangle  with  rounded corners. The layer above drops the
-- clipping region. The layers  that  should  be  clipped  must  be  put
-- between them,  e.g.  under  Add_Rectangular_Clip_Region  (...).Above.
-- When  Scaled  is  true  the  background  is  scaled to fit the parent
-- widget. The scaling of the rectangle is performed as follows:
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
--    The clipping on layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Rectangular_Clip_Region
             (  Under          : not null access Layer_Location'Class;
                Height         : GDouble     := 1.0;
                Width          : GDouble     := 1.0;
                Center         : Cairo_Tuple := (0.0, 0.0);
                Rotation_Angle : GDouble     := 0.0;
                Corner_Radius  : GDouble     := 0.0;
                Scaled         : Boolean     := False
             );
   function Add_Rectangular_Clip_Region
            (  Under          : not null access Layer_Location'Class;
               Height         : GDouble     := 1.0;
               Width          : GDouble     := 1.0;
               Center         : Cairo_Tuple := (0.0, 0.0);
               Rotation_Angle : GDouble     := 0.0;
               Corner_Radius  : GDouble     := 0.0;
               Scaled         : Boolean     := False
            )  return not null access Rectangular_Clip_Region_On_Layer;
--
-- Get_Center -- Of the rectangle
--
--    Layer - The clip layer
--
-- Returns :
--
--    The center coordinates
--
   function Get_Center (Layer : Rectangular_Clip_Region_On_Layer)
      return Cairo_Tuple;
--
-- Get_Corner_Radius -- The radius of the corners
--
--    Layer - The clip layer
--
-- Returns :
--
--    The corner's radius
--
   function Get_Corner_Radius (Layer : Rectangular_Clip_Region_On_Layer)
      return GDouble;
--
-- Get_Height -- The height of the rectangle
--
--    Layer - The clip layer
--
-- Returns :
--
--    The rectangle's height
--
   function Get_Height (Layer : Rectangular_Clip_Region_On_Layer)
      return GDouble;
--
-- Get_Rotation_Angle -- The rotation angle
--
--    Layer - The clip layer
--
-- Returns :
--
--    The angle
--
   function Get_Rotation_Angle
            (  Layer : Rectangular_Clip_Region_On_Layer
            )  return GDouble;
--
-- Get_Width -- The width of the rectangle
--
--    Layer - The clip layer
--
-- Returns :
--
--    The rectangle's width
--
   function Get_Width
            (  Layer : Rectangular_Clip_Region_On_Layer
            )  return GDouble;
--
-- Set -- Parameters of the layer
--
--    Layer          - The layer
--    Height         - Of the rectangle
--    Width          - Of the rectangle
--    Center         - The location of the rectangle's center
--    Rotation_Angle - The angle to the width axis of the rectangle
--    Corner_Radius  - The radius of the circles rounding the corners
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer   : in out Rectangular_Clip_Region_On_Layer;
                Height         : GDouble;
                Width          : GDouble;
                Center         : Cairo_Tuple;
                Rotation_Angle : GDouble;
                Corner_Radius  : GDouble
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access
                         Rectangular_Clip_Region_On_Layer;
   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access
                         Rectangular_Clip_Region_Off_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Rectangular_Clip_Region_On_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Draw
                (  Layer   : in out Rectangular_Clip_Region_Off_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Finalize
                (  Layer : in out Rectangular_Clip_Region_On_Layer
                );
   overriding
      procedure Finalize
                (  Layer : in out Rectangular_Clip_Region_Off_Layer
                );
   overriding
      function Get_Properties_Number
               (  Layer : Rectangular_Clip_Region_On_Layer
               )  return Natural;
   overriding
      function Get_Properties_Number
               (  Layer : Rectangular_Clip_Region_Off_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Rectangular_Clip_Region_On_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Specification
               (  Layer    : Rectangular_Clip_Region_Off_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Rectangular_Clip_Region_On_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Property_Value
               (  Layer    : Rectangular_Clip_Region_Off_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled
               (  Layer : Rectangular_Clip_Region_On_Layer
               )  return Boolean;
   overriding
      function Is_Updated (Layer : Rectangular_Clip_Region_On_Layer)
         return Boolean;
   overriding
      function Is_Updated (Layer : Rectangular_Clip_Region_Off_Layer)
         return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Rectangular_Clip_Region_On_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Move
                (  Layer  : in out Rectangular_Clip_Region_Off_Layer;
                   Offset : Cairo_Tuple
                )  is null;
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Rectangular_Clip_Region_On_Layer
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Rectangular_Clip_Region_Off_Layer
                )  is null;
   overriding
      procedure Scale
                (  Layer  : in out Rectangular_Clip_Region_On_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Scale
                (  Layer  : in out Rectangular_Clip_Region_Off_Layer;
                   Factor : GDouble
                )  is null;
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Rectangular_Clip_Region_On_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Rectangular_Clip_Region_Off_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Rectangular_Clip_Region_On_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Rectangular_Clip_Region_On_Layer
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Rectangular_Clip_Region_Off_Layer
                )  is null;
private
   type Rectangular_Clip_Region_On_Layer_Ptr is
      access all Rectangular_Clip_Region_On_Layer;
   type Rectangular_Clip_Region_Off_Layer_Ptr is
      access all Rectangular_Clip_Region_Off_Layer;

   type Rectangular_Clip_Region_On_Layer is
      new Abstract_Layer and Scalable_Layer with
   record
      Height  : GDouble;
      Width   : GDouble;
      Center  : Cairo_Tuple;
      Angle   : GDouble;
      Radius  : GDouble;
      Off     : Rectangular_Clip_Region_Off_Layer_Ptr;
      Drawn   : Boolean := False;
      Scaled  : Boolean := False;
      Updated : Boolean := True;
   end record;
   overriding
      procedure Remove
                (  Layer : in out Rectangular_Clip_Region_On_Layer
                );

   type Rectangular_Clip_Region_Off_Layer is
      new Abstract_Layer with
   record
      On : Rectangular_Clip_Region_On_Layer_Ptr;
   end record;

end Gtk.Layered.Rectangular_Clip_Region;
