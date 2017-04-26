--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Abstract_Bordered               Luebeck            --
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

with Gtk.Enums;  use Gtk.Enums;

package Gtk.Layered.Abstract_Bordered is
--
-- Border_Color_Type -- The border color
--
--    Style_Color - When true, the color is taken from the widget styles
--    Color       - The implicit color
--
   type Border_Color_Type (Style_Color : Boolean := True) is record
      case Style_Color is
         when True  => null;
         when False => Color : Gdk_Color;
      end case;
   end record;
   Default_Color : constant Border_Color_Type := (Style_Color => True);
--
-- Abstract_Bordered_Layer -- An abstract layer with a border
--
   type Abstract_Bordered_Layer is
      abstract new Abstract_Layer
               and Scalable_Layer
               and Widened_Layer with private;
--
-- Foreground_Layer -- A  foreground  layer  automatically  added by the
--                     border  layer.  The  border  widget  changes  the
-- effective widget's size for the layers above it. When the  border  is
-- scalable and the layers above it are scalable too, this will have the
-- effect of scaling them to fit into the border. The  foreground  layer
-- restores the effective widget's size. Thus all layers  put  above  it
-- will not be scaled with the border.
--
   type Foreground_Layer is new Abstract_Layer with private;
--
-- Draw_Contents -- Draw the layer contents
--
--    Layer   - The layer
--    Context - To draw into
--    Area    - The widget's rectangle
--
-- When Draw_Contents is called, the border around it is already  drawn.
-- The  path  set  by  Set_Contents_Path is scaled to the required size.
-- E.g. when it is an area to fill, the implementation would simply  set
-- the  color  and  call  Fill (Context).  The context is translated and
-- scaled according to the required location and size of the contents.
--
   procedure Draw_Contents
             (  Layer   : in out Abstract_Bordered_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is abstract;
--
-- Get_Aspected -- The border behavior when the parent widget is resized
--
--    Layer - The background layer
--
-- See Set_Aspected for further information.
--
-- Returns :
--
--    True if border width has the aspect of the widget
--
   function Get_Aspected (Layer : Abstract_Bordered_Layer)
      return Boolean;
--
-- Get_Border_Color -- The border color
--
--    Layer - The border layer
--
-- Returns :
--
--    The border color
--
   function Get_Border_Color (Layer : Abstract_Bordered_Layer)
      return Border_Color_Type;
--
-- Get_Border_Depth -- The border depth
--
--    Layer - The border layer
--
-- Returns :
--
--    The border depth
--
   function Get_Border_Depth (Layer : Abstract_Bordered_Layer)
      return Gdouble;
--
-- Get_Border_Shadow -- The shadow type
--
--    Layer - The border layer
--
-- Returns :
--
--    The shadow type
--
   function Get_Border_Shadow (Layer : Abstract_Bordered_Layer)
      return Gtk_Shadow_Type;
--
-- Get_Border_Width -- The border width
--
--    Layer - The border layer
--
-- Returns :
--
--    The border width
--
   function Get_Border_Width (Layer : Abstract_Bordered_Layer)
      return Gdouble;
--
-- Get_Deepened -- The behavior when the parent widget is resized
--
--    Layer - The background layer
--
-- Returns :
--
--    The shadow deeping mode
--
   function Get_Deepened (Layer : Abstract_Bordered_Layer)
      return Boolean;
--
-- Get_Foreground -- The border's foreground layer
--
--    Layer - The border layer
--
-- Returns :
--
--    The foreground layer or null
--
   function Get_Foreground (Layer : Abstract_Bordered_Layer)
      return access Foreground_Layer'Class;
--
-- Set -- Parameters of the border
--
--    Layer         - The background layer
--    Border_Width  - Border width
--    Border_Depth  - Border depth
--    Border_Color  - The border color
--    Border_Shadow - The border shape
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer         : in out Abstract_Bordered_Layer;
                Border_Width  : Gdouble;
                Border_Depth  : Gdouble;
                Border_Color  : Border_Color_Type;
                Border_Shadow : Gtk_Shadow_Type
             );
--
-- Set_Aspected -- Border behavior when the parent widget is resized
--
--    Layer    - The background layer
--    Aspected - Border width has the aspect of the widget
--
-- If aspected is set to false, when the widget is  resized  the  border
-- width's aspect remains constant. The option  has  no  visible  effect
-- when  the  widget's aspect ratio is 1. For widgets having rectangular
-- border  this option should be true, otherwise vertical and horizontal
-- borders  will  have  different  widths.  The  widgets having circular
-- borders should have this set to false.
--
   procedure Set_Aspected
             (  Layer    : in out Abstract_Bordered_Layer;
                Aspected : Boolean
             );
--
-- Set_Contents_Path -- Set the path around which to draw the border
--
--    Layer   - The layer
--    Context - To draw into
--    Area    - The widget's rectangle
--
-- The  implementation  should create a path in Context. The border will
-- be drawn around the path. The path will be appropriately  scaled  and
-- finally Draw_Contents is called with the path.
--
   procedure Set_Contents_Path
             (  Layer   : in out Abstract_Bordered_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is abstract;
--
-- Set_Deepened -- Change the behavior when the parent widget is resized
--
--    Layer    - The background layer
--    Deepened - Border shadow deepening mode
--
   procedure Set_Deepened
             (  Layer    : in out Abstract_Bordered_Layer;
                Deepened : Boolean
             );
--
-- Set_Updated -- Mark the layer as updated (requiring redrawing)
--
--    Layer - The background layer
--
   procedure Set_Updated (Layer : in out Abstract_Bordered_Layer'Class);

   overriding
      procedure Add
                (  Layer : not null access Abstract_Bordered_Layer;
                   Under : not null access Layer_Location'Class
                );
   overriding
      procedure Draw
                (  Layer   : in out Abstract_Bordered_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Draw
                (  Layer   : in out Foreground_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Finalize (Layer : in out Abstract_Bordered_Layer);
   overriding
      procedure Finalize (Layer : in out Foreground_Layer);
   overriding
      function Get_Properties_Number
               (  Layer : Abstract_Bordered_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Abstract_Bordered_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Abstract_Bordered_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Abstract_Bordered_Layer)
         return Boolean;
   overriding
      function Get_Widened (Layer : Abstract_Bordered_Layer)
         return Boolean;
   overriding
      function Is_Updated (Layer : Abstract_Bordered_Layer)
         return Boolean;
   overriding
      function Is_Updated (Layer : Foreground_Layer) return Boolean;
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Abstract_Bordered_Layer
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Abstract_Bordered_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Abstract_Bordered_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Widened
                (  Layer   : in out Abstract_Bordered_Layer;
                   Widened : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Abstract_Bordered_Layer
                );
private
   type Foreground_Layer_Ptr is access Foreground_Layer;
   type Abstract_Bordered_Layer is
      abstract new Abstract_Layer
               and Scalable_Layer
               and Widened_Layer with
   record
      Foreground    : Foreground_Layer_Ptr;
      Border_Color  : Border_Color_Type := (Style_Color => True);
      Border_Shadow : Gtk_Shadow_Type   := Shadow_None;
      Border_Width  : Gdouble  := 0.0;
      Border_Depth  : Gdouble  := 1.0;
      Aspected      : Boolean := False;
      Scaled        : Boolean := False;
      Widened       : Boolean := False;
      Deepened      : Boolean := False;
      Updated       : Boolean := True;
   end record;
   overriding
      procedure Remove (Layer : in out Abstract_Bordered_Layer);

   type Foreground_Layer is new Abstract_Layer with record
      Size   : Gdouble;
      Border : access Abstract_Bordered_Layer'Class;
   end record;
   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Foreground_Layer;
   overriding
      function Get_Properties_Number
               (  Layer : Foreground_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Foreground_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Foreground_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      procedure Move
                (  Layer  : in out Foreground_Layer;
                   Offset : Cairo_Tuple
                )  is null;
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Foreground_Layer
                )  is null;
   overriding
      procedure Scale
                (  Layer  : in out Foreground_Layer;
                   Factor : Gdouble
                )  is null;
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Foreground_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Foreground_Layer
                )  is null;
end Gtk.Layered.Abstract_Bordered;
