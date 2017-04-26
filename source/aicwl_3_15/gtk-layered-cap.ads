--                                                                    --
--  package Gtk.Layered.Cap         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2010       --
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

with Gtk.Enums;
with Gtk.Layered.Abstract_Bordered;  use Gtk.Layered.Abstract_Bordered;

package Gtk.Layered.Cap is
--
-- Cap_Layer -- A layer shaped as an round cap
--
   type Cap_Layer (<>) is new Abstract_Bordered_Layer with private;
--
-- Add_Cap -- Add a cap
--
--    Under         - The layer or widget where to place the cap under
--    Center        - Of the cap
--    Radius        - The cap radius
--    From          - The color in upper left corner
--    To            - The color in lower right corner
--    Border_Width  - Border width
--    Border_Depth  - Border depth
--    Border_Color  - The border color
--    Border_Shadow - The border shape
--    Deepened      - The border depth is increased with the parent
--    Scaled        - The layer is scaled together with the parent
--    Widened       - The border line is widened with the parent
--
-- When Scaled is true the cap is scaled to fit the parent  widget.  The
-- scaling is performed as follows:
--
-- (o)  The center's X is multiplied by the widget's width and placed in
--      the coorinate system centered in the widget's center;
-- (o)  The  center's  Y is multiplied by the widget's height and placed
--      in the coorinate system centered in the widget's center;
-- (o)  The cap radius is multiplied by  the  minimum  of  the  widget's
--      height and width.
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Cap
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo_Tuple               := (0.0, 0.0);
      Radius        : Gdouble                   := 1.0;
      From          : Gdk_Color                 := RGB (1.0, 1.0, 1.0);
      To            : Gdk_Color                 := RGB (0.5, 0.5, 0.5);
      Border_Width  : Gdouble                   := 0.0;
      Border_Depth  : Gdouble                   := 1.0;
      Border_Color  : Border_Color_Type         := Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_Out;
      Deepened      : Boolean                   := False;
      Scaled        : Boolean                   := False;
      Widened       : Boolean                   := False);

   function Add_Cap
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo_Tuple               := (0.0, 0.0);
      Radius        : Gdouble                   := 1.0;
      From          : Gdk_Color                 := RGB (1.0, 1.0, 1.0);
      To            : Gdk_Color                 := RGB (0.5, 0.5, 0.5);
      Border_Width  : Gdouble                   := 0.0;
      Border_Depth  : Gdouble                   := 1.0;
      Border_Color  : Border_Color_Type         := Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_Out;
      Deepened      : Boolean                   := False;
      Scaled        : Boolean                   := False;
      Widened       : Boolean                   := False)
      return not null access Cap_Layer;

   --
   -- Get_Center -- The cap center
   --
   --    Layer - The cap layer
   --
   -- Returns :
   --
   --    The location of the cap
   --
   function Get_Center (Layer : Cap_Layer) return Cairo_Tuple;

   --
   -- Get_From -- The color in the upper left corner
   --
   --    Layer - The cap layer
   --
   -- Returns :
   --
   --    The color
   --
   function Get_From (Layer : Cap_Layer) return Gdk_Color;

   --
   -- Get_Radius -- The cap radius
   --
   --    Layer - The cap layer
   --
   -- Returns :
   --
   --    The radius
   --
   function Get_Radius (Layer : Cap_Layer) return Gdouble;

   --
   -- Get_To -- The color in the lower right corner
   --
   --    Layer - The cap layer
   --
   -- Returns :
   --
   --    The color
   --
   function Get_To (Layer : Cap_Layer) return Gdk_Color;

   --
   -- Set -- Parameters of the cap
   --
   --    Layer         - The cap layer
   --    Center        - Of the cap
   --    Radius        - The cap radius
   --    From          - The color in upper left corner
   --    To            - The color in lower right corner
   --    Border_Width  - Border width
   --    Border_Depth  - Border depth
   --    Border_Color  - The border color
   --    Border_Shadow - The border shape
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Set (Layer         : in out Cap_Layer;
                  Center        : Cairo_Tuple;
                  Radius        : Gdouble;
                  From          : Gdk_Color;
                  To            : Gdk_Color;
                  Border_Width  : Gdouble;
                  Border_Depth  : Gdouble;
                  Border_Color  : Border_Color_Type;
                  Border_Shadow : Gtk.Enums.Gtk_Shadow_Type);

   overriding
   function Add (Under  : not null access Layer_Location'Class;
                 Stream : not null access Root_Stream_Type'Class)
                 return not null access Cap_Layer;

   overriding
   procedure Draw_Contents (Layer   : in out Cap_Layer;
                            Context : Cairo_Context;
                            Area    : Gdk_Rectangle);

   overriding
   procedure Finalize (Layer : in out Cap_Layer);

   overriding
   function Get_Properties_Number (Layer : Cap_Layer) return Natural;

   overriding
   function Get_Property_Specification (Layer    : Cap_Layer;
                                        Property : Positive) return Param_Spec;

   overriding
   function Get_Property_Value (Layer    : Cap_Layer;
                                Property : Positive) return GValue;

   overriding
   procedure Move (Layer  : in out Cap_Layer;
                   Offset : Cairo_Tuple);

   overriding
   procedure Restore (Stream : in out Root_Stream_Type'Class;
                      Layer  : in out Cap_Layer);

   overriding
   procedure Scale (Layer  : in out Cap_Layer;
                    Factor : Gdouble);

   overriding
   procedure Set_Contents_Path (Layer   : in out Cap_Layer;
                                Context : Cairo_Context;
                                Area    : Gdk_Rectangle);

   overriding
   procedure Set_Property_Value (Layer    : in out Cap_Layer;
                                 Property : Positive;
                                 Value    : GValue);

   overriding
   procedure Store (Stream : in out Root_Stream_Type'Class;
                    Layer  : Cap_Layer);

private

   type Cap_Layer is new Abstract_Bordered_Layer with
      record
         Center  : Cairo_Tuple;
         Radius  : Gdouble;
         From    : Gdk_Color;
         To      : Gdk_Color;
         Mask    : Cairo_Pattern;
         Pattern : Cairo_Pattern;
      end record;

end Gtk.Layered.Cap;
