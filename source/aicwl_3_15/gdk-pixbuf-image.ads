--                                                                    --
--  package Gdk.Pixbuf.Image        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2013       --
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
--
--  The package  provides an  image in  the memory  which  pixels can be
--  manipulated directy.  The image layout  allows its  rendering onto a
--  cairo context without additional conversions.
--
with Cairo;      use Cairo;
with Gdk.Color;  use Gdk.Color;

with Ada.Finalization;

package Gdk.Pixbuf.Image is
--
-- X,Y_Axis -- Image  coordinates, (1,1) corresponds  to  the  left  top
--             corner of an image
--
   type X_Axis is new GInt;
   type Y_Axis is new GInt;
--
-- RGB_Pixel -- Pixel value
--
   type RGB_Pixel is record
      Red   : GUChar;
      Green : GUChar;
      Blue  : GUChar;
   end record;
   for RGB_Pixel'Size use 3 * 8;
   pragma Convention (C, RGB_Pixel);
--
-- RGB_Image -- Rectangular memory-mapped image
--
   type RGB_Image is tagged private;
--
-- Draw -- Image
--
--    Image   - The image
--    Context - The context
--    X, Y    - Where in the context to draw the image
--    X1..X2  - Specifies the image's rectangle to draw onto the
--    Y1..Y2  - context
--
-- Exceptions :
--
--    Constraint_Error - Subscript error
--
   procedure Draw
             (  Image    : RGB_Image;
                Context  : Cairo_Context;
                X        : GInt;
                Y        : GInt;
                X1, X2   : X_Axis;
                Y1, Y2   : Y_Axis
             );
--
-- Erase -- Set all image pixels
--
--    Image - The image
--    Color - The color used to erase pixels
--
   procedure Erase
             (  Image : in out RGB_Image;
                Pixel : RGB_Pixel
             );
   procedure Erase
             (  Image : in out RGB_Image;
                Color : Gdk_Color
             );
--
-- From_Pixel -- Conversion
--
--    Pixel - Value to convert
--
-- Returns :
--
--    Corresponding GDK color
--
   function From_Pixel (Pixel : RGB_Pixel) return Gdk_Color;
--
-- Get -- Image pixel
--
--    Image - The image
--    X, Y  - Pixel coordinates (1..)
--
-- Returns :
--
--    Pixel at (X, Y)
--
-- Exceptions :
--
--    Constraint_Error - Subscript error
--
   function Get
            (  Image : RGB_Image;
               X     : X_Axis;
               Y     : Y_Axis
            )  return RGB_Pixel;
--
-- Get_Height -- Image height
--
--    Image - The image
--
-- Returns :
--
--    Image height
--
   function Get_Height (Image : RGB_Image) return Y_Axis;
--
-- Get_Width -- Image width
--
--    Image - The image
--
-- Returns :
--
--    Image width
--
   function Get_Width (Image : RGB_Image) return X_Axis;
--
-- Set -- Image pixel
--
--    Image - The image
--    X, Y  - Pixel coordinates (1..)
--    Pixel - Value to set
--
-- Exceptions :
--
--    Constraint_Error - Subscript error
--
   procedure Set
             (  Image : in out RGB_Image;
                X     : X_Axis;
                Y     : Y_Axis;
                Pixel : RGB_Pixel
             );
   procedure Set
             (  Image : in out RGB_Image;
                X     : X_Axis;
                Y     : Y_Axis;
                Color : Gdk_Color
             );
--
-- Set_Size -- Change image size
--
--    Image  - The image
--    Width  - New image width
--    Height - New image height
--
   procedure Set_Size
             (  Image  : in out RGB_Image;
                Width  : X_Axis;
                Height : Y_Axis
             );
--
-- To_Pixel -- Conversion
--
--    Color - GDK color
--
-- Returns :
--
--    Corresponding pixel value
--
   function To_Pixel (Color : Gdk_Color) return RGB_Pixel;

private
   pragma Inline (From_Pixel);
   pragma Inline (Get);
   pragma Inline (Get_Height);
   pragma Inline (Get_Width);
   pragma Inline (Set);
   pragma Inline (To_Pixel);

   type RGB_Pixel_Ptr is access all RGB_Pixel;
   type RGB_Buffer is array (GInt range <>) of aliased RGB_Pixel;
   type RGB_Buffer_Ptr is access RGB_Buffer;
   type RGB_Image is new Ada.Finalization.Controlled with
   record
      X_Size : X_Axis := 0;
      Y_Size : Y_Axis := 0;
      Buffer : RGB_Buffer_Ptr;
   end record;
   overriding
      procedure Adjust (Image : in out RGB_Image);
   overriding
      procedure Finalize (Image : in out RGB_Image);

end Gdk.Pixbuf.Image;
