--                                                                    --
--  package Gdk.Pixbuf.Image        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2013       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Gtkada.Style;

package body Gdk.Pixbuf.Image is

   procedure Free is
      new Ada.Unchecked_Deallocation (RGB_Buffer, RGB_Buffer_Ptr);

   overriding procedure Adjust (Image : in out RGB_Image) is
      Size : constant Gint := Gint (Image.X_Size) * Gint (Image.Y_Size);
      Old  : constant RGB_Buffer_Ptr := Image.Buffer;
   begin
      if Size > 0 then
         Image.Buffer := new RGB_Buffer (1 .. Size);
         Image.Buffer.all (1 .. Size) := Old.all (1 .. Size);
      end if;
   end Adjust;

   procedure Draw
     (Image   : RGB_Image;
      Context : Cairo.Cairo_Context;
      X       : Gint;
      Y       : Gint;
      X1, X2  : X_Axis;
      Y1, Y2  : Y_Axis)
   is
      function "+" is
        new Ada.Unchecked_Conversion
          (RGB_Pixel_Ptr,
           Guchar_Array_Access);
      Icon : Gdk_Pixbuf;
   begin
      if X1 < 1 or else X2 > Image.X_Size then
         raise Constraint_Error with
               "X-range" &
               X_Axis'Image (X1) &
               " .." &
               X_Axis'Image (X2) &
               " not in 1 .." &
               X_Axis'Image (Image.X_Size);
      elsif Y1 < 1 or else Y2 > Image.Y_Size then
         raise Constraint_Error with
               "Y-range" &
               Y_Axis'Image (Y1) &
               " .." &
               Y_Axis'Image (Y2) &
               " not in 1 .." &
               Y_Axis'Image (Image.Y_Size);
      end if;
      Icon :=
         Gdk_New_From_Data
          (Data =>
             +Image.Buffer.all (Gint (Y1 - 1) * Gint (Image.X_Size) + Gint (X1))'Access,
           Width     => Gint (X2 - X1 + 1),
           Height    => Gint (Y2 - Y1 + 1),
           Rowstride => Gint (Image.X_Size) * 3,
           Auto_Destroy_Data => False);
      Gtkada.Style.Draw_Pixbuf
        (Cr     => Context,
         Pixbuf => Icon,
         X      => X,
         Y      => Y);
      Icon.all.Unref;
   end Draw;

   procedure Erase
     (Image : in out RGB_Image;
      Pixel : RGB_Pixel)
   is
      Size : constant Gint := Gint (Image.X_Size) * Gint (Image.Y_Size);
   begin
      if Size > 0 then
         Image.Buffer.all (1 .. Size) := (others => Pixel);
      end if;
   end Erase;

   procedure Erase
     (Image : in out RGB_Image;
      Color : Gdk.Color.Gdk_Color)
   is
      Pixel : constant RGB_Pixel := To_Pixel (Color);
      Size  : constant Gint :=
              Gint (Image.X_Size) * Gint (Image.Y_Size);
   begin
      if Size > 0 then
         Image.Buffer.all (1 .. Size) := (others => Pixel);
      end if;
   end Erase;

   overriding procedure Finalize (Image : in out RGB_Image) is
   begin
      Free (Image.Buffer);
   end Finalize;

   function From_Pixel (Pixel : RGB_Pixel) return Gdk.Color.Gdk_Color is
   begin
      return Color : Gdk.Color.Gdk_Color do
         Gdk.Color.Set_Rgb
           (Color => Color,
            Red   => Guint16 (Pixel.Red)   * 256,
            Green => Guint16 (Pixel.Green) * 256,
            Blue  => Guint16 (Pixel.Blue)  * 256);
      end return;
   end From_Pixel;

   function Get
     (Image : RGB_Image;
      X     : X_Axis;
      Y     : Y_Axis) return RGB_Pixel is
   begin
      if X not in 1 .. Image.X_Size then
         raise Constraint_Error with
               "X =" &
               X_Axis'Image (X) &
               " not in 1 .." &
               X_Axis'Image (Image.X_Size);
      elsif Y not in 1 .. Image.Y_Size then
         raise Constraint_Error with
               "Y =" &
               Y_Axis'Image (Y) &
               " not in 1 .." &
               Y_Axis'Image (Image.Y_Size);
      end if;
      return
        Image.Buffer.all
          (Gint (Y - 1) * Gint (Image.X_Size) + Gint (X));
   end Get;

   function Get_Width (Image : RGB_Image) return X_Axis is
   begin
      return Image.X_Size;
   end Get_Width;

   function Get_Height (Image : RGB_Image) return Y_Axis is
   begin
      return Image.Y_Size;
   end Get_Height;

   procedure Set
     (Image : in out RGB_Image;
      X     : X_Axis;
      Y     : Y_Axis;
      Pixel : RGB_Pixel) is
   begin
      if X not in 1 .. Image.X_Size then
         raise Constraint_Error with
               "X =" &
               X_Axis'Image (X) &
               " not in 1 .." &
               X_Axis'Image (Image.X_Size);
      elsif Y not in 1 .. Image.Y_Size then
         raise Constraint_Error with
               "Y =" &
               Y_Axis'Image (Y) &
               " not in 1 .." &
               Y_Axis'Image (Image.Y_Size);
      end if;
      Image.Buffer.all
        (Gint (Y - 1) * Gint (Image.X_Size) +  Gint (X)) := Pixel;
   end Set;

   procedure Set
     (Image : in out RGB_Image;
      X     : X_Axis;
      Y     : Y_Axis;
      Color : Gdk.Color.Gdk_Color) is
   begin
      if X not in 1 .. Image.X_Size then
         raise Constraint_Error with
               "X =" &
               X_Axis'Image (X) &
               " not in 1 .." &
               X_Axis'Image (Image.X_Size);
      elsif Y not in 1 .. Image.Y_Size then
         raise Constraint_Error with
               "Y =" &
               Y_Axis'Image (Y) &
               " not in 1 .." &
               Y_Axis'Image (Image.Y_Size);
      end if;
      Image.Buffer.all
        (Gint (Y - 1) * Gint (Image.X_Size) + Gint (X)) := To_Pixel (Color);
   end Set;

   procedure Set_Size
     (Image  : in out RGB_Image;
      Width  : X_Axis;
      Height : Y_Axis)
   is
      New_Size : constant Gint := Gint (Width) * Gint (Height);
      Old_Size : constant Gint :=
                 Gint (Image.X_Size) * Gint (Image.Y_Size);
   begin
      if New_Size > Old_Size then
         Free (Image.Buffer);
         Image.Buffer := new RGB_Buffer (1 .. New_Size);
      end if;
      Image.X_Size := Width;
      Image.Y_Size := Height;
   end Set_Size;

   function To_Pixel (Color : Gdk.Color.Gdk_Color) return RGB_Pixel is
   begin
      return
        (Red   => Guchar (Gdk.Color.Red   (Color) / 256),
         Green => Guchar (Gdk.Color.Green (Color) / 256),
         Blue  => Guchar (Gdk.Color.Blue  (Color) / 256));
   end To_Pixel;

   function "not" (Pixel : RGB_Pixel) return RGB_Pixel is
   begin
      return
        (Red   => Pixel.Red   + 128,
         Green => Pixel.Green + 128,
         Blue  => Pixel.Blue  + 128);
   end "not";

end Gdk.Pixbuf.Image;
