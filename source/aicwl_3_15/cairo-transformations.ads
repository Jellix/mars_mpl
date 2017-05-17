--                                                                    --
--  package Cairo.Transformations   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer 2013        --
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

with Ada.Numerics;
with Cairo.Ellipses;

package Cairo.Transformations is

   Pi : constant := Ada.Numerics.Pi;

   use type Cairo.Ellipses.Ellipse_Angle;

   --
   -- Translate_And_Rotate -- Transformation
   --
   type Translate_And_Rotate is record
      X, Y  : Gdouble := 0.0;
      Angle : Gdouble := 0.0;
      Cos   : Gdouble := 0.0;
      Sin   : Gdouble := 0.0;
   end record;

   --
   -- Rotate -- By an angle
   --
   --    T     - The transforamtion
   --    Angle - To rotate by (counter clockwise)
   --
   procedure Rotate
     (T     : in out Translate_And_Rotate;
      Angle : Gdouble);

   --
   -- Translate -- Move
   --
   --    T - The transforamtion
   --    X - Offset
   --    Y - Offset
   --
   procedure Translate
     (T : in out Translate_And_Rotate;
      X : Gdouble;
      Y : Gdouble);

   --
   -- Drawing operations while applying the transforamtion
   --
   procedure Arc
     (Context : Cairo_Context;
      T       : Translate_And_Rotate;
      X, Y    : Gdouble;
      Radius  : Gdouble;
      Angle1  : Gdouble;
      Angle2  : Gdouble);

   procedure Elliptic_Arc
     (Context  : Cairo_Context;
      T        : Translate_And_Rotate;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters;
      From     : Cairo.Ellipses.Ellipse_Angle := 0.0;
      Length   : Cairo.Ellipses.Ellipse_Angle := 2.0 * Pi);

   procedure Elliptic_Arc_Abs
     (Context  : Cairo_Context;
      T        : Translate_And_Rotate;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters;
      From     : Gdouble := 0.0;
      Length   : Gdouble := 2.0 * Pi);

   procedure Line_To
     (Context : Cairo_Context;
      T       : Translate_And_Rotate;
      X, Y    : Gdouble);

   procedure Move_To
     (Context : Cairo_Context;
      T       : Translate_And_Rotate;
      X, Y    : Gdouble);

private

   pragma Inline (Arc);
   pragma Inline (Line_To);
   pragma Inline (Move_To);

end Cairo.Transformations;
