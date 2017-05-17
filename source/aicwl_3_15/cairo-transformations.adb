--                                                                    --
--  package Cairo.Transformations   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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
-- __________________________________________________________________ --

with Cairo.Elementary_Functions;

package body Cairo.Transformations is

   function "*"
     (T : Translate_And_Rotate;
      V : Cairo.Ellipses.Cairo_Tuple) return Cairo.Ellipses.Cairo_Tuple;
   function "*"
     (T : Translate_And_Rotate;
      V : Cairo.Ellipses.Cairo_Tuple) return Cairo.Ellipses.Cairo_Tuple is
   begin
      return
        (X => V.X * T.Cos - V.Y * T.Sin + T.X,
         Y => V.X * T.Sin + V.Y * T.Cos + T.Y);
   end "*";

   function "*"
     (T       : Translate_And_Rotate;
      Ellipse : Cairo.Ellipses.Ellipse_Parameters)
      return Cairo.Ellipses.Ellipse_Parameters;
   function "*"
     (T       : Translate_And_Rotate;
      Ellipse : Cairo.Ellipses.Ellipse_Parameters)
      return Cairo.Ellipses.Ellipse_Parameters is
   begin
      return
        (Center          => T * Ellipse.Center,
         Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle + T.Angle);
   end "*";

   procedure Arc
     (Context : Cairo_Context;
      T       : Translate_And_Rotate;
      X, Y    : Gdouble;
      Radius  : Gdouble;
      Angle1  : Gdouble;
      Angle2  : Gdouble)
   is
      Point : constant Cairo.Ellipses.Cairo_Tuple := T * (X, Y);
   begin
      Arc
        (Cr     => Context,
         Xc     => Point.X,
         Yc     => Point.Y,
         Radius => Radius,
         Angle1 => Angle1 + T.Angle,
         Angle2 => Angle2 + T.Angle);
   end Arc;

   procedure Elliptic_Arc
     (Context  : Cairo_Context;
      T        : Translate_And_Rotate;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters;
      From     : Cairo.Ellipses.Ellipse_Angle := 0.0;
      Length   : Cairo.Ellipses.Ellipse_Angle := 2.0 * Pi) is
   begin
      Cairo.Ellipses.Elliptic_Arc
        (Context => Context,
         Ellipse => T * Ellipse,
         From    => From,
         Length  => Length);
   end Elliptic_Arc;

   procedure Elliptic_Arc_Abs
     (Context  : Cairo_Context;
      T        : Translate_And_Rotate;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters;
      From     : Gdouble := 0.0;
      Length   : Gdouble := 2.0 * Pi) is
   begin
      Cairo.Ellipses.Elliptic_Arc_Abs
        (Context => Context,
         Ellipse => T * Ellipse,
         From    => From,
         Length  => Length);
   end Elliptic_Arc_Abs;

   procedure Line_To
     (Context : Cairo_Context;
      T       : Translate_And_Rotate;
      X, Y    : Gdouble)
   is
      Point : constant Cairo.Ellipses.Cairo_Tuple := T * (X, Y);
   begin
      Line_To
        (Cr => Context,
         X  => Point.X,
         Y  => Point.Y);
   end Line_To;

   procedure Move_To
     (Context : Cairo_Context;
      T       : Translate_And_Rotate;
      X, Y    : Gdouble)
   is
      Point : constant Cairo.Ellipses.Cairo_Tuple := T * (X, Y);
   begin
      Move_To
        (Cr => Context,
         X  => Point.X,
         Y  => Point.Y);
   end Move_To;

   procedure Rotate
     (T     : in out Translate_And_Rotate;
      Angle : Gdouble) is
   begin
      T.Angle := T.Angle + Angle;
      T.Cos   := Cairo.Elementary_Functions.Cos (T.Angle);
      T.Sin   := Cairo.Elementary_Functions.Sin (T.Angle);
   end Rotate;

   procedure Translate
     (T : in out Translate_And_Rotate;
      X : Gdouble;
      Y : Gdouble) is
   begin
      T.X := T.X + X;
      T.Y := T.Y + Y;
   end Translate;

end Cairo.Transformations;
