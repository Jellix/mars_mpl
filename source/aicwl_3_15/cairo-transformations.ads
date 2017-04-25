--                                                                    --
--  package Cairo.Transformations   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer 2013       --
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

with Ada.Numerics;    use Ada.Numerics;
with Cairo.Ellipses;  use Cairo.Ellipses;

package Cairo.Transformations is
--
-- Translate_And_Rotate -- Transformation
--
   type Translate_And_Rotate is record
      X, Y  : GDouble := 0.0;
      Angle : GDouble := 0.0;
      Cos   : GDouble := 0.0;
      Sin   : GDouble := 0.0;
   end record;
--
-- Rotate -- By an angle
--
--    T     - The transforamtion
--    Angle - To rotate by (counter clockwise)
--
   procedure Rotate
             (  T     : in out Translate_And_Rotate;
                Angle : GDouble
             );
--
-- Translate -- Move
--
--    T - The transforamtion
--    X - Offset
--    Y - Offset
--
   procedure Translate
             (  T : in out Translate_And_Rotate;
                X : GDouble;
                Y : GDouble
             );
--
-- Drawing operations while applying the transforamtion
--
   procedure Arc
             (  Context : Cairo_Context;
                T       : Translate_And_Rotate;
                X, Y    : GDouble;
                Radius  : GDouble;
                Angle1  : GDouble;
                Angle2  : GDouble
             );
   procedure Elliptic_Arc
             (  Context  : Cairo_Context;
                T        : Translate_And_Rotate;
                Ellipse  : Ellipse_Parameters;
                From     : Ellipse_Angle := 0.0;
                Length   : Ellipse_Angle := 2.0 * Pi
             );
   procedure Elliptic_Arc_Abs
             (  Context  : Cairo_Context;
                T        : Translate_And_Rotate;
                Ellipse  : Ellipse_Parameters;
                From     : GDouble := 0.0;
                Length   : GDouble := 2.0 * Pi
             );
   procedure Line_To
             (  Context : Cairo_Context;
                T       : Translate_And_Rotate;
                X, Y    : GDouble
             );
   procedure Move_To
             (  Context : Cairo_Context;
                T       : Translate_And_Rotate;
                X, Y    : GDouble
             );
private
   pragma Inline (Arc);
   pragma Inline (Line_To);
   pragma Inline (Move_To);

end Cairo.Transformations;
