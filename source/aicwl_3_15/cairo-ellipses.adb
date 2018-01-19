--                                                                    --
--  package Cairo.Ellipses          Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2010       --
--                                                                    --
--                                Last revision :  22:46 07 Apr 2016  --
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

package body Cairo.Ellipses is

   function Cos (X : in Gdouble) return Gdouble renames Cairo.Elementary_Functions.Cos;
   function Sin (X : in Gdouble) return Gdouble renames Cairo.Elementary_Functions.Sin;
   function Arctan (Y : in Gdouble;
                    X : in Gdouble) return Gdouble renames Cairo.Elementary_Functions.Arctan;

   Eps : constant := 1.0E-6;

   function "*" (Gain    : Gdouble;
                 Ellipse : Ellipse_Parameters) return Ellipse_Parameters is
   begin
      return
        (Center          => (X => Ellipse.Center.X * Gain,
                             Y => Ellipse.Center.Y * Gain),
         Major_Curvature => Ellipse.Major_Curvature / Gain,
         Minor_Radius    => Ellipse.Minor_Radius * Gain,
         Angle           => Ellipse.Angle);
   end "*";

   function "*" (Ellipse : Ellipse_Parameters;
                 Gain    : Gdouble) return Ellipse_Parameters is
   begin
      return
        (Center          => (X => Ellipse.Center.X * Gain,
                             Y => Ellipse.Center.Y * Gain),
         Major_Curvature => Ellipse.Major_Curvature / Gain,
         Minor_Radius    => Ellipse.Minor_Radius * Gain,
         Angle           => Ellipse.Angle);
   end "*";

   function "*" (Angle   : Gdouble;
                 Ellipse : Ellipse_Parameters) return Ellipse_Angle is
   begin
      return Ellipse * Angle;
   end "*";

   function "*" (Ellipse : Ellipse_Parameters;
                 Angle   : Gdouble) return Ellipse_Angle is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         return Ellipse_Angle (Angle - Ellipse.Angle);
      else
         declare
            Cos_Ellipse : constant Gdouble := Cos (Ellipse.Angle);
            Sin_Ellipse : constant Gdouble := -Sin (Ellipse.Angle);
            Cos_Angle   : constant Gdouble := Cos (Angle);
            Sin_Angle   : constant Gdouble := Sin (Angle);
         begin
            return
               Ellipse_Angle
                (Arctan
                   (Y => (Sin_Ellipse * Cos_Angle +
                              Cos_Ellipse * Sin_Angle),
                    X => (Ellipse.Major_Curvature *
                              Ellipse.Minor_Radius *
                            (Cos_Ellipse * Cos_Angle -
                               Sin_Ellipse * Sin_Angle))));
         end;
      end if;
   end "*";

   function "+" (Ellipse : Ellipse_Parameters;
                 Offset  : Cairo_Tuple) return Ellipse_Parameters is
   begin
      return
        (Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (X => Ellipse.Center.X + Offset.X,
                             Y => Ellipse.Center.Y + Offset.Y));
   end "+";

   function "+" (Offset  : Cairo_Tuple;
                 Ellipse : Ellipse_Parameters) return Ellipse_Parameters is
   begin
      return
        (Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (X => Ellipse.Center.X + Offset.X,
                             Y => Ellipse.Center.Y + Offset.Y));
   end "+";

   function "-" (Ellipse : Ellipse_Parameters;
                 Offset  : Cairo_Tuple) return Ellipse_Parameters is
   begin
      return
        (Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (X => Ellipse.Center.X - Offset.X,
                             Y => Ellipse.Center.Y - Offset.Y));
   end "-";

   function "-" (Offset  : Cairo_Tuple;
                 Ellipse : Ellipse_Parameters) return Ellipse_Parameters is
   begin
      return
        (Major_Curvature => Ellipse.Major_Curvature,
         Minor_Radius    => Ellipse.Minor_Radius,
         Angle           => Ellipse.Angle,
         Center          => (X => Offset.X - Ellipse.Center.X,
                             Y => Offset.Y - Ellipse.Center.Y));
   end "-";

   function "/" (Ellipse : Ellipse_Parameters;
                 Gain    : Gdouble) return Ellipse_Parameters is
   begin
      return
        (Center          => (X => Ellipse.Center.X / Gain,
                             Y => Ellipse.Center.Y / Gain),
         Major_Curvature => Ellipse.Major_Curvature * Gain,
         Minor_Radius    => Ellipse.Minor_Radius / Gain,
         Angle           => Ellipse.Angle);
   end "/";

   --
   -- Get_Relative_Point -- Get ellipse point in relative coordinates
   --
   --    Ellipse - The ellipse
   --    Angle   - The angle in polar coordinates
   --    X, Y    - The coordinates of the point
   --
   procedure Get_Relative_Point
     (Ellipse : Ellipse_Parameters;
      Angle   : Ellipse_Angle;
      X, Y    : out Gdouble) with Inline;

   function "/" (Angle   : Ellipse_Angle;
                 Ellipse : Ellipse_Parameters) return Gdouble is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         return Gdouble (Angle) - Ellipse.Angle;
      else
         declare
            X, Y : Gdouble;
         begin
            Get_Relative_Point (Ellipse, Angle, X, Y);
            return Arctan (X => X, Y => Y);
         end;
      end if;
   end "/";

   procedure Elliptic_Arc
             (Context : Cairo_Context;
              Ellipse : Ellipse_Parameters;
              From    : Ellipse_Angle := 0.0;
              Length  : Ellipse_Angle := 2.0 * Ada.Numerics.Pi)
   is
      To : constant Ellipse_Angle := From + Length;
   begin
      if Ellipse.Major_Curvature < Eps then
         if abs Length >= Ada.Numerics.Pi then
            raise Constraint_Error with
                  "Greater than Pi angular length of flat elliptic arc";
         end if;
         declare
            Start : constant Ellipse_Angle :=
                    Ellipse_Angle'Remainder (From, Ada.Numerics.Pi);
         begin
            if Start = 0.0 then
               raise Constraint_Error;
            elsif Start > 0.0 then
               if Start + Length not in Eps .. Ada.Numerics.Pi - Eps then
                  raise Constraint_Error;
               end if;
            else
               if Start + Length not in Eps - Ada.Numerics.Pi .. Eps then
                  raise Constraint_Error;
               end if;
            end if;
         end;
         Line_To (Context,
                  Get_X (Ellipse, From),
                  Get_Y (Ellipse, From));
         Line_To (Context,
                  Get_X (Ellipse, To),
                  Get_Y (Ellipse, To));
      else
         declare
            State : Context_State := Save (Context);
            pragma Unreferenced (State);
         begin
            Translate (Context, Ellipse.Center.X, Ellipse.Center.Y);
            Rotate (Context, Ellipse.Angle);
            Scale (Context,
                   1.0 / Ellipse.Major_Curvature,
                   Ellipse.Minor_Radius);
            if Length > 0.0 then
               Arc (Cr     => Context,
                    Xc     => 0.0,
                    Yc     => 0.0,
                    Radius => 1.0,
                    Angle1 => Gdouble (From),
                    Angle2 => Gdouble (To));
            else
               Arc_Negative (Cr     => Context,
                             Xc     => 0.0,
                             Yc     => 0.0,
                             Radius => 1.0,
                             Angle1 => Gdouble (From),
                             Angle2 => Gdouble (To));
            end if;
         end;
      end if;
   end Elliptic_Arc;

   procedure Elliptic_Arc_Abs (Context : Cairo_Context;
                               Ellipse : Ellipse_Parameters;
                               From    : Gdouble := 0.0;
                               Length  : Gdouble := 2.0 * Ada.Numerics.Pi)
   is
      Start : constant Ellipse_Angle := Ellipse * From;
      Angle : Ellipse_Angle := Ellipse * (From + Length) - Start;
   begin
      if Length > 0.0 then
         if Angle < 0.0 then
            Angle := Angle + 2.0 * Ada.Numerics.Pi;
         end if;
      elsif Length < 0.0 then
         if Angle > 0.0 then
            Angle := Angle - 2.0 * Ada.Numerics.Pi;
         end if;
      end if;
      Elliptic_Arc (Context, Ellipse, Start, Angle);
   end Elliptic_Arc_Abs;

   overriding procedure Finalize (Save_Context : in out Context_State) is
   begin
      if Save_Context.Context /= Null_Context then
         Restore (Save_Context.Context);
         Save_Context.Context := Null_Context;
      end if;
   end Finalize;

   function Get_Path_Extents (Context : Cairo_Context)
      return Cairo_Box is
      X1, X2 : aliased Gdouble;
      Y1, Y2 : aliased Gdouble;
   begin
      Path_Extents (Cr => Context,
                    X1 => X1'Access,
                    Y1 => Y1'Access,
                    X2 => X2'Access,
                    Y2 => Y2'Access);
      return (X1 => X1, Y1 => Y1, X2 => X2, Y2 => Y2);
   end Get_Path_Extents;

   function Get_Point (Ellipse : Ellipse_Parameters;
                       Angle   : Ellipse_Angle) return Cairo_Tuple
   is
      X, Y : Gdouble;
   begin
      Get_Relative_Point (Ellipse, Angle, X, Y);
      return
        (X => Ellipse.Center.X + X,
         Y => Ellipse.Center.Y + Y);
   end Get_Point;

   procedure Get_Relative_Point (Ellipse : Ellipse_Parameters;
                                 Angle   : Ellipse_Angle;
                                 X, Y    : out Gdouble) is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         declare
            Beta      : constant Gdouble := Gdouble (Angle) + Ellipse.Angle;
            Cos_Gamma : constant Gdouble :=
                          abs Cos (Ada.Numerics.Pi / 2.0 + Ellipse.Angle -
                                                 Beta);
         begin
            if Cos_Gamma < Eps then
               raise Constraint_Error;
            end if;
            X := Ellipse.Minor_Radius * Cos (Beta) / Cos_Gamma;
            Y := Ellipse.Minor_Radius * Sin (Beta) / Cos_Gamma;
         end;
      else
         declare
            Cos_Angle   : constant Gdouble := Cos (Gdouble (Angle));
            Sin_Angle   : constant Gdouble := Sin (Gdouble (Angle));
            Cos_Ellipse : constant Gdouble := Cos (Ellipse.Angle);
            Sin_Ellipse : constant Gdouble := Sin (Ellipse.Angle);
         begin
            X :=
              (Cos_Angle * Cos_Ellipse / Ellipse.Major_Curvature -
                 Sin_Angle * Sin_Ellipse * Ellipse.Minor_Radius);
            Y :=
              (Cos_Angle * Sin_Ellipse / Ellipse.Major_Curvature +
                 Sin_Angle * Cos_Ellipse * Ellipse.Minor_Radius);
         end;
      end if;
   end Get_Relative_Point;

   function Get_X (Ellipse : Ellipse_Parameters;
                   Angle   : Ellipse_Angle) return Gdouble is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         declare
            Beta : constant Gdouble := Gdouble (Angle) + Ellipse.Angle;
            Cos_Gamma : constant Gdouble :=
                          abs Cos (Ada.Numerics.Pi / 2.0 + Ellipse.Angle -
                                                 Beta);
         begin
            if Cos_Gamma < Eps then
               raise Constraint_Error with "Infinite x-coordinate";
            end if;
            return
              (Ellipse.Center.X + Ellipse.Minor_Radius * Cos (Beta) / Cos_Gamma);
         end;
      else
         declare
            Cos_Angle : constant Gdouble := Cos (Gdouble (Angle));
            Sin_Angle : constant Gdouble := Sin (Gdouble (Angle));
         begin
            return
              (Ellipse.Center.X +
                 Cos_Angle * Cos (Ellipse.Angle) / Ellipse.Major_Curvature -
                   Sin_Angle * Sin (Ellipse.Angle) * Ellipse.Minor_Radius);
         end;
      end if;
   end Get_X;

   function Get_Y (Ellipse : Ellipse_Parameters;
                   Angle   : Ellipse_Angle) return Gdouble is
   begin
      if abs Ellipse.Major_Curvature < Eps then
         declare
            Beta : constant Gdouble := Gdouble (Angle) + Ellipse.Angle;
            Cos_Gamma : constant Gdouble :=
                          abs Cos (Ada.Numerics.Pi / 2.0 + Ellipse.Angle -
                                                 Beta);
         begin
            if Cos_Gamma < Eps then
               raise Constraint_Error with "Infinite y-coordinate";
            end if;
            return
              (Ellipse.Center.Y + Ellipse.Minor_Radius * Sin (Beta) / Cos_Gamma);
         end;
      else
         declare
            Cos_Angle : constant Gdouble := Cos (Gdouble (Angle));
            Sin_Angle : constant Gdouble := Sin (Gdouble (Angle));
         begin
            return
              (Ellipse.Center.Y +
                 Cos_Angle * Sin (Ellipse.Angle) / Ellipse.Major_Curvature +
                   Sin_Angle * Cos (Ellipse.Angle) * Ellipse.Minor_Radius);
         end;
      end if;
   end Get_Y;

   function Is_Bounded (Ellipse : Ellipse_Parameters) return Boolean is
   begin
      return abs Ellipse.Major_Curvature >= Eps;
   end Is_Bounded;

   function Save (Context : Cairo_Context) return Context_State is
   begin
      return Result : Context_State do
         Save (Context);
         Result.Context := Context;
      end return;
   end Save;

end Cairo.Ellipses;
