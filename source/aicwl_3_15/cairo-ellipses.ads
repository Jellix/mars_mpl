--                                                                    --
--  package Cairo.Ellipses          Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2010       --
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

with Ada.Finalization;

package Cairo.Ellipses is
--
-- Ellipse_Angle -- The angle of the ellipse parametric form
--
-- The value 0 corresponds to the left side of the ellipse's major axis.
-- The  value Pi/2 corresponds to the bottom side of the ellipse's minor
-- axis.
--
   type Ellipse_Angle is new Gdouble;
--
-- Cairo_Tuple -- Point
--
   type Cairo_Tuple is record
      X : Gdouble;
      Y : Gdouble;
   end record;
--
-- Cairo_Box -- Rectange
--
   type Cairo_Box is record
      X1 : Gdouble;
      Y1 : Gdouble;
      X2 : Gdouble;
      Y2 : Gdouble;
   end record;
--
-- Get_Path_Extents -- Bounding box of the path
--
--    Context - The context
--
-- Returns :
--
--    The box
--
   function Get_Path_Extents (Context : Cairo_Context)
      return Cairo_Box;
--
-- Ellipse_Parameters -- Describe an ellipse
--
--    Center          - The coordinates of the ellipse center
--    Major_Curvature - The curvature of the major axis, can be 0
--    Minor_Radius    - The radius of the minor axis
--    Angle           - Angle of the major axis
--
   type Ellipse_Parameters is record
      Center          : Cairo_Tuple;
      Major_Curvature : Gdouble;
      Minor_Radius    : Gdouble;
      Angle           : Gdouble := 0.0;
   end record;
   Unit_Circle : constant Ellipse_Parameters :=
                    ((0.0, 0.0), 1.0, 1.0, 0.0);
--
-- * -- Angle conversion
--
--    Ellipse - The ellipse
--    Angle   - The angle to convert
--
-- Returns :
--
--    The corresponding angle relatively to the ellipse major axis
--
   function "*" (Ellipse : Ellipse_Parameters;
                 Angle   : Gdouble)  return Ellipse_Angle;
   function "*" (Angle   : Gdouble;
                 Ellipse : Ellipse_Parameters)  return Ellipse_Angle;
--
-- / -- Angle conversion
--
--    Angle   - The angle relative to the ellipse major axis to convert
--    Ellipse - The ellipse
--
-- Returns :
--
--    The corresponding angle in absolute coordinates
--
   function "/" (Angle   : Ellipse_Angle;
                 Ellipse : Ellipse_Parameters)  return Gdouble;
--
-- *,/ -- Ellipse magnification
--
--    Ellipse - The ellipse
--    Gain    - Magnification factor
--
-- Returns :
--
--    Ellipse magnified by Gain
--
   function "*" (Ellipse : Ellipse_Parameters;
                 Gain    : Gdouble)  return Ellipse_Parameters;
   function "*" (Gain    : Gdouble;
                 Ellipse : Ellipse_Parameters)  return Ellipse_Parameters;
   function "/" (Ellipse : Ellipse_Parameters;
                 Gain    : Gdouble)  return Ellipse_Parameters;
--
-- +,- -- Ellipse movement
--
--    Ellipse - The ellipse
--    Offset  - To move the ellipse at
--
-- Returns :
--
--    Ellipse moved to Offset
--
   function "+" (Ellipse : Ellipse_Parameters;
                 Offset  : Cairo_Tuple) return Ellipse_Parameters;
   function "+" (Offset  : Cairo_Tuple;
                 Ellipse : Ellipse_Parameters) return Ellipse_Parameters;
   function "-" (Ellipse : Ellipse_Parameters;
                 Offset  : Cairo_Tuple) return Ellipse_Parameters;
   function "-" (Offset  : Cairo_Tuple;
                 Ellipse : Ellipse_Parameters) return Ellipse_Parameters;
--
-- Elliptic_Arc -- An elliptic arc
--
--    Context - The context
--    Ellipse - The full ellipse parameters
--    From    - The angle where the arc starts
--    Length  - The arc length
--
-- When Length is positive the arc is drawn clockwise. Otherwise  it  is
-- drawn counterclockwise.
--
-- Exceptions :
--
--    Constraint_Error - The length of infinite flat arc greater than Pi
--
   procedure Elliptic_Arc (Context  : Cairo_Context;
                           Ellipse  : Ellipse_Parameters;
                           From     : Ellipse_Angle := 0.0;
                           Length   : Ellipse_Angle := 2.0 * Ada.Numerics.Pi);
--
-- Elliptic_Arc_Abs -- An elliptic arc with absolute angles
--
--    Context - The context
--    Ellipse - The full ellipse parameters
--    From    - The absolute angle where the arc starts
--    Length  - The arc length
--
-- The  angle  From  is  relative  to the coordinates rather than to the
-- ellipse  axis.  When  Length  is positive the arc is drawn clockwise.
-- Otherwise it is drawn counterclockwise.
--
-- Exceptions :
--
--    Constraint_Error - The length of infinite flat arc greater than Pi
--
   procedure Elliptic_Arc_Abs (Context  : Cairo_Context;
                               Ellipse  : Ellipse_Parameters;
                               From     : Gdouble := 0.0;
                               Length   : Gdouble := 2.0 * Ada.Numerics.Pi);
--
-- Get_Point -- Get the coordinates of ellipse
--
--    Ellipse - The ellipse
--    Angle   - Of the point (relative)
--
-- Returns :
--
--    (X, Y) coordinates of Ellipse at Angle
--
-- Exceptions :
--
--    Constraint_Error - One of the coordinates is infinite
--
   function Get_Point (Ellipse : Ellipse_Parameters;
                       Angle   : Ellipse_Angle) return Cairo_Tuple;
--
-- Get_X -- Get the X coordinate of ellipse
--
--    Ellipse - The ellipse
--    Angle   - Of the point in ellipse
--
-- Returns :
--
--    X coordinate of Ellipse at Angle
--
-- Exceptions :
--
--    Constraint_Error - The coordinate is infinite
--
   function Get_X (Ellipse : Ellipse_Parameters;
                   Angle   : Ellipse_Angle) return Gdouble;
--
-- Get_Y -- Get the Y coordinate of ellipse
--
--    Ellipse - The ellipse
--    Angle   - Of the point
--
-- Returns :
--
--    Y coordinate of Ellipse at Angle
--
-- Exceptions :
--
--    Constraint_Error - The coordinate is infinite
--
   function Get_Y (Ellipse : Ellipse_Parameters;
                   Angle   : Ellipse_Angle) return Gdouble;
--
-- Is_Bounded -- Check if the ellipse is bounded
--
--    Ellipse - The ellipse
--
-- Returns :
--
--    True if the ellipse is bounded
--
   function Is_Bounded (Ellipse : Ellipse_Parameters) return Boolean;
--
-- Context_State -- Saves context
--
   type Context_State (<>) is
      new Ada.Finalization.Limited_Controlled with private;
   overriding procedure Finalize (Save : in out Context_State);
--
-- Save -- Create a context save
--
--    Context - To save
--
   function Save (Context : Cairo_Context) return Context_State;

private
   type Context_State is new Ada.Finalization.Limited_Controlled with
   record
      Context : Cairo_Context := Null_Context;
   end record;

   pragma Inline ("*", "/", "+", "-", Is_Bounded);
end Cairo.Ellipses;
