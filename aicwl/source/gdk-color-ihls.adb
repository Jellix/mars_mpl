--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gdk.Color.IHLS                              Luebeck            --
--  Implementation                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  10:00 13 Oct 2007  --
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

with Ada.Numerics.Generic_Elementary_Functions;

package body Gdk.Color.IHLS is

   pragma Warnings (Off, "declaration hides ""Blue""");
   pragma Warnings (Off, "declaration hides ""Color""");
   pragma Warnings (Off, "declaration hides ""Green""");
   pragma Warnings (Off, "declaration hides ""Impurify""");
   pragma Warnings (Off, "declaration hides ""Red""");

   package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Gdk_Stimulus);

   Hue_Range   : constant Gdk_Stimulus :=
                    Gdk_Stimulus (Gdk_Hue'Modulus);
   Hue_Quarter : constant Gdk_Stimulus := Hue_Range / 4.0;
   Hue_Third   : constant Gdk_Stimulus := Hue_Range / 3.0;
   Hue_Sixth   : constant Gdk_Stimulus := Hue_Range / 6.0;
   Hue_Eps     : constant Gdk_Stimulus := 1.0 / Hue_Range;
   RGB_Max     : constant Gdk_Stimulus := Gdk_Stimulus (Guint16'Last);
   S_Factor    : constant Gdk_Stimulus := Elementary_Functions.Sqrt (3.0) / 2.0;
   S_Eps       : constant Gdk_Stimulus :=
                    1.0 / Gdk_Stimulus (Gdk_Saturation'Modulus);

   function To_Hue (Hue : Gdk_Stimulus) return Gdk_Hue
     with Inline;

   function To_Luminance (Luminance : Gdk_Stimulus) return Gdk_Luminance
     with Inline;

   function Average (List : Gdk_Color_Array) return Gdk_Color is
      R_Sum : Gdk_Stimulus := 0.0;
      G_Sum : Gdk_Stimulus := 0.0;
      B_Sum : Gdk_Stimulus := 0.0;
   begin
      if List'Length = 0 then
         raise Constraint_Error;
      end if;
      --
      -- There   is   no   concern   about  accuracy  of  summation  for
      -- floating-point is far more precise than 16-bit color  intensity
      -- values involved. So a straightforward averaging is used.
      --
      declare
         Color : Gdk_Color;
      begin
         for Index in List'Range loop
            Color := List (Index);
            R_Sum := R_Sum + Gdk_Stimulus (Red   (Color));
            G_Sum := G_Sum + Gdk_Stimulus (Green (Color));
            B_Sum := B_Sum + Gdk_Stimulus (Blue  (Color));
         end loop;
      end;
      declare
         Factor : constant Gdk_Stimulus := Gdk_Stimulus (List'Length);
         Color  : Gdk_Color;
      begin
         Set_Rgb
           (Color,
            Red   => To_RGB (R_Sum / Factor),
            Green => To_RGB (G_Sum / Factor),
            Blue  => To_RGB (B_Sum / Factor));
         return Color;
      end;
   end Average;

   function Darken
     (Color : Gdk_IHLS_Color;
      By    : Gdk_Luminance) return Gdk_IHLS_Color is
   begin
      if Color.Luminance <= By then
         return (Color.Hue, 0, Color.Saturation);
      else
         return (Color.Hue, Color.Luminance - By, Color.Saturation);
      end if;
   end Darken;

   function Darken
     (Color : Gdk_Color;
      By    : Gdk_Luminance) return Gdk_Color is
   begin
      return To_RGB (Darken (To_IHLS (Color), By));
   end Darken;

   --
   -- Get_Hue -- Hue from R, G, B components
   --
   --    R, G, B - The arguments
   --
   -- The expression for Hue as specified in the paper is
   --
   --    if G >= B
   --       H = arcos (y/x), where
   --         y = R - G/2 - B/2
   --         x = sqrt (RR + GG + BB - RG - RB - GB)
   --    else
   --       H = 2Pi - arcos(y/x)
   --
   -- This expression is replaced by a  simpler  expression  based  on  the
   -- two-argument arctan:
   --
   --    arcos (y/x) = arctan (sqrt (xx-yy)/y)) =
   --  = arctan (sqrt(xx-yy), y)
   --
   -- Substitution of x and y gives:
   --
   --    arctan (sqrt(RR + GG   + BB   - RG - RB - GB
   --                -RR - GG/4 - BB/4 + RG + RB - GB/2), y) =
   --  = arctan (sqrt(3/4 GG + 3/4 BB - 3/2 GB), y) =
   --  = arctan (sqrt(3)/2 (G-B), y) =
   --  = arctan (sqrt(3)/2 (G-B), R - (G+B)/2) =
   --       (for G >= B)
   --
   --    2Pi + arctan (sqrt(3)/2 (G-B), R - (G+B)/2)
   --       (for G < B)
   --
   -- Returns :
   --
   --    Hue [0..Hue_Range[
   --
   function Get_Hue (R, G, B : Gdk_Stimulus)
      return Gdk_Stimulus is
      pragma Inline (Get_Hue);
      Hue : Gdk_Stimulus := R - (G + B) / 2.0;
   begin
      if abs Hue < Hue_Eps then
         if G < B then
            Hue := -Hue_Quarter;
         else
            Hue := Hue_Quarter;
         end if;
      else
         Hue := Elementary_Functions.Arctan (S_Factor * (G - B), Hue, Hue_Range);
      end if;
      if Hue < 0.0 then
         return Hue_Range + Hue;
      else
         return Hue;
      end if;
   end Get_Hue;

   function Impurify
     (Color : Gdk_IHLS_Color;
      By    : Gdk_Saturation) return Gdk_IHLS_Color is
   begin
      if Color.Saturation <= By then
         return (Color.Hue, Color.Luminance, 0);
      else
         return (Color.Hue, Color.Luminance, Color.Saturation - By);
      end if;
   end Impurify;

   function Impurify
     (Color : Gdk_Color;
      By    : Gdk_Saturation) return Gdk_Color is
   begin
      return To_RGB (Impurify (To_IHLS (Color), By));
   end Impurify;

   function Lighten
     (Color : Gdk_IHLS_Color;
      By    : Gdk_Luminance) return Gdk_IHLS_Color is
   begin
      if Color.Luminance + By < Color.Luminance then
         return (Color.Hue, Gdk_Luminance'Last, Color.Saturation);
      else
         return (Color.Hue, Color.Luminance + By, Color.Saturation);
      end if;
   end Lighten;

   function Lighten
     (Color    : Gdk_Color;
      By       : Gdk_Luminance;
      Impurify : Boolean := False) return Gdk_Color is
   begin
      return To_RGB (Lighten (To_IHLS (Color), By), Impurify);
   end Lighten;

   function Purify
     (Color : Gdk_IHLS_Color;
      By    : Gdk_Saturation) return Gdk_IHLS_Color is
   begin
      if Color.Saturation > Gdk_Saturation'Last - By then
         return (Color.Hue, Color.Luminance, Gdk_Saturation'Last);
      else
         return (Color.Hue, Color.Luminance, Color.Saturation + By);
      end if;
   end Purify;

   function Purify
     (Color : Gdk_Color;
      By    : Gdk_Saturation) return Gdk_Color is
   begin
      return To_RGB (Purify (To_IHLS (Color), By));
   end Purify;

   function To_Hue (Hue : Gdk_Stimulus) return Gdk_Hue is
   begin
      if Hue >= Hue_Range then
         return Gdk_Hue (Gdk_Stimulus'Floor (Hue - Hue_Range));
      elsif Hue < 0.0 then
         return Gdk_Hue (Gdk_Stimulus'Floor (Hue + Hue_Range));
      else
         return Gdk_Hue (Gdk_Stimulus'Floor (Hue));
      end if;
   end To_Hue;

   function To_IHLS (Color : Gdk_Color) return Gdk_IHLS_Color is
      G  : constant Guint16      := Green (Color);
      R  : constant Guint16      := Red   (Color);
      B  : constant Guint16      := Blue  (Color);
      FR : constant Gdk_Stimulus := Gdk_Stimulus (R);
      FG : constant Gdk_Stimulus := Gdk_Stimulus (G);
      FB : constant Gdk_Stimulus := Gdk_Stimulus (B);
      Result : Gdk_IHLS_Color;
   begin
      Result.Luminance :=
         To_Luminance (0.2126 * FR + 0.7152 * FG + 0.0722 * FB);
      Result.Hue := To_Hue (Get_Hue (FR, FG, FB));
      if B > G then
         Result.Saturation :=
            Gdk_Saturation (Guint16'Max (R, B) - Guint16'Min (R, G));
      else
         Result.Saturation :=
            Gdk_Saturation (Guint16'Max (R, G) - Guint16'Min (R, B));
      end if;
      return Result;
   end To_IHLS;

   function To_Luminance (Luminance : Gdk_Stimulus) return Gdk_Luminance is
   begin
      if Luminance >= Gdk_Stimulus (Gdk_Luminance'Last) then
         return Gdk_Luminance'Last;
      elsif Luminance <= Gdk_Stimulus (Gdk_Luminance'First) then
         return Gdk_Luminance'First;
      else
         return Gdk_Luminance (Luminance);
      end if;
   end To_Luminance;

   function To_RGB (Stimulus : Gdk_Stimulus) return Guint16 is
   begin
      if Stimulus >= RGB_Max then
         return Guint16'Last;
      elsif Stimulus <= 0.0 then
         return Guint16'First;
      else
         return Guint16 (Stimulus);
      end if;
   end To_RGB;

   procedure To_RGB
     (Color : Gdk_IHLS_Color;
      Red   : out Gdk_Stimulus;
      Green : out Gdk_Stimulus;
      Blue  : out Gdk_Stimulus)
   is
      Luminance : constant Gdk_Stimulus :=
                     Gdk_Stimulus (Color.Luminance);
      Hue : constant Gdk_Stimulus := Gdk_Stimulus (Color.Hue);
      C1  : Gdk_Stimulus := Hue;
      C2  : Gdk_Stimulus;
   begin
      if C1 >= Hue_Third then
         C1 := C1 - Hue_Third;
         if C1 >= Hue_Third then
            C1 := C1 - Hue_Third;
         end if;
      end if;
      if C1 >= Hue_Sixth then
         C1 := C1 - Hue_Sixth;
      end if;
      C2 := Elementary_Functions.Sin (Hue_Third - C1, Hue_Range);
      if C2 < S_Eps then
         Red   := Luminance;
         Green := Luminance;
         Blue  := Luminance;
      else
         declare
            C : constant Gdk_Stimulus :=
                   S_Factor * Gdk_Stimulus (Color.Saturation) / C2;
         begin
            C1 :=  C * Elementary_Functions.Cos (Hue, Hue_Range);
            C2 := -C * Elementary_Functions.Sin (Hue, Hue_Range);
            Red   := Luminance + 0.7875 * C1 + 0.3714 * C2;
            Green := Luminance - 0.2125 * C1 - 0.2059 * C2;
            Blue  := Luminance - 0.2125 * C1 + 0.9488 * C2;
         end;
      end if;
   end To_RGB;

   function To_RGB
     (Red   : Gdk_Stimulus;
      Green : Gdk_Stimulus;
      Blue  : Gdk_Stimulus) return Gdk_Color
   is
      Scale : Gdk_Stimulus := 1.0;
      Color : Gdk_Color;
   begin
      if Blue > RGB_Max then
         Scale := RGB_Max / Blue;
      end if;
      if Red > RGB_Max then
         Scale := Gdk_Stimulus'Min (RGB_Max / Red, Scale);
      end if;
      if Green > RGB_Max then
         Scale := Gdk_Stimulus'Min (RGB_Max / Green, Scale);
      end if;
      if Scale <= 1.0 then
         Set_Rgb
           (Color,
            Red   => To_RGB (Red),
            Green => To_RGB (Green),
            Blue  => To_RGB (Blue));
      else
         Set_Rgb
           (Color,
            Red   => To_RGB (Red   * Scale),
            Green => To_RGB (Green * Scale),
            Blue  => To_RGB (Blue  * Scale));
      end if;
      return Color;
   end To_RGB;

   function To_RGB_Saturating (Color : Gdk_IHLS_Color) return Gdk_Color;

   function To_RGB
     (Color    : Gdk_IHLS_Color;
      Impurify : Boolean := False) return Gdk_Color is
   begin
      if Impurify then
         return To_RGB_Saturating (Color);
      else
         declare
            Red   : Gdk_Stimulus;
            Green : Gdk_Stimulus;
            Blue  : Gdk_Stimulus;
         begin
            To_RGB (Color, Red, Green, Blue);
            return To_RGB (Red, Green, Blue);
         end;
      end if;
   end To_RGB;

   function To_RGB_Saturating (Color : Gdk_IHLS_Color) return Gdk_Color
   is
      Luminance : constant Gdk_Stimulus :=
                     Gdk_Stimulus (Color.Luminance);
      Hue   : constant Gdk_Stimulus := Gdk_Stimulus (Color.Hue);
      C1    : Gdk_Stimulus := Hue;
      C2    : Gdk_Stimulus;
      Red   : Gdk_Stimulus;
      Green : Gdk_Stimulus;
      Blue  : Gdk_Stimulus;
   begin
      if C1 >= Hue_Third then
         C1 := C1 - Hue_Third;
         if C1 >= Hue_Third then
            C1 := C1 - Hue_Third;
         end if;
      end if;
      if C1 >= Hue_Sixth then
         C1 := C1 - Hue_Sixth;
      end if;
      C2 := Elementary_Functions.Sin (Hue_Third - C1, Hue_Range);
      if C2 < S_Eps then
         Red   := Luminance;
         Green := Luminance;
         Blue  := Luminance;
      else
         declare
            L : constant Gdk_Stimulus := RGB_Max - Luminance;
            C : Gdk_Stimulus :=
                   S_Factor * Gdk_Stimulus (Color.Saturation) / C2;
         begin
            C1 :=  Elementary_Functions.Cos (Hue, Hue_Range);
            C2 := -Elementary_Functions.Sin (Hue, Hue_Range);
            Red   :=  0.7875 * C1 + 0.3714 * C2;
            Green := -0.2125 * C1 - 0.2059 * C2;
            Blue  := -0.2125 * C1 + 0.9488 * C2;
            if C * Red > L then
               C := L / Red;
            end if;
            if C * Green > L then
               C := L / Green;
            end if;
            if C * Blue > L then
               C := L / Blue;
            end if;
            Red   := Luminance + C * Red;
            Green := Luminance + C * Green;
            Blue  := Luminance + C * Blue;
         end;
      end if;
      return To_RGB (Red, Green, Blue);
   end To_RGB_Saturating;

   function Val
     (First : Gdk_IHLS_Color;
      Pos   : Natural;
      Cycle : Color_Cycle := 3) return Gdk_IHLS_Color
   is
      Power : constant Gdk_Stimulus := Gdk_Stimulus (Cycle);
      Hue   : Gdk_Stimulus := 0.0;
      Scale : Gdk_Stimulus := Gdk_Stimulus (Gdk_Hue'Modulus);
      Value : Natural      := Pos;
   begin
      while Value > 0 loop
         Scale := Scale / Power;
         Hue   := Hue + Scale * Gdk_Stimulus (Value rem Cycle);
         Value := Value / Cycle;
      end loop;
      return
        (Hue        => To_Hue (Hue + Gdk_Stimulus (First.Hue)),
         Saturation => First.Saturation,
         Luminance  => First.Luminance);
   end Val;

   pragma Warnings (On, "declaration hides ""Red""");
   pragma Warnings (On, "declaration hides ""Impurify""");
   pragma Warnings (On, "declaration hides ""Green""");
   pragma Warnings (On, "declaration hides ""Color""");
   pragma Warnings (On, "declaration hides ""Blue""");

end Gdk.Color.IHLS;
