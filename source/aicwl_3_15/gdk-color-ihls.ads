--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gdk.Color.IHLS                              Luebeck            --
--  Interface                                      Winter, 2007       --
--                                                                    --
--                                Last revision :  10:51 08 Jan 2011  --
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
--  This package provides RGB to IHLS color model  transformations.  The
--  IHLS  color  is an improved hue-luminance-staturation representation
--  of  colors  closer  to human perception than HSV and HLS models. The
--  package implements  transformations  as  described  in  "A  3D-polar
--  Coordinate Colour Representation Suitable  for  Image  Analysis"  by
--  Allan Hanbury and Jean Serra, December 16, 2002.
--     (http://www.prip.tuwien.ac.at/ftp/pub/publications/trs/tr77.pdf)
--  The  implementation of the algorithms described there was attuned in
--  terms of performance.
--
package Gdk.Color.IHLS is
--
-- Gdk_Hue -- The type of hue
--
-- Hue characterize the dominant color as perceived by human eye. Hue is
-- a  mudular type considered cyclic. The Hue value 0 corresponds to red
-- of  RGB.  The  value  of  Gdk_Hue'Modulus/3 corresponds to Green. The
-- value  2*Gdk_Hue'Modulus/3  corresponds to Blue. The type is declared
-- compatible with the color datatypes used in GDK.
--
   type Gdk_Hue is new GUInt16;
--
-- Gdk_Luminance -- The type luminance
--
-- Luminance  is  subjective  perception  of  luminous  intensity of the
-- color. Higher values of Liminance correspond to more light colors.
--
   type Gdk_Luminance is new GUInt16;
--
-- Gdk_Saturation -- The type saturation
--
-- Saturation  is  the  color  purity.  The  value 0 corresponds to grey
-- colors. The maximal value corresponds to monochrome (pure) colors.
--
   type Gdk_Saturation is new GUInt16;
--
-- Gdk_IHLS_Color -- The color value Hue/Luminance/Saturation
--
   type Gdk_IHLS_Color is record
      Hue        : Gdk_Hue;
      Luminance  : Gdk_Luminance;
      Saturation : Gdk_Saturation;
   end record;
--
-- Gdk_Stimulus -- RGB stimulus
--
-- The   values   from   0..GUInt16'Last   correspond   to   the  colors
-- representable in the additive RGB model.
--
   type Gdk_Stimulus is new Float;
--
-- Gdk_*_Luminance -- Subypes of visible chromatic luminances
--
   subtype Gdk_Red_Luminance is Gdk_Luminance
      range 0..Gdk_Luminance (Gdk_Stimulus (GUInt16'Last)*0.2126);
   subtype Gdk_Green_Luminance is Gdk_Luminance
      range 0..Gdk_Luminance (Gdk_Stimulus (GUInt16'Last)*0.7152);
   subtype Gdk_Blue_Luminance is Gdk_Luminance
      range 0..Gdk_Luminance (Gdk_Stimulus (GUInt16'Last)*0.0722);
--
-- Average -- An array of colors preserving lightness
--
--    List - Of colors to average
--
-- This  function averages colors from List so that the luminance of the
-- result would be equal to the averaged luminance of the colors in  the
-- list. This function can be used for natural mixing colors.
--
-- Returns :
--
--    The average
--
-- Exceptions :
--
--    Constraint_Error - List is empty
--
   function Average (List : Gdk_Color_Array) return Gdk_Color;
--
-- Darken -- A color
--
--    Color - The argument
--    By    - The luminance to subtract
--
-- When  the value of By exceeds one of Color, the result will have zero
-- luminance (Black).
--
-- Returns :
--
--    Color darkened by the value of By
--
   function Darken
            (  Color : Gdk_IHLS_Color;
               By    : Gdk_Luminance
            )  return Gdk_IHLS_Color;
   function Darken
            (  Color : Gdk_Color;
               By    : Gdk_Luminance
            )  return Gdk_Color;
   pragma Inline (Darken);
--
-- Lighten -- A color
--
--    Color     - The argument
--    By        - The luminance to add
-- [ Impurify ] - Allowing colors of lesser saturation
--
-- When the value of By plus one of Color exceeds the maximal luminance,
-- the result will have maximal luminance. The parameter Impurify allows
-- the result to be less pure but closer  to  the  requested  luminance,
-- when Gdk_Color is lightened.
--
-- Returns :
--
--    Color lightened by the value of By
--
   function Lighten
            (  Color : Gdk_IHLS_Color;
               By    : Gdk_Luminance
            )  return Gdk_IHLS_Color;
   function Lighten
            (  Color    : Gdk_Color;
               By       : Gdk_Luminance;
               Impurify : Boolean := False
            )  return Gdk_Color;
   pragma Inline (Lighten);
--
-- Impurify -- A color
--
--    Color - The argument
--    By    - The saturation to subtract
--
-- When  the  value  of By exceeds one of Color the the result will have
-- minimal saturation (Grey).
--
-- Returns :
--
--    Color impurified by the value of By
--
   function Impurify
            (  Color : Gdk_IHLS_Color;
               By    : Gdk_Saturation
            )  return Gdk_IHLS_Color;
   function Impurify
            (  Color : Gdk_Color;
               By    : Gdk_Saturation
            )  return Gdk_Color;
   pragma Inline (Impurify);
--
-- Purify -- A color
--
--    Color - The argument
--    By    - The saturation to add
--
-- When the value of By plus one of Color exceeds the maximal saturation,
-- the result will have maximal saturation (monochrome).
--
-- Returns :
--
--    Color purified by the value of By
--
   function Purify
            (  Color : Gdk_IHLS_Color;
               By    : Gdk_Saturation
            )  return Gdk_IHLS_Color;
   function Purify
            (  Color : Gdk_Color;
               By    : Gdk_Saturation
            )  return Gdk_Color;
   pragma Inline (Purify);
--
-- To_RGB -- Convert a stimulus value to Gdk_Color stimulus
--
--    Stimulus - A color stimulus
--
-- Returns :
--
--    A Gdk color component in the range of GUInt16
--
   function To_RGB (Stimulus : Gdk_Stimulus) return GUInt16;
   pragma Inline (To_RGB);
--
-- To_RGB -- Color model conversion
--
--    Color    - A color in IHLS model
--    Impurify - Allowing colors of lesser saturation
--
-- This  function  converts  an  IHLS  color to RGB. Some colors in IHLS
-- model have no corresponding colors in RGB. In  particular  monochrome
-- blue  colors  have  limited luminance. When the parameter Impurify is
-- false, the function keeps saturation of the result at the cost of its
-- luminance.  I.e.  the  result  will  appear  darker  than Color. When
-- Impurify is true, the function will keep luminance making result more
-- impure. I.e. very light colors will appear as white.
--
-- Returns :
--
--    The corresponding RGB color
--
   function To_RGB
            (  Color    : Gdk_IHLS_Color;
               Impurify : Boolean := False
            )  return Gdk_Color;
   pragma Inline (To_RGB);
--
-- To_RGB -- Color model conversion
--
--    Red   - Red stimulus
--    Green - Green stimulus
--    Blue  - Blue stimulus
--
-- When the stimuli exceed the ranges the result will have the same  hue
-- but lesser luminance. Its saturation is decreased.
--
-- Returns :
--
--    The corresponding RGB color
--
   function To_RGB
            (  Red   : Gdk_Stimulus;
               Green : Gdk_Stimulus;
               Blue  : Gdk_Stimulus
            )  return Gdk_Color;
   pragma Inline (To_RGB);
--
-- To_RGB -- Color model conversion
--
--    Color - A color in IHLS model
--    Red   - Red stimulus
--    Green - Green stimulus
--    Blue  - Blue stimulus
--
-- This procedure evaluates RGB stimuli an IHLS color. The result
-- may lie outside the ranges of RGB components.
--
   procedure To_RGB
             (  Color : Gdk_IHLS_Color;
                Red   : out Gdk_Stimulus;
                Green : out Gdk_Stimulus;
                Blue  : out Gdk_Stimulus
             );
--
-- To_IHLS -- Color model conversion
--
--    Color - A color in RGB model
--
-- Returns :
--
--    The corresponding IHLS color
--
   function To_IHLS (Color : Gdk_Color) return Gdk_IHLS_Color;
--
-- Color_Cycle -- Number of colors
--
   subtype Color_Cycle is Positive range 2..Positive'Last;
--
-- Val -- Get color by position
--
--    First - The color at the position 0
--    Pos   - The position of the result
--    Cycle - The colors cycle
--
-- This  function  is used to generate sequence of distinct color having
-- same saturation and luminance. The colors of the sequence are ordered
-- by  the  position starting from 0. The first color of the sequence is
-- First.  The next Cycle colors are computed by incrementing the hue by
-- Gdk_Hue'Modulus/Color_Cycle. For the following Cycle*Cycle colors the
-- increment is Gdk_Hue'Modulus/Color_Cycle**2 and so on. The colors  in
-- the sequence do not repeat. They  are  chosen  to  maximize  the  hue
-- distance  between  subsequent  colors, though preventing colors which
-- composition  would  be  grey. Such sequences are useful for assigning
-- colors to plotted curves.
--
-- Returns :
--
--    The color whose position is Pos
--
   function Val
            (  First : Gdk_IHLS_Color;
               Pos   : Natural;
               Cycle : Color_Cycle := 3
            )  return Gdk_IHLS_Color;

end Gdk.Color.IHLS;
