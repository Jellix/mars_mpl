--                                                                    --
--  package Gtk.Layered.Arc         Copyright (c)  Dmitry A. Kazakov  --
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
with Gtk.Missed;

package Gtk.Layered.Arc is

   --
   -- Arc_Layer -- A layer shaped as an elliptic arc
   --
   type Arc_Layer (<>) is
     new Abstract_Layer
     and Scalable_Layer
     and Widened_Layer with private;

   --
   -- Add_Arc -- Add an elliptic arc
   --
   --    Under    - The layer or widget where to place the arc under
   --    Ellipse  - The ellipse to which the arc belongs
   --    From     - The angle where the arc begins
   --    Length   - The angular length of the arc
   --    Width    - The line width
   --    Color    - The line color
   --    Line_Cap - The way lines end
   --    Scaled   - The layer is scaled together with the parent widget
   --    Widened  - The layer's line is widened together with the widget
   --
   -- When Length  is  positive  the  arc  is  drawn  clockwise,  otherwise
   -- counterclockwise.  When  Scaled  is true the arc is scaled to fit the
   -- parent widget. The scaling is performed as follows:
   --
   -- (o)  The points are multiplied by the widget's size and placed in the
   --      coorinate system centered in the widget's center;
   -- (o)  The ellipse major axis curvature is divided by the the  widget's
   --      size;
   -- (o)  The  ellipse  minor  axis  radius  is multiplied by the widget's
   --      size.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Arc
     (Under    : not null access Layer_Location'Class;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle;
      From     : Gdouble                           := 0.0;
      Length   : Gdouble                           := 2.0 * Ada.Numerics.Pi;
      Width    : Gdouble                           := 1.0;
      Color    : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap              := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                           := False;
      Widened  : Boolean                           := False);

   function Add_Arc
     (Under    : not null access Layer_Location'Class;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle;
      From     : Gdouble                           := 0.0;
      Length   : Gdouble                           := 2.0 * Ada.Numerics.Pi;
      Width    : Gdouble                           := 1.0;
      Color    : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap              := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                           := False;
      Widened  : Boolean                           := False)
      return not null access Arc_Layer;

   --
   -- Get_Ellipse -- Ellipse parameters of the arc
   --
   --    Layer - The arc layer
   --
   -- Returns :
   --
   --    The parameters of the arc's ellipse
   --
   function Get_Ellipse (Layer : Arc_Layer)
                         return Cairo.Ellipses.Ellipse_Parameters;

   --
   -- Get_From -- Angle of the arc's beginning
   --
   --    Layer - The arc layer
   --
   -- Returns :
   --
   --    The angle
   --
   function Get_From (Layer : Arc_Layer) return Gdouble;

   --
   -- Get_Length -- Angular length of the arc
   --
   --    Layer - The arc layer
   --
   -- Returns :
   --
   --    The angle
   --
   function Get_Length (Layer : Arc_Layer) return Gdouble;

   --
   -- Get_Line -- Arc's line parameters
   --
   --    Layer - The arc layer
   --
   -- Returns :
   --
   --    The arc's line parameters
   --
   function Get_Line (Layer : Arc_Layer) return Line_Parameters;

   --
   -- Set -- Parameters of the arc
   --
   --    Layer   - The arc layer
   --    Ellipse - The ellipse to which the arc belongs
   --    From    - The angle where the arc begins
   --    Length  - The angular length of the arc
   --    Line    - The arc line parameters
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Set
     (Layer   : in out Arc_Layer;
      Ellipse : Cairo.Ellipses.Ellipse_Parameters;
      From    : Gdouble;
      Length  : Gdouble;
      Line    : Line_Parameters);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Arc_Layer;

   overriding function Get_Properties_Number (Layer : Arc_Layer) return Natural;

   overriding function Get_Property_Specification
     (Layer    : Arc_Layer;
      Property : Positive) return Param_Spec;

   overriding function Get_Property_Value
     (Layer    : Arc_Layer;
      Property : Positive) return Glib.Values.GValue;

   overriding function Get_Scaled (Layer : Arc_Layer) return Boolean;

   overriding  function Get_Widened (Layer : Arc_Layer) return Boolean;

   overriding procedure Draw
     (Layer   : in out Arc_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle);

   overriding function Is_Updated (Layer : Arc_Layer) return Boolean;

   overriding procedure Move
     (Layer  : in out Arc_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple);

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Arc_Layer);

   overriding procedure Scale
     (Layer  : in out Arc_Layer;
      Factor : Gdouble);

   overriding procedure Set_Property_Value
     (Layer    : in out Arc_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue);

   overriding procedure Set_Scaled (Layer : in out Arc_Layer; Scaled : Boolean);

   overriding procedure Set_Widened
     (Layer   : in out Arc_Layer;
      Widened : Boolean);

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Arc_Layer);

private

   type Arc_Layer is
     new Abstract_Layer
     and Scalable_Layer
     and Widened_Layer with
      record
         Ellipse : Cairo.Ellipses.Ellipse_Parameters;
         From    : Gdouble;
         Length  : Gdouble;
         Line    : Line_Parameters;
         Scaled  : Boolean := False;
         Widened : Boolean := False;
         Updated : Boolean := True;
      end record;

end Gtk.Layered.Arc;
