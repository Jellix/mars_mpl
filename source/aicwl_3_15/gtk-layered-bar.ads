--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Bar                             Luebeck            --
--  Interface                                      Winter, 2011       --
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

with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Missed;

package Gtk.Layered.Bar is

   --
   -- Bar_Layer -- An line indicating a value
   --
   type Bar_Layer (<>) is
     new Abstract_Layer
     and Gauge_Needle
     and Scalable_Layer
     and Widened_Layer with private;
   --
   -- Add_Bar_Layer -- Add Bar
   --
   --    Under      - The layer or widget where to place the arc under
   --    From       - The point where the line begins
   --  [ Angle, Length | To ] - The line angle and length or end point
   --    Width      - The bar's width
   --    Color      - The Bar color
   --    Line_Cap   - The style of elliptic bar ending
   --    Adjustment - The value source
   --    Scaled     - The layer is scaled together with the parent widget
   --    Widened    - The border line is widened with the parent
   --
   -- When Adjustment is not null the bar moves each time the adjustment is
   -- changed.  Note that it also redraws the layered widget it belongs to.
   -- For complex widgets it is not recommended to use adjustment and event
   -- controlled   layered   widgets.  As  an  alternative  consider  using
   -- Set_Value instead  and  redrawing  the  layered  widget  periodically
   -- independently  on  the  value  state.  When Scaled is true the bar is
   -- scaled  to  fit the parent widget. The scaling of bar is performed as
   -- follows:
   --
   -- (o)  The points coordinates are multiplied  by the widget's  size and
   --      placed in the coorinate system centered in the widget's center;
   -- (o)  Length is multiplied by  the widget's size.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Bar
     (Under      : not null access Layer_Location'Class;
      From       : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Angle      : Gdouble                            := 0.0;
      Length     : Gdouble                            := 1.0;
      Width      : Gdouble                            := 1.0;
      Color      : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Line_Cap   : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      Adjustment : access Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean                            := False;
      Widened    : Boolean                            := False);

   procedure Add_Bar
     (Under      : not null access Layer_Location'Class;
      From       : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      To         : Cairo.Ellipses.Cairo_Tuple         := (0.0, 1.0);
      Width      : Gdouble                            := 1.0;
      Color      : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Line_Cap   : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      Adjustment : access Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean                            := False;
      Widened    : Boolean                            := False);

   function Add_Bar
     (Under      : not null access Layer_Location'Class;
      From       : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Angle      : Gdouble                            := 0.0;
      Length     : Gdouble                            := 1.0;
      Width      : Gdouble                            := 1.0;
      Color      : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Line_Cap   : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      Adjustment : access Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean                            := False;
      Widened    : Boolean                            := False) return not null access Bar_Layer;

   function Add_Bar
     (Under      : not null access Layer_Location'Class;
      From       : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      To         : Cairo.Ellipses.Cairo_Tuple         := (0.0, 1.0);
      Width      : Gdouble                            := 1.0;
      Color      : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Line_Cap   : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      Adjustment : access Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean                            := False;
      Widened    : Boolean                            := False) return not null access Bar_Layer;

   --
   -- Get_Angle -- The line angle
   --
   --    Layer - The bar
   --
   -- Returns :
   --
   --    The line angle
   --
   function Get_Angle (Layer : Bar_Layer) return Gdouble;

   --
   -- Get_From -- The position of the lowest value
   --
   --    Layer - The bar
   --
   -- Returns :
   --
   --    The angle
   --
   function Get_From (Layer : Bar_Layer) return Cairo.Ellipses.Cairo_Tuple;

   --
   -- Get_Length -- The length of the bar
   --
   --    Layer - The bar
   --
   -- Returns :
   --
   --    The length
   --
   function Get_Length (Layer : Bar_Layer) return Gdouble;

   --
   -- Get_Line -- Line's parameters
   --
   --    Layer - The bar
   --
   -- Returns :
   --
   --    The arc's line parameters
   --
   function Get_Line (Layer : Bar_Layer) return Line_Parameters;

   --
   -- Get_To -- The point where the line ends
   --
   --    Layer - The line layer
   --
   -- Returns :
   --
   --    The line end point
   --
   function Get_To (Layer : Bar_Layer) return Cairo.Ellipses.Cairo_Tuple;

   --
   -- Set -- Parameters of the arc
   --
   --    Layer - The bar
   --    From  - The point where the line begins
   --  [ Angle, Length | To ] - The line angle and length or end point
   --    Line  - The arc line parameters
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Set
     (Layer  : in out Bar_Layer;
      From   : Cairo.Ellipses.Cairo_Tuple;
      Angle  : Gdouble;
      Length : Gdouble;
      Line   : Line_Parameters);

   procedure Set
     (Layer : in out Bar_Layer;
      From  : Cairo.Ellipses.Cairo_Tuple;
      To    : Cairo.Ellipses.Cairo_Tuple;
      Line  : Line_Parameters);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Bar_Layer;

   overriding procedure Draw
     (Layer   : in out Bar_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk_Rectangle);

   overriding procedure Finalize (Layer : in out Bar_Layer);

   overriding function Get_Adjustment (Layer : Bar_Layer) return Gtk_Adjustment;

   overriding function Get_Properties_Number (Layer : Bar_Layer) return Natural;

   overriding function Get_Property_Specification
     (Layer    : Bar_Layer;
      Property : Positive) return Param_Spec;

   overriding function Get_Property_Value
     (Layer    : Bar_Layer;
      Property : Positive) return Glib.Values.GValue;

   overriding function Get_Scaled (Layer : Bar_Layer) return Boolean;

   overriding function Get_Value (Layer : Bar_Layer) return Gdouble;

   overriding function Get_Widened (Layer : Bar_Layer) return Boolean;

   overriding function Is_Updated (Layer : Bar_Layer) return Boolean;

   overriding procedure Move
     (Layer  : in out Bar_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple);

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Bar_Layer);

   overriding procedure Scale
     (Layer  : in out Bar_Layer;
      Factor : Gdouble);

   overriding procedure Set_Property_Value
     (Layer    : in out Bar_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue);

   overriding procedure Set_Scaled
     (Layer  : in out Bar_Layer;
      Scaled : Boolean);

   overriding procedure Set_Value
     (Layer : in out Bar_Layer;
      Value : Gdouble);

   overriding procedure Set_Widened
     (Layer   : in out Bar_Layer;
      Widened : Boolean);

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Bar_Layer);

private

   type Bar_Layer is
     new Abstract_Layer
     and Gauge_Needle
     and Scalable_Layer
     and Widened_Layer with
      record
         From          : Cairo.Ellipses.Cairo_Tuple;
         To            : Cairo.Ellipses.Cairo_Tuple;
         Line          : Line_Parameters;
         Value         : Gdouble := 0.0;
         Adjustment    : Gtk_Adjustment;
         Changed       : Handler_Id;
         Value_Changed : Handler_Id;
         Scaled        : Boolean := False;
         Widened       : Boolean := False;
         Updated       : Boolean := True;
         pragma Atomic (Value);
      end record;

end Gtk.Layered.Bar;
