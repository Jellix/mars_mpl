--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Graph_Paper_Annotation          Luebeck            --
--  Interface                                      Summer, 2011       --
--                                                                    --
--                                Last revision :  10:27 26 Mar 2016  --
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

with Ada.Strings;              use Ada.Strings;
with Gtk.Layered.Graph_Paper;  use Gtk.Layered.Graph_Paper;
with Gtk.Layered.Waveform;     use Gtk.Layered.Waveform;
with Gtk.Missed;

with Ada.Calendar;

package Gtk.Layered.Graph_Paper_Annotation is

   --
   -- Graph_Paper_Annotation_Layer -- A layer consisting of texts drawn  at
   --                                 the positions relative to  an  X-  or
   --                                 Y-axis of a graph paper.
   --
   type Graph_Paper_Annotation_Layer (<>) is
     new Abstract_Layer
     and Scalable_Layer
     and Graph_Paper_Annotation_Interface with private;

   type Axis_Orientation is (Horizontal, Vertical);
   type Axis_Alignment is (Absolute, Relative);
   type Axis_Location
     (Orientation : Axis_Orientation := Horizontal;
      Alignment   : Axis_Alignment   := Relative)
   is
      record
         case Orientation is
            when Horizontal =>
               Left  : Gdouble := -0.5;
               Right : Gdouble :=  0.5;
               case Alignment is
                  when Absolute =>
                     Y_Position : Gdouble := 0.0;
                  when Relative =>
                     Y_Value : Y_Axis := 0.0;
               end case;
            when Vertical =>
               Top    : Gdouble := -0.5;
               Bottom : Gdouble :=  0.5;
               case Alignment is
                  when Absolute =>
                     X_Position : Gdouble := 0.0;
                  when Relative =>
                     X_Value : X_Axis := 0.0;
               end case;
         end case;
      end record;

   --
   -- Add_Graph_Paper_Annotation -- Add annotation texts
   --
   --    Under       - The layer or widget where to place it under
   --    Paper       - The graph paper layer
   --    Location    - The annotation location
   --    Face        - The text font
   --    Height      - The text height
   --    Stretch     - The text width scale relative to its height
   --    Color       - The text color
   --    Text_Angle  - The text angle
   --    Justify_X   - The text box horizontal alignment after rotation
   --    Justify_Y   - The text box vertical alignment after rotation
   --    Superscript - Superscript is allowed for exponents
   --    Background  - The color of the text background box
   --    Border      - Added to the text's extents box to get background
   --    Overlap     - The text box overlap
   --    Opacity     - The text backgound box's opacity
   --    Scaled      - The layer is scaled together with the parent
   --    Enlarged    - The annotation texts enlarged with the parent
   --
   -- The texts are drawn along the axis specified by Location. The axis is
   -- either horizontal or vertical. A horizontal axis is specified by  its
   -- left and right points and the vertical position (Y_Position) or  else
   -- the amplified source value (Y_Value). A vertical axis is specified by
   -- its top and bottom points and the horizontal position (X_Position) or
   -- else the sweeper's value (X_Value). An annotation text is shown  only
   -- when its box is  located  between  Left..Right  or  Top..Bottom.  The
   -- parameter  Overlap  determines  the  non-overlapping  text boxes. The
   -- value  of  Overlap  is  added  to  the  box  left and top margins and
   -- subtracted  from  the  right  and bottom margins. If the obtained box
   -- overlaps the text box of the following text box, the  latter  is  not
   -- shown.  When  a value determines the location of the annotation texts
   -- then a text is shown only when in the graph paper's box horizontal or
   -- vertical range correspondingly. The scaling is performed as follows:
   --
   -- (o)  Left, Right, Top, Bottom, X_Position, Y_Position of Location are
   --      multiplied  by  the  widget's  size  and placed in the coorinate
   --      system centered in the widget's center;
   -- (o)  Height is multiplied by the widget's size if Enlarged is true.
   -- (o)  Border  and  Overlap  are  multiplied  by  the  widget's size if
   --      Enlarged is true.
   --
   -- The parameter  Superscript when true allows use superscript digits in
   -- the exponent powers. Otherwise the format E<n> is used.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Graph_Paper_Annotation
     (Under       : not null access Layer_Location'Class;
      Paper       : not null access Graph_Paper_Layer'Class;
      Location    : Axis_Location       :=
        (Orientation => Vertical,
         Alignment   => Absolute,
         Top         => -0.5,
         Bottom      => -0.5,
         X_Position  =>  0.0);
      Face        : Pango_Cairo_Font    :=
        Create_Pango ("arial unicode ms");
      Height      : Gdouble             := 12.0;
      Stretch     : Gdouble             := 1.0;
      Color       : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble             := 0.0;
      Justify_X   : Alignment           := Center;
      Justify_Y   : Vertical_Alignment  := Center;
      Superscript : Boolean             := True;
      Background  : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Border      : Gdouble             := 2.0;
      Overlap     : Gdouble             := -2.0;
      Opacity     : Fill_Opacity        := 1.0;
      Scaled      : Boolean             := False;
      Enlarged    : Boolean             := False);

   function Add_Graph_Paper_Annotation
     (Under       : not null access Layer_Location'Class;
      Paper       : not null access Graph_Paper_Layer'Class;
      Location    : Axis_Location       :=
        (Orientation => Vertical,
         Alignment   => Absolute,
         Top         => -0.5,
         Bottom      =>  0.5,
         X_Position  =>  0.0);
      Face        : Pango_Cairo_Font    :=
        Create_Pango ("arial unicode ms");
      Height      : Gdouble             := 12.0;
      Stretch     : Gdouble             := 1.0;
      Color       : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble             := 0.0;
      Justify_X   : Alignment           := Center;
      Justify_Y   : Vertical_Alignment  := Center;
      Superscript : Boolean             := True;
      Background  : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Border      : Gdouble             := 2.0;
      Overlap     : Gdouble             := -2.0;
      Opacity     : Fill_Opacity        := 1.0;
      Scaled      : Boolean             := False;
      Enlarged    : Boolean             := False)
      return not null access Graph_Paper_Annotation_Layer;

   --
   -- Get_Background_Color -- The text background color
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The text background color
   --
   function Get_Background_Color
     (Layer : Graph_Paper_Annotation_Layer) return Gdk.Color.Gdk_Color;

   --
   -- Get_Border -- The border around annotation texts
   --
   --    Layer - The annotation layer
   --
   -- The  border  is  the  amount  added to the annotation text extents in
   -- order to obtain the text's background box. The box is filled with the
   -- background color.
   --
   -- Returns :
   --
   --    The border
   --
   function Get_Border (Layer : Graph_Paper_Annotation_Layer)
                        return Gdouble;

   --
   -- Get_Color -- The text color
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The text color
   --
   function Get_Color
     (Layer : Graph_Paper_Annotation_Layer) return Gdk.Color.Gdk_Color;

   --
   -- Get_Enlarged -- Text enlargement
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    True if the annotation texts are enlarged with the parent
   --
   function Get_Enlarged (Layer : Graph_Paper_Annotation_Layer)
                          return Boolean;

   --
   -- Get_Face -- Text font face
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The font face
   --
   function Get_Face (Layer : Graph_Paper_Annotation_Layer)
                      return Pango_Cairo_Font;

   --
   -- Get_Justify_X -- The text justification
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The text justification
   --
   function Get_Justify_X (Layer : Graph_Paper_Annotation_Layer)
                           return Alignment;

   --
   -- Get_Justify_Y -- The text vertical justification
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The text justification
   --
   function Get_Justify_Y (Layer : Graph_Paper_Annotation_Layer)
                           return Vertical_Alignment;

   --
   -- Get_Height -- The text height
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The text height
   --
   function Get_Height (Layer : Graph_Paper_Annotation_Layer)
                        return Gdouble;

   --
   -- Get_Location -- The the annotation axis location
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The first annotation position
   --
   function Get_Location (Layer : Graph_Paper_Annotation_Layer)
                          return Axis_Location;

   --
   -- Get_Opacity -- The background opacity
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The opacity of the background box
   --
   function Get_Opacity (Layer : Graph_Paper_Annotation_Layer)
                         return Fill_Opacity;

   --
   -- Get_Overlap -- The text box overlap
   --
   --    Layer - The annotation layer
   --
   -- The text boxs may overlap the specified area of the text box.
   --
   -- Returns :
   --
   --    The overlap
   --
   function Get_Overlap (Layer : Graph_Paper_Annotation_Layer)
                         return Gdouble;

   --
   -- Get_Stretch -- The text stretch
   --
   --    Layer - The label layer
   --
   -- The text stretch is how the text width should be scaled relatively to
   -- its height. For example, 2.0 means twice as wide than normal.
   --
   -- Returns :
   --
   --    The text stretch
   --
   function Get_Stretch (Layer : Graph_Paper_Annotation_Layer)
                         return Gdouble;

   --
   -- Get_Suffix -- The text added to all annotation texts
   --
   --    Layer  - The annotation layer
   --
   -- Returns :
   --
   --    The suffix text
   --
   function Get_Suffix (Layer : Graph_Paper_Annotation_Layer)
                        return UTF8_String;

   --
   -- Get_Superscript -- Usage of superscript
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    True if supescript digits are allowed to use
   --
   function Get_Superscript (Layer : Graph_Paper_Annotation_Layer)
                             return Boolean;

   --
   -- Get_Text_Angle -- The angle of the annotation texts
   --
   --    Layer - The annotation layer
   --
   -- Returns :
   --
   --    The angle of the texts
   --
   function Get_Text_Angle (Layer : Graph_Paper_Annotation_Layer)
                            return Gdouble;

   --
   -- Image -- Textual representation of a value
   --
   --    Layer - The annotation layer
   --    Value - To render
   --
   -- This function returns the text  representing  Value in the format the
   -- annotation layer would use to render a tick's value.
   --
   -- Returns :
   --
   --    The representation of the value
   --
   function Image
     (Layer : Graph_Paper_Annotation_Layer;
      Value : Gdouble) return UTF8_String;

   --
   -- Image -- Conversion of a time stamp to human-readable format
   --
   --    Stamp / Interval - Time stamp
   --
   -- Returns :
   --
   --    A text representation of the argument
   --
   function Image (Stamp : Ada.Calendar.Time) return UTF8_String;
   function Image (Interval : Duration) return UTF8_String;

   --
   -- Set -- Parameters of the annotation
   --
   --    Layer       - The annotation layer
   --    Location    - The annotation location
   --    Face        - The text font
   --    Height      - The text height
   --    Stretch     - The text width scale relative to its height
   --    Color       - The text color
   --    Text_Angle  - The text angle
   --    Justify_X   - The text box horizontal alignment after rotation
   --    Justify_Y   - The text box vertical alignment after rotation
   --    Superscript - Superscript is allowed for exponents
   --    Background  - The color of the text background box
   --    Border      - Added to the text extents box to get background box
   --    Overlap     - The text box overlap
   --    Opacity     - The text backgound box's opacity
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Set
     (Layer       : in out Graph_Paper_Annotation_Layer;
      Location    : Axis_Location;
      Face        : Pango_Cairo_Font;
      Height      : Gdouble;
      Stretch     : Gdouble;
      Color       : Gdk.Color.Gdk_Color;
      Text_Angle  : Gdouble;
      Justify_X   : Alignment;
      Justify_Y   : Vertical_Alignment;
      Superscript : Boolean;
      Background  : Gdk.Color.Gdk_Color;
      Border      : Gdouble;
      Overlap     : Gdouble;
      Opacity     : Fill_Opacity);

   --
   -- Set_Enlarged -- Text enlargement
   --
   --    Layer    - The annotation layer
   --    Enlarged - The annotation texts enlarged with the parent
   --
   procedure Set_Enlarged
     (Layer    : in out Graph_Paper_Annotation_Layer;
      Enlarged : Boolean);

   --
   -- Set_Face -- Set font face
   --
   --    Layer - The annotation layer
   --    Face  - The text font
   --
   procedure Set_Face
     (Layer : in out Graph_Paper_Annotation_Layer;
      Face  : Pango_Cairo_Font);

   --
   -- Set_Renderer -- Set custom annotation text renderer
   --
   --    Layer    - The annotation layer
   --    Renderer - The custom function to use for text rendering
   --
   -- When set as null, which is default, the primitive Render operation is
   -- used.  When not  null  it is  used  instead of Render.  The  renderer
   -- function profile is following:
   --
   --    Layer  - The annotation layer
   --    Value  - The value to render
   --    Raster - The scale used to render the text
   --
   -- Returns :
   --
   --    Result text
   --
   type Renderer_Function is access
     function
       (Layer  : Graph_Paper_Annotation_Layer'Class;
        Value  : Gdouble;
        Raster : Gtk.Layered.Waveform.Rasters.Scale) return UTF8_String;

   procedure Set_Renderer
     (Layer    : in out Graph_Paper_Annotation_Layer;
      Renderer : Renderer_Function);

   --
   -- Set_Suffix -- A text to suffix all annotation texts
   --
   --    Layer  - The annotation layer
   --    Suffix - The text
   --
   -- The specified text is added to the end of all annotation texts
   --
   procedure Set_Suffix
     (Layer  : in out Graph_Paper_Annotation_Layer;
      Suffix : UTF8_String);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Graph_Paper_Annotation_Layer;

   overriding procedure Changed
     (Layer  : in out Graph_Paper_Annotation_Layer;
      Paper  : Graph_Paper_Layer'Class;
      Box    : Cairo.Ellipses.Cairo_Box;
      X1, X2 : X_Axis;
      Y1, Y2 : Y_Axis);

   overriding procedure Detached
     (Annotation : in out Graph_Paper_Annotation_Layer);

   overriding procedure Draw
     (Layer   : in out Graph_Paper_Annotation_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk_Rectangle);

   overriding procedure Finalize (Layer : in out Graph_Paper_Annotation_Layer);

   overriding function Get_Properties_Number
     (Layer : Graph_Paper_Annotation_Layer) return Natural;

   overriding function Get_Property_Specification
     (Layer    : Graph_Paper_Annotation_Layer;
      Property : Positive) return Param_Spec;

   overriding function Get_Property_Value
     (Layer    : Graph_Paper_Annotation_Layer;
      Property : Positive) return Glib.Values.GValue;

   overriding function Get_Scaled (Layer : Graph_Paper_Annotation_Layer)
                                   return Boolean;

   overriding function Is_Updated (Layer : Graph_Paper_Annotation_Layer)
                                   return Boolean;

   overriding procedure Move
     (Layer  : in out Graph_Paper_Annotation_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple);

   overriding function Render
     (Layer  : Graph_Paper_Annotation_Layer;
      Value  : Gdouble;
      Raster : Gtk.Layered.Waveform.Rasters.Scale) return UTF8_String;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Graph_Paper_Annotation_Layer);

   overriding procedure Scale
     (Layer  : in out Graph_Paper_Annotation_Layer;
      Factor : Gdouble);

   overriding procedure Set_Property_Value
     (Layer    : in out Graph_Paper_Annotation_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue);

   overriding procedure Set_Scaled
     (Layer  : in out Graph_Paper_Annotation_Layer;
      Scaled : Boolean);

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Graph_Paper_Annotation_Layer);

   --
   -- Graph_Paper_Time_Annotation_Layer -- A layer derived from graph paper
   --                                      annotation layer,  which renders
   -- seconds in the human readable form.
   --
   type Graph_Paper_Time_Annotation_Layer (<>) is
     new Graph_Paper_Annotation_Layer with private;

   --
   -- Add_Graph_Paper_Time_Annotation -- Add annotation texts
   --
   --    Under       - The layer or widget where to place it under
   --    Paper       - The graph paper layer
   --    Location    - The annotation location
   --    Face        - The text font
   --    Height      - The text height
   --    Stretch     - The text width scale relative to its height
   --    Color       - The text color
   --    Text_Angle  - The text angle
   --    Justify_X   - The text box horizontal alignment after rotation
   --    Justify_Y   - The text box vertical alignment after rotation
   --    Superscript - Superscript is allowed for exponents
   --    Background  - The color of the text background box
   --    Border      - Added to the text extents box to get background box
   --    Overlap     - The text box overlap
   --    Opacity     - The text backgound box's opacity
   --    Scaled      - The layer is scaled together with the parent
   --    Enlarged    - The annotation texts enlarged with the parent
   --
   -- See the description of Add_Graph_Paper_Annotation.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Graph_Paper_Time_Annotation
     (Under       : not null access Layer_Location'Class;
      Paper       : not null access Graph_Paper_Layer'Class;
      Location    : Axis_Location       :=
        (Orientation => Horizontal,
         Alignment   => Absolute,
         Left        => -0.5,
         Right       => -0.5,
         Y_Position  =>  0.0);
      Face        : Pango_Cairo_Font    :=
        Create_Pango ("arial unicode ms");
      Height      : Gdouble             := 12.0;
      Stretch     : Gdouble             := 1.0;
      Color       : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble             := 0.0;
      Justify_X   : Alignment           := Center;
      Justify_Y   : Vertical_Alignment  := Center;
      Superscript : Boolean             := True;
      Background  : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Border      : Gdouble             := 2.0;
      Overlap     : Gdouble             := -2.0;
      Opacity     : Fill_Opacity        := 1.0;
      Scaled      : Boolean             := False;
      Enlarged    : Boolean             := False);

   function Add_Graph_Paper_Time_Annotation
     (Under       : not null access Layer_Location'Class;
      Paper       : not null access Graph_Paper_Layer'Class;
      Location    : Axis_Location       :=
        (Orientation => Horizontal,
         Alignment   => Absolute,
         Left        => -0.5,
         Right       => -0.5,
         Y_Position  =>  0.0);
      Face        : Pango_Cairo_Font    :=
        Create_Pango ("arial unicode ms");
      Height      : Gdouble             := 12.0;
      Stretch     : Gdouble             := 1.0;
      Color       : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble             := 0.0;
      Justify_X   : Alignment           := Center;
      Justify_Y   : Vertical_Alignment  := Center;
      Superscript : Boolean             := True;
      Background  : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Border      : Gdouble             := 2.0;
      Overlap     : Gdouble             := -2.0;
      Opacity     : Fill_Opacity        := 1.0;
      Scaled      : Boolean             := False;
      Enlarged    : Boolean             := False)
      return not null access Graph_Paper_Time_Annotation_Layer;

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Graph_Paper_Time_Annotation_Layer;

   overriding function Add_Graph_Paper_Annotation
     (Under       : not null access Layer_Location'Class;
      Paper       : not null access Graph_Paper_Layer'Class;
      Location    : Axis_Location;
      Face        : Pango_Cairo_Font;
      Height      : Gdouble;
      Stretch     : Gdouble;
      Color       : Gdk.Color.Gdk_Color;
      Text_Angle  : Gdouble;
      Justify_X   : Alignment;
      Justify_Y   : Vertical_Alignment;
      Superscript : Boolean;
      Background  : Gdk.Color.Gdk_Color;
      Border      : Gdouble;
      Overlap     : Gdouble;
      Opacity     : Fill_Opacity;
      Scaled      : Boolean;
      Enlarged    : Boolean)
      return not null access Graph_Paper_Time_Annotation_Layer renames
     Add_Graph_Paper_Time_Annotation;

   overriding function Render
     (Layer  : Graph_Paper_Time_Annotation_Layer;
      Value  : Gdouble;
      Raster : Gtk.Layered.Waveform.Rasters.Scale) return UTF8_String;

private

   type Annotation_Text (Size : Natural) is record
      Length : Natural;
      Buffer : UTF8_String (1 .. Size);
   end record;
   type Annotation_Text_Ptr is access Annotation_Text;
   function "+" (Value : Annotation_Text_Ptr) return UTF8_String with Inline;

   type Annotation_List is
     array (Positive range <>) of Annotation_Text_Ptr;
   type Annotation_List_Ptr is access Annotation_List;

   type Graph_Paper_Annotation_Layer is
     new Abstract_Layer
     and Scalable_Layer
     and Graph_Paper_Annotation_Interface with
      record
         Location     : Axis_Location;
         Paper        : access Graph_Paper_Layer;
         Face         : Pango_Cairo_Font;
         Height       : Gdouble;
         Stretch      : Gdouble;
         Text_Angle   : Gdouble;
         Color        : Gdk.Color.Gdk_Color;
         Background   : Gdk.Color.Gdk_Color;
         Border       : Gdouble;
         Overlap      : Gdouble;
         Opacity      : Fill_Opacity;
         Texts        : Annotation_List_Ptr;
         Suffix       : Annotation_Text_Ptr;
         Justify_X    : Alignment;
         Justify_Y    : Vertical_Alignment;
         Renderer     : Renderer_Function;
         Enlarged     : Boolean                            := False;
         Scaled       : Boolean                            := False;
         Updated      : Boolean                            := True;
         Superscript  : Boolean                            := True;
         -- Parent's data, set when Updated is true
         T1, T2       : Gdouble                            := 0.0; -- Horizontal range
         V1, V2       : Gdouble                            := 0.0; -- Vertical range
         Box          : Cairo.Ellipses.Cairo_Box           := (others => 0.0);
         Raster       : Gtk.Layered.Waveform.Rasters.Scale :=
                          (Minor     => 1.0,
                           Low_Value => 0.0,
                           Low_Tick  => 0,
                           Ticks     => 1,
                           Small     => 0);
         -- The last value with annotations closest to the middle
         Middle_Value : Gdouble                            := Gdouble'First;
      end record;
   type Graph_Paper_Annotation_Ptr is
     access all Graph_Paper_Annotation_Layer;

   procedure Set_Text
     (Layer    : in out Graph_Paper_Annotation_Layer;
      Position : Positive;
      Text     : UTF8_String);

   type Graph_Paper_Time_Annotation_Layer is
     new Graph_Paper_Annotation_Layer with null record;
   type Graph_Paper_Time_Annotation_Ptr is
     access Graph_Paper_Time_Annotation_Layer;

end Gtk.Layered.Graph_Paper_Annotation;
