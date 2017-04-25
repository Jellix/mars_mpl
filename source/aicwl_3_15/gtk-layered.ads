--                                                                    --
--  package Gtk.Layered             Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Autumn, 2010       --
--                                                                    --
--                                Last revision :  09:08 05 Mar 2017  --
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

with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Streams;             use Ada.Streams;
with Cairo;                   use Cairo;
with Cairo.Ellipses;          use Cairo.Ellipses;
with Glib.Values;             use Glib.Values;
with Gdk.Event;               use Gdk.Event;
with Gdk.Color;               use Gdk.Color;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gtk.Adjustment;          use Gtk.Adjustment;
with Gtk.Drawing_Area;        use Gtk.Drawing_Area;
with Gtk.Enums.String_Lists;  use Gtk.Enums.String_Lists;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Widget;              use Gtk.Widget;
with Interfaces.C;            use Interfaces.C;
with Pango.Cairo.Fonts;       use Pango.Cairo.Fonts;
with System;                  use System;

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with Gtk.Handlers;

package Gtk.Layered is
--
-- End_Parameters -- Parameters of a needle end
--
--    Length - The distance of the end to the center (can be negative)
--    Width  - The width of the needle in the end
--    Cap    - The style of the end
--
   type End_Parameters is record
      Length : GDouble;
      Width  : GDouble;
      Cap    : Cairo_Line_Cap;
   end record;
--
-- Text_Transformation -- Alignment and transformation of texts
--
-- The text  is transformed  and aligned in accordance  to a vector with
-- the coordinates X, Y and the angle Alpha:
--
-- (o)  Moved_Inside  moves  the  text  to X, Y so that a curve with the
--      vector as its perpendicular would bound the text from outside.
-- (o)  Moved_Centered  moves  the text to X, Y so that a curve with the
--      vector as its perpendicular would go through the text's center.
-- (o)  Moved_Outside moves the text to X, Y so that a  curve  with  the
--      vector as its perpendicular would bound the text from inside.
-- (o)  Rotated moves the text's center to X, Y  rotating  the  text  so
--      that its vertical axis would be parallel to the vector.
-- (o)  Skewed  moves the text's center to X, Y skewing and rotating the
--      text  so  that  its  vertical axis would be perpendicular to the
--      vector  and  its horizontal axis would be parallel to some third
--      vector  (e.g. the major axis of some ellipse). The height of the
--      text's parallelogram is kept equal to the original height of the
--      text.
--
   type Text_Transformation is
        (  Moved_Inside,
           Moved_Centered,
           Moved_Outside,
           Rotated,
           Skewed
        );
--
-- Interpolation_Mode -- Interpolation mode
--
--    Left   - The value at the left point is used
--    Linear - The value is interpolated between two points
--
   type Interpolation_Mode is (Left, Linear);
--
-- Line_Parameters -- Parameters of a line
--
--    Width    - The line width
--    Color    - The line color
--    Line_Cap - The way lines ends
--
   type Line_Parameters is record
      Width    : GDouble;
      Color    : Gdk_Color;
      Line_Cap : Cairo_Line_Cap;
   end record;
--
-- Tick parameters -- Parameters of a gauge scale
--
--    Step    - The angular step of the tick
--    First   - The number of the first tick
--    Skipped - The number of the skipped tick
--
-- Ticks  are  drawn  at  angles incremented by Step. Ticks are numbered
-- from 1 to Skipped and again from 1 etc. The  ticks  with  the  number
-- equal to Skipped are not drawn.
--
   subtype Tick_Number is Integer range 1..1_000_000;
   type Tick_Parameters is record
       Step    : GDouble;
       First   : Tick_Number;
       Skipped : Tick_Number;
   end record;
--
-- Elliptic_Shape_Type -- Types of shapes bound by an elliptic arc
--
   type Elliptic_Shape_Type is (Sector, Segment, Bagel);
--
-- Elliptic_Arc_Closure -- Parameters of an elliptic closure
--
   type Elliptic_Arc_Closure
        (  Shape : Elliptic_Shape_Type := Sector
        )  is
   record
      case Shape is
         when Sector  => Center : Cairo_Tuple;
         when Bagel   => Arc    : Ellipse_Parameters;
         when Segment => null;
      end case;
   end record;
--
-- Fill_Opacity -- Opacity, when filling shapes
--
   subtype Fill_Opacity is GDouble range 0.0..1.0;
--
-- Vertical alignment -- Alignment along the vertical axis
--
   type Vertical_Alignment is (Top, Center, Bottom);
--
-- Waveform_Drawing_Method -- The method of the waveform
--
--    Resample_New_And_Stroke - The  historic  part  of  the waveform is
--                              moved  the new one is resampled and then
--                              the waveform line is drawn using Stroke.
--    Resample_All_And_Stroke - The waveform is sampled new and then the
--                              waveform line is drawn using Stroke.
--
   type Waveform_Drawing_Method is
        (  Resample_New_And_Stroke,
           Resample_All_And_Stroke
        );
--
-- Layer_Object -- The layer object's interface
--
   type Layer_Object is limited interface;
------------------------------------------------------------------------
-- Layer_Needle
--
   type Gauge_Needle is limited interface;
--
-- Get_Adjustment -- The adjustment object used by the needle
--
--    Layer - The needle
--
-- Returns :
--
--    The adjustment object or else null
--
   function Get_Adjustment (Layer : Gauge_Needle)
      return Gtk_Adjustment is abstract;
--
-- Get_Value -- Get the value indicated by the needle
--
--    Layer - The needle
--
-- The value is in  the  range  0.0..1.0.  The  implementation  must  be
-- task-safe, callable on the context of a task different from  the  GTK
-- loop task.
--
-- Returns :
--
--    The value
--
   function Get_Value (Layer : Gauge_Needle) return GDouble is abstract;
--
-- Set_Value -- Change the value indicated by the needle
--
--    Layer - The needle
--    Value - The value in the range 0.0..1.0
--
-- When  the value is out of range it is saturated to the nearest bound.
-- The implementation must be task-safe, callable on the  context  of  a
-- task different from the GTK loop task. Note that changing  the  value
-- has  no immediate effect on the widget the needle's layer belongs. In
-- order to redraw it you should call Queue_Draw.
--
   procedure Set_Value
             (  Layer : in out Gauge_Needle;
                Value : GDouble
             )  is abstract;
------------------------------------------------------------------------
-- Layer_Location -- Where a layer can be placed
--
   type Layer_Location is limited interface;
--
-- Add -- Place layer object under the location specified
--
--    Layer - The layer object
--    Under - The layer object location
--
   procedure Add
             (  Layer : not null access Layer_Object;
                Under : not null access Layer_Location'Class
             )  is abstract;
------------------------------------------------------------------------
-- Annotation_Layer -- Interface of the annotation layers
--
   type Annotation_Layer is limited interface;
--
-- Get_Face -- Text font face
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The font face
--
   function Get_Face (Layer : Annotation_Layer)
      return Pango_Cairo_Font is abstract;
--
-- Get_Height -- The text height
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The text height
--
   function Get_Height (Layer : Annotation_Layer)
      return GDouble is abstract;
--
-- Get_Markup -- The annotation text markup
--
--    Layer    - The annotation layer
--    Position - The text number 1..Get_Texts_Number
--
-- The texts are drawn at ticks in their postion order.
--
-- Returns :
--
--    True if the text uses pango markup
--
-- Exceptions :
--
--    Constraint_Error -- Position is greater than the texts number
--
   function Get_Markup
            (  Layer    : Annotation_Layer;
               Position : Positive
            )  return Boolean is abstract;
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
   function Get_Stretch (Layer : Annotation_Layer)
      return GDouble is abstract;
--
-- Get_Text -- The annotation text
--
--    Layer    - The annotation layer
--    Position - The text number 1..Get_Texts_Number
--
-- The texts are drawn at ticks in their postion order.
--
-- Returns :
--
--    The text
--
-- Exceptions :
--
--    Constraint_Error -- Position is greater than the texts number
--
   function Get_Text
            (  Layer    : Annotation_Layer;
               Position : Positive
            )  return UTF8_String is abstract;
--
-- Get_Texts_Number -- The number of texts
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The number of texts the annotation draws
--
   function Get_Texts_Number (Layer : Annotation_Layer)
      return Natural is abstract;
--
-- Get_Ticks -- The ticks where texts are located
--
--    Layer - The annotation layer
--
-- Returns :
--
--    The ticks parameters
--
   function Get_Ticks (Layer : Annotation_Layer)
      return Tick_Parameters is abstract;
--
-- Set_Face -- Set font face
--
--    Layer - The annotation layer
--    Face  - The text font
--
   procedure Set_Face
             (  Layer : in out Annotation_Layer;
                Face  : Pango_Cairo_Font
             )  is abstract;
--
-- Set_Text -- Set an annotation text
--
--    Layer    - The annotation layer
--    Position - The text number 1..Get_Texts_Number
--    Text     - The new text
--    Markup   - True if the text contains markup directives
--
-- The texts are drawn at ticks in their postion order.
--
-- Exceptions :
--
--    Constraint_Error -- Position is greater than the texts number + 1
--
   procedure Set_Text
             (  Layer    : in out Annotation_Layer;
                Position : Positive;
                Text     : UTF8_String;
                Markup   : Boolean := False
             )  is abstract;
--
-- Set_Texts -- Set annotation texts
--
--    Layer       - The annotation layer
--    Texts       - The new texts
--  [ Delimiter ] - Text delimiter
--    Markup      - True if the texts contain markup directives
--
-- The texts are drawn at ticks in their postion order. The texts can be
-- specified  as a list, a controlled list (in the form "a"/"b"/"c"), or
-- as a single string separated by the Delimiter character.
--
   procedure Set_Texts
             (  Layer  : in out Annotation_Layer;
                Texts  : Gtk.Enums.String_List.GList;
                Markup : Boolean := False
             )  is abstract;
   procedure Set_Texts
             (  Layer  : in out Annotation_Layer'Class;
                Texts  : Controlled_String_List;
                Markup : Boolean := False
             );
   procedure Set_Texts
             (  Layer     : in out Annotation_Layer;
                Texts     : UTF8_String;
                Delimiter : Character := ' ';
                Markup    : Boolean   := False
             )  is abstract;
------------------------------------------------------------------------
-- Scalable_Layer -- Interface of scalable layers
--
   type Scalable_Layer is limited interface;
--
-- Get_Scaled -- The behavior when the parent widget is resized
--
--    Layer  - The layer
--
-- Returns :
--
--    The scaling mode
--
   function Get_Scaled (Layer : Scalable_Layer)
      return Boolean is abstract;
--
-- Set_Scaled -- Change the behavior when the parent widget is resized
--
--    Layer  - The layer
--    Scaled - Scaling mode
--
   procedure Set_Scaled
             (  Layer  : in out Scalable_Layer;
                Scaled : Boolean
             )  is abstract;
------------------------------------------------------------------------
-- Widened_Layer -- Interface of the layers which lines are widened
--
   type Widened_Layer is limited interface;
--
-- Get_Widened -- The behavior when the parent widget is resized
--
--    Layer  - The layer
--
-- Returns :
--
--    The line scaling mode
--
   function Get_Widened (Layer : Widened_Layer)
      return Boolean is abstract;
--
-- Set_Widened -- Change the behavior when the parent widget is resized
--
--    Layer   - The layer
--    Widened - Line widening mode
--
   procedure Set_Widened
             (  Layer   : in out Widened_Layer;
                Widened : Boolean
             )  is abstract;
------------------------------------------------------------------------
-- Gtk_Layered_Record -- The graphical widget containing layers
--
-- Signals:
--
--    layer-added   - emitted  when  the layer has been added. The first
--                    parameter is the position of the layer.
--    layer-removed - emitted when the layer has been removed. The first
--                    parameter is the position the removed layer had.
--
   type Gtk_Layered_Record is
      new Gtk_Widget_Record and Layer_Location with private;
   type Gtk_Layered is access all Gtk_Layered_Record'Class;
--
-- Abstract_Layer
--
   type Abstract_Layer is
      abstract new Ada.Finalization.Limited_Controlled
               and Layer_Object
               and Layer_Location with private;
------------------------------------------------------------------------
-- Operations of layered widgets:
--
-- Erase -- Deletes all layers of the widget
--
--    Widget - The widget
--
   procedure Erase (Widget : in out Gtk_Layered_Record);
--
-- Finalize -- Destruction is called upon destruction
--
--    Widget - The widget being destroyed
--
   procedure Finalize (Widget : in out Gtk_Layered_Record) is null;
--
-- Get_Aspect_Ratio -- Get the widget aspect ratio
--
--    Widget - The widget
--
-- Returns :
--
--    Get the aspect ratio
--
   function Get_Aspect_Ratio
            (  Widget : not null access constant Gtk_Layered_Record
            )  return GDouble;
--
-- Get_Bottom -- The bottom layer of the widget
--
--    Widget - The layered widget
--
-- Returns :
--
--    The most bottom layer or the widget itself when depth is 0
--
   function Get_Bottom (Widget : not null access Gtk_Layered_Record)
      return not null access Layer_Location'Class;
--
-- Get_Depth -- Get maximal depth of the widget
--
--    Widget - The layered widget
--
-- Returns :
--
--    Number of layers the widget presently has
--
   function Get_Depth
            (  Widget : not null access constant Gtk_Layered_Record
            )  return Natural;
--
-- Get_Center -- The drawn widget center
--
--    Widget - The widget
--
-- Returns :
--
--    Current center of the widget when the layers are drawn
--
   function Get_Center
            (  Widget : not null access constant Gtk_Layered_Record
            )  return Cairo_Tuple;
--
-- Get_Drawing_Time -- The topmost layer of the widget
--
--    Widget - The layered widget
--
-- This function returns the time of drawing. The  widgets  which  state
-- depend on real time should use this value when draw their states.
--
-- Returns :
--
--    The time the state of layers being drawn must reflect
--
   function Get_Drawing_Time
            (  Widget : not null access constant Gtk_Layered_Record
            )  return Time;
--
-- Get_Layer -- Get the layer by its number
--
--    Widget - The widget
--    Layer  - The layer position 1..Get_Depth (Widget)
--
-- The deepest layer has the number 1. The topmost layer has the  number
-- Get_Depth (Widget).
--
-- Returns :
--
--    The layer with the number Layer or null
--
   function Get_Layer
            (  Widget : not null access Gtk_Layered_Record;
               Layer  : Positive
            )  return access Abstract_Layer'Class;
--
-- Get_Lower -- The bottom layer of the widget
--
--    Widget - The layered widget
--
-- Returns :
--
--    The bottom layer or null
--
   function Get_Lower (Widget : not null access Gtk_Layered_Record)
      return access Abstract_Layer'Class;
--
-- Get_Size -- The drawn widget size
--
--    Widget - The widget
--
-- Returns :
--
--    Current size of the widget when the layers are drawn
--
   function Get_Size
            (  Widget : not null access constant Gtk_Layered_Record
            )  return GDouble;
--
-- Get_Type -- The type of the widget
--
-- Returns :
--
--    The GTK type of the widget
--
   function Get_Type return GType;
--
-- Get_Upper -- The topmost layer of the widget
--
--    Widget - The layered widget
--
-- Returns :
--
--    The topmost layer or null
--
   function Get_Upper (Widget : not null access Gtk_Layered_Record)
      return access Abstract_Layer'Class;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--
   procedure Gtk_New (Widget : out Gtk_Layered);
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The widget to initialize
--
   procedure Initialize
             (  Widget : not null access Gtk_Layered_Record'Class
             );
--
-- Insert -- A layer at the specified depth
--
--    Widget   - The widget
--    Layer    - The layer to move
--    Position - The layer's new depth
--
-- This  procedure  moves or adds the layer to the depth To. If Position
-- is greater than Get_Depth (Widget) the layer is moved to the widget's
-- top.
--
   procedure Insert
             (  Widget   : not null access Gtk_Layered_Record'Class;
                Layer    : in out Abstract_Layer'Class;
                Position : Positive
             );
--
-- Refresh -- Refresh the widget contents
--
--    Widget  - The widget
--    Context - Drawing context
--
-- This procedure should not be called directly. Use standard GTK+ means
-- instead, e.g. Queue_Draw.
--
   procedure Refresh
             (  Widget  : not null access Gtk_Layered_Record;
                Context : Cairo_Context
             );
--
-- Remove -- Layer by its number
--
--    Widget - The widget
--    Layer  - The layer position 1..Get_Depth (Widget)
--
   procedure Remove
             (  Widget : not null access Gtk_Layered_Record;
                Layer  : Positive
             );
--
-- Resized -- Called upon widget size or position change
--
--    Widget     - The widget
--    Allocation - The new widget position and size
--
   procedure Resized
             (  Widget     : not null access Gtk_Layered_Record;
                Allocation : Gtk_Allocation
             )  is null;
--
-- Set_Aspect_Ratio -- Set the widget aspect ratio
--
--    Widget       - The widget
--    Aspect_Ratio - The aspect ratio
--
-- The widget aspect ratio is the relation of the widget contents' width
-- to the height. The ratio influences the way  the  widget  layers  are
-- scaled with the widget (when marked as scaled). When the layer's Draw
-- is called the parameter  Size  determines  the  widget's  size.  This
-- parameter  is  computed from the actual widget's width and height and
-- the aspect ratio:
--
--     Size := Height when Width / Height >= Aspect_Ratio
--     Size := Width  when Width / Height < Aspect_Ratio
--
-- The default aspect ratio is 1. This is a good choice for the contents
-- bounded by a circle.
--
-- Exceptions :
--
--    Constraint_Error - Illegal aspect ratio
--
   procedure Set_Aspect_Ratio
             (  Widget       : not null access Gtk_Layered_Record;
                Aspect_Ratio : GDouble
             );
--
-- Snapshot -- Transfer the contents of the widget onto a surface
--
--    Widget - The widget
--    Target - A handle to the surface / context
--
-- This procedure  is used for taking snapshots of the widget.  E.g. for
-- rendering  its contents  into a PDF file etc.  Note that  this is not
-- conventional drawing, for which see Queue_Draw.
--
   procedure Snapshot
             (  Widget : not null access Gtk_Layered_Record;
                Target : Cairo_Surface
             );
   procedure Snapshot
             (  Widget : not null access Gtk_Layered_Record;
                Target : Cairo_Context
             );
--
-- Style_Changed -- The widget style was set
--
--    Widget - The widget
--
   procedure Style_Changed
             (  Widget : not null access Gtk_Layered_Record
             )  is null;
------------------------------------------------------------------------
-- Operations of abstract layers:
--
-- Above -- The layer above this one
--
--    Layer - The layer
--
-- Returns :
--
--    The layer above or null
--
   function Above (Layer : Abstract_Layer)
      return access Abstract_Layer'Class;
--
-- Atop -- The location atop of the layer
--
--    Layer - The layer
--
-- Returns :
--
--    The location atop layer
--
   function Atop (Layer : Abstract_Layer)
      return not null access Layer_Location'Class;
--
-- Add -- Add a new layer which parameters are read from a stream
--
--    Under  - The layer or widget where to place the region under
--    Stream - The to read the parameters from
--
-- Returns :
--
--    The layer
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--    I/O errors
--
   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Abstract_Layer is abstract;
--
-- Below -- The layer below this one
--
--    Layer - The layer
--
-- Returns :
--
--    The layer below or null
--
   function Below (Layer : Abstract_Layer)
      return access Abstract_Layer'Class;
--
-- Draw -- The layer
--
--    Layer   - The layer to draw
--    Context - The drawing context
--    Area    - The drawing area
--
-- The  implementation  need not to draw outside Area rectangle. After a
-- successful call to Draw, Is_Updated should return False.  The  layer,
-- when  scalable  should consider the widget's center in Get_Center and
-- its size of Get_Size.
--
   procedure Draw
             (  Layer   : in out Abstract_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is abstract;
--
-- Finalize -- Destruction
--
--    Layer - The layer being finalized
--
-- When overridden the implementation must call this one from its body.
--
   procedure Finalize (Layer : in out Abstract_Layer);
--
-- Find_Property -- Get number of layer properties
--
--    Layer      - The layer
--    Name       - The property name
--    Constraint - The expected property type
--
-- This  function  returns  the  number of the property of the specified
-- name.  The result is 0 if the layer does not have this property. When
-- Constraint is not GType_Invalid the type of the property must match.
--
-- Returns :
--
--    The property number or 0 if no property found
--
   function Find_Property
            (  Layer      : Abstract_Layer'Class;
               Name       : String;
               Constraint : GType := GType_Invalid
            )  return Natural;
--
-- Find_Property -- Get number of layer properties
--
--    Layer   - The layer
--    Pattern - The expected property
--
-- This  function  returns  the number of the property matching Pattern.
-- The property matches pattern if the nick name,  type,  minimum  (when
-- applied), maximum (when applied) match.
--
-- Returns :
--
--    The property number or 0 if no property found
--
   function Find_Property
            (  Layer      : Abstract_Layer'Class;
               Constraint : Param_Spec
            )  return Natural;
--
--
-- Get_Position -- Get the layer's position
--
--    Layer - The layer
--
-- Returns :
--
--    The layer's position or 0 if the layer is not of any widget
--
   function Get_Position (Layer : Abstract_Layer) return Natural;
--
-- Get_Properties_Number -- Get number of layer properties
--
--    Layer - The layer
--
-- Layers are not GLib objects and thus do not have properties of  their
-- own. But they support properties interface. The layer  can  be  asked
-- for its "properties" and  their  values  can  be  get  and  set  thus
-- influencing  the  layer's parameters. This can be used by the layered
-- widget in different ways. The styles and properties of the widget may
-- map to the layer's "properties." An application can control layers of
-- a widget by setting the "properties" of the layers.
--
-- Returns :
--
--    The number of properties
--
   function Get_Properties_Number (Layer : Abstract_Layer)
      return Natural is abstract;
--
-- Get_Property_Specification -- Get specification of a property
--
--    Layer    - The layer
--    Property - The property number
--
-- The result must be released by the caller using Unref.
--
-- Returns :
--
--    The property specification
--
-- Exceptions :
--
--    Constraint_Error - The property number is invalid
--
   function Get_Property_Specification
            (  Layer    : Abstract_Layer;
               Property : Positive
            )  return Param_Spec is abstract;
--
-- Get_Property_Value -- Get the value of a property
--
--    Layer    - The layer
--    Property - The property number
--
-- The result must be released by the caller using Unset.
--
-- Returns :
--
--    The property value
--
-- Exceptions :
--
--    Constraint_Error - The property number is invalid
--
   function Get_Property_Value
            (  Layer    : Abstract_Layer;
               Property : Positive
            )  return GValue is abstract;
--
-- Get_Widget -- Get the layer's widget
--
--    Layer - The layer
--
-- Returns :
--
--    The layer's widget
--
   function Get_Widget (Layer : Abstract_Layer)
      return not null access Gtk_Layered_Record'Class;
--
-- Is_Caching -- Check if the layer is a caching layer
--
--    Layer - The layer
--
-- Returns :
--
--    True when the layer is caching
--
   function Is_Caching (Layer : Abstract_Layer) return Boolean;
--
-- Is_Updated -- Update check
--
--    Layer - The layer
--
-- Returns :
--
--    True when the layer contents was changed
--
   function Is_Updated (Layer : Abstract_Layer)
      return Boolean is abstract;
--
-- Move -- The layer in XY-surface
--
--    Layer  - The layer
--    Offset - To move by
--
-- The  implementation  moves  all geometric shapes the layer draws from
-- the original position (x,y) to the position (x,y) + Offset. Note that
-- when  the  layer  implements  the   interface   Scalable_Layer,   and
-- Get_Scaled   returns   true,  then  the  effective  Offset  in  cairo
-- coordinates will depend on the current widget's size  (see Get_Size),
-- on  which  Offset is multiplied. Therefore to move the scalable layer
-- in cairo coordinates, Offset must be divided to  the  widget's  size.
-- Note also that some layers, e.g. ones with border, influence Get_Size
-- of the layers above.
--
   procedure Move
             (  Layer  : in out Abstract_Layer;
                Offset : Cairo_Tuple
             )  is abstract;
--
-- Prepare -- Called when the widget's layers are about to be drawn
--
--    Layer   - The layer to draw
--    Context - The drawing context
--    Area    - The drawing area
--
-- This procedure is called for all layers before  drawing  occurs.  The
-- implementation shall not change the list of widget layers nor perform
-- drawing onto Context. The default implementation does nothing.
--
   procedure Prepare
             (  Layer   : in out Abstract_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is null;
--
-- Property_Set -- Called when a property of the layer's widget is set
--
--    Layer - The layer
--    Param - The property parameter specification
--
-- The  procedure  is  called  when  the layer calls Install_Property at
-- least once from inside  the  body  of  Register.  The  implementation
-- should check if the layer's appearance need to be changed  because  a
-- property of the layer's widget has been set.
--
   procedure Property_Set
             (  Layer : in out Abstract_Layer;
                Param : Param_Spec
             )  is null;
--
-- Resized -- Called when the layer's widget is resized
--
--    Layer - The layer
--    Area  - The widget's rectangle
--
   procedure Resized
             (  Layer : in out Abstract_Layer;
                Area  : Gdk_Rectangle
             )  is null;
--
-- Restore -- The layer from streamed properties
--
--    Stream - The stream to write the layer's properties into
--    Layer  - The layer being restore
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--    I/O errors
--
   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Abstract_Layer
             )  is abstract;
--
-- Scale -- The layer in XY-surface
--
--    Layer  - The layer
--    Factor - Magnification factor
--
-- The  implementation magnifies all geometric shapes the layer draws by
-- Factor.
--
-- Exceptions :
--
--    Constraint_Error - Illegal factor
--
   procedure Scale
             (  Layer  : in out Abstract_Layer;
                Factor : GDouble
             )  is abstract;
--
-- Set_Property_Value -- Set value of a property
--
--    Layer    - The layer
--    Property - The property number
--    Value    - The property value
--
-- Exceptions :
--
--    Constraint_Error - The property number is invalid
--
   procedure Set_Property_Value
             (  Layer    : in out Abstract_Layer;
                Property : Positive;
                Value    : GValue
             )  is abstract;
--
-- Store -- Set cache from the context
--
--    Layer   - A caching layer
--    Context - To read cache from
--
-- For  the  layers which return True from Is_Caching, this procedure is
-- called  when  all underlying layers are drawn. The layer should cache
-- the  image  drawn.  Later  when the widget need to be redrawn and the
-- underlying layers  return  False  from  their  Is_Updated,  they  are
-- excluded from drawing and only the  Draw  of  the  caching  layer  is
-- called.
--
   procedure Store
             (  Layer   : in out Abstract_Layer;
                Context : Cairo_Context
             )  is null;
--
-- Store -- The layer properties
--
--    Stream - The stream to write the layer's properties into
--    Layer  - The layer
--
-- Exceptions :
--
--    I/O errors
--
   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Abstract_Layer
             )  is abstract;
--
-- Style_Set -- Called when the style of the layer's widget is set
--
--    Layer - The layer
--
-- The  procedure is called when the widget's style property is set. The
-- default implementation does nothing.
--
   procedure Style_Set (Layer : in out Abstract_Layer) is null;

   overriding
      procedure Add
                (  Layer : not null access Abstract_Layer;
                   Under : not null access Layer_Location'Class
                );

   function Image (Location : Address) return String;
private
   Min_Step : constant := 1.0E-9;

   type Abstract_Layer_Ptr is access all Abstract_Layer'Class;

   type Abstract_Layer is
       abstract new Ada.Finalization.Limited_Controlled
                and Layer_Object
                and Layer_Location with
   record
      Prev, Next : Abstract_Layer_Ptr;
      Widget     : Gtk_Layered;
   end record;
--
-- Remove -- Layer from the widget where it resides
--
--    Layer - The layer
--
-- Note, this does not destroy the object. Usually the widget need to be
-- redrawn and the layer object destroyed.
--
   procedure Remove (Layer : in out Abstract_Layer);
--
-- Gtk_Layered_Record -- The widget implementation
--
   type Gtk_Layered_Record is
      new Gtk_Drawing_Area_Record and Layer_Location with
   record
      Bottom        : Abstract_Layer_Ptr;
      Depth         : Natural     := 0;
      Aspect_Ratio  : GDouble     := 1.0;
      Center        : Cairo_Tuple := (0.0, 0.0);
      Size          : GDouble     := 0.0;
      Updated       : Boolean     := True;
      Drawing       : Boolean     := False;
      Drawing_Time  : Time        := Clock;
   end record;
--
-- Destroy -- Handler of "destroy"
--
   procedure Destroy
             (  Widget : access Gtk_Layered_Record'Class
             );
--
-- Draw -- Handler of "draw"
--
   function Draw
            (  Widget  : access Gtk_Layered_Record'Class;
               Context : Cairo.Cairo_Context
            )  return Boolean;
--
-- Emit -- Emit signal
--
   procedure Emit
             (  Widget : not null access Gtk_Layered_Record'Class;
                Signal : Signal_ID;
                Value  : GUInt
             );
--
-- Notify -- The property set event's callback
--
   procedure Notify
             (  Widget : access Gtk_Layered_Record'Class;
                Params : GValues
             );
--
-- Size_Allocate -- The size_allocate event's callback
--
   procedure Size_Allocate
             (  Widget     : access Gtk_Layered_Record'Class;
                Allocation : Gtk_Allocation_Access
             );
--
-- Style_Updated -- The style-updated event's callback
--
   procedure Style_Updated
             (  Widget : access Gtk_Layered_Record'Class
             );
--
-- Instantiations of the callback handlers
--
   package Return_Boolean_Callback is
      new Gtk.Handlers.Return_Callback
          (  Gtk_Layered_Record,
             Boolean
          );
   package Widget_Callback is
      new Gtk.Handlers.Callback (Gtk_Layered_Record);

   package Allocation_Marshaller is
      new Widget_Callback.Marshallers.Generic_Marshaller
          (  Gtk_Allocation_Access,
             Get_Allocation
          );

   pragma Inline (Get_Center);
   pragma Inline (Get_Size);
   pragma Inline (Get_Widget);

   function Get_First_Tick (First, Skipped : Tick_Number)
      return Tick_Number;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Abstract_Layer'Class,
             Abstract_Layer_Ptr
          );

   ---------------------------------------------------------------------
   type Tracing_Flags is mod 2**2;
   Trace_Amplifier : Tracing_Flags := 2**0;
   Trace_Waveform  : Tracing_Flags := 2**1;
   Tracing_Mode    : constant Tracing_Flags := 0;
   Trace_File : constant String  := "c:/temp/aicwl.txt";

   procedure Trace (Data : System.Address; Text : String);
   procedure Trace_Line (Data : System.Address; Text : String);
   ---------------------------------------------------------------------

end Gtk.Layered;
