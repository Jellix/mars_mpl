--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Graph_Paper                     Luebeck            --
--  Interface                                      Spring, 2011       --
--                                                                    --
--                                Last revision :  16:49 28 Feb 2016  --
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

with Gtk.Handlers.References;  use Gtk.Handlers.References;
with Gtk.Layered.Waveform;     use Gtk.Layered.Waveform;
with Gtk.Missed;

package Gtk.Layered.Graph_Paper is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""X_Axis""");
   pragma Warnings (Off, "declaration hides ""Y_Axis""");

   --
   -- Graph_Paper_Layer -- A layer showing graph paper
   --
   type Graph_Paper_Layer (<>) is
     new Abstract_Layer
     and Scalable_Layer
     and Widened_Layer with private;

   --
   -- Graph_Paper_Annotation_Interface -- A    graph    paper    annotation
   --                                     interface.   The   interface   is
   -- intended  to  receive notifications upon layer changing. Typically an
   -- annotation  implements  the  interface to place texts along the graph
   -- paper lines.
   --
   type Graph_Paper_Annotation_Interface is limited interface;

   ------------------------------------------------------------------------
   -- Changed -- Scaling changing callback
   --
   --    Annotation   - The annotation being notified
   --    Layer        - The graph paper layer
   --    Box          - The graph paper box
   --    From,  To    - The range of X-axis values
   --    Lower, Upper - The range of Y-axis values
   --
   -- Typically  an  annotation  connected  to  the   layer   queries   its
   -- dimensions and X/Y axis rasters to place its texts accordingly.
   --
   procedure Changed
     (Annotation   : in out Graph_Paper_Annotation_Interface;
      Layer        : Graph_Paper_Layer'Class;
      Box          : Cairo.Ellipses.Cairo_Box;
      From,  To    : X_Axis;
      Lower, Upper : Y_Axis) is abstract;

   --
   -- Detached -- Notification that the annotatioc was detached
   --
   --    Annotation - The annotation to detach
   --
   -- The procedure is  called  when Annotation  is detached  from  a graph
   -- paper layer.
   --
   procedure Detached
     (Annotation : in out Graph_Paper_Annotation_Interface) is null;

   --
   -- Render - Renders value to text
   --
   --    Layer  - The annotation layer
   --    Value  - The value to render
   --    Raster - The scale used to render the text
   --
   -- Returns :
   --
   --    Result text
   --
   function Render
     (Layer  : Graph_Paper_Annotation_Interface;
      Value  : Gdouble;
      Raster : Gtk.Layered.Waveform.Rasters.Scale)
      return UTF8_String is abstract;

   ------------------------------------------------------------------------
   -- Add_Graph_Paper -- Add graph paper
   --
   --    Under          - The layer or widget where to place it under
   --    Box            - The waveform's rectangle
   --    X_Tick_Length  - The approximate length of the major tick
   --    Y_Tick_Length  - The approximate length of the major tick
   --    Major_Width    - The major tick line width
   --    Minor_Width    - The minor tick line width
   --    Major_Color    - The major tick line color
   --    Minor_Color    - The minor tick line color
   --    Major_Line_Cap - The major tick line cap style
   --    Minor_Line_Cap - The minor tick line cap style
   --    X_Axis         - The horizontal axis adjustment
   --    Y_Axis         - The vertical axis adjustment
   --    Scaled         - The layer is scaled together with the parent
   --    Widened        - The lines are widened together with the widget
   --
   -- The  layer  has  vertical  lines  when  Y_Axis  is  not  null  and/or
   -- horizontal  lines  when  X_Axis is  not null.  The layer  scaling  is
   -- performed as follows:
   --
   -- (o)  The  x-coordinates  of  the  box  are multiplied by the widget's
   --      horizontal  size  and  then  added  to  the  x-coordinate of the
   --      widget's center;
   -- (o)  The  y-coordinates  of  the  box  are multiplied by the widget's
   --      vertical   size  and  then  added  to  the  y-coordinate  of the
   --      widget's center.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Graph_Paper
     (Under          : not null access Layer_Location'Class;
      Box            : Cairo.Ellipses.Cairo_Box;
      X_Tick_Length  : Positive                           := 50;
      Y_Tick_Length  : Positive                           := 50;
      Major_Width    : Gdouble                            := 1.0;
      Minor_Width    : Gdouble                            := 1.0;
      Major_Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Minor_Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.5, 0.5, 0.5);
      Major_Line_Cap : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      Minor_Line_Cap : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      X_Axis         : access Gtk_Adjustment_Record'Class := null;
      Y_Axis         : access Gtk_Adjustment_Record'Class := null;
      Scaled         : Boolean                            := False;
      Widened        : Boolean                            := False);

   function Add_Graph_Paper
     (Under          : not null access Layer_Location'Class;
      Box            : Cairo.Ellipses.Cairo_Box;
      X_Tick_Length  : Positive                           := 50;
      Y_Tick_Length  : Positive                           := 50;
      Major_Width    : Gdouble                            := 1.0;
      Minor_Width    : Gdouble                            := 1.0;
      Major_Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Minor_Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.5, 0.5, 0.5);
      Major_Line_Cap : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      Minor_Line_Cap : Cairo.Cairo_Line_Cap               := Cairo.Cairo_Line_Cap_Butt;
      X_Axis         : access Gtk_Adjustment_Record'Class := null;
      Y_Axis         : access Gtk_Adjustment_Record'Class := null;
      Scaled         : Boolean                            := False;
      Widened        : Boolean                            := False)
      return not null access Graph_Paper_Layer;

   --
   -- Attach -- Attach an axis annotation
   --
   --    Layer      - The graph paper layer
   --    Annotation - The annotation to attach
   --
   -- The attached annotation must be detached using Detach.  When attached
   -- the annotation receives notification callbacks.
   --
   procedure Attach
     (Layer      : in out Graph_Paper_Layer;
      Annotation : in out Graph_Paper_Annotation_Interface'Class);

   --
   -- Detach -- Detach an axis annotation
   --
   --    Layer      - The graph paper layer
   --    Annotation - The annotation to detach
   --
   procedure Detach
     (Layer      : in out Graph_Paper_Layer;
      Annotation : in out Graph_Paper_Annotation_Interface'Class);

   --
   -- Get_Box -- The box bounding the graph paper
   --
   --    Layer - The graph paper layer
   --
   -- Returns :
   --
   --    The box
   --
   function Get_Box (Layer : Graph_Paper_Layer) return Cairo.Ellipses.Cairo_Box;

   --
   -- Get_{Major|Minor}_Line -- Ticks line parameters
   --
   --    Layer - The graph paper layer
   --
   -- Returns :
   --
   --    The line parameters
   --
   function Get_Major_Line (Layer : Graph_Paper_Layer)
                            return Line_Parameters;
   function Get_Minor_Line (Layer : Graph_Paper_Layer)
                            return Line_Parameters;

   --
   -- Get_{X|Y}_Tick_Length -- Get approximate length of ticks
   --
   --    Sweeper - The adjustment object
   --
   -- Returns :
   --
   --    The approximate length of ticks in pixels
   --
   function Get_X_Tick_Length (Layer : Graph_Paper_Layer)
                               return Positive;
   function Get_Y_Tick_Length (Layer : Graph_Paper_Layer)
                               return Positive;

   --
   -- Get_{X|Y}_Axis -- Adjustment controlling the corresponding axis
   --
   --    Layer - The graph paper layer
   --
   -- When the result is null the graph paper lacks the lines perpendicular
   -- to the axis.
   --
   -- Returns :
   --
   --    The adjustment or null
   --
   function Get_X_Axis (Layer : Graph_Paper_Layer)
                        return Gtk_Adjustment;
   function Get_Y_Axis (Layer : Graph_Paper_Layer)
                        return Gtk_Adjustment;

   --
   -- Get_{X|Y}_Raster -- The scale of the corresponding axis
   --
   --    Layer - The graph paper layer
   --
   -- The exception Use_Error is propagated when the scale  information  is
   -- not available because the layer has been changed and not  yet  drawn,
   -- so the scale is undetermined. Note that this would be the case if the
   -- annotation  layer  is  drawn before the graph paper one. In that case
   -- the  caller probably should requeue another Draw operation and ignore
   -- the exception.
   --
   -- Returns :
   --
   --    The current scale of the axis
   --
   -- Exceptions :
   --
   --    Use_Error - The layer was changed
   --
   function Get_X_Raster (Layer : Graph_Paper_Layer)
                          return Gtk.Layered.Waveform.Rasters.Scale;
   function Get_Y_Raster (Layer : Graph_Paper_Layer)
                          return Gtk.Layered.Waveform.Rasters.Scale;

   --
   -- Set -- Parameters of the graph paper
   --
   --    Layer         - The graph paper layer
   --    Box           - The waveform's rectangle
   --    X_Tick_Length - The approximate length of the major tick
   --    Y_Tick_Length - The approximate length of the major tick
   --    Major_Line    - The tick lines parameters
   --    Minor_Line    - The tick lines parameters
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Set
     (Layer         : in out Graph_Paper_Layer;
      Box           : Cairo.Ellipses.Cairo_Box;
      X_Tick_Length : Positive;
      Y_Tick_Length : Positive;
      Major_Line    : Line_Parameters;
      Minor_Line    : Line_Parameters);

   --
   -- Set_{X|Y}_Axis -- Set the adjustment controlling the axis
   --
   --    Layer      - The graph paper layer
   --    Adjustment - The adjustment object or null
   --
   procedure Set_X_Axis
     (Layer      : not null access Graph_Paper_Layer;
      Adjustment : access Gtk_Adjustment_Record'Class);

   procedure Set_Y_Axis
     (Layer      : not null access Graph_Paper_Layer;
      Adjustment : access Gtk_Adjustment_Record'Class);

   --
   -- Set_{X|Y}_Tick_Length -- Set approximate length of major ticks
   --
   --    Layer  - The graph paper layer
   --    Length - The approximate length in pixels
   --
   -- This procedure  sets the approximate  length of major ticks used when
   -- the scaling fits at the raster.
   --
   procedure Set_X_Tick_Length
     (Layer  : not null access Graph_Paper_Layer;
      Length : Positive);

   procedure Set_Y_Tick_Length
     (Layer  : not null access Graph_Paper_Layer;
      Length : Positive);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Graph_Paper_Layer;

   overriding procedure Draw
     (Layer   : in out Graph_Paper_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk_Rectangle);

   overriding procedure Finalize (Layer : in out Graph_Paper_Layer);

   overriding function Get_Scaled (Layer : Graph_Paper_Layer) return Boolean;

   overriding function Get_Properties_Number
     (Layer : Graph_Paper_Layer) return Natural;

   overriding function Get_Property_Specification
     (Layer    : Graph_Paper_Layer;
      Property : Positive) return Param_Spec;

   overriding function Get_Property_Value
     (Layer    : Graph_Paper_Layer;
      Property : Positive) return Glib.Values.GValue;

   overriding function Get_Widened (Layer : Graph_Paper_Layer) return Boolean;

   overriding function Is_Updated (Layer : Graph_Paper_Layer) return Boolean;

   overriding procedure Move
     (Layer  : in out Graph_Paper_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple);

   overriding procedure Prepare
     (Layer   : in out Graph_Paper_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk_Rectangle);

   overriding procedure Resized
     (Layer : in out Graph_Paper_Layer;
      Area  : Gdk_Rectangle);

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Graph_Paper_Layer);

   overriding procedure Scale
     (Layer  : in out Graph_Paper_Layer;
      Factor : Gdouble);

   overriding procedure Set_Property_Value
     (Layer    : in out Graph_Paper_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue);

   overriding procedure Set_Scaled
     (Layer  : in out Graph_Paper_Layer;
      Scaled : Boolean);

   overriding procedure Set_Widened
     (Layer   : in out Graph_Paper_Layer;
      Widened : Boolean);

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Graph_Paper_Layer);

private

   type References_List is array (1 .. 4) of Handler_Reference;
   type Item
     (Annotation : not null access Graph_Paper_Annotation_Interface'Class)
   is limited
      record
         Previous : not null access Item := Item'Unchecked_Access;
         Next     : not null access Item := Item'Unchecked_Access;
      end record;
   type Item_Ptr is access all Item;

   type Graph_Paper_Layer is
     new Abstract_Layer and Scalable_Layer and Widened_Layer with
      record
         Box           : Cairo.Ellipses.Cairo_Box;
         Major_Line    : Line_Parameters;
         Minor_Line    : Line_Parameters;
         X_Axis        : Gtk_Adjustment;
         Y_Axis        : Gtk_Adjustment;
         X_Raster      : Gtk.Layered.Waveform.Rasters.Scale;
         Y_Raster      : Gtk.Layered.Waveform.Rasters.Scale;
         X_Tick_Length : Guint   := 50;
         Y_Tick_Length : Guint   := 50;
         Changed       : Boolean := True;  -- Raster is invalid
         Scaled        : Boolean := False;
         Widened       : Boolean := False;
         Updated       : Boolean := True;
         Handlers      : References_List;
         X_Sweeper     : access Waveform_Sweeper'Class;
         Y_Sweeper     : access Waveform_Sweeper'Class;
         Annotations   : access Item;
      end record;
   type Graph_Paper_Ptr is access all Graph_Paper_Layer;

   function Find
     (Layer      : Graph_Paper_Layer;
      Annotation : Graph_Paper_Annotation_Interface'Class) return Item_Ptr;

   pragma Warnings (On, "declaration hides ""Y_Axis""");
   pragma Warnings (On, "declaration hides ""X_Axis""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Layered.Graph_Paper;
