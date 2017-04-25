--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform                        Luebeck            --
--  Interface                                      Winter, 2011       --
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
--____________________________________________________________________--

with Ada.Real_Time;            use Ada.Real_Time;
with Gtk.Handlers.References;  use Gtk.Handlers.References;

with Ada.Calendar;
with Strings_Edit.Float_Edit;
with Strings_Edit.Generic_Scale;

package Gtk.Layered.Waveform is
   type X_Axis is new GDouble;
   type Y_Axis is new GDouble;
--
-- Waveform_Layer -- A waveform layer
--
-- The layer represents a running curve  drawn  from  some  source.  The
-- curve  can  be scalled vertically manually or by an amplifier object.
-- Horizontally it is scalled manually or by a sweeper object.
--
   type Waveform_Layer (<>) is
      new Abstract_Layer
      and Scalable_Layer
      and Widened_Layer with private;
------------------------------------------------------------------------
-- Waveform_Data_Scanner -- The  scanner  object  used  to  access   the
--                          waveform data.
--
   type Waveform_Data_Scanner is interface;
--
-- Backward/Forward -- Buffer for a measurement point
--
--    Source   - The waveform data scanner
--    T        - The position in the buffer to start scan
--    V        - The value
--  [ Got_It ] - False if T is outside the buffer bounds
--
-- The procedure increases or decreases T to the nearest available point
-- in  the  buffer.  T is then set to the position of the point, V is to
-- the point's value.
--
-- Exceptions :
--
--    End_Error - T is outside the buffer bounds (if no Got_It used)
--
   procedure Backward
             (  Source : in out Waveform_Data_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis
             )  is abstract;
   procedure Backward
             (  Source : in out Waveform_Data_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is abstract;
   procedure Forward
             (  Source : in out Waveform_Data_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis
             )  is abstract;
   procedure Forward
             (  Source : in out Waveform_Data_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is abstract;
--
-- First -- Point in the buffer
--
--    Source   - The waveform data scanner
--    T        - The position of the first point in the buffer
--    V        - The value at this point
--  [ Got_It ] - False if the buffer is empty
--
-- Exceptions :
--
--    End_Error - Empty buffer
--
   procedure First
             (  Source : in out Waveform_Data_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis
             )  is abstract;
   procedure First
             (  Source : in out Waveform_Data_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is abstract;
--
-- Get -- Data source value
--
--    Source   - The data source scanner
--    T        - The argument
--    Mode     - The interpolation mode
--    V        - The result, the value corresponding to T
--  [ Got_It ] - False if The value is not defined by the data source
--
-- The procedure does not extrapolate the data source values,  End_Error
-- is propagated when Source does not have defined values around T.
--
-- Exceptions :
--
--    End_Error - The value is not defined by the data source
--
   procedure Get
             (  Source : in out Waveform_Data_Scanner'Class;
                T      : X_Axis;
                Mode   : Interpolation_Mode;
                V      : out Y_Axis
             );
   procedure Get
             (  Source : in out Waveform_Data_Scanner'Class;
                T      : X_Axis;
                Mode   : Interpolation_Mode;
                V      : out Y_Axis;
                Got_It : out Boolean
             );
--
-- Is_In -- Test if a point is in the buffer
--
--    Source - The waveform data scanner
--    T      - The position
--
-- Returns :
--
--    True if T is within the buffer bounds
--
   function Is_In
            (  Source : Waveform_Data_Scanner;
               T      : X_Axis
            )  return Boolean is abstract;
--
-- Last -- Point in the buffer
--
--    Source   - The waveform data scanner
--    T        - The position of the last point in the buffer
--    V        - The value at this point
--  [ Got_It ] - False if the buffer is empty
--
-- Exceptions :
--
--    End_Error - Empty buffer
--
   procedure Last
             (  Source : in out Waveform_Data_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis
             )  is abstract;
   procedure Last
             (  Source : in out Waveform_Data_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is abstract;
------------------------------------------------------------------------
-- Waveform_Data_Source -- The  source  of  the  waveform data, a set of
--                         sampled  points  (t,v), where t is time and v
--   .__________.          is value.            .______________________.
--   |         1|------------------------------>|1                     |
--   | Producer |                               |                      |
--   |__________|                               |                      |
--   .______________________________________.   | Waveform_Data_Source |
--   |           ._______________________.  |   |                      |
--   | Waveform  |                      N|<-----|1                     |
--   |           | Waveform_Data_Scanner |  |   |______________________|
--   |  .-.   <--|_______________________|  |
--   | /   \_                               |
--   |       \/                             |
--   |______________________________________|
--
   type Waveform_Data_Source is limited interface;
--
-- Connected -- Notification that the source was connected
--
--    Source - The waveform source to notify
--    Layer  - The waveform
--
-- This procedure is called to notify a data source that a waveform  was
-- connected to it.
--
   procedure Connected
             (  Source : in out Waveform_Data_Source;
                Layer  : in out Waveform_Layer'Class
             )  is abstract;
--
-- Create -- A data source scanner
--
--    Source - The source the waveform should represent
--
-- Returns :
--
--    The waveform source scanner to access the source
--
   function Create
            (  Source : not null access Waveform_Data_Source
            )  return Waveform_Data_Scanner'Class is abstract;
--
-- Disconnected -- Notification that the source was disconnected
--
--    Source - The waveform source to notify
--    Layer  - The waveform
--
-- This procedure is called to notify a data source that a waveform  was
-- disconnected from it.
--
   procedure Disconnected
             (  Source : in out Waveform_Data_Source;
                Layer  : in out Waveform_Layer'Class
             )  is abstract;
------------------------------------------------------------------------
-- Waveform_Data_Feed -- The interface for feeding a source with data
--
   type Waveform_Data_Feed is limited interface
      and Waveform_Data_Source;
--
-- Erase -- Delete all points from the data buffer
--
--    Source - The waveform data source
--
   procedure Erase (Source : in out Waveform_Data_Feed) is abstract;
--
-- Put -- Write a new point into the data buffer
--
--    Source - The waveform data source
--    T, V   - The point to write
--
-- This procedure must be called from only task  at  a  time.  When  the
-- source contains a value with same T, the old value is replaced.  When
-- the source is full, the value with the least T is dropped, unless the
-- new value has T lesser, in which case Put is void.
--
   procedure Put
             (  Source : in out Waveform_Data_Feed;
                T      : X_Axis;
                V      : Y_Axis
             )  is abstract;
------------------------------------------------------------------------
-- Waveform_Amplifier -- The waveform amplifier interface
--
-- The  interface  can be added to Gtk_Adjustment object supplied to the
-- waveform in order to perform additional  actions  like  auto-scaling.
-- The amplifier runs between two states:
--
--    Add_Range - Accumulation of the waveform ranges
--    Add_Range
--    Add_Range
--    Get_Value - Evaluation and setting the adjustment parameters
--    Get_Value - The adjustment parameters do not change
--    Get_Value   (or Lower, Upper, Page_Size)
--    Add_Range - Starting new accumulation cycle
--    Add_Range
--    Add_Range
--      ...
--
   type Waveform_Amplifier is interface;
   subtype Waveform_Scaling is GDouble range 0.0..10.0;
--
-- Add_Range -- Notify the amplifier about values range of a waveform
--
--    Amplifier    - The object
--    Layer        - The layer notifying the object
--    From, To     - The indicated segment of the x-axis
--    Lower, Upper - The indicated segment of the y-axis
--
-- The  procedure is called upon sampling waveform source. The amplifier
-- may change its parameters, for example in order  to  make  the  whole
-- range  visible  on  the y-axis. Note that the amplifier can be shared
-- between   several   layers   sharing   same   y-axis.   Therefore  an
-- implementation of Add_Range should prepare itself to multiple calls.
--
   procedure Add_Range
             (  Amplifier    : not null access Waveform_Amplifier;
                Layer        : Waveform_Layer'Class;
                From,  To    : X_Axis;
                Lower, Upper : Y_Axis
             )  is abstract;
------------------------------------------------------------------------
-- Waveform_Sweeper -- The waveform sweeper interface
--
-- The  interface  can be added to Gtk_Adjustment object supplied to the
-- waveform in order to sweep opon drawing.
--
   type Waveform_Sweeper is interface;
--
-- Set_Current_Time -- Change time indicated by the sweeper
--
--    Sweeper - The adjustment object
--    Stamp   - The time indicated by the sweeper
--    Active  - Increase active count until return from the procedure
--
-- This procedure is called before drawing the layer. The implementation
-- can change the adjustment to sweep the waveform. Note that it can  be
-- called multiple times when the sweeper object is  shared  by  several
-- waveforms.  Stamp corresponds  to the end (right margin) of the page.
-- When Active is passed True  the active count is increased.  Is_Active
-- returns true if the count is greater than zero.
--
   procedure Set_Current_Time
             (  Sweeper : not null access Waveform_Sweeper;
                Stamp   : Time;
                Active  : Boolean := False
             )  is abstract;
--
-- Is_Active -- Check if setting time is active
--
--    Sweeper - The adjustment object
--
-- Layers during  preparation to drawing may call Set_Current_Time  with
-- Active set True.  When they  receive the  "changed"  signal  they use
-- Is_Active to decide not to a new queue drawing request.
--
   function Is_Active
            (  Sweeper : not null access Waveform_Sweeper
            )  return Boolean is abstract;
------------------------------------------------------------------------
-- Add_Waveform -- Add a waveform
--
--    Under     - The layer or widget where to place the waveform under
--    Box       - The box within which the waveform is to shown
--    Width     - The width of the line
--    Color     - The waveform color
--    Line_Cap  - The style of the line ends
--    Sweeper   - The adjustment of the horizontal axis (sweep axis)
--    Amplifier - The adjustment of the vertical axis (amplifier)
--    Mode      - Interpolation mode
--    Left      - Extrapolation mode to the left
--    Right     - Extrapolation mode to the right
--    Opacity   - Filling color opacity
--    Scaled    - The layer is scaled together with the parent widget
--    Widened   - The line is widened together with the parent widget
--
-- These procedure and function create a waveform layer.  The  parameter
-- Under specifies the layer location. The  parameter  Box  is  the  box
-- bounding  the  waveform.  Width  specifies  the width of the waveform
-- line.  Color  is  the  line  color.  The  type  of  the  line ends is
-- determined  by  the  parameter  Line_Cap.   The   parameter   Opacity
-- determines  the transparency of the filling under the waveform curve.
-- The  parameter  Sweeper  is the object controlling how the horizontal
-- axis  of  the  waveform  is  scaled.  It  is analogous to the sweeper
-- section of an oscilloscope. Several waveforms may share  one  sweeper
-- object. When this parameter  is  null  the  waveform  must  be  swept
-- manually  using  the  procedure Sweep. The parameter Amplifier is the
-- object controlling the vertical axis. When this parameter is null the
-- waveform must  be  scaled  using  explicit  calls  to  the  procedure
-- Amplify. The parameter Mode specified the  interpolation  mode.  When
-- Scaled is true the layer is scaled to  fit  the  parent  widget.  The
-- scaling s performed as follows:
--
-- (o)  The  x-coordinates  of  the  box  are multiplied by the widget's
--      horizontal  size  and  then  added  to  the  x-coordinate of the
--      widget's center;
-- (o)  The  y-coordinates  of  the  box  are multiplied by the widget's
--      vertical   size  and  then  added  to  the  y-coordinate  of the
--      widget's center.
--
-- Widened  when  true  indicates  that  the  line  width  is  increased
-- proportionally to the widget's size.
--
--
-- Returns :
--
--    The layer (optional)
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Add_Waveform
             (  Under     : not null access Layer_Location'Class;
                Box       : Cairo_Box;
                Width     : GDouble            := 1.0;
                Color     : Gdk_Color          := RGB (1.0, 0.0, 0.0);
                Line_Cap  : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
                Sweeper   : access Gtk_Adjustment_Record'Class := null;
                Amplifier : access Gtk_Adjustment_Record'Class := null;
                Mode      : Interpolation_Mode := Linear;
                Left      : Boolean            := False;
                Right     : Boolean            := False;
                Opacity   : Fill_Opacity       := 1.0;
                Scaled    : Boolean            := False;
                Widened   : Boolean            := False
             );
   function Add_Waveform
            (  Under     : not null access Layer_Location'Class;
               Box       : Cairo_Box;
               Width     : GDouble            := 1.0;
               Color     : Gdk_Color          := RGB (1.0, 0.0, 0.0);
               Line_Cap  : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
               Sweeper   : access Gtk_Adjustment_Record'Class := null;
               Amplifier : access Gtk_Adjustment_Record'Class := null;
               Mode      : Interpolation_Mode := Linear;
               Left      : Boolean            := False;
               Right     : Boolean            := False;
               Opacity   : Fill_Opacity       := 1.0;
               Scaled    : Boolean            := False;
               Widened   : Boolean            := False
            )  return not null access Waveform_Layer;
--
-- Amplify -- Set the waveform values at the box margins
--
--    Layer - The waveform
--    Lower - The value corresponding to the bottom of the waveform box
--    Upper - The value corresponding to the top of the waveform box
--
-- Exceptions :
--
--    Constraint_Error - Illegal range of values
--
   procedure Amplify
             (  Layer : in out Waveform_Layer;
                Lower : Y_Axis;
                Upper : Y_Axis
             );
--
-- Changed -- Notification that the source was changed
--
--    Layer - The waveform
--    From  - Starting position of the interval where the change applies
--    To    - Stop position of the interval
--
-- This   procedure   is   called   by   an   implementation   of    the
-- Waveform_Data_Source  interface  in  order  to  notify that a part of
-- source was changed. This procedure is not intended to perform  actual
-- rendering, rather to remember which parts need to be redrawn when the
-- time comes.
--
   procedure Changed
             (  Layer : in out Waveform_Layer;
                From  : X_Axis;
                To    : X_Axis
             );
--
-- Get -- The data source value by horizontal coordinate
--
--    Layer - The waveform
--    X     - The coordinate to query the value
--
-- The argument is relative to the widget of  the  layer.  The  function
-- searches the data source for a value corresponding to the  coordinate
-- X.  The result depends  on the waveform interpolation mode.  See also
-- Get_Point.
--
-- Returns :
--
--    The corresponding value
--
-- Exceptions :
--
--    End_Error - There is no value corresponding to X
--
   function Get (Layer : Waveform_Layer; X : GDouble) return Y_Axis;
--
-- Get -- The data source value by horizontal coordinate
--
--    Layer  - The waveform
--    X      - The coordinate to query the value
--    Y      - The value corresponding to X
--    Got_It - False if there is no value
--
-- The argument is relative to the widget of  the  layer.  The  function
-- searches the data source for a value corresponding to the  coordinate
-- X.  The result depends  on the waveform interpolation mode.  See also
-- Get_Point.
--
   procedure Get
             (  Layer  : Waveform_Layer;
                X      : GDouble;
                Y      : out Y_Axis;
                Got_It : out Boolean
             );
--
-- Get_Amplifier -- Get vertical adjustment
--
--    Layer - The waveform
--
-- Returns :
--
--    The adjustment or null
--
   function Get_Amplifier (Layer : Waveform_Layer)
      return Gtk_Adjustment;
--
-- Get_Box -- The box bounding the waveform
--
--    Layer - The waveform
--
-- Returns :
--
--    The box
--
   function Get_Box (Layer : Waveform_Layer) return Cairo_Box;
--
-- Get_Interpolation_Mode -- Waveform interpolation mode
--
--    Layer - The waveform
--
-- Returns :
--
--    The interpolation mode
--
   function Get_Interpolation_Mode (Layer : Waveform_Layer)
      return Interpolation_Mode;
--
-- Get_Left_Extrapolation_Mode -- Waveform extrapolation mode
--
--    Layer - The waveform
--
-- Returns :
--
--    True if extrapolated to the left
--
   function Get_Left_Extrapolation_Mode (Layer : Waveform_Layer)
      return Boolean;
--
-- Get_Line -- Waveform line parameters
--
--    Layer - The waveform
--
-- Returns :
--
--    The line parameters
--
   function Get_Line (Layer : Waveform_Layer) return Line_Parameters;
--
-- Get_Method -- Currently used rendering method
--
--    Layer - The waveform
--
-- Returns :
--
--    The method
--
   function Get_Method (Layer : Waveform_Layer)
      return Waveform_Drawing_Method;
--
-- Get_Opacity -- Waveform filling opacity
--
--    Layer - The waveform
--
-- Returns :
--
--    The opacity (0..1)
--
   function Get_Opacity (Layer : Waveform_Layer) return Fill_Opacity;
--
-- Get_Point -- The data source point by horizontal coordinate
--
--    Layer  - The waveform
--    X      - The coordinate to query the value
--    T      - The horizontal axis value corresponding to X
--    V      - The waveform value at T
--    Got_It - False if there is no value corresponding to X
--
-- The argument is relative to the widget of the  layer.  The  procedure
-- searches the data source for a value corresponding to the  coordinate
-- X. The result depends on the waveform interpolation mode.
--
-- Exceptions :
--
--    End_Error - There is no value corresponding to X
--
   procedure Get_Point
             (  Layer : Waveform_Layer;
                X     : GDouble;
                T     : out X_Axis;
                V     : out Y_Axis
             );
   procedure Get_Point
             (  Layer  : Waveform_Layer;
                X      : GDouble;
                T      : out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             );
--
-- Get_Preferred_Method -- Get preferred rendering method
--
--    Layer - The waveform
--
-- Returns :
--
--    The preferred rendering method
--
   function Get_Preferred_Method (Layer : Waveform_Layer)
      return Waveform_Drawing_Method;
--
-- Get_Right_Extrapolation_Mode -- Waveform extrapolation mode
--
--    Layer - The waveform
--
-- Returns :
--
--    True if extrapolated to the right
--
   function Get_Right_Extrapolation_Mode (Layer : Waveform_Layer)
      return Boolean;
--
-- Get_Source -- Get the waveform data source
--
--    Layer - The waveform
--
-- Returns :
--
--    The data source or null
--
   function Get_Source
            (  Layer : Waveform_Layer
            )  return access Waveform_Data_Source'Class;
--
-- Get_Source -- Get the waveform data source
--
--    Scanner - The waveform scanner
--
-- Returns :
--
--    The data source
--
   function Get_Source
            (  Scanner : Waveform_Data_Scanner
            )  return not null access
                      Waveform_Data_Source'Class is abstract;
--
-- Get_Sweeper -- Get horizontal adjustment
--
--    Layer - The waveform
--
-- Returns :
--
--    The adjustment or null
--
   function Get_Sweeper (Layer : Waveform_Layer) return Gtk_Adjustment;
--
-- Get_{T|V} -- Convert widget coordinates to data source values
--
--    Layer - The waveform
--    X | Y - The coordinate to convert
--
-- The argument is relative to the widget of the layer.
--
-- Returns :
--
--    The corresponding value
--
   function Get_T (Layer : Waveform_Layer; X : GDouble) return X_Axis;
   function Get_V (Layer : Waveform_Layer; Y : GDouble) return Y_Axis;
--
-- Get_{T1|T2|V1|V2} -- Get values at the box margins
--
--    Layer - The waveform
--
-- Returns :
--
--    The value corresponding to the box margin
--
   function Get_T1 (Layer : Waveform_Layer) return X_Axis;
   function Get_T2 (Layer : Waveform_Layer) return X_Axis;
   function Get_V1 (Layer : Waveform_Layer) return Y_Axis;
   function Get_V2 (Layer : Waveform_Layer) return Y_Axis;
--
-- Get_{X|Y} -- Convert to widget coordinates
--
--    Layer - The waveform
--    T | V - The value to convert
--
-- The result is relative to the widget of the layer.
--
-- Returns :
--
--    The oordinate of the argument
--
   function Get_X (Layer : Waveform_Layer; T : X_Axis) return GDouble;
   function Get_Y (Layer : Waveform_Layer; V : Y_Axis) return GDouble;
--
-- Is_Visible -- Get visibility status
--
--    Layer - The waveform
--
-- Returns :
--
--    True if the waveform is visible
--
   function Is_Visible (Layer : Waveform_Layer) return Boolean;
--
-- Set -- Parameters of the curve
--
--    Layer   - The waveform
--    Box     - The waveform's rectangle
--    Line    - The angle (position) of the lowest value
--    Mode    - Interpolation mode
--    Left    - Extrapolation mode to the left
--    Right   - Extrapolation mode to the right
--    Opacity - Of the waveform filling
--
-- Exceptions :
--
--    Constraint_Error - Wrong parameters
--
   procedure Set
             (  Layer   : in out Waveform_Layer;
                Box     : Cairo_Box;
                Line    : Line_Parameters;
                Mode    : Interpolation_Mode;
                Left    : Boolean;
                Right   : Boolean;
                Opacity : Fill_Opacity
             );
--
-- Set_Amplifier -- Set/change amplifier
--
--    Layer     - The waveform
--    Amplifier - To use with the layer
--
   procedure Set_Amplifier
             (  Layer     : in out Waveform_Layer;
                Amplifier : access Gtk_Adjustment_Record'Class := null
             );
--
-- Set_Color -- Set curve color
--
--    Layer - The waveform
--    Color - The color to set
--
   procedure Set_Color
             (  Layer : in out Waveform_Layer;
                Color : Gdk_Color
             );
--
-- Set_Extrapolation_Mode -- Set waveform extrapolation mode
--
--    Layer - The waveform
--    Left  - Extrapolation mode to the left
--    Right - Extrapolation mode to the left
--
   procedure Set_Extrapolation_Mode
             (  Layer : in out Waveform_Layer;
                Left  : Boolean;
                Right : Boolean
             );
--
-- Set_Interpolation_Mode -- Set waveform interpolation mode
--
--    Layer - The waveform
--    Mode  - Interpolation mode
--
   procedure Set_Interpolation_Mode
             (  Layer : in out Waveform_Layer;
                Mode  : Interpolation_Mode
             );
--
-- Set_Opacity -- Set waveform filling opacity
--
--    Layer   - The waveform
--    Opacity - Opacity level 0..1
--
   procedure Set_Opacity
             (  Layer   : in out Waveform_Layer;
                Opacity : Fill_Opacity
             );
--
-- Set_Preferred_Method -- Set preferred rendering method
--
--    Layer  - The waveform
--    Method - The preferred method
--
-- The rendering method  is selected  when the waveform parameters allow
-- its application.
--
   procedure Set_Preferred_Method
             (  Layer  : in out Waveform_Layer;
                Method : Waveform_Drawing_Method
             );
--
-- Set_Source -- Set waveform data source
--
--    Layer    - The waveform
--  [ Source ] - The data source
--
-- This  procedure  is  called  to  set  the  waveform  data source. The
-- waveform  represents the data from the source. The old source if any,
-- is  disconnected.  Without  the  second  parameter  no  new source is
-- connected.
--
   procedure Set_Source
             (  Layer  : in out Waveform_Layer;
                Source : in out Waveform_Data_Source'Class
             );
   procedure Set_Source (Layer : in out Waveform_Layer);
--
-- Set_Sweeper -- Set/change sweeper
--
--    Layer   - The waveform
--    Sweeper - To use with the layer
--
   procedure Set_Sweeper
             (  Layer   : in out Waveform_Layer;
                Sweeper : access Gtk_Adjustment_Record'Class := null
             );
--
-- Set_Visible -- Change visibility status
--
--    Layer   - The waveform
--    Visible - True if the waveform must be visible
--
   procedure Set_Visible
             (  Layer   : in out Waveform_Layer;
                Visible : Boolean
             );
--
-- Sweep -- Set the time corresponding to the right margin
--
--    Layer - The waveform
--    To    - The time corresponding to the right margin
--
-- The left margin is moved by the same amount as the right one.
--
-- Exceptions :
--
--    Constraint_Error - Illegal time
--
   procedure Sweep (Layer : in out Waveform_Layer; To : X_Axis);
--
-- Sweep -- Set the visible time range
--
--    Layer - The waveform
--    From  - The time corresponding to the left margin of the box
--    To    - The time corresponding to the right margin
--
-- Exceptions :
--
--    Constraint_Error - Illegal time span
--
   procedure Sweep
             (  Layer : in out Waveform_Layer;
                From  : X_Axis;
                To    : X_Axis
             );

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Waveform_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Waveform_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding procedure Finalize (Layer : in out Waveform_Layer);
   overriding
      function Get_Properties_Number
               (  Layer : Waveform_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Waveform_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Waveform_Layer;
                  Property : Positive
               )  return GValue;
   overriding
      function Get_Scaled (Layer : Waveform_Layer) return Boolean;
   overriding
      function Get_Widened (Layer : Waveform_Layer) return Boolean;
   overriding
      function Is_Updated (Layer : Waveform_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Waveform_Layer;
                   Offset : Cairo_Tuple
                );
   overriding
      procedure Prepare
                (  Layer   : in out Waveform_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Resized
                (  Layer : in out Waveform_Layer;
                   Area  : Gdk_Rectangle
                );
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Waveform_Layer
                );
   overriding
      procedure Scale
                (  Layer  : in out Waveform_Layer;
                   Factor : GDouble
                );
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Waveform_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Set_Scaled
                (  Layer  : in out Waveform_Layer;
                   Scaled : Boolean
                );
   overriding
      procedure Set_Widened
                (  Layer   : in out Waveform_Layer;
                   Widened : Boolean
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Waveform_Layer
                );
------------------------------------------------------------------------
--
-- Get_Epoch -- The epoch calendar time
--
-- Returns :
--
--    The epoch time used in time to number conversions
--
   function Get_Epoch return Ada.Calendar.Time;
   function Get_Epoch return Time;
--
-- To_Double -- Time to seconds count conversion
--
--    Value - Time to convert
--
-- Returns :
--
--    Value - Epoch
--
-- Exceptions :
--
--    Constraint_Error - Value cannot be converted
--
   function To_Double (Value : Time) return GDouble;
   function To_Double (Value : Ada.Calendar.Time) return GDouble;
--
-- To_Time -- Seconds count to time conversion
--
--    Value - Seconds since the epoch
--
-- Returns :
--
--    Epoch + Value
--
-- Exceptions :
--
--    Constraint_Error - Value cannot be converted
--
   function To_Time (Value : GDouble) return Time;
   function To_Time (Value : GDouble) return Ada.Calendar.Time;

   package Rasters is new Strings_Edit.Generic_Scale (GDouble);
   package Edit is new Strings_Edit.Float_Edit (GDouble);

private
   pragma Inline (Get_Epoch);
   pragma Inline (Get_Method);
   pragma Inline (Get_Preferred_Method);
   pragma Inline (Get_T1);
   pragma Inline (Get_T2);
   pragma Inline (Get_V1);
   pragma Inline (Get_V2);
   pragma Inline (Is_Visible);
   pragma Inline (To_Double);
   pragma Inline (To_Time);

   type Horizontal_Offset is new GInt;
   type Point_Data is record
      X : Horizontal_Offset;
      Y : Y_Axis;
   end record;
   type Points_Array is array (Natural range <>) of Point_Data;
   type Points_Array_Ptr is access Points_Array;
   type References_List is array (1..4) of Handler_Reference;
   type Waveform_Data_Scanner_Ptr is access Waveform_Data_Scanner'Class;
   --
   -- Line_Method_Data -- Specific to line stroking
   --
   type Line_Method_Data is record
      Count      : Natural := 0;     -- Number of values in the buffer
      First      : Natural := 0;     -- The first value in the buffer
      Last_Count : Natural := 0;     -- Offset next to sampled
      Last_T     : X_Axis  := X_Axis'First; -- Last time in the buffer
      Points     : Points_Array_Ptr;        -- Sampled line points
   end record;

   type Waveform_Layer is
      new Abstract_Layer
      and Scalable_Layer
      and Widened_Layer with
   record
      Box       : Cairo_Box;
      Data      : Waveform_Data_Scanner_Ptr;
      Method    : Waveform_Drawing_Method := Resample_New_And_Stroke;
      Preferred : Waveform_Drawing_Method := Resample_New_And_Stroke;
      Line      : Line_Parameters;
      T1        : X_Axis := 1.0; -- The time at the box boundary X1
      T2        : X_Axis := 0.0; --                              X2
      V1        : Y_Axis := 0.0; -- The value at the box boundary Y1
      V2        : Y_Axis := 0.0; --                               Y2
      Y0, YY    : GDouble; -- Linear conversion to Y coordinates
      dT        : X_Axis := 0.0;  -- Current step, time per one pixel
      Mode      : Interpolation_Mode := Linear;
      Opacity   : Fill_Opacity       := 1.0;
      Handlers  : References_List;
      Last_T1   : X_Axis;            -- Last T1 at that time
      Sampled   : Boolean  := False; -- No need to sample
      Valid     : Boolean  := False; -- Some sampled data are valid
      Scaled    : Boolean  := False;
      Updated   : Boolean  := True;
      Visible   : Boolean  := True;
      Widened   : Boolean  := False;
      Extrapolate_Left     : Boolean := False;
      Extrapolate_Right    : Boolean := False;
      Sweeper_Adjustment   : Gtk_Adjustment;
      Amplifier_Adjustment : Gtk_Adjustment;
      Sweeper_Object       : access Waveform_Sweeper'Class;
      Amplifier_Object     : access Waveform_Amplifier'Class;
      Line_Data            : Line_Method_Data;  -- Stroke mode
      pragma Atomic (Updated);
   end record;
   type Waveform_Ptr is access all Waveform_Layer;
--
-- Changed_Amplifier -- Event handler
--
   procedure Changed_Amplifier
             (  Adjustment : access GObject_Record'Class;
                Layer      : Waveform_Ptr
             );
--
-- Changed_Sweeper -- Event handler
--
   procedure Changed_Sweeper
             (  Adjustment : access GObject_Record'Class;
                Layer      : Waveform_Ptr
             );
--
-- Draw_Lines -- Draw lines
--
--    Layer   - The waveform
--    Context - Drawing context
--    Data    - The drawing data
--
   procedure Draw_Lines
             (  Layer   : in out Waveform_Layer;
                Context : Cairo_Context;
                Data    : in out Line_Method_Data
             );
--
-- Query_Amplifier -- Set the vertical exis according to the amplifier
--
--    Layer - The waveform
--
   procedure Query_Amplifier (Layer : in out Waveform_Layer);
--
-- Query_Sweeper -- Set the horizonatal axis according to the sweeper
--
--    Layer - The waveform
--
   procedure Query_Sweeper (Layer : in out Waveform_Layer);
--
-- Sample_Lines -- Points to draw
--
--    Layer  - The waveform
--    Data   - The drawing data
--    X1, X2 - Horizontal coordinates of the waveform box
--
   procedure Sample_Lines
             (  Layer  : in out Waveform_Layer;
                Data   : in out Line_Method_Data;
                X1, X2 : Horizontal_Offset
             );
--
-- Set_Y_Conversion -- Set value to y-coordinate conversion
--
--    Layer  - The waveform
--    Y1, Y2 - Vertical coordinates of the box
--
   procedure Set_Y_Conversion
             (  Layer  : in out Waveform_Layer;
                Y1, Y2 : GDouble
             );
--
-- To_Y -- Value to y-coordinate conversion
--
--    Layer - The waveform
--    V     - The value to convert
--
-- Returns :
--
--    The y-coordinate
--
   function To_Y (Layer : Waveform_Layer; V : Y_Axis) return GDouble;

   pragma Inline (To_Y);

end Gtk.Layered.Waveform;
