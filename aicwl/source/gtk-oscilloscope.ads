--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--  Interface                                      Summer, 2011       --
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
-- __________________________________________________________________ --
--
--  The package implements a multichannel osciloscope widget. The widget
--  is rendered asynchronously to  the channel  feed allowing processing
--  a large amount of data. The channels are sorted into groups. A group
--  is controlled by the same amplifier, so that the coresponding graphs
--  are shown in the same scale.
--

with Ada.Calendar;
with Ada.Finalization;
with Ada.Real_Time;
with Ada.Strings;
with Cairo.Ellipses;
with Gdk.Color;
with Gdk.Event;
with Glib.Values;
with Gtk.Box;
with Gtk.Fixed;
with Gtk.Grid;
with Gtk.Handlers.Generic_Callback;
with Gtk.Handlers.References;
with Gtk.Layered.Graph_Paper;
with Gtk.Layered.Graph_Paper_Annotation;
with Gtk.Layered.Line;
with Gtk.Layered.Rectangle;
with Gtk.Layered.Refresh_Engine;
with Gtk.Layered.Waveform.Amplifier;
with Gtk.Layered.Waveform.Ring_Data_Buffer;
with Gtk.Layered.Waveform.Sweeper;
with Gtk.List_Store;
with Gtk.Missed;
with Gtk.Scale;
with Gtk.Widget;
with Pango.Cairo.Fonts;

package Gtk.Oscilloscope is

   pragma Warnings (Off, "declaration hides ""Group""");
   pragma Warnings (Off, "declaration hides ""Left""");
   pragma Warnings (Off, "declaration hides ""Oscilloscope""");
   pragma Warnings (Off, "declaration hides ""Refresh_Period""");
   pragma Warnings (Off, "declaration hides ""Right""");
   pragma Warnings (Off, "declaration hides ""Scale""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   --
   -- Selection_Action -- What to do when user selects a square area of
   --
   type Selection_Action is
     (Interactive,
      Zoom_In,
      Zoom_In_Time,
      Zoom_In_Values,
      Zoom_Out,
      Zoom_Out_Time,
      Zoom_Out_Values,
      Copy_Values,
      Copy_Differences,
      Copy_Range,
      User_Action,
      None);

   --
   -- Popup menu items -- Shown on right mouse click
   --
   type Dropdown_Items is mod 2 ** 6;
   Grid_Item          : constant Dropdown_Items := 2 ** 0;
   Hold_Release_Item  : constant Dropdown_Items := 2 ** 1;
   Interpolation_Item : constant Dropdown_Items := 2 ** 2;
   Latest_Data_Item   : constant Dropdown_Items := 2 ** 3;
   Snapshot_Item      : constant Dropdown_Items := 2 ** 4;
   Undo_Redo_Item     : constant Dropdown_Items := 2 ** 5;

   --
   -- Class_Name - Of the widget
   --
   Class_Name : constant String := "GtkOscilloscope";

   --
   -- Amplifier_Type -- The oscilloscope's amplifier
   --
   --    Left   - The amplifier left of the waveform box
   --    Middle - The amplifier on the waveform box
   --    Right  - The amplifier right of the waveform box
   --
   type Amplifier_Type is (Left, Right, Middle);

   --
   -- Sweeper_Type -- The oscilloscope's sweeper
   --
   --    Lower - The sweeper below the waveform box
   --    Upper - The sweeper above the waveform box
   --
   type Sweeper_Type is (Upper, Lower);

   --
   -- Channel_Count -- Channel numbers
   --
   type Channel_Count is new Natural;
   subtype Channel_Number is Channel_Count range 1 .. Channel_Count'Last;

   --
   -- Group_Count -- Group numbers
   --
   type Group_Count is new Natural;
   subtype Group_Number is Group_Count range 1 .. Group_Count'Last;

   --
   -- Zooming_State -- To save or restore
   --
   type Zooming_State is mod 2 ** 2;
   Values_Zooming : constant Zooming_State := 2 ** 0;
   Time_Zooming   : constant Zooming_State := 2 ** 1;

   --
   -- Snapshot_Format -- The file format used for snapshot
   --
   type Snapshot_Format is (No_Snapshot, PDF_Snapshot, SVG_Snapshot);

   --
   -- Gtk_Oscilloscope -- Oscilloscope
   --
   type Gtk_Oscilloscope_Record (<>) is
     new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Oscilloscope is access all Gtk_Oscilloscope_Record'Class;

   --
   -- Add_Channel -- Create a new channel
   --
   --    Widget  - The oscilloscope
   --  [ Group ] - The group the new channel will belong
   --  [ Color ] - The waveform color
   --    Mode    - Interpolation mode
   --    Left    - Extrapolate to the left
   --    Right   - Extrapolate to the right
   --    Name    - The waveform name
   --    Sweeper - The sweeper type
   --    Buffer  - The buffer to use for the channel (created if null)
   --
   -- When  the  parameter Group is omitted, a new group is created for the
   -- new  channel.  When  no color is specified its selected automatically
   -- according to the channel number.
   --
   -- Returns :
   --
   --    The channel number
   --
   -- Exception :
   --
   --    Constraint_Error - Illegal group number or too many channels
   --
   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Group   : Group_Number;
      Color   : Gdk.Color.Gdk_Color;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number;

   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Group   : Group_Number;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number;

   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Color   : Gdk.Color.Gdk_Color;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number;

   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number;

   --
   -- Drawing_Measurement_Point -- When to measure time period
   --
   --    Refresh_Period - Refresh period measured at the start of drawing
   --    Drawing_Time   - The drawing time
   --
   type Drawing_Measurement_Point is (Refresh_Period, Drawing_Time);

   --
   -- Add_Deviation_Channel -- Create the deviation channel
   --
   --    Widget   - The oscilloscope
   --  [ Group ]  - The group the new channel will belong
   --  [ Color ]  - The waveform color
   --    Measured - The measurement point
   --    Name     - The waveform name
   --    Sweeper  - The sweeper type
   --
   -- These procedures add a deviation channel. The channel is fed  by  the
   -- oscilloscope's refresh period. When the parameter Group is omitted, a
   -- new group is created for the new channel. When no color is  specified
   -- its selected automatically according to the channel number.
   --
   -- Returns :
   --
   --    The channel number
   --
   -- Exception :
   --
   --    Constraint_Error - Illegal group number or too many channels
   --
   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Group    : Group_Number;
      Color    : Gdk.Color.Gdk_Color;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number;

   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Group    : Group_Number;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number;

   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Color    : Gdk.Color.Gdk_Color;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number;

   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number;

   --
   -- Add_Group -- Create a new channel group
   --
   --    Widget    - The oscilloscope
   --    Name      - The group name
   --    Amplifier - Used with this group, created new if null
   --
   -- Channels  are  grouped  by  the amplifiers they share. The groups are
   -- referenced by their numbers.
   --
   -- Returns :
   --
   --    The channel group number
   --
   -- Exception :
   --
   --    Constraint_Error - Too many groups
   --
   function Add_Group
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Name      : String := "";
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier := null)
      return Group_Number;

   --
   -- Add_Shadow_Channel -- Create a new shadow channel
   --
   --    Widget  - The oscilloscope
   --    Channel - The origin channel
   --  [ Color ] - The waveform color
   --    Name    - The waveform name
   --    Sweeper - The sweeper type
   --
   -- A shadow channel has the same source and the amplifier (group) as the
   -- origin. Normally shadow channel has another sweeper, so than both the
   -- origin  and  the  source can be superimposed and shifted horizontally
   -- relatively  each  other. When Name is an empty string the name of the
   -- shadow  channel is the origin name plus " (shadow)". When no color is
   -- specified its selected automatically according to the channel number.
   --
   -- Returns :
   --
   --    The channel number
   --
   -- Exception :
   --
   --    Constraint_Error - Too many channels or wrong channel number
   --
   function Add_Shadow_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Color   : Gdk.Color.Gdk_Color;
      Name    : String := "";
      Sweeper : Sweeper_Type := Upper) return Channel_Number;

   function Add_Shadow_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Name    : String := "";
      Sweeper : Sweeper_Type := Upper) return Channel_Number;

   --
   -- Capture_{PDF|SVG} -- Capture the contents of the oscilloscope
   --
   --    Widget - The oscilloscope
   --    File   - The file to write
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Capture_PDF
     (Widget : not null access Gtk_Oscilloscope_Record;
      File   : UTF8_String);

   procedure Capture_SVG
     (Widget : not null access Gtk_Oscilloscope_Record;
      File   : UTF8_String);

   --
   -- Create_Annotation -- Factory operation for axis annotation
   --
   --    Widget            - The oscilloscope
   --    Amplifier/Sweeper - The axis
   --
   -- Returns :
   --
   --    The axis annotation object
   --
   function Create_Annotation
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
      return not null access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class;

   function Create_Annotation
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type)
      return not null access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class;

   --
   -- Delete_Channel -- Delete a channel
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- This procedure deletes a  channel.  Note  that  this  operation  will
   -- change the channel numbers greater than Channel.
   --
   -- Exception :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Delete_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number);

   --
   -- Erase_Redo_Stack -- Empty the redo stack
   --
   --    Widget - The oscilloscope
   --
   procedure Erase_Redo_Stack
     (Widget : not null access Gtk_Oscilloscope_Record);

   --
   -- Erase_Undo_Buffer -- Emoty the undo stack
   --
   --    Widget - The oscilloscope
   --
   procedure Erase_Undo_Stack
     (Widget : not null access Gtk_Oscilloscope_Record);

   --
   -- Feed -- Feed data of a channel
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --  [ T ]     - The time, current time, when omitted
   --    V       - The value
   --
   -- These procedure feed the channel with  data.  The  procedure  can  be
   -- called  asynchronously  to the GTK task, i.e. from a concurrent task.
   -- Note though that there must be only one task feeding the channel.
   --
   -- Exception :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Feed
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      T       : Ada.Real_Time.Time;
      V       : Gdouble);

   procedure Feed
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      T       : Ada.Calendar.Time;
      V       : Gdouble);

   procedure Feed
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      V       : Gdouble);

   procedure Feed
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      T       : Gdouble;
      V       : Gdouble);

   --
   -- Get_Amplifier -- Get amplifier object of a vertical axis
   --
   --    Widget                  - The oscilloscope
   --    Amplifier/Channel/Group - The vertical axis, channel, group
   --
   -- Returns :
   --
   --    The amplifier of the axis
   --
   -- Exceptions :
   --
   --    Constraint_Error - To group assigned to the axis, wrong channel,
   --                       wrong group
   --
   function Get_Amplifier
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
      return not null access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class;

   function Get_Amplifier
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number)
      return not null access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class;

   function Get_Amplifier
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number)
      return not null access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class;

   --
   -- Get_Auto_Scaling -- Get scaling mode of an amplifier
   --
   --    Widget          - The oscilloscope
   --    Amplifier/Group - The amplifier or channels group
   --
   -- Returns :
   --
   --    True if amplifier automatically scales its page to fit the curve
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel group number
   --
   function Get_Auto_Scaling
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean;

   function Get_Auto_Scaling
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number) return Boolean;

   --
   -- Get_Box -- The waveform's box
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The box containing the waveforms (relatively to the widget)
   --
   function Get_Box
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Cairo.Ellipses.Cairo_Box;

   --
   -- Get_Buffer -- Get the waveform buffer
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    The waveform buffer
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Buffer
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number)
      return not null access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer_Record'Class;

   --
   -- Get_Channel_List -- Get the list store containing channel names
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The list of channels
   --
   function Get_Channel_List
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Gtk.List_Store.Gtk_List_Store;

   --
   -- Get_Channels_Number -- The number of channels
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The number of channels
   --
   function Get_Channels_Number
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Channel_Count;

   --
   -- Get_Color -- Get the waveform color
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    The waveform color
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Color
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Gdk.Color.Gdk_Color;

   --
   -- Get_Default_Face -- The default font face to use for axis
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The font face to use when an axis annotation is created
   --
   function Get_Default_Face
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Pango.Cairo.Fonts.Pango_Cairo_Font;

   --
   -- Get_Enabled_Dropdown_Items -- Get currently enabled menu items
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The items
   --
   function Get_Enabled_Dropdown_Items
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Dropdown_Items;

   --
   -- Get_From -- Get the first time time of the sweeper
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- This function returns the first time of the widget's sweeper
   --
   -- Returns :
   --
   --    The first time
   --
   function Get_From
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type := Lower) return Ada.Real_Time.Time;

   function Get_From
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type := Lower) return Ada.Calendar.Time;

   --
   -- Get_Frozen -- Get the status of a sweeper
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The true if the sweeper is frozen
   --
   function Get_Frozen
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type := Lower) return Boolean;

   --
   -- Get_Grid_Colors -- Get colors of grid lines
   --
   --    Widget      - The oscilloscope
   --    Major_Color - The color used for major tick lines
   --    Minor_Color - The color used for minor tick lines
   --
   procedure Get_Grid_Colors
     (Widget      : not null access constant Gtk_Oscilloscope_Record;
      Major_Color : out Gdk.Color.Gdk_Color;
      Minor_Color : out Gdk.Color.Gdk_Color);

   --
   -- Get_Group -- Get the group associated with an amplifier
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type or channel number
   --
   -- Returns :
   --
   --    The group number
   --
   -- Exceptions :
   --
   --    Constraint_Error - No group assigned or wrong channel
   --
   function Get_Group
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Group_Number;

   function Get_Group
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Group_Number;

   --
   -- Get_Group_List -- Get the list store containing group names
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The list of groups
   --
   function Get_Group_List
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Gtk.List_Store.Gtk_List_Store;

   --
   -- Get_Groups_Number -- The number of channel groups
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The number of groups
   --
   function Get_Groups_Number
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Group_Count;

   --
   -- Get_Interpolation_Mode -- Get interpolation mode
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    The interpolation mode
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Interpolation_Mode
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Gtk.Layered.Interpolation_Mode;

   --
   -- Get_Left_Extrapolation_Mode -- Get extrapolation mode to the left
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    True if extrapolated to the left
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Left_Extrapolation_Mode
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Boolean;

   --
   -- Get_Manual_Sweep -- Allow user to togle sweepers
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    True if user is allowed to freeze/release sweepers
   --
   function Get_Manual_Sweep
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean;

   --
   -- Get_Name -- Get the channel name
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    The name of the channel
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Name
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return String;

   --
   -- Get_Name -- Get the name of a channels group
   --
   --    Widget - The oscilloscope
   --    Group  - The group number
   --
   -- Returns :
   --
   --    The name of the group
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong group number
   --
   function Get_Name
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number) return String;

   --
   -- Get_Offset -- Get the offset to the right margin
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The delay the values appear at the oscilloscope's right margin
   --
   function Get_Offset
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Duration;

   --
   -- Get_Page_Span -- Get the duration visible in the oscilloscope
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The duration between the oscilloscope margins
   --
   function Get_Page_Span
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Duration;

   --
   -- Get_Redo_Stub -- Get the topmost stub of the redo stack
   --
   --    Widget - The oscilloscope
   --    Depth  - The stub depth, 1 is the topmost stub
   --
   -- Returns :
   --
   --    The stub name
   --
   -- Exceptions :
   --
   --    End_Error - There is no such stub
   --
   function Get_Redo_Stub
     (Widget : not null access Gtk_Oscilloscope_Record;
      Depth  : Positive := 1) return UTF8_String;

   --
   -- Get_Release_To_Latest -- Get sweeper release behavior
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    True if sweeper when released by user jumps to latest time
   --
   function Get_Release_To_Latest
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean;

   --
   -- Get_Right_Extrapolation_Mode -- Get extrapolation mode to the right
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    True if extrapolated to the right
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Right_Extrapolation_Mode
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Boolean;

   --
   -- Get_Selection_Mode -- What to do on selection
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The action to perform on selection
   --
   function Get_Selection_Mode
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Selection_Action;

   --
   -- Get_Snapshot_File -- Get snapshot file name
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The name of the file used to store snapshots
   --
   function Get_Snapshot_File
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return UTF8_String;

   --
   -- Get_Snapshot_Format -- Get snapshot file name
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    The format of the file used to store snapshots
   --
   function Get_Snapshot_Format
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Snapshot_Format;

   --
   -- Get_Superscript -- Get subscript usage mode
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    True if subscript digits are allowed when formatting numbers
   --
   function Get_Superscript
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean;

   --
   -- Get_Sweeper -- Get a oscilloscope's sweeper
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The sweeper
   --
   function Get_Sweeper
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type)
      return not null access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;

   --
   -- Get_Sweeper -- Get a oscilloscope's sweeper
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    The sweeper type of the channel
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Sweeper
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Sweeper_Type;

   --
   -- Get_Time -- Get current time of a oscilloscope's sweeper
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --  [ X ]     - The horizontal coordinate (right margin by default)
   --
   -- The parameter X is the horizontal coordinate relative to Widget.
   --
   -- Returns :
   --
   --    The time corresponding to the end (right margin) of the page
   --
   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Real_Time.Time;

   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      X       : Gint) return Ada.Real_Time.Time;

   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Calendar.Time;

   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      X       : Gint) return Ada.Calendar.Time;

   --
   -- Get_Time_Axis -- Get time axis visibility status
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The time axis visibility status
   --
   function Get_Time_Axis
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean;

   --
   -- Get_Time_Axis_Annotation -- Get time axis annotation
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The annotation layer or null
   --
   function Get_Time_Axis_Annotation
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type)
      return access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class;

   --
   -- Get_Time_Axis_As_Time -- Get time axis rendering mode
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    True if time axis values are rendered as time stamps
   --
   function Get_Time_Axis_As_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean;

   --
   -- Get_Time_Axis_Height -- Get horizontal axis height
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The height of the axis when visibile
   --
   function Get_Time_Axis_Height
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Sweeper   : Sweeper_Type) return Natural;

   --
   -- Get_Time_Grid -- Get time axis grid
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- The time grid is  a set of vertical lines.  It is shown only when the
   -- time axis is visible.
   --
   -- Returns :
   --
   --    True if the time grid is shown
   --
   function Get_Time_Grid
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean;

   --
   -- Get_Time_Scale -- Get time scale visibility status
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    True if scale appears when sweeper is frozen
   --
   function Get_Time_Scale
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean;

   --
   -- Get_Time_Text_Angle -- Get time axis text angle
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The angle between the text line and x-axis of Cairo coordinates
   --
   function Get_Time_Text_Angle
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdouble;

   --
   -- Get_Time_Text_Color -- Get time axis text color
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The text color
   --
   function Get_Time_Text_Color
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdk.Color.Gdk_Color;

   --
   -- Get_Time_Text_Face -- Get time axis text font face
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The font face the result can be null handle is not used
   --
   function Get_Time_Text_Face
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Pango.Cairo.Fonts.Pango_Cairo_Font;

   --
   -- Get_Time_Text_Height -- Get time axis text height
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The text height
   --
   function Get_Time_Text_Height
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdouble;

   --
   -- Get_Time_Text_Horizontal_Alignment -- Get horizontal alignment
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The horizontal alignment of the time text annotation boxes
   --
   function Get_Time_Text_Horizontal_Alignment
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Strings.Alignment;

   --
   -- Get_Time_Text_Stretch -- Get time axis text stretch
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The text stretch
   --
   function Get_Time_Text_Stretch
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdouble;

   --
   -- Get_Time_Text_Vertical_Alignment -- Get vertical alignment
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- Returns :
   --
   --    The vertical alignment of the time text annotation boxes
   --
   function Get_Time_Text_Vertical_Alignment
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gtk.Layered.Vertical_Alignment;

   --
   -- Get_Time_Tooltip_Suffix -- Get the suffix of the value in the tooltip
   --
   --    Widget  - The oscilloscope
   --    Channel - Associated with the indicated value
   --
   -- Returns :
   --
   --    The suffix text
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Time_Tooltip_Suffix
     (Widget   : not null access constant Gtk_Oscilloscope_Record;
      Channel  : Channel_Number) return UTF8_String;

   --
   -- Get_Time_Tooltip -- Get tooltip time visibility status
   --
   --    Widget - The oscilloscope
   --
   -- Returns :
   --
   --    True when time is represented in the tiooltip text
   --
   function Get_Time_Tooltip
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean;

   --
   -- Get_To -- Get the last time time of the sweeper
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --
   -- This function returns the last time of the widget's sweeper
   --
   -- Returns :
   --
   --    The last time
   --
   function Get_To
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Real_Time.Time;

   function Get_To
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Calendar.Time;

   --
   -- Get_Tooltip_Annotation -- Get the annotation tooltip text
   --
   --    Widget  - The oscilloscope
   --    Channel - Associated with the indicated value
   --
   -- Returns :
   --
   --    The text introducing the channel appearance in the tooltip
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Tooltip_Annotation
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return UTF8_String;

   --
   -- Get_Type -- The type of the widget
   --
   -- Returns :
   --
   --    The GTK type of the widget
   --
   function Get_Type return GType;
   --
   -- Get_Undo_Stub -- Get the topmost stub of the undo stack
   --
   --    Widget - The oscilloscope
   --    Depth  - The stub depth, 1 is the topmost stub
   --
   -- Returns :
   --
   --    The stub name
   --
   -- Exceptions :
   --
   --    End_Error - There is no such stub
   --
   --
   function Get_Undo_Stub
     (Widget : not null access Gtk_Oscilloscope_Record;
      Depth  : Positive := 1) return UTF8_String;

   --
   -- Get_Value -- Get value at the vertical axis
   --
   --    Widget          - The oscilloscope
   --    Amplifier/Group - The amplifier type
   --    Y               - Vertical coordinate relative to Widget
   --
   -- Returns :
   --
   --    The value at Y
   --
   -- Exceptions :
   --
   --    Constraint_Error - No group assigned or wrong group
   --
   function Get_Value
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Y         : Gint) return Gdouble;

   function Get_Value
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      Y      : Gint) return Gdouble;

   --
   -- Get_Values_Axis -- Get vertical axis visibility status
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    True if the axis is visibile
   --
   function Get_Values_Axis
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean;

   --
   -- Get_Values_Axis_Annotation -- Get vertical axis annotation
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The annotation layer or null
   --
   function Get_Values_Axis_Annotation
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
      return access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class;

   --
   -- Get_Values_Axis_Width -- Get vertical axis width
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The horizontal space occupied by the axis when visible
   --
   function Get_Values_Axis_Width
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Natural;

   --
   -- Get_Values_Horizontal_Alignment -- Get vertical axis alignment
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The horizontal alignment
   --
   function Get_Values_Horizontal_Alignment
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Ada.Strings.Alignment;

   --
   -- Get_Values_Grid -- Get vertical axis grid
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- The  value  grid  is a set of horizontal lines. It is shown only when
   -- the value axis is visible.
   --
   -- Returns :
   --
   --    True if a group is assigned and the horizontal grid is shown
   --
   function Get_Values_Grid
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean;

   --
   -- Get_Values_Scale -- Get values scale visibility status
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    True if scale appears when the amplifier is in non-automatic mode
   --
   function Get_Values_Scale
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean;

   --
   -- Get_Values_Text_Angle -- Get vertical axis text angle
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The angle between the text line and x-axis of Cairo coordinates
   --
   function Get_Values_Text_Angle
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdouble;

   --
   -- Get_Values_Text_Color -- Get vertical axis text color
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The text color
   --
   function Get_Values_Text_Color
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdk.Color.Gdk_Color;

   --
   -- Get_Values_Text_Face -- Get vertical axis text font face
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The font face the result can be null handle is not used
   --
   function Get_Values_Text_Face
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Pango.Cairo.Fonts.Pango_Cairo_Font;

   --
   -- Get_Values_Text_Height -- Get vertical axis text height
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The text height
   --
   function Get_Values_Text_Height
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdouble;

   --
   -- Get_Values_Text_Stretch -- Get vertical axis text stretch
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    The text stretch
   --
   function Get_Values_Text_Stretch
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdouble;

   --
   -- Get_Values_Tooltip_Suffix -- Get the suffix of the value
   --
   --    Widget  - The oscilloscope
   --    Channel - Associated with the indicated value
   --
   -- Returns :
   --
   --    The suffix text
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Values_Tooltip_Suffix
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return UTF8_String;

   --
   -- Get_Waveform -- Get the waveform of a channel
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    The waveform of the channel
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   function Get_Waveform
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number)
      return not null access Gtk.Layered.Waveform.Waveform_Layer;

   --
   -- Get_X -- Get horizontal coordinate from time
   --
   --    Widget          - The oscilloscope
   --    Amplifier/Group - The amplifier type
   --    Stamp           - The time to convert
   --    Crop            - To the nearest margin of the waveform box
   --
   -- Returns :
   --
   --    The coordinate of Time
   --
   -- Exceptions :
   --
   --    Layout_Error - Not in the waveform box
   --
   function Get_X
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Real_Time.Time;
      Crop    : Boolean := False) return Gint;

   function Get_X
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Calendar.Time;
      Crop    : Boolean := False) return Gint;

   --
   -- Get_Y -- Get vertical coordinate from value
   --
   --    Widget          - The oscilloscope
   --    Amplifier/Group - The amplifier type
   --    Value           - The value to convert
   --    Crop            - To the nearest margin of the waveform box
   --
   -- Returns :
   --
   --    The coordinate of Value
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong group or no group assigned
   --    Layout_Error     - Not in the waveform box
   --
   function Get_Y
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Value     : Gdouble;
      Crop      : Boolean := False) return Gint;

   function Get_Y
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      Value  : Gdouble;
      Crop   : Boolean := False) return Gint;

   --
   -- Gtk_New -- Widget construction
   --
   --    Widget         - The result
   --    Lower_Sweeper  - The sweeper to use under the waveform box
   --    Upper_Sweeper  - The sweeper to use above the waveform box
   --    Refresh_Engine - The refresh engine or refresh period
   --    Background     - The background color
   --    Buffer_Size    - The default buffer size allocated for a channel
   --    Max_Channels   - The maximal number of channels
   --
   -- When no sweeper or no refresh engine is specified they are created as
   -- necessary.
   --
   procedure Gtk_New
     (Widget         : out Gtk_Oscilloscope;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Refresh_Engine : not null access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Background     : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Buffer_Size    : Positive  := 1024 * 60;
      Max_Channels   : Channel_Number := 64);

   procedure Gtk_New
     (Widget         : out Gtk_Oscilloscope;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Refresh_Period : Duration  := 0.02;
      Background     : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Buffer_Size    : Positive  := 1024 * 60;
      Max_Channels   : Channel_Number := 64);

   --
   -- Has_Group -- Check if amplifier has a group assigned
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --
   -- Returns :
   --
   --    True if the amplifier has a group
   --
   function Has_Group
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean;

   --
   -- Initialize -- The widget initialization
   --
   --    Widget         - The widget to initialize
   --    Lower_Sweeper  - The sweeper to use under the waveform box
   --    Upper_Sweeper  - The sweeper to use above the waveform box
   --    Refresh_Engine - The refresh engine
   --    Background     - The background color
   --    Buffer_Size    - The default buffer size allocated for a channel
   --
   procedure Initialize
     (Widget         : not null access Gtk_Oscilloscope_Record'Class;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Refresh_Engine : not null access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Background     : Gdk.Color.Gdk_Color;
      Buffer_Size    : Positive);

   procedure Initialize
     (Widget         : not null access Gtk_Oscilloscope_Record'Class;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Refresh_Period : Duration := 0.2;
      Background     : Gdk.Color.Gdk_Color;
      Buffer_Size    : Positive);

   --
   -- Is_Visible -- Channel visibility status
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --
   -- Returns :
   --
   --    True if channel is visible
   --
   function Is_Visible
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Boolean;

   --
   -- Move_Channel -- Reorder channels
   --
   --    Widget     - The oscilloscope
   --    Old_Number - The old channel number
   --    New_Number - The new channel number
   --
   -- This  procedure  changes the channel number Old_Number to New_Number.
   -- This   operation   also  changes  the numbers  of   channels  between
   -- Old_Number and New_Number.
   --
   -- Exception :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Move_Channel
     (Widget     : not null access Gtk_Oscilloscope_Record;
      Old_Number : Channel_Number;
      New_Number : Channel_Number);

   --
   -- On_Selection -- Action upon selection
   --
   --    Widget   - The oscilloscope
   --    Selected - The rectangular area selected
   --
   -- This procedure  is  called when  the  user selects a rectangular area
   -- of  the  oscilloscope  and  the current  selection  mode  is  set  to
   -- User_Action. The default implementation does nothing.
   --
   procedure On_Selection
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Selected : Cairo.Ellipses.Cairo_Box) is null;

   --
   -- Push_Stub -- Push stack stub
   --
   --    Widget - The oscilloscope
   --    Name   - Of the stub
   --
   -- The procedure  pushes  a stub  with  the name  Name onto the the undo
   -- stack.  All actions  after the stub can be reverted  using  a pattern
   -- that matches Name as the parameter Till in Undo.
   --
   procedure Push_Stub
     (Widget : not null access Gtk_Oscilloscope_Record;
      Name   : UTF8_String);

   --
   -- Push_Undo -- Push zooming state onto the undo stack
   --
   --    Widget - The oscilloscope
   --    State  - What state to save
   --
   -- The procedure pushes the data  to restore the current state specified
   -- onto the the undo stack.
   --
   procedure Push_Undo
     (Widget : not null access Gtk_Oscilloscope_Record;
      State  : Zooming_State := Values_Zooming or Time_Zooming);

   --
   -- Redo -- Repeat zooming change
   --
   --    Widget - The oscilloscope
   --    Till   - Wildcard pattern
   --    Stub   - The name of the new stub or empty string
   --
   -- When Till is empty the procedure repeats one  reverted user action of
   -- zooming  stored on  the top of the redo stack.  The top  of the  redo
   -- stack is popped. The information to revert this action is pushed onto
   -- the undo  stack.  When Till is not empty  the procedure  reverts  all
   -- actions until  a stub  action with the  name matched by the  wildcard
   -- pattern contained by Till. Additionally when Stub is not empty a stub
   -- is put onto the undo stack before the operation.
   --
   procedure Redo
     (Widget : not null access Gtk_Oscilloscope_Record;
      Till   : UTF8_String := "";
      Stub   : UTF8_String := "");

   --
   -- Set_Auto_Scaling -- Set auto scaling mode
   --
   --    Widget          - The oscilloscope
   --    Amplifier/Group - The amplifier or group
   --    Auto            - Scaling mode
   --
   -- Amplifier automatically  scales  its  page  to  fit  the  curve  when
   -- Auto is set true.
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel group number
   --
   procedure Set_Auto_Scaling
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Auto      : Boolean);

   procedure Set_Auto_Scaling
     (Widget : not null access Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      Auto   : Boolean);

   --
   -- Set_Default_Face -- Set the default font face to use for axis
   --
   --    Widget - The oscilloscope
   --    Face   - The face to use when an axis annotation is created
   --
   procedure Set_Default_Face
     (Widget : not null access Gtk_Oscilloscope_Record;
      Face   : Pango.Cairo.Fonts.Pango_Cairo_Font);

   --
   -- Set_Enabled_Dropdown_Items -- Set enabled menu items
   --
   --    Widget - The oscilloscope
   --    Items  - Enabled
   --
   procedure Set_Enabled_Dropdown_Items
     (Widget : not null access Gtk_Oscilloscope_Record;
      Items  : Dropdown_Items);

   --
   -- Set_Extrapolation_Mode -- Channel interpolation mode
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --    Left    - Extrapolate to the left
   --    Right   - Extrapolate to the right
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Set_Extrapolation_Mode
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Left    : Boolean;
      Right   : Boolean);

   --
   -- Set_Frequency -- Set the sweeping frequency
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Frames  - The number of frames per second
   --
   -- This procedure changes the sweeping frequency in frames  (pages)  per
   -- second. It is alternative to Set_Page_Span, which explicitly sets the
   -- page span.
   --
   procedure Set_Frequency
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Frames  : Gdouble);

   --
   -- Set_Frozen -- Set frozen state
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Frozen  - Desired state
   --
   -- When frozen the oscilloscope does not sweep its waveforms.
   --
   procedure Set_Frozen
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Frozen  : Boolean);

   --
   -- Set_Grid_Colors -- Set colors of grid lines
   --
   --    Widget      - The oscilloscope
   --    Major_Color - The color used for major tick lines
   --    Minor_Color - The color used for minor tick lines
   --
   procedure Set_Grid_Colors
     (Widget      : not null access Gtk_Oscilloscope_Record;
      Major_Color : Gdk.Color.Gdk_Color;
      Minor_Color : Gdk.Color.Gdk_Color);

   --
   -- Set_Group -- Change the group assigned to a vertical axis
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --    Group     - The group of channels
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong group number
   --
   procedure Set_Group
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Group     : Group_Number);

   --
   -- Set_Interpolation_Mode -- Channel interpolation mode
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --    Mode    - The interpolation mode
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Set_Interpolation_Mode
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Mode    : Gtk.Layered.Interpolation_Mode);

   --
   -- Set_Manual_Sweep -- Allow user to toggle sweepers
   --
   --    Widget - The oscilloscope
   --    Enable - True if user is allowed to freeze/release sweepers
   --
   procedure Set_Manual_Sweep
     (Widget : not null access Gtk_Oscilloscope_Record;
      Enable : Boolean);

   --
   -- Set_Page_Span -- Set the duration visible in the oscilloscope
   --
   --    Widget    - The oscilloscope
   --    Sweeper   - The sweeper type
   --    Page_Span - The duration between the oscilloscope margins
   --
   -- The page span determines the frequency of sweeping. Which is the page
   -- width / page span.
   --
   procedure Set_Page_Span
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Sweeper   : Sweeper_Type;
      Page_Span : Duration);

   --
   -- Set_Preferred_Method -- Set preferred rendering method
   --
   --    Widget - The oscilloscope
   --    Method - The preferred method
   --
   -- The rendering method  is selected  when the waveform parameters allow
   -- its application.
   --
   procedure Set_Preferred_Method
     (Widget : not null access Gtk_Oscilloscope_Record;
      Method : Gtk.Layered.Waveform_Drawing_Method);

   --
   -- Set_Release_To_Latest -- Allow user to toggle sweepers
   --
   --    Widget - The oscilloscope
   --    Enable - If sweepers when released by user jump to latest time
   --
   procedure Set_Release_To_Latest
     (Widget : not null access Gtk_Oscilloscope_Record;
      Enable : Boolean);

   --
   -- Set_Selection_Mode -- Define what to do on selection
   --
   --    Widget - The oscilloscope
   --    Action - The action to perform on selection
   --
   procedure Set_Selection_Mode
     (Widget : not null access Gtk_Oscilloscope_Record;
      Action : Selection_Action);

   --
   -- Set_Snapshot_File -- Set snapshot format and file name
   --
   --    Widget - The oscilloscope
   --    Format - The snapshot format
   --    Name   - The file name
   --
   -- This  procedure  is  used  to  set the format and name of the file to
   -- write  the  widget  contents  when the user presses the snapshot menu
   -- item.  When  Format  is  set to No_Snapshot or file name is empty, no
   -- menu item appears. When file is written snapshot-captured is emitted.
   -- The handler may change the file name upon this signal.
   --
   procedure Set_Snapshot_File
     (Widget : not null access Gtk_Oscilloscope_Record;
      Format : Snapshot_Format := No_Snapshot;
      Name   : String := "");

   --
   -- Set_Superscript -- Set subscript usage mode
   --
   --    Widget      - The oscilloscope
   --    Superscript - True if allowed
   --
   procedure Set_Superscript
     (Widget      : not null access Gtk_Oscilloscope_Record;
      Superscript : Boolean);

   --
   -- Set_Time -- The time at the right margin
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Stamp   - To set
   --
   procedure Set_Time
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Real_Time.Time);

   procedure Set_Time
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Calendar.Time);

   --
   -- Set_Time_Axis -- Change time axis visibility status
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Visible - The visibility status
   --    As_Time - Use time format for the axis
   --
   -- The  time  axis  renders  horizontal  values  in  the  time format by
   -- default.  When  As_Time  is  set  to  false the horizontal values are
   -- rendered as plain numbers.
   --
   procedure Set_Time_Axis
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Visible : Boolean;
      As_Time : Boolean := True);

   --
   -- Set_Time_Axis_Height -- Get vertical size of the axis
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Height  - The desired height of the axis when visible
   --
   procedure Set_Time_Axis_Height
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Height  : Natural);

   --
   -- Set_Time_Grid -- Change time grid visibility status
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Visible - The visibility status
   --
   procedure Set_Time_Grid
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Visible : Boolean);

   --
   -- Set_Time_Scale -- Change time scale visibility status
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Visible - The visibility status
   --
   -- This procedure changes the visibility status. When set the invisible,
   -- the  scale  is  never  shown.  When  set visible it is shown when the
   -- corresponding sweeper is frozen. In running mode scale is not shown.
   --
   procedure Set_Time_Scale
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Visible : Boolean);

   --
   -- Set_Time_Text_Alignment -- Set time annotation text alignment
   --
   --    Widget     - The oscilloscope
   --    Sweeper    - The sweeper type
   --    Horizontal - Horizontal alignment
   --    Vertical   - Vertical alignment
   --
   procedure Set_Time_Text_Alignment
     (Widget     : not null access Gtk_Oscilloscope_Record;
      Sweeper    : Sweeper_Type;
      Horizontal : Ada.Strings.Alignment;
      Vertical   : Gtk.Layered.Vertical_Alignment);

   --
   -- Set_Time_Text_Font -- Set font used for the time axis annotation
   --
   --    Widget  - The oscilloscope
   --    Sweeper - The sweeper type
   --    Face    - The font face
   --    Height  - Height
   --    Stretch - Stretch
   --    Color   - Color
   --    Angle   - The text angle
   --
   -- Exceptions :
   --
   --    Constraint_Error - Illegal parameter
   --
   procedure Set_Time_Text_Font
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Face    : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height  : Gdouble;
      Stretch : Gdouble   := 1.0;
      Color   : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle   : Gdouble   := 0.0);

   --
   -- Set_Time_Tooltip -- Change tooltip time visibility status
   --
   --    Widget  - The oscilloscope
   --    Visible - The visibility status
   --
   -- When set True, the time is represented in  the  tiooltip  text  shown
   -- when the mouse cursor is hovering above a waveform.
   --
   procedure Set_Time_Tooltip
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Visible : Boolean);

   --
   -- Set_Time_Tooltip_Suffix -- Set the suffix of the value in the tooltip
   --
   --    Widget  - The oscilloscope
   --    Channel - Associated with the indicated value
   --    Suffix  - The text to set
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Set_Time_Tooltip_Suffix
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Suffix  : UTF8_String);

   --
   -- Set_Tooltip_Annotation -- Set the tooltip annotation
   --
   --    Widget  - The oscilloscope
   --    Channel - Associated with the indicated value
   --    Text    - The text introducing the channel in the tooltip
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Set_Tooltip_Annotation
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Text    : UTF8_String);

   --
   -- Set_Values_Axis_Alignment -- Get vertical axis alignment
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --    Horizontal - Horizontal alignment
   --    Vertical   - Vertical alignment
   --
   procedure Set_Values_Alignment
     (Widget     : not null access Gtk_Oscilloscope_Record;
      Amplifier  : Amplifier_Type;
      Horizontal : Ada.Strings.Alignment;
      Vertical   : Gtk.Layered.Vertical_Alignment);

   --
   -- Set_Values_Axis -- Make visible values axis
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --    Visible   - The visibility status
   --
   -- Exceptions :
   --
   --    Constraint_Error - No group assigned
   --
   procedure Set_Values_Axis
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Visible   : Boolean);

   --
   -- Set_Values_Axis_Width -- Get vertical axis width
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --    Width     - The desired width of the axis when visible
   --
   procedure Set_Values_Axis_Width
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Width     : Natural);

   --
   -- Set_Values_Grid -- Change time grid visibility status
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --    Visible   - The visibility status
   --
   -- Exceptions :
   --
   --    Constraint_Error - No group assigned
   --
   procedure Set_Values_Grid
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Visible   : Boolean);

   --
   -- Set_Values_Scale -- Change values scale visibility status
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --    Visible   - The visibility status
   --
   -- This procedure changes the visibility status. When set the invisible,
   -- the  scale  is  never  shown.  When  set visible it is shown when the
   -- corresponding  amplifier is in non-automatic mode.  In automatic mode
   -- scale is not shown.
   --
   procedure Set_Values_Scale
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Visible   : Boolean);

   --
   -- Set_Values_Text_Font -- Set font used for the time axis annotation
   --
   --    Widget    - The oscilloscope
   --    Amplifier - The amplifier type
   --    Face      - The font face
   --    Height    - Height
   --    Stretch   - Stretch
   --    Color     - Color
   --    Angle     - The text angle
   --
   -- Exceptions :
   --
   --    Constraint_Error - Illegal parameter
   --
   procedure Set_Values_Text_Font
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Face      : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height    : Gdouble;
      Stretch   : Gdouble   := 1.0;
      Color     : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle     : Gdouble   := 0.0);

   --
   -- Set_Values_Tooltip_Suffix -- Set the suffix of the value
   --
   --    Widget  - The oscilloscope
   --    Channel - Associated with the indicated value
   --    Suffix  - The text to set
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Set_Values_Tooltip_Suffix
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Suffix  : UTF8_String);

   --
   -- Set_Visible -- Set channel visibility status
   --
   --    Widget  - The oscilloscope
   --    Channel - The channel number
   --    Visible - The visibility status
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong channel number
   --
   procedure Set_Visible
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Visible : Boolean);

   --
   -- Undo -- Revert zooming change
   --
   --    Widget - The oscilloscope
   --    Till   - Wildcard pattern
   --    Stub   - The name of the new stub or empty
   --
   -- When Till is empty the  procedure reverts  one user action  caused  a
   -- change  of zooming stored on the top  of the  undo stack.  The top of
   -- the undo stack  is popped.  The information  to revert this action is
   -- pushed onto the redo  stack.  When  Till is not empty  the  procedure
   -- reverts all actions until a stub  action with the name matched by the
   -- wildcard  pattern contained by Till.  Additionally  when Stub  is not
   -- empty a stub is put onto the redo stack before the operation.
   --
   procedure Undo
     (Widget : not null access Gtk_Oscilloscope_Record;
      Till   : UTF8_String := "";
      Stub   : UTF8_String := "");

private
   pragma Inline (Feed);
   pragma Inline (Get_Amplifier);
   pragma Inline (Get_Auto_Scaling);
   pragma Inline (Get_Channels_Number);
   pragma Inline (Get_Color);
   pragma Inline (Get_From);
   pragma Inline (Get_Frozen);
   pragma Inline (Get_Group);
   pragma Inline (Get_Groups_Number);
   pragma Inline (Get_Interpolation_Mode);
   pragma Inline (Get_Name);
   pragma Inline (Get_Offset);
   pragma Inline (Get_Page_Span);
   pragma Inline (Get_Snapshot_File);
   pragma Inline (Get_Snapshot_Format);
   pragma Inline (Get_Superscript);
   pragma Inline (Get_Sweeper);
   pragma Inline (Get_Time);
   pragma Inline (Get_Time_Axis);
   pragma Inline (Get_Time_Axis_As_Time);
   pragma Inline (Get_Time_Scale);
   pragma Inline (Get_Time_Text_Angle);
   pragma Inline (Get_Time_Text_Color);
   pragma Inline (Get_Time_Text_Face);
   pragma Inline (Get_Time_Grid);
   pragma Inline (Get_Time_Text_Height);
   pragma Inline (Get_Time_Text_Horizontal_Alignment);
   pragma Inline (Get_Time_Text_Stretch);
   pragma Inline (Get_Time_Text_Vertical_Alignment);
   pragma Inline (Get_Time_Tooltip);
   pragma Inline (Get_Values_Axis);
   pragma Inline (Get_Values_Axis_Width);
   pragma Inline (Get_Values_Horizontal_Alignment);
   pragma Inline (Get_Values_Grid);
   pragma Inline (Get_To);
   pragma Inline (Is_Visible);

   type Layered_Refresh_Engine_Ptr is
     access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;

   type Channel_Value is (Undefined, Absolute, Difference);
   type Waveform_Layer_Ptr is access all Gtk.Layered.Waveform.Waveform_Layer;
   type Channel_Data is record
      Waveform     : Waveform_Layer_Ptr;
      Source       : Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer;
      Status       : Channel_Value := Undefined;
      Group        : Group_Number;
      Tip_Prefix   : String_Ptr;
      Tip_X_Suffix : String_Ptr;
      Tip_Y_Suffix : String_Ptr;
      Value_1      : Gtk.Layered.Waveform.Y_Axis;
      Value_2      : Gtk.Layered.Waveform.Y_Axis;
   end record;
   type Channel_List is
     array (Channel_Number range <>) of aliased Channel_Data;

   type Group is record
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
   end record;
   type Group_List is array (Group_Number range <>) of Group;
   type Group_List_Ptr is access Group_List;

   type Line_Layer_Ptr is access all Gtk.Layered.Line.Line_Layer;
   type Rectangle_Layer_Ptr is access all Gtk.Layered.Rectangle.Rectangle_Layer;
   type Graph_Paper_Layer_Ptr is
     access all Gtk.Layered.Graph_Paper.Graph_Paper_Layer;

   type Graph_Paper_Annotation_Layer_Ptr is
     access all Gtk.Layered.Graph_Paper_Annotation.
       Graph_Paper_Annotation_Layer'Class;

   type Time_Axis_Data is record
      On         : Boolean                        := False;
      Grid       : Boolean                        := False;
      No_Scale   : Boolean                        := False;
      Width_Set  : Boolean                        := False;
      Time_Mode  : Boolean                        := True;
      Sweeper    : Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
      Face       : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Color      : Gdk.Color.Gdk_Color            := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle      : Gdouble                        := 0.0;
      Height     : Gdouble                        := 9.0;
      Stretch    : Gdouble                        := 1.0;
      Offset     : Gdouble                        := 0.0;
      Width      : Guint                          := 24;
      Channels   : Natural                        := 0;
      Justify_X  : Ada.Strings.Alignment          := Ada.Strings.Center;
      Justify_Y  : Gtk.Layered.Vertical_Alignment := Gtk.Layered.Center;
      Line       : Line_Layer_Ptr;
      Ticks      : Graph_Paper_Layer_Ptr;
      Box        : Gtk.Box.Gtk_Hbox;
      Left_Fill  : Gtk.Fixed.Gtk_Fixed;
      Right_Fill : Gtk.Fixed.Gtk_Fixed;
      Scale      : Gtk.Scale.Gtk_Hscale;
      Texts      : Graph_Paper_Annotation_Layer_Ptr;
   end record;
   type Time_Axis_Data_Ptr is access all Time_Axis_Data;

   type Sweeper_List is array (Sweeper_Type) of aliased Time_Axis_Data;

   type Values_Axis_Data is record
      On               : Boolean                        := False;
      Grid             : Boolean                        := False;
      No_Scale         : Boolean                        := False;
      Width_Set        : Boolean                        := False;
      Group            : Group_Count                    := 0;
      Face             : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Color            : Gdk.Color.Gdk_Color            := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle            : Gdouble                        := 0.0;
      Height           : Gdouble                        := 9.0;
      Stretch          : Gdouble                        := 1.0;
      Offset           : Gdouble                        := 0.0;
      Width            : Guint                          := 50;
      Justify_X        : Ada.Strings.Alignment          := Ada.Strings.Center;
      Justify_Y        : Gtk.Layered.Vertical_Alignment := Gtk.Layered.Center;
      Line             : Line_Layer_Ptr;
      Ticks            : Graph_Paper_Layer_Ptr;
      Texts            : Graph_Paper_Annotation_Layer_Ptr;
      Scale            : Gtk.Scale.Gtk_Vscale;
      Box              : Gtk.Box.Gtk_Vbox;
      Upper_Fill       : Gtk.Fixed.Gtk_Fixed;
      Lower_Fill       : Gtk.Fixed.Gtk_Fixed;
      Settings_Changed : Gtk.Handlers.References.Handler_Reference;
      Value_Changed    : Gtk.Handlers.References.Handler_Reference;
   end record;
   type Amplifier_List is array (Amplifier_Type) of Values_Axis_Data;

   type Do_Item;
   type Do_Item_Ptr is access Do_Item'Class;
   type Items_Stack is record
      Actions : Do_Item_Ptr;
      Stubs   : Do_Item_Ptr;
   end record;

   function Get_Stub
     (Stack : Items_Stack;
      Depth : Positive) return UTF8_String;

   type Selection_State (Size : Group_Number) is record
      Area    : Rectangle_Layer_Ptr;
      Right   : Boolean;
      Below   : Boolean;
      Engaged : Boolean := False;
      Saved   : Boolean := False;
   end record;
   type Selection_State_Ptr is access Selection_State;

   type Gtk_Graphs_Record is new Gtk.Layered.Gtk_Layered_Record with record
      Oscilloscope : Gtk_Oscilloscope;
      Last_Time    : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   end record;

   overriding procedure Refresh
     (Widget  : not null access Gtk_Graphs_Record;
      Context : Cairo.Cairo_Context);

   overriding procedure Resized
     (Widget     : not null access Gtk_Graphs_Record;
      Allocation : Gtk.Widget.Gtk_Allocation);

   type Do_Item is
     abstract new Ada.Finalization.Limited_Controlled with
      record
         First : Boolean     := False;
         Next  : Do_Item_Ptr := null;
      end record;
   --
   -- Do_It -- Undo zooming
   --
   --    Item         - The undo item
   --    First        - True if first in the sequence
   --    Oscilloscope - The oscilloscope
   --    Inverse      - The opposite list head
   --
   procedure Do_It
     (Item         : Do_Item;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null) is abstract;

   procedure Delete (List : in out Do_Item_Ptr);
   --
   -- Do_Auto_Amplifier -- Set auto scaling mode
   --
   type Do_Auto_Amplifier is new Do_Item with record
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
   end record;

   procedure Push_Auto_Amplifier
     (Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      List      : access Items_Stack;
      First     : in out Boolean);

   overriding
   procedure Do_It
     (Item         : Do_Auto_Amplifier;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null);

   overriding
   procedure Finalize (Item : in out Do_Auto_Amplifier);
   --
   -- Do_Amplifier_Zoom -- Set to fixed size
   --
   type Do_Amplifier_Zoom is new Do_Auto_Amplifier with record
      Value     : Gdouble;
      Page_Size : Gdouble;
   end record;
   procedure Push_Amplifier_Zoom
     (Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      List      : access Items_Stack;
      First     : in out Boolean);

   overriding
   procedure Do_It
     (Item         : Do_Amplifier_Zoom;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null);

   --
   -- Do_Release_Sweeper -- Set release mode
   --
   type Do_Release_Sweeper is new Do_Item with record
      Sweeper : Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
   end record;

   procedure Push_Release_Sweeper
     (Sweeper : Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
      List    : access Items_Stack;
      First   : in out Boolean);

   overriding
   procedure Do_It
     (Item         : Do_Release_Sweeper;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null);

   overriding
   procedure Finalize (Item : in out Do_Release_Sweeper);
   --
   -- Do_Sweeper_Zoom -- Set to fixed size
   --
   type Do_Sweeper_Zoom is new Do_Release_Sweeper with record
      Time : Ada.Real_Time.Time;
      Page : Duration;
   end record;

   procedure Push_Sweeper_Zoom
     (Sweeper : Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
      List    : access Items_Stack;
      First   : in out Boolean);

   overriding
   procedure Do_It
     (Item         : Do_Sweeper_Zoom;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null);

   --
   -- Do_Stub -- Undo stack stub
   --
   type Do_Stub (Length : Natural) is new Do_Item with record
      Stack    : not null access Items_Stack;
      Previous : Do_Item_Ptr;
      Name     : String (1 .. Length);
   end record;

   procedure Push_Stub
     (Name  : String;
      List  : access Items_Stack;
      First : in out Boolean);

   overriding
   procedure Do_It
     (Item         : Do_Stub;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null);

   type Gtk_Oscilloscope_Record (Size : Channel_Number) is
     new Gtk.Grid.Gtk_Grid_Record with
      record
         Refresh_Engine  : Layered_Refresh_Engine_Ptr;
         Background      : access Gtk.Layered.Rectangle.Rectangle_Layer;
         Proximity       : Gdouble                           := 15.0;
         Selection_Mode  : Selection_Action                  := Interactive;
         Menu_Enabled    : Dropdown_Items                    := Dropdown_Items'Last;
         -- Components
         Layers          : not null access Gtk_Graphs_Record :=
                             new Gtk_Graphs_Record;
         -- Axes
         Time_Axis       : Sweeper_List;
         Values_Axis     : Amplifier_List;
         -- Shared properties of waveforms
         Buffer_Size     : Positive                          := 1024 * 40;
         Line_Cap        : Cairo.Cairo_Line_Cap              := Cairo.Cairo_Line_Cap_Butt;
         Width           : Gdouble                           := 1.0;
         Opacity         : Gtk.Layered.Fill_Opacity          := 0.0;
         Widened         : Boolean                           := False;
         Show_Time       : Boolean                           := True;
         Manual_Sweep    : Boolean                           := False;
         Jump_On_Thaw    : Boolean                           := False;
         Superscript     : Boolean                           := True;
         -- List stores
         Channel_Names   : Gtk.List_Store.Gtk_List_Store;
         Group_Names     : Gtk.List_Store.Gtk_List_Store;
         -- Default font
         Default_Face    : Pango.Cairo.Fonts.Pango_Cairo_Font :=
                             Pango.Cairo.Fonts.Create_Pango ("arial unicode ms");
         -- Channels and groups
         Channels_Number : Channel_Count                     := 0;
         Groups_Number   : Group_Count                       := 0;
         Refresh_Period  : Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer;
         Drawing_Time    : Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Waveform_Ring_Data_Buffer;
         Channels        : Channel_List (1 .. Size);
         Groups          : Group_List_Ptr;
         Selection       : Selection_State_Ptr;
         Tip_Text        : String (1 .. 2048);
         -- Undo/Redo stacks
         Undo_Stack      : aliased Items_Stack;
         Redo_Stack      : aliased Items_Stack;
         -- Grid parameters
         Major_Color     : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
         Minor_Color     : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.5, 0.5, 0.5);
         -- Snapshots
         Format          : Snapshot_Format                   := No_Snapshot;
         File            : String_Ptr;
      end record;

   procedure Fix_Numbers
     (Widget : not null access Gtk_Oscilloscope_Record;
      Start  : Channel_Number;
      Stop   : Channel_Number);

   function Get_Annotation_Height
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gint;

   function Get_Annotation_Width
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gint;

   function Get_X
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Gdouble;
      Crop    : Boolean) return Gint;

   procedure Change_Selection
     (Oscilloscope : not null access Gtk_Oscilloscope_Record;
      Point        : Cairo.Ellipses.Cairo_Tuple);

   function Mouse_Event
     (Oscilloscope : not null access Gtk_Oscilloscope_Record;
      Event        : Gdk.Event.Gdk_Event;
      Hint         : Boolean) return Cairo.Ellipses.Cairo_Tuple;

   procedure On_Autoscaling_Changed
     (Amplifier    : access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   function On_Button_Press
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean;

   function On_Button_Release
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean;

   procedure On_Cancel_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Copy_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Destroy
     (Object       : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Difference_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Format_Time
     (Scale     : not null access Gtk.Scale.Gtk_Scale_Record'Class;
      Arguments : Glib.Values.GValue_Array;
      Result    : in out Glib.Values.GValue;
      Data      : Time_Axis_Data_Ptr);

   procedure On_Freezing_Changed
     (Sweeper      : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Latest
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   function On_Leave
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean;

   function On_Motion
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean;

   procedure On_Pause
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Range_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Redo
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Release
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Snapshot
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Toggle_Grid
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Toggle_Interpolation
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Undo
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Style_Updated
     (Object       : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Zoom_In
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Zoom_In_T
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Zoom_In_V
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Zoom_Out
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Zoom_Out_T
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Zoom_Out_V
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Offset_Changed
     (Sweeper      : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure On_Raster_Mode_Changed
     (Amplifier    : access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class;
      Oscilloscope : Gtk_Oscilloscope);

   procedure Restore_State
     (Widget : not null access Gtk_Oscilloscope_Record);

   procedure Save_Amplifier
     (Widget : not null access Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      First  : in out Boolean);

   procedure Save_Sweeper
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      First   : in out Boolean);

   procedure Style_Changed_Time_Axis
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type);

   procedure Style_Changed_Values_Axis
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type);

   procedure Update_Value
     (Widget : not null access Gtk_Oscilloscope_Record);

   procedure Update_Amplifier
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type);

   procedure Zoom_In
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      T1, T2  : Ada.Calendar.Time);

   procedure Zoom_In
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      V1, V2    : Gdouble);

   procedure Zoom_Out
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      T1, T2  : Ada.Calendar.Time);

   procedure Zoom_Out
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      V1, V2    : Gdouble);

   package Menu_Handlers is
     new Gtk.Handlers.User_Callback (GObject_Record, Gtk_Oscilloscope);

   package Oscilloscope_Handlers is
     new Gtk.Handlers.User_Return_Callback (GObject_Record,
                                            Boolean,
                                            Gtk_Oscilloscope);

   package Amplifier_Handlers is
     new Gtk.Handlers.User_Callback
       (Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record,
        Gtk_Oscilloscope);

   package Format_Handlers is
     new Gtk.Handlers.Generic_Callback (Gtk.Scale.Gtk_Scale_Record,
                                        Time_Axis_Data_Ptr);

   package Sweeper_Handlers is
     new Gtk.Handlers.User_Callback
       (Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record,
        Gtk_Oscilloscope);

   pragma Warnings (On, "declaration hides ""Group""");
   pragma Warnings (On, "declaration hides ""Left""");
   pragma Warnings (On, "declaration hides ""Oscilloscope""");
   pragma Warnings (On, "declaration hides ""Refresh_Period""");
   pragma Warnings (On, "declaration hides ""Right""");
   pragma Warnings (On, "declaration hides ""Scale""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Oscilloscope;
