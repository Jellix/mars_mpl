--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--  Implementation                                 Summer, 2011       --
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

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;

with Cairo.Elementary_Functions;
with Cairo.Line_Cap_Property;
with Cairo.PDF;
with Cairo.SVG;
with Gdk.Color.IHLS;
with Gdk.Types;
with Glib.Messages;
with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Gtkada.Types;
with Gtk.Adjustment;
with Gtk.Enums;
with Gtk.Image;
with Gtk.Image_Menu_Item;
with Gtk.Menu;
with Gtk.Separator_Menu_Item;
with Gtk.Stock;
with Gtk.Tree_Model;
with Gtk.Widget.Styles.Line_Cap_Property;
with Strings_Edit.Integers;
with Strings_Edit.UTF8.Wildcards.Case_Insensitive;

package body Gtk.Oscilloscope is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Amplifier""");
   pragma Warnings (Off, "declaration hides ""Box""");
   pragma Warnings (Off, "declaration hides ""Group""");
   pragma Warnings (Off, "declaration hides ""Left""");
   pragma Warnings (Off, "declaration hides ""Lower""");
   pragma Warnings (Off, "declaration hides ""Menu""");
   pragma Warnings (Off, "declaration hides ""Oscilloscope""");
   pragma Warnings (Off, "declaration hides ""Params""");
   pragma Warnings (Off, "declaration hides ""Refresh_Period""");
   pragma Warnings (Off, "declaration hides ""Right""");
   pragma Warnings (Off, "declaration hides ""Scale""");
   pragma Warnings (Off, "declaration hides ""Sweeper""");
   pragma Warnings (Off, "declaration hides ""Value""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   function "+" (Value : String_Ptr) return UTF8_String;
   function "+" (Value : String_Ptr) return UTF8_String is
   begin
      if Value = null then
         return "";
      else
         return Value.all;
      end if;
   end "+";

   procedure Free is
     new Ada.Unchecked_Deallocation (Gtk.Layered.Line.Line_Layer,
                                     Line_Layer_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Gtk.Layered.Graph_Paper.Graph_Paper_Layer,
                                     Graph_Paper_Layer_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class,
      Graph_Paper_Annotation_Layer_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Group_List, Group_List_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Do_Item'Class, Do_Item_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation
       (Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine,
        Layered_Refresh_Engine_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Gtk.Layered.Rectangle.Rectangle_Layer,
                                     Rectangle_Layer_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Selection_State, Selection_State_Ptr);

   Signal_Names : constant Gtkada.Types.Chars_Ptr_Array :=
                    (0  => Interfaces.C.Strings.New_String ("autoscaling-changed"),
                     1  => Interfaces.C.Strings.New_String ("raster-mode-changed"),
                     2  => Interfaces.C.Strings.New_String ("freezing-changed"),
                     3  => Interfaces.C.Strings.New_String ("offset-changed"),
                     4  => Interfaces.C.Strings.New_String ("group-changed"),
                     5  => Interfaces.C.Strings.New_String ("x-axis-toggled"),
                     6  => Interfaces.C.Strings.New_String ("y-axis-toggled"),
                     7  => Interfaces.C.Strings.New_String ("x-grid-toggled"),
                     8  => Interfaces.C.Strings.New_String ("y-grid-toggled"),
                     9  => Interfaces.C.Strings.New_String ("visibility-toggled"),
                     10 => Interfaces.C.Strings.New_String ("interpolation-changed"),
                     11 => Interfaces.C.Strings.New_String ("position-changed"),
                     12 => Interfaces.C.Strings.New_String ("channel-added"),
                     13 => Interfaces.C.Strings.New_String ("channel-deleted"),
                     14 => Interfaces.C.Strings.New_String ("snapshot-captured"),
                     15 => Interfaces.C.Strings.New_String ("extrapolation-changed"));
   Signal_IDs   : array (Signal_Names'Range) of Signal_Id :=
                    (others => Invalid_Signal_Id);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Cycle           : constant := 6;
   Selection_Color : constant Gdk.Color.Gdk_Color      := Gtk.Missed.RGB (1.0, 0.0, 0.0);
   First_Color     : constant Gdk.Color.IHLS.Gdk_IHLS_Color :=
                       Gdk.Color.IHLS.To_IHLS (Gtk.Missed.RGB (1.0, 0.0, 0.0));

   procedure Emit (Widget : not null access Gtk_Oscilloscope_Record'Class;
                   Signal : Signal_Id;
                   Value  : String);
   procedure Emit (Widget  : not null access Gtk_Oscilloscope_Record'Class;
                   Signal  : Signal_Id;
                   Value_1 : Guint;
                   Value_2 : Gdouble;
                   Value_3 : Gdouble);
   procedure Emit (Widget : not null access Gtk_Oscilloscope_Record'Class;
                   Signal : Signal_Id;
                   Value  : Guint);
   procedure EmitV (Params : System.Address;
                    Signal : Signal_Id;
                    Quark  : GQuark;
                    Result : System.Address);
   pragma Import (C, EmitV, "g_signal_emitv");

   function Where (Name : String) return String;

   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Group   : Group_Number;
      Color   : Gdk.Color.Gdk_Color;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number is
   begin
      if Widget.all.Channels_Number >= Widget.all.Size then
         raise Constraint_Error with
           ("More than" & Channel_Count'Image (Widget.all.Size) & " channels");
      end if;
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      end if;
      return Index : constant Channel_Number :=
        Widget.all.Channels_Number + 1 do
         declare
            This : Channel_Data renames Widget.all.Channels (Index);
            Row  : Gtk.Tree_Model.Gtk_Tree_Iter;
         begin
            This.Waveform :=
              Gtk.Layered.Waveform.Add_Waveform
                (Under     => Widget.all.Layers,
                 Box       => Widget.all.Get_Box,
                 Width     => Widget.all.Width,
                 Color     => Color,
                 Line_Cap  => Widget.all.Line_Cap,
                 Sweeper   => Widget.all.Time_Axis (Sweeper).Sweeper,
                 Amplifier => Widget.all.Groups.all (Group).Amplifier,
                 Mode      => Mode,
                 Left      => Left,
                 Right     => Right,
                 Opacity   => Widget.all.Opacity,
                 Scaled    => False,
                 Widened   => Widget.all.Widened).all'Unchecked_Access;
            Widget.all.Channel_Names.all.Append (Row);
            if Name'Length = 0 then
               Widget.all.Channel_Names.all.Set
                 (Row,
                  0,
                  "Channel " & Channel_Number'Image (Index));
            else
               Widget.all.Channel_Names.all.Set (Row, 0, Name);
            end if;
            Widget.all.Channel_Names.all.Set (Row, 1, Gint (Index));
            Widget.all.Channel_Names.all.Set (Row, 2, Gint (Group));
            Widget.all.Channel_Names.all.Set (Row, 3, True);
            Widget.all.Channel_Names.all.Set (Row, 4, Gtk.Layered.Interpolation_Mode'Pos (Mode));
            Widget.all.Channel_Names.all.Set (Row, 6, Left);
            Widget.all.Channel_Names.all.Set (Row, 7, Right);
            This.Group := Group;
            if Buffer = null then
               Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_New
                 (This.Source, Widget.all.Buffer_Size);
            else
               This.Source := Buffer.all'Unchecked_Access;
               This.Source.all.Ref;
            end if;
            This.Waveform.all.Set_Source (This.Source.all);
            Widget.all.Time_Axis (Sweeper).Channels :=
              Widget.all.Time_Axis (Sweeper).Channels + 1;
         end;
         Widget.all.Channels_Number := Index;
         Emit (Widget, Signal_IDs (12), Guint (Index));
      end return;
   end Add_Channel;

   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Group   : Group_Number;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number is
   begin
      return
        Add_Channel (Widget  => Widget,
                     Group   => Group,
                     Mode    => Mode,
                     Left    => Left,
                     Right   => Right,
                     Name    => Name,
                     Sweeper => Sweeper,
                     Buffer  => Buffer,
                     Color   =>
                       Gdk.Color.IHLS.To_RGB
                         (Gdk.Color.IHLS.Val
                              (First_Color,
                               Natural (Widget.all.Channels_Number),
                               Cycle)));
   end Add_Channel;

   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Color   : Gdk.Color.Gdk_Color;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number is
   begin
      return
        Add_Channel
          (Widget  => Widget,
           Group   => Add_Group (Widget),
           Color   => Color,
           Mode    => Mode,
           Left    => Left,
           Right   => Right,
           Name    => Name,
           Buffer  => Buffer,
           Sweeper => Sweeper);
   end Add_Channel;

   function Add_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Mode    : Gtk.Layered.Interpolation_Mode := Gtk.Layered.Linear;
      Left    : Boolean := False;
      Right   : Boolean := False;
      Name    : String  := "";
      Sweeper : Sweeper_Type := Lower;
      Buffer  : access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null)
      return Channel_Number is
   begin
      return
        Add_Channel
          (Widget  => Widget,
           Group   => Add_Group (Widget),
           Mode    => Mode,
           Left    => Left,
           Right   => Right,
           Name    => Name,
           Sweeper => Sweeper,
           Buffer  => Buffer,
           Color   =>
             Gdk.Color.IHLS.To_RGB
               (Gdk.Color.IHLS.Val
                  (First_Color,
                   Natural (Widget.all.Channels_Number),
                   Cycle)));
   end Add_Channel;

   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Group    : Group_Number;
      Color    : Gdk.Color.Gdk_Color;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number is
   begin
      if Widget.all.Channels_Number >= Widget.all.Size then
         raise Constraint_Error with
           ("More than" & Channel_Count'Image (Widget.all.Size) & " channels");
      end if;
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      end if;
      return Index : constant Channel_Number :=
        Widget.all.Channels_Number + 1 do
         declare
            use type Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer;
            This : Channel_Data renames Widget.all.Channels (Index);
            Row  : Gtk.Tree_Model.Gtk_Tree_Iter;
         begin
            This.Waveform :=
              Gtk.Layered.Waveform.Add_Waveform
                (Under     => Widget.all.Layers,
                 Box       => Widget.all.Get_Box,
                 Width     => Widget.all.Width,
                 Color     => Color,
                 Line_Cap  => Widget.all.Line_Cap,
                 Sweeper   => Widget.all.Time_Axis (Sweeper).Sweeper,
                 Amplifier => Widget.all.Groups.all (Group).Amplifier,
                 Mode      => Gtk.Layered.Left,
                 Opacity   => Widget.all.Opacity,
                 Scaled    => False,
                 Widened   => Widget.all.Widened).all'Unchecked_Access;
            Widget.all.Channel_Names.all.Append (Row);
            if Name'Length = 0 then
               case Measured is
                  when Refresh_Period =>
                     Widget.all.Channel_Names.all.Set (Row,
                                                       0,
                                                       "Refresh period");
                  when Drawing_Time =>
                     Widget.all.Channel_Names.all.Set (Row,
                                                       0,
                                                       "Drawing time");
               end case;
            else
               Widget.all.Channel_Names.all.Set (Row, 0, Name);
            end if;
            Widget.all.Channel_Names.all.Set (Row, 1, Gint (Index));
            Widget.all.Channel_Names.all.Set (Row, 2, Gint (Group));
            Widget.all.Channel_Names.all.Set (Row, 3, True);
            Widget.all.Channel_Names.all.Set
              (Row,
               4,
               Gtk.Layered.Interpolation_Mode'Pos (Gtk.Layered.Left));
            Widget.all.Channel_Names.all.Set (Row, 6, False);
            Widget.all.Channel_Names.all.Set (Row, 7, False);
            case Measured is
               when Refresh_Period =>
                  if Widget.all.Refresh_Period = null then
                     Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_New
                       (Widget.all.Refresh_Period, Widget.all.Buffer_Size);
                  end if;
                  This.Source := Widget.all.Refresh_Period;
               when Drawing_Time =>
                  if Widget.all.Drawing_Time = null then
                     Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_New
                       (Widget.all.Drawing_Time, Widget.all.Buffer_Size);
                  end if;
                  This.Source := Widget.all.Drawing_Time;
            end case;
            This.Source.all.Ref;
            This.Group := Group;
            This.Waveform.all.Set_Source (This.Source.all);
         end;
         Widget.all.Channels_Number := Index;
         Emit (Widget, Signal_IDs (12), Guint (Index));
      end return;
   end Add_Deviation_Channel;

   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Group    : Group_Number;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number is
   begin
      return
        Add_Deviation_Channel
          (Widget   => Widget,
           Group    => Group,
           Measured => Measured,
           Name     => Name,
           Sweeper  => Sweeper,
           Color    =>
             Gdk.Color.IHLS.To_RGB
               (Gdk.Color.IHLS.Val
                  (First_Color,
                   Natural (Widget.all.Channels_Number),
                   Cycle)));
   end Add_Deviation_Channel;

   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Color    : Gdk.Color.Gdk_Color;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number is
   begin
      return
        Add_Deviation_Channel
          (Widget   => Widget,
           Group    => Add_Group (Widget),
           Measured => Measured,
           Color    => Color,
           Name     => Name,
           Sweeper  => Sweeper);
   end Add_Deviation_Channel;

   function Add_Deviation_Channel
     (Widget   : not null access Gtk_Oscilloscope_Record;
      Measured : Drawing_Measurement_Point := Refresh_Period;
      Name     : String := "";
      Sweeper  : Sweeper_Type := Lower) return Channel_Number is
   begin
      return
        Add_Deviation_Channel
          (Widget   => Widget,
           Group    => Add_Group (Widget),
           Measured => Measured,
           Sweeper  => Sweeper,
           Name     => Name,
           Color    =>
             Gdk.Color.IHLS.To_RGB
               (Gdk.Color.IHLS.Val
                  (First_Color,
                   Natural (Widget.all.Channels_Number),
                   Cycle)));
   end Add_Deviation_Channel;

   function Add_Group
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Name      : String := "";
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier := null)
      return Group_Number
   is
      use type Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Widget.all.Groups_Number >= Widget.all.Groups'Last then
         raise Constraint_Error with
           ("More than" & Channel_Count'Image (Widget.all.Size) & " groups");
      end if;
      return Index : constant Group_Number :=
        Widget.all.Groups_Number + 1 do
         if Amplifier = null then
            Gtk.Layered.Waveform.Amplifier.Gtk_New
              (Widget.all.Groups.all (Index).Amplifier);
         else
            Widget.all.Groups.all (Index).Amplifier := Amplifier;
         end if;
         Amplifier_Handlers.Connect
           (Widget.all.Groups.all (Index).Amplifier,
            "autoscaling-changed",
            On_Autoscaling_Changed'Access,
            Widget.all'Unchecked_Access);
         Amplifier_Handlers.Connect
           (Widget.all.Groups.all (Index).Amplifier,
            "raster-mode-changed",
            On_Raster_Mode_Changed'Access,
            Widget.all'Unchecked_Access);
         Widget.all.Groups.all (Index).Amplifier.all.Ref;
         Widget.all.Group_Names.all.Append (Row);
         if Name'Length = 0 then
            Widget.all.Group_Names.all.Set
              (Row,
               0,
               "Group" & Group_Count'Image (Index));
         else
            Widget.all.Group_Names.all.Set (Row, 0, Name);
         end if;
         Widget.all.Groups_Number := Index;
      end return;
   end Add_Group;

   function Add_Shadow_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Color   : Gdk.Color.Gdk_Color;
      Name    : String := "";
      Sweeper : Sweeper_Type := Upper) return Channel_Number is
   begin
      if Widget.all.Channels_Number >= Widget.all.Size then
         raise Constraint_Error with
           ("More than" & Channel_Count'Image (Widget.all.Size) & " channels");
      elsif Channel > Widget.all.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      end if;
      return Index : constant Channel_Number :=
        Widget.all.Channels_Number + 1 do
         declare
            That : Channel_Data renames Widget.all.Channels (Channel);
            This : Channel_Data renames Widget.all.Channels (Index);
            Row  : Gtk.Tree_Model.Gtk_Tree_Iter;
         begin
            This.Waveform :=
              Gtk.Layered.Waveform.Add_Waveform
                (Under     => Widget.all.Layers,
                 Box       => Widget.all.Get_Box,
                 Width     => Widget.all.Width,
                 Color     => Color,
                 Line_Cap  => Widget.all.Line_Cap,
                 Sweeper   => Widget.all.Time_Axis (Sweeper).Sweeper,
                 Amplifier => Widget.all.Groups.all (That.Group).Amplifier,
                 Mode      => That.Waveform.all.Get_Interpolation_Mode,
                 Left      => That.Waveform.all.Get_Left_Extrapolation_Mode,
                 Right     => That.Waveform.all.Get_Right_Extrapolation_Mode,
                 Opacity   => Widget.all.Opacity,
                 Scaled    => False,
                 Widened   => Widget.all.Widened).all'Unchecked_Access;
            Widget.all.Channel_Names.all.Append (Row);
            if Name'Length = 0 then
               Widget.all.Channel_Names.all.Set
                 (Row,
                  0,
                  Widget.all.Get_Name (Channel) & " (shadow)");
            else
               Widget.all.Channel_Names.all.Set (Row, 0, Name);
            end if;
            Widget.all.Channel_Names.all.Set (Row, 1, Gint (Index));
            Widget.all.Channel_Names.all.Set (Row, 2, Gint (That.Group));
            Widget.all.Channel_Names.all.Set (Row, 3, True);
            Widget.all.Channel_Names.all.Set
              (Row,
               4,
               Gtk.Layered.Interpolation_Mode'Pos
                 (That.Waveform.all.Get_Interpolation_Mode));
            Widget.all.Channel_Names.all.Set
              (Row,
               6,
               That.Waveform.all.Get_Left_Extrapolation_Mode);
            Widget.all.Channel_Names.all.Set
              (Row,
               7,
               That.Waveform.all.Get_Right_Extrapolation_Mode);
            This.Group  := That.Group;
            This.Source := That.Source;
            This.Source.all.Ref;
            This.Waveform.all.Set_Source (This.Source.all);
            Widget.all.Time_Axis (Sweeper).Channels :=
              Widget.all.Time_Axis (Sweeper).Channels + 1;
         end;
         Widget.all.Channels_Number := Index;
         Emit (Widget, Signal_IDs (12), Guint (Index));
      end return;
   end Add_Shadow_Channel;

   function Add_Shadow_Channel
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Name    : String := "";
      Sweeper : Sweeper_Type := Upper) return Channel_Number is
   begin
      return
        Add_Shadow_Channel
          (Widget  => Widget,
           Channel => Channel,
           Sweeper => Sweeper,
           Name    => Name,
           Color   =>
             Gdk.Color.IHLS.To_RGB
               (Gdk.Color.IHLS.Val
                  (First_Color,
                   Natural (Widget.all.Channels_Number),
                   Cycle)));
   end Add_Shadow_Channel;

   procedure Box_Changed
     (Widget : not null access Gtk_Oscilloscope_Record'Class);
   procedure Box_Changed
     (Widget : not null access Gtk_Oscilloscope_Record'Class)
   is
      Box : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
   begin
      Widget.all.Background.all.Set
        (Box        => Box,
         Color      => Widget.all.Background.all.Get_Color,
         Line_Width => 0.0,
         Opacity    => Widget.all.Background.all.Get_Opacity);
      for Index in 1 .. Widget.all.Channels_Number loop
         declare
            This : Gtk.Layered.Waveform.Waveform_Layer renames
                     Widget.all.Channels (Index).Waveform.all;
         begin
            This.Set
              (Box     => Box,
               Line    => This.Get_Line,
               Mode    => This.Get_Interpolation_Mode,
               Left    => This.Get_Left_Extrapolation_Mode,
               Right   => This.Get_Right_Extrapolation_Mode,
               Opacity => This.Get_Opacity);
         end;
      end loop;
      for Sweeper in Sweeper_Type'Range loop
         Style_Changed_Time_Axis (Widget, Sweeper);
      end loop;
      for Amplifier in Amplifier_Type'Range loop
         Style_Changed_Values_Axis (Widget, Amplifier);
      end loop;
   end Box_Changed;

   procedure Capture_PDF
     (Widget : not null access Gtk_Oscilloscope_Record;
      File   : UTF8_String)
   is
      Surface : Cairo.Cairo_Surface;
   begin
      Surface :=
        Cairo.PDF.Create
          (Filename         => File,
           Width_In_Points  => Gdouble (Widget.all.Get_Allocated_Width),
           Height_In_Points => Gdouble (Widget.all.Get_Allocated_Height));
      Widget.all.Layers.all.Snapshot (Surface);
      Cairo.Surface_Destroy (Surface);
   exception
      when others =>
         Cairo.Surface_Destroy (Surface);
         raise;
   end Capture_PDF;

   procedure Capture_SVG (Widget : not null access Gtk_Oscilloscope_Record;
                          File   : UTF8_String)
   is
      Surface : Cairo.Cairo_Surface;
   begin
      Surface :=
        Cairo.SVG.Create
          (Filename        => File,
           Width_In_Point  => Gdouble (Widget.all.Get_Allocated_Width),
           Height_In_Point => Gdouble (Widget.all.Get_Allocated_Height));
      Widget.all.Layers.all.Snapshot (Surface);
      Cairo.Surface_Destroy (Surface);
   exception
      when others =>
         Cairo.Surface_Destroy (Surface);
         raise;
   end Capture_SVG;

   procedure Change_Selection
     (Oscilloscope : not null access Gtk_Oscilloscope_Record;
      Point        : Cairo.Ellipses.Cairo_Tuple)
   is
      Selected : Cairo.Ellipses.Cairo_Box :=
                   Oscilloscope.all.Selection.all.Area.all.Get_Box;
      Area     : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Get_Box;
      X, Y     : Gdouble;
   begin
      if Point.X <= Area.X1 then
         X := Area.X1;
      elsif Point.X >= Area.X2 then
         X := Area.X2;
      else
         X := Point.X;
      end if;
      if Point.Y < Area.Y1 then
         Y := Area.Y1;
      elsif Point.Y > Area.Y2 then
         Y := Area.Y2;
      else
         Y := Point.Y;
      end if;
      if X < Selected.X1 then
         Selected.X1 := X;
         Oscilloscope.all.Selection.all.Right := False;
      elsif X > Selected.X2 then
         Selected.X2 := X;
         Oscilloscope.all.Selection.all.Right := True;
      else
         if Oscilloscope.all.Selection.all.Right then
            Selected.X2 := X;
         else
            Selected.X1 := X;
         end if;
      end if;
      if Y < Selected.Y1 then
         Selected.Y1 := Y;
         Oscilloscope.all.Selection.all.Below := False;
      elsif Y > Selected.Y2 then
         Selected.Y2 := Y;
         Oscilloscope.all.Selection.all.Below := True;
      else
         if Oscilloscope.all.Selection.all.Below then
            Selected.Y2 := Y;
         else
            Selected.Y1 := Y;
         end if;
      end if;
      Oscilloscope.all.Selection.all.Area.all.Set
        (Box        => Selected,
         Line_Width => Oscilloscope.all.Selection.all.Area.all.Get_Line_Width,
         Opacity    => Oscilloscope.all.Selection.all.Area.all.Get_Opacity,
         Color      => Oscilloscope.all.Selection.all.Area.all.Get_Color);
   end Change_Selection;

   function Create_Annotation
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
      return not null access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class
   is
      Data  : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
      Layer : Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer renames
                Gtk.Layered.Graph_Paper_Annotation.Add_Graph_Paper_Annotation
                  (Under       => Data.Line.all.Atop,
                   Paper       => Data.Ticks,
                   Face        => Data.Face,
                   Color       => Data.Color,
                   Height      => Data.Height,
                   Stretch     => Data.Stretch,
                   Text_Angle  => Data.Angle,
                   Justify_X   => Data.Justify_X,
                   Justify_Y   => Data.Justify_Y,
                   Superscript => Widget.all.Superscript,
                   Background  =>
                     Gtk.Widget.Styles.Style_Get
                       (Widget,
                        "values-text-border-color",
                        Gtk.Missed.RGB (1.0, 1.0, 1.0)),
                   Border      =>
                     Gdouble
                       (Guint'
                            (Gtk.Widget.Styles.Style_Get
                                 (Widget, "values-text-border"))),
                   Overlap     =>
                     Gdouble
                       (Gint'
                            (Gtk.Widget.Styles.Style_Get
                                 (Widget, "values-text-overlap"))),
                   Opacity     =>
                     Gdouble'
                       (Gtk.Widget.Styles.Style_Get
                            (Widget,
                             "values-text-border-opacity"))).all;
   begin
      return Layer'Unchecked_Access;
   end Create_Annotation;

   function Create_Annotation
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type)
      return not null access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class
   is
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if Data.Time_Mode then
         declare
            Layer : Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Time_Annotation_Layer renames
                      Gtk.Layered.Graph_Paper_Annotation.Add_Graph_Paper_Time_Annotation
                        (Under      => Data.Line.all.Atop,
                         Paper      => Data.Ticks,
                         Face       => Data.Face,
                         Color      => Data.Color,
                         Height     => Data.Height,
                         Stretch    => Data.Stretch,
                         Text_Angle => Data.Angle,
                         Justify_X  => Data.Justify_X,
                         Justify_Y  => Data.Justify_Y,
                         Background =>
                           Gtk.Widget.Styles.Style_Get
                             (Widget,
                              "time-text-border-color",
                              Gtk.Missed.RGB (1.0, 1.0, 1.0)),
                         Border     =>
                           Gdouble
                             (Guint'
                                  (Gtk.Widget.Styles.Style_Get
                                       (Widget, "time-text-border"))),
                         Overlap    =>
                           Gdouble
                             (Gint'
                                  (Gtk.Widget.Styles.Style_Get
                                       (Widget, "time-text-overlap"))),
                         Opacity    =>
                           Gdouble'
                             (Gtk.Widget.Styles.Style_Get
                                  (Widget, "time-text-border-opacity"))) .all;
         begin
            return Layer'Unchecked_Access;
         end;
      else
         declare
            Layer : Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer renames
                      Gtk.Layered.Graph_Paper_Annotation.Add_Graph_Paper_Annotation
                        (Under      => Data.Line.all.Atop,
                         Paper      => Data.Ticks,
                         Location   =>
                           (Orientation => Gtk.Layered.Graph_Paper_Annotation.Horizontal,
                            Alignment   => Gtk.Layered.Graph_Paper_Annotation.Absolute,
                            Left        => -0.5,
                            Right       => -0.5,
                            Y_Position  =>  0.0),
                         Face       => Data.Face,
                         Color      => Data.Color,
                         Height     => Data.Height,
                         Stretch    => Data.Stretch,
                         Text_Angle => Data.Angle,
                         Justify_X  => Data.Justify_X,
                         Justify_Y  => Data.Justify_Y,
                         Background =>
                           Gtk.Widget.Styles.Style_Get
                             (Widget,
                              "time-text-border-color",
                              Gtk.Missed.RGB (1.0, 1.0, 1.0)),
                         Border     =>
                           Gdouble
                             (Guint'
                                  (Gtk.Widget.Styles.Style_Get
                                       (Widget, "time-text-border"))),
                         Overlap    =>
                           Gdouble
                             (Gint'
                                  (Gtk.Widget.Styles.Style_Get
                                       (Widget, "time-text-overlap"))),
                         Opacity    =>
                           Gdouble'
                             (Gtk.Widget.Styles.Style_Get
                                  (Widget,
                                   "time-text-border-opacity"))).all;
         begin
            return Layer'Unchecked_Access;
         end;
      end if;
   end Create_Annotation;

   procedure Delete (List : in out Do_Item_Ptr) is
      Next : Do_Item_Ptr;
   begin
      while List /= null loop
         Next := List.all.Next;
         Free (List);
         List := Next;
      end loop;
   end Delete;

   procedure Delete_Channel (Widget  : not null access Gtk_Oscilloscope_Record;
                             Channel : Channel_Number)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation (Gtk.Layered.Waveform.Waveform_Layer,
                                        Waveform_Layer_Ptr);
      This : Channel_Data;
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      This := Widget.all.Channels (Channel);
      Free (This.Tip_Prefix);
      Free (This.Tip_X_Suffix);
      Free (This.Tip_Y_Suffix);
      for Index in Channel .. Widget.all.Channels_Number - 1 loop
         Widget.all.Channels (Index) := Widget.all.Channels (Index + 1);
      end loop;
      if Channel < Widget.all.Channels_Number then
         Widget.all.Channel_Names.all.Move_After
           (Iter     =>
              Widget.all.Channel_Names.all.Nth_Child
                (Gtk.Tree_Model.Null_Iter,
                 Gint (Channel) - 1),
            Position =>
              Widget.all.Channel_Names.all.Nth_Child
                (Gtk.Tree_Model.Null_Iter,
                 Gint (Widget.all.Channels_Number) - 1));
         Widget.all.Fix_Numbers (Channel, Widget.all.Channels_Number - 1);
      end if;
      Widget.all.Channels (Widget.all.Channels_Number).Tip_Prefix   := null;
      Widget.all.Channels (Widget.all.Channels_Number).Tip_X_Suffix := null;
      Widget.all.Channels (Widget.all.Channels_Number).Tip_Y_Suffix := null;
      Widget.all.Channels_Number := Widget.all.Channels_Number - 1;
      Row :=
        Widget.all.Channel_Names.all.Nth_Child
          (Gtk.Tree_Model.Null_Iter,
           Gint (Widget.all.Channels_Number));
      Widget.all.Channel_Names.all.Remove (Row);
      for Index in Widget.all.Time_Axis'Range loop
         declare
            use type Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
            Data : Time_Axis_Data renames Widget.all.Time_Axis (Index);
         begin
            if
              Data.Sweeper /= null and then
              Data.Sweeper.all'Unchecked_Access = This.Waveform.all.Get_Sweeper
            then
               Data.Channels := Data.Channels - 1;
               exit;
            end if;
         end;
      end loop;
      Free (This.Waveform);
      This.Source.all.Unref;
      Emit (Widget, Signal_IDs (13), Guint (Channel));
   end Delete_Channel;

   procedure Do_Init
     (Widget         : not null access Gtk_Oscilloscope_Record'Class;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Refresh_Engine : access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Refresh_Period : Duration;
      Background     : Gdk.Color.Gdk_Color;
      Buffer_Size    : Positive);
   procedure Do_Init
     (Widget         : not null access Gtk_Oscilloscope_Record'Class;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Refresh_Engine : access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Refresh_Period : Duration;
      Background     : Gdk.Color.Gdk_Color;
      Buffer_Size    : Positive) is separate;

   overriding procedure Do_It
     (Item         : Do_Amplifier_Zoom;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null)
   is
      pragma Unreferenced (Oscilloscope);
   begin
      if Item.Amplifier.all.Get_Auto_Scaling then
         Push_Auto_Amplifier (Item.Amplifier, Inverse, First);
         Item.Amplifier.all.Set_Auto_Scaling (False);
      else
         Push_Amplifier_Zoom (Item.Amplifier, Inverse, First);
      end if;
      Item.Amplifier.all.Set_Page_Size (Item.Page_Size);
      Item.Amplifier.all.Set_Value (Item.Value);
   end Do_It;

   overriding procedure Do_It
     (Item         : Do_Auto_Amplifier;
      First        : in out Boolean;
      Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
      Inverse      : access Items_Stack := null)
   is
      pragma Unreferenced (Oscilloscope);
   begin
      if not Item.Amplifier.all.Get_Auto_Scaling then
         Push_Amplifier_Zoom (Item.Amplifier, Inverse, First);
         Item.Amplifier.all.Set_Auto_Scaling (True);
      end if;
   end Do_It;

   overriding procedure Do_It (Item         : Do_Release_Sweeper;
                               First        : in out Boolean;
                               Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                               Inverse      : access Items_Stack := null)
   is
      pragma Unreferenced (Oscilloscope);
   begin
      if Item.Sweeper.all.Get_Frozen then
         Push_Sweeper_Zoom (Item.Sweeper, Inverse, First);
         Item.Sweeper.all.Set_Frozen (False);
      end if;
   end Do_It;

   overriding procedure Do_It (Item         : Do_Sweeper_Zoom;
                               First        : in out Boolean;
                               Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                               Inverse      : access Items_Stack := null)
   is
      pragma Unreferenced (Oscilloscope);
   begin
      if Item.Sweeper.all.Get_Frozen then
         Push_Sweeper_Zoom (Item.Sweeper, Inverse, First);
      else
         Push_Release_Sweeper (Item.Sweeper, Inverse, First);
         Item.Sweeper.all.Set_Frozen (True);
      end if;
      Item.Sweeper.all.Set_Page_Span (Item.Page);
      Item.Sweeper.all.Set_Time (Item.Time);
   end Do_It;

   overriding procedure Do_It (Item         : Do_Stub;
                               First        : in out Boolean;
                               Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                               Inverse      : access Items_Stack := null)
   is
      pragma Unreferenced (First, Inverse, Oscilloscope);
   begin
      Item.Stack.all.Stubs := Item.Previous;
      --    Push_Stub (Item.Name, Inverse, First); -- Don't move it!
   end Do_It;

   procedure Emit (Widget : not null access Gtk_Oscilloscope_Record'Class;
                   Signal : Signal_Id;
                   Value  : String)
   is
      procedure Set_Object (Value  : in out Glib.Values.GValue;
                            Object : System.Address);
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : Glib.Values.GValue_Array (0 .. 1);
      Result : Glib.Values.GValue;
   begin
      if Class_Record /= Glib.Object.Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Glib.Values.Init (Params (0), This);
            Set_Object (Params (0),
                        Gtk.Widget.Convert (Widget.all'Unchecked_Access));
            Glib.Values.Init (Params (1), GType_String);
            Glib.Values.Set_String (Params (1), Value);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Glib.Values.Unset (Params (0));
            Glib.Values.Unset (Params (1));
         end;
      end if;
   end Emit;

   procedure Emit (Widget  : not null access Gtk_Oscilloscope_Record'Class;
                   Signal  : Signal_Id;
                   Value_1 : Guint;
                   Value_2 : Gdouble;
                   Value_3 : Gdouble)
   is
      procedure Set_Object (Value  : in out Glib.Values.GValue;
                            Object : System.Address);
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : Glib.Values.GValue_Array (0 .. 3);
      Result : Glib.Values.GValue;
   begin
      if Class_Record /= Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Glib.Values.Init (Params (0), This);
            Set_Object (Params (0),
                        Gtk.Widget.Convert (Widget.all'Unchecked_Access));
            Glib.Values.Init (Params (1), GType_Uint);
            Glib.Values.Set_Uint (Params (1), Value_1);
            Glib.Values.Init (Params (2), GType_Double);
            Glib.Values.Set_Double (Params (2), Value_2);
            Glib.Values.Init (Params (3), GType_Double);
            Glib.Values.Set_Double (Params (3), Value_3);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Glib.Values.Unset (Params (0));
            Glib.Values.Unset (Params (1));
            Glib.Values.Unset (Params (2));
            Glib.Values.Unset (Params (3));
         end;
      end if;
   end Emit;

   procedure Emit (Widget : not null access Gtk_Oscilloscope_Record'Class;
                   Signal : Signal_Id;
                   Value  : Guint)
   is
      procedure Set_Object (Value  : in out Glib.Values.GValue;
                            Object : System.Address);
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : Glib.Values.GValue_Array (0 .. 1);
      Result : Glib.Values.GValue;
   begin
      if Class_Record /= Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Glib.Values.Init (Params (0), This);
            Set_Object (Params (0),
                        Gtk.Widget.Convert (Widget.all'Unchecked_Access));
            Glib.Values.Init (Params (1), GType_Uint);
            Glib.Values.Set_Uint (Params (1), Value);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Glib.Values.Unset (Params (0));
            Glib.Values.Unset (Params (1));
         end;
      end if;
   end Emit;

   procedure Erase_Redo_Stack
     (Widget : not null access Gtk_Oscilloscope_Record) is
   begin
      Delete (Widget.all.Redo_Stack.Actions);
      Widget.all.Redo_Stack.Stubs := null;
   end Erase_Redo_Stack;

   procedure Erase_Undo_Stack
     (Widget : not null access Gtk_Oscilloscope_Record) is
   begin
      Delete (Widget.all.Undo_Stack.Actions);
      Widget.all.Undo_Stack.Stubs := null;
   end Erase_Undo_Stack;

   procedure Feed (Widget  : not null access Gtk_Oscilloscope_Record;
                   Channel : Channel_Number;
                   T       : Ada.Real_Time.Time;
                   V       : Gdouble) is
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.all.Channels (Channel).Source.all.Put
        (T => Gtk.Layered.Waveform.X_Axis (Gtk.Layered.Waveform.To_Double (T)),
         V => Gtk.Layered.Waveform.Y_Axis (V));
   end Feed;

   procedure Feed (Widget  : not null access Gtk_Oscilloscope_Record;
                   Channel : Channel_Number;
                   T       : Ada.Calendar.Time;
                   V       : Gdouble) is
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.all.Channels (Channel).Source.all.Put
        (T => Gtk.Layered.Waveform.X_Axis (Gtk.Layered.Waveform.To_Double (T)),
         V => Gtk.Layered.Waveform.Y_Axis (V));
   end Feed;

   procedure Feed (Widget  : not null access Gtk_Oscilloscope_Record;
                   Channel : Channel_Number;
                   V       : Gdouble) is
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.all.Channels (Channel).Source.all.Put
        (T =>
           Gtk.Layered.Waveform.X_Axis
             (Gtk.Layered.Waveform.To_Double (Ada.Real_Time.Clock)),
         V => Gtk.Layered.Waveform.Y_Axis (V));
   end Feed;

   procedure Feed (Widget  : not null access Gtk_Oscilloscope_Record;
                   Channel : Channel_Number;
                   T       : Gdouble;
                   V       : Gdouble) is
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.all.Channels (Channel).Source.all.Put
        (T => Gtk.Layered.Waveform.X_Axis (T),
         V => Gtk.Layered.Waveform.Y_Axis (V));
   end Feed;

   overriding procedure Finalize (Item : in out Do_Auto_Amplifier) is
   begin
      Item.Amplifier.all.Unref;
   end Finalize;

   overriding procedure Finalize (Item : in out Do_Release_Sweeper) is
   begin
      Item.Sweeper.all.Unref;
   end Finalize;

   procedure Fix_Numbers (Widget : not null access Gtk_Oscilloscope_Record;
                          Start  : Channel_Number;
                          Stop   : Channel_Number)
   is
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter;
      From : constant Gint := Gint (Start) - 1;
      To   : constant Gint := Gint (Stop)  - 1;
   begin
      Row := Widget.all.Channel_Names.all.Nth_Child (Gtk.Tree_Model.Null_Iter,
                                                     From);
      for Index in From .. To loop
         Widget.all.Channel_Names.all.Set (Row, 1, Index + 1);
         Widget.all.Channel_Names.all.Next (Row);
      end loop;
   end Fix_Numbers;

   function Get_Amplifier
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
      return not null access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class
   is
      Group : constant Group_Count :=
                Widget.all.Values_Axis (Amplifier).Group;
   begin
      if Group > 0 then
         return Widget.all.Groups.all (Group).Amplifier;
      else
         raise Constraint_Error with
           "No group is assigned to the axis";
      end if;
   end Get_Amplifier;

   function Get_Amplifier
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number)
      return not null access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class is
   begin
      if Channel <= Widget.all.Channels_Number then
         return
           Widget.all.Groups.all (Widget.all.Channels (Channel).Group).Amplifier;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Amplifier;

   function Get_Amplifier
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number)
      return not null access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class is
   begin
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      else
         return Widget.all.Groups.all (Group).Amplifier;
      end if;
   end Get_Amplifier;

   function Get_Annotation_Height
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gint
   is
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if Data.On then
         return Gint (Data.Width);
      else
         return 0;
      end if;
   end Get_Annotation_Height;

   function Get_Annotation_Width
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gint
   is
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if Data.On then
         return Gint (Data.Width);
      else
         return 0;
      end if;
   end Get_Annotation_Width;

   function Get_Auto_Scaling
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean is
   begin
      if Widget.all.Values_Axis (Amplifier).Group = 0 then
         raise Constraint_Error with
           "No group assigned to the amplifier";
      else
         return
           Widget.all.Get_Auto_Scaling
             (Widget.all.Values_Axis (Amplifier).Group);
      end if;
   end Get_Auto_Scaling;

   function Get_Auto_Scaling
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number)  return Boolean is
   begin
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      else
         return Widget.all.Groups.all (Group).Amplifier.all.Get_Auto_Scaling;
      end if;
   end Get_Auto_Scaling;

   function Get_Box (Widget : not null access constant Gtk_Oscilloscope_Record)
                     return Cairo.Ellipses.Cairo_Box
   is
      Width  : constant Gdouble :=
                 Gdouble (Widget.all.Layers.all.Get_Allocated_Width);
      Height : constant Gdouble :=
                 Gdouble (Widget.all.Layers.all.Get_Allocated_Height);
      X1     : Gdouble := Widget.all.Values_Axis (Left).Offset;
      X2     : Gdouble := Width - Widget.all.Values_Axis (Right).Offset;
      Y1     : Gdouble := Widget.all.Time_Axis (Upper).Offset;
      Y2     : Gdouble := Height - Widget.all.Time_Axis (Lower).Offset;
   begin
      if X1 > Width then
         X1 := Width;
         X2 := X1;
      elsif X2 < 0.0 then
         X2 := 0.0;
      end if;
      if Y1 > Height then
         Y1 := Height;
         Y2 := Y1;
      elsif Y2 < 0.0 then
         Y2 := 0.0;
      end if;
      return (X1 => X1, X2 => X2, Y1 => Y1, Y2 => Y2);
   end Get_Box;

   function Get_Buffer
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number)
      return not null access Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer_Record'Class is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Source.all'Unchecked_Access;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Buffer;

   function Get_Channel_List
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Gtk.List_Store.Gtk_List_Store is
   begin
      return Widget.all.Channel_Names;
   end Get_Channel_List;

   function Get_Channels_Number
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Channel_Count is
   begin
      return Widget.all.Channels_Number;
   end Get_Channels_Number;

   function Get_Color
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number)  return Gdk.Color.Gdk_Color is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Waveform.all.Get_Line.Color;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Color;

   function Get_Default_Face
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Pango.Cairo.Fonts.Pango_Cairo_Font is
   begin
      return Widget.all.Default_Face;
   end Get_Default_Face;

   function Get_Enabled_Dropdown_Items
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Dropdown_Items is
   begin
      return Widget.all.Menu_Enabled;
   end Get_Enabled_Dropdown_Items;

   function Get_From
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type := Lower) return Ada.Real_Time.Time is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_From;
   end Get_From;

   function Get_From
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type := Lower) return Ada.Calendar.Time is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_From;
   end Get_From;

   function Get_Frozen
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type := Lower) return Boolean is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_Frozen;
   end Get_Frozen;

   procedure Get_Grid_Colors
     (Widget      : not null access constant Gtk_Oscilloscope_Record;
      Major_Color : out Gdk.Color.Gdk_Color;
      Minor_Color : out Gdk.Color.Gdk_Color) is
   begin
      Major_Color := Widget.all.Major_Color;
      Minor_Color := Widget.all.Minor_Color;
   end Get_Grid_Colors;

   function Get_Group
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Group_Number
   is
      Result : constant Group_Count :=
                 Widget.all.Values_Axis (Amplifier).Group;
   begin
      if Result > 0 then
         return Result;
      else
         raise Constraint_Error with "No group assigned";
      end if;
   end Get_Group;

   function Get_Group
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Group_Number is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Group;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Group;

   function Get_Group_List
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Gtk.List_Store.Gtk_List_Store is
   begin
      return Widget.all.Group_Names;
   end Get_Group_List;

   function Get_Groups_Number
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Group_Count is
   begin
      return Widget.all.Groups_Number;
   end Get_Groups_Number;

   function Get_Interpolation_Mode
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Gtk.Layered.Interpolation_Mode is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Waveform.all.
           Get_Interpolation_Mode;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Interpolation_Mode;

   function Get_Left_Extrapolation_Mode
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Boolean is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Waveform.all.
           Get_Left_Extrapolation_Mode;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Left_Extrapolation_Mode;

   function Get_Manual_Sweep
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean
   is
   begin
      return Widget.all.Manual_Sweep;
   end Get_Manual_Sweep;

   function Get_Name
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return String is
   begin
      if Channel <= Widget.all.Channels_Number then
         return
           Widget.all.Channel_Names.all.Get_String
             (Widget.all.Channel_Names.all.Nth_Child
                (Gtk.Tree_Model.Null_Iter,
                 Gint (Channel) - 1),
              0);
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Name;

   function Get_Name
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number) return String is
   begin
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      else
         return
           Widget.all.Group_Names.all.Get_String
             (Widget.all.Group_Names.all.Nth_Child
                (Gtk.Tree_Model.Null_Iter,
                 Gint (Group) - 1),
              0);
      end if;
   end Get_Name;

   function Get_Offset
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Duration is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_Offset;
   end Get_Offset;

   function Get_Page_Span
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Duration is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_Page_Span;
   end Get_Page_Span;

   function Get_Redo_Stub
     (Widget : not null access Gtk_Oscilloscope_Record;
      Depth  : Positive := 1) return UTF8_String is
   begin
      return Get_Stub (Widget.all.Redo_Stack, Depth);
   end Get_Redo_Stub;

   function Get_Release_To_Latest
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean
   is
   begin
      return Widget.all.Jump_On_Thaw;
   end Get_Release_To_Latest;

   function Get_Right_Extrapolation_Mode
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Boolean is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Waveform.all.
           Get_Right_Extrapolation_Mode;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Right_Extrapolation_Mode;

   function Get_Selection_Mode
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Selection_Action is
   begin
      return Widget.all.Selection_Mode;
   end Get_Selection_Mode;

   function Get_Snapshot_File
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return UTF8_String is
   begin
      if Widget.all.Format = No_Snapshot or else Widget.all.File = null then
         return "";
      else
         return Widget.all.File.all;
      end if;
   end Get_Snapshot_File;

   function Get_Snapshot_Format
     (Widget : not null access constant Gtk_Oscilloscope_Record)
      return Snapshot_Format is
   begin
      return Widget.all.Format;
   end Get_Snapshot_Format;

   function Get_Stub (Stack : Items_Stack;
                      Depth : Positive) return UTF8_String
   is
      Count : Natural := Depth;
      This  : Do_Item_Ptr := Stack.Stubs;
   begin
      while This /= null loop
         if This.all in Do_Stub'Class then
            if Count = 1 then
               return Do_Stub'Class (This.all).Name;
            end if;
            Count := Count - 1;
         end if;
         This := This.all.Next;
      end loop;
      raise Ada.IO_Exceptions.End_Error with "No such stub";
   end Get_Stub;

   function Get_Superscript
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean
   is
   begin
      return Widget.all.Superscript;
   end Get_Superscript;

   function Get_Sweeper
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type)
      return not null access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all'Unchecked_Access;
   end Get_Sweeper;

   function Get_Sweeper
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Sweeper_Type
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Channel <= Widget.all.Channels_Number then
         for Index in Widget.all.Time_Axis'Range loop
            if Widget.all.Channels (Channel).Waveform.all.Get_Sweeper =
              Gtk.Adjustment.Gtk_Adjustment_Record'Class
                (Widget.all.Time_Axis (Index).Sweeper.all)'Access
            then
               return Index;
            end if;
         end loop;
      end if;
      raise Constraint_Error with
        "Wrong channel number" & Channel_Count'Image (Channel);
   end Get_Sweeper;

   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Real_Time.Time is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_Time;
   end Get_Time;

   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      X       : Gint) return Ada.Real_Time.Time
   is
      Adjustment : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
                     Widget.all.Time_Axis (Sweeper).Sweeper;
      Box        : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
      Lower      : constant Gdouble := Gtk.Layered.Waveform.Sweeper.Get_Value (Adjustment);
      Page       : constant Gdouble := Gtk.Layered.Waveform.Sweeper.Get_Page_Size (Adjustment);
   begin
      if Box.X1 >= Gdouble (X) then
         return Gtk.Layered.Waveform.To_Time (Lower);
      elsif Box.X2 <= Gdouble (X) then
         return Gtk.Layered.Waveform.To_Time (Lower + Page);
      else
         return
           Gtk.Layered.Waveform.To_Time (Lower + Page * (Gdouble (X) - Box.X1) / (Box.X2 - Box.X1));
      end if;
   end Get_Time;

   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Calendar.Time is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_Time;
   end Get_Time;

   function Get_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      X       : Gint) return Ada.Calendar.Time
   is
      Adjustment : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
                     Widget.all.Time_Axis (Sweeper).Sweeper;
      Box        : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
      Lower      : constant Gdouble := Gtk.Layered.Waveform.Sweeper.Get_Value (Adjustment);
      Page       : constant Gdouble := Gtk.Layered.Waveform.Sweeper.Get_Page_Size (Adjustment);
   begin
      if Box.X1 >= Gdouble (X) then
         return Gtk.Layered.Waveform.To_Time (Lower);
      elsif Box.X2 <= Gdouble (X) then
         return Gtk.Layered.Waveform.To_Time (Lower + Page);
      else
         return
           Gtk.Layered.Waveform.To_Time
             (Lower + Page * (Gdouble (X) - Box.X1) / (Box.X2 - Box.X1));
      end if;
   end Get_Time;

   function Get_Time_Axis
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean is
   begin
      return Widget.all.Time_Axis (Sweeper).On;
   end Get_Time_Axis;

   function Get_Time_Axis_Annotation
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type)
      return access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class
   is
   begin
      return Widget.all.Time_Axis (Sweeper).Texts;
   end Get_Time_Axis_Annotation;

   function Get_Time_Axis_As_Time
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean is
   begin
      return Widget.all.Time_Axis (Sweeper).Time_Mode;
   end Get_Time_Axis_As_Time;

   function Get_Time_Axis_Height
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Natural is
   begin
      return Natural (Widget.all.Time_Axis (Sweeper).Width);
   end Get_Time_Axis_Height;

   function Get_Time_Grid
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean is
   begin
      return Widget.all.Time_Axis (Sweeper).Grid;
   end Get_Time_Grid;

   function Get_Time_Scale
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Boolean is
   begin
      return not Widget.all.Time_Axis (Sweeper).No_Scale;
   end Get_Time_Scale;

   function Get_Time_Text_Angle
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdouble is
   begin
      return Widget.all.Time_Axis (Sweeper).Angle;
   end Get_Time_Text_Angle;

   function Get_Time_Text_Color
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdk.Color.Gdk_Color is
   begin
      return Widget.all.Time_Axis (Sweeper).Color;
   end Get_Time_Text_Color;

   function Get_Time_Text_Face
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Pango.Cairo.Fonts.Pango_Cairo_Font is
   begin
      return Widget.all.Time_Axis (Sweeper).Face;
   end Get_Time_Text_Face;

   function Get_Time_Text_Height
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdouble is
   begin
      return Widget.all.Time_Axis (Sweeper).Height;
   end Get_Time_Text_Height;

   function Get_Time_Text_Horizontal_Alignment
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Strings.Alignment is
   begin
      return Widget.all.Time_Axis (Sweeper).Justify_X;
   end Get_Time_Text_Horizontal_Alignment;

   function Get_Time_Text_Stretch
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gdouble is
   begin
      return Widget.all.Time_Axis (Sweeper).Stretch;
   end Get_Time_Text_Stretch;

   function Get_Time_Text_Vertical_Alignment
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Gtk.Layered.Vertical_Alignment is
   begin
      return Widget.all.Time_Axis (Sweeper).Justify_Y;
   end Get_Time_Text_Vertical_Alignment;

   function Get_Time_Tooltip
     (Widget : not null access constant Gtk_Oscilloscope_Record) return Boolean
   is
   begin
      return Widget.all.Show_Time;
   end Get_Time_Tooltip;

   function Get_Time_Tooltip_Suffix
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return UTF8_String is
   begin
      if Channel <= Widget.all.Channels_Number then
         return +Widget.all.Channels (Channel).Tip_X_Suffix;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Time_Tooltip_Suffix;

   function Get_To
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Real_Time.Time is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_To;
   end Get_To;

   function Get_To
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type) return Ada.Calendar.Time is
   begin
      return Widget.all.Time_Axis (Sweeper).Sweeper.all.Get_To;
   end Get_To;

   function Get_Tooltip_Annotation
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return UTF8_String is
   begin
      if Channel <= Widget.all.Channels_Number then
         return +Widget.all.Channels (Channel).Tip_Prefix;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Tooltip_Annotation;

   function Get_Type return GType is separate;

   function Get_Undo_Stub
     (Widget : not null access Gtk_Oscilloscope_Record;
      Depth  : Positive := 1) return UTF8_String is
   begin
      return Get_Stub (Widget.all.Undo_Stack, Depth);
   end Get_Undo_Stub;

   function Get_Value
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      Y      : Gint) return Gdouble is
   begin
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      else
         declare
            Adjustment : constant Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier :=
                           Widget.all.Groups.all (Group).Amplifier;
            Box        : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
            Value      : constant Gdouble := Gtk.Layered.Waveform.Amplifier.Get_Value (Adjustment);
            Page       : constant Gdouble := Gtk.Layered.Waveform.Amplifier.Get_Page_Size (Adjustment);
         begin
            if Box.Y1 >= Gdouble (Y) then
               return Value + Page;
            elsif Box.Y2 <= Gdouble (Y) then
               return Value;
            else
               return
                 (Value + Page * (Box.Y2 - Gdouble (Y)) / (Box.Y2 - Box.Y1));
            end if;
         end;
      end if;
   end Get_Value;

   function Get_Value
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Y         : Gint) return Gdouble
   is
      Group : constant Group_Count :=
                Widget.all.Values_Axis (Amplifier).Group;
   begin
      if Group > 0 then
         return Get_Value (Widget, Group, Y);
      else
         raise Constraint_Error with "No group assigned";
      end if;
   end Get_Value;

   function Get_Values_Axis
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean is
   begin
      return
        (Widget.all.Values_Axis (Amplifier).Group > 0 and then
         Widget.all.Values_Axis (Amplifier).On);
   end Get_Values_Axis;

   function Get_Values_Axis_Annotation
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
      return access Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class is
   begin
      return Widget.all.Values_Axis (Amplifier).Texts;
   end Get_Values_Axis_Annotation;

   function Get_Values_Axis_Width
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Natural is
   begin
      return Natural (Widget.all.Values_Axis (Amplifier).Width);
   end Get_Values_Axis_Width;

   function Get_Values_Grid
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean is
   begin
      return
        (Widget.all.Values_Axis (Amplifier).Group > 0 and then
         Widget.all.Values_Axis (Amplifier).Grid);
   end Get_Values_Grid;

   function Get_Values_Horizontal_Alignment
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Ada.Strings.Alignment is
   begin
      return Widget.all.Values_Axis (Amplifier).Justify_X;
   end Get_Values_Horizontal_Alignment;

   function Get_Values_Scale
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean is
   begin
      return not Widget.all.Values_Axis (Amplifier).No_Scale;
   end Get_Values_Scale;

   function Get_Values_Text_Angle
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdouble is
   begin
      return Widget.all.Values_Axis (Amplifier).Angle;
   end Get_Values_Text_Angle;

   function Get_Values_Text_Color
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdk.Color.Gdk_Color is
   begin
      return Widget.all.Values_Axis (Amplifier).Color;
   end Get_Values_Text_Color;

   function Get_Values_Text_Face
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Pango.Cairo.Fonts.Pango_Cairo_Font is
   begin
      return Widget.all.Values_Axis (Amplifier).Face;
   end Get_Values_Text_Face;

   function Get_Values_Text_Height
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdouble is
   begin
      return Widget.all.Values_Axis (Amplifier).Height;
   end Get_Values_Text_Height;

   function Get_Values_Text_Stretch
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Gdouble is
   begin
      return Widget.all.Values_Axis (Amplifier).Stretch;
   end Get_Values_Text_Stretch;

   function Get_Values_Tooltip_Suffix
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return UTF8_String is
   begin
      if Channel <= Widget.all.Channels_Number then
         return +Widget.all.Channels (Channel).Tip_Y_Suffix;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Values_Tooltip_Suffix;

   function Get_Waveform
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number)
      return not null access Gtk.Layered.Waveform.Waveform_Layer is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Waveform;
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Waveform;

   function Get_X
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Gdouble;
      Crop    : Boolean) return Gint
   is
      Adjustment : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
                     Widget.all.Time_Axis (Sweeper).Sweeper;
      Box        : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
      Lower      : constant Gdouble := Gtk.Layered.Waveform.Sweeper.Get_Value (Adjustment);
      Page       : constant Gdouble := Gtk.Layered.Waveform.Sweeper.Get_Page_Size (Adjustment);
   begin
      if Stamp > Lower + Page then
         if Crop then
            return Gint (Box.X2);
         else
            raise Ada.IO_Exceptions.Layout_Error with
              "Time right of the waveform box";
         end if;
      elsif Stamp < Lower then
         if Crop then
            return Gint (Box.X1);
         else
            raise Ada.IO_Exceptions.Layout_Error with
              "Time left of the waveform box";
         end if;
      else
         return Gint (Box.X1 + (Stamp - Lower) * (Box.X2 - Box.X1) / Page);
      end if;
   end Get_X;

   function Get_X
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Real_Time.Time;
      Crop    : Boolean := False) return Gint is
   begin
      return Get_X (Widget,
                    Sweeper,
                    Gtk.Layered.Waveform.To_Double (Stamp),
                    Crop);
   end Get_X;

   function Get_X
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Calendar.Time;
      Crop    : Boolean := False) return Gint is
   begin
      return Get_X (Widget,
                    Sweeper,
                    Gtk.Layered.Waveform.To_Double (Stamp),
                    Crop);
   end Get_X;

   function Get_Y
     (Widget : not null access constant Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      Value  : Gdouble;
      Crop   : Boolean := False) return Gint is
   begin
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      else
         declare
            Adjustment : constant Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier :=
                           Widget.all.Groups.all (Group).Amplifier;
            Box        : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
            Lower      : constant Gdouble := Gtk.Layered.Waveform.Amplifier.Get_Value (Adjustment);
            Page       : constant Gdouble := Gtk.Layered.Waveform.Amplifier.Get_Page_Size (Adjustment);
         begin
            if Value < Lower then
               if Crop then
                  return Gint (Box.Y2);
               else
                  raise Ada.IO_Exceptions.Layout_Error with
                    "Value below the waveform box";
               end if;
            elsif Value > Lower + Page then
               if Crop then
                  return Gint (Box.Y1);
               else
                  raise Ada.IO_Exceptions.Layout_Error with
                    "Value above the waveform box";
               end if;
            else
               return
                 Gint (Box.Y2 - (Value - Lower) * (Box.Y2 - Box.Y1) / Page);
            end if;
         end;
      end if;
   end Get_Y;

   function Get_Y
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Value     : Gdouble;
      Crop      : Boolean := False) return Gint
   is
      Group : constant Group_Count :=
                Widget.all.Values_Axis (Amplifier).Group;
   begin
      if Group > 0 then
         return Get_Y (Widget, Group, Value, Crop);
      else
         raise Constraint_Error with "No group assigned";
      end if;
   end Get_Y;

   procedure Gtk_New
     (Widget         : out Gtk_Oscilloscope;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Refresh_Engine : not null access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Background     : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Buffer_Size    : Positive  := 1024 * 60;
      Max_Channels   : Channel_Number := 64) is
   begin
      Widget := new Gtk_Oscilloscope_Record (Max_Channels);
      Do_Init
        (Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => Refresh_Engine,
         Refresh_Period => 1.0,
         Buffer_Size    => Buffer_Size,
         Background     => Background);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget         : out Gtk_Oscilloscope;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class := null;
      Refresh_Period : Duration  := 0.02;
      Background     : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
      Buffer_Size    : Positive  := 1024 * 60;
      Max_Channels   : Channel_Number := 64) is
   begin
      Widget := new Gtk_Oscilloscope_Record (Max_Channels);
      Do_Init
        (Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => null,
         Refresh_Period => Refresh_Period,
         Buffer_Size    => Buffer_Size,
         Background     => Background);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   function Has_Group
     (Widget    : not null access constant Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type) return Boolean is
   begin
      return Widget.all.Values_Axis (Amplifier).Group > 0;
   end Has_Group;

   procedure Initialize
     (Widget         : not null access Gtk_Oscilloscope_Record'Class;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Refresh_Engine : not null access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Background     : Gdk.Color.Gdk_Color;
      Buffer_Size    : Positive) is
   begin
      Do_Init
        (Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => Refresh_Engine,
         Refresh_Period => 1.0,
         Buffer_Size    => Buffer_Size,
         Background     => Background);
   end Initialize;

   procedure Initialize
     (Widget         : not null access Gtk_Oscilloscope_Record'Class;
      Lower_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Upper_Sweeper  : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Refresh_Period : Duration := 0.2;
      Background     : Gdk.Color.Gdk_Color;
      Buffer_Size    : Positive) is
   begin
      Do_Init
        (Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => null,
         Refresh_Period => Refresh_Period,
         Buffer_Size    => Buffer_Size,
         Background     => Background);
   end Initialize;

   function Is_Visible
     (Widget  : not null access constant Gtk_Oscilloscope_Record;
      Channel : Channel_Number) return Boolean is
   begin
      if Channel <= Widget.all.Channels_Number then
         return Widget.all.Channels (Channel).Waveform.all.Is_Visible;
      else
         return False;
      end if;
   end Is_Visible;

   function Mouse_Event
     (Oscilloscope : not null access Gtk_Oscilloscope_Record;
      Event        : Gdk.Event.Gdk_Event;
      Hint         : Boolean) return Cairo.Ellipses.Cairo_Tuple
   is
      pragma Unreferenced (Oscilloscope);
   begin
      return Result : Cairo.Ellipses.Cairo_Tuple do
         if Hint then
            --              declare
            --                 use Gdk.Device_Manager;
            --                 Mask   : Gdk.Types.Gdk_Modifier_Type;
            --                 Area   : Gdk_Rectangle;
            --                 Window : Gdk.Gdk_Window := Oscilloscope.Get_Window;
            --              begin
            --                 Gdk.Window.Get_Device_Position
            --                 (  Window,
            --                    Get_Device_Manager
            --                    (  Get_Display (Window)
            --                    ) .Get_Client_Pointer,
            --                    GInt (Result.X),
            --                    GInt (Result.Y),
            --                    Mask,
            --                    Window
            --                 );
            --                 Oscilloscope.Get_Allocation (Area);
            --                 Result.X := Result.X - GDouble (Area.X);
            --                 Result.Y := Result.Y - GDouble (Area.Y);
            --              end;
            Gdk.Event.Get_Coords (Event, Result.X, Result.Y);
         else
            Gdk.Event.Get_Axis (Event, Gdk.Types.Axis_X, Result.X);
            Gdk.Event.Get_Axis (Event, Gdk.Types.Axis_Y, Result.Y);
         end if;
      end return;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Mouse_Event"));
         return (0.0, 0.0);
   end Mouse_Event;

   --     procedure Move (From, To : in out Do_Item_Ptr) is
   --        This : Do_Item_Ptr := From;
   --        List : Do_Item_Ptr;
   --     begin
   --        while This /= null loop
   --           From := This.Next;
   --           This.Next := List;
   --           List := This;
   --           exit when This.First;
   --           This := From;
   --        end loop;
   --        List := This;
   --        while This /= null loop
   --           List := This.Next;
   --           This.Next := To;
   --           To := This;
   --           This := List;
   --        end loop;
   --     end Move;

   procedure Move_Channel
     (Widget     : not null access Gtk_Oscilloscope_Record;
      Old_Number : Channel_Number;
      New_Number : Channel_Number)
   is
      This : Channel_Data;
   begin
      if Old_Number > Widget.all.Channels_Number then
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Old_Number);
      elsif New_Number > Widget.all.Channels_Number then
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (New_Number);
      end if;
      if Old_Number /= New_Number then
         This := Widget.all.Channels (Old_Number);
         if Old_Number < New_Number then
            for Index in Old_Number .. New_Number - 1 loop
               Widget.all.Channels (Index) := Widget.all.Channels (Index + 1);
            end loop;
         else
            for Index in reverse New_Number + 1 .. Old_Number loop
               Widget.all.Channels (Index) := Widget.all.Channels (Index - 1);
            end loop;
         end if;
         Widget.all.Channels (New_Number) := This;
         if New_Number > 1 then
            Widget.all.Channel_Names.all.Move_After
              (Iter     =>
                 Widget.all.Channel_Names.all.Nth_Child
                   (Gtk.Tree_Model.Null_Iter,
                    Gint (Old_Number) - 1),
               Position =>
                 Widget.all.Channel_Names.all.Nth_Child
                   (Gtk.Tree_Model.Null_Iter,
                    Gint (New_Number) - 2));
         else
            Widget.all.Channel_Names.all.Move_Before
              (Iter     =>
                 Widget.all.Channel_Names.all.Nth_Child
                   (Gtk.Tree_Model.Null_Iter,
                    Gint (Old_Number) - 1),
               Position =>
                 Widget.all.Channel_Names.all.Nth_Child
                   (Gtk.Tree_Model.Null_Iter,
                    Gint (New_Number) - 1));
         end if;
         if Old_Number < New_Number then
            Widget.all.Fix_Numbers (Old_Number, New_Number);
         else
            Widget.all.Fix_Numbers (New_Number, Old_Number);
         end if;
      end if;
   end Move_Channel;

   procedure On_Autoscaling_Changed
     (Amplifier    : access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class;
      Oscilloscope : Gtk_Oscilloscope) is
   begin
      for Index in Oscilloscope.all.Values_Axis'Range loop
         declare
            This  : Values_Axis_Data renames
                      Oscilloscope.all.Values_Axis (Index);
            Group : constant Group_Count := This.Group;
         begin
            if
              Group > 0 and then
              Amplifier = Oscilloscope.all.Groups.all (Group).Amplifier
            then
               Oscilloscope.all.Update_Amplifier (Index);
               Emit (Oscilloscope,
                     Signal_IDs (0),
                     Guint (Amplifier_Type'Pos (Index)));
            end if;
         end;
      end loop;
   end On_Autoscaling_Changed;

   function On_Button_Press
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean is separate;

   function On_Button_Release
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean is separate;

   procedure On_Cancel_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
   begin
      Oscilloscope.all.Restore_State;
      Free (Oscilloscope.all.Selection.all.Area);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " &  Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Cancel_Selection"));
   end On_Cancel_Selection;

   procedure On_Copy_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Y      : constant Gdouble := Oscilloscope.all.Selection.all.Area.all.Get_Box.Y2;
      Got_It : Boolean;
   begin
      for Index in 1 .. Oscilloscope.all.Channels_Number loop
         declare
            Data : Channel_Data renames Oscilloscope.all.Channels (Index);
         begin
            Data.Waveform.all.Get (Y, Data.Value_1, Got_It);
            if Got_It then
               Data.Status := Absolute;
            else
               Data.Status := Undefined;
            end if;
         end;
      end loop;
      Oscilloscope.all.Update_Value;
      Oscilloscope.all.Restore_State;
      Free (Oscilloscope.all.Selection.all.Area);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Copy_Selection"));
   end On_Copy_Selection;

   procedure On_Destroy
     (Object       : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      use type Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      use type Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer;
      use type Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
      pragma Unreferenced (Object);
   begin
      if Oscilloscope.all.Refresh_Engine /= null then
         Oscilloscope.all.Refresh_Engine.all.Delete (Oscilloscope.all.Layers);
         Free (Oscilloscope.all.Refresh_Engine);
      end if;
      Oscilloscope.all.Remove (Oscilloscope.all.Layers);
      Oscilloscope.all.Channel_Names.all.Unref;
      Oscilloscope.all.Group_Names.all.Unref;
      for Index in Oscilloscope.all.Groups'Range loop
         if Oscilloscope.all.Groups.all (Index).Amplifier /= null then
            Oscilloscope.all.Groups.all (Index).Amplifier.all.Unref;
         end if;
      end loop;
      for Index in 1 .. Oscilloscope.all.Channels_Number loop
         if Oscilloscope.all.Channels (Index).Source /= null then
            Oscilloscope.all.Channels (Index).Source.all.Unref;
            Oscilloscope.all.Channels (Index).Source := null;
         end if;
         Free (Oscilloscope.all.Channels (Index).Tip_Prefix);
         Free (Oscilloscope.all.Channels (Index).Tip_X_Suffix);
         Free (Oscilloscope.all.Channels (Index).Tip_Y_Suffix);
      end loop;
      for Index in Oscilloscope.all.Time_Axis'Range loop
         if Oscilloscope.all.Time_Axis (Index).Sweeper /= null then
            Oscilloscope.all.Time_Axis (Index).Sweeper.all.Unref;
            Oscilloscope.all.Time_Axis (Index).Sweeper := null;
         end if;
      end loop;
      if Oscilloscope.all.Refresh_Period /= null then
         Oscilloscope.all.Refresh_Period.all.Unref;
         Oscilloscope.all.Refresh_Period := null;
      end if;
      if Oscilloscope.all.Drawing_Time /= null then
         Oscilloscope.all.Drawing_Time.all.Unref;
         Oscilloscope.all.Drawing_Time := null;
      end if;
      Free (Oscilloscope.all.Groups);
      Free (Oscilloscope.all.Selection);
      Oscilloscope.all.Erase_Undo_Stack;
      Oscilloscope.all.Erase_Redo_Stack;
      Free (Oscilloscope.all.File);
   end On_Destroy;

   procedure On_Difference_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Y : constant Gdouble := Oscilloscope.all.Selection.all.Area.all.Get_Box.Y2;
   begin
      for Index in 1 .. Oscilloscope.all.Channels_Number loop
         declare
            Data   : Channel_Data renames Oscilloscope.all.Channels (Index);
            Got_It : Boolean;
         begin
            if Data.Status /= Undefined then
               Data.Waveform.all.Get (Y, Data.Value_2, Got_It);
               if Got_It then
                  Data.Status  := Difference;
               else
                  if Data.Status = Difference then
                     Data.Status := Undefined;
                  end if;
               end if;
            end if;
         end;
      end loop;
      Oscilloscope.all.Update_Value;
      Oscilloscope.all.Restore_State;
      Free (Oscilloscope.all.Selection.all.Area);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Difference_Selection"));
   end On_Difference_Selection;

   procedure On_Format_Time
     (Scale     : not null access Gtk.Scale.Gtk_Scale_Record'Class;
      Arguments : Glib.Values.GValue_Array;
      Result    : in out Glib.Values.GValue;
      Data      : Time_Axis_Data_Ptr)
   is
      pragma Unreferenced (Arguments, Scale);
      use type Ada.Calendar.Time;
      Page  : constant Duration := Data.all.Sweeper.all.Get_Page_Span / 2;
      Right : constant Ada.Calendar.Time := Data.all.Sweeper.all.Get_Time;
   begin
      if Data.all.Time_Mode then
         Glib.Values.Set_String
           (Result,
            (Gtk.Layered.Graph_Paper_Annotation.Image (Right - Page)
             &  " "
             &  Strings_Edit.UTF8.Image (16#00B1#)
             &  " "
             &  Gtk.Layered.Graph_Paper_Annotation.Image (Page)
             &  "s"));
      else
         if Data.all.Texts /= null then
            Glib.Values.Set_String
              (Result,
               Data.all.Texts.all.Image
                 (Gtk.Layered.Waveform.To_Double (Right - Page)));
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Format_Time"));
   end On_Format_Time;

   procedure On_Freezing_Changed
     (Sweeper      : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      use type Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
   begin
      for Index in Oscilloscope.all.Time_Axis'Range loop
         if Oscilloscope.all.Time_Axis (Index).Sweeper = Sweeper then
            Emit
              (Oscilloscope,
               Signal_IDs (2),
               Guint (Sweeper_Type'Pos (Index)));
            return;
         end if;
      end loop;
   end On_Freezing_Changed;

   procedure On_Latest
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
   begin
      for Sweeper in Sweeper_Type'Range loop
         Oscilloscope.all.Set_Time
           (Sweeper,
            Ada.Real_Time.Time'
              (Gtk.Layered.Waveform.To_Time
                   (Oscilloscope.all.Time_Axis (Sweeper).Sweeper.all.Get_Upper)));
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Latest"));
   end On_Latest;

   function On_Leave
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean
   is
      pragma Unreferenced (Event, Object, Oscilloscope);
   begin
      return False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Leave"));
         return False;
   end On_Leave;

   function On_Motion
     (Object       : access GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean is separate;

   procedure On_Offset_Changed
     (Sweeper      : access Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      use type Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
   begin
      for Index in Oscilloscope.all.Time_Axis'Range loop
         if Oscilloscope.all.Time_Axis (Index).Sweeper = Sweeper then
            Emit
              (Oscilloscope,
               Signal_IDs (2),
               Guint (Sweeper_Type'Pos (Index)));
            return;
         end if;
      end loop;
   end On_Offset_Changed;

   procedure On_Pause
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
   begin
      for Sweeper in Sweeper_Type'Range loop
         Oscilloscope.all.Set_Frozen (Sweeper, True);
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Pause"));
   end On_Pause;

   procedure On_Range_Selection
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Y1 : constant Gdouble := Oscilloscope.all.Selection.all.Area.all.Get_Box.Y1;
      Y2 : constant Gdouble := Oscilloscope.all.Selection.all.Area.all.Get_Box.Y2;
   begin
      for Index in 1 .. Oscilloscope.all.Channels_Number loop
         declare
            Data   : Channel_Data renames Oscilloscope.all.Channels (Index);
            Got_It : Boolean;
         begin
            Data.Waveform.all.Get (Y1, Data.Value_1, Got_It);
            if Got_It then
               Data.Waveform.all.Get (Y2, Data.Value_2, Got_It);
            end if;
            if Got_It then
               Data.Status := Difference;
            else
               if Data.Status = Difference then
                  Data.Status := Undefined;
               end if;
            end if;
         end;
      end loop;
      Oscilloscope.all.Update_Value;
      Oscilloscope.all.Restore_State;
      Free (Oscilloscope.all.Selection.all.Area);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Range_Selection"));
   end On_Range_Selection;

   procedure On_Raster_Mode_Changed
     (Amplifier    : access Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      use type Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
   begin
      for Index in Oscilloscope.all.Values_Axis'Range loop
         if
           Oscilloscope.all.Values_Axis (Index).Group > 0 and then
           Amplifier =
             Oscilloscope.all.Groups.all
               (Oscilloscope.all.Values_Axis (Index).Group).Amplifier
         then
            Emit
              (Oscilloscope,
               Signal_IDs (1),
               Guint (Amplifier_Type'Pos (Index)));
            return;
         end if;
      end loop;
   end On_Raster_Mode_Changed;

   procedure On_Redo
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
   begin
      Oscilloscope.all.Redo;
   end On_Redo;

   procedure On_Release
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
   begin
      for Sweeper in Sweeper_Type'Range loop
         Oscilloscope.all.Set_Frozen (Sweeper, False);
         if Oscilloscope.all.Jump_On_Thaw then
            Oscilloscope.all.Set_Time
              (Sweeper,
               Ada.Real_Time.Time'
                 (Gtk.Layered.Waveform.To_Time
                      (Oscilloscope.all.Time_Axis
                           (Sweeper).Sweeper.all.Get_Upper)));
         end if;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Release"));
   end On_Release;

   procedure On_Snapshot
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Surface : Cairo.Cairo_Surface;
   begin
      case Oscilloscope.all.Format is
         when No_Snapshot =>
            return;
         when PDF_Snapshot =>
            Surface :=
              Cairo.PDF.Create
                (Filename         => Oscilloscope.all.File.all,
                 Width_In_Points  =>
                    Gdouble (Oscilloscope.all.Get_Allocated_Width),
                 Height_In_Points =>
                    Gdouble (Oscilloscope.all.Get_Allocated_Height));
         when SVG_Snapshot =>
            Surface :=
              Cairo.SVG.Create
                (Filename        => Oscilloscope.all.File.all,
                 Width_In_Point  =>
                    Gdouble (Oscilloscope.all.Get_Allocated_Width),
                 Height_In_Point =>
                    Gdouble (Oscilloscope.all.Get_Allocated_Height));
      end case;
      Oscilloscope.all.Layers.all.Snapshot (Surface);
      Emit (Oscilloscope, Signal_IDs (14), Oscilloscope.all.File.all);
      Cairo.Surface_Destroy (Surface);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Snapshot"));
         Cairo.Surface_Destroy (Surface);
   end On_Snapshot;

   procedure On_Style_Updated
     (Object       : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Object);
   begin
      Oscilloscope.all.Line_Cap :=
        Gtk.Widget.Styles.Line_Cap_Property.Style_Get
          (Oscilloscope, "waveform-line-cap");
      Oscilloscope.all.Background.all.Set
        (Box        => Oscilloscope.all.Background.all.Get_Box,
         Color      =>
           Gtk.Widget.Styles.Style_Get
             (Oscilloscope,
              "background-color",
              Oscilloscope.all.Background.all.Get_Color),
         Line_Width => 0.0,
         Opacity    =>
           Gtk.Layered.Fill_Opacity
             (Gdouble'
                  (Gtk.Widget.Styles.Style_Get
                     (Oscilloscope, "background-opacity"))));

      for Sweeper in Sweeper_Type'Range loop
         Style_Changed_Time_Axis (Oscilloscope, Sweeper);
      end loop;
      for Amplifier in Amplifier_Type'Range loop
         Style_Changed_Values_Axis (Oscilloscope, Amplifier);
      end loop;
      Oscilloscope.all.Proximity :=
        Gdouble
          (Guint'
             (Gtk.Widget.Styles.Style_Get
                (Oscilloscope, "waveform-proximity")));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Style_Updated"));
   end On_Style_Updated;

   procedure On_Toggle_Grid
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      On      : Boolean;
      Defined : Boolean := False;
   begin
      for Index in Amplifier_Type range Left .. Right loop
         if Oscilloscope.all.Has_Group (Index) then
            if not Defined then
               On := not Oscilloscope.all.Get_Values_Grid (Index);
               Defined := True;
            end if;
            Oscilloscope.all.Set_Values_Grid (Index, On);
         end if;
      end loop;
      for Index in Sweeper_Type'Range loop
         if Oscilloscope.all.Time_Axis (Index).Channels > 0 then
            if not Defined then
               On := not Oscilloscope.all.Get_Time_Grid (Index);
               Defined := True;
            end if;
            Oscilloscope.all.Set_Time_Grid (Index, On);
         end if;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Toggle_Grid"));
   end On_Toggle_Grid;

   procedure On_Toggle_Interpolation
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
   begin
      for Index in 1 .. Oscilloscope.all.Channels_Number loop
         declare
            Waveform : Gtk.Layered.Waveform.Waveform_Layer renames
                         Oscilloscope.all.Channels (Index).Waveform.all;
         begin
            case Gtk.Layered.Waveform.Get_Interpolation_Mode (Waveform) is
               when Gtk.Layered.Left =>
                  Waveform.Set_Interpolation_Mode (Gtk.Layered.Linear);
               when Gtk.Layered.Linear =>
                  Waveform.Set_Interpolation_Mode (Gtk.Layered.Left);
            end case;
         end;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Toggle_Interpolation"));
   end On_Toggle_Interpolation;

   procedure On_Undo
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
   begin
      Oscilloscope.all.Undo;
   end On_Undo;

   procedure On_Zoom_In
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Box : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Selection.all.Area.all.Get_Box;
   begin
      Oscilloscope.all.Restore_State;
      Oscilloscope.all.Erase_Redo_Stack;
      Free (Oscilloscope.all.Selection.all.Area);
      Oscilloscope.all.Push_Undo;
      for Group in 1 .. Oscilloscope.all.Groups_Number loop
         Oscilloscope.all.Zoom_In
           (Oscilloscope.all.Groups.all (Group).Amplifier,
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y1)));
      end loop;
      for Index in Sweeper_Type'Range loop
         Oscilloscope.all.Zoom_In
           (Index,
            Get_Time (Oscilloscope, Index, Gint (Box.X1)),
            Get_Time (Oscilloscope, Index, Gint (Box.X2)));
      end loop;
      Oscilloscope.all.Selection.all.Saved := False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Zoom_In"));
   end On_Zoom_In;

   procedure On_Zoom_In_T
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Box : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Selection.all.Area.all.Get_Box;
   begin
      Oscilloscope.all.Restore_State;
      Oscilloscope.all.Erase_Redo_Stack;
      Free (Oscilloscope.all.Selection.all.Area);
      Oscilloscope.all.Push_Undo (Time_Zooming);
      for Index in Sweeper_Type'Range loop
         Oscilloscope.all.Zoom_In
           (Index,
            Get_Time (Oscilloscope, Index, Gint (Box.X1)),
            Get_Time (Oscilloscope, Index, Gint (Box.X2)));
      end loop;
      Oscilloscope.all.Selection.all.Saved := False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Zoom_In_T"));
   end On_Zoom_In_T;

   procedure On_Zoom_In_V
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Box : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Selection.all.Area.all.Get_Box;
   begin
      Oscilloscope.all.Restore_State;
      Oscilloscope.all.Erase_Redo_Stack;
      Free (Oscilloscope.all.Selection.all.Area);
      Oscilloscope.all.Push_Undo (Values_Zooming);
      for Group in 1 .. Oscilloscope.all.Groups_Number loop
         Oscilloscope.all.Zoom_In
           (Oscilloscope.all.Groups.all (Group).Amplifier,
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y1)));
      end loop;
      Oscilloscope.all.Selection.all.Saved := False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Zoom_In_V"));
   end On_Zoom_In_V;

   procedure On_Zoom_Out
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Box : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Selection.all.Area.all.Get_Box;
   begin
      Oscilloscope.all.Restore_State;
      Oscilloscope.all.Erase_Redo_Stack;
      Free (Oscilloscope.all.Selection.all.Area);
      Oscilloscope.all.Push_Undo;
      for Group in 1 .. Oscilloscope.all.Groups_Number loop
         Oscilloscope.all.Zoom_Out
           (Oscilloscope.all.Groups.all (Group).Amplifier,
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y1)));
      end loop;
      for Index in Sweeper_Type'Range loop
         Oscilloscope.all.Zoom_Out
           (Index,
            Get_Time (Oscilloscope, Index, Gint (Box.X1)),
            Get_Time (Oscilloscope, Index, Gint (Box.X2)));
      end loop;
      Oscilloscope.all.Selection.all.Saved := False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Zoom_Out"));
   end On_Zoom_Out;

   procedure On_Zoom_Out_T
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Box : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Selection.all.Area.all.Get_Box;
   begin
      Oscilloscope.all.Restore_State;
      Oscilloscope.all.Erase_Redo_Stack;
      Free (Oscilloscope.all.Selection.all.Area);
      Oscilloscope.all.Push_Undo (Time_Zooming);
      for Index in Sweeper_Type'Range loop
         Oscilloscope.all.Zoom_Out
           (Index,
            Get_Time (Oscilloscope, Index, Gint (Box.X1)),
            Get_Time (Oscilloscope, Index, Gint (Box.X2)));
      end loop;
      Oscilloscope.all.Selection.all.Saved := False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Zoom_Out_T"));
   end On_Zoom_Out_T;

   procedure On_Zoom_Out_V
     (Menu         : access GObject_Record'Class;
      Oscilloscope : Gtk_Oscilloscope)
   is
      pragma Unreferenced (Menu);
      Box : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Selection.all.Area.all.Get_Box;
   begin
      Oscilloscope.all.Restore_State;
      Oscilloscope.all.Erase_Redo_Stack;
      Free (Oscilloscope.all.Selection.all.Area);
      Oscilloscope.all.Push_Undo (Values_Zooming);
      for Group in 1 .. Oscilloscope.all.Groups_Number loop
         Oscilloscope.all.Zoom_Out
           (Oscilloscope.all.Groups.all (Group).Amplifier,
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.all.Get_Value (Group, Gint (Box.Y1)));
      end loop;
      Oscilloscope.all.Selection.all.Saved := False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("On_Zoom_Out_V"));
   end On_Zoom_Out_V;

   procedure Push_Amplifier_Zoom
     (Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      List      : access Items_Stack;
      First     : in out Boolean) is
   begin
      if List /= null then
         List.all.Actions :=
           new Do_Amplifier_Zoom'
             (Ada.Finalization.Limited_Controlled with
              First     => First,
              Next      => List.all.Actions,
              Amplifier => Amplifier,
              Value     => Amplifier.all.Get_Value,
              Page_Size => Amplifier.all.Get_Page_Size);
         Amplifier.all.Ref;
         First := False;
      end if;
   end Push_Amplifier_Zoom;

   procedure Push_Auto_Amplifier
     (Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      List      : access Items_Stack;
      First     : in out Boolean) is
   begin
      if List /= null then
         List.all.Actions :=
           new Do_Auto_Amplifier'
             (Ada.Finalization.Limited_Controlled
              with
              First     => First,
              Next      => List.all.Actions,
              Amplifier => Amplifier);
         Amplifier.all.Ref;
         First := False;
      end if;
   end Push_Auto_Amplifier;

   procedure Push_Release_Sweeper
     (Sweeper : Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
      List    : access Items_Stack;
      First   : in out Boolean) is
   begin
      if List /= null then
         List.all.Actions :=
           new Do_Release_Sweeper'
             (Ada.Finalization.Limited_Controlled with
              First   => First,
              Next    => List.all.Actions,
              Sweeper => Sweeper);
         Sweeper.all.Ref;
         First := False;
      end if;
   end Push_Release_Sweeper;

   procedure Push_Stub (Widget : not null access Gtk_Oscilloscope_Record;
                        Name   : UTF8_String)
   is
      First : Boolean := True;
   begin
      Push_Stub (Name, Widget.all.Undo_Stack'Access, First);
      pragma Unreferenced (First);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Push_Stub"));
   end Push_Stub;

   procedure Push_Stub (Name  : String;
                        List  : access Items_Stack;
                        First : in out Boolean) is
   begin
      if List /= null then
         List.all.Actions :=
           new Do_Stub'
             (Ada.Finalization.Limited_Controlled
              with
              First    => First,
              Next     => List.all.Actions,
              Stack    => List,
              Previous => List.all.Stubs,
              Length   => Name'Length,
              Name     => Name);
         List.all.Stubs := List.all.Actions;
         First := False;
      end if;
   end Push_Stub;

   procedure Push_Sweeper_Zoom
     (Sweeper : Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
      List    : access Items_Stack;
      First   : in out Boolean) is
   begin
      if List /= null then
         List.all.Actions :=
           new Do_Sweeper_Zoom'
             (Ada.Finalization.Limited_Controlled
              with
              First   => First,
              Next    => List.all.Actions,
              Sweeper => Sweeper,
              Time    => Sweeper.all.Get_Time,
              Page    => Sweeper.all.Get_Page_Span);
         Sweeper.all.Ref;
         First := False;
      end if;
   end Push_Sweeper_Zoom;

   procedure Push_Undo
     (Widget : not null access Gtk_Oscilloscope_Record;
      State  : Zooming_State := Values_Zooming or Time_Zooming)
   is
      First : Boolean := True;
   begin
      if 0 /= (State and Values_Zooming) then
         for Group in 1 .. Widget.all.Groups_Number loop
            Widget.all.Save_Amplifier (Group, First);
         end loop;
      end if;
      if 0 /= (State and Time_Zooming) then
         for Index in Sweeper_Type'Range loop
            Widget.all.Save_Sweeper (Index, First);
         end loop;
      end if;
   end Push_Undo;

   procedure Redo
     (Widget : not null access Gtk_Oscilloscope_Record;
      Till   : UTF8_String := "";
      Stub   : UTF8_String := "")
   is
      This  : Do_Item_Ptr := Widget.all.Redo_Stack.Actions;
      First : Boolean := True;
      Done  : Boolean := False;
   begin
      if Till'Length > 0 and then Stub'Length > 0 then
         Push_Stub (Stub, Widget.all.Undo_Stack'Access, First);
      end if;
      while This /= null and then not Done loop
         Widget.all.Redo_Stack.Actions := This.all.Next;
         if Till'Length > 0 then
            if This.all in Do_Stub'Class then
               Done :=
                 Strings_Edit.UTF8.Wildcards.Case_Insensitive.Match_Insensitive
                   (Do_Stub'Class (This.all).Name, Till, True);
            else
               Done := False; -- Never stop
            end if;
         else
            Done := This.all.First; -- Sequence beginning stop
         end if;
         This.all.Do_It (First, Widget.all, Widget.all.Undo_Stack'Access);
         Free (This);
         This := Widget.all.Redo_Stack.Actions;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Redo"));
   end Redo;

   overriding procedure Refresh (Widget  : not null access Gtk_Graphs_Record;
                                 Context : Cairo.Cairo_Context)
   is
      use type Gtk.Layered.Waveform.Ring_Data_Buffer.Gtk_Wavefrom_Ring_Data_Buffer;
   begin
      Gtk.Layered.Gtk_Layered_Record (Widget.all).Refresh (Context);
      if
        Widget.all.Oscilloscope.all.Refresh_Period /= null or else
        Widget.all.Oscilloscope.all.Drawing_Time /= null
      then
         declare
            use type Ada.Real_Time.Time;
            T1 : constant Ada.Real_Time.Time := Widget.all.Get_Drawing_Time;
            T2 : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
         begin
            if Widget.all.Oscilloscope.all.Refresh_Period /= null then
               if Widget.all.Last_Time /= Ada.Real_Time.Time_First then
                  Widget.all.Oscilloscope.all.Refresh_Period.all.Put
                    (T =>
                       Gtk.Layered.Waveform.X_Axis
                         (Gtk.Layered.Waveform.To_Double (T2)),
                     V =>
                       Gtk.Layered.Waveform.Y_Axis
                         (Ada.Real_Time.To_Duration
                              (T1 - Widget.all.Last_Time)));
               end if;
               Widget.all.Last_Time := T1;
            end if;
            if Widget.all.Oscilloscope.all.Drawing_Time /= null then
               Widget.all.Oscilloscope.all.Drawing_Time.all.Put
                 (T =>
                    Gtk.Layered.Waveform.X_Axis
                      (Gtk.Layered.Waveform.To_Double (T2)),
                  V =>
                    Gtk.Layered.Waveform.Y_Axis
                      (Ada.Real_Time.To_Duration (T2 - T1)));
            end if;
         end;
      end if;
   end Refresh;

   overriding
   procedure Resized
     (Widget     : not null access Gtk_Graphs_Record;
      Allocation : Gtk.Widget.Gtk_Allocation)
   is
      Width  : constant Gdouble := Gdouble (Allocation.Width);
      Height : constant Gdouble := Gdouble (Allocation.Height);
   begin
      Widget.all.Set_Aspect_Ratio (Width / Height);
      Box_Changed (Widget.all.Oscilloscope);
   end Resized;

   procedure Restore_State (Widget : not null access Gtk_Oscilloscope_Record)
   is
      First : Boolean     := True;
      This  : Do_Item_Ptr := Widget.all.Undo_Stack.Actions;
   begin
      while This /= null loop
         Widget.all.Undo_Stack.Actions := This.all.Next;
         declare
            Done : constant Boolean := This.all.First;
         begin
            This.all.Do_It (First, Widget.all);
            Free (This);
            exit when Done;
         end;
         This := Widget.all.Undo_Stack.Actions;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Restore_State"));
   end Restore_State;

   procedure Save_Amplifier
     (Widget : not null access Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      First  : in out Boolean) is
   begin
      if Widget.all.Groups /= null and then Group in Widget.all.Groups'Range
      then
         declare
            This : constant Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier :=
                     Widget.all.Groups.all (Group).Amplifier;
         begin
            if This.all.Get_Auto_Scaling then
               Push_Auto_Amplifier
                 (This,
                  Widget.all.Undo_Stack'Access,
                  First);
            else
               Push_Amplifier_Zoom
                 (This,
                  Widget.all.Undo_Stack'Access,
                  First);
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Save_Amplifier"));
   end Save_Amplifier;

   procedure Save_Sweeper
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      First   : in out Boolean)
   is
      This : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
               Widget.all.Time_Axis (Sweeper).Sweeper;
   begin
      if This.all.Get_Frozen then
         Push_Sweeper_Zoom (This, Widget.all.Undo_Stack'Access, First);
      else
         Push_Release_Sweeper (This, Widget.all.Undo_Stack'Access, First);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Save_Sweeper"));
   end Save_Sweeper;

   procedure Set_Auto_Scaling
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Auto      : Boolean)
   is
      This  : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
      Group : constant Group_Count := This.Group;
   begin
      if Group = 0 then
         raise Constraint_Error with
           "No group assigned to the amplifier";
      else
         Widget.all.Groups.all (Group).Amplifier.all.Set_Auto_Scaling (Auto);
      end if;
   end Set_Auto_Scaling;

   procedure Set_Auto_Scaling
     (Widget : not null access Gtk_Oscilloscope_Record;
      Group  : Group_Number;
      Auto   : Boolean) is
   begin
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      else
         Widget.all.Groups.all (Group).Amplifier.all.Set_Auto_Scaling (Auto);
      end if;
   end Set_Auto_Scaling;

   procedure Set_Default_Face
     (Widget : not null access Gtk_Oscilloscope_Record;
      Face   : Pango.Cairo.Fonts.Pango_Cairo_Font) is
   begin
      Widget.all.Default_Face := Face;
   end Set_Default_Face;

   procedure Set_Enabled_Dropdown_Items
     (Widget : not null access Gtk_Oscilloscope_Record;
      Items  : Dropdown_Items) is
   begin
      Widget.all.Menu_Enabled := Items;
   end Set_Enabled_Dropdown_Items;

   procedure Set_Extrapolation_Mode
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Left    : Boolean;
      Right   : Boolean)
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      else
         declare
            Waveform : Gtk.Layered.Waveform.Waveform_Layer renames
                         Widget.all.Channels (Channel).Waveform.all;
         begin
            if
              Left /= Gtk.Layered.Waveform.Get_Left_Extrapolation_Mode (Waveform) or else
              Right /= Gtk.Layered.Waveform.Get_Right_Extrapolation_Mode (Waveform)
            then
               Gtk.Layered.Waveform.Set_Extrapolation_Mode
                 (Waveform, Left, Right);
               Row :=
                 Widget.all.Channel_Names.all.Nth_Child
                   (Gtk.Tree_Model.Null_Iter, Gint (Channel) - 1);
               Widget.all.Channel_Names.all.Set (Row, 6, Left);
               Widget.all.Channel_Names.all.Set (Row, 7, Right);
               Emit (Widget, Signal_IDs (15), Guint (Channel));
            end if;
         end;
      end if;
   end Set_Extrapolation_Mode;

   procedure Set_Frequency
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Frames  : Gdouble)
   is
      Box : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
   begin
      Set_Page_Span
        (Widget,
         Sweeper,
         Duration ((Box.X2 - Box.X1) / Frames));
   end Set_Frequency;

   procedure Set_Frozen
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Frozen  : Boolean)
   is
      use type Gtk.Scale.Gtk_Scale;
      This : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if Frozen and then not This.No_Scale then
         if This.Scale = null and then not Widget.all.Selection.all.Engaged then
            Gtk.Box.Gtk_New_Hbox (This.Box);
            Gtk.Fixed.Gtk_New (This.Left_Fill);
            This.Left_Fill.all.Set_Size_Request
              (Width  => Widget.all.Get_Annotation_Width (Left),
               Height => 1);
            This.Box.all.Pack_Start (This.Left_Fill, False, False);
            Gtk.Scale.Gtk_New_Hscale
              (This.Scale, This.Sweeper.all'Unchecked_Access);
            This.Scale.all.Set_Hexpand (True);
            This.Scale.all.Set_Vexpand (False);
            This.Box.all.Pack_Start (This.Scale);
            Gtk.Fixed.Gtk_New (This.Right_Fill);
            This.Right_Fill.all.Set_Size_Request
              (Width  => Widget.all.Get_Annotation_Width (Right),
               Height => 1);
            This.Box.all.Pack_Start (This.Right_Fill, False, False);
            case Sweeper is
               when Upper =>
                  This.Scale.all.Set_Value_Pos (Gtk.Enums.Pos_Top);
                  Widget.all.Attach_Next_To
                    (This.Box,
                     Widget.all.Layers,
                     Gtk.Enums.Pos_Top);
               when Lower =>
                  This.Scale.all.Set_Value_Pos (Gtk.Enums.Pos_Bottom);
                  Widget.all.Attach_Next_To
                    (This.Box,
                     Widget.all.Layers,
                     Gtk.Enums.Pos_Bottom);
            end case;
            This.Box.all.Show;
            This.Left_Fill.all.Show;
            This.Right_Fill.all.Show;
            This.Box.all.Show_All;
            Format_Handlers.Connect
              (This.Scale,
               "format_value",
               On_Format_Time'Access,
               This'Unchecked_Access,
               True);
         end if;
      else
         if This.Scale /= null then
            Widget.all.Remove (This.Box);
            This.Scale      := null;
            This.Box        := null;
            This.Left_Fill  := null;
            This.Right_Fill := null;
         end if;
      end if;
      This.Sweeper.all.Set_Frozen (Frozen);
   end Set_Frozen;

   procedure Set_Grid_Colors
     (Widget      : not null access Gtk_Oscilloscope_Record;
      Major_Color : Gdk.Color.Gdk_Color;
      Minor_Color : Gdk.Color.Gdk_Color)
   is
      procedure Set_Grid
        (Paper : in out Gtk.Layered.Graph_Paper.Graph_Paper_Layer);
      procedure Set_Grid
        (Paper : in out Gtk.Layered.Graph_Paper.Graph_Paper_Layer)
      is
         Major : Gtk.Layered.Line_Parameters := Paper.Get_Major_Line;
         Minor : Gtk.Layered.Line_Parameters := Paper.Get_Minor_Line;
      begin
         Major.Color := Major_Color;
         Minor.Color := Minor_Color;
         Paper.Set
           (Box           => Paper.Get_Box,
            X_Tick_Length => Paper.Get_X_Tick_Length,
            Y_Tick_Length => Paper.Get_Y_Tick_Length,
            Major_Line    => Major,
            Minor_Line    => Minor);
      end Set_Grid;
   begin
      Widget.all.Major_Color := Major_Color;
      Widget.all.Minor_Color := Minor_Color;
      for Index in Widget.all.Values_Axis'Range loop
         declare
            Data : Values_Axis_Data renames Widget.all.Values_Axis (Index);
         begin
            if Data.Group > 0 and then Data.Ticks /= null then
               Set_Grid (Data.Ticks.all);
            end if;
         end;
      end loop;
      for Index in Sweeper_Type'Range loop
         declare
            Data : Time_Axis_Data renames Widget.all.Time_Axis (Index);
         begin
            if Data.Ticks /= null then
               Set_Grid (Data.Ticks.all);
            end if;
         end;
      end loop;
   end Set_Grid_Colors;

   procedure Set_Group
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Group     : Group_Number)
   is
      use type Gtk.Scale.Gtk_Scale;
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if Group > Widget.all.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      end if;
      if Data.Group /= Group then
         Data.Group := Group;
         if Data.Ticks /= null then
            Data.Ticks.all.Set_Y_Axis (Widget.all.Groups.all (Group).Amplifier);
         end if;
         if Data.Scale /= null then
            Widget.all.Remove (Data.Box);
            Data.Scale      := null;
            Data.Scale      := null;
            Data.Box        := null;
            Data.Upper_Fill := null;
            Data.Lower_Fill := null;
         end if;
         Widget.all.Update_Amplifier (Amplifier);
         Emit
           (Widget,
            Signal_IDs (4),
            Amplifier_Type'Pos (Amplifier));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Group"));
   end Set_Group;

   procedure Set_Interpolation_Mode
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Mode    : Gtk.Layered.Interpolation_Mode)
   is
      use type Gtk.Layered.Interpolation_Mode;
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      elsif
        Widget.all.Channels (Channel).Waveform.all.Get_Interpolation_Mode /= Mode
      then
         Widget.all.Channels (Channel).Waveform.all.
           Set_Interpolation_Mode (Mode);
         Widget.all.Channel_Names.all.Set
           (Widget.all.Channel_Names.all.Nth_Child
              (Gtk.Tree_Model.Null_Iter, Gint (Channel) - 1),
            3,
            Gtk.Layered.Interpolation_Mode'Pos (Mode));
         Emit (Widget, Signal_IDs (10), Guint (Channel));
      end if;
   end Set_Interpolation_Mode;

   procedure Set_Manual_Sweep
     (Widget : not null access Gtk_Oscilloscope_Record;
      Enable : Boolean) is
   begin
      Widget.all.Manual_Sweep := Enable;
   end Set_Manual_Sweep;

   procedure Set_Page_Span
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Sweeper   : Sweeper_Type;
      Page_Span : Duration)
   is
      Adjustment : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
                     Widget.all.Time_Axis (Sweeper).Sweeper;
   begin
      if Adjustment.all.Get_Frozen then
         declare
            use type Ada.Real_Time.Time;
            Old : constant Ada.Real_Time.Time := Adjustment.all.Get_Time;
         begin
            Adjustment.all.Set_Page_Span (Page_Span);
            if Old /= Adjustment.all.Get_Time then
               Emit
                 (Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper));
            end if;
         end;
      else
         declare
            Offset : constant Duration := Adjustment.all.Get_Offset;
         begin
            Adjustment.all.Set_Page_Span (Page_Span);
            if Offset /= Adjustment.all.Get_Offset then
               Emit
                 (Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper));
            end if;
         end;
      end if;
   end Set_Page_Span;

   procedure Set_Preferred_Method
     (Widget : not null access Gtk_Oscilloscope_Record;
      Method : Gtk.Layered.Waveform_Drawing_Method) is
   begin
      for Index in 1 .. Widget.all.Channels_Number loop
         Widget.all.Channels (Index).Waveform.all.Set_Preferred_Method (Method);
      end loop;
   end Set_Preferred_Method;

   procedure Set_Release_To_Latest
     (Widget : not null access Gtk_Oscilloscope_Record;
      Enable : Boolean) is
   begin
      Widget.all.Jump_On_Thaw := Enable;
   end Set_Release_To_Latest;

   procedure Set_Selection_Mode
     (Widget : not null access Gtk_Oscilloscope_Record;
      Action : Selection_Action) is
   begin
      Widget.all.Selection_Mode := Action;
   end Set_Selection_Mode;

   procedure Set_Snapshot_File
     (Widget : not null access Gtk_Oscilloscope_Record;
      Format : Snapshot_Format := No_Snapshot;
      Name   : String := "") is
   begin
      if Widget.all.File /= null then
         Widget.all.Format := No_Snapshot;
         Free (Widget.all.File);
      end if;
      if Format /= No_Snapshot and then Name'Length > 0 then
         Widget.all.File := new String'(Name);
         Widget.all.Format := Format;
      end if;
   end Set_Snapshot_File;

   procedure Set_Superscript
     (Widget      : not null access Gtk_Oscilloscope_Record;
      Superscript : Boolean) is
   begin
      Widget.all.Superscript := Superscript;
      for Amplifier in Widget.all.Values_Axis'Range loop
         declare
            Data : Values_Axis_Data renames
                     Widget.all.Values_Axis (Amplifier);
         begin
            if Data.Texts /= null then
               Data.Texts.all.Set
                 (Location    => Data.Texts.all.Get_Location,
                  Face        => Data.Texts.all.Get_Face,
                  Height      => Data.Texts.all.Get_Height,
                  Stretch     => Data.Texts.all.Get_Stretch,
                  Color       => Data.Texts.all.Get_Color,
                  Text_Angle  => Data.Texts.all.Get_Text_Angle,
                  Justify_X   => Data.Texts.all.Get_Justify_X,
                  Justify_Y   => Data.Texts.all.Get_Justify_Y,
                  Background  => Data.Texts.all.Get_Background_Color,
                  Border      => Data.Texts.all.Get_Border,
                  Overlap     => Data.Texts.all.Get_Overlap,
                  Opacity     => Data.Texts.all.Get_Opacity,
                  Superscript => Superscript);
            end if;
         end;
      end loop;
      for Sweeper in Widget.all.Time_Axis'Range loop
         declare
            Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
         begin
            if Data.Texts /= null then
               Data.Texts.all.Set
                 (Location    => Data.Texts.all.Get_Location,
                  Face        => Data.Texts.all.Get_Face,
                  Height      => Data.Texts.all.Get_Height,
                  Stretch     => Data.Texts.all.Get_Stretch,
                  Color       => Data.Texts.all.Get_Color,
                  Text_Angle  => Data.Texts.all.Get_Text_Angle,
                  Justify_X   => Data.Texts.all.Get_Justify_X,
                  Justify_Y   => Data.Texts.all.Get_Justify_Y,
                  Background  => Data.Texts.all.Get_Background_Color,
                  Border      => Data.Texts.all.Get_Border,
                  Overlap     => Data.Texts.all.Get_Overlap,
                  Opacity     => Data.Texts.all.Get_Opacity,
                  Superscript => Superscript);
            end if;
         end;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Superscript"));
   end Set_Superscript;

   procedure Set_Time
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Real_Time.Time)
   is
      Adjustment : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
                     Widget.all.Time_Axis (Sweeper).Sweeper;
   begin
      if Adjustment.all.Get_Frozen then
         declare
            use type Ada.Real_Time.Time;
            Old : constant Ada.Real_Time.Time := Adjustment.all.Get_Time;
         begin
            Adjustment.all.Set_Time (Stamp);
            if Old /= Adjustment.all.Get_Time then
               Emit
                 (Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper));
            end if;
         end;
      else
         declare
            Offset : constant Duration := Adjustment.all.Get_Offset;
         begin
            Adjustment.all.Set_Time (Stamp);
            if Offset /= Adjustment.all.Get_Offset then
               Emit
                 (Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper));
            end if;
         end;
      end if;
   end Set_Time;

   procedure Set_Time
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Stamp   : Ada.Calendar.Time)
   is
      Adjustment : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
                     Widget.all.Time_Axis (Sweeper).Sweeper;
   begin
      if Adjustment.all.Get_Frozen then
         declare
            use type Ada.Real_Time.Time;
            Old : constant Ada.Real_Time.Time := Adjustment.all.Get_Time;
         begin
            Adjustment.all.Set_Time (Stamp);
            if Old /= Adjustment.all.Get_Time then
               Emit
                 (Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper));
            end if;
         end;
      else
         declare
            Offset : constant Duration := Adjustment.all.Get_Offset;
         begin
            Adjustment.all.Set_Time (Stamp);
            if Offset /= Adjustment.all.Get_Offset then
               Emit
                 (Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper));
            end if;
         end;
      end if;
   end Set_Time;

   procedure Set_Time_Axis
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Visible : Boolean;
      As_Time : Boolean := True)
   is
      use type Pango.Cairo.Fonts.Font_Type;
      use type Gtk.Scale.Gtk_Scale;
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if (Visible xor Data.On) or else (As_Time xor Data.Time_Mode) then
         if Visible then
            if Data.On then -- Turn it off and change the mode
               Widget.all.Set_Time_Axis (Sweeper, False, As_Time);
            else
               Data.Time_Mode := As_Time;
            end if;
            if not Data.Width_Set then
               Data.Width :=
                 Guint'
                   (Gtk.Widget.Styles.Style_Get (Widget, "time-axis-height"));
            end if;
            Data.Offset := Gdouble (Data.Width);
            Data.Line :=
              Gtk.Layered.Line.Add_Line
                (Under => Widget.all.Background.all.Atop,
                 Angle => 0.0).all'Unchecked_Access;
            if not Data.Grid then
               Data.Ticks :=
                 Gtk.Layered.Graph_Paper.Add_Graph_Paper
                   (Under  => Data.Line,
                    Box    => (-0.5, -0.5, 0.5, 0.5),
                    X_Axis => Data.Sweeper).all'Unchecked_Access;
            end if;
            if Pango.Cairo.Fonts.Get_Type (Data.Face) = Pango.Cairo.Fonts.Null_Font then
               Data.Face := Widget.all.Default_Face;
            end if;
            Data.Texts :=
              Gtk_Oscilloscope_Record'Class (Widget.all).
              Create_Annotation (Sweeper).all'Unchecked_Access;
         else
            Data.Offset := 0.0;
            Data.Time_Mode := As_Time;
            Free (Data.Line);
            if not Data.Grid then
               Free (Data.Ticks);
            end if;
            Free (Data.Texts);
         end if;
         Data.On := Visible;
         if Data.Scale /= null then
            Data.Scale.all.Set_Draw_Value (not Visible);
         end if;
         Box_Changed (Widget);
         Emit (Widget, Signal_IDs (5), Sweeper_Type'Pos (Sweeper));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Time_Axis"));
   end Set_Time_Axis;

   procedure Set_Time_Axis_Height
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Height  : Natural)
   is
      use type Gtk.Scale.Gtk_Scale;
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      Data.Width_Set := True;
      if Data.Width /= Guint (Height) then
         Data.Width := Guint (Height);
         if Data.On then
            Data.Offset := Gdouble (Data.Width);
            Box_Changed (Widget);
         end if;
         for Amplifier in Widget.all.Values_Axis'Range loop
            case Amplifier is
               when Left | Right =>
                  declare
                     This : Values_Axis_Data renames
                              Widget.all.Values_Axis (Amplifier);
                  begin
                     if This.Scale /= null then
                        case Sweeper is
                           when Upper =>
                              This.Upper_Fill.all.Set_Size_Request
                                (Width  => 1,
                                 Height =>
                                    Widget.all.Get_Annotation_Height (Sweeper));
                           when Lower =>
                              This.Lower_Fill.all.Set_Size_Request
                                (Width  => 1,
                                 Height =>
                                    Widget.all.Get_Annotation_Height (Sweeper));
                        end case;
                     end if;
                  end;
               when Middle =>
                  null;
            end case;
         end loop;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Time_Axis_Height"));
   end Set_Time_Axis_Height;

   procedure Set_Time_Grid
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Visible : Boolean)
   is
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if Data.Grid xor Visible then
         if Visible then
            if not Data.On then
               Data.Ticks :=
                 Gtk.Layered.Graph_Paper.Add_Graph_Paper
                   (Under       => Widget.all.Background.all.Atop,
                    Box         => (-0.5, -0.5, 0.5, 0.5),
                    X_Axis      => Data.Sweeper,
                    Major_Color => Widget.all.Major_Color,
                    Minor_Color => Widget.all.Minor_Color).all'Unchecked_Access;
            end if;
         else
            if not Data.On then
               Free (Data.Ticks);
            end if;
         end if;
         Data.Grid := Visible;
         for Sweeper in Sweeper_Type'Range loop
            Style_Changed_Time_Axis (Widget, Sweeper);
         end loop;
         Emit (Widget, Signal_IDs (7), Sweeper_Type'Pos (Sweeper));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Time_Grid"));
   end Set_Time_Grid;

   procedure Set_Time_Scale
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Visible : Boolean)
   is
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if Data.No_Scale xor not Visible then
         Data.No_Scale := not Visible;
         Widget.all.Set_Frozen (Sweeper, Widget.all.Get_Frozen (Sweeper));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Time_Scale"));
   end Set_Time_Scale;

   procedure Set_Time_Text_Alignment
     (Widget     : not null access Gtk_Oscilloscope_Record;
      Sweeper    : Sweeper_Type;
      Horizontal : Ada.Strings.Alignment;
      Vertical   : Gtk.Layered.Vertical_Alignment)
   is
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      Data.Justify_X := Horizontal;
      Data.Justify_Y := Vertical;
      if Data.On then
         Data.Texts.all.Set
           (Location    => Data.Texts.all.Get_Location,
            Face        => Data.Texts.all.Get_Face,
            Height      => Data.Texts.all.Get_Height,
            Stretch     => Data.Texts.all.Get_Stretch,
            Color       => Data.Texts.all.Get_Color,
            Text_Angle  => Data.Texts.all.Get_Text_Angle,
            Justify_X   => Horizontal,
            Justify_Y   => Vertical,
            Superscript => Widget.all.Get_Superscript,
            Background  => Data.Texts.all.Get_Background_Color,
            Border      => Data.Texts.all.Get_Border,
            Overlap     => Data.Texts.all.Get_Overlap,
            Opacity     => Data.Texts.all.Get_Opacity);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Time_Text_Alignment"));
   end Set_Time_Text_Alignment;

   procedure Set_Time_Text_Font
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      Face    : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height  : Gdouble;
      Stretch : Gdouble   := 1.0;
      Color   : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle   : Gdouble   := 0.0)
   is
      Data : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      elsif Stretch <= 0.0 then
         raise Constraint_Error with "Non-positive stretch";
      end if;
      Data.Angle   := Angle;
      Data.Color   := Color;
      Data.Height  := Height;
      Data.Stretch := Stretch;
      Data.Face    := Face;
      if Data.Texts /= null then
         Data.Texts.all.Set
           (Location    => Data.Texts.all.Get_Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Color       => Color,
            Text_Angle  => Angle,
            Justify_X   => Data.Texts.all.Get_Justify_X,
            Justify_Y   => Data.Texts.all.Get_Justify_Y,
            Superscript => Widget.all.Get_Superscript,
            Background  => Data.Texts.all.Get_Background_Color,
            Border      => Data.Texts.all.Get_Border,
            Overlap     => Data.Texts.all.Get_Overlap,
            Opacity     => Data.Texts.all.Get_Opacity);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Time_Text_Font"));
   end Set_Time_Text_Font;

   procedure Set_Time_Tooltip
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Visible : Boolean) is
   begin
      Widget.all.Show_Time := Visible;
   end Set_Time_Tooltip;

   procedure Set_Time_Tooltip_Suffix
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Suffix  : UTF8_String) is
   begin
      if Channel <= Widget.all.Channels_Number then
         Free (Widget.all.Channels (Channel).Tip_X_Suffix);
         Widget.all.Channels (Channel).Tip_X_Suffix := new String'(Suffix);
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Set_Time_Tooltip_Suffix;

   procedure Set_Tooltip_Annotation
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Text    : UTF8_String) is
   begin
      if Channel <= Widget.all.Channels_Number then
         Free (Widget.all.Channels (Channel).Tip_Prefix);
         Widget.all.Channels (Channel).Tip_Prefix := new String'(Text);
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Set_Tooltip_Annotation;

   procedure Set_Values_Alignment
     (Widget     : not null access Gtk_Oscilloscope_Record;
      Amplifier  : Amplifier_Type;
      Horizontal : Ada.Strings.Alignment;
      Vertical   : Gtk.Layered.Vertical_Alignment)
   is
      use type Ada.Strings.Alignment;
      use type Gtk.Layered.Vertical_Alignment;
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if
        Data.Justify_X /= Horizontal or else
        Data.Justify_Y /= Vertical
      then
         Data.Justify_X := Horizontal;
         Data.Justify_Y := Vertical;
         if Data.On and then Data.Group > 0 then
            Data.Texts.all.Set
              (Location    => Data.Texts.all.Get_Location,
               Face        => Data.Texts.all.Get_Face,
               Height      => Data.Texts.all.Get_Height,
               Stretch     => Data.Texts.all.Get_Stretch,
               Color       => Data.Texts.all.Get_Color,
               Text_Angle  => Data.Texts.all.Get_Text_Angle,
               Justify_X   => Data.Justify_X,
               Justify_Y   => Data.Justify_Y,
               Superscript => Widget.all.Get_Superscript,
               Background  => Data.Texts.all.Get_Background_Color,
               Border      => Data.Texts.all.Get_Border,
               Overlap     => Data.Texts.all.Get_Overlap,
               Opacity     => Data.Texts.all.Get_Opacity);
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Values_Alignment"));
   end Set_Values_Alignment;

   procedure Set_Values_Axis
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Visible   : Boolean)
   is
      use type Pango.Cairo.Fonts.Font_Type;
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if Data.Group = 0 then
         raise Constraint_Error with "No group assigned";
      end if;
      if Visible xor Data.On then
         if Visible then
            if not Data.Width_Set then
               Data.Width :=
                 Guint'
                   (Gtk.Widget.Styles.Style_Get (Widget, "values-axis-width"));
            end if;
            case Amplifier is
               when Left | Right =>
                  Data.Offset := Gdouble (Data.Width);
               when Middle =>
                  null;
            end case;
            Data.Line :=
              Gtk.Layered.Line.Add_Line
                (Under => Widget.all.Background.all.Atop,
                 Angle => 0.0).all'Unchecked_Access;
            if not Data.Grid then
               Data.Ticks :=
                 Gtk.Layered.Graph_Paper.Add_Graph_Paper
                   (Under   => Data.Line,
                    Box     => (-0.5, -0.5, 0.5, 0.5),
                    Y_Axis  =>
                      Widget.all.Groups.all
                        (Widget.all.Values_Axis
                           (Amplifier).Group).Amplifier).all'Unchecked_Access;
            end if;
            if Pango.Cairo.Fonts.Get_Type (Data.Face) = Pango.Cairo.Fonts.Null_Font then
               Data.Face := Widget.all.Default_Face;
            end if;
            Data.Texts :=
              Gtk_Oscilloscope_Record'Class (Widget.all).
              Create_Annotation (Amplifier).all'Unchecked_Access;
         else
            case Amplifier is
               when Left | Right =>
                  Data.Offset := 0.0;
               when Middle =>
                  null;
            end case;
            Free (Data.Line);
            if not Data.Grid then
               Free (Data.Ticks);
            end if;
            Free (Data.Texts);
         end if;
         Data.On := Visible;
         Box_Changed (Widget);
         Emit
           (Widget,
            Signal_IDs (6),
            Amplifier_Type'Pos (Amplifier));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Values_Axis"));
   end Set_Values_Axis;

   procedure Set_Values_Axis_Width
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Width     : Natural)
   is
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      Data.Width_Set := True;
      if Data.Width /= Guint (Width) then
         Data.Width := Guint (Width);
         if Data.On then
            Data.Offset := Gdouble (Data.Width);
            Box_Changed (Widget);
         end if;
         for Sweeper in Widget.all.Time_Axis'Range loop
            declare
               use type Gtk.Scale.Gtk_Scale;
               This : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
            begin
               if This.Scale /= null then
                  case Amplifier is
                     when Left =>
                        This.Left_Fill.all.Set_Size_Request
                          (Height => 1,
                           Width  =>
                              Widget.all.Get_Annotation_Width (Amplifier));
                     when Right =>
                        This.Right_Fill.all.Set_Size_Request
                          (Height => 1,
                           Width  =>
                              Widget.all.Get_Annotation_Width (Amplifier));
                     when others =>
                        null;
                  end case;
               end if;
            end;
         end loop;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Values_Axis_Width"));
   end Set_Values_Axis_Width;

   procedure Set_Values_Grid
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Visible   : Boolean)
   is
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if Data.Group = 0 then
         raise Constraint_Error with "No group assigned";
      end if;
      if Data.Grid xor Visible then
         if Visible then
            if not Data.On and then Data.Group > 0 then
               Data.Ticks :=
                 Gtk.Layered.Graph_Paper.Add_Graph_Paper
                   (Under       => Widget.all.Background.all.Atop,
                    Box         => (-0.5, -0.5, 0.5, 0.5),
                    Y_Axis      => Widget.all.Groups.all (Data.Group).Amplifier,
                    Major_Color => Widget.all.Major_Color,
                    Minor_Color => Widget.all.Minor_Color).all'Unchecked_Access;
            end if;
         else
            if not Data.On then
               Free (Data.Ticks);
            end if;
         end if;
         Data.Grid := Visible;
         for Amplifier in Amplifier_Type'Range loop
            Style_Changed_Values_Axis (Widget, Amplifier);
         end loop;
         Emit
           (Widget,
            Signal_IDs (8),
            Amplifier_Type'Pos (Amplifier));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Values_Grid"));
   end Set_Values_Grid;

   procedure Set_Values_Scale
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Visible   : Boolean)
   is
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if Data.No_Scale xor not Visible then
         Data.No_Scale := not Visible;
         Widget.all.Update_Amplifier (Amplifier);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Values_Scale"));
   end Set_Values_Scale;

   procedure Set_Values_Text_Font
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type;
      Face      : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height    : Gdouble;
      Stretch   : Gdouble   := 1.0;
      Color     : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle     : Gdouble   := 0.0)
   is
      Data : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      elsif Stretch <= 0.0 then
         raise Constraint_Error with "Non-positive stretch";
      end if;
      Data.Angle   := Angle;
      Data.Color   := Color;
      Data.Height  := Height;
      Data.Stretch := Stretch;
      Data.Face    := Face;
      if Data.Texts /= null then
         Data.Texts.all.Set
           (Location    => Data.Texts.all.Get_Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Color       => Color,
            Text_Angle  => Angle,
            Justify_X   => Data.Texts.all.Get_Justify_X,
            Justify_Y   => Data.Texts.all.Get_Justify_Y,
            Superscript => Widget.all.Get_Superscript,
            Background  => Data.Texts.all.Get_Background_Color,
            Border      => Data.Texts.all.Get_Border,
            Overlap     => Data.Texts.all.Get_Overlap,
            Opacity     => Data.Texts.all.Get_Opacity);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Values_Text_Font"));
   end Set_Values_Text_Font;

   procedure Set_Values_Tooltip_Suffix
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Suffix  : UTF8_String) is
   begin
      if Channel <= Widget.all.Channels_Number then
         Free (Widget.all.Channels (Channel).Tip_Y_Suffix);
         Widget.all.Channels (Channel).Tip_Y_Suffix := new String'(Suffix);
      else
         raise Constraint_Error with
           "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Set_Values_Tooltip_Suffix;

   procedure Set_Visible
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Channel : Channel_Number;
      Visible : Boolean) is
   begin
      if Channel > Widget.all.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      end if;
      if Widget.all.Channels (Channel).Waveform.all.Is_Visible xor Visible then
         Widget.all.Channels (Channel).Waveform.all.Set_Visible (Visible);
         Widget.all.Channel_Names.all.Set
           (Widget.all.Channel_Names.all.Nth_Child
              (Gtk.Tree_Model.Null_Iter, Gint (Channel) - 1),
            3,
            Visible);
         Emit (Widget, Signal_IDs (9), Guint (Channel));
      end if;
   end Set_Visible;

   procedure Style_Changed_Time_Axis
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type)
   is
      Box    : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
      Height : Gdouble;
      Data   : Time_Axis_Data renames Widget.all.Time_Axis (Sweeper);
   begin
      if Data.On then
         Height :=
           Gdouble
             (Guint'
                (Gtk.Widget.Styles.Style_Get
                   (Widget, "time-tick-height")));
         declare -- Setting style of the line
            Line : Gtk.Layered.Line_Parameters;
            Y    : Gdouble;
         begin
            Line := Data.Line.all.Get_Line;
            Line.Color :=
              Gtk.Widget.Styles.Style_Get
                (Widget, "time-line-color", Line.Color);
            Line.Width :=
              Gdouble
                (Guint'
                   (Gtk.Widget.Styles.Style_Get (Widget, "time-line-width")));
            Line.Line_Cap :=
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "time-line-cap");
            case Sweeper is
               when Lower =>
                  Y := Gdouble'Floor (Box.Y2) + 0.5;
               when Upper =>
                  Y := Gdouble'Floor (Box.Y1) + 0.5;
            end case;
            Data.Line.all.Set
              (From => (Box.X1, Y),
               To   => (Box.X2, Y),
               Line => Line);
         end;
         declare -- Setting the time annotation
            Location : Gtk.Layered.Graph_Paper_Annotation.Axis_Location :=
                         Data.Texts.all.Get_Location;
         begin
            Location.Left  := Box.X1;
            Location.Right := Box.X2;
            case Sweeper is
               when Lower =>
                  Location.Y_Position :=
                    (Box.Y2 +
                       (Widget.all.Time_Axis (Lower).Offset + Height) / 2.0);
               when Upper =>
                  Location.Y_Position :=
                    (Box.Y1 -
                       (Widget.all.Time_Axis (Upper).Offset + Height) / 2.0);
            end case;
            Data.Texts.all.Set
              (Location    => Location,
               Face        => Data.Face,
               Height      => Data.Height,
               Stretch     => Data.Stretch,
               Color       => Data.Color,
               Text_Angle  => Data.Angle,
               Justify_X   => Data.Justify_X,
               Justify_Y   => Data.Justify_Y,
               Superscript => Widget.all.Get_Superscript,
               Background  =>
                 Gtk.Widget.Styles.Style_Get
                   (Widget,
                    "time-text-border-color",
                    Data.Texts.all.Get_Background_Color),
               Border      =>
                 Gdouble
                   (Guint'
                        (Gtk.Widget.Styles.Style_Get
                           (Widget, "time-text-border"))),
               Overlap     =>
                 Gdouble
                   (Gint'
                        (Gtk.Widget.Styles.Style_Get
                           (Widget, "time-text-overlap"))),
               Opacity     =>
                 Gdouble'
                   (Gtk.Widget.Styles.Style_Get
                        (Widget, "time-text-border-opacity")));
         end;
      end if;
      if Data.Grid or else Data.On then
         declare -- Setting the time ticks
            Major_Line : Gtk.Layered.Line_Parameters;
            Minor_Line : Gtk.Layered.Line_Parameters;
            Y1, Y2     : Gdouble;
         begin
            case Sweeper is
               when Lower =>
                  if Data.Grid then
                     Y1 := Box.Y1;
                  else
                     Y1 := Box.Y2;
                  end if;
                  if Data.On then
                     Y2 := Box.Y2 + Height;
                  else
                     Y2 := Box.Y2;
                  end if;
               when Upper =>
                  if Data.On then
                     Y1 := Box.Y1 - Height;
                  else
                     Y1 := Box.Y1;
                  end if;
                  if Data.Grid then
                     Y2 := Box.Y2;
                  else
                     Y2 := Box.Y1;
                  end if;
            end case;
            Major_Line := Data.Ticks.all.Get_Major_Line;
            Major_Line.Color :=
              Gtk.Widget.Styles.Style_Get
                (Widget,
                 "time-major-tick-color",
                 Major_Line.Color);
            Major_Line.Width :=
              Gdouble
                (Guint'
                   (Gtk.Widget.Styles.Style_Get
                      (Widget, "time-major-tick-width")));
            Major_Line.Line_Cap :=
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "time-major-tick-cap");
            Minor_Line := Data.Ticks.all.Get_Minor_Line;
            Minor_Line.Color :=
              Gtk.Widget.Styles.Style_Get
                (Widget,
                 "time-minor-tick-color",
                 Minor_Line.Color);
            Minor_Line.Width :=
              Gdouble
                (Guint'
                   (Gtk.Widget.Styles.Style_Get
                      (Widget, "time-minor-tick-width")));
            Minor_Line.Line_Cap :=
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "time-minor-tick-cap");
            Data.Ticks.all.Set
              (Box           => (X1 => Box.X1,
                                 X2 => Box.X2,
                                 Y1 => Y1,
                                 Y2 => Y2),
               X_Tick_Length =>
                 Positive
                   (Guint'
                        (Gtk.Widget.Styles.Style_Get
                           (Widget, "time-tick-step"))),
               Y_Tick_Length => 50,
               Major_Line    => Major_Line,
               Minor_Line    => Minor_Line);
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Style_Changed_Time_Axis"));
   end Style_Changed_Time_Axis;

   procedure Style_Changed_Values_Axis
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
   is
      Box   : constant Cairo.Ellipses.Cairo_Box := Widget.all.Get_Box;
      Width : Gdouble;
      Data  : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
   begin
      if Data.On then
         Width :=
           Gdouble
             (Guint'
                (Gtk.Widget.Styles.Style_Get (Widget, "values-tick-width")));
         declare -- Setting style of the line
            Line : Gtk.Layered.Line_Parameters;
            X    : Gdouble;
         begin
            Line := Data.Line.all.Get_Line;
            Line.Color :=
              Gtk.Widget.Styles.Style_Get
                (Widget, "values-line-color", Line.Color);
            Line.Width :=
              Gdouble
                (Guint'
                   (Gtk.Widget.Styles.Style_Get (Widget, "values-line-width")));
            Line.Line_Cap :=
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "values-line-cap");
            case Amplifier is
               when Left =>
                  X := Gdouble'Floor (Box.X1) + 0.5;
               when Middle =>
                  X := Gdouble'Rounding ((Box.X1 + Box.X2) / 2.0) + 0.5;
               when Right =>
                  X := Gdouble'Floor (Box.X2) + 0.5;
            end case;
            Data.Line.all.Set
              (From => (X, Box.Y1),
               To   => (X, Box.Y2),
               Line => Line);
         end;
         declare -- Setting the time annotation
            Location : Gtk.Layered.Graph_Paper_Annotation.Axis_Location :=
                         Data.Texts.all.Get_Location;
         begin
            Location.Top := Box.Y1;
            Location.Bottom := Box.Y2;
            case Amplifier is
               when Left =>
                  Location.X_Position := Box.X1 - Width;
               when Middle =>
                  Location.X_Position :=
                    (Box.X1 + Box.X2) / 2.0 + Width;
               when Right =>
                  Location.X_Position := Box.X2 + Width;
            end case;
            Data.Texts.all.Set
              (Location    => Location,
               Face        => Data.Face,
               Height      => Data.Height,
               Stretch     => Data.Stretch,
               Color       => Data.Color,
               Text_Angle  => Data.Angle,
               Justify_X   => Data.Justify_X,
               Justify_Y   => Data.Justify_Y,
               Superscript => Widget.all.Superscript,
               Background  =>
                 Gtk.Widget.Styles.Style_Get
                   (Widget,
                    "values-text-border-color",
                    Data.Texts.all.Get_Background_Color),
               Border      =>
                 Gdouble
                   (Guint'
                        (Gtk.Widget.Styles.Style_Get
                           (Widget, "values-text-border"))),
               Overlap     =>
                 Gdouble
                   (Gint'
                        (Gtk.Widget.Styles.Style_Get
                           (Widget, "values-text-overlap"))),
               Opacity     =>
                 Gdouble'
                   (Gtk.Widget.Styles.Style_Get
                        (Widget, "values-text-border-opacity")));
         end;
      end if;
      if Data.Grid or else Data.On then
         declare -- Setting the time ticks
            Major_Line : Gtk.Layered.Line_Parameters;
            Minor_Line : Gtk.Layered.Line_Parameters;
            X1, X2     : Gdouble;
         begin
            case Amplifier is
               when Left =>
                  if Data.On then
                     X1 := Box.X1 - Width;
                  else
                     X1 := Box.X1;
                  end if;
                  if Data.Grid then
                     X2 := Box.X2;
                  else
                     X2 := Box.X1;
                  end if;
               when Middle =>
                  if Data.Grid then
                     X1 := Box.X1;
                     X2 := Box.X2;
                  elsif Data.On then
                     X1 := (Box.X1 + Box.X2 - Width) / 2.0;
                     X2 := X1 + Width;
                  end if;
               when Right =>
                  if Data.Grid then
                     X1 := Box.X1;
                  else
                     X1 := Box.X2;
                  end if;
                  if Data.On then
                     X2 := Box.X2 + Width;
                  else
                     X2 := Box.X2;
                  end if;
            end case;
            Major_Line := Data.Ticks.all.Get_Major_Line;
            Major_Line.Color :=
              Gtk.Widget.Styles.Style_Get
                (Widget,
                 "values-major-tick-color",
                 Major_Line.Color);
            Major_Line.Width :=
              Gdouble
                (Guint'
                   (Gtk.Widget.Styles.Style_Get
                      (Widget, "values-major-tick-width")));
            Major_Line.Line_Cap :=
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "values-major-tick-cap");
            Minor_Line := Data.Ticks.all.Get_Minor_Line;
            Minor_Line.Color :=
              Gtk.Widget.Styles.Style_Get
                (Widget,
                 "values-minor-tick-color",
                 Minor_Line.Color);
            Minor_Line.Width :=
              Gdouble
                (Guint'
                   (Gtk.Widget.Styles.Style_Get
                      (Widget, "values-minor-tick-width")));
            Minor_Line.Line_Cap :=
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "values-minor-tick-cap");
            Data.Ticks.all.Set
              (Box           => (X1 => X1,
                                 X2 => X2,
                                 Y1 => Box.Y1,
                                 Y2 => Box.Y2),
               X_Tick_Length =>
                 Positive
                   (Guint'
                        (Gtk.Widget.Styles.Style_Get
                           (Widget, "values-tick-width"))),
               Y_Tick_Length => 50,
               Major_Line    => Major_Line,
               Minor_Line    => Minor_Line);
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Style_Changed_Values_Axis"));
   end Style_Changed_Values_Axis;

   procedure Undo
     (Widget : not null access Gtk_Oscilloscope_Record;
      Till   : UTF8_String := "";
      Stub   : UTF8_String := "")
   is
      This  : Do_Item_Ptr := Widget.all.Undo_Stack.Actions;
      First : Boolean := True;
      Done  : Boolean := False;
   begin
      if Till'Length > 0 and then Stub'Length > 0 then
         Push_Stub (Stub, Widget.all.Undo_Stack'Access, First);
      end if;
      while This /= null and then not Done loop
         Widget.all.Undo_Stack.Actions := This.all.Next;
         if Till'Length > 0 then
            if This.all in Do_Stub'Class then
               Done :=
                 Strings_Edit.UTF8.Wildcards.Case_Insensitive.Match_Insensitive
                   (Do_Stub'Class (This.all).Name, Till, True);
            else
               Done := False; -- Never stop
            end if;
         else
            Done := This.all.First; -- Sequence beginning stop
         end if;
         This.all.Do_It (First, Widget.all, Widget.all.Redo_Stack'Access);
         Free (This);
         This := Widget.all.Undo_Stack.Actions;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Undo"));
   end Undo;

   procedure Update_Amplifier
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Amplifier_Type)
   is
      use type Gtk.Scale.Gtk_Scale;
      Data  : Values_Axis_Data renames Widget.all.Values_Axis (Amplifier);
      Group : constant Group_Count := Data.Group;
   begin
      if Group > 0 then
         if
           Widget.all.Groups.all (Group).Amplifier.all.Get_Auto_Scaling or else
           Data.No_Scale
         then
            Gtk.Handlers.References.Set (Data.Settings_Changed);
            Gtk.Handlers.References.Set (Data.Value_Changed);
            if Data.Scale /= null then
               Widget.all.Remove (Data.Box);
               Data.Scale      := null;
               Data.Scale      := null;
               Data.Box        := null;
               Data.Upper_Fill := null;
               Data.Lower_Fill := null;
            end if;
         else
            if
              Data.Scale = null and then
              not Widget.all.Selection.all.Engaged and then
              Amplifier /= Middle
            then
               Gtk.Box.Gtk_New_Vbox (Data.Box);
               Data.Box.all.Set_Spacing (0);
               Gtk.Fixed.Gtk_New (Data.Upper_Fill);
               Data.Upper_Fill.all.Set_App_Paintable (True);
               Data.Box.all.Pack_Start (Data.Upper_Fill, False, False);
               Data.Upper_Fill.all.Set_Size_Request
                 (Width  => 1,
                  Height => Widget.all.Get_Annotation_Height (Upper));
               Gtk.Scale.Gtk_New_Vscale
                 (Data.Scale,
                  Widget.all.Groups.all (Group).Amplifier.all'Unchecked_Access);
               Data.Box.all.Pack_Start (Data.Scale);
               Data.Scale.all.Set_Hexpand (False);
               Data.Scale.all.Set_Vexpand (True);
               Data.Scale.all.Set_Draw_Value (False);
               Gtk.Fixed.Gtk_New (Data.Lower_Fill);
               Data.Lower_Fill.all.Set_App_Paintable (True);
               Data.Box.all.Pack_Start (Data.Lower_Fill, False, False);
               Data.Lower_Fill.all.Set_Size_Request
                 (Width  => 1,
                  Height => Widget.all.Get_Annotation_Height (Lower));
               case Amplifier is
                  when Left =>
                     Data.Scale.all.Set_Value_Pos (Gtk.Enums.Pos_Left);
                     Widget.all.Attach_Next_To
                       (Data.Box,
                        Widget.all.Layers,
                        Gtk.Enums.Pos_Left);
                  when Right =>
                     Data.Scale.all.Set_Value_Pos (Gtk.Enums.Pos_Right);
                     Widget.all.Attach_Next_To
                       (Data.Box,
                        Widget.all.Layers,
                        Gtk.Enums.Pos_Right);
                  when others =>
                     null;
               end case;
               Data.Scale.all.Show_All;
            end if;
         end if;
      end if;
   end Update_Amplifier;

   procedure Update_Value (Widget : not null access Gtk_Oscilloscope_Record)
   is
      use type Ada.Calendar.Time;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter :=
                 Widget.all.Channel_Names.all.Get_Iter_First;
      T1, T2 : Ada.Calendar.Time;
   begin
      for Index in 1 .. Widget.all.Channels_Number loop
         declare
            use type Gtk.Layered.Waveform.Y_Axis;
            Data : Channel_Data renames Widget.all.Channels (Index);
         begin
            case Data.Status is
               when Undefined =>
                  Widget.all.Channel_Names.all.Set (Row, 5, "");
               when Absolute =>
                  Widget.all.Channel_Names.all.Set
                    (Row,
                     5,
                     Gtk.Layered.Waveform.Edit.Image (Gdouble (Data.Value_1), RelSmall => 6));
               when Difference =>
                  Widget.all.Channel_Names.all.Set
                    (Row,
                     5,
                     Gtk.Layered.Waveform.Edit.Image
                       (Gdouble (Data.Value_1 - Data.Value_2),
                        RelSmall => 6));
            end case;
         exception
            when others =>
               Widget.all.Channel_Names.all.Set (Row, 5, "");
         end;
         Widget.all.Channel_Names.all.Next (Row);
      end loop;
      for Sweeper in Widget.all.Time_Axis'Range loop
         T1 :=
           Get_Time
             (Widget,
              Sweeper,
              Gint (Widget.all.Selection.all.Area.all.Get_Box.X1));
         T2 :=
           Get_Time
             (Widget,
              Sweeper,
              Gint (Widget.all.Selection.all.Area.all.Get_Box.X2));
         Emit
           (Widget,
            Signal_IDs (11),
            Sweeper_Type'Pos (Sweeper),
            Gtk.Layered.Waveform.To_Double (T1),
            Gdouble (T2 - T1));
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Update_Value"));
   end Update_Value;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Oscilloscope." & Name;
   end Where;

   procedure Zoom_In
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      T1, T2  : Ada.Calendar.Time)
   is
      use type Ada.Calendar.Time;
      -- T : Ada.Calendar.Time := Widget.Get_Time (Sweeper);
   begin
      Widget.all.Set_Page_Span (Sweeper, T2 - T1);
      Widget.all.Set_Time (Sweeper, T2);
   end Zoom_In;

   procedure Zoom_In
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      V1, V2    : Gdouble)
   is
      pragma Unreferenced (Widget);
   begin
      Amplifier.all.Set_Page_Size (V2 - V1);
      Amplifier.all.Set_Value (V1);
   end Zoom_In;
   --
   -- Zooming out:                    L' = a L + b      L = a X1 + b
   --                                 U' = a U + b      U = a X2 + b
   --       L    X1     X2    U
   --       |<---|///////|--->|       a = (U - L) / (X2 - X1)
   --       |\               /|       b = U - X2 (U - L) / (X2 - X1) =
   --       | \             / |         = (L X2 - U X1) / (X2 - X1)
   --       |  \           /  |
   --       |   \         /   |       U' - L' = a (U - L)
   --       |<---|-------|--->|
   --       L'   L       U    U'
   --
   procedure Zoom_Out
     (Widget  : not null access Gtk_Oscilloscope_Record;
      Sweeper : Sweeper_Type;
      T1, T2  : Ada.Calendar.Time)
   is
      use type Ada.Calendar.Time;
      Page : constant Gdouble :=
               Gdouble (Widget.all.Get_Page_Span (Sweeper));
      T    : constant Gdouble :=
               Gtk.Layered.Waveform.To_Double
                 (Ada.Calendar.Time'(Get_Time (Widget, Sweeper)));
      A    : constant Gdouble := Page / Gdouble (T2 - T1);
      B    : constant Gdouble := T - A * Gtk.Layered.Waveform.To_Double (T2);
   begin
      Widget.all.Set_Page_Span (Sweeper, Duration (Page * A));
      Widget.all.Set_Time
        (Sweeper,
         Ada.Real_Time.Time'(Gtk.Layered.Waveform.To_Time (A * T + B)));
   end Zoom_Out;

   procedure Zoom_Out
     (Widget    : not null access Gtk_Oscilloscope_Record;
      Amplifier : Gtk.Layered.Waveform.Amplifier.Gtk_Waveform_Amplifier;
      V1, V2    : Gdouble)
   is
      pragma Unreferenced (Widget);
      Page  : constant Gdouble := Amplifier.all.Get_Page_Size;
      Value : constant Gdouble := Amplifier.all.Get_Value;
      A     : constant Gdouble := Page / (V2 - V1);
      B     : constant Gdouble := Value - A * V2;
   begin
      Amplifier.all.Set_Page_Size (Page * A);
      Amplifier.all.Set_Value (A * Value + B);
   end Zoom_Out;

   pragma Warnings (On, "declaration hides ""Adjustment""");
   pragma Warnings (On, "declaration hides ""Amplifier""");
   pragma Warnings (On, "declaration hides ""Box""");
   pragma Warnings (On, "declaration hides ""Group""");
   pragma Warnings (On, "declaration hides ""Left""");
   pragma Warnings (On, "declaration hides ""Lower""");
   pragma Warnings (On, "declaration hides ""Menu""");
   pragma Warnings (On, "declaration hides ""Oscilloscope""");
   pragma Warnings (On, "declaration hides ""Params""");
   pragma Warnings (On, "declaration hides ""Refresh_Period""");
   pragma Warnings (On, "declaration hides ""Right""");
   pragma Warnings (On, "declaration hides ""Scale""");
   pragma Warnings (On, "declaration hides ""Sweeper""");
   pragma Warnings (On, "declaration hides ""Value""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Oscilloscope;
