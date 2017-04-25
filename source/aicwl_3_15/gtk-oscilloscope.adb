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

-- with Ada.Numerics;                use Ada.Numerics;
with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
-- with Cairo.Font_Face;             use Cairo.Font_Face;
with Cairo.Line_Cap_Property;     use Cairo.Line_Cap_Property;
-- with Cairo.Region;                use Cairo.Region;
with Gdk.Color.IHLS;              use Gdk.Color.IHLS;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Window;                  use Gdk.Window;
-- with Glib.Error;                  use Glib.Error;
with Glib.Messages;               use Glib.Messages;
with Glib.Properties.Creation;    use Glib.Properties.Creation;
with Gtkada.Types;                use Gtkada.Types;
-- with Gtk.Dialog;                  use Gtk.Dialog;
-- with Gtk.Message_Dialog;          use Gtk.Message_Dialog;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Image;                   use Gtk.Image;
with Gtk.Separator_Menu_Item;     use Gtk.Separator_Menu_Item;
with Gtk.Stock;                   use Gtk.Stock;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Widget.Styles;           use Gtk.Widget.Styles;
-- with Gtk.Window;                  use Gtk.Window;
with Strings_Edit.Integers;       use Strings_Edit.Integers;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Cairo.PDF;
-- with Cairo.Png;
with Cairo.SVG;
-- with Gdk.Device_Manager;
with Glib.Object.Checked_Destroy;
with Gtk.Widget.Styles.Line_Cap_Property;
with Interfaces.C.Strings;
with Strings_Edit.UTF8.Wildcards.Case_Insensitive;
-- with Strings_Edit.Integers.Superscript;

package body Gtk.Oscilloscope is
   use Gtk.Widget.Styles.Line_Cap_Property;

   function "+" (Value : String_Ptr) return UTF8_String;
   function "+" (Value : String_Ptr) return UTF8_String is
   begin
      if Value = null then
         return "";
      else
         return Value.all;
      end if;
   end "+";

   function Where (Name : String) return String;
   function Where (Name : String) return String is
   begin
      return " in Gtk.Oscilloscope." & Name;
   end Where;

   Cycle            : constant := 6;
   First_Color      : constant Gdk_IHLS_Color :=
                                  To_IHLS (RGB (1.0, 0.0, 0.0));
   -- Corner           : constant := 1.0 / 30.0;
   Selection_Color  : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   -- Background_Color : constant Gdk_Color := RGB (1.0, 1.0, 1.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;
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
   Signal_IDs : array (Signal_Names'Range) of Signal_Id :=
                   (others => Invalid_Signal_Id);

--     procedure Dump_Stack (Head : String; Stack : Items_Stack) is
--        use Ada.Tags, Ada.Text_IO;
--        This : Do_Item_Ptr := Stack.Actions;
--     begin
--        Put_Line (Head & " ---------------->");
--        while This /= null loop
--           if This.First then
--              Put_Line ("   = " & Expanded_Name (This.all'Tag));
--           else
--              Put_Line ("     " & Expanded_Name (This.all'Tag));
--           end if;
--           This := This.Next;
--        end loop;
--        Put_Line ("<--------------- " & Head);
--     end Dump_Stack;

   procedure Free is
      new Ada.Unchecked_Deallocation (Line_Layer, Line_Layer_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Graph_Paper_Layer, Graph_Paper_Layer_Ptr);
   procedure Free is new Ada.Unchecked_Deallocation
     (Gtk.Layered.Graph_Paper_Annotation.Graph_Paper_Annotation_Layer'Class,
      Graph_Paper_Annotation_Layer_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Group_List, Group_List_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Do_Item'Class, Do_Item_Ptr);
   procedure Free is
     new Ada.Unchecked_Deallocation (Layered_Refresh_Engine,
                                     Layered_Refresh_Engine_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Rectangle_Layer, Rectangle_Layer_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (Selection_State, Selection_State_Ptr);

   procedure Do_Init
             (Widget         : not null access Gtk_Oscilloscope_Record'Class;
              Lower_Sweeper  : access Gtk_Waveform_Sweeper_Record'Class;
              Upper_Sweeper  : access Gtk_Waveform_Sweeper_Record'Class;
              Refresh_Engine : access Layered_Refresh_Engine;
              Refresh_Period : Duration;
              Background     : Gdk_Color;
              Buffer_Size    : Positive) is separate;
   function On_Button_Press
     (Object       : access GObject_Record'Class;
      Event        : Gdk_Event;
      Oscilloscope : Gtk_Oscilloscope) return Boolean is separate;

   function On_Button_Release
            (  Object       : access GObject_Record'Class;
               Event        : Gdk_Event;
               Oscilloscope : Gtk_Oscilloscope
            )  return Boolean is separate;

   procedure EmitV
             (  Params : System.Address;
                Signal : Signal_Id;
                Quark  : GQuark;
                Result : System.Address
             );
   pragma Import (C, EmitV, "g_signal_emitv");

   procedure Emit
             (  Widget : not null access
                         Gtk_Oscilloscope_Record'Class;
                Signal : Signal_Id;
                Value  : Guint
             )  is
      procedure Set_Object
                (  Value  : in out GValue;
                   Object : System.Address
                );
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : GValue_Array (0..1);
      Result : GValue;
   begin
      if Class_Record /= Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Init (Params (0), This);
            Set_Object
            (  Params (0),
               Gtk.Widget.Convert (Widget.all'Unchecked_Access)
            );
            Init (Params (1), GType_Uint);
            Set_Uint (Params (1), Value);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Unset (Params (0));
            Unset (Params (1));
         end;
      end if;
   end Emit;

   procedure Emit
             (  Widget  : not null access
                          Gtk_Oscilloscope_Record'Class;
                Signal  : Signal_Id;
                Value_1 : Guint;
                Value_2 : Gdouble;
                Value_3 : Gdouble
             )  is
      procedure Set_Object
                (  Value  : in out GValue;
                   Object : System.Address
                );
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : GValue_Array (0..3);
      Result : GValue;
   begin
      if Class_Record /= Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Init (Params (0), This);
            Set_Object
            (  Params (0),
               Gtk.Widget.Convert (Widget.all'Unchecked_Access)
            );
            Init (Params (1), GType_Uint);
            Set_Uint (Params (1), Value_1);
            Init (Params (2), GType_Double);
            Set_Double (Params (2), Value_2);
            Init (Params (3), GType_Double);
            Set_Double (Params (3), Value_3);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Unset (Params (0));
            Unset (Params (1));
            Unset (Params (2));
            Unset (Params (3));
         end;
      end if;
   end Emit;

   procedure Emit
             (  Widget : not null access
                         Gtk_Oscilloscope_Record'Class;
                Signal : Signal_Id;
                Value  : String
             )  is
      procedure Set_Object
                (  Value  : in out GValue;
                   Object : System.Address
                );
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : GValue_Array (0..1);
      Result : GValue;
   begin
      if Class_Record /= Glib.Object.Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Init (Params (0), This);
            Set_Object
            (  Params (0),
               Gtk.Widget.Convert (Widget.all'Unchecked_Access)
            );
            Init (Params (1), GType_String);
            Set_String (Params (1), Value);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Unset (Params (0));
            Unset (Params (1));
         end;
      end if;
   end Emit;

   function Add_Channel
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Group   : Group_Number;
               Color   : Gdk_Color;
               Mode    : Interpolation_Mode := Linear;
               Left    : Boolean := False;
               Right   : Boolean := False;
               Name    : String  := "";
               Sweeper : Sweeper_Type := Lower;
               Buffer  : access
                  Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null
            )  return Channel_Number is
   begin
      if Widget.all.Channels_Number >= Widget.Size then
         raise Constraint_Error with
               (  "More than"
               &  Channel_Count'Image (Widget.Size)
               &  " channels"
               );
      end if;
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      end if;
      return Index : constant Channel_Number :=
                     Widget.Channels_Number + 1 do
         declare
            This : Channel_Data renames Widget.Channels (Index);
            Row  : Gtk_Tree_Iter;
         begin
            This.Waveform :=
               Add_Waveform
               (  Under     => Widget.Layers,
                  Box       => Widget.Get_Box,
                  Width     => Widget.Width,
                  Color     => Color,
                  Line_Cap  => Widget.Line_Cap,
                  Sweeper   => Widget.Time_Axis (Sweeper).Sweeper,
                  Amplifier => Widget.Groups (Group).Amplifier,
                  Mode      => Mode,
                  Left      => Left,
                  Right     => Right,
                  Opacity   => Widget.Opacity,
                  Scaled    => False,
                  Widened   => Widget.Widened
               ) .all'Unchecked_Access;
            Widget.Channel_Names.Append (Row);
            if Name'Length = 0 then
               Widget.Channel_Names.Set
               (  Row,
                  0,
                  "Channel " & Channel_Number'Image (Index)
               );
            else
               Widget.Channel_Names.Set (Row, 0, Name);
            end if;
            Widget.Channel_Names.Set (Row, 1, Gint (Index));
            Widget.Channel_Names.Set (Row, 2, Gint (Group));
            Widget.Channel_Names.Set (Row, 3, True);
            Widget.Channel_Names.Set
               (Row, 4, Interpolation_Mode'Pos (Mode));
            Widget.Channel_Names.Set (Row, 6, Left);
            Widget.Channel_Names.Set (Row, 7, Right);
            This.Group := Group;
            if Buffer = null then
               Gtk_New (This.Source, Widget.Buffer_Size);
            else
               This.Source := Buffer.all'Unchecked_Access;
               This.Source.Ref;
            end if;
            This.Waveform.Set_Source (This.Source.all);
            Widget.Time_Axis (Sweeper).Channels :=
               Widget.Time_Axis (Sweeper).Channels + 1;
         end;
         Widget.Channels_Number := Index;
         Emit (Widget, Signal_IDs (12), Guint (Index));
      end return;
   end Add_Channel;

   function Add_Channel
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Group   : Group_Number;
               Mode    : Interpolation_Mode := Linear;
               Left    : Boolean := False;
               Right   : Boolean := False;
               Name    : String  := "";
               Sweeper : Sweeper_Type := Lower;
               Buffer  : access
                  Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null
            )  return Channel_Number is
   begin
      return
         Add_Channel
         (  Widget  => Widget,
            Group   => Group,
            Mode    => Mode,
            Left    => Left,
            Right   => Right,
            Name    => Name,
            Sweeper => Sweeper,
            Buffer  => Buffer,
            Color   => To_RGB
                       (  Val
                          (  First_Color,
                             Natural (Widget.Channels_Number),
                             Cycle
         )             )  );
   end Add_Channel;

   function Add_Channel
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Color   : Gdk_Color;
               Mode    : Interpolation_Mode := Linear;
               Left    : Boolean := False;
               Right   : Boolean := False;
               Name    : String  := "";
               Sweeper : Sweeper_Type := Lower;
               Buffer  : access
                  Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null
            )  return Channel_Number is
   begin
      return
         Add_Channel
         (  Widget  => Widget,
            Group   => Add_Group (Widget),
            Color   => Color,
            Mode    => Mode,
            Left    => Left,
            Right   => Right,
            Name    => Name,
            Buffer  => Buffer,
            Sweeper => Sweeper
         );
   end Add_Channel;

   function Add_Channel
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Mode    : Interpolation_Mode := Linear;
               Left    : Boolean := False;
               Right   : Boolean := False;
               Name    : String  := "";
               Sweeper : Sweeper_Type := Lower;
               Buffer  : access
                  Gtk_Wavefrom_Ring_Data_Buffer_Record'Class := null
            )  return Channel_Number is
   begin
      return
         Add_Channel
         (  Widget  => Widget,
            Group   => Add_Group (Widget),
            Mode    => Mode,
            Left    => Left,
            Right   => Right,
            Name    => Name,
            Sweeper => Sweeper,
            Buffer  => Buffer,
            Color   => To_RGB
                       (  Val
                          (  First_Color,
                             Natural (Widget.Channels_Number),
                             Cycle
         )             )  );
   end Add_Channel;

   function Add_Deviation_Channel
            (  Widget   : not null access Gtk_Oscilloscope_Record;
               Group    : Group_Number;
               Color    : Gdk_Color;
               Measured : Drawing_Measurement_Point := Refresh_Period;
               Name     : String := "";
               Sweeper  : Sweeper_Type := Lower
            )  return Channel_Number is
   begin
      if Widget.Channels_Number >= Widget.Size then
         raise Constraint_Error with
               (  "More than"
               &  Channel_Count'Image (Widget.Size)
               &  " channels"
               );
      end if;
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      end if;
      return Index : constant Channel_Number :=
                     Widget.Channels_Number + 1 do
         declare
            This : Channel_Data renames Widget.Channels (Index);
            Row  : Gtk_Tree_Iter;
         begin
            This.Waveform :=
               Add_Waveform
               (  Under     => Widget.Layers,
                  Box       => Widget.Get_Box,
                  Width     => Widget.Width,
                  Color     => Color,
                  Line_Cap  => Widget.Line_Cap,
                  Sweeper   => Widget.Time_Axis (Sweeper).Sweeper,
                  Amplifier => Widget.Groups (Group).Amplifier,
                  Mode      => Left,
                  Opacity   => Widget.Opacity,
                  Scaled    => False,
                  Widened   => Widget.Widened
               ) .all'Unchecked_Access;
            Widget.Channel_Names.Append (Row);
            if Name'Length = 0 then
               case Measured is
                  when Refresh_Period =>
                     Widget.Channel_Names.Set
                     (  Row,
                        0,
                        "Refresh period"
                     );
                  when Drawing_Time =>
                     Widget.Channel_Names.Set
                     (  Row,
                        0,
                        "Drawing time"
                     );
               end case;
            else
               Widget.Channel_Names.Set (Row, 0, Name);
            end if;
            Widget.Channel_Names.Set (Row, 1, Gint (Index));
            Widget.Channel_Names.Set (Row, 2, Gint (Group));
            Widget.Channel_Names.Set (Row, 3, True);
            Widget.Channel_Names.Set
               (Row, 4, Interpolation_Mode'Pos (Left));
            Widget.Channel_Names.Set (Row, 6, False);
            Widget.Channel_Names.Set (Row, 7, False);
            case Measured is
               when Refresh_Period =>
                  if Widget.Refresh_Period = null then
                     Gtk_New
                     (  Widget.Refresh_Period,
                        Widget.Buffer_Size
                     );
                  end if;
                  This.Source := Widget.Refresh_Period;
               when Drawing_Time =>
                  if Widget.Drawing_Time = null then
                     Gtk_New
                     (  Widget.Drawing_Time,
                        Widget.Buffer_Size
                     );
                  end if;
                  This.Source := Widget.Drawing_Time;
            end case;
            This.Source.Ref;
            This.Group := Group;
            This.Waveform.Set_Source (This.Source.all);
         end;
         Widget.Channels_Number := Index;
         Emit (Widget, Signal_IDs (12), Guint (Index));
      end return;
   end Add_Deviation_Channel;

   function Add_Deviation_Channel
            (  Widget   : not null access Gtk_Oscilloscope_Record;
               Group    : Group_Number;
               Measured : Drawing_Measurement_Point := Refresh_Period;
               Name     : String := "";
               Sweeper  : Sweeper_Type := Lower
            )  return Channel_Number is
   begin
      return
         Add_Deviation_Channel
         (  Widget   => Widget,
            Group    => Group,
            Measured => Measured,
            Name     => Name,
            Sweeper  => Sweeper,
            Color    => To_RGB
                        (  Val
                           (  First_Color,
                              Natural (Widget.Channels_Number),
                              Cycle
         )              )  );
   end Add_Deviation_Channel;

   function Add_Deviation_Channel
            (  Widget   : not null access Gtk_Oscilloscope_Record;
               Color    : Gdk_Color;
               Measured : Drawing_Measurement_Point := Refresh_Period;
               Name     : String := "";
               Sweeper  : Sweeper_Type := Lower
            )  return Channel_Number is
   begin
      return
         Add_Deviation_Channel
         (  Widget   => Widget,
            Group    => Add_Group (Widget),
            Measured => Measured,
            Color    => Color,
            Name     => Name,
            Sweeper  => Sweeper
         );
   end Add_Deviation_Channel;

   function Add_Deviation_Channel
            (  Widget   : not null access Gtk_Oscilloscope_Record;
               Measured : Drawing_Measurement_Point := Refresh_Period;
               Name     : String := "";
               Sweeper  : Sweeper_Type := Lower
            )  return Channel_Number is
   begin
      return
         Add_Deviation_Channel
         (  Widget   => Widget,
            Group    => Add_Group (Widget),
            Measured => Measured,
            Sweeper  => Sweeper,
            Name     => Name,
            Color    => To_RGB
                        (  Val
                           (  First_Color,
                              Natural (Widget.Channels_Number),
                              Cycle
         )              )  );
   end Add_Deviation_Channel;

   function Add_Group
            (  Widget    : not null access Gtk_Oscilloscope_Record;
               Name      : String := "";
               Amplifier : Gtk_Waveform_Amplifier := null
            )  return Group_Number is
      use Amplifier_Handlers;
      Row : Gtk_Tree_Iter;
   begin
      if Widget.Groups_Number >= Widget.Groups'Last then
         raise Constraint_Error with
               (  "More than"
               &  Channel_Count'Image (Widget.Size)
               &  " groups"
               );
      end if;
      return Index : constant Group_Number :=
                     Widget.Groups_Number + 1 do
         if Amplifier = null then
            Gtk_New (Widget.Groups (Index).Amplifier);
         else
            Widget.Groups (Index).Amplifier := Amplifier;
         end if;
         Connect
         (  Widget.Groups (Index).Amplifier,
            "autoscaling-changed",
            On_Autoscaling_Changed'Access,
            Widget.all'Unchecked_Access
         );
         Connect
         (  Widget.Groups (Index).Amplifier,
            "raster-mode-changed",
            On_Raster_Mode_Changed'Access,
            Widget.all'Unchecked_Access
         );
         Widget.Groups (Index).Amplifier.Ref;
         Widget.Group_Names.Append (Row);
         if Name'Length = 0 then
            Widget.Group_Names.Set
            (  Row,
               0,
               "Group" & Group_Count'Image (Index)
            );
         else
            Widget.Group_Names.Set (Row, 0, Name);
         end if;
         Widget.Groups_Number := Index;
      end return;
   end Add_Group;

   function Add_Shadow_Channel
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Channel : Channel_Number;
               Color   : Gdk_Color;
               Name    : String := "";
               Sweeper : Sweeper_Type := Upper
            )  return Channel_Number is
   begin
      if Widget.Channels_Number >= Widget.Size then
         raise Constraint_Error with
               (  "More than"
               &  Channel_Count'Image (Widget.Size)
               &  " channels"
               );
      elsif Channel > Widget.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      end if;
      return Index : constant Channel_Number :=
                     Widget.Channels_Number + 1 do
         declare
            That : Channel_Data renames Widget.Channels (Channel);
            This : Channel_Data renames Widget.Channels (Index);
            Row  : Gtk_Tree_Iter;
         begin
            This.Waveform :=
               Add_Waveform
               (  Under     => Widget.Layers,
                  Box       => Widget.Get_Box,
                  Width     => Widget.Width,
                  Color     => Color,
                  Line_Cap  => Widget.Line_Cap,
                  Sweeper   => Widget.Time_Axis (Sweeper).Sweeper,
                  Amplifier => Widget.Groups (That.Group).Amplifier,
                  Mode      => That.Waveform.Get_Interpolation_Mode,
                  Left      => That.Waveform.
                               Get_Left_Extrapolation_Mode,
                  Right     => That.Waveform.
                               Get_Right_Extrapolation_Mode,
                  Opacity   => Widget.Opacity,
                  Scaled    => False,
                  Widened   => Widget.Widened
               ) .all'Unchecked_Access;
            Widget.Channel_Names.Append (Row);
            if Name'Length = 0 then
               Widget.Channel_Names.Set
               (  Row,
                  0,
                  Widget.Get_Name (Channel) & " (shadow)"
               );
            else
               Widget.Channel_Names.Set (Row, 0, Name);
            end if;
            Widget.Channel_Names.Set (Row, 1, Gint (Index));
            Widget.Channel_Names.Set (Row, 2, Gint (That.Group));
            Widget.Channel_Names.Set (Row, 3, True);
            Widget.Channel_Names.Set
            (  Row,
               4,
               Interpolation_Mode'Pos
               (  That.Waveform.Get_Interpolation_Mode
            )  );
            Widget.Channel_Names.Set
            (  Row,
               6,
               That.Waveform.Get_Left_Extrapolation_Mode
            );
            Widget.Channel_Names.Set
            (  Row,
               7,
               That.Waveform.Get_Right_Extrapolation_Mode
            );
            This.Group  := That.Group;
            This.Source := That.Source;
            This.Source.Ref;
            This.Waveform.Set_Source (This.Source.all);
            Widget.Time_Axis (Sweeper).Channels :=
               Widget.Time_Axis (Sweeper).Channels + 1;
         end;
         Widget.Channels_Number := Index;
         Emit (Widget, Signal_IDs (12), Guint (Index));
      end return;
   end Add_Shadow_Channel;

   function Add_Shadow_Channel
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Channel : Channel_Number;
               Name    : String := "";
               Sweeper : Sweeper_Type := Upper
            )  return Channel_Number is
   begin
      return
         Add_Shadow_Channel
         (  Widget  => Widget,
            Channel => Channel,
            Sweeper => Sweeper,
            Name    => Name,
            Color   => To_RGB
                       (  Val
                          (  First_Color,
                             Natural (Widget.Channels_Number),
                             Cycle
         )             )  );
   end Add_Shadow_Channel;

   procedure Box_Changed
             (  Widget : not null access Gtk_Oscilloscope_Record'Class
             )  is
      Box : constant Cairo_Box := Widget.Get_Box;
   begin
      Widget.Background.Set
      (  Box        => Box,
         Color      => Widget.Background.Get_Color,
         Line_Width => 0.0,
         Opacity    => Widget.Background.Get_Opacity
      );
      for Index in 1..Widget.Channels_Number loop
         declare
            This : Waveform_Layer renames
                   Widget.Channels (Index).Waveform.all;
         begin
            This.Set
            (  Box     => Box,
               Line    => This.Get_Line,
               Mode    => This.Get_Interpolation_Mode,
               Left    => This.Get_Left_Extrapolation_Mode,
               Right   => This.Get_Right_Extrapolation_Mode,
               Opacity => This.Get_Opacity
            );
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
             (  Widget : not null access Gtk_Oscilloscope_Record;
                File   : UTF8_String
             )  is
      Surface : Cairo_Surface;
   begin
      Surface :=
         Cairo.PDF.Create
         (  Filename         => File,
            Width_In_Points  => Gdouble (Widget.Get_Allocated_Width),
            Height_In_Points => Gdouble (Widget.Get_Allocated_Height)
         );
      Widget.Layers.Snapshot (Surface);
      Surface_Destroy (Surface);
   exception
      when others =>
         Surface_Destroy (Surface);
         raise;
   end Capture_PDF;

   procedure Capture_SVG
             (  Widget : not null access Gtk_Oscilloscope_Record;
                File   : UTF8_String
             )  is
      Surface : Cairo_Surface;
   begin
      Surface :=
         Cairo.SVG.Create
         (  Filename        => File,
            Width_In_Point  => Gdouble (Widget.Get_Allocated_Width),
            Height_In_Point => Gdouble (Widget.Get_Allocated_Height)
         );
      Widget.Layers.Snapshot (Surface);
      Surface_Destroy (Surface);
   exception
      when others =>
         Surface_Destroy (Surface);
         raise;
   end Capture_SVG;

   procedure Change_Selection
             (  Oscilloscope : not null access Gtk_Oscilloscope_Record;
                Point        : Cairo_Tuple
             )  is
      Selected : Cairo_Box := Oscilloscope.Selection.Area.Get_Box;
      Area     : constant Cairo_Box := Oscilloscope.Get_Box;
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
         Oscilloscope.Selection.Right := False;
      elsif X > Selected.X2 then
         Selected.X2 := X;
         Oscilloscope.Selection.Right := True;
      else
         if Oscilloscope.Selection.Right then
            Selected.X2 := X;
         else
            Selected.X1 := X;
         end if;
      end if;
      if Y < Selected.Y1 then
         Selected.Y1 := Y;
         Oscilloscope.Selection.Below := False;
      elsif Y > Selected.Y2 then
         Selected.Y2 := Y;
         Oscilloscope.Selection.Below := True;
      else
         if Oscilloscope.Selection.Below then
            Selected.Y2 := Y;
         else
            Selected.Y1 := Y;
         end if;
      end if;
      Oscilloscope.Selection.Area.Set
      (  Box        => Selected,
         Line_Width => Oscilloscope.Selection.Area.Get_Line_Width,
         Opacity    => Oscilloscope.Selection.Area.Get_Opacity,
         Color      => Oscilloscope.Selection.Area.Get_Color
      );
   end Change_Selection;

   function Create_Annotation
            (  Widget    : not null access Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return not null access
                      Gtk.Layered.Graph_Paper_Annotation.
                      Graph_Paper_Annotation_Layer'Class is
      use Gtk.Layered.Graph_Paper_Annotation;
      Data  : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
      Layer : Graph_Paper_Annotation_Layer renames
              Add_Graph_Paper_Annotation
              (  Under       => Data.Line.Atop,
                 Paper       => Data.Ticks,
                 Face        => Data.Face,
                 Color       => Data.Color,
                 Height      => Data.Height,
                 Stretch     => Data.Stretch,
                 Text_Angle  => Data.Angle,
                 Justify_X   => Data.Justify_X,
                 Justify_Y   => Data.Justify_Y,
                 Superscript => Widget.Superscript,
                 Background =>
                    Style_Get
                    (  Widget,
                       "values-text-border-color",
                       RGB (1.0, 1.0, 1.0)
                    ),
                 Border =>
                    Gdouble
                    (  Guint'(Style_Get (Widget, "values-text-border"))
                    ),
                 Overlap =>
                    Gdouble
                    (  Gint'(Style_Get (Widget, "values-text-overlap"))
                    ),
                 Opacity =>
                    Gdouble'
                    (  Style_Get
                       (  Widget,
                          "values-text-border-opacity"
              )     )  ) .all;
   begin
      return Layer'Unchecked_Access;
   end Create_Annotation;

   function Create_Annotation
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return not null access
                      Gtk.Layered.Graph_Paper_Annotation.
                      Graph_Paper_Annotation_Layer'Class is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      if Data.Time_Mode then
         declare
            Layer : Graph_Paper_Time_Annotation_Layer renames
               Add_Graph_Paper_Time_Annotation
               (  Under      => Data.Line.Atop,
                  Paper      => Data.Ticks,
                  Face       => Data.Face,
                  Color      => Data.Color,
                  Height     => Data.Height,
                  Stretch    => Data.Stretch,
                  Text_Angle => Data.Angle,
                  Justify_X  => Data.Justify_X,
                  Justify_Y  => Data.Justify_Y,
                  Background =>
                     Style_Get
                     (  Widget,
                        "time-text-border-color",
                        RGB (1.0, 1.0, 1.0)
                     ),
                  Border =>
                     Gdouble
                     (  Guint'(Style_Get (Widget, "time-text-border"))
                     ),
                  Overlap =>
                     Gdouble
                     (  Gint'(Style_Get (Widget, "time-text-overlap"))
                     ),
                  Opacity =>
                     Gdouble'
                     (  Style_Get
                        (  Widget,
                           "time-text-border-opacity"
               )     )  ) .all;
         begin
            return Layer'Unchecked_Access;
         end;
      else
         declare
            Layer : Graph_Paper_Annotation_Layer renames
               Add_Graph_Paper_Annotation
               (  Under      => Data.Line.Atop,
                   Paper      => Data.Ticks,
                   Location   => (  Orientation => Horizontal,
                                    Alignment   => Absolute,
                                    Left        => -0.5,
                                    Right       => -0.5,
                                    Y_Position  =>  0.0
                                 ),
                   Face       => Data.Face,
                   Color      => Data.Color,
                   Height     => Data.Height,
                   Stretch    => Data.Stretch,
                   Text_Angle => Data.Angle,
                   Justify_X  => Data.Justify_X,
                   Justify_Y  => Data.Justify_Y,
                   Background => Style_Get
                                 (  Widget,
                                    "time-text-border-color",
                                    RGB (1.0, 1.0, 1.0)
                                 ),
                   Border =>
                      Gdouble
                      (  Guint'(Style_Get (Widget, "time-text-border"))
                      ),
                   Overlap =>
                      Gdouble
                      (  Gint'(Style_Get (Widget, "time-text-overlap"))
                      ),
                   Opacity =>
                      Gdouble'
                      (  Style_Get
                         (  Widget,
                            "time-text-border-opacity"
               )      )  ) .all;
         begin
            return Layer'Unchecked_Access;
         end;
      end if;
   end Create_Annotation;

   procedure Delete_Channel
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Waveform_Layer,
                Waveform_Layer_Ptr
             );
      This   : Channel_Data;
      Remove : Boolean := False;
      Row    : Gtk_Tree_Iter;
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      This := Widget.Channels (Channel);
      Free (This.Tip_Prefix);
      Free (This.Tip_X_Suffix);
      Free (This.Tip_Y_Suffix);
      for Index in Channel..Widget.Channels_Number - 1 loop
         Widget.Channels (Index) := Widget.Channels (Index + 1);
      end loop;
      if Channel < Widget.Channels_Number then
         Widget.Channel_Names.Move_After
         (  Iter =>
               Widget.Channel_Names.Nth_Child
               (  Null_Iter,
                  Gint (Channel) - 1
               ),
            Position =>
               Widget.Channel_Names.Nth_Child
               (  Null_Iter,
                  Gint (Widget.Channels_Number) - 1
         )     );
         Widget.Fix_Numbers (Channel, Widget.Channels_Number - 1);
      end if;
      Widget.Channels (Widget.Channels_Number).Tip_Prefix   := null;
      Widget.Channels (Widget.Channels_Number).Tip_X_Suffix := null;
      Widget.Channels (Widget.Channels_Number).Tip_Y_Suffix := null;
      Widget.Channels_Number := Widget.Channels_Number - 1;
      Row :=
         Widget.Channel_Names.Nth_Child
         (  Null_Iter,
            Gint (Widget.Channels_Number)
         );
      Widget.Channel_Names.Remove (Row);
      for Index in Widget.Time_Axis'Range loop
         declare
            Data : Time_Axis_Data renames Widget.Time_Axis (Index);
         begin
            if (  Data.Sweeper /= null
               and then
                  Data.Sweeper.all'Unchecked_Access =
                  This.Waveform.Get_Sweeper
               )
            then
               Data.Channels := Data.Channels - 1;
               exit;
            end if;
         end;
      end loop;
      Free (This.Waveform);
      This.Source.Unref;
      Emit (Widget, Signal_IDs (13), Guint (Channel));
   end Delete_Channel;

   procedure Do_It
             (  Item         : Do_Auto_Amplifier;
                First        : in out Boolean;
                Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                Inverse      : access Items_Stack := null
             )  is
   begin
      if not Item.Amplifier.Get_Auto_Scaling then
         Push_Amplifier_Zoom (Item.Amplifier, Inverse, First);
         Item.Amplifier.Set_Auto_Scaling (True);
      end if;
   end Do_It;

   procedure Do_It
             (  Item         : Do_Amplifier_Zoom;
                First        : in out Boolean;
                Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                Inverse      : access Items_Stack := null
             )  is
   begin
      if Item.Amplifier.Get_Auto_Scaling then
         Push_Auto_Amplifier (Item.Amplifier, Inverse, First);
         Item.Amplifier.Set_Auto_Scaling (False);
      else
         Push_Amplifier_Zoom (Item.Amplifier, Inverse, First);
      end if;
      Item.Amplifier.Set_Page_Size (Item.Page_Size);
      Item.Amplifier.Set_Value (Item.Value);
   end Do_It;

   procedure Do_It
             (  Item         : Do_Release_Sweeper;
                First        : in out Boolean;
                Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                Inverse      : access Items_Stack := null
             )  is
   begin
      if Item.Sweeper.Get_Frozen then
         Push_Sweeper_Zoom (Item.Sweeper, Inverse, First);
         Item.Sweeper.Set_Frozen (False);
      end if;
   end Do_It;

   procedure Do_It
             (  Item         : Do_Sweeper_Zoom;
                First        : in out Boolean;
                Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                Inverse      : access Items_Stack := null
             )  is
   begin
      if Item.Sweeper.Get_Frozen then
         Push_Sweeper_Zoom (Item.Sweeper, Inverse, First);
      else
         Push_Release_Sweeper (Item.Sweeper, Inverse, First);
         Item.Sweeper.Set_Frozen (True);
      end if;
      Item.Sweeper.Set_Page_Span (Item.Page);
      Item.Sweeper.Set_Time (Item.Time);
   end Do_It;

   procedure Do_It
             (  Item         : Do_Stub;
                First        : in out Boolean;
                Oscilloscope : in out Gtk_Oscilloscope_Record'Class;
                Inverse      : access Items_Stack := null
             )  is
   begin
      Item.Stack.Stubs := Item.Previous;
--    Push_Stub (Item.Name, Inverse, First); -- Don't move it!
   end Do_It;

   procedure Erase_Redo_Stack
             (  Widget : not null access Gtk_Oscilloscope_Record
             )  is
   begin
      Delete (Widget.Redo_Stack.Actions);
      Widget.Redo_Stack.Stubs := null;
   end Erase_Redo_Stack;

   procedure Erase_Undo_Stack
             (  Widget : not null access Gtk_Oscilloscope_Record
             )  is
   begin
      Delete (Widget.Undo_Stack.Actions);
      Widget.Undo_Stack.Stubs := null;
   end Erase_Undo_Stack;

   procedure Finalize (Item : in out Do_Auto_Amplifier) is
   begin
      Item.Amplifier.Unref;
   end Finalize;

   procedure Finalize (Item : in out Do_Release_Sweeper) is
   begin
      Item.Sweeper.Unref;
   end Finalize;

   procedure Fix_Numbers
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Start  : Channel_Number;
                Stop   : Channel_Number
             )  is
      Row  : Gtk_Tree_Iter;
      From : constant Gint := Gint (Start) - 1;
      To   : constant Gint := Gint (Stop)  - 1;
   begin
      Row := Widget.Channel_Names.Nth_Child (Null_Iter, From);
      for Index in From..To loop
         Widget.Channel_Names.Set (Row, 1, Index + 1);
         Widget.Channel_Names.Next (Row);
      end loop;
   end Fix_Numbers;

   procedure Feed
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                T       : Time;
                V       : Gdouble
             )  is
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.Channels (Channel).Source.Put
      (  T => X_Axis (To_Double (T)),
         V => Y_Axis (V)
      );
   end Feed;

   procedure Feed
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                T       : Ada.Calendar.Time;
                V       : Gdouble
             )  is
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.Channels (Channel).Source.Put
      (  T => X_Axis (To_Double (T)),
         V => Y_Axis (V)
      );
   end Feed;

   procedure Feed
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                V       : Gdouble
             )  is
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.Channels (Channel).Source.Put
      (  T => X_Axis (To_Double (Clock)),
         V => Y_Axis (V)
      );
   end Feed;

   procedure Feed
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                T       : Gdouble;
                V       : Gdouble
             )  is
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
      Widget.Channels (Channel).Source.Put
      (  T => X_Axis (T),
         V => Y_Axis (V)
      );
   end Feed;

   function Get_Amplifier
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return not null access
                      Gtk_Waveform_Amplifier_Record'Class is
      Group : constant Group_Count :=
              Widget.Values_Axis (Amplifier).Group;
   begin
      if Group > 0 then
         return Widget.Groups (Group).Amplifier;
      else
         raise Constraint_Error with
            "No group is assigned to the axis";
      end if;
   end Get_Amplifier;

   function Get_Amplifier
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return not null access
                      Gtk_Waveform_Amplifier_Record'Class is
   begin
      if Channel <= Widget.Channels_Number then
         return
            Widget.Groups (Widget.Channels (Channel).Group).Amplifier;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Amplifier;

   function Get_Amplifier
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record;
               Group  : Group_Number
            )  return not null access
                      Gtk_Waveform_Amplifier_Record'Class is
   begin
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      else
         return Widget.Groups (Group).Amplifier;
      end if;
   end Get_Amplifier;

   function Get_Auto_Scaling
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Boolean is
   begin
      if Widget.Values_Axis (Amplifier).Group = 0 then
         raise Constraint_Error with
            "No group assigned to the amplifier";
      else
         return
            Widget.Get_Auto_Scaling
            (  Widget.Values_Axis (Amplifier).Group
            );
      end if;
   end Get_Auto_Scaling;

   function Get_Auto_Scaling
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record;
               Group  : Group_Number
            )  return Boolean is
   begin
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      else
         return Widget.Groups (Group).Amplifier.Get_Auto_Scaling;
      end if;
   end Get_Auto_Scaling;

   function Get_Box
            (  Widget : not null access constant Gtk_Oscilloscope_Record
            )  return Cairo_Box is
      Width  : constant Gdouble :=
               Gdouble (Widget.Layers.Get_Allocated_Width);
      Height : constant Gdouble :=
               Gdouble (Widget.Layers.Get_Allocated_Height);
      X1     : Gdouble := Widget.Values_Axis (Left).Offset;
      X2     : Gdouble := Width - Widget.Values_Axis (Right).Offset;
      Y1     : Gdouble := Widget.Time_Axis (Upper).Offset;
      Y2     : Gdouble := Height - Widget.Time_Axis (Lower).Offset;
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
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return not null access
                      Gtk_Wavefrom_Ring_Data_Buffer_Record'Class is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Source.all'Unchecked_Access;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Buffer;

   function Get_Channel_List
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return Gtk_List_Store is
   begin
      return Widget.Channel_Names;
   end Get_Channel_List;

   function Get_Channels_Number
            (  Widget : not null access constant Gtk_Oscilloscope_Record
            )  return Channel_Count is
   begin
      return Widget.Channels_Number;
   end Get_Channels_Number;

   function Get_Color
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return Gdk_Color is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Waveform.Get_Line.Color;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Color;

   function Get_Default_Face
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return Pango_Cairo_Font is
   begin
      return Widget.Default_Face;
   end Get_Default_Face;

   function Get_Enabled_Dropdown_Items
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return Dropdown_Items is
   begin
      return Widget.Menu_Enabled;
   end Get_Enabled_Dropdown_Items;

   function Get_From
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type := Lower
            )  return Time is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_From;
   end Get_From;

   function Get_From
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type := Lower
            )  return Ada.Calendar.Time is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_From;
   end Get_From;

   function Get_Frozen
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type := Lower
            )  return Boolean is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_Frozen;
   end Get_Frozen;

   procedure Get_Grid_Colors
             (  Widget      : not null access constant
                              Gtk_Oscilloscope_Record;
                Major_Color : out Gdk_Color;
                Minor_Color : out Gdk_Color
             )  is
   begin
      Major_Color := Widget.Major_Color;
      Minor_Color := Widget.Minor_Color;
   end Get_Grid_Colors;

   function Get_Group
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Group_Number is
      Result : constant Group_Count :=
               Widget.Values_Axis (Amplifier).Group;
   begin
      if Result > 0 then
         return Result;
      else
         raise Constraint_Error with "No group assigned";
      end if;
   end Get_Group;

   function Get_Group
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return Group_Number is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Group;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Group;

   function Get_Group_List
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return Gtk_List_Store is
   begin
      return Widget.Group_Names;
   end Get_Group_List;

   function Get_Groups_Number
            (  Widget : not null access constant Gtk_Oscilloscope_Record
            )  return Group_Count is
   begin
      return Widget.Groups_Number;
   end Get_Groups_Number;

   function Get_Interpolation_Mode
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return Interpolation_Mode is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Waveform.
                Get_Interpolation_Mode;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Interpolation_Mode;

   function Get_Left_Extrapolation_Mode
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return Boolean is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Waveform.
                Get_Left_Extrapolation_Mode;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Left_Extrapolation_Mode;

   function Get_Manual_Sweep
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return Boolean is
   begin
      return Widget.Manual_Sweep;
   end Get_Manual_Sweep;

   function Get_Name
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return String is
   begin
      if Channel <= Widget.Channels_Number then
         return
            Widget.Channel_Names.Get_String
            (  Widget.Channel_Names.Nth_Child
               (  Null_Iter,
                  Gint (Channel) - 1
               ),
               0
            );
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Name;

   function Get_Name
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record;
               Group  : Group_Number
            )  return String is
   begin
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      else
         return
            Widget.Group_Names.Get_String
            (  Widget.Group_Names.Nth_Child
               (  Null_Iter,
                  Gint (Group) - 1
               ),
               0
            );
      end if;
   end Get_Name;

   function Get_Offset
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Duration is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_Offset;
   end Get_Offset;

   function Get_Page_Span
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Duration is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_Page_Span;
   end Get_Page_Span;

   function Get_Redo_Stub
            (  Widget : not null access Gtk_Oscilloscope_Record;
               Depth  : Positive := 1
            )  return UTF8_String is
   begin
      return Get_Stub (Widget.Redo_Stack, Depth);
   end Get_Redo_Stub;

   function Get_Release_To_Latest
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return Boolean is
   begin
      return Widget.Jump_On_Thaw;
   end Get_Release_To_Latest;

   function Get_Right_Extrapolation_Mode
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return Boolean is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Waveform.
                Get_Right_Extrapolation_Mode;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Right_Extrapolation_Mode;

   function Get_Selection_Mode
            (  Widget : not null access constant Gtk_Oscilloscope_Record
            )  return Selection_Action is
   begin
      return Widget.Selection_Mode;
   end Get_Selection_Mode;

   function Get_Snapshot_File
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return UTF8_String is
   begin
      if Widget.Format = No_Snapshot or else Widget.File = null then
         return "";
      else
         return Widget.File.all;
      end if;
   end Get_Snapshot_File;

   function Get_Snapshot_Format
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record
            )  return Snapshot_Format is
   begin
      return Widget.Format;
   end Get_Snapshot_Format;

   function Get_Stub
            (  Stack : Items_Stack;
               Depth : Positive
            )  return UTF8_String is
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
         This := This.Next;
      end loop;
      raise End_Error with "No such stub";
   end Get_Stub;

   function Get_Superscript
            (  Widget : not null access constant Gtk_Oscilloscope_Record
            )  return Boolean is
   begin
      return Widget.Superscript;
   end Get_Superscript;

   function Get_Sweeper
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return not null access
                      Gtk_Waveform_Sweeper_Record'Class is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.all'Unchecked_Access;
   end Get_Sweeper;

   function Get_Sweeper
            (  Widget  : not null access Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return Sweeper_Type is
   begin
      if Channel <= Widget.Channels_Number then
         for Index in Widget.Time_Axis'Range loop
            if Widget.Channels (Channel).Waveform.Get_Sweeper =
               Gtk_Adjustment_Record'Class
               (  Widget.Time_Axis (Index).Sweeper.all
               ) 'Access
            then
               return Index;
            end if;
         end loop;
      end if;
      raise Constraint_Error with
         "Wrong channel number" & Channel_Count'Image (Channel);
   end Get_Sweeper;

   function Get_Time
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Time is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_Time;
   end Get_Time;

   function Get_Time
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type;
               X       : Gint
            )  return Time is
      Adjustment : constant Gtk_Waveform_Sweeper :=
                   Widget.Time_Axis (Sweeper).Sweeper;
      Box   : constant Cairo_Box := Widget.Get_Box;
      Lower : constant Gdouble := Get_Value (Adjustment);
      Page  : constant Gdouble := Get_Page_Size (Adjustment);
   begin
      if Box.X1 >= Gdouble (X) then
         return To_Time (Lower);
      elsif Box.X2 <= Gdouble (X) then
         return To_Time (Lower + Page);
      else
         return To_Time
                (  Lower
                +  Page * (Gdouble (X) - Box.X1) /(Box.X2 - Box.X1)
                );
      end if;
   end Get_Time;

   function Get_Time
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Ada.Calendar.Time is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_Time;
   end Get_Time;

   function Get_Time
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type;
               X       : Gint
            )  return Ada.Calendar.Time is
      Adjustment : constant Gtk_Waveform_Sweeper :=
                   Widget.Time_Axis (Sweeper).Sweeper;
      Box   : constant Cairo_Box := Widget.Get_Box;
      Lower : constant Gdouble := Get_Value (Adjustment);
      Page  : constant Gdouble := Get_Page_Size (Adjustment);
   begin
      if Box.X1 >= Gdouble (X) then
         return To_Time (Lower);
      elsif Box.X2 <= Gdouble (X) then
         return To_Time (Lower + Page);
      else
         return To_Time
                (  Lower
                +  Page * (Gdouble (X) - Box.X1) /(Box.X2 - Box.X1)
                );
      end if;
   end Get_Time;

   function Get_Time_Axis
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Boolean is
   begin
      return Widget.Time_Axis (Sweeper).On;
   end Get_Time_Axis;

   function Get_Time_Axis_Annotation
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return access Graph_Paper_Annotation_Layer'Class is
   begin
      return Widget.Time_Axis (Sweeper).Texts;
   end Get_Time_Axis_Annotation;

   function Get_Time_Axis_As_Time
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Boolean is
   begin
      return Widget.Time_Axis (Sweeper).Time_Mode;
   end Get_Time_Axis_As_Time;

   function Get_Time_Axis_Height
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Natural is
   begin
      return Natural (Widget.Time_Axis (Sweeper).Width);
   end Get_Time_Axis_Height;

   function Get_Time_Grid
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Boolean is
   begin
      return Widget.Time_Axis (Sweeper).Grid;
   end Get_Time_Grid;

   function Get_Time_Scale
             (  Widget  : not null access constant
                          Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type
             )  return Boolean is
   begin
      return not Widget.Time_Axis (Sweeper).No_Scale;
   end Get_Time_Scale;

   function Get_Time_Text_Angle
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Gdouble is
   begin
      return Widget.Time_Axis (Sweeper).Angle;
   end Get_Time_Text_Angle;

   function Get_Time_Text_Color
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Gdk_Color is
   begin
      return Widget.Time_Axis (Sweeper).Color;
   end Get_Time_Text_Color;

   function Get_Time_Text_Face
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Pango_Cairo_Font is
   begin
      return Widget.Time_Axis (Sweeper).Face;
   end Get_Time_Text_Face;

   function Get_Time_Text_Height
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Gdouble is
   begin
      return Widget.Time_Axis (Sweeper).Height;
   end Get_Time_Text_Height;

   function Get_Time_Text_Horizontal_Alignment
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Alignment is
   begin
      return Widget.Time_Axis (Sweeper).Justify_X;
   end Get_Time_Text_Horizontal_Alignment;

   function Get_Time_Text_Stretch
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Gdouble is
   begin
      return Widget.Time_Axis (Sweeper).Stretch;
   end Get_Time_Text_Stretch;

   function Get_Time_Text_Vertical_Alignment
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Vertical_Alignment is
   begin
      return Widget.Time_Axis (Sweeper).Justify_Y;
   end Get_Time_Text_Vertical_Alignment;

   function Get_Time_Tooltip
             (  Widget : not null access constant
                         Gtk_Oscilloscope_Record
             )  return Boolean is
   begin
      return Widget.Show_Time;
   end Get_Time_Tooltip;

   function Get_Time_Tooltip_Suffix
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return UTF8_String is
   begin
      if Channel <= Widget.Channels_Number then
         return +Widget.Channels (Channel).Tip_X_Suffix;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Time_Tooltip_Suffix;

   function Get_To
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Time is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_To;
   end Get_To;

   function Get_To
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Ada.Calendar.Time is
   begin
      return Widget.Time_Axis (Sweeper).Sweeper.Get_To;
   end Get_To;

   function Get_Tooltip_Annotation
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return UTF8_String is
   begin
      if Channel <= Widget.Channels_Number then
         return +Widget.Channels (Channel).Tip_Prefix;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Tooltip_Annotation;

   function Get_Type return GType is separate;

   function Get_Undo_Stub
            (  Widget : not null access Gtk_Oscilloscope_Record;
               Depth  : Positive := 1
            )  return UTF8_String is
   begin
      return Get_Stub (Widget.Undo_Stack, Depth);
   end Get_Undo_Stub;

   function Get_Value
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record;
               Group  : Group_Number;
               Y      : Gint
            )  return Gdouble is
   begin
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      else
         declare
            Adjustment : constant Gtk_Waveform_Amplifier :=
                         Widget.Groups (Group).Amplifier;
            Box   : constant Cairo_Box := Widget.Get_Box;
            Value : constant Gdouble := Get_Value (Adjustment);
            Page  : constant Gdouble := Get_Page_Size (Adjustment);
         begin
            if Box.Y1 >= Gdouble (Y) then
               return Value + Page;
            elsif Box.Y2 <= Gdouble (Y) then
               return Value;
            else
               return
               (  Value
               +  Page * (Box.Y2 - Gdouble (Y)) /(Box.Y2 - Box.Y1)
               );
            end if;
         end;
      end if;
   end Get_Value;

   function Get_Value
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type;
               Y         : Gint
            )  return Gdouble is
      Group : constant Group_Count :=
              Widget.Values_Axis (Amplifier).Group;
   begin
      if Group > 0 then
         return Get_Value (Widget, Group, Y);
      else
         raise Constraint_Error with "No group assigned";
      end if;
   end Get_Value;

   function Get_Values_Axis
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Boolean is
   begin
      return
      (  Widget.Values_Axis (Amplifier).Group > 0
      and then
         Widget.Values_Axis (Amplifier).On
      );
   end Get_Values_Axis;

   function Get_Values_Horizontal_Alignment
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Alignment is
   begin
      return Widget.Values_Axis (Amplifier).Justify_X;
   end Get_Values_Horizontal_Alignment;

   function Get_Values_Axis_Annotation
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return access Graph_Paper_Annotation_Layer'Class is
   begin
      return Widget.Values_Axis (Amplifier).Texts;
   end Get_Values_Axis_Annotation;

   function Get_Values_Axis_Width
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Natural is
   begin
      return Natural (Widget.Values_Axis (Amplifier).Width);
   end Get_Values_Axis_Width;

   function Get_Values_Grid
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Boolean is
   begin
      return
      (  Widget.Values_Axis (Amplifier).Group > 0
      and then
         Widget.Values_Axis (Amplifier).Grid
      );
   end Get_Values_Grid;

   function Get_Values_Scale
             (  Widget    : not null access constant
                            Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type
             )  return Boolean is
   begin
      return not Widget.Values_Axis (Amplifier).No_Scale;
   end Get_Values_Scale;

   function Get_Values_Text_Angle
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Gdouble is
   begin
      return Widget.Values_Axis (Amplifier).Angle;
   end Get_Values_Text_Angle;

   function Get_Values_Text_Color
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Gdk_Color is
   begin
      return Widget.Values_Axis (Amplifier).Color;
   end Get_Values_Text_Color;

   function Get_Values_Text_Face
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Pango_Cairo_Font is
   begin
      return Widget.Values_Axis (Amplifier).Face;
   end Get_Values_Text_Face;

   function Get_Values_Text_Height
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Gdouble is
   begin
      return Widget.Values_Axis (Amplifier).Height;
   end Get_Values_Text_Height;

   function Get_Values_Text_Stretch
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Gdouble is
   begin
      return Widget.Values_Axis (Amplifier).Stretch;
   end Get_Values_Text_Stretch;

   function Get_Values_Tooltip_Suffix
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return UTF8_String is
   begin
      if Channel <= Widget.Channels_Number then
         return +Widget.Channels (Channel).Tip_Y_Suffix;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Values_Tooltip_Suffix;

   function Get_Waveform
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return not null access Waveform_Layer is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Waveform;
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Get_Waveform;

   function Get_Annotation_Height
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type
            )  return Gint is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      if Data.On then
         return Gint (Data.Width);
      else
         return 0;
      end if;
   end Get_Annotation_Height;

   function Get_Annotation_Width
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Gint is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      if Data.On then
         return Gint (Data.Width);
      else
         return 0;
      end if;
   end Get_Annotation_Width;

   function Get_X
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type;
               Stamp   : Gdouble;
               Crop    : Boolean
            )  return Gint is
      Adjustment : constant Gtk_Waveform_Sweeper :=
                   Widget.Time_Axis (Sweeper).Sweeper;
      Box   : constant Cairo_Box := Widget.Get_Box;
      Lower : constant Gdouble := Get_Value (Adjustment);
      Page  : constant Gdouble := Get_Page_Size (Adjustment);
   begin
      if Stamp > Lower + Page then
         if Crop then
            return Gint (Box.X2);
         else
            raise Layout_Error with "Time right of the waveform box";
         end if;
      elsif Stamp < Lower then
         if Crop then
            return Gint (Box.X1);
         else
            raise Layout_Error with "Time left of the waveform box";
         end if;
      else
         return
            Gint
            (  Box.X1
            +  (Stamp - Lower) * (Box.X2 - Box.X1) / Page
            );
      end if;
   end Get_X;

   function Get_X
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type;
               Stamp   : Time;
               Crop    : Boolean := False
            )  return Gint is
   begin
      return Get_X (Widget, Sweeper, To_Double (Stamp), Crop);
   end Get_X;

   function Get_X
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Sweeper : Sweeper_Type;
               Stamp   : Ada.Calendar.Time;
               Crop    : Boolean := False
            )  return Gint is
   begin
      return Get_X (Widget, Sweeper, To_Double (Stamp), Crop);
   end Get_X;

   function Get_Y
            (  Widget : not null access constant
                        Gtk_Oscilloscope_Record;
               Group  : Group_Number;
               Value  : Gdouble;
               Crop   : Boolean := False
            )  return Gint is
   begin
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      else
         declare
            Adjustment : constant Gtk_Waveform_Amplifier :=
                         Widget.Groups (Group).Amplifier;
            Box   : constant Cairo_Box := Widget.Get_Box;
            Lower : constant Gdouble := Get_Value (Adjustment);
            Page  : constant Gdouble := Get_Page_Size (Adjustment);
         begin
            if Value < Lower then
               if Crop then
                  return Gint (Box.Y2);
               else
                  raise Layout_Error with
                     "Value below the waveform box";
               end if;
            elsif Value > Lower + Page then
               if Crop then
                  return Gint (Box.Y1);
               else
                  raise Layout_Error with
                     "Value above the waveform box";
               end if;
            else
               return
                  Gint
                  (  Box.Y2
                  -  (Value - Lower) * (Box.Y2 - Box.Y1) / Page
                  );
            end if;
         end;
      end if;
   end Get_Y;

   function Get_Y
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type;
               Value     : Gdouble;
               Crop      : Boolean := False
            )  return Gint is
      Group : constant Group_Count :=
              Widget.Values_Axis (Amplifier).Group;
   begin
      if Group > 0 then
         return Get_Y (Widget, Group, Value, Crop);
      else
         raise Constraint_Error with "No group assigned";
      end if;
   end Get_Y;

   procedure Gtk_New
             (  Widget         : out Gtk_Oscilloscope;
                Lower_Sweeper  : access
                             Gtk_Waveform_Sweeper_Record'Class := null;
                Upper_Sweeper  : access
                             Gtk_Waveform_Sweeper_Record'Class := null;
                Refresh_Engine : not null access Layered_Refresh_Engine;
                Background     : Gdk_Color := RGB (1.0, 1.0, 1.0);
                Buffer_Size    : Positive  := 1024 * 60;
                Max_Channels   : Channel_Number := 64
             )  is
   begin
      Widget := new Gtk_Oscilloscope_Record (Max_Channels);
      Do_Init
      (  Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => Refresh_Engine,
         Refresh_Period => 1.0,
         Buffer_Size    => Buffer_Size,
         Background     => Background
      );
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget         : out Gtk_Oscilloscope;
                Lower_Sweeper  : access
                             Gtk_Waveform_Sweeper_Record'Class := null;
                Upper_Sweeper  : access
                             Gtk_Waveform_Sweeper_Record'Class := null;
                Refresh_Period : Duration  := 0.02;
                Background     : Gdk_Color := RGB (1.0, 1.0, 1.0);
                Buffer_Size    : Positive  := 1024 * 60;
                Max_Channels   : Channel_Number := 64
             )  is
   begin
      Widget := new Gtk_Oscilloscope_Record (Max_Channels);
      Do_Init
      (  Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => null,
         Refresh_Period => Refresh_Period,
         Buffer_Size    => Buffer_Size,
         Background     => Background
      );
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   function Has_Group
            (  Widget    : not null access constant
                           Gtk_Oscilloscope_Record;
               Amplifier : Amplifier_Type
            )  return Boolean is
   begin
      return Widget.Values_Axis (Amplifier).Group > 0;
   end Has_Group;

   procedure Initialize
             (  Widget : not null access Gtk_Oscilloscope_Record'Class;
                Lower_Sweeper  : access
                                 Gtk_Waveform_Sweeper_Record'Class;
                Upper_Sweeper  : access
                                 Gtk_Waveform_Sweeper_Record'Class;
                Refresh_Engine : not null access Layered_Refresh_Engine;
                Background     : Gdk_Color;
                Buffer_Size    : Positive
             )  is
   begin
      Do_Init
      (  Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => Refresh_Engine,
         Refresh_Period => 1.0,
         Buffer_Size    => Buffer_Size,
         Background     => Background
      );
   end Initialize;

   procedure Initialize
             (  Widget : not null access Gtk_Oscilloscope_Record'Class;
                Lower_Sweeper  : access
                                 Gtk_Waveform_Sweeper_Record'Class;
                Upper_Sweeper  : access
                                 Gtk_Waveform_Sweeper_Record'Class;
                Refresh_Period : Duration := 0.2;
                Background     : Gdk_Color;
                Buffer_Size    : Positive
             )  is
   begin
      Do_Init
      (  Widget         => Widget,
         Lower_Sweeper  => Lower_Sweeper,
         Upper_Sweeper  => Upper_Sweeper,
         Refresh_Engine => null,
         Refresh_Period => Refresh_Period,
         Buffer_Size    => Buffer_Size,
         Background     => Background
      );
   end Initialize;

   function Is_Visible
            (  Widget  : not null access constant
                         Gtk_Oscilloscope_Record;
               Channel : Channel_Number
            )  return Boolean is
   begin
      if Channel <= Widget.Channels_Number then
         return Widget.Channels (Channel).Waveform.Is_Visible;
      else
         return False;
      end if;
   end Is_Visible;

   function Mouse_Event
            (  Oscilloscope : not null access Gtk_Oscilloscope_Record;
               Event : Gdk_Event;
               Hint  : Boolean
            )  return Cairo_Tuple is
   begin
      return Result : Cairo_Tuple do
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
            Get_Coords (Event, Result.X, Result.Y);
         else
            Get_Axis (Event, Axis_X, Result.X);
            Get_Axis (Event, Axis_Y, Result.Y);
         end if;
      end return;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Mouse_Event")
         )  );
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
             (  Widget     : not null access Gtk_Oscilloscope_Record;
                Old_Number : Channel_Number;
                New_Number : Channel_Number
             )  is
      This : Channel_Data;
   begin
      if Old_Number > Widget.Channels_Number then
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Old_Number);
      elsif New_Number > Widget.Channels_Number then
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (New_Number);
      end if;
      if Old_Number /= New_Number then
         This := Widget.Channels (Old_Number);
         if Old_Number < New_Number then
            for Index in Old_Number..New_Number - 1 loop
               Widget.Channels (Index) := Widget.Channels (Index + 1);
            end loop;
         else
            for Index in reverse New_Number + 1..Old_Number loop
               Widget.Channels (Index) := Widget.Channels (Index - 1);
            end loop;
         end if;
         Widget.Channels (New_Number) := This;
         if New_Number > 1 then
            Widget.Channel_Names.Move_After
            (  Iter =>
                  Widget.Channel_Names.Nth_Child
                  (  Null_Iter,
                     Gint (Old_Number) - 1
                  ),
               Position =>
                  Widget.Channel_Names.Nth_Child
                  (  Null_Iter,
                     Gint (New_Number) - 2
            )     );
         else
            Widget.Channel_Names.Move_Before
            (  Iter =>
                  Widget.Channel_Names.Nth_Child
                  (  Null_Iter,
                     Gint (Old_Number) - 1
                  ),
               Position =>
                  Widget.Channel_Names.Nth_Child
                  (  Null_Iter,
                     Gint (New_Number) - 1
            )     );
         end if;
         if Old_Number < New_Number then
            Widget.Fix_Numbers (Old_Number, New_Number);
         else
            Widget.Fix_Numbers (New_Number, Old_Number);
         end if;
      end if;
   end Move_Channel;

   procedure On_Autoscaling_Changed
             (  Amplifier : access Gtk_Waveform_Amplifier_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Index in Oscilloscope.Values_Axis'Range loop
         declare
            This  : Values_Axis_Data renames
                    Oscilloscope.Values_Axis (Index);
            Group : constant Group_Count := This.Group;
         begin
            if (  Group > 0
               and then
                  Amplifier = Oscilloscope.Groups (Group).Amplifier
               )
            then
               Oscilloscope.Update_Amplifier (Index);
               Emit
               (  Oscilloscope,
                  Signal_IDs (0),
                  Guint (Amplifier_Type'Pos (Index))
               );
            end if;
         end;
      end loop;
   end On_Autoscaling_Changed;

   procedure On_Cancel_Selection
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      Oscilloscope.Restore_State;
      Free (Oscilloscope.Selection.Area);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Cancel_Selection")
         )  );
   end On_Cancel_Selection;

   procedure On_Copy_Selection
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Y : constant Gdouble := Oscilloscope.Selection.Area.Get_Box.Y2;
      Got_It : Boolean;
   begin
      for Index in 1..Oscilloscope.Channels_Number loop
         declare
            Data : Channel_Data renames Oscilloscope.Channels (Index);
         begin
            Data.Waveform.Get (Y, Data.Value_1, Got_It);
            if Got_It then
               Data.Status := Absolute;
            else
               Data.Status := Undefined;
            end if;
         end;
      end loop;
      Oscilloscope.Update_Value;
      Oscilloscope.Restore_State;
      Free (Oscilloscope.Selection.Area);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Copy_Selection")
         )  );
   end On_Copy_Selection;

   procedure Delete (List : in out Do_Item_Ptr) is
      Next : Do_Item_Ptr;
   begin
      while List /= null loop
         Next := List.Next;
         Free (List);
         List := Next;
      end loop;
   end Delete;

   procedure On_Destroy
             (  Object       : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      if Oscilloscope.Refresh_Engine /= null then
         Oscilloscope.Refresh_Engine.Delete (Oscilloscope.Layers);
         Free (Oscilloscope.Refresh_Engine);
      end if;
      Oscilloscope.Remove (Oscilloscope.Layers);
      Oscilloscope.Channel_Names.Unref;
      Oscilloscope.Group_Names.Unref;
      for Index in Oscilloscope.Groups'Range loop
         if Oscilloscope.Groups (Index).Amplifier /= null then
            Oscilloscope.Groups (Index).Amplifier.Unref;
         end if;
      end loop;
      for Index in 1..Oscilloscope.Channels_Number loop
         if Oscilloscope.Channels (Index).Source /= null then
            Oscilloscope.Channels (Index).Source.Unref;
            Oscilloscope.Channels (Index).Source := null;
         end if;
         Free (Oscilloscope.Channels (Index).Tip_Prefix);
         Free (Oscilloscope.Channels (Index).Tip_X_Suffix);
         Free (Oscilloscope.Channels (Index).Tip_Y_Suffix);
      end loop;
      for Index in Oscilloscope.Time_Axis'Range loop
         if Oscilloscope.Time_Axis (Index).Sweeper /= null then
            Oscilloscope.Time_Axis (Index).Sweeper.Unref;
            Oscilloscope.Time_Axis (Index).Sweeper := null;
         end if;
      end loop;
      if Oscilloscope.Refresh_Period /= null then
         Oscilloscope.Refresh_Period.Unref;
         Oscilloscope.Refresh_Period := null;
      end if;
      if Oscilloscope.Drawing_Time /= null then
         Oscilloscope.Drawing_Time.Unref;
         Oscilloscope.Drawing_Time := null;
      end if;
      Free (Oscilloscope.Groups);
      Free (Oscilloscope.Selection);
      Oscilloscope.Erase_Undo_Stack;
      Oscilloscope.Erase_Redo_Stack;
      Free (Oscilloscope.File);
   end On_Destroy;

   procedure On_Difference_Selection
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Y : constant Gdouble := Oscilloscope.Selection.Area.Get_Box.Y2;
   begin
      for Index in 1..Oscilloscope.Channels_Number loop
         declare
            Data   : Channel_Data renames Oscilloscope.Channels (Index);
            Got_It : Boolean;
         begin
            if Data.Status /= Undefined then
               Data.Waveform.Get (Y, Data.Value_2, Got_It);
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
      Oscilloscope.Update_Value;
      Oscilloscope.Restore_State;
      Free (Oscilloscope.Selection.Area);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Difference_Selection")
         )  );
   end On_Difference_Selection;

   procedure On_Format_Time
             (  Scale     : not null access Gtk_Scale_Record'Class;
                Arguments : GValue_Array;
                Result    : in out GValue;
                Data      : Time_Axis_Data_Ptr
             )  is
      use type Ada.Calendar.Time;
      Page  : constant Duration := Data.Sweeper.Get_Page_Span / 2;
      Right : constant Ada.Calendar.Time := Data.Sweeper.Get_Time;
   begin
      if Data.Time_Mode then
         Set_String
         (  Result,
            (  Gtk.Layered.Graph_Paper_Annotation.Image (Right - Page)
            &  " "
            &  Strings_Edit.UTF8.Image (16#00B1#)
            &  " "
            &  Gtk.Layered.Graph_Paper_Annotation.Image (Page)
            &  "s"
         )  );
      else
         if Data.Texts /= null then
            Set_String
            (  Result,
               Data.Texts.Image (To_Double (Right - Page))
            );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Format_Time")
         )  );
   end On_Format_Time;

   procedure On_Freezing_Changed
             (  Sweeper      : access Gtk_Waveform_Sweeper_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Index in Oscilloscope.Time_Axis'Range loop
         if Oscilloscope.Time_Axis (Index).Sweeper = Sweeper then
            Emit
            (  Oscilloscope,
               Signal_IDs (2),
               Guint (Sweeper_Type'Pos (Index))
            );
            return;
         end if;
      end loop;
   end On_Freezing_Changed;

   procedure On_Latest
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Sweeper in Sweeper_Type'Range loop
         Oscilloscope.Set_Time
         (  Sweeper,
            Time'
            (  To_Time
               (  Oscilloscope.Time_Axis (Sweeper).Sweeper.Get_Upper
         )  )  );
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Latest")
         )  );
   end On_Latest;

   function On_Leave
            (  Object       : access GObject_Record'Class;
               Event        : Gdk_Event;
               Oscilloscope : Gtk_Oscilloscope
            )  return Boolean is
   begin
      return False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Leave")
         )  );
         return False;
   end On_Leave;

   function On_Motion
            (  Object       : access GObject_Record'Class;
               Event        : Gdk_Event;
               Oscilloscope : Gtk_Oscilloscope
            )  return Boolean is separate;

   procedure On_Offset_Changed
             (  Sweeper      : access Gtk_Waveform_Sweeper_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Index in Oscilloscope.Time_Axis'Range loop
         if Oscilloscope.Time_Axis (Index).Sweeper = Sweeper then
            Emit
            (  Oscilloscope,
               Signal_IDs (2),
               Guint (Sweeper_Type'Pos (Index))
            );
            return;
         end if;
      end loop;
   end On_Offset_Changed;

   procedure On_Pause
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Sweeper in Sweeper_Type'Range loop
         Oscilloscope.Set_Frozen (Sweeper, True);
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Pause")
         )  );
   end On_Pause;

   procedure On_Range_Selection
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Y1 : constant Gdouble := Oscilloscope.Selection.Area.Get_Box.Y1;
      Y2 : constant Gdouble := Oscilloscope.Selection.Area.Get_Box.Y2;
   begin
      for Index in 1..Oscilloscope.Channels_Number loop
         declare
            Data   : Channel_Data renames Oscilloscope.Channels (Index);
            Got_It : Boolean;
         begin
            Data.Waveform.Get (Y1, Data.Value_1, Got_It);
            if Got_It then
               Data.Waveform.Get (Y2, Data.Value_2, Got_It);
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
      Oscilloscope.Update_Value;
      Oscilloscope.Restore_State;
      Free (Oscilloscope.Selection.Area);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Range_Selection")
         )  );
   end On_Range_Selection;

   procedure On_Raster_Mode_Changed
             (  Amplifier : access Gtk_Waveform_Amplifier_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Index in Oscilloscope.Values_Axis'Range loop
         if (  Oscilloscope.Values_Axis (Index).Group > 0
            and then
               (  Amplifier
               =  Oscilloscope.Groups
                  (  Oscilloscope.Values_Axis (Index).Group
                  ) .Amplifier
            )  )
         then
            Emit
            (  Oscilloscope,
               Signal_IDs (1),
               Guint (Amplifier_Type'Pos (Index))
            );
            return;
         end if;
      end loop;
   end On_Raster_Mode_Changed;

   procedure On_Redo
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      Oscilloscope.Redo;
   end On_Redo;

   procedure On_Release
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Sweeper in Sweeper_Type'Range loop
         Oscilloscope.Set_Frozen (Sweeper, False);
         if Oscilloscope.Jump_On_Thaw then
            Oscilloscope.Set_Time
            (  Sweeper,
               Time'
               (  To_Time
                  (  Oscilloscope.Time_Axis (Sweeper).Sweeper.
                     Get_Upper
            )  )  );
         end if;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Release")
         )  );
   end On_Release;

   procedure On_Snapshot
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Surface : Cairo_Surface;
   begin
      case Oscilloscope.Format is
         when No_Snapshot =>
            return;
         when PDF_Snapshot =>
            Surface :=
               Cairo.PDF.Create
               (  Filename =>
                     Oscilloscope.File.all,
                  Width_In_Points =>
                     Gdouble (Oscilloscope.Get_Allocated_Width),
                  Height_In_Points =>
                     Gdouble (Oscilloscope.Get_Allocated_Height)
               );
         when SVG_Snapshot =>
            Surface :=
               Cairo.SVG.Create
               (  Filename =>
                     Oscilloscope.File.all,
                  Width_In_Point =>
                     Gdouble (Oscilloscope.Get_Allocated_Width),
                  Height_In_Point =>
                     Gdouble (Oscilloscope.Get_Allocated_Height)
               );
      end case;
      Oscilloscope.Layers.Snapshot (Surface);
      Emit (Oscilloscope, Signal_IDs (14), Oscilloscope.File.all);
      Surface_Destroy (Surface);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Snapshot")
         )  );
         Surface_Destroy (Surface);
   end On_Snapshot;

   procedure On_Style_Updated
             (  Object       : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      Oscilloscope.Line_Cap :=
         Style_Get (Oscilloscope, "waveform-line-cap");
      Oscilloscope.Background.Set
      (  Box   => Oscilloscope.Background.Get_Box,
         Color => Style_Get
                  (  Oscilloscope,
                     "background-color",
                     Oscilloscope.Background.Get_Color
                  ),
         Line_Width => 0.0,
         Opacity    => Fill_Opacity
                       (  Gdouble'
                          (  Style_Get
                             (  Oscilloscope,
                                "background-opacity"
      )                )  )  );
      for Sweeper in Sweeper_Type'Range loop
         Style_Changed_Time_Axis (Oscilloscope, Sweeper);
      end loop;
      for Amplifier in Amplifier_Type'Range loop
         Style_Changed_Values_Axis (Oscilloscope, Amplifier);
      end loop;
      Oscilloscope.Proximity :=
         Gdouble
         (  Guint'(Style_Get (Oscilloscope, "waveform-proximity"))
         );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Style_Updated")
         )  );
   end On_Style_Updated;

   procedure On_Toggle_Grid
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      On      : Boolean;
      Defined : Boolean := False;
   begin
      for Index in Amplifier_Type range Left..Right loop
         if Oscilloscope.Has_Group (Index) then
            if not Defined then
               On := not Oscilloscope.Get_Values_Grid (Index);
               Defined := True;
            end if;
            Oscilloscope.Set_Values_Grid (Index, On);
         end if;
      end loop;
      for Index in Sweeper_Type'Range loop
         if Oscilloscope.Time_Axis (Index).Channels > 0 then
            if not Defined then
               On := not Oscilloscope.Get_Time_Grid (Index);
               Defined := True;
            end if;
            Oscilloscope.Set_Time_Grid (Index, On);
         end if;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggle_Grid")
         )  );
   end On_Toggle_Grid;

   procedure On_Toggle_Interpolation
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      for Index in 1..Oscilloscope.Channels_Number loop
         declare
            Waveform : Waveform_Layer renames
                       Oscilloscope.Channels (Index).Waveform.all;
         begin
            case Waveform.Get_Interpolation_Mode is
               when Left =>
                  Waveform.Set_Interpolation_Mode (Linear);
               when Linear =>
                  Waveform.Set_Interpolation_Mode (Left);
            end case;
         end;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggle_Interpolation")
         )  );
   end On_Toggle_Interpolation;

   procedure On_Undo
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
   begin
      Oscilloscope.Undo;
   end On_Undo;

   procedure On_Zoom_In
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Box : constant Cairo_Box := Oscilloscope.Selection.Area.Get_Box;
   begin
      Oscilloscope.Restore_State;
      Oscilloscope.Erase_Redo_Stack;
      Free (Oscilloscope.Selection.Area);
      Oscilloscope.Push_Undo;
      for Group in 1..Oscilloscope.Groups_Number loop
         Oscilloscope.Zoom_In
         (  Oscilloscope.Groups (Group).Amplifier,
            Oscilloscope.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.Get_Value (Group, Gint (Box.Y1))
         );
      end loop;
      for Index in Sweeper_Type'Range loop
         Oscilloscope.Zoom_In
         (  Index,
            Oscilloscope.Get_Time (Index, Gint (Box.X1)),
            Oscilloscope.Get_Time (Index, Gint (Box.X2))
         );
      end loop;
      Oscilloscope.Selection.Saved := False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Zoom_In")
         )  );
   end On_Zoom_In;

   procedure On_Zoom_In_T
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Box : constant Cairo_Box := Oscilloscope.Selection.Area.Get_Box;
   begin
      Oscilloscope.Restore_State;
      Oscilloscope.Erase_Redo_Stack;
      Free (Oscilloscope.Selection.Area);
      Oscilloscope.Push_Undo (Time_Zooming);
      for Index in Sweeper_Type'Range loop
         Oscilloscope.Zoom_In
         (  Index,
            Oscilloscope.Get_Time (Index, Gint (Box.X1)),
            Oscilloscope.Get_Time (Index, Gint (Box.X2))
         );
      end loop;
      Oscilloscope.Selection.Saved := False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Zoom_In_T")
         )  );
   end On_Zoom_In_T;

   procedure On_Zoom_In_V
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Box : constant Cairo_Box := Oscilloscope.Selection.Area.Get_Box;
   begin
      Oscilloscope.Restore_State;
      Oscilloscope.Erase_Redo_Stack;
      Free (Oscilloscope.Selection.Area);
      Oscilloscope.Push_Undo (Values_Zooming);
      for Group in 1..Oscilloscope.Groups_Number loop
         Oscilloscope.Zoom_In
         (  Oscilloscope.Groups (Group).Amplifier,
            Oscilloscope.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.Get_Value (Group, Gint (Box.Y1))
         );
      end loop;
      Oscilloscope.Selection.Saved := False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Zoom_In_V")
         )  );
   end On_Zoom_In_V;

   procedure On_Zoom_Out
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Box : constant Cairo_Box := Oscilloscope.Selection.Area.Get_Box;
   begin
      Oscilloscope.Restore_State;
      Oscilloscope.Erase_Redo_Stack;
      Free (Oscilloscope.Selection.Area);
      Oscilloscope.Push_Undo;
      for Group in 1..Oscilloscope.Groups_Number loop
         Oscilloscope.Zoom_Out
         (  Oscilloscope.Groups (Group).Amplifier,
            Oscilloscope.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.Get_Value (Group, Gint (Box.Y1))
         );
      end loop;
      for Index in Sweeper_Type'Range loop
         Oscilloscope.Zoom_Out
         (  Index,
            Oscilloscope.Get_Time (Index, Gint (Box.X1)),
            Oscilloscope.Get_Time (Index, Gint (Box.X2))
         );
      end loop;
      Oscilloscope.Selection.Saved := False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Zoom_Out")
         )  );
   end On_Zoom_Out;

   procedure On_Zoom_Out_T
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Box : constant Cairo_Box := Oscilloscope.Selection.Area.Get_Box;
   begin
      Oscilloscope.Restore_State;
      Oscilloscope.Erase_Redo_Stack;
      Free (Oscilloscope.Selection.Area);
      Oscilloscope.Push_Undo (Time_Zooming);
      for Index in Sweeper_Type'Range loop
         Oscilloscope.Zoom_Out
         (  Index,
            Oscilloscope.Get_Time (Index, Gint (Box.X1)),
            Oscilloscope.Get_Time (Index, Gint (Box.X2))
         );
      end loop;
      Oscilloscope.Selection.Saved := False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Zoom_Out_T")
         )  );
   end On_Zoom_Out_T;

   procedure On_Zoom_Out_V
             (  Menu         : access GObject_Record'Class;
                Oscilloscope : Gtk_Oscilloscope
             )  is
      Box : constant Cairo_Box := Oscilloscope.Selection.Area.Get_Box;
   begin
      Oscilloscope.Restore_State;
      Oscilloscope.Erase_Redo_Stack;
      Free (Oscilloscope.Selection.Area);
      Oscilloscope.Push_Undo (Values_Zooming);
      for Group in 1..Oscilloscope.Groups_Number loop
         Oscilloscope.Zoom_Out
         (  Oscilloscope.Groups (Group).Amplifier,
            Oscilloscope.Get_Value (Group, Gint (Box.Y2)),
            Oscilloscope.Get_Value (Group, Gint (Box.Y1))
         );
      end loop;
      Oscilloscope.Selection.Saved := False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Zoom_Out_V")
         )  );
   end On_Zoom_Out_V;

   procedure Push_Amplifier_Zoom
             (  Amplifier : Gtk_Waveform_Amplifier;
                List      : access Items_Stack;
                First     : in out Boolean
             )  is
   begin
      if List /= null then
         List.Actions :=
            new Do_Amplifier_Zoom'
                (  Ada.Finalization.Limited_Controlled
                with
                   First     => First,
                   Next      => List.Actions,
                   Amplifier => Amplifier,
                   Value     => Amplifier.Get_Value,
                   Page_Size => Amplifier.Get_Page_Size
                );
         Amplifier.Ref;
         First := False;
      end if;
   end Push_Amplifier_Zoom;

   procedure Push_Auto_Amplifier
             (  Amplifier : Gtk_Waveform_Amplifier;
                List      : access Items_Stack;
                First     : in out Boolean
             )  is
   begin
      if List /= null then
         List.Actions :=
            new Do_Auto_Amplifier'
                (  Ada.Finalization.Limited_Controlled
                with
                   First     => First,
                   Next      => List.Actions,
                   Amplifier => Amplifier
                );
         Amplifier.Ref;
         First := False;
      end if;
   end Push_Auto_Amplifier;

   procedure Push_Release_Sweeper
             (  Sweeper : Gtk_Waveform_Sweeper;
                List    : access Items_Stack;
                First   : in out Boolean
             )  is
   begin
      if List /= null then
         List.Actions :=
            new Do_Release_Sweeper'
                (  Ada.Finalization.Limited_Controlled
                with
                   First   => First,
                   Next    => List.Actions,
                   Sweeper => Sweeper
                );
         Sweeper.Ref;
         First := False;
      end if;
   end Push_Release_Sweeper;

   procedure Push_Sweeper_Zoom
             (  Sweeper : Gtk_Waveform_Sweeper;
                List    : access Items_Stack;
                First   : in out Boolean
             )  is
   begin
      if List /= null then
         List.Actions :=
            new Do_Sweeper_Zoom'
                (  Ada.Finalization.Limited_Controlled
                with
                   First   => First,
                   Next    => List.Actions,
                   Sweeper => Sweeper,
                   Time    => Sweeper.Get_Time,
                   Page    => Sweeper.Get_Page_Span
                );
         Sweeper.Ref;
         First := False;
      end if;
   end Push_Sweeper_Zoom;

   procedure Push_Stub
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Name   : UTF8_String
             )  is
      First : Boolean := True;
   begin
      Push_Stub (Name, Widget.Undo_Stack'Access, First);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Push_Stub")
         )  );
   end Push_Stub;

   procedure Push_Stub
             (  Name  : String;
                List  : access Items_Stack;
                First : in out Boolean
             )  is
   begin
      if List /= null then
         List.Actions :=
            new Do_Stub'
                (  Ada.Finalization.Limited_Controlled
                with
                   First    => First,
                   Next     => List.Actions,
                   Stack    => List,
                   Previous => List.Stubs,
                   Length   => Name'Length,
                   Name     => Name
                );
         List.Stubs := List.Actions;
         First := False;
      end if;
   end Push_Stub;

   procedure Push_Undo
             (  Widget : not null access Gtk_Oscilloscope_Record;
                State  : Zooming_State := Values_Zooming or Time_Zooming
             )  is
      First : Boolean := True;
   begin
      if 0 /= (State and Values_Zooming) then
         for Group in 1..Widget.Groups_Number loop
            Widget.Save_Amplifier (Group, First);
         end loop;
      end if;
      if 0 /= (State and Time_Zooming) then
         for Index in Sweeper_Type'Range loop
            Widget.Save_Sweeper (Index, First);
         end loop;
      end if;
   end Push_Undo;

   procedure Redo
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Till   : UTF8_String := "";
                Stub   : UTF8_String := ""
             )  is
      use Strings_Edit.UTF8.Wildcards.Case_Insensitive;
      This  : Do_Item_Ptr := Widget.Redo_Stack.Actions;
      First : Boolean := True;
      Done  : Boolean := False;
   begin
      if Till'Length > 0 and then Stub'Length > 0 then
         Push_Stub (Stub, Widget.Undo_Stack'Access, First);
      end if;
      while This /= null and then not Done loop
         Widget.Redo_Stack.Actions := This.Next;
         if Till'Length > 0 then
            if This.all in Do_Stub'Class then
               Done := Match_Insensitive
                       (  Do_Stub'Class (This.all).Name,
                          Till,
                          True
                       );
            else
               Done := False; -- Never stop
            end if;
         else
            Done := This.First; -- Sequence beginning stop
         end if;
         This.Do_It (First, Widget.all, Widget.Undo_Stack'Access);
         Free (This);
         This := Widget.Redo_Stack.Actions;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Redo")
         )  );
   end Redo;

   procedure Refresh
             (  Widget  : not null access Gtk_Graphs_Record;
                Context : Cairo_Context
             )  is
   begin
      Gtk_Layered_Record (Widget.all).Refresh (Context);
      if (  Widget.Oscilloscope.Refresh_Period /= null
         or else
            Widget.Oscilloscope.Drawing_Time /= null
         )
      then
         declare
            T1 : constant Time := Widget.Get_Drawing_Time;
            T2 : constant Time := Clock;
         begin
            if Widget.Oscilloscope.Refresh_Period /= null then
               if Widget.Last_Time /= Time_First then
                  Widget.Oscilloscope.Refresh_Period.Put
                  (  T => X_Axis (To_Double (T2)),
                     V => Y_Axis (To_Duration (T1 - Widget.Last_Time))
                  );
               end if;
               Widget.Last_Time := T1;
            end if;
            if Widget.Oscilloscope.Drawing_Time /= null then
               Widget.Oscilloscope.Drawing_Time.Put
               (  T => X_Axis (To_Double (T2)),
                  V => Y_Axis (To_Duration (T2 - T1))
               );
            end if;
         end;
      end if;
   end Refresh;

   procedure Resized
             (  Widget     : not null access Gtk_Graphs_Record;
                Allocation : Gtk_Allocation
             )  is
      Width  : constant Gdouble := Gdouble (Allocation.Width);
      Height : constant Gdouble := Gdouble (Allocation.Height);
   begin
      Widget.Set_Aspect_Ratio (Width / Height);
      Box_Changed (Widget.Oscilloscope);
   end Resized;

   procedure Restore_State
             (  Widget : not null access Gtk_Oscilloscope_Record
             )  is
      First : Boolean     := True;
      This  : Do_Item_Ptr := Widget.Undo_Stack.Actions;
   begin
      while This /= null loop
         Widget.Undo_Stack.Actions := This.Next;
         declare
            Done : constant Boolean := This.First;
         begin
            This.Do_It (First, Widget.all);
            Free (This);
            exit when Done;
         end;
         This := Widget.Undo_Stack.Actions;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Restore_State")
         )  );
   end Restore_State;

   procedure Save_Amplifier
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Group  : Group_Number;
                First  : in out Boolean
             )  is
   begin
      if Widget.Groups /= null and then Group in Widget.Groups'Range
      then
         declare
            This : constant Gtk_Waveform_Amplifier :=
                   Widget.Groups (Group).Amplifier;
         begin
            if This.Get_Auto_Scaling then
               Push_Auto_Amplifier
               (  This,
                  Widget.Undo_Stack'Access,
                  First
               );
            else
               Push_Amplifier_Zoom
               (  This,
                  Widget.Undo_Stack'Access,
                  First
               );
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Save_Amplifier")
         )  );
   end Save_Amplifier;

   procedure Save_Sweeper
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                First   : in out Boolean
             )  is
      This : constant Gtk_Waveform_Sweeper :=
             Widget.Time_Axis (Sweeper).Sweeper;
   begin
      if This.Get_Frozen then
         Push_Sweeper_Zoom (This, Widget.Undo_Stack'Access, First);
      else
         Push_Release_Sweeper (This, Widget.Undo_Stack'Access, First);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Save_Sweeper")
         )  );
   end Save_Sweeper;

   procedure Set_Auto_Scaling
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type;
                Auto      : Boolean
             )  is
      This  : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
      Group : constant Group_Count := This.Group;
   begin
      if Group = 0 then
         raise Constraint_Error with
            "No group assigned to the amplifier";
      else
         Widget.Groups (Group).Amplifier.Set_Auto_Scaling (Auto);
      end if;
   end Set_Auto_Scaling;

   procedure Set_Auto_Scaling
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Group  : Group_Number;
                Auto   : Boolean
             )  is
   begin
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong group number";
      else
         Widget.Groups (Group).Amplifier.Set_Auto_Scaling (Auto);
      end if;
   end Set_Auto_Scaling;

   procedure Set_Default_Face
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Face   : Pango_Cairo_Font
             )  is
   begin
      Widget.Default_Face := Face;
   end Set_Default_Face;

   procedure Set_Enabled_Dropdown_Items
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Items  : Dropdown_Items
             )  is
   begin
      Widget.Menu_Enabled := Items;
   end Set_Enabled_Dropdown_Items;

   procedure Set_Extrapolation_Mode
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                Left    : Boolean;
                Right   : Boolean
             )  is
      Row : Gtk_Tree_Iter;
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      else
         declare
            Waveform : Waveform_Layer renames
                       Widget.Channels (Channel).Waveform.all;
         begin
            if (  Left /= Waveform.Get_Left_Extrapolation_Mode
               or else
                  Right /= Waveform.Get_Right_Extrapolation_Mode
               )
            then
               Waveform.Set_Extrapolation_Mode (Left, Right);
               Row := Widget.Channel_Names.Nth_Child
                      (  Null_Iter,
                         Gint (Channel) - 1
                      );
               Widget.Channel_Names.Set (Row, 6, Left);
               Widget.Channel_Names.Set (Row, 7, Right);
               Emit (Widget, Signal_IDs (15), Guint (Channel));
            end if;
         end;
      end if;
   end Set_Extrapolation_Mode;

   procedure Set_Frequency
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Frames  : Gdouble
             )  is
      Box : constant Cairo_Box := Widget.Get_Box;
   begin
      Set_Page_Span
      (  Widget,
         Sweeper,
         Duration ((Box.X2 - Box.X1) / Frames)
      );
   end Set_Frequency;

   procedure Set_Frozen
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Frozen  : Boolean
             )  is
      This : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      if Frozen and then not This.No_Scale then
         if This.Scale = null and then not Widget.Selection.Engaged then
            Gtk_New_Hbox (This.Box);
            Gtk_New (This.Left_Fill);
            This.Left_Fill.Set_Size_Request
            (  Width  => Widget.Get_Annotation_Width (Left),
               Height => 1
            );
            This.Box.Pack_Start (This.Left_Fill, False, False);
            Gtk_New_Hscale
            (  This.Scale,
               This.Sweeper.all'Unchecked_Access
            );
            This.Scale.Set_Hexpand (True);
            This.Scale.Set_Vexpand (False);
            This.Box.Pack_Start (This.Scale);
            Gtk_New (This.Right_Fill);
            This.Right_Fill.Set_Size_Request
            (  Width  => Widget.Get_Annotation_Width (Right),
               Height => 1
            );
            This.Box.Pack_Start (This.Right_Fill, False, False);
            case Sweeper is
               when Upper =>
                  This.Scale.Set_Value_Pos (Pos_Top);
                  Widget.Attach_Next_To
                  (  This.Box,
                     Widget.Layers,
                     Pos_Top
                  );
               when Lower =>
                  This.Scale.Set_Value_Pos (Pos_Bottom);
                  Widget.Attach_Next_To
                  (  This.Box,
                     Widget.Layers,
                     Pos_Bottom
                  );
            end case;
            This.Box.Show;
            This.Left_Fill.Show;
            This.Right_Fill.Show;
            This.Box.Show_All;
            Format_Handlers.Connect
            (  This.Scale,
               "format_value",
               On_Format_Time'Access,
               This'Unchecked_Access,
               True
            );
         end if;
      else
         if This.Scale /= null then
            Widget.Remove (This.Box);
            This.Scale      := null;
            This.Box        := null;
            This.Left_Fill  := null;
            This.Right_Fill := null;
         end if;
      end if;
      This.Sweeper.Set_Frozen (Frozen);
   end Set_Frozen;

   procedure Set_Grid_Colors
             (  Widget      : not null access Gtk_Oscilloscope_Record;
                Major_Color : Gdk_Color;
                Minor_Color : Gdk_Color
             )  is
      procedure Set_Grid (Paper : in out Graph_Paper_Layer) is
         Major : Line_Parameters := Paper.Get_Major_Line;
         Minor : Line_Parameters := Paper.Get_Minor_Line;
      begin
         Major.Color := Major_Color;
         Minor.Color := Minor_Color;
         Paper.Set
         (  Box           => Paper.Get_Box,
            X_Tick_Length => Paper.Get_X_Tick_Length,
            Y_Tick_Length => Paper.Get_Y_Tick_Length,
            Major_Line    => Major,
            Minor_Line    => Minor
         );
      end Set_Grid;
   begin
      Widget.Major_Color := Major_Color;
      Widget.Minor_Color := Minor_Color;
      for Index in Widget.Values_Axis'Range loop
         declare
            Data : Values_Axis_Data renames Widget.Values_Axis (Index);
         begin
            if Data.Group > 0 and then Data.Ticks /= null then
               Set_Grid (Data.Ticks.all);
            end if;
         end;
      end loop;
      for Index in Sweeper_Type'Range loop
         declare
            Data : Time_Axis_Data renames Widget.Time_Axis (Index);
         begin
            if Data.Ticks /= null then
               Set_Grid (Data.Ticks.all);
            end if;
         end;
      end loop;
   end Set_Grid_Colors;

   procedure Set_Group
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type;
                Group     : Group_Number
             )  is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      if Group > Widget.Groups_Number then
         raise Constraint_Error with "Wrong channel group number";
      end if;
      if Data.Group /= Group then
         Data.Group := Group;
         if Data.Ticks /= null then
            Data.Ticks.Set_Y_Axis (Widget.Groups (Group).Amplifier);
         end if;
         if Data.Scale /= null then
            Widget.Remove (Data.Box);
            Data.Scale      := null;
            Data.Scale      := null;
            Data.Box        := null;
            Data.Upper_Fill := null;
            Data.Lower_Fill := null;
         end if;
         Widget.Update_Amplifier (Amplifier);
         Emit
         (  Widget,
            Signal_IDs (4),
            Amplifier_Type'Pos (Amplifier)
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Group")
         )  );
   end Set_Group;

   procedure Set_Interpolation_Mode
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                Mode    : Interpolation_Mode
             )  is
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      elsif Widget.Channels (Channel).Waveform.Get_Interpolation_Mode /=
            Mode then
         Widget.Channels (Channel).Waveform.
            Set_Interpolation_Mode (Mode);
         Widget.Channel_Names.Set
         (  Widget.Channel_Names.Nth_Child
            (  Null_Iter,
               Gint (Channel) - 1
            ),
            3,
            Interpolation_Mode'Pos (Mode)
         );
         Emit (Widget, Signal_IDs (10), Guint (Channel));
      end if;
   end Set_Interpolation_Mode;

   procedure Set_Manual_Sweep
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Enable : Boolean
             )  is
   begin
      Widget.Manual_Sweep := Enable;
   end Set_Manual_Sweep;

   procedure Set_Page_Span
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Sweeper   : Sweeper_Type;
                Page_Span : Duration
             )  is
      Adjustment : constant Gtk_Waveform_Sweeper :=
                   Widget.Time_Axis (Sweeper).Sweeper;
   begin
      if Adjustment.Get_Frozen then
         declare
            Old : constant Time := Adjustment.Get_Time;
         begin
            Adjustment.Set_Page_Span (Page_Span);
            if Old /= Adjustment.Get_Time then
               Emit
               (  Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper)
               );
            end if;
         end;
      else
         declare
            Offset : constant Duration := Adjustment.Get_Offset;
         begin
            Adjustment.Set_Page_Span (Page_Span);
            if Offset /= Adjustment.Get_Offset then
               Emit
               (  Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper)
               );
            end if;
         end;
      end if;
   end Set_Page_Span;

   procedure Set_Preferred_Method
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Method : Waveform_Drawing_Method
             )  is
   begin
      for Index in 1..Widget.Channels_Number loop
         Widget.Channels (Index).Waveform.Set_Preferred_Method (Method);
      end loop;
   end Set_Preferred_Method;

   procedure Set_Release_To_Latest
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Enable : Boolean
             )  is
   begin
      Widget.Jump_On_Thaw := Enable;
   end Set_Release_To_Latest;

   procedure Set_Selection_Mode
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Action : Selection_Action
             )  is
   begin
      Widget.Selection_Mode := Action;
   end Set_Selection_Mode;

   procedure Set_Snapshot_File
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Format : Snapshot_Format := No_Snapshot;
                Name   : String := ""
             )  is
   begin
      if Widget.File /= null then
         Widget.Format := No_Snapshot;
         Free (Widget.File);
      end if;
      if Format /= No_Snapshot and then Name'Length > 0 then
         Widget.File := new String'(Name);
         Widget.Format := Format;
      end if;
   end Set_Snapshot_File;

   procedure Set_Superscript
             (  Widget      : not null access Gtk_Oscilloscope_Record;
                Superscript : Boolean
             )  is
   begin
      Widget.Superscript := Superscript;
      for Amplifier in Widget.Values_Axis'Range loop
         declare
            Data : Values_Axis_Data renames
                   Widget.Values_Axis (Amplifier);
         begin
            if Data.Texts /= null then
               Data.Texts.Set
               (  Location    => Data.Texts.Get_Location,
                  Face        => Data.Texts.Get_Face,
                  Height      => Data.Texts.Get_Height,
                  Stretch     => Data.Texts.Get_Stretch,
                  Color       => Data.Texts.Get_Color,
                  Text_Angle  => Data.Texts.Get_Text_Angle,
                  Justify_X   => Data.Texts.Get_Justify_X,
                  Justify_Y   => Data.Texts.Get_Justify_Y,
                  Background  => Data.Texts.Get_Background_Color,
                  Border      => Data.Texts.Get_Border,
                  Overlap     => Data.Texts.Get_Overlap,
                  Opacity     => Data.Texts.Get_Opacity,
                  Superscript => Superscript
              );
            end if;
         end;
      end loop;
      for Sweeper in Widget.Time_Axis'Range loop
         declare
            Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
         begin
            if Data.Texts /= null then
               Data.Texts.Set
               (  Location    => Data.Texts.Get_Location,
                  Face        => Data.Texts.Get_Face,
                  Height      => Data.Texts.Get_Height,
                  Stretch     => Data.Texts.Get_Stretch,
                  Color       => Data.Texts.Get_Color,
                  Text_Angle  => Data.Texts.Get_Text_Angle,
                  Justify_X   => Data.Texts.Get_Justify_X,
                  Justify_Y   => Data.Texts.Get_Justify_Y,
                  Background  => Data.Texts.Get_Background_Color,
                  Border      => Data.Texts.Get_Border,
                  Overlap     => Data.Texts.Get_Overlap,
                  Opacity     => Data.Texts.Get_Opacity,
                  Superscript => Superscript
              );
            end if;
         end;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Superscript")
         )  );
   end Set_Superscript;

   procedure Set_Time
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Stamp   : Time
             )  is
      Adjustment : constant Gtk_Waveform_Sweeper :=
                   Widget.Time_Axis (Sweeper).Sweeper;
   begin
      if Adjustment.Get_Frozen then
         declare
            Old : constant Time := Adjustment.Get_Time;
         begin
            Adjustment.Set_Time (Stamp);
            if Old /= Adjustment.Get_Time then
               Emit
               (  Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper)
               );
            end if;
         end;
      else
         declare
            Offset : constant Duration := Adjustment.Get_Offset;
         begin
            Adjustment.Set_Time (Stamp);
            if Offset /= Adjustment.Get_Offset then
               Emit
               (  Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper)
               );
            end if;
         end;
      end if;
   end Set_Time;

   procedure Set_Time
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Stamp   : Ada.Calendar.Time
             )  is
      Adjustment : constant Gtk_Waveform_Sweeper :=
                   Widget.Time_Axis (Sweeper).Sweeper;
   begin
      if Adjustment.Get_Frozen then
         declare
            Old : constant Time := Adjustment.Get_Time;
         begin
            Adjustment.Set_Time (Stamp);
            if Old /= Adjustment.Get_Time then
               Emit
               (  Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper)
               );
            end if;
         end;
      else
         declare
            Offset : constant Duration := Adjustment.Get_Offset;
         begin
            Adjustment.Set_Time (Stamp);
            if Offset /= Adjustment.Get_Offset then
               Emit
               (  Widget,
                  Signal_IDs (3),
                  Sweeper_Type'Pos (Sweeper)
               );
            end if;
         end;
      end if;
   end Set_Time;

   procedure Set_Time_Axis
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Visible : Boolean;
                As_Time : Boolean := True
             )  is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      if (Visible xor Data.On) or else (As_Time xor Data.Time_Mode) then
         if Visible then
            if Data.On then -- Turn it off and change the mode
               Widget.Set_Time_Axis (Sweeper, False, As_Time);
            else
               Data.Time_Mode := As_Time;
            end if;
            if not Data.Width_Set then
               Data.Width :=
                  Guint'(Style_Get (Widget, "time-axis-height"));
            end if;
            Data.Offset := Gdouble (Data.Width);
            Data.Line :=
               Add_Line
               (  Under => Widget.Background.Atop,
                  Angle => 0.0
               ) .all'Unchecked_Access;
            if not Data.Grid then
               Data.Ticks :=
                  Add_Graph_Paper
                  (  Under  => Data.Line,
                     Box    => (-0.5, -0.5, 0.5, 0.5),
                     X_Axis => Data.Sweeper
                  ) .all'Unchecked_Access;
            end if;
            if Get_Type (Data.Face) = Null_Font then
               Data.Face := Widget.Default_Face;
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
            Data.Scale.Set_Draw_Value (not Visible);
         end if;
         Box_Changed (Widget);
         Emit (Widget, Signal_IDs (5), Sweeper_Type'Pos (Sweeper));
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Time_Axis")
         )  );
   end Set_Time_Axis;

   procedure Set_Time_Axis_Height
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Height  : Natural
             )  is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      Data.Width_Set := True;
      if Data.Width /= Guint (Height) then
         Data.Width := Guint (Height);
         if Data.On then
            Data.Offset := Gdouble (Data.Width);
            Box_Changed (Widget);
         end if;
         for Amplifier in Widget.Values_Axis'Range loop
            case Amplifier is
               when Left | Right =>
                  declare
                     This : Values_Axis_Data renames
                            Widget.Values_Axis (Amplifier);
                  begin
                     if This.Scale /= null then
                        case Sweeper is
                           when Upper =>
                              This.Upper_Fill.Set_Size_Request
                              (  Width  => 1,
                                 Height => Widget.Get_Annotation_Height
                                           (  Sweeper
                              )            );
                           when Lower =>
                              This.Lower_Fill.Set_Size_Request
                              (  Width  => 1,
                                 Height => Widget.Get_Annotation_Height
                                           (  Sweeper
                              )            );
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
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Time_Axis_Height")
         )  );
   end Set_Time_Axis_Height;

   procedure Set_Time_Grid
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Visible : Boolean
             )  is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      if Data.Grid xor Visible then
         if Visible then
            if not Data.On then
               Data.Ticks :=
                  Add_Graph_Paper
                  (  Under       => Widget.Background.Atop,
                     Box         => (-0.5, -0.5, 0.5, 0.5),
                     X_Axis      => Data.Sweeper,
                     Major_Color => Widget.Major_Color,
                     Minor_Color => Widget.Minor_Color
                  ) .all'Unchecked_Access;
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
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Time_Grid")
         )  );
   end Set_Time_Grid;

   procedure Set_Time_Scale
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Visible : Boolean
             )  is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      if Data.No_Scale xor not Visible then
         Data.No_Scale := not Visible;
         Widget.Set_Frozen (Sweeper, Widget.Get_Frozen (Sweeper));
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Time_Scale")
         )  );
   end Set_Time_Scale;

   procedure Set_Time_Text_Alignment
             (  Widget     : not null access Gtk_Oscilloscope_Record;
                Sweeper    : Sweeper_Type;
                Horizontal : Alignment;
                Vertical   : Vertical_Alignment
             )  is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      Data.Justify_X := Horizontal;
      Data.Justify_Y := Vertical;
      if Data.On then
         Data.Texts.Set
         (  Location    => Data.Texts.Get_Location,
            Face        => Data.Texts.Get_Face,
            Height      => Data.Texts.Get_Height,
            Stretch     => Data.Texts.Get_Stretch,
            Color       => Data.Texts.Get_Color,
            Text_Angle  => Data.Texts.Get_Text_Angle,
            Justify_X   => Horizontal,
            Justify_Y   => Vertical,
            Superscript => Widget.Get_Superscript,
            Background  => Data.Texts.Get_Background_Color,
            Border      => Data.Texts.Get_Border,
            Overlap     => Data.Texts.Get_Overlap,
            Opacity     => Data.Texts.Get_Opacity
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Time_Text_Alignment")
         )  );
   end Set_Time_Text_Alignment;

   procedure Set_Time_Text_Font
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                Face    : Pango_Cairo_Font;
                Height  : Gdouble;
                Stretch : Gdouble   := 1.0;
                Color   : Gdk_Color := RGB (0.0, 0.0, 0.0);
                Angle   : Gdouble   := 0.0
             )  is
      Data : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
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
         Data.Texts.Set
         (  Location    => Data.Texts.Get_Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Color       => Color,
            Text_Angle  => Angle,
            Justify_X   => Data.Texts.Get_Justify_X,
            Justify_Y   => Data.Texts.Get_Justify_Y,
            Superscript => Widget.Get_Superscript,
            Background  => Data.Texts.Get_Background_Color,
            Border      => Data.Texts.Get_Border,
            Overlap     => Data.Texts.Get_Overlap,
            Opacity     => Data.Texts.Get_Opacity
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Time_Text_Font")
         )  );
   end Set_Time_Text_Font;

   procedure Set_Time_Tooltip
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Visible : Boolean
             )  is
   begin
      Widget.Show_Time := Visible;
   end Set_Time_Tooltip;

   procedure Set_Time_Tooltip_Suffix
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                Suffix  : UTF8_String
             )  is
   begin
      if Channel <= Widget.Channels_Number then
         Free (Widget.Channels (Channel).Tip_X_Suffix);
         Widget.Channels (Channel).Tip_X_Suffix := new String'(Suffix);
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Set_Time_Tooltip_Suffix;

   procedure Set_Tooltip_Annotation
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                Text    : UTF8_String
             )  is
   begin
      if Channel <= Widget.Channels_Number then
         Free (Widget.Channels (Channel).Tip_Prefix);
         Widget.Channels (Channel).Tip_Prefix := new String'(Text);
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Set_Tooltip_Annotation;

   procedure Set_Values_Alignment
             (  Widget     : not null access Gtk_Oscilloscope_Record;
                Amplifier  : Amplifier_Type;
                Horizontal : Alignment;
                Vertical   : Vertical_Alignment
             )  is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      if (  Data.Justify_X /= Horizontal
         or else
            Data.Justify_Y /= Vertical
         )
      then
         Data.Justify_X := Horizontal;
         Data.Justify_Y := Vertical;
         if Data.On and then Data.Group > 0 then
            Data.Texts.Set
            (  Location    => Data.Texts.Get_Location,
               Face        => Data.Texts.Get_Face,
               Height      => Data.Texts.Get_Height,
               Stretch     => Data.Texts.Get_Stretch,
               Color       => Data.Texts.Get_Color,
               Text_Angle  => Data.Texts.Get_Text_Angle,
               Justify_X   => Data.Justify_X,
               Justify_Y   => Data.Justify_Y,
               Superscript => Widget.Get_Superscript,
               Background  => Data.Texts.Get_Background_Color,
               Border      => Data.Texts.Get_Border,
               Overlap     => Data.Texts.Get_Overlap,
               Opacity     => Data.Texts.Get_Opacity
            );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Values_Alignment")
         )  );
   end Set_Values_Alignment;

   procedure Set_Values_Axis
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type;
                Visible   : Boolean
             )  is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      if Data.Group = 0 then
         raise Constraint_Error with "No group assigned";
      end if;
      if Visible xor Data.On then
         if Visible then
            if not Data.Width_Set then
               Data.Width :=
                  Guint'(Style_Get (Widget, "values-axis-width"));
            end if;
            case Amplifier is
               when Left | Right =>
                  Data.Offset := Gdouble (Data.Width);
               when Middle =>
                  null;
            end case;
            Data.Line :=
               Add_Line
               (  Under => Widget.Background.Atop,
                  Angle => 0.0
               ) .all'Unchecked_Access;
            if not Data.Grid then
               Data.Ticks :=
                  Add_Graph_Paper
                  (  Under  => Data.Line,
                     Box    => (-0.5, -0.5, 0.5, 0.5),
                     Y_Axis =>
                        Widget.Groups
                        (  Widget.Values_Axis (Amplifier).Group
                        ) .Amplifier
                  ) .all'Unchecked_Access;
            end if;
            if Get_Type (Data.Face) = Null_Font then
               Data.Face := Widget.Default_Face;
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
         (  Widget,
            Signal_IDs (6),
            Amplifier_Type'Pos (Amplifier)
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Values_Axis")
         )  );
   end Set_Values_Axis;

   procedure Set_Values_Axis_Width
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type;
                Width     : Natural
             )  is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      Data.Width_Set := True;
      if Data.Width /= Guint (Width) then
         Data.Width := Guint (Width);
         if Data.On then
            Data.Offset := Gdouble (Data.Width);
            Box_Changed (Widget);
         end if;
         for Sweeper in Widget.Time_Axis'Range loop
            declare
               This : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
            begin
               if This.Scale /= null then
                  case Amplifier is
                     when Left =>
                        This.Left_Fill.Set_Size_Request
                        (  Height => 1,
                           Width  => Widget.Get_Annotation_Width
                                     (  Amplifier
                        )            );
                     when Right =>
                        This.Right_Fill.Set_Size_Request
                        (  Height => 1,
                           Width  => Widget.Get_Annotation_Width
                                     (  Amplifier
                        )            );
                     when others =>
                        null;
                  end case;
               end if;
            end;
         end loop;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Values_Axis_Width")
         )  );
   end Set_Values_Axis_Width;

   procedure Set_Values_Grid
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type;
                Visible   : Boolean
             )  is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      if Data.Group = 0 then
         raise Constraint_Error with "No group assigned";
      end if;
      if Data.Grid xor Visible then
         if Visible then
            if not Data.On and then Data.Group > 0 then
               Data.Ticks :=
                  Add_Graph_Paper
                  (  Under  => Widget.Background.Atop,
                     Box    => (-0.5, -0.5, 0.5, 0.5),
                     Y_Axis => Widget.Groups (Data.Group).Amplifier,
                     Major_Color => Widget.Major_Color,
                     Minor_Color => Widget.Minor_Color
                  ) .all'Unchecked_Access;
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
         (  Widget,
            Signal_IDs (8),
            Amplifier_Type'Pos (Amplifier)
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Values_Grid")
         )  );
   end Set_Values_Grid;

   procedure Set_Values_Scale
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type;
                Visible   : Boolean
             )  is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      if Data.No_Scale xor not Visible then
         Data.No_Scale := not Visible;
         Widget.Update_Amplifier (Amplifier);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Values_Scale")
         )  );
   end Set_Values_Scale;

   procedure Set_Values_Text_Font
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type;
                Face      : Pango_Cairo_Font;
                Height    : Gdouble;
                Stretch   : Gdouble   := 1.0;
                Color     : Gdk_Color := RGB (0.0, 0.0, 0.0);
                Angle     : Gdouble   := 0.0
             )  is
      Data : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
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
         Data.Texts.Set
         (  Location    => Data.Texts.Get_Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Color       => Color,
            Text_Angle  => Angle,
            Justify_X   => Data.Texts.Get_Justify_X,
            Justify_Y   => Data.Texts.Get_Justify_Y,
            Superscript => Widget.Get_Superscript,
            Background  => Data.Texts.Get_Background_Color,
            Border      => Data.Texts.Get_Border,
            Overlap     => Data.Texts.Get_Overlap,
            Opacity     => Data.Texts.Get_Opacity
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Values_Text_Font")
         )  );
   end Set_Values_Text_Font;

   procedure Set_Values_Tooltip_Suffix
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                Suffix  : UTF8_String
             )  is
   begin
      if Channel <= Widget.Channels_Number then
         Free (Widget.Channels (Channel).Tip_Y_Suffix);
         Widget.Channels (Channel).Tip_Y_Suffix := new String'(Suffix);
      else
         raise Constraint_Error with
            "Wrong channel number" & Channel_Count'Image (Channel);
      end if;
   end Set_Values_Tooltip_Suffix;

   procedure Set_Visible
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Channel : Channel_Number;
                Visible : Boolean
             )  is
   begin
      if Channel > Widget.Channels_Number then
         raise Constraint_Error with "Wrong channel number";
      end if;
      if Widget.Channels (Channel).Waveform.Is_Visible xor Visible then
         Widget.Channels (Channel).Waveform.Set_Visible (Visible);
         Widget.Channel_Names.Set
         (  Widget.Channel_Names.Nth_Child
            (  Null_Iter,
               Gint (Channel) - 1
            ),
            3,
            Visible
         );
         Emit (Widget, Signal_IDs (9), Guint (Channel));
      end if;
   end Set_Visible;

   procedure Style_Changed_Time_Axis
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type
             )  is
      Box    : constant Cairo_Box := Widget.Get_Box;
      Height : Gdouble;
      Data   : Time_Axis_Data renames Widget.Time_Axis (Sweeper);
   begin
      if Data.On then
         Height :=
            Gdouble (Guint'(Style_Get (Widget, "time-tick-height")));
         declare -- Setting style of the line
            Line : Line_Parameters;
            Y    : Gdouble;
         begin
            Line := Data.Line.Get_Line;
            Line.Color :=
               Style_Get (Widget, "time-line-color", Line.Color);
            Line.Width :=
               Gdouble (Guint'(Style_Get (Widget, "time-line-width")));
            Line.Line_Cap := Style_Get (Widget, "time-line-cap");
            case Sweeper is
               when Lower =>
                  Y := Gdouble'Floor (Box.Y2) + 0.5;
               when Upper =>
                  Y := Gdouble'Floor (Box.Y1) + 0.5;
            end case;
            Data.Line.Set
            (  From => (Box.X1, Y),
               To   => (Box.X2, Y),
               Line => Line
            );
         end;
         declare -- Setting the time annotation
            Location : Axis_Location := Data.Texts.Get_Location;
         begin
            Location.Left  := Box.X1;
            Location.Right := Box.X2;
            case Sweeper is
               when Lower =>
                  Location.Y_Position :=
                     (  Box.Y2
                     +  (Widget.Time_Axis (Lower).Offset + Height)
                     /  2.0
                     );
               when Upper =>
                  Location.Y_Position :=
                     (  Box.Y1
                     -  (Widget.Time_Axis (Upper).Offset + Height)
                     /  2.0
                     );
            end case;
            Data.Texts.Set
            (  Location    => Location,
               Face        => Data.Face,
               Height      => Data.Height,
               Stretch     => Data.Stretch,
               Color       => Data.Color,
               Text_Angle  => Data.Angle,
               Justify_X   => Data.Justify_X,
               Justify_Y   => Data.Justify_Y,
               Superscript => Widget.Get_Superscript,
               Background =>
                  Style_Get
                  (  Widget,
                     "time-text-border-color",
                     Data.Texts.Get_Background_Color
                  ),
               Border =>
                  Gdouble
                  (  Guint'(Style_Get (Widget, "time-text-border"))
                  ),
               Overlap =>
                  Gdouble
                  (  Gint'(Style_Get (Widget, "time-text-overlap"))
                  ),
               Opacity =>
                  Gdouble'
                  (  Style_Get (Widget, "time-text-border-opacity")
            )     );
         end;
      end if;
      if Data.Grid or else Data.On then
         declare -- Setting the time ticks
            Major_Line : Line_Parameters;
            Minor_Line : Line_Parameters;
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
            Major_Line := Data.Ticks.Get_Major_Line;
            Major_Line.Color :=
               Style_Get
               (  Widget,
                  "time-major-tick-color",
                  Major_Line.Color
               );
            Major_Line.Width :=
               Gdouble
               (  Guint'(Style_Get (Widget, "time-major-tick-width"))
               );
            Major_Line.Line_Cap :=
               Style_Get (Widget, "time-major-tick-cap");
            Minor_Line := Data.Ticks.Get_Minor_Line;
            Minor_Line.Color :=
               Style_Get
               (  Widget,
                  "time-minor-tick-color",
                  Minor_Line.Color
               );
            Minor_Line.Width :=
               Gdouble
               (  Guint'(Style_Get (Widget, "time-minor-tick-width"))
               );
            Minor_Line.Line_Cap :=
               Style_Get (Widget, "time-minor-tick-cap");
            Data.Ticks.Set
            (  Box => (X1 => Box.X1, X2 => Box.X2, Y1 => Y1, Y2 => Y2),
               X_Tick_Length =>
                  Positive
                  (  Guint'
                     (  Style_Get (Widget, "time-tick-step")
                  )  ),
               Y_Tick_Length => 50,
               Major_Line => Major_Line,
               Minor_Line => Minor_Line
            );
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Changed_Time_Axis")
         )  );
   end Style_Changed_Time_Axis;

   procedure Style_Changed_Values_Axis
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type
             )  is
      Box   : constant Cairo_Box := Widget.Get_Box;
      Width : Gdouble;
      Data  : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
   begin
      if Data.On then
         Width :=
            Gdouble (Guint'(Style_Get (Widget, "values-tick-width")));
         declare -- Setting style of the line
            Line : Line_Parameters;
            X    : Gdouble;
         begin
            Line := Data.Line.Get_Line;
            Line.Color :=
               Style_Get (Widget, "values-line-color", Line.Color);
            Line.Width :=
               Gdouble (Guint'(Style_Get (Widget, "values-line-width")));
            Line.Line_Cap := Style_Get (Widget, "values-line-cap");
            case Amplifier is
               when Left =>
                  X := Gdouble'Floor (Box.X1) + 0.5;
               when Middle =>
                  X := Gdouble'Rounding ((Box.X1 + Box.X2) / 2.0) + 0.5;
               when Right =>
                  X := Gdouble'Floor (Box.X2) + 0.5;
            end case;
            Data.Line.Set
            (  From => (X, Box.Y1),
               To   => (X, Box.Y2),
               Line => Line
            );
         end;
         declare -- Setting the time annotation
            Location : Axis_Location := Data.Texts.Get_Location;
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
            Data.Texts.Set
            (  Location    => Location,
               Face        => Data.Face,
               Height      => Data.Height,
               Stretch     => Data.Stretch,
               Color       => Data.Color,
               Text_Angle  => Data.Angle,
               Justify_X   => Data.Justify_X,
               Justify_Y   => Data.Justify_Y,
               Superscript => Widget.Superscript,
               Background =>
                  Style_Get
                  (  Widget,
                     "values-text-border-color",
                     Data.Texts.Get_Background_Color
                  ),
               Border =>
                  Gdouble
                  (  Guint'(Style_Get (Widget, "values-text-border"))
                  ),
               Overlap =>
                  Gdouble
                  (  Gint'(Style_Get (Widget, "values-text-overlap"))
                  ),
               Opacity =>
                  Gdouble'
                  (  Style_Get (Widget, "values-text-border-opacity")
            )     );
         end;
      end if;
      if Data.Grid or else Data.On then
         declare -- Setting the time ticks
            Major_Line : Line_Parameters;
            Minor_Line : Line_Parameters;
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
            Major_Line := Data.Ticks.Get_Major_Line;
            Major_Line.Color :=
               Style_Get
               (  Widget,
                  "values-major-tick-color",
                  Major_Line.Color
               );
            Major_Line.Width :=
               Gdouble
               (  Guint'(Style_Get (Widget, "values-major-tick-width"))
               );
            Major_Line.Line_Cap :=
               Style_Get (Widget, "values-major-tick-cap");
            Minor_Line := Data.Ticks.Get_Minor_Line;
            Minor_Line.Color :=
               Style_Get
               (  Widget,
                  "values-minor-tick-color",
                  Minor_Line.Color
               );
            Minor_Line.Width :=
               Gdouble
               (  Guint'(Style_Get (Widget, "values-minor-tick-width"))
               );
            Minor_Line.Line_Cap :=
               Style_Get (Widget, "values-minor-tick-cap");
            Data.Ticks.Set
            (  Box => (X1 => X1, X2 => X2, Y1 => Box.Y1, Y2 => Box.Y2),
               X_Tick_Length =>
                  Positive
                  (  Guint'
                     (  Style_Get (Widget, "values-tick-width")
                  )  ),
               Y_Tick_Length => 50,
               Major_Line => Major_Line,
               Minor_Line => Minor_Line
            );
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Changed_Values_Axis")
         )  );
   end Style_Changed_Values_Axis;

   procedure Undo
             (  Widget : not null access Gtk_Oscilloscope_Record;
                Till   : UTF8_String := "";
                Stub   : UTF8_String := ""
             )  is
      use Strings_Edit.UTF8.Wildcards.Case_Insensitive;
      This  : Do_Item_Ptr := Widget.Undo_Stack.Actions;
      First : Boolean := True;
      Done  : Boolean := False;
   begin
      if Till'Length > 0 and then Stub'Length > 0 then
         Push_Stub (Stub, Widget.Undo_Stack'Access, First);
      end if;
      while This /= null and then not Done loop
         Widget.Undo_Stack.Actions := This.Next;
         if Till'Length > 0 then
            if This.all in Do_Stub'Class then
               Done := Match_Insensitive
                       (  Do_Stub'Class (This.all).Name,
                          Till,
                          True
                       );
            else
               Done := False; -- Never stop
            end if;
         else
            Done := This.First; -- Sequence beginning stop
         end if;
         This.Do_It (First, Widget.all, Widget.Redo_Stack'Access);
         Free (This);
         This := Widget.Undo_Stack.Actions;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Undo")
         )  );
   end Undo;

   procedure Update_Amplifier
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Amplifier_Type
             )  is
      Data  : Values_Axis_Data renames Widget.Values_Axis (Amplifier);
      Group : constant Group_Count := Data.Group;
   begin
      if Group > 0 then
         if (  Widget.Groups (Group).Amplifier.Get_Auto_Scaling
            or else
               Data.No_Scale
            )
         then
            Set (Data.Settings_Changed);
            Set (Data.Value_Changed);
            if Data.Scale /= null then
               Widget.Remove (Data.Box);
               Data.Scale      := null;
               Data.Scale      := null;
               Data.Box        := null;
               Data.Upper_Fill := null;
               Data.Lower_Fill := null;
            end if;
         else
            if (  Data.Scale = null
               and then
                  not Widget.Selection.Engaged
               and then
                  Amplifier /= Middle
               )
            then
               Gtk_New_Vbox (Data.Box);
               Data.Box.Set_Spacing (0);
               Gtk_New (Data.Upper_Fill);
               Data.Upper_Fill.Set_App_Paintable (True);
               Data.Box.Pack_Start (Data.Upper_Fill, False, False);
               Data.Upper_Fill.Set_Size_Request
               (  Width  => 1,
                  Height => Widget.Get_Annotation_Height (Upper)
               );
               Gtk_New_Vscale
               (  Data.Scale,
                  Widget.Groups (Group).Amplifier.all'Unchecked_Access
               );
               Data.Box.Pack_Start (Data.Scale);
               Data.Scale.Set_Hexpand (False);
               Data.Scale.Set_Vexpand (True);
               Data.Scale.Set_Draw_Value (False);
               Gtk_New (Data.Lower_Fill);
               Data.Lower_Fill.Set_App_Paintable (True);
               Data.Box.Pack_Start (Data.Lower_Fill, False, False);
               Data.Lower_Fill.Set_Size_Request
               (  Width  => 1,
                  Height => Widget.Get_Annotation_Height (Lower)
               );
               case Amplifier is
                  when Left =>
                     Data.Scale.Set_Value_Pos (Pos_Left);
                     Widget.Attach_Next_To
                     (  Data.Box,
                        Widget.Layers,
                        Pos_Left
                     );
                  when Right =>
                     Data.Scale.Set_Value_Pos (Pos_Right);
                     Widget.Attach_Next_To
                     (  Data.Box,
                        Widget.Layers,
                        Pos_Right
                     );
                  when others =>
                     null;
               end case;
               Data.Scale.Show_All;
            end if;
         end if;
      end if;
   end Update_Amplifier;

   procedure Update_Value
             (  Widget : not null access Gtk_Oscilloscope_Record
             )  is
      use type Ada.Calendar.Time;
      Row    : Gtk_Tree_Iter := Widget.Channel_Names.Get_Iter_First;
      T1, T2 : Ada.Calendar.Time;
   begin
      for Index in 1..Widget.Channels_Number loop
         declare
            Data : Channel_Data renames Widget.Channels (Index);
         begin
            case Data.Status is
               when Undefined =>
                  Widget.Channel_Names.Set (Row, 5, "");
               when Absolute =>
                  Widget.Channel_Names.Set
                  (  Row,
                     5,
                     Edit.Image (Gdouble (Data.Value_1), RelSmall => 6)
                  );
               when Difference =>
                  Widget.Channel_Names.Set
                  (  Row,
                     5,
                     Edit.Image
                     (  Gdouble (Data.Value_1 - Data.Value_2),
                        RelSmall => 6
                  )  );
            end case;
         exception
            when others =>
               Widget.Channel_Names.Set (Row, 5, "");
         end;
         Widget.Channel_Names.Next (Row);
      end loop;
      for Sweeper in Widget.Time_Axis'Range loop
         T1 := Widget.Get_Time
               (  Sweeper,
                  Gint (Widget.Selection.Area.Get_Box.X1)
               );
         T2 := Widget.Get_Time
               (  Sweeper,
                  Gint (Widget.Selection.Area.Get_Box.X2)
               );
         Emit
         (  Widget,
            Signal_IDs (11),
            Sweeper_Type'Pos (Sweeper),
            To_Double (T1),
            Gdouble (T2 - T1)
         );
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update_Value")
         )  );
   end Update_Value;

   procedure Zoom_In
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                T1, T2  : Ada.Calendar.Time
             )  is
      use Ada.Calendar;
      T : Ada.Calendar.Time := Widget.Get_Time (Sweeper);
   begin
      Widget.Set_Page_Span (Sweeper, T2 - T1);
      Widget.Set_Time (Sweeper, T2);
   end Zoom_In;

   procedure Zoom_In
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Gtk_Waveform_Amplifier;
                V1, V2    : Gdouble
             )  is
   begin
      Amplifier.Set_Page_Size (V2 - V1);
      Amplifier.Set_Value (V1);
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
             (  Widget  : not null access Gtk_Oscilloscope_Record;
                Sweeper : Sweeper_Type;
                T1, T2  : Ada.Calendar.Time
             )  is
      use type Ada.Calendar.Time;
      Page : constant Gdouble :=
                Gdouble (Widget.Get_Page_Span (Sweeper));
      T    : constant Gdouble :=
                To_Double
                (  Ada.Calendar.Time'(Widget.Get_Time (Sweeper))
                );
      A    : constant Gdouble := Page / Gdouble (T2 - T1);
      B    : constant Gdouble := T - A * To_Double (T2);
   begin
      Widget.Set_Page_Span (Sweeper, Duration (Page * A));
      Widget.Set_Time (Sweeper, Time'(To_Time (A * T + B)));
   end Zoom_Out;

   procedure Zoom_Out
             (  Widget    : not null access Gtk_Oscilloscope_Record;
                Amplifier : Gtk_Waveform_Amplifier;
                V1, V2    : Gdouble
             )  is
      Page  : constant Gdouble := Amplifier.Get_Page_Size;
      Value : constant Gdouble := Amplifier.Get_Value;
      A     : constant Gdouble := Page / (V2 - V1);
      B     : constant Gdouble := Value - A * V2;
   begin
      Amplifier.Set_Page_Size (Page * A);
      Amplifier.Set_Value (A * Value + B);
   end Zoom_Out;

end Gtk.Oscilloscope;
