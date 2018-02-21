--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Thermo_Dual                       Luebeck            --
--  Implementation                                 Summer, 2012       --
--                                                                    --
--                                Last revision :  22:46 07 Apr 2016  --
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
with Ada.Strings;

with Cairo.Line_Cap_Property;

with Gdk.Color.IHLS;

with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;

with Gtk.Enums;
with Gtk.Layered.Waveform;
with Gtk.Widget.Styles.Line_Cap_Property;

with Pango.Cairo.Fonts;

with Strings_Edit;

package body Gtk.Meter.Thermo_Dual is

   type Bar_Mode is (Celsius_Mode, Fahrenheit_Mode);

   Background_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Text_Color        : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

--     Celsius      : constant String := Character'Val (16#C2#) &
--                                       Character'Val (16#B0#) & 'C';
--     Fahrenheit   : constant String := Character'Val (16#C2#) &
--                                       Character'Val (16#B0#) & 'F';

   Corner                  : constant := 1.0 / 10.0;
   Length                  : constant := 2.5;
   First                   : constant := Length * 0.5;
   Bar_Width               : constant := 0.09;
   Reflection_Offset       : constant := Bar_Width * 0.35;
   Bar_Offset              : constant := 0.00;
   Left_Annotation_Offset  : constant := -0.19;
   Left_Tick_Offset        : constant := -0.07;
   Left_Label_Offset       : constant := -0.30;
   Right_Annotation_Offset : constant := 0.23;
   Right_Tick_Offset       : constant := 0.07;
   Right_Label_Offset      : constant := 0.30;
   Stem_Length             : constant := 0.15;
   Label_Height            : constant := -1.45;

   use type Gdk.Color.IHLS.Gdk_Luminance;

   Reflection_Shift : constant Gdk.Color.IHLS.Gdk_Luminance :=
                        Gdk.Color.IHLS.Gdk_Luminance'Last / 2;

   Color_Span : constant Gdk.Color.IHLS.Gdk_Luminance :=
                  Gdk.Color.IHLS.Gdk_Luminance'Last / 2;
   pragma Unreferenced (Color_Span);

   function Celsius (T : Gdouble) return Gdouble is
      F : constant := 5.0 / 9.0;
   begin
      return (T - 32.0) * F;
   exception
      when Constraint_Error =>
         if T < 0.0 then
            return -273.16;
         else
            return Gdouble'Last;
         end if;
   end Celsius;

   function Fahrenheit (T : Gdouble) return Gdouble is
      F : constant := 9.0 / 5.0;
   begin
      return T * F + 32.0;
   exception
      when Constraint_Error =>
         if T < 0.0 then
            return -459.67;
         else
            return Gdouble'Last;
         end if;
   end Fahrenheit;

   function Get_Background
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return not null access Gtk.Layered.Rectangular_Background.Rectangular_Background_Layer
   is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Bar
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return not null access Gtk.Layered.Bar.Bar_Layer is
   begin
      return Widget.all.Bar;
   end Get_Bar;

   function Get_Bar_Color
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return Gdk.Color.Gdk_Color is
   begin
      return Widget.all.Bar.all.Get_Line.Color;
   end Get_Bar_Color;

   function Get_Cache
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Celsius_Annotation
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return not null access Gtk.Layered.Flat_Annotation.Flat_Annotation_Layer
   is
   begin
      return Widget.all.Celsius.Annotation;
   end Get_Celsius_Annotation;

   function Get_Celsius_Label
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return not null access Gtk.Layered.Label.Label_Layer is
   begin
      return Widget.all.Celsius.Label;
   end Get_Celsius_Label;

   function Get_Celsius_Value
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record) return Gdouble is
   begin
      return
        (Widget.all.Bar.all.Get_Value * Widget.all.Celsius.Span
         +  Widget.all.Celsius.From);
   end Get_Celsius_Value;

   function Get_Fahrenheit_Annotation
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return not null access Gtk.Layered.Flat_Annotation.Flat_Annotation_Layer
   is
   begin
      return Widget.all.Fahrenheit.Annotation;
   end Get_Fahrenheit_Annotation;

   function Get_Fahrenheit_Label
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
      return not null access Gtk.Layered.Label.Label_Layer is
   begin
      return Widget.all.Fahrenheit.Label;
   end Get_Fahrenheit_Label;

   function Get_Fahrenheit_Value
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record) return Gdouble is
   begin
      return
        (Widget.all.Bar.all.Get_Value * Widget.all.Fahrenheit.Span
         +  Widget.all.Fahrenheit.From);
   end Get_Fahrenheit_Value;

   function Get_Type return GType is
   begin
      if
        Initialize_Class_Record
          (Ancestor     => Gtk.Layered.Get_Type,
           Class_Record => Class_Record'Access,
           Type_Name    => Class_Name)
      then
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "background-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Background color",
               Blurb      => "The background color"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "line-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Line color",
               Blurb      =>
                  "The color of the circle bounding the ticks"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "major-tick-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Major ticks color",
               Blurb      => "Major ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "major-tick-line-cap",
               Nick    => "Major tick cap",
               Blurb   => "The line cap style used for major ticks",
               Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "middle-tick-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Middle ticks color",
               Blurb      => "Middle ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "middle-tick-line-cap",
               Nick    => "Middle tick cap",
               Blurb   => "The line cap style used for middle ticks",
               Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "minor-tick-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Minor ticks color",
               Blurb      => "Minor ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "minor-tick-line-cap",
               Nick    => "Minor tick cap",
               Blurb   => "The line cap style used for minor ticks",
               Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "text-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Text color",
               Blurb      => "Text color"));
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New_Celsius
     (Widget     : out Gtk_Meter_Thermo_Dual;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Sectors    : Positive            := 8;
      Color      : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0)) is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Celsius (Widget, Adjustment, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Celsius;

   procedure Gtk_New_Celsius
     (Widget  : out Gtk_Meter_Thermo_Dual;
      Lower   : Gdouble             := -40.0;
      Upper   : Gdouble             := 50.0;
      Sectors : Positive            := 8;
      Color   : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0)) is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Celsius (Widget, Lower, Upper, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Celsius;

   procedure Gtk_New_Fahrenheit
     (Widget     : out Gtk_Meter_Thermo_Dual;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Sectors    : Positive            := 8;
      Color      : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0)) is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Fahrenheit (Widget, Adjustment, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Fahrenheit;

   procedure Gtk_New_Fahrenheit
     (Widget  : out Gtk_Meter_Thermo_Dual;
      Lower   : Gdouble             := 20.0;
      Upper   : Gdouble             := 220.0;
      Sectors : Positive            := 8;
      Color   : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0)) is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Fahrenheit (Widget, Lower, Upper, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Fahrenheit;

   procedure Initialize
     (Widget     : not null access Gtk_Meter_Thermo_Dual_Record'Class;
      Mode       : Bar_Mode;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Lower      : Gdouble;
      Upper      : Gdouble;
      Count      : Positive;
      Color      : Gdk.Color.Gdk_Color)
   is
      procedure Create_Annotation
        (Scale   : in out Temperature_Scale;
         Justify : Ada.Strings.Alignment;
         Offset  : Gdouble)
      is
         Texts   : UTF8_String (1 .. 2048);
         Pointer : Integer := Texts'First;
      begin
         for Index in 0 .. Scale.Sectors + 3 loop
            Gtk.Layered.Waveform.Edit.Put
              (Destination => Texts,
               Pointer     => Pointer,
               AbsSmall    => Scale.Small,
               Value       =>
                 (Scale.Lower
                  +  Gdouble (Index)
                  *  Gdouble (Scale.Step / Scale.Factor)));
            Strings_Edit.Put (Texts, Pointer, " ");
         end loop;
         Scale.Annotation :=
           Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
             (Under       => Widget.all.Cache,
              From        => (Offset, First - Scale.First),
              Length      => Length - Scale.First,
              Scale_Angle => 3.0 * Ada.Numerics.Pi / 2.0,
              Texts       => Texts (1 .. Pointer - 1),
              Justify     => Justify,
              Step        => Scale.Step,
              Height      => 0.1,
              Color       => Text_Color,
              Scaled      => True,
              Face        =>
                Pango.Cairo.Fonts.Create_Toy
                  (Family => "arial",
                   Slant  => Cairo.Cairo_Font_Slant_Normal,
                   Weight => Cairo.Cairo_Font_Weight_Normal));
      end Create_Annotation;

      procedure Create_Ticks
        (Scale   : in out Temperature_Scale;
         Lower   : Gdouble;
         Upper   : Gdouble;
         Offset  : Gdouble;
         Sign    : Gdouble;
         Sectors : Positive)
      is
         Raster : constant Gtk.Layered.Waveform.Rasters.Scale :=
                    Gtk.Layered.Waveform.Rasters.Create
                      (Lower, Upper, Sectors);
      begin
         Scale.Sectors := Sectors;
         Scale.From := Lower;
         Scale.Span := Upper - Lower;
         Scale.Factor := Length / Scale.Span;
         Scale.Small  := Integer'Min (Raster.Small, 0);
         Scale.Step :=
           (Gdouble (Raster.Minor)
            *  Scale.Factor
            *  Gdouble (Raster.Ticks + 1));
         if Raster.Low_Tick = 0 then
            Scale.Lower := Raster.Low_Value;
         else
            Scale.Lower :=
              (Raster.Low_Value
               +  Raster.Minor
               *  Gdouble (Raster.Ticks + 1 - Raster.Low_Tick));
         end if;
         Scale.First := Scale.Factor * Gdouble (Scale.Lower - Lower);
         Scale.Major_Ticks :=
           Gtk.Layered.Flat_Scale.Add_Flat_Scale
             (Under   => Widget.all.Background.all.Get_Foreground,
              From    => (Offset + Sign * 0.07, First - Scale.First),
              Length  => Length - Scale.First,
              Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
              Breadth => 0.13,
              Color   => Major_Tick_Color,
              Width   => 4.0 / 600.0,
              Step    => Scale.Step,
              Scaled  => True,
              Widened => True);
         declare
            Start : Gdouble := Scale.First;
            Tick  : Gtk.Layered.Tick_Number;
         begin
            if Start >= 0.5 * Scale.Step then
               Start := Start - 0.5 * Scale.Step;
               Tick := 1;
            else
               Tick := 2;
            end if;
            Scale.Middle_Ticks :=
              Gtk.Layered.Flat_Scale.Add_Flat_Scale
                (Under   => Widget.all.Background.all.Get_Foreground,
                 From    => (Offset + Sign * 0.05, First - Start),
                 Length  => Length - Start,
                 Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
                 Breadth => 0.10,
                 Color   => Middle_Tick_Color,
                 Width   => 2.0 / 600.0,
                 Step    => 0.5 * Scale.Step,
                 Skipped => 2,
                 First   => Tick,
                 Scaled  => True,
                 Widened => True);
         end;
         declare
            Start : Gdouble := Scale.First;
            Tick  : Gtk.Layered.Tick_Number := 10;
         begin
            if Start > 0.1 * Scale.Step then
               while Start >= 0.1 * Scale.Step loop
                  Start := Start - 0.1 * Scale.Step;
                  Tick  := Tick - 1;
               end loop;
               if Tick > 5 then
                  Tick := Tick - 5;
               end if;
            end if;
            Scale.Minor_Ticks :=
              Gtk.Layered.Flat_Scale.Add_Flat_Scale
                (Under   => Widget.all.Background.all.Get_Foreground,
                 From    => (Offset + Sign * 0.04, First - Start),
                 Length  => Length - Start,
                 Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
                 Breadth => 0.07,
                 Color   => Minor_Tick_Color,
                 Width   => 1.0 / 600.0,
                 Step    => 0.1 * Scale.Step,
                 Skipped => 5,
                 First   => Tick,
                 Scaled  => True,
                 Widened => True);
         end;
      end Create_Ticks;
   begin
      if Lower >= Upper then
         raise Constraint_Error with
               "Lower temperature is greater than the upper one";
      end if;
      case Mode is
         when Celsius_Mode =>
            if Lower < -273.16 then
               raise Constraint_Error with
                     "Lower temperature is less than absolute zero";
            elsif Upper > 10.0E8 then
               raise Constraint_Error with
                     "Upper temperature is too high";
            end if;
         when Fahrenheit_Mode =>
            if Lower < -459.67 then
               raise Constraint_Error with
                     "Lower temperature is less than absolute zero";
            elsif Upper > 10.0E8 then
               raise Constraint_Error with
                     "Upper temperature is too high";
            end if;
      end case;
      --
      -- Create background
      --
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Set_Aspect_Ratio (Widget, 1.0 / 3.0);
      Widget.all.Background :=
        Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
          (Under         => Widget,
           Height        => 3.0,
           Width         => 1.0,
           Center        => (0.0, 0.0),
           Corner_Radius => Corner,
           Color         => Background_Color,
           Border_Width  => 0.03,
           Border_Depth  => 0.01,
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      case Mode is
         when Celsius_Mode =>
            Create_Ticks
              (Widget.all.Celsius,
               Lower,
               Upper,
               Right_Tick_Offset,
               1.0,
               Count);
            Create_Ticks
              (Widget.all.Fahrenheit,
               Fahrenheit (Lower),
               Fahrenheit (Upper),
               Left_Tick_Offset,
               -1.0,
               Integer'Max (1, (Count * 9) / 5));
         when Fahrenheit_Mode =>
            Create_Ticks
              (Widget.all.Celsius,
               Celsius (Lower),
               Celsius (Upper),
               Left_Tick_Offset,
               1.0,
               Integer'Max (1, (Count * 5) / 9));
            Create_Ticks
              (Widget.all.Fahrenheit,
               Lower,
               Upper,
               Right_Tick_Offset,
               -1.0,
               Count);
      end case;
      Widget.all.Stem :=
        Gtk.Layered.Line.Add_Line
          (Under    => Widget.all.Background.all.Get_Foreground,
           From     => (Bar_Offset, First + Stem_Length),
           To       => (Bar_Offset, First),
           Width    => Bar_Width,
           Color    => Color,
           Line_Cap => Cairo.Cairo_Line_Cap_Butt,
           Scaled   => True,
           Widened  => True);
      Widget.all.Bulb :=
        Gtk.Layered.Line.Add_Line
          (Under    => Widget.all.Background.all.Get_Foreground,
           From     => (Bar_Offset, First + Stem_Length),
           To       => (Bar_Offset, First + Stem_Length),
           Width    => Bar_Width * 2.0,
           Color    => Color,
           Line_Cap => Cairo.Cairo_Line_Cap_Round,
           Scaled   => True,
           Widened  => True);
      Widget.all.Reflection :=
        Gtk.Layered.Line.Add_Line
          (Under    => Widget.all.Background.all.Get_Foreground,
           From     => (Bar_Offset          - Reflection_Offset,
                        First + Stem_Length - Reflection_Offset),
           To       => (Bar_Offset          - Reflection_Offset,
                        First + Stem_Length - Reflection_Offset),
           Width    => Reflection_Offset * 2.0,
           Line_Cap => Cairo.Cairo_Line_Cap_Round,
           Scaled   => True,
           Widened  => True,
           Color    =>
             Gdk.Color.IHLS.Lighten
               (Widget.all.Bulb.all.Get_Line.Color,
                Reflection_Shift,
                True));
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
      --
      -- Create annotation
      --
      Create_Annotation
        (Widget.all.Fahrenheit,
         Ada.Strings.Right,
         Left_Annotation_Offset);
      Widget.all.Fahrenheit.Label :=
        Gtk.Layered.Label.Add_Label
          (Under    => Widget.all.Get_Cache,
           Face     =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Color    => Text_Color,
           Height   => 0.12,
           Mode     => Gtk.Layered.Moved_Centered,
           Scaled   => True,
           Text     => Character'Val (16#C2#) &
               Character'Val (16#B0#) & 'F',
           Location => (Left_Label_Offset, Label_Height));
      Create_Annotation
        (Widget.all.Celsius,
         Ada.Strings.Left,
         Right_Annotation_Offset);
      Widget.all.Celsius.Label :=
        Gtk.Layered.Label.Add_Label
          (Under    => Widget.all.Get_Cache,
           Face     =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Color    => Text_Color,
           Height   => 0.12,
           Mode     => Gtk.Layered.Moved_Centered,
           Scaled   => True,
           Text     => Character'Val (16#C2#) &
               Character'Val (16#B0#) & 'C',
           Location => (Right_Label_Offset, Label_Height));
      --
      -- Create foreground
      --
      Widget.all.Bar :=
        Gtk.Layered.Bar.Add_Bar
          (Under      => Widget.all.Background.all.Get_Foreground,
           From       => (Bar_Offset, First),
           Length     => Length,
           Angle      => 3.0 * Ada.Numerics.Pi / 2.0,
           Adjustment => Adjustment,
           Line_Cap   => Cairo.Cairo_Line_Cap_Butt,
           Width      => Bar_Width,
           Color      => Color,
           Scaled     => True,
           Widened    => True);
   end Initialize;

   procedure Initialize_Celsius
     (Widget     : not null access Gtk_Meter_Thermo_Dual_Record'Class;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Sectors    : Positive;
      Color      : Gdk.Color.Gdk_Color) is
   begin
      Initialize
        (Widget,
         Celsius_Mode,
         Adjustment.all'Unchecked_Access,
         Adjustment.all.Get_Lower,
         Adjustment.all.Get_Upper,
         Sectors,
         Color);
   end Initialize_Celsius;

   procedure Initialize_Celsius
     (Widget  : not null access Gtk_Meter_Thermo_Dual_Record'Class;
      Lower   : Gdouble;
      Upper   : Gdouble;
      Sectors : Positive;
      Color   : Gdk.Color.Gdk_Color) is
   begin
      Initialize
        (Widget,
         Celsius_Mode,
         null,
         Lower,
         Upper,
         Sectors,
         Color);
   end Initialize_Celsius;

   procedure Initialize_Fahrenheit
     (Widget     : not null access Gtk_Meter_Thermo_Dual_Record'Class;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class;
      Sectors    : Positive;
      Color      : Gdk.Color.Gdk_Color) is
   begin
      Initialize
        (Widget,
         Fahrenheit_Mode,
         Adjustment.all'Unchecked_Access,
         Adjustment.all.Get_Lower,
         Adjustment.all.Get_Upper,
         Sectors,
         Color);
   end Initialize_Fahrenheit;

   procedure Initialize_Fahrenheit
     (Widget  : not null access Gtk_Meter_Thermo_Dual_Record'Class;
      Lower   : Gdouble;
      Upper   : Gdouble;
      Sectors : Positive;
      Color   : Gdk.Color.Gdk_Color) is
   begin
      Initialize
        (Widget,
         Fahrenheit_Mode,
         null,
         Lower,
         Upper,
         Sectors,
         Color);
   end Initialize_Fahrenheit;

   procedure Set_Bar_Color
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record;
      Color  : Gdk.Color.Gdk_Color) is
   begin
      Widget.all.Bar.all.Set
        (From => Widget.all.Bar.all.Get_From,
         To   => Widget.all.Bar.all.Get_To,
         Line => (Width    => Widget.all.Bar.all.Get_Line.Width,
                  Color    => Color,
                  Line_Cap => Widget.all.Bar.all.Get_Line.Line_Cap));
      Widget.all.Bulb.all.Set
        (From => Widget.all.Bulb.all.Get_From,
         To   => Widget.all.Bulb.all.Get_To,
         Line =>
           (Width    => Widget.all.Bulb.all.Get_Line.Width,
            Color    => Color,
            Line_Cap => Widget.all.Bulb.all.Get_Line.Line_Cap));
      Widget.all.Reflection.all.Set
        (From => Widget.all.Reflection.all.Get_From,
         To   => Widget.all.Reflection.all.Get_To,
         Line =>
           (Width    => Widget.all.Reflection.all.Get_Line.Width,
            Line_Cap => Widget.all.Reflection.all.Get_Line.Line_Cap,
            Color    =>
              Gdk.Color.IHLS.Lighten
                (Color, Reflection_Shift, True)));
      Widget.all.Stem.all.Set
        (From   => Widget.all.Stem.all.Get_From,
         To     => Widget.all.Stem.all.Get_To,
         Line   =>
           (Width    => Widget.all.Stem.all.Get_Line.Width,
            Color    => Color,
            Line_Cap => Widget.all.Stem.all.Get_Line.Line_Cap));
   end Set_Bar_Color;

   procedure Set_Celsius_Value
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record;
      Value  : Gdouble) is
   begin
      Widget.all.Bar.all.Set_Value
        ((Value - Widget.all.Celsius.From)
         /  Widget.all.Celsius.Span);
   end Set_Celsius_Value;

   procedure Set_Fahrenheit_Value
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record;
      Value  : Gdouble) is
   begin
      Widget.all.Bar.all.Set_Value
        ((Value - Widget.all.Fahrenheit.From)
         /  Widget.all.Fahrenheit.Span);
   end Set_Fahrenheit_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Meter_Thermo_Dual_Record)
   is
      procedure Set (Scale : in out Temperature_Scale) is
      begin
         Scale.Minor_Ticks.all.Set
           (From    => Scale.Minor_Ticks.all.Get_From,
            Length  => Scale.Minor_Ticks.all.Get_Length,
            Breadth => Scale.Minor_Ticks.all.Get_Breadth,
            Angle   => Scale.Minor_Ticks.all.Get_Angle,
            Ticks   => Scale.Minor_Ticks.all.Get_Ticks,
            Line    =>
              (Scale.Minor_Ticks.all.Get_Line.Width,
               Gtk.Widget.Styles.Style_Get
                 (Widget, "minor-tick-color", Minor_Tick_Color),
               Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                 (Widget, "minor-tick-line-cap")));
         Scale.Middle_Ticks.all.Set
           (From    => Scale.Middle_Ticks.all.Get_From,
            Length  => Scale.Middle_Ticks.all.Get_Length,
            Breadth => Scale.Middle_Ticks.all.Get_Breadth,
            Angle   => Scale.Middle_Ticks.all.Get_Angle,
            Ticks   => Scale.Middle_Ticks.all.Get_Ticks,
            Line    =>
              (Scale.Middle_Ticks.all.Get_Line.Width,
               Gtk.Widget.Styles.Style_Get
                 (Widget, "middle-tick-color", Middle_Tick_Color),
               Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                 (Widget, "middle-tick-line-cap")));
         Scale.Major_Ticks.all.Set
           (From    => Scale.Major_Ticks.all.Get_From,
            Length  => Scale.Major_Ticks.all.Get_Length,
            Breadth => Scale.Major_Ticks.all.Get_Breadth,
            Angle   => Scale.Major_Ticks.all.Get_Angle,
            Ticks   => Scale.Major_Ticks.all.Get_Ticks,
            Line    =>
              (Scale.Major_Ticks.all.Get_Line.Width,
               Gtk.Widget.Styles.Style_Get
                 (Widget, "major-tick-color", Major_Tick_Color),
               Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                 (Widget, "major-tick-line-cap")));
         Scale.Annotation.all.Set
           (Ticks       => Scale.Annotation.all.Get_Ticks,
            From        => Scale.Annotation.all.Get_From,
            Length      => Scale.Annotation.all.Get_Length,
            Face        => Scale.Annotation.all.Get_Face,
            Scale_Angle => Scale.Annotation.all.Get_Scale_Angle,
            Height      => Scale.Annotation.all.Get_Height,
            Stretch     => Scale.Annotation.all.Get_Stretch,
            Text_Angle  => Scale.Annotation.all.Get_Text_Angle,
            Justify     => Scale.Annotation.all.Get_Justify,
            Color       =>
              Gtk.Widget.Styles.Style_Get (Widget, "text-color", Text_Color));
         Scale.Label.all.Set
           (Location => Scale.Label.all.Get_Location,
            Face     => Scale.Label.all.Get_Face,
            Height   => Scale.Label.all.Get_Height,
            Stretch  => Scale.Label.all.Get_Stretch,
            Mode     => Scale.Label.all.Get_Mode,
            Angle    => Scale.Label.all.Get_Angle,
            Skew     => Scale.Label.all.Get_Skew,
            Color    =>
              Gtk.Widget.Styles.Style_Get (Widget, "text-color", Text_Color));
      end Set;
   begin
      Widget.all.Background.all.Set
        (Height         => Widget.all.Background.all.Get_Height,
         Width          => Widget.all.Background.all.Get_Width,
         Center         => Widget.all.Background.all.Get_Center,
         Rotation_Angle => Widget.all.Background.all.Get_Rotation_Angle,
         Corner_Radius  => Widget.all.Background.all.Get_Corner_Radius,
         Border_Width   => Widget.all.Background.all.Get_Border_Width,
         Border_Depth   => Widget.all.Background.all.Get_Border_Depth,
         Border_Color   => Widget.all.Background.all.Get_Border_Color,
         Border_Shadow  => Widget.all.Background.all.Get_Border_Shadow,
         Color          =>
           Gtk.Widget.Styles.Style_Get
             (Widget, "background-color", Background_Color));
      Set (Widget.all.Celsius);
      Set (Widget.all.Fahrenheit);
   end Style_Changed;

end Gtk.Meter.Thermo_Dual;
