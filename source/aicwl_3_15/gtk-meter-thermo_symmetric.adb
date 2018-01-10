--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Thermo_Symmetric                  Luebeck            --
--  Implementation                                 Summer, 2012       --
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
with Ada.Strings;

with Cairo.Line_Cap_Property;

with Gdk.Color.IHLS;

with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;

with Gtk.Widget.Styles.Line_Cap_Property;

with Pango.Cairo.Fonts;

package body Gtk.Meter.Thermo_Symmetric is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Background_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Text_Color        : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

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

   procedure Create_Background
     (Widget  : not null access Gtk_Meter_Thermo_Symmetric_Record'Class;
      Sectors : Positive;
      Color   : Gdk.Color.Gdk_Color) is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Sectors := Sectors;
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
      Widget.all.Left_Scale.Major_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (Left_Tick_Offset - 0.07, First),
           Length  => Length,
           Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
           Breadth => 0.13,
           Color   => Major_Tick_Color,
           Width   => 4.0 / 600.0,
           Step    => Length / Gdouble (Sectors),
           Scaled  => True,
           Widened => True);
      Widget.all.Left_Scale.Middle_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (Left_Tick_Offset - 0.05, First),
           Length  => Length,
           Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
           Breadth => 0.10,
           Color   => Middle_Tick_Color,
           Width   => 2.0 / 600.0,
           Step    => 0.5 * Length / Gdouble (Sectors),
           Skipped => 2,
           Scaled  => True,
           Widened => True);
      Widget.all.Left_Scale.Minor_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (Left_Tick_Offset - 0.04, First),
           Length  => Length,
           Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
           Breadth => 0.07,
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 600.0,
           Step    => 0.1 * Length / Gdouble (Sectors),
           Skipped => 5,
           Scaled  => True,
           Widened => True);
      Widget.all.Right_Scale.Major_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (Right_Tick_Offset + 0.07, First),
           Length  => Length,
           Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
           Breadth => 0.13,
           Color   => Major_Tick_Color,
           Width   => 4.0 / 600.0,
           Step    => Length / Gdouble (Sectors),
           Scaled  => True,
           Widened => True);
      Widget.all.Right_Scale.Middle_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (Right_Tick_Offset + 0.05, First),
           Length  => Length,
           Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
           Breadth => 0.10,
           Color   => Middle_Tick_Color,
           Width   => 2.0 / 600.0,
           Step    => 0.5 * Length / Gdouble (Sectors),
           Skipped => 2,
           Scaled  => True,
           Widened => True);
      Widget.all.Right_Scale.Minor_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (Right_Tick_Offset + 0.04, First),
           Length  => Length,
           Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
           Breadth => 0.07,
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 600.0,
           Step    => 0.1 * Length / Gdouble (Sectors),
           Skipped => 5,
           Scaled  => True,
           Widened => True);
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
   end Create_Background;

   procedure Create_Foreground
     (Widget     : not null access Gtk_Meter_Thermo_Symmetric_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Color      : Gdk.Color.Gdk_Color) is
   begin
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
   end Create_Foreground;

   function Get_Background
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return not null access Gtk.Layered.Rectangular_Background.Rectangular_Background_Layer
   is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Bar
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return not null access Gtk.Layered.Bar.Bar_Layer is
   begin
      return Widget.all.Bar;
   end Get_Bar;

   function Get_Bar_Color
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return Gdk.Color.Gdk_Color is
   begin
      return Widget.all.Bar.all.Get_Line.Color;
   end Get_Bar_Color;

   function Get_Cache
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Left_Annotation
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return not null access Gtk.Layered.Flat_Annotation.Flat_Annotation_Layer
   is
   begin
      return Widget.all.Left_Scale.Annotation;
   end Get_Left_Annotation;

   function Get_Left_Label
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return not null access Gtk.Layered.Label.Label_Layer is
   begin
      return Widget.all.Left_Scale.Label;
   end Get_Left_Label;

   function Get_Right_Annotation
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return not null access Gtk.Layered.Flat_Annotation.Flat_Annotation_Layer
   is
   begin
      return Widget.all.Right_Scale.Annotation;
   end Get_Right_Annotation;

   function Get_Right_Label
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
      return not null access Gtk.Layered.Label.Label_Layer is
   begin
      return Widget.all.Right_Scale.Label;
   end Get_Right_Label;

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
               Blurb      => "The color of the circle bounding the ticks"));
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

   procedure Gtk_New
     (Widget      : out Gtk_Meter_Thermo_Symmetric;
      Texts       : Gtk.Enums.String_List.Glist;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors     : Positive                      := 10;
      Left_Label  : String                        := Celsius;
      Right_Label : String                        := Celsius;
      Color       : Gdk.Color.Gdk_Color           := Gtk.Missed.RGB (1.0, 0.0, 0.0))
   is
   begin
      Widget := new Gtk_Meter_Thermo_Symmetric_Record;
      Initialize
        (Widget,
         Texts,
         Adjustment,
         Sectors,
         Left_Label,
         Right_Label,
         Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget      : out Gtk_Meter_Thermo_Symmetric;
      Texts       : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors     : Positive                      := 10;
      Left_Label  : String                        := Celsius;
      Right_Label : String                        := Celsius;
      Color       : Gdk.Color.Gdk_Color           := Gtk.Missed.RGB (1.0, 0.0, 0.0))
   is
   begin
      Widget := new Gtk_Meter_Thermo_Symmetric_Record;
      Initialize
        (Widget,
         Texts,
         Adjustment,
         Sectors,
         Left_Label,
         Right_Label,
         Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget      : out Gtk_Meter_Thermo_Symmetric;
      Texts       : UTF8_String;
      Delimiter   : Character                     := ' ';
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors     : Positive                      := 10;
      Left_Label  : String                        := Celsius;
      Right_Label : String                        := Celsius;
      Color       : Gdk.Color.Gdk_Color           := Gtk.Missed.RGB (1.0, 0.0, 0.0))
   is
   begin
      Widget := new Gtk_Meter_Thermo_Symmetric_Record;
      Initialize
        (Widget,
         Texts,
         Delimiter,
         Adjustment,
         Sectors,
         Left_Label,
         Right_Label,
         Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget      : not null access Gtk_Meter_Thermo_Symmetric_Record'Class;
      Texts       : Gtk.Enums.String_List.Glist;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Sectors     : Positive;
      Left_Label  : String;
      Right_Label : String;
      Color       : Gdk.Color.Gdk_Color) is
   begin
      Create_Background (Widget, Sectors, Color);
      Widget.all.Left_Scale.Annotation :=
        Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
          (Under       => Widget.all.Cache,
           From        => (Left_Annotation_Offset, First),
           Length      => Length,
           Scale_Angle => 3.0 * Ada.Numerics.Pi / 2.0,
           Texts       => Texts,
           Justify     => Ada.Strings.Right,
           Step        => Length / Gdouble (Sectors),
           Height      => 0.1,
           Color       => Text_Color,
           Scaled      => True,
           Face        =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Normal));
      Widget.all.Left_Scale.Label :=
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
           Text     => Left_Label,
           Location => (Left_Label_Offset, Label_Height));
      Widget.all.Right_Scale.Annotation :=
        Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
          (Under       => Widget.all.Cache,
           From        => (Right_Annotation_Offset, First),
           Length      => Length,
           Scale_Angle => 3.0 * Ada.Numerics.Pi / 2.0,
           Texts       => Texts,
           Justify     => Ada.Strings.Left,
           Step        => Length / Gdouble (Sectors),
           Height      => 0.1,
           Color       => Text_Color,
           Scaled      => True,
           Face        =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Normal));
      Widget.all.Right_Scale.Label :=
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
           Text     => Right_Label,
           Location => (Right_Label_Offset, Label_Height));
      Create_Foreground (Widget, Adjustment, Color);
   end Initialize;

   procedure Initialize
     (Widget      : not null access Gtk_Meter_Thermo_Symmetric_Record'Class;
      Texts       : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Sectors     : Positive;
      Left_Label  : String;
      Right_Label : String;
      Color       : Gdk.Color.Gdk_Color) is
   begin
      Initialize
        (Widget,
         Gtk.Enums.String_Lists.Get_GList (Texts),
         Adjustment,
         Sectors,
         Left_Label,
         Right_Label,
         Color);
   end Initialize;

   procedure Initialize
     (Widget      : not null access Gtk_Meter_Thermo_Symmetric_Record'Class;
      Texts       : UTF8_String;
      Delimiter   : Character;
      Adjustment  : Gtk.Adjustment.Gtk_Adjustment;
      Sectors     : Positive;
      Left_Label  : String;
      Right_Label : String;
      Color       : Gdk.Color.Gdk_Color) is
   begin
      Create_Background (Widget, Sectors, Color);
      Widget.all.Left_Scale.Annotation :=
        Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
          (Under       => Widget.all.Cache,
           From        => (Left_Annotation_Offset, First),
           Length      => Length,
           Scale_Angle => 3.0 * Ada.Numerics.Pi / 2.0,
           Texts       => Texts,
           Delimiter   => Delimiter,
           Justify     => Ada.Strings.Right,
           Step        => Length / Gdouble (Sectors),
           Height      => 0.1,
           Color       => Text_Color,
           Scaled      => True,
           Face        =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Normal));
      Widget.all.Left_Scale.Label :=
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
           Text     => Left_Label,
           Location => (Left_Label_Offset, Label_Height));
      Widget.all.Right_Scale.Annotation :=
        Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
          (Under       => Widget.all.Cache,
           From        => (Right_Annotation_Offset, First),
           Length      => Length,
           Scale_Angle => 3.0 * Ada.Numerics.Pi / 2.0,
           Texts       => Texts,
           Delimiter   => Delimiter,
           Justify     => Ada.Strings.Left,
           Step        => Length / Gdouble (Sectors),
           Height      => 0.1,
           Color       => Text_Color,
           Scaled      => True,
           Face        =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Normal));
      Widget.all.Right_Scale.Label :=
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
           Text     => Right_Label,
           Location => (Right_Label_Offset, Label_Height));
      Create_Foreground (Widget, Adjustment, Color);
   end Initialize;

   procedure Set_Bar_Color
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record;
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
         Line => (Width    => Widget.all.Bulb.all.Get_Line.Width,
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
         Line   => (Width    => Widget.all.Stem.all.Get_Line.Width,
                    Color    => Color,
                    Line_Cap => Widget.all.Stem.all.Get_Line.Line_Cap));
   end Set_Bar_Color;

   procedure Set_Value
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record;
      Value  : Gdouble) is
   begin
      Widget.all.Bar.all.Set_Value (Value);
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Meter_Thermo_Symmetric_Record)
   is
      procedure Set (Scale : in out Temperature_Scale) is
      begin
         Scale.Minor_Ticks.all.Set
           (From    => Scale.Minor_Ticks.all.Get_From,
            Length  => Scale.Minor_Ticks.all.Get_Length,
            Breadth => Scale.Minor_Ticks.all.Get_Breadth,
            Angle   => Scale.Minor_Ticks.all.Get_Angle,
            Ticks   => Scale.Minor_Ticks.all.Get_Ticks,
            Line =>
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
            Line =>
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
            Line =>
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
      Set (Widget.all.Left_Scale);
      Set (Widget.all.Right_Scale);
   end Style_Changed;

   pragma Warnings (On, "declaration hides ""Adjustment""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Meter.Thermo_Symmetric;
