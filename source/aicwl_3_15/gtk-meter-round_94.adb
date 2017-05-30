--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Round_94                       Luebeck            --
--  Implementation                                 Winter, 2011       --
--                                                                    --
--                                Last revision :  22:07 23 Jul 2014  --
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

with Cairo.Ellipses;
with Cairo.Line_Cap_Property;

with Gdk.Color;

with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;

with Gtk.Missed;
with Gtk.Widget.Styles.Line_Cap_Property;

with Pango.Cairo.Fonts;

package body Gtk.Meter.Round_94 is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Needle_Color      : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0);
   Background_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Line_Color        : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Pin_Color         : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.6, 0.6, 0.6);
   Text_Color        : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Center : constant Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0855);
   Corner : constant := 1.0 / 30.0;
   pragma Unreferenced (Corner);
   Length : constant := 94.0 * Ada.Numerics.Pi / 180.0;
   Pin    : constant := 0.4;
   First  : constant := (Ada.Numerics.Pi * 3.0 - Length) / 2.0;
   From   : constant := 41.0 * Ada.Numerics.Pi / 180.0;
   Shift  : constant := -32.0 * Ada.Numerics.Pi / 180.0;

   procedure Create_Background
     (Widget  : not null access Gtk_Meter_Round_94_Record'Class;
      Sectors : Positive);
   procedure Create_Background
     (Widget : not null access Gtk_Meter_Round_94_Record'Class;
      Sectors : Positive) is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 1.207);
      Widget.all.Background :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget,
           Outer         => (Center, 1.0 / 0.5, 0.5, 0.0),
           From          => Ada.Numerics.Pi - From,
           Length        => Ada.Numerics.Pi + 2.0 * From,
           Color         => Background_Color,
           Border_Width  => 0.01,
           Border_Depth  => 0.005,
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.all.Line_1 :=
        Gtk.Layered.Arc.Add_Arc
          (Under    => Widget.all.Background.all.Get_Foreground,
           Color    => Line_Color,
           Width    => 1.5 / 200.0,
           Ellipse  => ((0.0, Pin), 1.0 / 0.47, 0.47, 0.0),
           From     => First,
           Length   => Length,
           Line_Cap => Cairo.Cairo_Line_Cap_Round,
           Widened  => True,
           Scaled   => True);
      Widget.all.Line_2 :=
        Gtk.Layered.Arc.Add_Arc
          (Under   => Widget.all.Background.all.Get_Foreground,
           Color   => Line_Color,
           Width   => 1.0 / 400.0,
           Ellipse => ((0.0, Pin), 1.0 / 0.52, 0.52, 0.0),
           From    => First,
           Length  => Length,
           Widened => True,
           Scaled  => True);
      Widget.all.Major_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under => Widget.all.Background.all.Get_Foreground,
           Outer =>
             (Center          => (0.0, Pin),
              Major_Curvature => 1.0 / 0.56,
              Minor_Radius    => 0.56,
              Angle           => 0.0),
           Inner =>
             (Center          => (0.0, Pin),
              Major_Curvature => 1.0 / 0.47,
              Minor_Radius    => 0.47,
              Angle           => 0.0),
           Color   => Major_Tick_Color,
           Width   => 1.5 / 200.0,
           Step    => Length / Gdouble (Sectors),
           From    => First,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Middle_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Outer   =>
             (Center          => (0.0, Pin),
              Major_Curvature => 1.0 / 0.54,
              Minor_Radius    => 0.54,
              Angle           => 0.0),
           Inner   =>
             (Center          => (0.0, Pin),
              Major_Curvature => 1.0 / 0.47,
              Minor_Radius    => 0.47,
              Angle           => 0.0),
           Color   => Middle_Tick_Color,
           Width   => 1.5 / 400.0,
           Step    => 0.5 * Length / Gdouble (Sectors),
           Skipped => 2,
           From    => First,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Minor_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Outer   =>
             (Center          => (0.0, Pin),
              Major_Curvature => 1.0 / 0.52,
              Minor_Radius    => 0.52,
              Angle           => 0.0),
           Inner   =>
             (Center          => (0.0, Pin),
              Major_Curvature => 1.0 / 0.47,
              Minor_Radius    => 0.47,
              Angle           => 0.0),
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 400.0,
           Step    => 0.1 * Length / Gdouble (Sectors),
           Skipped => 5,
           From    => First,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Pin :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget.all.Background.all.Get_Foreground,
           Outer         => ((0.0, Pin), 1.0 / 0.11, 0.11, 0.0),
           From          => Ada.Numerics.Pi - (From + Shift),
           Length        => Ada.Numerics.Pi + 2.0 * (From + Shift),
           Color         => Pin_Color,
           Border_Width  => 0.012,
           Border_Depth  => 0.005,
           Border_Shadow => Gtk.Enums.Shadow_Etched_In,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
   end Create_Background;

   procedure Create_Foreground
     (Widget     : not null access Gtk_Meter_Round_94_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Create_Foreground
     (Widget     : not null access Gtk_Meter_Round_94_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget.all.Needle :=
        Gtk.Layered.Needle.Add_Needle
          (Under       => Widget.all.Background.all.Get_Foreground,
           Center      => (0.0, Pin),
           Tip_Cap     => Cairo.Cairo_Line_Cap_Square,
           Adjustment  => Adjustment,
           Tip_Length  => 0.52,
           Tip_Width   => 0.008,
           Rear_Length => 0.0,
           Rear_Width  => 0.025,
           Rear_Cap    => Cairo.Cairo_Line_Cap_Round,
           Color       => Needle_Color,
           From        => First,
           Length      => Length,
           Scaled      => True);
   end Create_Foreground;

   function Get_Annotation
     (Widget : not null access Gtk_Meter_Round_94_Record)
      return not null access Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer
   is
   begin
      return Widget.all.Annotation;
   end Get_Annotation;

   function Get_Background
     (Widget : not null access Gtk_Meter_Round_94_Record)
      return not null access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer
   is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Cache
     (Widget : not null access Gtk_Meter_Round_94_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Needle
     (Widget : not null access Gtk_Meter_Round_94_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer is
   begin
      return Widget.all.Needle;
   end Get_Needle;

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
              (Name       => "needle-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Needle color",
               Blurb      => "The color of the gauge's needle"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the needle tip",
               Default => Cairo.Cairo_Line_Cap_Round));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the needle rear",
               Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "backgound-color",
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
              (Name       => "pin-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Pin color",
               Blurb      => "Arrow pin color"));
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
     (Widget     : out Gtk_Meter_Round_94;
      Texts      : Gtk.Enums.String_List.Glist;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive                      := 5) is
   begin
      Widget := new Gtk_Meter_Round_94_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Meter_Round_94;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive                      := 5) is
   begin
      Widget := new Gtk_Meter_Round_94_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Meter_Round_94;
      Texts      : UTF8_String;
      Delimiter  : Character                     := ' ';
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive                      := 5) is
   begin
      Widget := new Gtk_Meter_Round_94_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget     : not null access Gtk_Meter_Round_94_Record'Class;
      Texts      : Gtk.Enums.String_List.Glist;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget.all.Cache,
           Ellipse   =>
             (Center          => (0.0, 0.0 + Pin),
              Major_Curvature => 1.0 / 0.6,
              Minor_Radius    => 0.6,
              Angle           => 0.0),
           Texts     => Texts,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Length / Gdouble (Sectors),
           Height    => 0.05,
           Stretch   => 1.0,
           Color     => Text_Color,
           From      => First,
           Length    => Length,
           Mode      => Gtk.Layered.Rotated,
           Scaled    => True);
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Meter_Round_94_Record'Class;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Initialize
        (Widget, Gtk.Enums.String_Lists.Get_GList (Texts), Adjustment, Sectors);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Meter_Round_94_Record'Class;
      Texts      : UTF8_String;
      Delimiter  : Character;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget.all.Cache,
           Ellipse   =>
             (Center          => (0.0, 0.0 + Pin),
              Major_Curvature => 1.0 / 0.6,
              Minor_Radius    => 0.6,
              Angle           => 0.0),
           Texts     => Texts,
           Delimiter => Delimiter,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Length / Gdouble (Sectors),
           Height    => 0.05,
           Stretch   => 1.0,
           Color     => Text_Color,
           From      => First,
           Length    => Length,
           Mode      => Gtk.Layered.Rotated,
           Scaled    => True);
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
     (Widget : not null access Gtk_Meter_Round_94_Record;
      Value  : Gdouble) is
   begin
      Widget.all.Needle.all.Set_Value (Value);
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Meter_Round_94_Record) is
   begin
      Widget.all.Needle.all.Set
        (Center => Widget.all.Needle.all.Get_Center,
         From   => Widget.all.Needle.all.Get_From,
         Length => Widget.all.Needle.all.Get_Length,
         Tip    =>
           (Length => Widget.all.Needle.all.Get_Tip.Length,
            Width  => Widget.all.Needle.all.Get_Tip.Width,
            Cap    =>
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "needle-tip-cap")),
         Rear   =>
           (Length => Widget.all.Needle.all.Get_Rear.Length,
            Width  => Widget.all.Needle.all.Get_Rear.Width,
            Cap    =>
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get (Widget, "needle-color", Needle_Color));
      Widget.all.Background.all.Set
        (Outer         => Widget.all.Background.all.Get_Outer,
         Inner         => Widget.all.Background.all.Get_Inner,
         From          => Widget.all.Background.all.Get_From,
         Length        => Widget.all.Background.all.Get_Length,
         Border_Width  => Widget.all.Background.all.Get_Border_Width,
         Border_Depth  => Widget.all.Background.all.Get_Border_Depth,
         Border_Color  => Widget.all.Background.all.Get_Border_Color,
         Border_Shadow => Widget.all.Background.all.Get_Border_Shadow,
         Color         =>
           Gtk.Widget.Styles.Style_Get
             (Widget, "backgound-color", Background_Color));
      Widget.all.Line_1.all.Set
        (Ellipse => Widget.all.Line_1.all.Get_Ellipse,
         From    => Widget.all.Line_1.all.Get_From,
         Length  => Widget.all.Line_1.all.Get_Length,
         Line    =>
           (Widget.all.Line_1.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get (Widget, "line-color", Line_Color),
            Widget.all.Line_1.all.Get_Line.Line_Cap));
      Widget.all.Line_2.all.Set
        (Ellipse => Widget.all.Line_2.all.Get_Ellipse,
         From    => Widget.all.Line_2.all.Get_From,
         Length  => Widget.all.Line_2.all.Get_Length,
         Line    =>
           (Widget.all.Line_2.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get (Widget, "line-color", Line_Color),
            Widget.all.Line_2.all.Get_Line.Line_Cap));
      Widget.all.Minor_Ticks.all.Set
        (Inner  => Widget.all.Minor_Ticks.all.Get_Inner,
         Outer  => Widget.all.Minor_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Minor_Ticks.all.Get_Ticks,
         From   => Widget.all.Minor_Ticks.all.Get_From,
         Length => Widget.all.Minor_Ticks.all.Get_Length,
         Line =>
           (Widget.all.Minor_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get
              (Widget, "minor-tick-color", Minor_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "minor-tick-line-cap")));
      Widget.all.Middle_Ticks.all.Set
        (Inner  => Widget.all.Middle_Ticks.all.Get_Inner,
         Outer  => Widget.all.Middle_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Middle_Ticks.all.Get_Ticks,
         From   => Widget.all.Middle_Ticks.all.Get_From,
         Length => Widget.all.Middle_Ticks.all.Get_Length,
         Line =>
           (Widget.all.Middle_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get
              (Widget, "middle-tick-color", Middle_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "middle-tick-line-cap")));
      Widget.all.Major_Ticks.all.Set
        (Inner  => Widget.all.Major_Ticks.all.Get_Inner,
         Outer  => Widget.all.Major_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Major_Ticks.all.Get_Ticks,
         From   => Widget.all.Major_Ticks.all.Get_From,
         Length => Widget.all.Major_Ticks.all.Get_Length,
         Line   =>
           (Widget.all.Major_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get
              (Widget, "major-tick-color", Major_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "major-tick-line-cap")));
      Widget.all.Pin.all.Set
        (Outer          => Widget.all.Pin.all.Get_Outer,
         Inner          => Widget.all.Pin.all.Get_Inner,
         From           => Widget.all.Pin.all.Get_From,
         Length         => Widget.all.Pin.all.Get_Length,
         Border_Width   => Widget.all.Pin.all.Get_Border_Width,
         Border_Depth   => Widget.all.Pin.all.Get_Border_Depth,
         Border_Color   => Widget.all.Pin.all.Get_Border_Color,
         Border_Shadow  => Widget.all.Pin.all.Get_Border_Shadow,
         Color          =>
           Gtk.Widget.Styles.Style_Get (Widget, "pin-color", Background_Color));
      Widget.all.Annotation.all.Set
        (Ellipse => Widget.all.Annotation.all.Get_Ellipse,
         Ticks   => Widget.all.Annotation.all.Get_Ticks,
         From    => Widget.all.Annotation.all.Get_From,
         Length  => Widget.all.Annotation.all.Get_Length,
         Face    => Widget.all.Annotation.all.Get_Face,
         Mode    => Widget.all.Annotation.all.Get_Mode,
         Height  => Widget.all.Annotation.all.Get_Height,
         Stretch => Widget.all.Annotation.all.Get_Stretch,
         Color   =>
           Gtk.Widget.Styles.Style_Get (Widget, "text-color", Text_Color));
   end Style_Changed;

   pragma Warnings (On, "declaration hides ""Adjustment""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Meter.Round_94;
