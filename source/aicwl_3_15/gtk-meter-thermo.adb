--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Thermo_Dual                       Luebeck            --
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
--____________________________________________________________________--

with Ada.Numerics;              use Ada.Numerics;
with Ada.Strings;               use Ada.Strings;
with Cairo;                     use Cairo;
with Cairo.Line_Cap_Property;   use Cairo.Line_Cap_Property;
with Gdk.Color.IHLS;            use Gdk.Color.IHLS;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GtkAda.Types;              use GtkAda.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with GLib.Object.Checked_Destroy;

with Gtk.Widget.Styles.Line_Cap_Property;
use  Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Meter.Thermo is

   Background_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Text_Color        : constant Gdk_Color := RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Corner            : constant := 1.0 / 10.0;
   Length            : constant := 2.5;
   First             : constant := Length * 0.5;
   Bar_Width         : constant := 0.09;
   Reflection_Offset : constant := Bar_Width * 0.35;
   Bar_Offset        : constant :=-0.25;
   Annotation_Offset : constant := 0.40;
   Tick_Offset       : constant :=-0.15;
   Stem_Length       : constant := 0.15;
   Label_Offset      : constant := 0.30;
   Label_Height      : constant :=-1.45;
   Reflection_Shift  : constant Gdk_Luminance := Gdk_Luminance'Last / 2;

   Color_Span : constant Gdk_Luminance := Gdk_Luminance'Last / 2;

   procedure Create_Background
             (  Widget  : not null access Gtk_Meter_Thermo_Record'Class;
                Sectors : Positive;
                Color   : Gdk_Color
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 1.0 / 3.0);
      Widget.Background :=
         Add_Rectangular_Background
         (  Under         => Widget,
            Height        => 3.0,
            Width         => 1.0,
            Center        => (0.0, 0.0),
            Corner_Radius => Corner,
            Color         => Background_Color,
            Border_Width  => 0.03,
            Border_Depth  => 0.01,
            Border_Shadow => Shadow_Etched_Out,
            Deepened      => True,
            Widened       => True,
            Scaled        => True
         );
      Widget.Major_Ticks :=
         Add_Flat_Scale
         (  Under   => Widget.Background.Get_Foreground,
            From    => (Tick_Offset + 0.12, First),
            Length  => Length,
            Angle   => 3.0 * Pi / 2.0,
            Breadth => 0.24,
            Color   => Major_Tick_Color,
            Width   => 4.0 / 600.0,
            Step    => Length / GDouble (Sectors),
            Scaled  => True,
            Widened => True
         );
      Widget.Middle_Ticks :=
         Add_Flat_Scale
         (  Under   => Widget.Background.Get_Foreground,
            From    => (Tick_Offset + 0.08, First),
            Length  => Length,
            Angle   => 3.0 * Pi / 2.0,
            Breadth => 0.16,
            Color   => Middle_Tick_Color,
            Width   => 2.0 / 600.0,
            Step    => 0.5 * Length / GDouble (Sectors),
            Skipped => 2,
            Scaled  => True,
            Widened => True
         );
      Widget.Minor_Ticks :=
         Add_Flat_Scale
         (  Under   => Widget.Background.Get_Foreground,
            From    => (Tick_Offset + 0.04, First),
            Length  => Length,
            Angle   => 3.0 * Pi / 2.0,
            Breadth => 0.08,
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 600.0,
            Step    => 0.1 * Length / GDouble (Sectors),
            Skipped => 5,
            Scaled  => True,
            Widened => True
         );
      Widget.Stem :=
         Add_Line
         (  Under    => Widget.Background.Get_Foreground,
            From     => (Bar_Offset, First + Stem_Length),
            To       => (Bar_Offset, First),
            Width    => Bar_Width,
            Color    => Color,
            Line_Cap => CAIRO_LINE_CAP_BUTT,
            Scaled   => True,
            Widened  => True
         );
      Widget.Bulb :=
         Add_Line
         (  Under    => Widget.Background.Get_Foreground,
            From     => (Bar_Offset, First + Stem_Length),
            To       => (Bar_Offset, First + Stem_Length),
            Width    => Bar_Width * 2.0,
            Color    => Color,
            Line_Cap => CAIRO_LINE_CAP_ROUND,
            Scaled   => True,
            Widened  => True
         );
      Widget.Reflection :=
         Add_Line
         (  Under    => Widget.Background.Get_Foreground,
            From     => (  Bar_Offset          - Reflection_Offset,
                           First + Stem_Length - Reflection_Offset
                        ),
            To       => (  Bar_Offset          - Reflection_Offset,
                           First + Stem_Length - Reflection_Offset
                        ),
            Width    => Reflection_Offset * 2.0,
            Line_Cap => CAIRO_LINE_CAP_ROUND,
            Scaled   => True,
            Widened  => True,
            Color    => Lighten
                        (  Widget.Bulb.Get_Line.Color,
                           Reflection_Shift,
                           True
         )              );
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
   end Create_Background;

   procedure Create_Foreground
             (  Widget : not null access Gtk_Meter_Thermo_Record'Class;
                Adjustment : Gtk_Adjustment;
                Color      : Gdk_Color
             )  is
   begin
      Widget.Bar :=
         Add_Bar
         (  Under      => Widget.Background.Get_Foreground,
            From       => (Bar_Offset, First),
            Length     => Length,
            Angle      => 3.0 * Pi / 2.0,
            Adjustment => Adjustment,
            Line_Cap   => CAIRO_LINE_CAP_BUTT,
            Width      => Bar_Width,
            Color      => Color,
            Scaled     => True,
            Widened    => True
         );
   end Create_Foreground;

   function Get_Annotation
            (  Widget : not null access Gtk_Meter_Thermo_Record
            )  return not null access Flat_Annotation_Layer is
   begin
      return Widget.Annotation;
   end Get_Annotation;

   function Get_Background
            (  Widget : not null access Gtk_Meter_Thermo_Record
            )  return not null access Rectangular_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Bar
            (  Widget : not null access Gtk_Meter_Thermo_Record
            )  return not null access Bar_Layer is
   begin
      return Widget.Bar;
   end Get_Bar;

   function Get_Bar_Color
            (  Widget : not null access Gtk_Meter_Thermo_Record
            )  return Gdk_Color is
   begin
      return Widget.Bar.Get_Line.Color;
   end Get_Bar_Color;

   function Get_Cache
            (  Widget : not null access Gtk_Meter_Thermo_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Label
            (  Widget : not null access Gtk_Meter_Thermo_Record
            )  return not null access Label_Layer is
   begin
      return Widget.Label;
   end Get_Label;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Layered.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "backgound-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Background color",
               Blurb      => "The background color"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "line-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Line color",
               Blurb      =>
                  "The color of the circle bounding the ticks"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "major-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Major ticks color",
               Blurb      => "Major ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "major-tick-line-cap",
               Nick    => "Major tick cap",
               Blurb   => "The line cap style used for major ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "middle-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Middle ticks color",
               Blurb      => "Middle ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "middle-tick-line-cap",
               Nick    => "Middle tick cap",
               Blurb   => "The line cap style used for middle ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "minor-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Minor ticks color",
               Blurb      => "Minor ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "minor-tick-line-cap",
               Nick    => "Minor tick cap",
               Blurb   => "The line cap style used for minor ticks",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "text-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Text color",
               Blurb      => "Text color"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Meter_Thermo;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 10;
                Label      : String         := Celsius;
                Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0)
             )  is
   begin
      Widget := new Gtk_Meter_Thermo_Record;
      Initialize (Widget, Texts, Adjustment, Sectors, Label, Color);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Meter_Thermo;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 10;
                Label      : String         := Celsius;
                Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0)
             )  is
   begin
      Widget := new Gtk_Meter_Thermo_Record;
      Initialize (Widget, Texts, Adjustment, Sectors, Label, Color);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Meter_Thermo;
                Texts      : UTF8_String;
                Delimiter  : Character      := ' ';
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 10;
                Label      : String         := Celsius;
                Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0)
             )  is
   begin
      Widget := new Gtk_Meter_Thermo_Record;
      Initialize
      (  Widget,
         Texts,
         Delimiter,
         Adjustment,
         Sectors,
         Label,
         Color
      );
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Meter_Thermo_Record'Class;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive;
                Label      : String;
                Color      : Gdk_Color
             )  is
   begin
      Create_Background (Widget, Sectors, Color);
      Widget.Annotation :=
         Add_Flat_Annotation
         (  Under       => Widget.Cache,
            From        => (Annotation_Offset, First),
            Length      => Length,
            Scale_Angle => 3.0 * Pi / 2.0,
            Texts       => Texts,
            Justify     => Right,
            Step        => Length / GDouble (Sectors),
            Height      => 0.1,
            Color       => Text_Color,
            Scaled      => True,
            Face        => Create_Toy
                           (  Family => "arial",
                              Slant  => CAIRO_FONT_SLANT_NORMAL,
                              Weight => CAIRO_FONT_WEIGHT_NORMAL
         )                 );
      Widget.Label :=
         Add_Label
         (  Under    => Widget.Get_Cache,
            Face     => Create_Toy
                        (  Family => "arial",
                           Slant  => CAIRO_FONT_SLANT_NORMAL,
                           Weight => CAIRO_FONT_WEIGHT_BOLD
                        ),
            Color    => Text_Color,
            Height   => 0.12,
            Mode     => Moved_Centered,
            Scaled   => True,
            Text     => Label,
            Location => (Label_Offset, Label_Height)
         );
      Create_Foreground (Widget, Adjustment, Color);
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Meter_Thermo_Record'Class;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive;
                Label      : String;
                Color      : Gdk_Color
             )  is
   begin
      Initialize
      (  Widget,
         Get_GList (Texts),
         Adjustment,
         Sectors,
         Label,
         Color
      );
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Meter_Thermo_Record'Class;
                Texts      : UTF8_String;
                Delimiter  : Character;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive;
                Label      : String;
                Color      : Gdk_Color
             )  is
   begin
      Create_Background (Widget, Sectors, Color);
      Widget.Annotation :=
         Add_Flat_Annotation
         (  Under       => Widget.Cache,
            From        => (Annotation_Offset, First),
            Length      => Length,
            Texts       => Texts,
            Justify     => Right,
            Delimiter   => Delimiter,
            Scale_Angle => 3.0 * Pi / 2.0,
            Step        => Length / GDouble (Sectors),
            Height      => 0.1,
            Color       => Text_Color,
            Scaled      => True,
            Face        => Create_Toy
                           (  Family => "arial",
                              Slant  => CAIRO_FONT_SLANT_NORMAL,
                              Weight => CAIRO_FONT_WEIGHT_NORMAL
         )                 );
      Widget.Label :=
         Add_Label
         (  Under    => Widget.Get_Cache,
            Face     => Create_Toy
                        (  Family => "arial",
                           Slant  => CAIRO_FONT_SLANT_NORMAL,
                           Weight => CAIRO_FONT_WEIGHT_BOLD
                        ),
            Color    => Text_Color,
            Height   => 0.12,
            Mode     => Moved_Centered,
            Scaled   => True,
            Text     => Label,
            Location => (Label_Offset, Label_Height)
         );
      Create_Foreground (Widget, Adjustment, Color);
   end Initialize;

   procedure Set_Bar_Color
             (  Widget : not null access Gtk_Meter_Thermo_Record;
                Color  : Gdk_Color
             )  is
   begin
      Widget.Bar.Set
      (  From => Widget.Bar.Get_From,
         To   => Widget.Bar.Get_To,
         Line => (  Width    => Widget.Bar.Get_Line.Width,
                    Color    => Color,
                    Line_Cap => Widget.Bar.Get_Line.Line_Cap
      )          );
      Widget.Bulb.Set
      (  From => Widget.Bulb.Get_From,
         To   => Widget.Bulb.Get_To,
         Line => (  Width    => Widget.Bulb.Get_Line.Width,
                    Color    => Color,
                    Line_Cap => Widget.Bulb.Get_Line.Line_Cap
      )          );
      Widget.Reflection.Set
      (  From => Widget.Reflection.Get_From,
         To   => Widget.Reflection.Get_To,
         Line => (  Width    => Widget.Reflection.Get_Line.Width,
                    Line_Cap => Widget.Reflection.Get_Line.Line_Cap,
                    Color    => Lighten
                                (  Color,
                                   Reflection_Shift,
                                   True
      )          )              );
      Widget.Stem.Set
      (  From   => Widget.Stem.Get_From,
         To     => Widget.Stem.Get_To,
         Line   => (  Width    => Widget.Stem.Get_Line.Width,
                      Color    => Color,
                      Line_Cap => Widget.Stem.Get_Line.Line_Cap
      )            );
   end Set_Bar_Color;

   procedure Set_Value
             (  Widget : not null access Gtk_Meter_Thermo_Record;
                Value  : GDouble
             )  is
   begin
      Widget.Bar.Set_Value (Value);
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access Gtk_Meter_Thermo_Record
             )  is
   begin
      Widget.Background.Set
      (  Height         => Widget.Background.Get_Height,
         Width          => Widget.Background.Get_Width,
         Center         => Widget.Background.Get_Center,
         Rotation_Angle => Widget.Background.Get_Rotation_Angle,
         Corner_Radius  => Widget.Background.Get_Corner_Radius,
         Border_Width   => Widget.Background.Get_Border_Width,
         Border_Depth   => Widget.Background.Get_Border_Depth,
         Border_Color   => Widget.Background.Get_Border_Color,
         Border_Shadow  => Widget.Background.Get_Border_Shadow,
         Color  =>
            Style_Get (Widget, "backgound-color", Background_Color)
      );
      Widget.Minor_Ticks.Set
      (  From    => Widget.Minor_Ticks.Get_From,
         Length  => Widget.Minor_Ticks.Get_Length,
         Breadth => Widget.Minor_Ticks.Get_Breadth,
         Angle   => Widget.Minor_Ticks.Get_Angle,
         Ticks   => Widget.Minor_Ticks.Get_Ticks,
         Line =>
            (  Widget.Minor_Ticks.Get_Line.Width,
               Style_Get (Widget, "minor-tick-color", Minor_Tick_Color),
               Style_Get (Widget, "minor-tick-line-cap")
      )     );
      Widget.Middle_Ticks.Set
      (  From    => Widget.Middle_Ticks.Get_From,
         Length  => Widget.Middle_Ticks.Get_Length,
         Breadth => Widget.Middle_Ticks.Get_Breadth,
         Angle   => Widget.Middle_Ticks.Get_Angle,
         Ticks   => Widget.Middle_Ticks.Get_Ticks,
         Line =>
            (  Widget.Middle_Ticks.Get_Line.Width,
               Style_Get
               (  Widget,
                  "middle-tick-color",
                  Middle_Tick_Color
               ),
               Style_Get (Widget, "middle-tick-line-cap")
      )     );
      Widget.Major_Ticks.Set
      (  From    => Widget.Major_Ticks.Get_From,
         Length  => Widget.Major_Ticks.Get_Length,
         Breadth => Widget.Major_Ticks.Get_Breadth,
         Angle   => Widget.Major_Ticks.Get_Angle,
         Ticks   => Widget.Major_Ticks.Get_Ticks,
         Line =>
            (  Widget.Major_Ticks.Get_Line.Width,
               Style_Get (Widget, "major-tick-color", Major_Tick_Color),
               Style_Get (Widget, "major-tick-line-cap")
      )     );
      Widget.Annotation.Set
      (  Ticks       => Widget.Annotation.Get_Ticks,
         From        => Widget.Annotation.Get_From,
         Length      => Widget.Annotation.Get_Length,
         Face        => Widget.Annotation.Get_Face,
         Scale_Angle => Widget.Annotation.Get_Scale_Angle,
         Height      => Widget.Annotation.Get_Height,
         Stretch     => Widget.Annotation.Get_Stretch,
         Text_Angle  => Widget.Annotation.Get_Text_Angle,
         Justify     => Widget.Annotation.Get_Justify,
         Color       => Style_Get (Widget, "text-color", Text_Color)
      );
      Widget.Label.Set
      (  Location => Widget.Label.Get_Location,
         Face     => Widget.Label.Get_Face,
         Height   => Widget.Label.Get_Height,
         Stretch  => Widget.Label.Get_Stretch,
         Mode     => Widget.Label.Get_Mode,
         Angle    => Widget.Label.Get_Angle,
         Skew     => Widget.Label.Get_Skew,
         Color    => Style_Get (Widget, "text-color", Text_Color)
      );
   end Style_Changed;

end Gtk.Meter.Thermo;
