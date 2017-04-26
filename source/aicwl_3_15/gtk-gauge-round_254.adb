--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Round_254                         Luebeck            --
--  Implementation                                 Winter, 2010       --
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
with Cairo.Line_Cap_Property;   use Cairo.Line_Cap_Property;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GtkAda.Types;              use GtkAda.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with GLib.Object.Checked_Destroy;

with Gtk.Widget.Styles.Line_Cap_Property;
use  Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Gauge.Round_254 is

   Needle_Color     : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Pin_From_Color   : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Pin_To_Color     : constant Gdk_Color := RGB (0.5, 0.5, 0.5);
   Background_Color : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Major_Tick_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Major_Text_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Minor_Text_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Stretch          : constant := 0.8;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Length : constant := Pi * 254.0 / 180.0;
   Start  : constant := Pi * (1.0 - 37.0 / 180.0);

   procedure Create_Background
             (  Widget  : not null access
                          Gtk_Gauge_Round_254_Record'Class;
                Sectors : Positive
             )  is
      Step : constant GDouble := Length / GDouble (Sectors);
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Sectors := Sectors;
      Widget.Background :=
         Add_Elliptic_Background
         (  Under         => Widget,
            Color         => Background_Color,
            Outer         => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
            Border_Width  => 0.01,
            Border_Depth  => 0.005,
            Border_Shadow => Shadow_Etched_Out,
            Deepened      => True,
            Widened       => True,
            Scaled        => True
         );
      Widget.Pin :=
         Add_Cap
         (  Under         => Widget.Background.Get_Foreground,
            Center        => (0.0, 0.0),
            Radius        => 0.12,
            From          => Pin_From_Color,
            To            => Pin_To_Color,
            Border_Depth  => 0.015,
            Border_Shadow => Shadow_Out,
            Deepened      => True,
            Scaled        => True
         );
      Widget.Major_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / 0.43, 0.43, Length),
            Outer   => ((0.0, 0.0), 1.0 / 0.49, 0.49, Length),
            Color   => Major_Tick_Color,
            Width   => 6.0 / 400.0,
            Step    => Step,
            From    => Start,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Minor_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / 0.46, 0.46, Length),
            Outer   => ((0.0, 0.0), 1.0 / 0.49, 0.49, Length),
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 400.0,
            Step    => Step / 2.0,
            From    => Start,
            Skipped => 2,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
   end Create_Background;

   procedure Create_Needle
             (  Widget  : not null access
                          Gtk_Gauge_Round_254_Record'Class;
                Adjustment : Gtk_Adjustment
             )  is
   begin
      Widget.Needle :=
         Add_Needle
         (  Under       => Widget.Background.Get_Foreground,
            Adjustment  => Adjustment,
            Center      => (0.0, 0.0),
            From        => Start,
            Length      => Length,
            Tip_Length  => 0.46,
            Tip_Width   => 0.0175,
            Rear_Length => 0.135,
            Rear_Width  => 0.03,
            Color       => Needle_Color,
            Scaled      => True
         );
   end Create_Needle;

   function Get_Needle
            (  Widget : not null access Gtk_Gauge_Round_254_Record
            )  return not null access Needle_Layer is
   begin
      return Widget.Needle;
   end Get_Needle;

   function Get_Background
            (  Widget : not null access Gtk_Gauge_Round_254_Record
            )  return not null access Elliptic_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Cache
            (  Widget : not null access Gtk_Gauge_Round_254_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Major_Annotation
            (  Widget : not null access Gtk_Gauge_Round_254_Record
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return Widget.Major_Annotation;
   end Get_Major_Annotation;

   function Get_Minor_Annotation
            (  Widget : not null access Gtk_Gauge_Round_254_Record
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return Widget.Minor_Annotation;
   end Get_Minor_Annotation;

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
            (  Name       => "needle-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Needle color",
               Blurb      => "The color of the gauge's needle"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the needle tip",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the needle rear",
               Default => CAIRO_LINE_CAP_BUTT
         )  );
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
            (  Name       => "pin-dark-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Pin to",
               Blurb      => "Arrow pin dark color"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "pin-light-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Pin from",
               Blurb      => "Arrow pin light color"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "major-text-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Major text color",
               Blurb      => "Text color of the major ticks annotation"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "minor-text-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Minor text color",
               Blurb      => "Text color of the minor ticks annotation"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget      : out Gtk_Gauge_Round_254;
                Major_Texts : Gtk.Enums.String_List.GList;
                Minor_Texts : Gtk.Enums.String_List.GList;
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 17
             )  is
   begin
      Widget := new Gtk_Gauge_Round_254_Record;
      Initialize
      (  Widget,
         Major_Texts,
         Minor_Texts,
         Adjustment,
         Sectors
      );
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget      : out Gtk_Gauge_Round_254;
                Major_Texts : Controlled_String_List;
                Minor_Texts : Controlled_String_List;
                Adjustment  : Gtk_Adjustment := null;
                Sectors     : Positive       := 17
             )  is
   begin
      Widget := new Gtk_Gauge_Round_254_Record;
      Initialize
      (  Widget,
         Major_Texts,
         Minor_Texts,
         Adjustment,
         Sectors
      );
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget      : out Gtk_Gauge_Round_254;
                Major_Texts : UTF8_String;
                Minor_Texts : UTF8_String;
                Delimiter   : Character      := ' ';
                Adjustment  : Gtk_Adjustment := null;
                Sectors    : Positive        := 17
             )  is
   begin
      Widget := new Gtk_Gauge_Round_254_Record;
      Initialize
      (  Widget,
         Major_Texts,
         Minor_Texts,
         Delimiter,
         Adjustment,
         Sectors
      );
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record'Class;
                Major_Texts : Gtk.Enums.String_List.GList;
                Minor_Texts : Gtk.Enums.String_List.GList;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive
             )  is
      Step : constant GDouble := Length / GDouble (Sectors);
   begin
      Create_Background (Widget, Sectors);
      Widget.Major_Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget.Cache,
            Ellipse => ((0.0, 0.0), 1.0 / 0.41, 0.41, Length),
            Texts   => Major_Texts,
            Face    => Create_Toy
                       (  Family => "arial",
                          Slant  => CAIRO_FONT_SLANT_NORMAL,
                          Weight => CAIRO_FONT_WEIGHT_BOLD
                       ),
            Step    => 2.0 * Step,
            Height  => 0.05,
            Stretch => Stretch,
            Color   => Major_Text_Color,
            From    => Start  + Step,
            Length  => Length - Step,
            Mode    => Moved_Inside,
            Scaled  => True
         );
      Widget.Minor_Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget.Cache,
            Ellipse => ((0.0, 0.0), 1.0 / 0.41, 0.41, Length),
            Texts   => Minor_Texts,
            Face    => Create_Toy
                       (  Family => "arial",
                          Slant  => CAIRO_FONT_SLANT_NORMAL,
                          Weight => CAIRO_FONT_WEIGHT_BOLD
                       ),
            Step    => 2.0 * Step,
            Height  => 0.035,
            Stretch => Stretch,
            Color   => Minor_Text_Color,
            From    => Start,
            Length  => Length - Step,
            Mode    => Moved_Inside,
            Scaled  => True
         );
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record'Class;
                Major_Texts : Controlled_String_List;
                Minor_Texts : Controlled_String_List;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive
             )  is
   begin
      Initialize
      (  Widget,
         Get_GList (Major_Texts),
         Get_GList (Minor_Texts),
         Adjustment,
         Sectors
      );
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record'Class;
                Major_Texts : UTF8_String;
                Minor_Texts : UTF8_String;
                Delimiter   : Character;
                Adjustment  : Gtk_Adjustment;
                Sectors     : Positive
             )  is
      Step : constant GDouble := Length / GDouble (Sectors);
   begin
      Create_Background (Widget, Sectors);
      Widget.Major_Annotation :=
         Add_Elliptic_Annotation
         (  Under     => Widget.Cache,
            Ellipse   => ((0.0, 0.0), 1.0 / 0.41, 0.41, Length),
            Texts     => Major_Texts,
            Delimiter => Delimiter,
            Face      => Create_Toy
                         (  Family => "arial",
                            Slant  => CAIRO_FONT_SLANT_NORMAL,
                            Weight => CAIRO_FONT_WEIGHT_BOLD
                         ),
            Step      => 2.0 * Step,
            Height    => 0.06,
            Stretch   => Stretch,
            Color     => Major_Text_Color,
            From      => Start  + Step,
            Length    => Length - Step,
            Mode      => Moved_Inside,
            Scaled    => True
         );
      Widget.Minor_Annotation :=
         Add_Elliptic_Annotation
         (  Under     => Widget.Cache,
            Ellipse   => ((0.0, 0.0), 1.0 / 0.41, 0.41, Length),
            Texts     => Minor_Texts,
            Delimiter => Delimiter,
            Face      => Create_Toy
                         (  Family => "arial",
                            Slant  => CAIRO_FONT_SLANT_NORMAL,
                            Weight => CAIRO_FONT_WEIGHT_BOLD
                         ),
            Step      => 2.0 * Step,
            Height    => 0.04,
            Stretch   => Stretch,
            Color     => Minor_Text_Color,
            From      => Start,
            Length    => Length - Step,
            Mode      => Moved_Inside,
            Scaled    => True
         );
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record;
                Value  : GDouble
             )  is
   begin
      Widget.Needle.Set_Value (Value);
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Gauge_Round_254_Record
             )  is
   begin
      Widget.Needle.Set
      (  Center => Widget.Needle.Get_Center,
         From   => Widget.Needle.Get_From,
         Length => Widget.Needle.Get_Length,
         Tip    => (  Length => Widget.Needle.Get_Tip.Length,
                      Width  => Widget.Needle.Get_Tip.Width,
                      Cap    => Style_Get (Widget, "needle-tip-cap")
                   ),
         Rear   => (  Length => Widget.Needle.Get_Rear.Length,
                      Width  => Widget.Needle.Get_Rear.Width,
                      Cap    => Style_Get (Widget, "needle-rear-cap")
                   ),
         Color  => Style_Get (Widget, "needle-color", Needle_Color)
      );
      Widget.Pin.Set
      (  Center        => Widget.Pin.Get_Center,
         Radius        => Widget.Pin.Get_Radius,
         Border_Width  => Widget.Pin.Get_Border_Width,
         Border_Depth  => Widget.Pin.Get_Border_Depth,
         Border_Color  => Widget.Pin.Get_Border_Color,
         Border_Shadow => Widget.Pin.Get_Border_Shadow,
         From => Style_Get (Widget, "pin-light-color", Pin_From_Color),
         To   => Style_Get (Widget, "pin-dark-color", Pin_To_Color)
      );
      Widget.Background.Set
      (  Outer         => Widget.Background.Get_Outer,
         Inner         => Widget.Background.Get_Inner,
         From          => Widget.Background.Get_From,
         Length        => Widget.Background.Get_Length,
         Border_Width  => Widget.Background.Get_Border_Width,
         Border_Depth  => Widget.Background.Get_Border_Depth,
         Border_Color  => Widget.Background.Get_Border_Color,
         Border_Shadow => Widget.Background.Get_Border_Shadow,
         Color  =>
            Style_Get (Widget, "backgound-color", Background_Color)
      );
      Widget.Minor_Ticks.Set
      (  Inner  => Widget.Minor_Ticks.Get_Inner,
         Outer  => Widget.Minor_Ticks.Get_Outer,
         Ticks  => Widget.Minor_Ticks.Get_Ticks,
         From   => Widget.Minor_Ticks.Get_From,
         Length => Widget.Minor_Ticks.Get_Length,
         Line =>
            (  Widget.Minor_Ticks.Get_Line.Width,
               Style_Get (Widget, "minor-tick-color", Minor_Tick_Color),
               Style_Get (Widget, "minor-tick-line-cap")
      )     );
      Widget.Major_Ticks.Set
      (  Inner  => Widget.Major_Ticks.Get_Inner,
         Outer  => Widget.Major_Ticks.Get_Outer,
         Ticks  => Widget.Major_Ticks.Get_Ticks,
         From   => Widget.Major_Ticks.Get_From,
         Length => Widget.Major_Ticks.Get_Length,
         Line =>
            (  Widget.Major_Ticks.Get_Line.Width,
               Style_Get (Widget, "major-tick-color", Major_Tick_Color),
               Style_Get (Widget, "major-tick-line-cap")
      )     );
      Widget.Major_Annotation.Set
      (  Ellipse => Widget.Major_Annotation.Get_Ellipse,
         Ticks   => Widget.Major_Annotation.Get_Ticks,
         From    => Widget.Major_Annotation.Get_From,
         Length  => Widget.Major_Annotation.Get_Length,
         Face    => Widget.Major_Annotation.Get_Face,
         Mode    => Widget.Major_Annotation.Get_Mode,
         Height  => Widget.Major_Annotation.Get_Height,
         Stretch => Widget.Major_Annotation.Get_Stretch,
         Color =>
            Style_Get (Widget, "major-text-color", Major_Text_Color)
      );
      Widget.Minor_Annotation.Set
      (  Ellipse => Widget.Minor_Annotation.Get_Ellipse,
         Ticks   => Widget.Minor_Annotation.Get_Ticks,
         From    => Widget.Minor_Annotation.Get_From,
         Length  => Widget.Minor_Annotation.Get_Length,
         Face    => Widget.Minor_Annotation.Get_Face,
         Mode    => Widget.Minor_Annotation.Get_Mode,
         Height  => Widget.Minor_Annotation.Get_Height,
         Stretch => Widget.Minor_Annotation.Get_Stretch,
         Color =>
            Style_Get (Widget, "minor-text-color", Minor_Text_Color)
      );
   end Style_Changed;

end Gtk.Gauge.Round_254;
