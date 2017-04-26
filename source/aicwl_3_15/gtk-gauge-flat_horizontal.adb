--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Flat_Horizontal                   Luebeck            --
--  Implementation                                 Winter, 2011       --
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
with Cairo;                     use Cairo;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GtkAda.Types;              use GtkAda.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with GLib.Object.Checked_Destroy;

with Cairo.Line_Cap_Property;
use  Cairo.Line_Cap_Property;

with Gtk.Widget.Styles.Line_Cap_Property;
use  Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Gauge.Flat_Horizontal is

   Needle_Color      : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Background_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Major_Tick_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Middle_Tick_Color : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Text_Color        : constant Gdk_Color := RGB (1.0, 1.0, 1.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Corner : constant := 1.0 / 30.0;
   Length : constant := 0.9;
   First  : constant := -Length * 0.5;
   Bottom : constant := 0.06;

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
               Default => CAIRO_LINE_CAP_ROUND
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
            (  Name       => "line-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Line color",
               Blurb => "The color of the circle bounding the ticks"
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
            (  Name       => "pin-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Pin color",
               Blurb      => "Arrow pin color"
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

   procedure Create_Background
             (  Widget  : not null access
                          Gtk_Gauge_Flat_Horizontal_Record'Class;
                Sectors : Positive
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 3.0);
      Widget.Background :=
         Add_Rectangular_Background
         (  Under         => Widget,
            Height        => 0.3,
            Width         => 1.0,
            Center        => (0.0, 0.0),
            Corner_Radius => Corner,
            Color         => Background_Color,
            Border_Width  => 0.01,
            Border_Depth  => 0.005,
            Border_Shadow => Shadow_Etched_Out,
            Deepened      => True,
            Widened       => True,
            Scaled        => True
         );
      Widget.Major_Ticks :=
         Add_Flat_Scale
         (  Under   => Widget.Background.Get_Foreground,
            From    => (First, Bottom - 0.06 / 2.0),
            Length  => Length,
            Angle   => 0.0,
            Breadth => 0.06,
            Color   => Major_Tick_Color,
            Width   => 1.5 / 400.0,
            Step    => Length / GDouble (Sectors),
            Scaled  => True,
            Widened => True
         );
      Widget.Middle_Ticks :=
         Add_Flat_Scale
         (  Under   => Widget.Background.Get_Foreground,
            From    => (First, Bottom - 0.04 / 2.0),
            Length  => Length,
            Angle   => 0.0,
            Breadth => 0.04,
            Color   => Minor_Tick_Color,
            Width   => 1.5 / 600.0,
            Step    => 0.5 * Length / GDouble (Sectors),
            Skipped => 2,
            Scaled  => True,
            Widened => True
         );
      Widget.Minor_Ticks :=
         Add_Flat_Scale
         (  Under   => Widget.Background.Get_Foreground,
            From    => (First, Bottom - 0.02 / 2.0),
            Length  => Length,
            Angle   => 0.0,
            Breadth => 0.02,
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 600.0,
            Step    => 0.1 * Length / GDouble (Sectors),
            Skipped => 5,
            Scaled  => True,
            Widened => True
         );
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
   end Create_Background;

   procedure Create_Foreground
             (  Widget : not null access
                         Gtk_Gauge_Flat_Horizontal_Record'Class;
                Adjustment : Gtk_Adjustment
             )  is
   begin
      Widget.Needle :=
         Add_Flat_Needle
         (  Under       => Widget.Background.Get_Foreground,
            From        => (First, 0.15),
            Length      => Length,
            Angle       => 0.0,
            Tip_Cap     => CAIRO_LINE_CAP_ROUND,
            Adjustment  => Adjustment,
            Tip_Length  => 0.2,
            Tip_Width   => 0.01,
            Rear_Length =>-0.013,
            Rear_Width  => 0.015,
            Color       => Needle_Color,
            Scaled      => True
         );
   end Create_Foreground;

   function Get_Annotation
            (  Widget : not null access Gtk_Gauge_Flat_Horizontal_Record
            )  return not null access Flat_Annotation_Layer is
   begin
      return Widget.Annotation;
   end Get_Annotation;

   function Get_Needle
            (  Widget : not null access Gtk_Gauge_Flat_Horizontal_Record
            )  return not null access Flat_Needle_Layer is
   begin
      return Widget.Needle;
   end Get_Needle;

   function Get_Background
            (  Widget : not null access Gtk_Gauge_Flat_Horizontal_Record
            )  return not null access Rectangular_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Cache
            (  Widget : not null access Gtk_Gauge_Flat_Horizontal_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   procedure Gtk_New
             (  Widget     : out Gtk_Gauge_Flat_Horizontal;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 10
             )  is
   begin
      Widget := new Gtk_Gauge_Flat_Horizontal_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Gauge_Flat_Horizontal;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 10
             )  is
   begin
      Widget := new Gtk_Gauge_Flat_Horizontal_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Gauge_Flat_Horizontal;
                Texts      : UTF8_String;
                Delimiter  : Character      := ' ';
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 10
             )  is
   begin
      Widget := new Gtk_Gauge_Flat_Horizontal_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Gauge_Flat_Horizontal_Record'Class;
                Texts      : Gtk.Enums.String_List.GList;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Flat_Annotation
         (  Under   => Widget.Cache,
            From    => (First, -0.05),
            Length  => Length,
            Texts   => Texts,
            Face    => Create_Toy
                       (  Family => "arial",
                          Slant  => CAIRO_FONT_SLANT_NORMAL,
                          Weight => CAIRO_FONT_WEIGHT_BOLD
                       ),
            Step    => Length / GDouble (Sectors),
            Height  => 0.06,
            Stretch => 0.5,
            Color   => Text_Color,
            Scaled  => True
         );
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Gauge_Flat_Horizontal_Record'Class;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Initialize (Widget, Get_GList (Texts), Adjustment, Sectors);
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                                Gtk_Gauge_Flat_Horizontal_Record'Class;
                Texts      : UTF8_String;
                Delimiter  : Character;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Flat_Annotation
         (  Under     => Widget.Cache,
            From      => (First, -0.05),
            Length    => Length,
            Texts     => Texts,
            Delimiter => Delimiter,
            Face      => Create_Toy
                         (  Family => "arial",
                            Slant  => CAIRO_FONT_SLANT_NORMAL,
                            Weight => CAIRO_FONT_WEIGHT_BOLD
                         ),
            Step      => Length / GDouble (Sectors),
            Height    => 0.06,
            Stretch   => 0.5,
            Color     => Text_Color,
            Scaled    => True
         );
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
             (  Widget : not null access
                         Gtk_Gauge_Flat_Horizontal_Record;
                Value  : GDouble
             )  is
   begin
      Widget.Needle.Set_Value (Value);
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Gauge_Flat_Horizontal_Record
             )  is
   begin
      Widget.Needle.Set
      (  From   => Widget.Needle.Get_From,
         To     => Widget.Needle.Get_To,
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
   end Style_Changed;

end Gtk.Gauge.Flat_Horizontal;
