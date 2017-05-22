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
-- __________________________________________________________________ --

with Cairo.Line_Cap_Property;

with Gdk.Color;

with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;

with Gtk.Missed;
with Gtk.Widget.Styles.Line_Cap_Property;

with Pango.Cairo.Fonts;

package body Gtk.Gauge.Flat_Horizontal is

   Needle_Color      : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0);
   Background_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Major_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
   Middle_Tick_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
   Text_Color        : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Corner : constant := 1.0 / 30.0;
   Length : constant := 0.9;
   First  : constant := -Length * 0.5;
   Bottom : constant := 0.06;

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

   procedure Create_Background
     (Widget  : not null access Gtk_Gauge_Flat_Horizontal_Record'Class;
      Sectors : Positive) is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 3.0);
      Widget.all.Background :=
        Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
          (Under         => Widget,
           Height        => 0.3,
           Width         => 1.0,
           Center        => (0.0, 0.0),
           Corner_Radius => Corner,
           Color         => Background_Color,
           Border_Width  => 0.01,
           Border_Depth  => 0.005,
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.all.Major_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (First, Bottom - 0.06 / 2.0),
           Length  => Length,
           Angle   => 0.0,
           Breadth => 0.06,
           Color   => Major_Tick_Color,
           Width   => 1.5 / 400.0,
           Step    => Length / Gdouble (Sectors),
           Scaled  => True,
           Widened => True);
      Widget.all.Middle_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (First, Bottom - 0.04 / 2.0),
           Length  => Length,
           Angle   => 0.0,
           Breadth => 0.04,
           Color   => Minor_Tick_Color,
           Width   => 1.5 / 600.0,
           Step    => 0.5 * Length / Gdouble (Sectors),
           Skipped => 2,
           Scaled  => True,
           Widened => True);
      Widget.all.Minor_Ticks :=
        Gtk.Layered.Flat_Scale.Add_Flat_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           From    => (First, Bottom - 0.02 / 2.0),
           Length  => Length,
           Angle   => 0.0,
           Breadth => 0.02,
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 600.0,
           Step    => 0.1 * Length / Gdouble (Sectors),
           Skipped => 5,
           Scaled  => True,
           Widened => True);
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
   end Create_Background;

   procedure Create_Foreground
     (Widget     : not null access Gtk_Gauge_Flat_Horizontal_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget.all.Needle :=
        Gtk.Layered.Flat_Needle.Add_Flat_Needle
          (Under       => Widget.all.Background.all.Get_Foreground,
           From        => (First, 0.15),
           Length      => Length,
           Angle       => 0.0,
           Tip_Cap     => Cairo.Cairo_Line_Cap_Round,
           Adjustment  => Adjustment,
           Tip_Length  => 0.2,
           Tip_Width   => 0.01,
           Rear_Length => -0.013,
           Rear_Width  => 0.015,
           Color       => Needle_Color,
           Scaled      => True);
   end Create_Foreground;

   function Get_Annotation
     (Widget : not null access Gtk_Gauge_Flat_Horizontal_Record)
      return not null access Gtk.Layered.Flat_Annotation.Flat_Annotation_Layer
   is
   begin
      return Widget.all.Annotation;
   end Get_Annotation;

   function Get_Needle
     (Widget : not null access Gtk_Gauge_Flat_Horizontal_Record)
      return not null access Gtk.Layered.Flat_Needle.Flat_Needle_Layer is
   begin
      return Widget.all.Needle;
   end Get_Needle;

   function Get_Background
     (Widget : not null access Gtk_Gauge_Flat_Horizontal_Record)
      return not null access Gtk.Layered.Rectangular_Background.Rectangular_Background_Layer
   is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Cache
     (Widget : not null access Gtk_Gauge_Flat_Horizontal_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Flat_Horizontal;
      Texts      : Gtk.Enums.String_List.Glist;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive       := 10) is
   begin
      Widget := new Gtk_Gauge_Flat_Horizontal_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Flat_Horizontal;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive       := 10) is
   begin
      Widget := new Gtk_Gauge_Flat_Horizontal_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Flat_Horizontal;
      Texts      : UTF8_String;
      Delimiter  : Character      := ' ';
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive       := 10) is
   begin
      Widget := new Gtk_Gauge_Flat_Horizontal_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Flat_Horizontal_Record'Class;
      Texts      : Gtk.Enums.String_List.Glist;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
        Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
          (Under   => Widget.all.Cache,
           From    => (First, -0.05),
           Length  => Length,
           Texts   => Texts,
           Face    =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step    => Length / Gdouble (Sectors),
           Height  => 0.06,
           Stretch => 0.5,
           Color   => Text_Color,
           Scaled  => True);
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Flat_Horizontal_Record'Class;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Initialize
        (Widget,
         Gtk.Enums.String_Lists.Get_GList (Texts),
         Adjustment,
         Sectors);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Flat_Horizontal_Record'Class;
      Texts      : UTF8_String;
      Delimiter  : Character;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
        Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
          (Under     => Widget.all.Cache,
           From      => (First, -0.05),
           Length    => Length,
           Texts     => Texts,
           Delimiter => Delimiter,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Length / Gdouble (Sectors),
           Height    => 0.06,
           Stretch   => 0.5,
           Color     => Text_Color,
           Scaled    => True);
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
     (Widget : not null access Gtk_Gauge_Flat_Horizontal_Record;
      Value  : Gdouble) is
   begin
      Widget.all.Needle.all.Set_Value (Value);
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Gauge_Flat_Horizontal_Record) is
   begin
      Widget.all.Needle.all.Set
        (From   => Widget.all.Needle.all.Get_From,
         To     => Widget.all.Needle.all.Get_To,
         Tip    => (Length => Widget.all.Needle.all.Get_Tip.Length,
                    Width  => Widget.all.Needle.all.Get_Tip.Width,
                    Cap    =>
                      Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                        (Widget, "needle-tip-cap")),
         Rear   => (Length => Widget.all.Needle.all.Get_Rear.Length,
                    Width  => Widget.all.Needle.all.Get_Rear.Width,
                    Cap    =>
                      Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                        (Widget, "needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get (Widget, "needle-color", Needle_Color));
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
             (Widget,
              "backgound-color",
              Background_Color));
      Widget.all.Minor_Ticks.all.Set
        (From    => Widget.all.Minor_Ticks.all.Get_From,
         Length  => Widget.all.Minor_Ticks.all.Get_Length,
         Breadth => Widget.all.Minor_Ticks.all.Get_Breadth,
         Angle   => Widget.all.Minor_Ticks.all.Get_Angle,
         Ticks   => Widget.all.Minor_Ticks.all.Get_Ticks,
         Line =>
           (Widget.all.Minor_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get (Widget, "minor-tick-color", Minor_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "minor-tick-line-cap")));
      Widget.all.Middle_Ticks.all.Set
        (From    => Widget.all.Middle_Ticks.all.Get_From,
         Length  => Widget.all.Middle_Ticks.all.Get_Length,
         Breadth => Widget.all.Middle_Ticks.all.Get_Breadth,
         Angle   => Widget.all.Middle_Ticks.all.Get_Angle,
         Ticks   => Widget.all.Middle_Ticks.all.Get_Ticks,
         Line    =>
           (Widget.all.Middle_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get
              (Widget,
               "middle-tick-color",
               Middle_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "middle-tick-line-cap")));
      Widget.all.Major_Ticks.all.Set
        (From    => Widget.all.Major_Ticks.all.Get_From,
         Length  => Widget.all.Major_Ticks.all.Get_Length,
         Breadth => Widget.all.Major_Ticks.all.Get_Breadth,
         Angle   => Widget.all.Major_Ticks.all.Get_Angle,
         Ticks   => Widget.all.Major_Ticks.all.Get_Ticks,
         Line =>
           (Widget.all.Major_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get (Widget, "major-tick-color", Major_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "major-tick-line-cap")));
      Widget.all.Annotation.all.Set
        (Ticks       => Widget.all.Annotation.all.Get_Ticks,
         From        => Widget.all.Annotation.all.Get_From,
         Length      => Widget.all.Annotation.all.Get_Length,
         Face        => Widget.all.Annotation.all.Get_Face,
         Scale_Angle => Widget.all.Annotation.all.Get_Scale_Angle,
         Height      => Widget.all.Annotation.all.Get_Height,
         Stretch     => Widget.all.Annotation.all.Get_Stretch,
         Text_Angle  => Widget.all.Annotation.all.Get_Text_Angle,
         Justify     => Widget.all.Annotation.all.Get_Justify,
         Color       =>
           Gtk.Widget.Styles.Style_Get (Widget, "text-color", Text_Color));
   end Style_Changed;

end Gtk.Gauge.Flat_Horizontal;
