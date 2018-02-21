--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Round_270_60s                     Luebeck            --
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

with Ada.Numerics;
with Cairo;
with Cairo.Line_Cap_Property;
with Gdk.Color;
with Glib.Properties.Creation;
with Glib.Types;
with Gtk.Missed;
with Gtk.Widget.Styles;
with Pango.Cairo.Fonts;

with Glib.Object.Checked_Destroy;

with Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Gauge.Round_270_60s is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Pi : constant := Ada.Numerics.Pi;

   Needle_Color      : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.00, 0.00, 0.00);
   Pin_Color         : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.70, 0.70, 0.70);
   Background_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.17, 0.12, 0.11);
   Major_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.00, 1.00, 1.00);
   Middle_Tick_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.00, 1.00, 1.00);
   Minor_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.00, 1.00, 1.00);
   Text_Color        : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.00, 1.00, 1.00);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   procedure Create_Background
     (Widget  : not null access Gtk_Gauge_Round_270_60s_Record'Class;
      Sectors : Positive);

   procedure Create_Background
     (Widget  : not null access Gtk_Gauge_Round_270_60s_Record'Class;
      Sectors : Positive) is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Sectors := Sectors;
      Widget.all.Background :=
         Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget,
           Color         => Background_Color,
           Outer         => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
           Border_Width  => 0.01,
           Border_Depth  => 0.005,
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.all.Pin :=
         Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget.all.Background.all.Get_Foreground,
           Color         => Background_Color,
           Outer         => ((0.0, 0.0), 1.0 / 0.17, 0.17, 0.0),
           Border_Depth  => 0.01,
           Border_Width  => 0.05,
           Border_Color  => (False, Pin_Color),
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.all.Minor_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   => ((0.0, 0.0), 1.0 / 0.44, 0.44, Pi * 3.0 / 4.0),
           Outer   => ((0.0, 0.0), 1.0 / 0.47, 0.47, Pi * 3.0 / 4.0),
           Color   => Minor_Tick_Color,
           Width   => 1.2 / 200.0,
           Skipped => 5,
           Step    => 6.0 * Pi / (40.0 * Gdouble (Widget.all.Sectors)),
           From    => 3.0 * Pi / 4.0,
           Length  => 6.0 * Pi / 4.0,
           Scaled  => True,
           Widened => True);
      Widget.all.Middle_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   => ((0.0, 0.0), 1.0 / 0.44, 0.44, Pi * 3.0 / 4.0),
           Outer   => ((0.0, 0.0), 1.0 / 0.47, 0.47, Pi * 3.0 / 4.0),
           Color   => Middle_Tick_Color,
           Width   => 1.5 / 200.0,
           Skipped => 2,
           Step    => 6.0 * Pi / (4.0 * 2.0 * Gdouble (Widget.all.Sectors)),
           From    => 3.0 * Pi / 4.0,
           Length  => 6.0 * Pi / 4.0,
           Scaled  => True,
           Widened => True);
      Widget.all.Major_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   => ((0.0, 0.0), 1.0 / 0.42, 0.42, Pi * 3.0 / 4.0),
           Outer   => ((0.0, 0.0), 1.0 / 0.47, 0.47, Pi * 3.0 / 4.0),
           Color   => Major_Tick_Color,
           Width   => 2.2 / 200.0,
           Step    => 6.0 * Pi / (4.0 * Gdouble (Widget.all.Sectors)),
           From    => 3.0 * Pi / 4.0,
           Length  => 6.0 * Pi / 4.0,
           Scaled  => True,
           Widened => True);
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
   end Create_Background;

   procedure Create_Needle
     (Widget     : not null access Gtk_Gauge_Round_270_60s_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Create_Needle
     (Widget     : not null access Gtk_Gauge_Round_270_60s_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget.all.Needle :=
         Gtk.Layered.Needle.Add_Needle
          (Under       => Widget,
           Center      => (0.0, 0.0),
           Tip_Cap     => Cairo.Cairo_Line_Cap_Round,
           Adjustment  => Adjustment,
           Tip_Length  => 0.44,
           Tip_Width   => 0.01,
           Rear_Length => -0.165,
           Rear_Width  => 0.03,
           Color       => Needle_Color,
           Scaled      => True);
   end Create_Needle;

   function Get_Annotation
     (Widget : not null access Gtk_Gauge_Round_270_60s_Record)
      return not null access Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer is
   begin
      return Widget.all.Annotation;
   end Get_Annotation;

   function Get_Background
     (Widget : not null access Gtk_Gauge_Round_270_60s_Record)
      return not null access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Cache
     (Widget : not null access Gtk_Gauge_Round_270_60s_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Needle
     (Widget : not null access Gtk_Gauge_Round_270_60s_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer is
   begin
      return Widget.all.Needle;
   end Get_Needle;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record (Ancestor     => Gtk.Layered.Get_Type,
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
              (Name       => "background-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Background color",
               Blurb      => "The background color"));
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

   procedure Gtk_New (Widget     : out Gtk_Gauge_Round_270_60s;
                      Texts      : Gtk.Enums.String_List.Glist;
                      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
                      Sectors    : Positive                      := 12) is
   begin
      Widget := new Gtk_Gauge_Round_270_60s_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Round_270_60s;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive                      := 12) is
   begin
      Widget := new Gtk_Gauge_Round_270_60s_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New (Widget     : out Gtk_Gauge_Round_270_60s;
                      Texts      : UTF8_String;
                      Delimiter  : Character                     := ' ';
                      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
                      Sectors    : Positive                      := 12) is
   begin
      Widget := new Gtk_Gauge_Round_270_60s_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Round_270_60s_Record'Class;
      Texts      : Gtk.Enums.String_List.Glist;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
         Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under   => Widget.all.Cache,
           Ellipse => ((0.0, 0.0), 1.0 / 0.36, 0.36, 3.0 * Pi / 4.0),
           Texts   => Texts,
           Face    =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step    => 6.0 * Pi / (4.0 * Gdouble (Sectors)),
           Height  => 0.1,
           Stretch => 0.4,
           Color   => Text_Color,
           From    => 3.0 * Pi / 4.0,
           Length  => 6.0 * Pi / 4.0,
           Mode    => Gtk.Layered.Rotated,
           Scaled  => True);
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Round_270_60s_Record'Class;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Initialize (Widget     => Widget,
                  Texts      => Gtk.Enums.String_Lists.Get_GList (Texts),
                  Adjustment => Adjustment,
                  Sectors    => Sectors);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Round_270_60s_Record'Class;
      Texts      : UTF8_String;
      Delimiter  : Character;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
         Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget.all.Cache,
           Ellipse   => ((0.0, 0.0), 1.0 / 0.36, 0.36, 3.0 * Pi / 4.0),
           Texts     => Texts,
           Delimiter => Delimiter,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => 6.0 * Pi / (4.0 * Gdouble (Sectors)),
           Height    => 0.1,
           Stretch   => 0.4,
           Color     => Text_Color,
           From      => 3.0 * Pi / 4.0,
           Length    => 6.0 * Pi / 4.0,
           Mode      => Gtk.Layered.Rotated,
           Scaled    => True);
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
     (Widget : not null access Gtk_Gauge_Round_270_60s_Record;
      Value  : Gdouble) is
   begin
      Widget.all.Needle.all.Set_Value (Value);
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Gauge_Round_270_60s_Record) is
   begin
      Widget.all.Needle.all.Set
        (Center => Widget.all.Needle.all.Get_Center,
         From   => Widget.all.Needle.all.Get_From,
         Length => Widget.all.Needle.all.Get_Length,
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
      Widget.all.Pin.all.Set
        (Outer         => Widget.all.Pin.all.Get_Outer,
         Inner         => Widget.all.Pin.all.Get_Inner,
         From          => Widget.all.Pin.all.Get_From,
         Length        => Widget.all.Pin.all.Get_Length,
         Border_Width  => Widget.all.Pin.all.Get_Border_Width,
         Border_Depth  => Widget.all.Pin.all.Get_Border_Depth,
         Border_Color  => Widget.all.Pin.all.Get_Border_Color,
         Border_Shadow => Widget.all.Pin.all.Get_Border_Shadow,
         Color         =>
           Gtk.Widget.Styles.Style_Get (Widget, "pin-color", Pin_Color));
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
             (Widget, "background-color", Background_Color));
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
         Line =>
           (Widget.all.Major_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get
              (Widget, "major-tick-color", Major_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "major-tick-line-cap")));
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

   pragma Warnings (On, "declaration hides ""Widget""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Gauge.Round_270_60s;
