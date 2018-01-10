--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Elliptic_180                      Luebeck            --
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
with Cairo.Ellipses;
with Cairo.Line_Cap_Property;
with Gdk.Color;
with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;
with Gtk.Missed;
with Gtk.Widget.Styles.Line_Cap_Property;
with Pango.Cairo.Fonts;

package body Gtk.Gauge.Elliptic_180 is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Needle_Color     : constant Gdk.Color.Gdk_Color :=
                        Gtk.Missed.RGB (1.0, 0.0, 0.0);
   Background_Color : constant Gdk.Color.Gdk_Color :=
                        Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Scale_Area_Color : constant Gdk.Color.Gdk_Color :=
                        Gtk.Missed.RGB (1.0, 1.0, 1.0);
   Major_Tick_Color : constant Gdk.Color.Gdk_Color :=
                        Gtk.Missed.RGB (1.0, 0.0, 0.0);
   Text_Color       : constant Gdk.Color.Gdk_Color :=
                        Gtk.Missed.RGB (1.0, 0.6, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Pi     : constant := Ada.Numerics.Pi;

   Length : constant Gdouble := Pi;
   First  : constant Gdouble := Pi;
   Excess : constant Gdouble := Pi / 30.0;
   Y : constant := 0.225;

   Inner : constant Cairo.Ellipses.Ellipse_Parameters :=
             ((-0.045, Y), 1.0 / 0.40, 0.31, -Pi / 5.0);
   Outer : constant Cairo.Ellipses.Ellipse_Parameters :=
             ((-0.020, Y), 1.0 / 0.43, 0.36, -Pi / 4.0);

   procedure Create_Background
     (Widget  : not null access Gtk_Gauge_Elliptic_180_Record'Class;
      Sectors : Positive);
   procedure Create_Background
     (Widget  : not null access Gtk_Gauge_Elliptic_180_Record'Class;
      Sectors : Positive)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 1.81072266020785);
      Widget.all.Background :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget,
           Color         => Background_Color,
           Outer         => ((0.0, Y), 1.0 / 0.5, 0.5, 0.0),
           From          => First  - Excess,
           Length        => Length + Excess * 2.0,
           Border_Width  => 0.01,
           Border_Depth  => 0.005,
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.all.Scale_Area :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget.all.Background.all.Get_Foreground,
           Color         => Scale_Area_Color,
           Inner         => Inner,
           Outer         => Outer,
           From          => Pi,
           Length        => Pi,
           Scaled        => True);
      Widget.all.Major_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   => Outer * 0.5,
           Outer   => Outer,
           Color   => Background_Color,
           Width   => 3.0 / 400.0,
           Step    => Length / (2.0 * Gdouble (Widget.all.Sectors)),
           From    => First,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
   end Create_Background;

   procedure Create_Needle
     (Widget     : not null access Gtk_Gauge_Elliptic_180_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   procedure Create_Needle
     (Widget  : not null access Gtk_Gauge_Elliptic_180_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget.all.Needle :=
        Gtk.Layered.Needle.Add_Needle
          (Under       => Widget.all.Background.all.Get_Foreground,
           Center      => Outer.Center,
           Tip_Cap     => Cairo.Cairo_Line_Cap_Square,
           Adjustment  => Adjustment,
           Tip_Length  => 0.38,
           Tip_Width   => 0.01,
           Rear_Length => 0.0,
           Rear_Width  => 0.03,
           Rear_Cap    => Cairo.Cairo_Line_Cap_Round,
           Color       => Needle_Color,
           From        => First,
           Length      => Length,
           Scaled      => True);
   end Create_Needle;

   function Get_Annotation
     (Widget : not null access Gtk_Gauge_Elliptic_180_Record)
      return not null access Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer is
   begin
      return Widget.all.Annotation;
   end Get_Annotation;

   function Get_Background
     (Widget : not null access Gtk_Gauge_Elliptic_180_Record)
      return not null access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Cache
     (Widget : not null access Gtk_Gauge_Elliptic_180_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Needle
     (Widget : not null access Gtk_Gauge_Elliptic_180_Record)
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
              (Name       => "text-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Text color",
               Blurb      => "Text color"));
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Elliptic_180;
      Texts      : Gtk.Enums.String_List.Glist;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive                      := 8) is
   begin
      Widget := new Gtk_Gauge_Elliptic_180_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Elliptic_180;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive                      := 8) is
   begin
      Widget := new Gtk_Gauge_Elliptic_180_Record;
      Initialize (Widget, Texts, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Elliptic_180;
      Texts      : UTF8_String;
      Delimiter  : Character                     := ' ';
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : Positive                      := 8) is
   begin
      Widget := new Gtk_Gauge_Elliptic_180_Record;
      Initialize (Widget, Texts, Delimiter, Adjustment, Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Elliptic_180_Record'Class;
      Texts      : Gtk.Enums.String_List.Glist;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under   => Widget.all.Cache,
           Ellipse => Outer * 1.05,
           Texts   => Texts,
           Face    =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Italic,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step    => Length / Gdouble (Sectors),
           Height  => 0.03,
           Stretch => 0.9,
           Color   => Text_Color,
           From    => First,
           Length  => Length,
           Mode    => Gtk.Layered.Moved_Outside,
           Scaled  => True);
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Elliptic_180_Record'Class;
      Texts      : Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive) is
   begin
      Initialize
        (Widget, Gtk.Enums.String_Lists.Get_GList (Texts), Adjustment, Sectors);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Elliptic_180_Record'Class;
      Texts      : UTF8_String;
      Delimiter  : Character;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : Positive)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      Create_Background (Widget, Sectors);
      Widget.all.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget.all.Cache,
           Ellipse   => Outer * 1.05,
           Texts     => Texts,
           Delimiter => Delimiter,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Italic,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Length / Gdouble (Sectors),
           Height    => 0.03,
           Stretch   => 0.9,
           Color     => Text_Color,
           From      => First,
           Length    => Length,
           Mode      => Gtk.Layered.Moved_Outside,
           Scaled    => True);
      Create_Needle (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
     (Widget : not null access Gtk_Gauge_Elliptic_180_Record;
      Value  : Gdouble) is
   begin
      Widget.all.Needle.all.Set_Value (Value);
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Gauge_Elliptic_180_Record) is
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
      Widget.all.Background.all.Set
        (Outer         => Widget.all.Background.all.Get_Outer,
         Inner         => Widget.all.Background.all.Get_Inner,
         From          => Widget.all.Background.all.Get_From,
         Length        => Widget.all.Background.all.Get_Length,
         Border_Width  => Widget.all.Background.all.Get_Border_Width,
         Border_Depth  => Widget.all.Background.all.Get_Border_Depth,
         Border_Color  => Widget.all.Background.all.Get_Border_Color,
         Border_Shadow => Widget.all.Background.all.Get_Border_Shadow,
         Color  =>
           Gtk.Widget.Styles.Style_Get
             (Widget, "background-color", Background_Color));
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

end Gtk.Gauge.Elliptic_180;
