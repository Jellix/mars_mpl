--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Wall_Clock.Classic                Luebeck            --
--  Implementation                                 Winter, 2010       --
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

with Cairo.Line_Cap_Property;

with Gdk.Color;

with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;

with Gtk.Enums;
with Gtk.Missed;
with Gtk.Widget.Styles.Line_Cap_Property;

with Pango.Cairo.Fonts;

package body Gtk.Wall_Clock.Classic is

   Hour_Needle_Color   : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Minute_Needle_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Second_Needle_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Pin_Color           : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Background_Color    : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 1.0, 1.0);
   Line_Color          : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.4, 0.4, 0.4);
   Major_Tick_Color    : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color    : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.4, 0.4, 0.4);
   Second_Tick_Color   : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.4, 0.4, 0.4);
   Text_Color          : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Step  : constant Gdouble := 2.0 * Ada.Numerics.Pi / 12.0;
   First : constant Gdouble := Step - Ada.Numerics.Pi / 2.0;

   package Handlers is
     new Gtk.Handlers.User_Callback
       (GObject_Record,
        Gtk_Wall_Clock_Classic);

   procedure Changed
     (Adjustment : access GObject_Record'Class;
      Widget     : Gtk_Wall_Clock_Classic) is
   begin
      Set (Widget, Gtk.Adjustment.Get_Value (Widget.all.Adjustment));
      if
        Widget.all.Hour_Needle.all.Is_Updated or else
        Widget.all.Minute_Needle.all.Is_Updated or else
        Widget.all.Second_Needle.all.Is_Updated -- Signal draw to the widget
      then
         Queue_Draw (Widget);
      end if;
   end Changed;

   function Get_Annotation
     (Widget : not null access Gtk_Wall_Clock_Classic_Record)
      return not null access Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer
   is
   begin
      return Widget.all.Annotation;
   end Get_Annotation;

   function Get_Background
     (Widget : not null access Gtk_Wall_Clock_Classic_Record)
      return not null access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer
   is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Cache
     (Widget : not null access Gtk_Wall_Clock_Classic_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Hour_Hand
     (Widget : not null access Gtk_Wall_Clock_Classic_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer is
   begin
      return Widget.all.Hour_Needle;
   end Get_Hour_Hand;

   function Get_Minute_Hand
     (Widget : not null access Gtk_Wall_Clock_Classic_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer is
   begin
      return Widget.all.Minute_Needle;
   end Get_Minute_Hand;

   function Get_Second_Hand
     (Widget : not null access Gtk_Wall_Clock_Classic_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer is
   begin
      return Widget.all.Second_Needle;
   end Get_Second_Hand;

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
              (Name       => "hour-needle-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Hour needle color",
               Blurb      => "The color of the hour needle"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "hour-needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the hour needle tip",
               Default => Cairo.Cairo_Line_Cap_Round));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "hour-needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the hour needle rear",
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
              (Name       => "line-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Line color",
               Blurb => "The color of the circle bounding the ticks"));
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
              (Name       => "minute-needle-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Minute needle color",
               Blurb      => "The color of the minute needle"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "minute-needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the minute needle tip",
               Default => Cairo.Cairo_Line_Cap_Round));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "minute-needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the minute needle rear",
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
              (Name       => "second-needle-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Second needle color",
               Blurb      => "The color of the second needle"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "second-needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the second needle tip",
               Default => Cairo.Cairo_Line_Cap_Round));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "second-needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the second needle rear",
               Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boxed
              (Name       => "second-tick-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Second ticks color",
               Blurb      => "Seconf ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
              (Name    => "second-tick-line-cap",
               Nick    => "Second tick cap",
               Blurb   => "The line cap style used for second ticks",
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
     (Widget     : out Gtk_Wall_Clock_Classic;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment := null) is
   begin
      Widget := new Gtk_Wall_Clock_Classic_Record;
      Initialize (Widget, Adjustment);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget     : not null access Gtk_Wall_Clock_Classic_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Background :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under  => Widget,
           Color  => Background_Color,
           Outer  => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
           Scaled => True);
      Widget.all.Line_1 :=
        Gtk.Layered.Arc.Add_Arc
          (Under   => Widget.all.Background.all.Get_Foreground,
           Color   => Line_Color,
           Width   => 1.0 / 300.0,
           Ellipse => ((0.0, 0.0), 1.0 / 0.48, 0.48, 0.0),
           Widened => True,
           Scaled  => True);
      Widget.all.Line_2 :=
        Gtk.Layered.Arc.Add_Arc
          (Under   => Widget.all.Background.all.Get_Foreground,
           Color   => Line_Color,
           Width   => 1.0 / 300.0,
           Ellipse => ((0.0, 0.0), 1.0 / 0.45, 0.45, 0.0),
           Widened => True,
           Scaled  => True);
      Widget.all.Line_3 :=
        Gtk.Layered.Arc.Add_Arc
          (Under   => Widget.all.Background.all.Get_Foreground,
           Color   => Line_Color,
           Width   => 1.0 / 300.0,
           Ellipse => ((0.0, 0.0), 1.0 / 0.31, 0.31, 0.0),
           Widened => True,
           Scaled  => True);
      Widget.all.Pin_1 :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget.all.Background.all.Get_Foreground,
           Color         => Pin_Color,
           Outer         => ((0.0, 0.0), 1.0 / 0.05, 0.05, 0.0),
           Border_Shadow => Gtk.Enums.Shadow_Out,
           Scaled        => True);
      Widget.all.Minor_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   => ((0.0, 0.0), 1.0 / 0.45, 0.45, 0.0),
           Outer   => ((0.0, 0.0), 1.0 / 0.48, 0.48, 0.0),
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 300.0,
           Skipped => 5,
           Step    => Step / 5.0,
           From    => First,
           Scaled  => True,
           Widened => True);
      Widget.all.Major_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   => ((0.0, 0.0), 1.0 / 0.45, 0.45, 0.0),
           Outer   => ((0.0, 0.0), 1.0 / 0.48, 0.48, 0.0),
           Color   => Major_Tick_Color,
           Width   => 3.5 / 200.0,
           Step    => Step,
           From    => First,
           Scaled  => True,
           Widened => True);
      Widget.all.Pin_2 :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget.all.Background.all.Get_Foreground,
           Color         => Pin_Color,
           Outer         => ((0.0, 0.18), 1.0 / 0.015, 0.015, 0.0),
           Border_Shadow => Gtk.Enums.Shadow_Out,
           Scaled        => True);
      Widget.all.Second_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   => ((0.0, 0.18), 1.0 / 0.078, 0.078, 0.0),
           Outer   => ((0.0, 0.18), 1.0 / 0.100, 0.100, 0.0),
           Color   => Second_Tick_Color,
           Width   => 1.0 / 200.0,
           Step    => 2.0 * Ada.Numerics.Pi / 12.0,
           From    => First,
           Scaled  => True,
           Widened => True);
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
      Widget.all.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget.all.Cache,
           Ellipse   => ((0.0, 0.0), 1.0 / 0.38, 0.38, 0.0),
           Texts     => "I II III IV V VI VII VIII IX X XI XII",
           Delimiter => ' ',
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "times",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Step,
           Height    => 0.11,
           Stretch   => 0.4,
           Color     => Text_Color,
           From      => First,
           Mode      => Gtk.Layered.Rotated,
           Scaled    => True);
      Widget.all.Hour_Needle :=
        Gtk.Layered.Needle.Add_Needle
          (Under       => Widget.all.Background.all.Get_Foreground,
           Center      => (0.0, 0.0),
           Adjustment  => Adjustment,
           Tip_Length  => 0.32,
           Tip_Width   => 0.03,
           Tip_Cap     => Cairo.Cairo_Line_Cap_Square,
           Rear_Length => 0.0,
           Rear_Width  => 0.03,
           From        => 3.0 * Ada.Numerics.Pi / 2.0,
           Length      => 2.0 * Ada.Numerics.Pi,
           Color       => Hour_Needle_Color,
           Scaled      => True);
      Widget.all.Minute_Needle :=
        Gtk.Layered.Needle.Add_Needle
          (Under       => Widget.all.Background.all.Get_Foreground,
           Center      => (0.0, 0.0),
           Tip_Length  => 0.46,
           Tip_Width   => 0.015,
           Rear_Length => 0.0,
           Rear_Width  => 0.025,
           From        => 3.0 * Ada.Numerics.Pi / 2.0,
           Length      => 2.0 * Ada.Numerics.Pi,
           Color       => Minute_Needle_Color,
           Scaled      => True);
      Widget.all.Second_Needle :=
        Gtk.Layered.Needle.Add_Needle
          (Under       => Widget.all.Background.all.Get_Foreground,
           Center      => (0.0, 0.18),
           Tip_Length  => 0.097,
           Tip_Width   => 0.005,
           Rear_Length => 0.04,
           Rear_Width  => 0.015,
           From        => 3.0 * Ada.Numerics.Pi / 2.0,
           Length      => 2.0 * Ada.Numerics.Pi,
           Color       => Second_Needle_Color,
           Scaled      => True);
      if Adjustment /= null then
         Gtk.Adjustment.Ref (Adjustment);
         Widget.all.Adjustment := Adjustment;
         Gtk.Handlers.References.Set
           (Widget.all.Changed,
            Handlers.Connect
              (Adjustment,
               "changed",
               Handlers.To_Marshaller (Changed'Access),
               Widget.all'Unchecked_Access));
         Gtk.Handlers.References.Set
           (Widget.all.Value_Changed,
            Handlers.Connect
              (Adjustment,
               "value_changed",
               Handlers.To_Marshaller (Changed'Access),
               Widget.all'Unchecked_Access));
      end if;
   end Initialize;

   procedure Set
     (Widget  : not null access Gtk_Wall_Clock_Classic_Record;
      Seconds : Gdouble)
   is
      Hours  : Gdouble := Gdouble'Remainder (Seconds / 3600.0, 12.0);
      Minute : Gdouble := Gdouble'Remainder (Seconds / 60.0, 60.0);
      Sec    : Gdouble := Gdouble'Remainder (Seconds, 60.0);
   begin
      if Hours < 0.0 then
         Hours := Hours + 12.0;
      end if;
      if Minute < 0.0 then
         Minute := Minute + 60.0;
      end if;
      if Sec < 0.0 then
         Sec := Sec + 60.0;
      end if;
      Widget.all.Hour_Needle.all.Set_Value   (Hours / 12.0);
      Widget.all.Minute_Needle.all.Set_Value (Minute / 60.0);
      Widget.all.Second_Needle.all.Set_Value (Sec / 60.0);
   end Set;

   procedure Set_Value
     (Widget : not null access Gtk_Wall_Clock_Classic_Record;
      Value  : Ada.Calendar.Time) is
   begin
      Set (Widget, Gdouble (Ada.Calendar.Seconds (Value)));
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Wall_Clock_Classic_Record) is
   begin
      Widget.all.Hour_Needle.all.Set
        (Center => Widget.all.Hour_Needle.all.Get_Center,
         From   => Widget.all.Hour_Needle.all.Get_From,
         Length => Widget.all.Hour_Needle.all.Get_Length,
         Tip    =>
           (Length => Widget.all.Hour_Needle.all.Get_Tip.Length,
            Width  => Widget.all.Hour_Needle.all.Get_Tip.Width,
            Cap    =>
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "hour-needle-tip-cap")),
         Rear   =>
           (Length => Widget.all.Hour_Needle.all.Get_Rear.Length,
            Width  => Widget.all.Hour_Needle.all.Get_Rear.Width,
            Cap    =>
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "hour-needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get
             (Widget, "hour-needle-color", Hour_Needle_Color));
      Widget.all.Minute_Needle.all.Set
        (Center => Widget.all.Minute_Needle.all.Get_Center,
         From   => Widget.all.Minute_Needle.all.Get_From,
         Length => Widget.all.Minute_Needle.all.Get_Length,
         Tip    =>
           (Length => Widget.all.Minute_Needle.all.Get_Tip.Length,
            Width  => Widget.all.Minute_Needle.all.Get_Tip.Width,
            Cap    =>
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "minute-needle-tip-cap")),
         Rear   => (Length => Widget.all.Minute_Needle.all.Get_Rear.Length,
                    Width  => Widget.all.Minute_Needle.all.Get_Rear.Width,
                    Cap    =>
                      Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                        (Widget, "minute-needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get
             (Widget, "minute-needle-color", Minute_Needle_Color));
      Widget.all.Second_Needle.all.Set
        (Center => Widget.all.Second_Needle.all.Get_Center,
         From   => Widget.all.Second_Needle.all.Get_From,
         Length => Widget.all.Second_Needle.all.Get_Length,
         Tip    =>
           (Length => Widget.all.Second_Needle.all.Get_Tip.Length,
            Width  => Widget.all.Second_Needle.all.Get_Tip.Width,
            Cap    =>
              Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                (Widget, "second-needle-tip-cap")),
         Rear   => (Length => Widget.all.Second_Needle.all.Get_Rear.Length,
                    Width  => Widget.all.Second_Needle.all.Get_Rear.Width,
                    Cap    =>
                      Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                        (Widget, "second-needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get
             (Widget, "second-needle-color", Second_Needle_Color));
      Widget.all.Pin_1.all.Set
        (Outer         => Widget.all.Pin_1.all.Get_Outer,
         Inner         => Widget.all.Pin_1.all.Get_Inner,
         From          => Widget.all.Pin_1.all.Get_From,
         Length        => Widget.all.Pin_1.all.Get_Length,
         Border_Width  => Widget.all.Pin_1.all.Get_Border_Width,
         Border_Depth  => Widget.all.Pin_1.all.Get_Border_Depth,
         Border_Color  => Widget.all.Pin_1.all.Get_Border_Color,
         Border_Shadow => Widget.all.Pin_1.all.Get_Border_Shadow,
         Color         =>
           Gtk.Widget.Styles.Style_Get (Widget, "pin-color", Pin_Color));
      Widget.all.Pin_2.all.Set
        (Outer         => Widget.all.Pin_2.all.Get_Outer,
         Inner         => Widget.all.Pin_2.all.Get_Inner,
         From          => Widget.all.Pin_2.all.Get_From,
         Length        => Widget.all.Pin_2.all.Get_Length,
         Border_Width  => Widget.all.Pin_2.all.Get_Border_Width,
         Border_Depth  => Widget.all.Pin_2.all.Get_Border_Depth,
         Border_Color  => Widget.all.Pin_2.all.Get_Border_Color,
         Border_Shadow => Widget.all.Pin_2.all.Get_Border_Shadow,
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
         Line   =>
           (Widget.all.Minor_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get
              (Widget, "minor-tick-color", Minor_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "minor-tick-line-cap")));
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
      Widget.all.Second_Ticks.all.Set
        (Inner  => Widget.all.Second_Ticks.all.Get_Inner,
         Outer  => Widget.all.Second_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Second_Ticks.all.Get_Ticks,
         From   => Widget.all.Second_Ticks.all.Get_From,
         Length => Widget.all.Second_Ticks.all.Get_Length,
         Line   =>
           (Widget.all.Second_Ticks.all.Get_Line.Width,
            Gtk.Widget.Styles.Style_Get
              (Widget, "second-tick-color", Second_Tick_Color),
            Gtk.Widget.Styles.Line_Cap_Property.Style_Get
              (Widget, "second-tick-line-cap")));
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

end Gtk.Wall_Clock.Classic;
