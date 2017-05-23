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
--____________________________________________________________________--

with Ada.Numerics;
with Cairo;                  use Cairo;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Types;                use Glib.Types;
with Gtkada.Types;              use Gtkada.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;      use Pango.Cairo.Fonts;

with Cairo.Line_Cap_Property;
with Glib.Object.Checked_Destroy;
with Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Wall_Clock.Classic is

   use Cairo.Line_Cap_Property;
   use Gtk.Widget.Styles.Line_Cap_Property;

   Hour_Needle_Color   : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Minute_Needle_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Second_Needle_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Pin_Color           : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Background_Color    : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Line_Color          : constant Gdk_Color := RGB (0.4, 0.4, 0.4);
   Major_Tick_Color    : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color    : constant Gdk_Color := RGB (0.4, 0.4, 0.4);
   Second_Tick_Color   : constant Gdk_Color := RGB (0.4, 0.4, 0.4);
   Text_Color          : constant Gdk_Color := RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Step  : constant Gdouble := 2.0 * Ada.Numerics.Pi / 12.0;
   First : constant Gdouble := Step - Ada.Numerics.Pi / 2.0;

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Wall_Clock_Classic
          );

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Widget     : Gtk_Wall_Clock_Classic
             )  is
   begin
      Set (Widget, Get_Value (Widget.Adjustment));
      if (  Widget.Hour_Needle.Is_Updated
         or else
            Widget.Minute_Needle.Is_Updated
         or else
            Widget.Second_Needle.Is_Updated
         )  -- Signal draw to the widget
      then
         Queue_Draw (Widget);
      end if;
   end Changed;

   function Get_Annotation
            (  Widget : not null access Gtk_Wall_Clock_Classic_Record
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return Widget.Annotation;
   end Get_Annotation;

   function Get_Background
            (  Widget : not null access Gtk_Wall_Clock_Classic_Record
            )  return not null access Elliptic_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Cache
            (  Widget : not null access Gtk_Wall_Clock_Classic_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Hour_Hand
            (  Widget : not null access Gtk_Wall_Clock_Classic_Record
            )  return not null access Needle_Layer is
   begin
      return Widget.Hour_Needle;
   end Get_Hour_Hand;

   function Get_Minute_Hand
            (  Widget : not null access Gtk_Wall_Clock_Classic_Record
            )  return not null access Needle_Layer is
   begin
      return Widget.Minute_Needle;
   end Get_Minute_Hand;

   function Get_Second_Hand
            (  Widget : not null access Gtk_Wall_Clock_Classic_Record
            )  return not null access Needle_Layer is
   begin
      return Widget.Second_Needle;
   end Get_Second_Hand;

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
            (  Name       => "hour-needle-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Hour needle color",
               Blurb      => "The color of the hour needle"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "hour-needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the hour needle tip",
               Default => Cairo_Line_Cap_Round
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "hour-needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the hour needle rear",
               Default => Cairo_Line_Cap_Butt
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
               Default => Cairo_Line_Cap_Butt
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
               Default => Cairo_Line_Cap_Butt
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "minute-needle-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Minute needle color",
               Blurb      => "The color of the minute needle"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "minute-needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the minute needle tip",
               Default => Cairo_Line_Cap_Round
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "minute-needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the minute needle rear",
               Default => Cairo_Line_Cap_Butt
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
            (  Name       => "second-needle-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Second needle color",
               Blurb      => "The color of the second needle"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "second-needle-tip-cap",
               Nick    => "Tip cap",
               Blurb   => "The style used for the second needle tip",
               Default => Cairo_Line_Cap_Round
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "second-needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the second needle rear",
               Default => Cairo_Line_Cap_Butt
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "second-tick-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Second ticks color",
               Blurb      => "Seconf ticks color"
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "second-tick-line-cap",
               Nick    => "Second tick cap",
               Blurb   => "The line cap style used for second ticks",
               Default => Cairo_Line_Cap_Butt
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
             (  Widget     : out Gtk_Wall_Clock_Classic;
                Adjustment : Gtk_Adjustment := null
             )  is
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
             (  Widget : not null access
                         Gtk_Wall_Clock_Classic_Record'Class;
                Adjustment : Gtk_Adjustment
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Background :=
         Add_Elliptic_Background
         (  Under  => Widget,
            Color  => Background_Color,
            Outer  => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
            Scaled => True
         );
      Widget.Line_1 :=
         Add_Arc
         (  Under   => Widget.Background.Get_Foreground,
            Color   => Line_Color,
            Width   => 1.0 / 300.0,
            Ellipse => ((0.0, 0.0), 1.0 / 0.48, 0.48, 0.0),
            Widened => True,
            Scaled  => True
         );
      Widget.Line_2 :=
         Add_Arc
         (  Under   => Widget.Background.Get_Foreground,
            Color   => Line_Color,
            Width   => 1.0 / 300.0,
            Ellipse => ((0.0, 0.0), 1.0 / 0.45, 0.45, 0.0),
            Widened => True,
            Scaled  => True
         );
      Widget.Line_3 :=
         Add_Arc
         (  Under   => Widget.Background.Get_Foreground,
            Color   => Line_Color,
            Width   => 1.0 / 300.0,
            Ellipse => ((0.0, 0.0), 1.0 / 0.31, 0.31, 0.0),
            Widened => True,
            Scaled  => True
         );
      Widget.Pin_1 :=
         Add_Elliptic_Background
         (  Under         => Widget.Background.Get_Foreground,
            Color         => Pin_Color,
            Outer         => ((0.0, 0.0), 1.0 / 0.05, 0.05, 0.0),
            Border_Shadow => Shadow_Out,
            Scaled        => True
         );
      Widget.Minor_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / 0.45, 0.45, 0.0),
            Outer   => ((0.0, 0.0), 1.0 / 0.48, 0.48, 0.0),
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 300.0,
            Skipped => 5,
            Step    => Step / 5.0,
            From    => First,
            Scaled  => True,
            Widened => True
         );
      Widget.Major_Ticks :=
         Add_Elliptic_Scale
         (  Under   => Widget.Background.Get_Foreground,
            Inner   => ((0.0, 0.0), 1.0 / 0.45, 0.45, 0.0),
            Outer   => ((0.0, 0.0), 1.0 / 0.48, 0.48, 0.0),
            Color   => Major_Tick_Color,
            Width   => 3.5 / 200.0,
            Step    => Step,
            From    => First,
            Scaled  => True,
            Widened => True
         );
      Widget.Pin_2 :=
         Add_Elliptic_Background
         (  Under         => Widget.Background.Get_Foreground,
            Color         => Pin_Color,
            Outer         => ((0.0, 0.18), 1.0 / 0.015, 0.015, 0.0),
            Border_Shadow => Shadow_Out,
            Scaled        => True
         );
      Widget.Second_Ticks :=
        Add_Elliptic_Scale
          (Under   => Widget.Background.Get_Foreground,
           Inner   => ((0.0, 0.18), 1.0 / 0.078, 0.078, 0.0),
           Outer   => ((0.0, 0.18), 1.0 / 0.100, 0.100, 0.0),
           Color   => Second_Tick_Color,
           Width   => 1.0 / 200.0,
           Step    => 2.0 * Ada.Numerics.Pi / 12.0,
           From    => First,
           Scaled  => True,
           Widened => True);
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
      Widget.Annotation :=
         Add_Elliptic_Annotation
         (  Under     => Widget.Cache,
            Ellipse   => ((0.0, 0.0), 1.0 / 0.38, 0.38, 0.0),
            Texts     => "I II III IV V VI VII VIII IX X XI XII",
            Delimiter => ' ',
            Face      => Create_Toy
                         (  Family => "times",
                            Slant  => Cairo_Font_Slant_Normal,
                            Weight => Cairo_Font_Weight_Bold
                         ),
            Step      => Step,
            Height    => 0.11,
            Stretch   => 0.4,
            Color     => Text_Color,
            From      => First,
            Mode      => Rotated,
            Scaled    => True
         );
      Widget.Hour_Needle :=
        Add_Needle
          (Under       => Widget.Background.Get_Foreground,
           Center      => (0.0, 0.0),
           Adjustment  => Adjustment,
           Tip_Length  => 0.32,
           Tip_Width   => 0.03,
           Tip_Cap     => Cairo_Line_Cap_Square,
           Rear_Length => 0.0,
           Rear_Width  => 0.03,
           From        => 3.0 * Ada.Numerics.Pi / 2.0,
           Length      => 2.0 * Ada.Numerics.Pi,
           Color       => Hour_Needle_Color,
           Scaled      => True);
      Widget.Minute_Needle :=
        Add_Needle
          (Under       => Widget.Background.Get_Foreground,
           Center      => (0.0, 0.0),
           Tip_Length  => 0.46,
           Tip_Width   => 0.015,
           Rear_Length => 0.0,
           Rear_Width  => 0.025,
           From        => 3.0 * Ada.Numerics.Pi / 2.0,
           Length      => 2.0 * Ada.Numerics.Pi,
           Color       => Minute_Needle_Color,
           Scaled      => True);
      Widget.Second_Needle :=
        Add_Needle
          (Under       => Widget.Background.Get_Foreground,
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
         Ref (Adjustment);
         Widget.Adjustment := Adjustment;
         Set
         (  Widget.Changed,
            Handlers.Connect
            (  Adjustment,
               "changed",
               Handlers.To_Marshaller (Changed'Access),
               Widget.all'Unchecked_Access
         )  );
         Set
         (  Widget.Value_Changed,
            Handlers.Connect
            (  Adjustment,
               "value_changed",
               Handlers.To_Marshaller (Changed'Access),
               Widget.all'Unchecked_Access
         )  );
      end if;
   end Initialize;

   procedure Set
             (  Widget  : not null access Gtk_Wall_Clock_Classic_Record;
                Seconds : Gdouble
             )  is
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
      Widget.Hour_Needle.Set_Value   (Hours / 12.0);
      Widget.Minute_Needle.Set_Value (Minute / 60.0);
      Widget.Second_Needle.Set_Value (Sec / 60.0);
   end Set;

   procedure Set_Value
             (  Widget : not null access Gtk_Wall_Clock_Classic_Record;
                Value  : Time
             )  is
   begin
      Set (Widget, Gdouble (Seconds (Value)));
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Wall_Clock_Classic_Record
             )  is
   begin
      Widget.Hour_Needle.Set
      (  Center => Widget.Hour_Needle.Get_Center,
         From   => Widget.Hour_Needle.Get_From,
         Length => Widget.Hour_Needle.Get_Length,
         Tip    => (  Length => Widget.Hour_Needle.Get_Tip.Length,
                      Width  => Widget.Hour_Needle.Get_Tip.Width,
                      Cap => Style_Get (Widget, "hour-needle-tip-cap")
                   ),
         Rear   => (  Length => Widget.Hour_Needle.Get_Rear.Length,
                      Width  => Widget.Hour_Needle.Get_Rear.Width,
                      Cap => Style_Get (Widget, "hour-needle-rear-cap")
                   ),
         Color  => Style_Get
                   (  Widget,
                      "hour-needle-color",
                      Hour_Needle_Color
      )            );
      Widget.Minute_Needle.Set
      (  Center => Widget.Minute_Needle.Get_Center,
         From   => Widget.Minute_Needle.Get_From,
         Length => Widget.Minute_Needle.Get_Length,
         Tip    => (  Length => Widget.Minute_Needle.Get_Tip.Length,
                      Width  => Widget.Minute_Needle.Get_Tip.Width,
                      Cap => Style_Get (Widget, "minute-needle-tip-cap")
                   ),
         Rear   => (  Length => Widget.Minute_Needle.Get_Rear.Length,
                      Width  => Widget.Minute_Needle.Get_Rear.Width,
                      Cap => Style_Get
                             (  Widget,
                                "minute-needle-rear-cap"
                   )         ),
         Color  => Style_Get
                   (  Widget,
                      "minute-needle-color",
                      Minute_Needle_Color
      )            );
      Widget.Second_Needle.Set
      (  Center => Widget.Second_Needle.Get_Center,
         From   => Widget.Second_Needle.Get_From,
         Length => Widget.Second_Needle.Get_Length,
         Tip    => (  Length => Widget.Second_Needle.Get_Tip.Length,
                      Width  => Widget.Second_Needle.Get_Tip.Width,
                      Cap => Style_Get (Widget, "second-needle-tip-cap")
                   ),
         Rear   => (  Length => Widget.Second_Needle.Get_Rear.Length,
                      Width  => Widget.Second_Needle.Get_Rear.Width,
                      Cap => Style_Get
                             (  Widget,
                                "second-needle-rear-cap"
                   )         ),
         Color  => Style_Get
                   (  Widget,
                      "second-needle-color",
                      Second_Needle_Color
      )            );
      Widget.Pin_1.Set
      (  Outer         => Widget.Pin_1.Get_Outer,
         Inner         => Widget.Pin_1.Get_Inner,
         From          => Widget.Pin_1.Get_From,
         Length        => Widget.Pin_1.Get_Length,
         Border_Width  => Widget.Pin_1.Get_Border_Width,
         Border_Depth  => Widget.Pin_1.Get_Border_Depth,
         Border_Color  => Widget.Pin_1.Get_Border_Color,
         Border_Shadow => Widget.Pin_1.Get_Border_Shadow,
         Color         => Style_Get (Widget, "pin-color", Pin_Color)
      );
      Widget.Pin_2.Set
      (  Outer         => Widget.Pin_2.Get_Outer,
         Inner         => Widget.Pin_2.Get_Inner,
         From          => Widget.Pin_2.Get_From,
         Length        => Widget.Pin_2.Get_Length,
         Border_Width  => Widget.Pin_2.Get_Border_Width,
         Border_Depth  => Widget.Pin_2.Get_Border_Depth,
         Border_Color  => Widget.Pin_2.Get_Border_Color,
         Border_Shadow => Widget.Pin_2.Get_Border_Shadow,
         Color         => Style_Get (Widget, "pin-color", Pin_Color)
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
      Widget.Line_1.Set
      (  Ellipse => Widget.Line_1.Get_Ellipse,
         From    => Widget.Line_1.Get_From,
         Length  => Widget.Line_1.Get_Length,
         Line    => (  Widget.Line_1.Get_Line.Width,
                       Style_Get (Widget, "line-color", Line_Color),
                       Widget.Line_1.Get_Line.Line_Cap
      )             );
      Widget.Line_2.Set
      (  Ellipse => Widget.Line_2.Get_Ellipse,
         From    => Widget.Line_2.Get_From,
         Length  => Widget.Line_2.Get_Length,
         Line    => (  Widget.Line_2.Get_Line.Width,
                       Style_Get (Widget, "line-color", Line_Color),
                       Widget.Line_2.Get_Line.Line_Cap
      )             );
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
      Widget.Second_Ticks.Set
      (  Inner  => Widget.Second_Ticks.Get_Inner,
         Outer  => Widget.Second_Ticks.Get_Outer,
         Ticks  => Widget.Second_Ticks.Get_Ticks,
         From   => Widget.Second_Ticks.Get_From,
         Length => Widget.Second_Ticks.Get_Length,
         Line =>
            (  Widget.Second_Ticks.Get_Line.Width,
               Style_Get
               (  Widget,
                  "second-tick-color",
                  Second_Tick_Color
               ),
               Style_Get (Widget, "second-tick-line-cap")
      )     );
      Widget.Annotation.Set
      (  Ellipse => Widget.Annotation.Get_Ellipse,
         Ticks   => Widget.Annotation.Get_Ticks,
         From    => Widget.Annotation.Get_From,
         Length  => Widget.Annotation.Get_Length,
         Face    => Widget.Annotation.Get_Face,
         Mode    => Widget.Annotation.Get_Mode,
         Height  => Widget.Annotation.Get_Height,
         Stretch => Widget.Annotation.Get_Stretch,
         Color   => Style_Get (Widget, "text-color", Text_Color)
      );
   end Style_Changed;

end Gtk.Wall_Clock.Classic;
