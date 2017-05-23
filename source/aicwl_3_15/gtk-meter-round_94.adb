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
--____________________________________________________________________--

with Ada.Numerics;
with Cairo;                     use Cairo;
with Cairo.Ellipses;            use Cairo.Ellipses;
with Cairo.Line_Cap_Property;   use Cairo.Line_Cap_Property;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Types;                use Glib.Types;
with Gtkada.Types;              use Gtkada.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with Glib.Object.Checked_Destroy;
with Gtk.Widget.Styles.Line_Cap_Property;

package body Gtk.Meter.Round_94 is
   use Gtk.Widget.Styles.Line_Cap_Property;

   Needle_Color      : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Background_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Line_Color        : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Pin_Color         : constant Gdk_Color := RGB (0.6, 0.6, 0.6);
   Text_Color        : constant Gdk_Color := RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Center : constant Cairo_Tuple := (0.0, 0.0855);
   Corner : constant := 1.0 / 30.0;
   Length : constant := 94.0 * Ada.Numerics.Pi / 180.0;
   Pin    : constant := 0.4;
   First  : constant := (Ada.Numerics.Pi * 3.0 - Length) / 2.0;
   From   : constant := 41.0 * Ada.Numerics.Pi / 180.0;
   Shift  : constant := -32.0 * Ada.Numerics.Pi / 180.0;

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
               Default => Cairo_Line_Cap_Round
         )  );
         Install_Style
         (  Class_Ref (Class_Record.The_Type),
            Cairo.Line_Cap_Property.Gnew_Enum
            (  Name    => "needle-rear-cap",
               Nick    => "Rear cap",
               Blurb   => "The style used for the needle rear",
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
             (  Widget : not null access
                         Gtk_Meter_Round_94_Record'Class;
                Sectors : Positive
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.Sectors := Sectors;
      Set_Aspect_Ratio (Widget, 1.207);
      Widget.Background :=
        Add_Elliptic_Background
          (Under         => Widget,
           Outer         => (Center, 1.0 / 0.5, 0.5, 0.0),
           From          => Ada.Numerics.Pi - From,
           Length        => Ada.Numerics.Pi + 2.0 * From,
           Color         => Background_Color,
           Border_Width  => 0.01,
           Border_Depth  => 0.005,
           Border_Shadow => Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.Line_1 :=
         Add_Arc
         (  Under    => Widget.Background.Get_Foreground,
            Color    => Line_Color,
            Width    => 1.5 / 200.0,
            Ellipse  => ((0.0, Pin), 1.0 / 0.47, 0.47, 0.0),
            From     => First,
            Length   => Length,
            Line_Cap => Cairo_Line_Cap_Round,
            Widened  => True,
            Scaled   => True
         );
      Widget.Line_2 :=
         Add_Arc
         (  Under   => Widget.Background.Get_Foreground,
            Color   => Line_Color,
            Width   => 1.0 / 400.0,
            Ellipse => ((0.0, Pin), 1.0 / 0.52, 0.52, 0.0),
            From    => First,
            Length  => Length,
            Widened => True,
            Scaled  => True
         );
      Widget.Major_Ticks :=
         Add_Elliptic_Scale
         (  Under => Widget.Background.Get_Foreground,
            Outer =>
               (  Center          => (0.0, Pin),
                  Major_Curvature => 1.0 / 0.56,
                  Minor_Radius    => 0.56,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (0.0, Pin),
                  Major_Curvature => 1.0 / 0.47,
                  Minor_Radius    => 0.47,
                  Angle           => 0.0
               ),
            Color   => Major_Tick_Color,
            Width   => 1.5 / 200.0,
            Step    => Length / Gdouble (Sectors),
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Middle_Ticks :=
         Add_Elliptic_Scale
         (  Under => Widget.Background.Get_Foreground,
            Outer =>
               (  Center          => (0.0, Pin),
                  Major_Curvature => 1.0 / 0.54,
                  Minor_Radius    => 0.54,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (0.0, Pin),
                  Major_Curvature => 1.0 / 0.47,
                  Minor_Radius    => 0.47,
                  Angle           => 0.0
               ),
            Color   => Middle_Tick_Color,
            Width   => 1.5 / 400.0,
            Step    => 0.5 * Length / Gdouble (Sectors),
            Skipped => 2,
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Minor_Ticks :=
         Add_Elliptic_Scale
         (  Under => Widget.Background.Get_Foreground,
            Outer =>
               (  Center          => (0.0, Pin),
                  Major_Curvature => 1.0 / 0.52,
                  Minor_Radius    => 0.52,
                  Angle           => 0.0
               ),
            Inner =>
               (  Center          => (0.0, Pin),
                  Major_Curvature => 1.0 / 0.47,
                  Minor_Radius    => 0.47,
                  Angle           => 0.0
               ),
            Color   => Minor_Tick_Color,
            Width   => 1.0 / 400.0,
            Step    => 0.1 * Length / Gdouble (Sectors),
            Skipped => 5,
            From    => First,
            Length  => Length,
            Scaled  => True,
            Widened => True
         );
      Widget.Pin :=
        Add_Elliptic_Background
          (Under         => Widget.Background.Get_Foreground,
           Outer         => ((0.0, Pin), 1.0 / 0.11, 0.11, 0.0),
           From          => Ada.Numerics.Pi - (From + Shift),
           Length        => Ada.Numerics.Pi + 2.0 * (From + Shift),
           Color         => Pin_Color,
           Border_Width  => 0.012,
           Border_Depth  => 0.005,
           Border_Shadow => Shadow_Etched_In,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
   end Create_Background;

   procedure Create_Foreground
             (  Widget : not null access
                         Gtk_Meter_Round_94_Record'Class;
                Adjustment : Gtk_Adjustment
             )  is
   begin
      Widget.Needle :=
         Add_Needle
         (  Under       => Widget.Background.Get_Foreground,
            Center      => (0.0, Pin),
            Tip_Cap     => Cairo_Line_Cap_Square,
            Adjustment  => Adjustment,
            Tip_Length  => 0.52,
            Tip_Width   => 0.008,
            Rear_Length => 0.0,
            Rear_Width  => 0.025,
            Rear_Cap    => Cairo_Line_Cap_Round,
            Color       => Needle_Color,
            From        => First,
            Length      => Length,
            Scaled      => True
         );
   end Create_Foreground;

   function Get_Annotation
            (  Widget : not null access
                        Gtk_Meter_Round_94_Record
            )  return not null access Elliptic_Annotation_Layer is
   begin
      return Widget.Annotation;
   end Get_Annotation;

   function Get_Needle
            (  Widget : not null access
                        Gtk_Meter_Round_94_Record
            )  return not null access Needle_Layer is
   begin
      return Widget.Needle;
   end Get_Needle;

   function Get_Background
            (  Widget : not null access
                        Gtk_Meter_Round_94_Record
            )  return not null access Elliptic_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Cache
            (  Widget : not null access
                        Gtk_Meter_Round_94_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   procedure Gtk_New
             (  Widget     : out Gtk_Meter_Round_94;
                Texts      : Gtk.Enums.String_List.Glist;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
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
             (  Widget     : out Gtk_Meter_Round_94;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
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
             (  Widget     : out Gtk_Meter_Round_94;
                Texts      : UTF8_String;
                Delimiter  : Character      := ' ';
                Adjustment : Gtk_Adjustment := null;
                Sectors    : Positive       := 5
             )  is
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
             (  Widget : not null access
                         Gtk_Meter_Round_94_Record'Class;
                Texts      : Gtk.Enums.String_List.Glist;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget.Cache,
            Ellipse =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / 0.6,
                  Minor_Radius    => 0.6,
                  Angle           => 0.0
               ),
            Texts     => Texts,
            Face      => Create_Toy
                         (  Family => "arial",
                            Slant  => Cairo_Font_Slant_Normal,
                            Weight => Cairo_Font_Weight_Bold
                         ),
            Step      => Length / Gdouble (Sectors),
            Height    => 0.05,
            Stretch   => 1.0,
            Color     => Text_Color,
            From      => First,
            Length    => Length,
            Mode      => Rotated,
            Scaled    => True
         );
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Meter_Round_94_Record'Class;
                Texts      : Controlled_String_List;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Initialize (Widget, Get_GList (Texts), Adjustment, Sectors);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Meter_Round_94_Record'Class;
                Texts      : UTF8_String;
                Delimiter  : Character;
                Adjustment : Gtk_Adjustment;
                Sectors    : Positive
             )  is
   begin
      Create_Background (Widget, Sectors);
      Widget.Annotation :=
         Add_Elliptic_Annotation
         (  Under   => Widget.Cache,
            Ellipse =>
               (  Center          => (0.0, 0.0 + Pin),
                  Major_Curvature => 1.0 / 0.6,
                  Minor_Radius    => 0.6,
                  Angle           => 0.0
               ),
            Texts     => Texts,
            Delimiter => Delimiter,
            Face      => Create_Toy
                         (  Family => "arial",
                            Slant  => Cairo_Font_Slant_Normal,
                            Weight => Cairo_Font_Weight_Bold
                         ),
            Step      => Length / Gdouble (Sectors),
            Height    => 0.05,
            Stretch   => 1.0,
            Color     => Text_Color,
            From      => First,
            Length    => Length,
            Mode      => Rotated,
            Scaled    => True
         );
      Create_Foreground (Widget, Adjustment);
   end Initialize;

   procedure Set_Value
             (  Widget : not null access
                         Gtk_Meter_Round_94_Record;
                Value  : Gdouble
             )  is
   begin
      Widget.Needle.Set_Value (Value);
   end Set_Value;

   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Meter_Round_94_Record
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
      Widget.Middle_Ticks.Set
      (  Inner  => Widget.Middle_Ticks.Get_Inner,
         Outer  => Widget.Middle_Ticks.Get_Outer,
         Ticks  => Widget.Middle_Ticks.Get_Ticks,
         From   => Widget.Middle_Ticks.Get_From,
         Length => Widget.Middle_Ticks.Get_Length,
         Line =>
            (  Widget.Middle_Ticks.Get_Line.Width,
               Style_Get (Widget, "middle-tick-color", Middle_Tick_Color),
               Style_Get (Widget, "middle-tick-line-cap")
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
      Widget.Pin.Set
      (  Outer          => Widget.Pin.Get_Outer,
         Inner          => Widget.Pin.Get_Inner,
         From           => Widget.Pin.Get_From,
         Length         => Widget.Pin.Get_Length,
         Border_Width   => Widget.Pin.Get_Border_Width,
         Border_Depth   => Widget.Pin.Get_Border_Depth,
         Border_Color   => Widget.Pin.Get_Border_Color,
         Border_Shadow  => Widget.Pin.Get_Border_Shadow,
         Color  =>
            Style_Get (Widget, "pin-color", Background_Color)
      );
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

end Gtk.Meter.Round_94;
