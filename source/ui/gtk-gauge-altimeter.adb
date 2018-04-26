--                                                                    --
--  package                         Copyright (c)  Heisenbug Ltd.     --
--     Gtk.Gauge.Altimeter                         Brussels           --
--  Implementation                                 January 2018       --
--                                                                    --
--                                Last revision :  09:41 09 Jan 2018  --
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
with Gtk.Layered.Abstract_Bordered;
with Gtk.Colors;
with Gtk.Missed;
with Gtk.Widget.Styles.Line_Cap_Property;
with Pango.Cairo.Fonts;

package body Gtk.Gauge.Altimeter is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Pi : constant := Ada.Numerics.Pi;

   Needle_Color_1    : constant Gdk.Color.Gdk_Color := Gtk.Colors.Red;
   Needle_Color_2    : constant Gdk.Color.Gdk_Color := Gtk.Colors.Yellow;
   Needle_Color_3    : constant Gdk.Color.Gdk_Color := Gtk.Colors.Green;

   Pin_Color         : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red => 0.70, Green => 0.70, Blue => 0.70);
   Background_Color  : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red => 0.17, Green => 0.12, Blue => 0.11);
   Major_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Colors.White;
   Middle_Tick_Color : constant Gdk.Color.Gdk_Color := Gtk.Colors.White;
   Minor_Tick_Color  : constant Gdk.Color.Gdk_Color := Gtk.Colors.White;
   Text_Color        : constant Gdk.Color.Gdk_Color := Gtk.Colors.White;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   The_Center   : constant Cairo.Ellipses.Cairo_Tuple :=
                    Cairo.Ellipses.Cairo_Tuple'(X => 0.0, Y => 0.0);
   From_North   : constant Glib.Gdouble               := 6.0 * Pi / 4.0;
   Full_Circle  : constant Glib.Gdouble               := 8.0 * Pi / 4.0;

   procedure Create_Background
     (Widget  : not null access Gtk_Gauge_Altimeter_Record'Class;
      Sectors : in              Positive);

   procedure Create_Background
     (Widget  : not null access Gtk_Gauge_Altimeter_Record'Class;
      Sectors : in              Positive) is
   begin
      G_New (Object => Widget, Typ => Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Sectors := Sectors;
      Widget.all.Background :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget,
           Color         => Background_Color,
           Outer         =>
             Cairo.Ellipses.Ellipse_Parameters'(Center          => The_Center,
                                                Major_Curvature => 1.0 / 0.5,
                                                Minor_Radius    => 0.5,
                                                Angle           => 0.0),
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
           Outer         =>
             Cairo.Ellipses.Ellipse_Parameters'(Center          => The_Center,
                                                Major_Curvature => 1.0 / 0.10,
                                                Minor_Radius    => 0.10,
                                                Angle           => 0.0),
           Border_Depth  => 0.01,
           Border_Width  => 0.02,
           Border_Color  =>
             Gtk.Layered.Abstract_Bordered.Border_Color_Type'
               (Style_Color => False,
                Color       => Pin_Color),
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
      Widget.all.Minor_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.45,
                Minor_Radius    => 0.45,
                Angle           => Pi * 3.0 / 4.0),
           Outer   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.47,
                Minor_Radius    => 0.47,
                Angle           => Pi * 3.0 / 4.0),
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 200.0,
           Skipped => 5,
           Step    => Full_Circle / (10.0 * Gdouble (Widget.all.Sectors)),
           From    => From_North,
           Length  => Full_Circle,
           Scaled  => True,
           Widened => True);
      Widget.all.Middle_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.44,
                Minor_Radius    => 0.44,
                Angle           => Pi * 3.0 / 4.0),
           Outer   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.47,
                Minor_Radius    => 0.47,
                Angle           => Pi * 3.0 / 4.0),
           Color   => Middle_Tick_Color,
           Width   => 1.5 / 200.0,
           Skipped => 2,
           Step    => Full_Circle / (2.0 * Gdouble (Widget.all.Sectors)),
           From    => From_North,
           Length  => Full_Circle,
           Scaled  => True,
           Widened => True);
      Widget.all.Major_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.42,
                Minor_Radius    => 0.42,
                Angle           => Pi * 3.0 / 4.0),
           Outer   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.47,
                Minor_Radius    => 0.47,
                Angle           => Pi * 3.0 / 4.0),
           Color   => Major_Tick_Color,
           Width   => 2.2 / 200.0,
           Step    => Full_Circle / Gdouble (Widget.all.Sectors),
           From    => From_North,
           Length  => Full_Circle,
           Scaled  => True,
           Widened => True);
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
   end Create_Background;

   procedure Create_Needle
     (Widget     : not null access Gtk_Gauge_Altimeter_Record'Class;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment);

   procedure Create_Needle
     (Widget     : not null access Gtk_Gauge_Altimeter_Record'Class;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment)
   is
      Rear_Length : constant Glib.Gdouble               := -0.095;
      Tip_Cap     : constant Cairo.Cairo_Line_Cap       :=
                      Cairo.Cairo_Line_Cap_Square;
      --  Constants for all needles.
      --  Where the needle starts (Rear_Length), and tip style (Tip_Cap).
   begin
      Widget.all.Needle_Main :=
         Gtk.Layered.Needle.Add_Needle
          (Under       => Widget,
           Center      => The_Center,
           From        => From_North,
           Length      => Full_Circle,
           Tip_Cap     => Tip_Cap,
           Adjustment  => Adjustment,
           Tip_Length  => 0.35,
           Tip_Width   => 0.0525,
           Rear_Length => Rear_Length,
           Rear_Width  => 0.03,
           Color       => Needle_Color_1,
           Scaled      => True);
      Widget.all.Needle_10ths :=
         Gtk.Layered.Needle.Add_Needle
          (Under       => Widget.all.Needle_Main,
           Center      => The_Center,
           From        => From_North,
           Length      => Full_Circle,
           Tip_Cap     => Tip_Cap,
           Adjustment  => Adjustment,
           Tip_Length  => 0.38,
           Tip_Width   => 0.04375,
           Rear_Length => Rear_Length,
           Rear_Width  => 0.025,
           Color       => Needle_Color_2,
           Scaled      => True);
      Widget.all.Needle_100ths :=
         Gtk.Layered.Needle.Add_Needle
          (Under       => Widget.all.Needle_10ths,
           Center      => The_Center,
           From        => From_North,
           Length      => Full_Circle,
           Tip_Cap     => Tip_Cap,
           Adjustment  => Adjustment,
           Tip_Length  => 0.41,
           Tip_Width   => 0.035,
           Rear_Length => Rear_Length,
           Rear_Width  => 0.02,
           Color       => Needle_Color_3,
           Scaled      => True);
   end Create_Needle;

   function Get_Annotation
     (Widget : not null access Gtk_Gauge_Altimeter_Record)
      return not null access
     Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer is
   begin
      return Widget.all.Annotation;
   end Get_Annotation;

   function Get_Background
     (Widget : not null access Gtk_Gauge_Altimeter_Record)
      return not null access
     Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Cache
     (Widget : not null access Gtk_Gauge_Altimeter_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Needle
     (Widget : not null access Gtk_Gauge_Altimeter_Record)
      return not null access Gtk.Layered.Needle.Needle_Layer is
   begin
      return Widget.all.Needle_Main;
   end Get_Needle;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record (Ancestor     => Gtk.Layered.Get_Type,
                                  Class_Record => Class_Record'Access,
                                  Type_Name    => Class_Name)
      then
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (T => Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "needle-color-1",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Needle color",
                 Blurb      => "The color of the gauge's main needle"));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "needle-color-2",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Needle color",
                 Blurb      => "The color of the gauge's tenths needle"));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "needle-color-3",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Needle color",
                 Blurb      => "The color of the gauge's hundreths needle"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Class     => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Enum_Spec =>
              Cairo.Line_Cap_Property.Gnew_Enum
                (Name    => "needle-tip-cap",
                 Nick    => "Tip cap",
                 Blurb   => "The style used for the needle tip",
                 Default => Cairo.Cairo_Line_Cap_Square));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Class     => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Enum_Spec =>
              Cairo.Line_Cap_Property.Gnew_Enum
                (Name    => "needle-rear-cap",
                 Nick    => "Rear cap",
                 Blurb   => "The style used for the needle rear",
                 Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "background-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Background color",
                 Blurb      => "The background color"));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "major-tick-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Major ticks color",
                 Blurb      => "Major ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Class     => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Enum_Spec =>
              Cairo.Line_Cap_Property.Gnew_Enum
                (Name    => "major-tick-line-cap",
                 Nick    => "Major tick cap",
                 Blurb   => "The line cap style used for major ticks",
                 Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "middle-tick-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Middle ticks color",
                 Blurb      => "Middle ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Class     => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Enum_Spec =>
              Cairo.Line_Cap_Property.Gnew_Enum
                (Name    => "middle-tick-line-cap",
                 Nick    => "Middle tick cap",
                 Blurb   => "The line cap style used for middle ticks",
                 Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "minor-tick-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Minor ticks color",
                 Blurb      => "Minor ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Class => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Enum_Spec =>
              Cairo.Line_Cap_Property.Gnew_Enum
                (Name    => "minor-tick-line-cap",
                 Nick    => "Minor tick cap",
                 Blurb   => "The line cap style used for minor ticks",
                 Default => Cairo.Cairo_Line_Cap_Butt));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "pin-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Pin color",
                 Blurb      => "Arrow pin color"));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "text-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Text color",
                 Blurb      => "Text color"));
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New (Widget     : out Gtk_Gauge_Altimeter;
                      Texts      : in  Gtk.Enums.String_List.Glist;
                      Adjustment : in  Gtk.Adjustment.Gtk_Adjustment := null;
                      Sectors    : in  Positive                      := 12) is
   begin
      Widget := new Gtk_Gauge_Altimeter_Record;
      Initialize (Widget     => Widget,
                  Texts      => Texts,
                  Adjustment => Adjustment,
                  Sectors    => Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget     : out Gtk_Gauge_Altimeter;
      Texts      : in  Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : in  Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : in  Positive                      := 12) is
   begin
      Widget := new Gtk_Gauge_Altimeter_Record;
      Initialize (Widget     => Widget,
                  Texts      => Texts,
                  Adjustment => Adjustment,
                  Sectors    => Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New (Widget     : out Gtk_Gauge_Altimeter;
                      Texts      : in  UTF8_String;
                      Delimiter  : in  Character                     := ' ';
                      Adjustment : in  Gtk.Adjustment.Gtk_Adjustment := null;
                      Sectors    : in  Positive                      := 12) is
   begin
      Widget := new Gtk_Gauge_Altimeter_Record;
      Initialize (Widget     => Widget,
                  Texts      => Texts,
                  Delimiter  => Delimiter,
                  Adjustment => Adjustment,
                  Sectors    => Sectors);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Altimeter_Record'Class;
      Texts      : in              Gtk.Enums.String_List.Glist;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : in              Positive) is
   begin
      Create_Background (Widget  => Widget,
                         Sectors => Sectors);
      Widget.all.Annotation :=
         Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under   => Widget.all.Cache,
           Ellipse =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.35,
                Minor_Radius    => 0.35,
                Angle           => 3.0 * Pi / 4.0),
           Texts   => Texts,
           Face    =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step    => Full_Circle / Gdouble (Sectors),
           Height  => 0.1,
           Stretch => 1.0,
           Color   => Text_Color,
           From    => From_North,
           Length  => Full_Circle,
           Mode    => Gtk.Layered.Moved_Centered,
           Scaled  => True);
      Create_Needle (Widget     => Widget,
                     Adjustment => Adjustment);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Altimeter_Record'Class;
      Texts      : in              Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : in              Positive) is
   begin
      Initialize (Widget     => Widget,
                  Texts      => Gtk.Enums.String_Lists.Get_GList (Texts),
                  Adjustment => Adjustment,
                  Sectors    => Sectors);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Gauge_Altimeter_Record'Class;
      Texts      : in              UTF8_String;
      Delimiter  : in              Character;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : in              Positive) is
   begin
      Create_Background (Widget  => Widget,
                         Sectors => Sectors);
      Widget.all.Annotation :=
         Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget.all.Cache,
           Ellipse   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => The_Center,
                Major_Curvature => 1.0 / 0.36,
                Minor_Radius    => 0.36,
                Angle           => 3.0 * Pi / 4.0),
           Texts     => Texts,
           Delimiter => Delimiter,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Full_Circle / Gdouble (Sectors),
           Height    => 0.1,
           Stretch   => 0.4,
           Color     => Text_Color,
           From      => From_North,
           Length    => Full_Circle,
           Mode      => Gtk.Layered.Rotated,
           Scaled    => True);
      Create_Needle (Widget     => Widget,
                     Adjustment => Adjustment);
   end Initialize;

   procedure Set_Value
     (Widget : not null access Gtk_Gauge_Altimeter_Record;
      Value  : in              Gdouble)
   is
      Clamped_Value : constant Gdouble :=
                        Gdouble'Min (1.0, Gdouble'Max (0.0, Value));
   begin
      Set_1s_Needle :
      declare
         Ones : constant Glib.Gdouble :=
                  Glib.Gdouble'Remainder (Clamped_Value, 1.0);
      begin
         Widget.all.Needle_Main.all.Set_Value
           (Value => (if Ones < 0.0 then 1.0 + Ones else Ones));
      end Set_1s_Needle;

      Set_Tenths_Needle :
      declare
         Tenths : constant Glib.Gdouble :=
                    Glib.Gdouble'Remainder (Clamped_Value, 0.1);
      begin
         Widget.all.Needle_10ths.all.Set_Value
           (Value =>
              10.0 * (if Tenths < 0.0 then 0.1 + Tenths else Tenths));
      end Set_Tenths_Needle;

      Set_Hundreths_Needle :
      declare
         Hundreths : constant Glib.Gdouble :=
                       Glib.Gdouble'Remainder (Clamped_Value, 0.01);
      begin
         Widget.all.Needle_100ths.all.Set_Value
           (Value =>
              100.0 * (if Hundreths < 0.0 then 0.01 + Hundreths else Hundreths));
      end Set_Hundreths_Needle;
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Gauge_Altimeter_Record) is
   begin
      Widget.all.Needle_Main.all.Set
        (Center => Widget.all.Needle_Main.all.Get_Center,
         From   => Widget.all.Needle_Main.all.Get_From,
         Length => Widget.all.Needle_Main.all.Get_Length,
         Tip    =>
           Gtk.Layered.End_Parameters'
             (Length => Widget.all.Needle_Main.all.Get_Tip.Length,
              Width  => Widget.all.Needle_Main.all.Get_Tip.Width,
              Cap    =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "needle-tip-cap")),
         Rear   =>
           Gtk.Layered.End_Parameters'
             (Length => Widget.all.Needle_Main.all.Get_Rear.Length,
              Width  => Widget.all.Needle_Main.all.Get_Rear.Width,
              Cap    =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get (Widget        => Widget,
                                        Property_Name => "needle-color-1",
                                        Default       => Needle_Color_1));
      Widget.all.Needle_10ths.all.Set
        (Center => Widget.all.Needle_10ths.all.Get_Center,
         From   => Widget.all.Needle_10ths.all.Get_From,
         Length => Widget.all.Needle_10ths.all.Get_Length,
         Tip    =>
           Gtk.Layered.End_Parameters'
             (Length => Widget.all.Needle_10ths.all.Get_Tip.Length,
              Width  => Widget.all.Needle_10ths.all.Get_Tip.Width,
              Cap    =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "needle-tip-cap")),
         Rear   =>
           Gtk.Layered.End_Parameters'
             (Length => Widget.all.Needle_10ths.all.Get_Rear.Length,
              Width  => Widget.all.Needle_10ths.all.Get_Rear.Width,
              Cap    =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get
             (Widget        => Widget,
              Property_Name => "needle-color-2",
              Default       => Needle_Color_2));
      Widget.all.Needle_100ths.all.Set
        (Center => Widget.all.Needle_100ths.all.Get_Center,
         From   => Widget.all.Needle_100ths.all.Get_From,
         Length => Widget.all.Needle_100ths.all.Get_Length,
         Tip    =>
           Gtk.Layered.End_Parameters'
             (Length => Widget.all.Needle_100ths.all.Get_Tip.Length,
              Width  => Widget.all.Needle_100ths.all.Get_Tip.Width,
              Cap    =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "needle-tip-cap")),
         Rear   =>
           Gtk.Layered.End_Parameters'
             (Length => Widget.all.Needle_100ths.all.Get_Rear.Length,
              Width  => Widget.all.Needle_100ths.all.Get_Rear.Width,
              Cap    =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "needle-rear-cap")),
         Color  =>
           Gtk.Widget.Styles.Style_Get (Widget        => Widget,
                                        Property_Name => "needle-color-3",
                                        Default       => Needle_Color_3));
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
           Gtk.Widget.Styles.Style_Get (Widget        => Widget,
                                        Property_Name => "pin-color",
                                        Default       => Pin_Color));
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
             (Widget        => Widget,
              Property_Name => "background-color",
              Default       => Background_Color));
      Widget.all.Minor_Ticks.all.Set
        (Inner  => Widget.all.Minor_Ticks.all.Get_Inner,
         Outer  => Widget.all.Minor_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Minor_Ticks.all.Get_Ticks,
         From   => Widget.all.Minor_Ticks.all.Get_From,
         Length => Widget.all.Minor_Ticks.all.Get_Length,
         Line   =>
           Gtk.Layered.Line_Parameters'
             (Width => Widget.all.Minor_Ticks.all.Get_Line.Width,
              Color =>
                Gtk.Widget.Styles.Style_Get
                  (Widget        => Widget,
                   Property_Name => "minor-tick-color",
                   Default       => Minor_Tick_Color),
              Line_Cap =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "minor-tick-line-cap")));
      Widget.all.Middle_Ticks.all.Set
        (Inner  => Widget.all.Middle_Ticks.all.Get_Inner,
         Outer  => Widget.all.Middle_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Middle_Ticks.all.Get_Ticks,
         From   => Widget.all.Middle_Ticks.all.Get_From,
         Length => Widget.all.Middle_Ticks.all.Get_Length,
         Line   =>
           Gtk.Layered.Line_Parameters'
             (Width    => Widget.all.Middle_Ticks.all.Get_Line.Width,
              Color    =>
                Gtk.Widget.Styles.Style_Get
                  (Widget        => Widget,
                   Property_Name => "middle-tick-color",
                   Default       => Middle_Tick_Color),
              Line_Cap =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "middle-tick-line-cap")));
      Widget.all.Major_Ticks.all.Set
        (Inner  => Widget.all.Major_Ticks.all.Get_Inner,
         Outer  => Widget.all.Major_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Major_Ticks.all.Get_Ticks,
         From   => Widget.all.Major_Ticks.all.Get_From,
         Length => Widget.all.Major_Ticks.all.Get_Length,
         Line =>
           Gtk.Layered.Line_Parameters'
             (Width    => Widget.all.Major_Ticks.all.Get_Line.Width,
              Color    =>
                Gtk.Widget.Styles.Style_Get
                  (Widget        => Widget,
                   Property_Name => "major-tick-color",
                   Default       => Major_Tick_Color),
              Line_Cap =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "major-tick-line-cap")));
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
           Gtk.Widget.Styles.Style_Get (Widget        => Widget,
                                        Property_Name => "text-color",
                                        Default       => Text_Color));
   end Style_Changed;

   pragma Warnings (On, "declaration hides ""Widget""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Gauge.Altimeter;
