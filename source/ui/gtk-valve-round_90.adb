--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Round_90                          Luebeck            --
--  Implementation                                 Winter, 2017       --
--                                                                    --
--                                Last revision :  19:07 02 Jan 2018  --
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
with Cairo.Ellipses;
with Cairo.Line_Cap_Property;
with Gdk.Color;
with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;
--  with Gtkada.Types;
with Gtk.Enums;
with Gtk.Missed;
with Gtk.Widget.Styles;
with Gtk.Widget.Styles.Line_Cap_Property;
with Pango.Cairo.Fonts;

package body Gtk.Valve.Round_90 is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Pi : constant := Ada.Numerics.Pi;

   R1 : constant Gdouble := 0.36;
   R2 : constant Gdouble := 0.39;
   R3 : constant Gdouble := 0.40;
   R4 : constant Gdouble := 0.43;

   --     Reflection_X1       : constant := -0.2;
   --     Reflection_X2       : constant := -0.2;
   --     Reflection_Y1       : constant := -0.2;
   --     Reflection_Y2       : constant := -0.2;
   --     Reflection_Width    : constant := 0.25;
   --     Reflection_Opacity  : constant := 0.5;
   --     Reflection_Color    : constant Gdk_Color := RGB (1.0, 1.0, 1.0);

   Cover_Color       : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red   => 0.1,
                                         Green => 0.1,
                                         Blue  => 0.1);
   Pin_Color         : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red   => 0.2,
                                         Green => 0.2,
                                         Blue  => 0.2);
   Background_Color  : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red   => 1.0,
                                         Green => 1.0,
                                         Blue  => 1.0);
   Major_Tick_Color  : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red   => 0.0,
                                         Green => 0.0,
                                         Blue  => 0.0);
   --     Middle_Tick_Color : constant Gdk.Color.Gdk_Color :=
   --       Gtk.Missed.RGB (0.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red   => 0.0,
                                         Green => 0.0,
                                         Blue  => 0.0);
   Text_Color        : constant Gdk.Color.Gdk_Color :=
                         Gtk.Missed.RGB (Red   => 0.0,
                                         Green => 0.0,
                                         Blue  => 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Length : constant Gdouble := Pi / 2.0;
   First  : constant Gdouble := 8.0 * Pi / 4.0;

   --
   procedure Create_Background
     (Widget  : not null access Gtk_Valve_Round_90_Record'Class;
      Sectors : in              Positive);

   procedure Create_Foreground
     (Widget  : not null access Gtk_Valve_Round_90_Record'Class;
      Sectors : in              Positive);

   procedure Create_Needle
     (Widget     : not null access Gtk_Valve_Round_90_Record'Class;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment);
   --

   procedure Create_Background
     (Widget  : not null access Gtk_Valve_Round_90_Record'Class;
      Sectors : in              Positive) is
   begin
      G_New (Object => Widget,
             Typ    => Get_Type);
      Gtk.Layered.Initialize (Widget);
      Widget.all.Sectors := Sectors;
      Widget.all.Background :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget,
           Color         => Background_Color,
           Outer         =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / 0.5,
                Minor_Radius    => 0.5,
                Angle           => 0.0),
           Border_Width  => 0.01,
           Border_Depth  => 0.005,
           Border_Shadow => Gtk.Enums.Shadow_Etched_Out,
           Deepened      => True,
           Widened       => True,
           Scaled        => True);
   end Create_Background;

   procedure Create_Foreground
     (Widget  : not null access Gtk_Valve_Round_90_Record'Class;
      Sectors : in              Positive)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      Widget.all.Upper.Minor_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R1,
                Minor_Radius    => R1,
                Angle           => First),
           Outer   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R2,
                Minor_Radius    => R2,
                Angle           => First),
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 300.0,
           Skipped => 5,
           Step    => 0.2 * Length / Gdouble (Widget.all.Sectors),
           First   => 5,
           From    => First,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Upper.Major_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R1,
                Minor_Radius    => R1,
                Angle           => First),
           Outer   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R3,
                Minor_Radius    => R3,
                Angle           => First),
           Color   => Major_Tick_Color,
           Width   => 2.0 / 300.0,
           Skipped => Sectors,
           Step    => Length / Gdouble (Widget.all.Sectors),
           First   => Sectors,
           From    => First,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Lower.Minor_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R1,
                Minor_Radius    => R1,
                Angle           => First),
           Outer   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R2,
                Minor_Radius    => R2,
                Angle           => First),
           Color   => Minor_Tick_Color,
           Width   => 1.0 / 300.0,
           Skipped => 5,
           Step    => 0.2 * Length / Gdouble (Widget.all.Sectors),
           First   => 5,
           From    => First - Pi,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Lower.Major_Ticks :=
        Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
          (Under   => Widget.all.Background.all.Get_Foreground,
           Inner   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R1,
                Minor_Radius    => R1,
                Angle           => First),
           Outer   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R3,
                Minor_Radius    => R3,
                Angle           => First),
           Color   => Major_Tick_Color,
           Width   => 2.0 / 300.0,
           Skipped => Sectors,
           Step    => Length / Gdouble (Widget.all.Sectors),
           First   => Sectors,
           From    => First - Pi,
           Length  => Length,
           Scaled  => True,
           Widened => True);
      Widget.all.Left_Cover :=
        Gtk.Layered.Sector_Needle.Add_Sector_Needle
          (Under  => Widget.all.Background.all.Get_Foreground,
           From   => First - Length,
           Outer  => Cairo.Ellipses.Unit_Circle / 2.0,
           Center => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                 Y => 0.0),
           Length => Length,
           Color  => Cover_Color,
           Scaled => True);
      Widget.all.Left_Cover.all.Set_Value (1.0);
      Widget.all.Right_Cover :=
        Gtk.Layered.Sector_Needle.Add_Sector_Needle
          (Under  => Widget.all.Background.all.Get_Foreground,
           From   => First + Length,
           Outer  => Cairo.Ellipses.Unit_Circle / 2.0,
           Center => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                 Y => 0.0),
           Length => Length,
           Color  => Cover_Color,
           Scaled => True);
      Widget.all.Right_Cover.all.Set_Value (1.0);
      Widget.all.Pin :=
        Gtk.Layered.Elliptic_Background.Add_Elliptic_Background
          (Under         => Widget.all.Background.all.Get_Foreground,
           Color         => Pin_Color,
           Outer         =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / 0.09,
                Minor_Radius    => 0.09,
                Angle           => 0.0),
           Border_Shadow => Gtk.Enums.Shadow_Out,
           Scaled        => True);
   end Create_Foreground;

   procedure Create_Needle
     (Widget     : not null access Gtk_Valve_Round_90_Record'Class;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget.all.Needle :=
        Gtk.Layered.Disk_Needle.Add_Disk_Needle
          (Under       => Widget.all.Background.all.Get_Foreground,
           Center      => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                      Y => 0.0),
           From        => First,
           Radius      => 0.5,
           Length      => Length,
           Sectors     => True,
           On_Color    => Gtk.Layered.Disk_Needle.Needle_On_Color,
           Off_Color   => Gtk.Layered.Disk_Needle.Needle_Off_Color,
           Adjustment  => Adjustment,
           Scaled      => True);
   end Create_Needle;

   function Get_Annotation
     (Widget : not null access Gtk_Valve_Round_90_Record)
      return not null access Gtk.Layered.Elliptic_Annotation.Elliptic_Annotation_Layer is
   begin
      return Widget.all.Upper.Annotation;
   end Get_Annotation;

   function Get_Background
     (Widget : not null access Gtk_Valve_Round_90_Record)
      return not null access Gtk.Layered.Elliptic_Background.Elliptic_Background_Layer is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Needle
     (Widget : not null access Gtk_Valve_Round_90_Record)
      return not null access Gtk.Layered.Disk_Needle.Disk_Needle_Layer is
   begin
      return Widget.all.Needle;
   end Get_Needle;

   function Get_Type return GType is
   begin
      if
        Initialize_Class_Record
          (Ancestor     => Gtk.Layered.Get_Type,
           Class_Record => Class_Record'Access,
           Type_Name    => Class_Name)
      then
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "needle-on-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Needle on color",
                 Blurb      => "The color of the needle's half " &
                   "corresponding to the on state"));
         Gtk.Widget.Install_Style_Property
           (Self  => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Pspec =>
              Glib.Properties.Creation.Gnew_Boxed
                (Name       => "needle-off-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Needle off color",
                 Blurb      => "The color of the needle's half " &
                   "corresponding to the off state"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Class     => Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Enum_Spec =>
              Cairo.Line_Cap_Property.Gnew_Enum
                (Name    => "needle-tip-cap",
                 Nick    => "Tip cap",
                 Blurb   => "The style used for the needle tip",
                 Default => Cairo.Cairo_Line_Cap_Round));
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
                (Name       => "minor-tick-color",
                 Boxed_Type => Gdk.Color.Gdk_Color_Type,
                 Nick       => "Minor ticks color",
                 Blurb      => "Minor ticks color"));
         Gtk.Widget.Styles.Line_Cap_Property.Install_Style
           (Class     => Glib.Types.Class_Ref (Class_Record.all.The_Type),
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
            Pspec => Glib.Properties.Creation.Gnew_Boxed
              (Name       => "text-color",
               Boxed_Type => Gdk.Color.Gdk_Color_Type,
               Nick       => "Text color",
               Blurb      => "Text color"));
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New
     (Widget     : out Gtk_Valve_Round_90;
      Texts      : in  Gtk.Enums.String_List.Glist;
      Adjustment : in  Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : in  Positive                      := 5) is
   begin
      Widget := new Gtk_Valve_Round_90_Record;
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
     (Widget     : out Gtk_Valve_Round_90;
      Texts      : in  Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : in  Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : in  Positive                      := 5) is
   begin
      Widget := new Gtk_Valve_Round_90_Record;
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
     (Widget     : out Gtk_Valve_Round_90;
      Texts      : in  UTF8_String;
      Delimiter  : in  Character                     := ' ';
      Adjustment : in  Gtk.Adjustment.Gtk_Adjustment := null;
      Sectors    : in  Positive                      := 5) is
   begin
      Widget := new Gtk_Valve_Round_90_Record;
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
     (Widget     : not null access Gtk_Valve_Round_90_Record'Class;
      Texts      : in              Gtk.Enums.String_List.Glist;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : in              Positive) is
   begin
      Create_Background (Widget  => Widget,
                         Sectors => Sectors);
      Create_Needle (Widget     => Widget,
                     Adjustment => Adjustment);
      Create_Foreground (Widget  => Widget,
                         Sectors => Sectors);
      Widget.all.Upper.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under   => Widget,
           Ellipse =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R4,
                Minor_Radius    => R4,
                Angle           => First),
           Texts   => Texts,
           Face    =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step    => Length / Gdouble (Sectors),
           Height  => 0.05,
           Color   => Text_Color,
           From    => First,
           Length  => Length,
           Mode    => Gtk.Layered.Rotated,
           Scaled  => True);
      Widget.all.Lower.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under   => Widget,
           Ellipse =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R4,
                Minor_Radius    => R4,
                Angle           => First + Pi),
           Texts   => Texts,
           Face    =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step    => Length / Gdouble (Sectors),
           Height  => 0.05,
           Color   => Text_Color,
           From    => First + Pi,
           Length  => Length,
           Mode    => Gtk.Layered.Rotated,
           Scaled  => True);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Valve_Round_90_Record'Class;
      Texts      : in              Gtk.Enums.String_Lists.Controlled_String_List;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : in              Positive)  is
   begin
      Initialize (Widget     => Widget,
                  Texts      => Gtk.Enums.String_Lists.Get_GList (Texts),
                  Adjustment => Adjustment,
                  Sectors    => Sectors);
   end Initialize;

   procedure Initialize
     (Widget     : not null access Gtk_Valve_Round_90_Record'Class;
      Texts      : in              UTF8_String;
      Delimiter  : in              Character;
      Adjustment : in              Gtk.Adjustment.Gtk_Adjustment;
      Sectors    : in              Positive) is
   begin
      Create_Background (Widget  => Widget,
                         Sectors => Sectors);
      Create_Needle (Widget     => Widget,
                     Adjustment => Adjustment);
      Create_Foreground (Widget  => Widget,
                         Sectors => Sectors);
      Widget.all.Upper.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget,
           Ellipse   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R4,
                Minor_Radius    => R4,
                Angle           => First),
           Texts     => Texts,
           Delimiter => Delimiter,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Length / Gdouble (Sectors),
           Height    => 0.05,
           Color     => Text_Color,
           From      => First,
           Length    => Length,
           Mode      => Gtk.Layered.Rotated,
           Scaled    => True);
      Widget.all.Lower.Annotation :=
        Gtk.Layered.Elliptic_Annotation.Add_Elliptic_Annotation
          (Under     => Widget,
           Ellipse   =>
             Cairo.Ellipses.Ellipse_Parameters'
               (Center          => Cairo.Ellipses.Cairo_Tuple'(X => 0.0,
                                                               Y => 0.0),
                Major_Curvature => 1.0 / R4,
                Minor_Radius    => R4,
                Angle           => First + Pi),
           Texts     => Texts,
           Delimiter => Delimiter,
           Face      =>
             Pango.Cairo.Fonts.Create_Toy
               (Family => "arial",
                Slant  => Cairo.Cairo_Font_Slant_Normal,
                Weight => Cairo.Cairo_Font_Weight_Bold),
           Step      => Length / Gdouble (Sectors),
           Height    => 0.05,
           Color     => Text_Color,
           From      => First + Pi,
           Length    => Length,
           Mode      => Gtk.Layered.Rotated,
           Scaled    => True);
   end Initialize;

   procedure Set_Value
     (Widget : not null access Gtk_Valve_Round_90_Record;
      Value  : in              Gdouble)  is
   begin
      Widget.all.Needle.all.Set_Value (Value);
   end Set_Value;

   overriding procedure Style_Changed
     (Widget : not null access Gtk_Valve_Round_90_Record) is
   begin
      Widget.all.Needle.all.Set
        (Center    => Widget.all.Needle.all.Get_Center,
         From      => Widget.all.Needle.all.Get_From,
         Length    => Widget.all.Needle.all.Get_Length,
         Radius    => Widget.all.Needle.all.Get_Radius,
         Sectors   => Widget.all.Needle.all.Get_Sectors,
         On_Color  =>
           Gtk.Widget.Styles.Style_Get
             (Widget        => Widget,
              Property_Name => "needle-on_color",
              Default       => Gtk.Layered.Disk_Needle.Needle_On_Color),
         Off_Color =>
           Gtk.Widget.Styles.Style_Get
             (Widget        => Widget,
              Property_Name => "needle-off_color",
              Default       => Gtk.Layered.Disk_Needle.Needle_Off_Color));
      Widget.all.Pin.all.Set
        (Outer         => Widget.all.Pin.all.Get_Outer,
         Inner         => Widget.all.Pin.all.Get_Inner,
         From          => Widget.all.Pin.all.Get_From,
         Length        => Widget.all.Pin.all.Get_Length,
         Border_Width  => Widget.all.Pin.all.Get_Border_Width,
         Border_Depth  => Widget.all.Pin.all.Get_Border_Depth,
         Border_Color  => Widget.all.Pin.all.Get_Border_Color,
         Border_Shadow => Widget.all.Pin.all.Get_Border_Shadow,
         --           Lens_Reflex   => Widget.Pin.Get_Lens_Reflex,
         --           Lens_Shadow   => Widget.Pin.Get_Lens_Shadow,
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
         --           Lens_Reflex   => Widget.Background.Get_Lens_Reflex,
         --           Lens_Shadow   => Widget.Background.Get_Lens_Shadow,
         Color         =>
           Gtk.Widget.Styles.Style_Get (Widget        => Widget,
                                        Property_Name => "background-color",
                                        Default       => Background_Color));
      Widget.all.Upper.Minor_Ticks.all.Set
        (Inner  => Widget.all.Upper.Minor_Ticks.all.Get_Inner,
         Outer  => Widget.all.Upper.Minor_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Upper.Minor_Ticks.all.Get_Ticks,
         From   => Widget.all.Upper.Minor_Ticks.all.Get_From,
         Length => Widget.all.Upper.Minor_Ticks.all.Get_Length,
         Line   =>
           Gtk.Layered.Line_Parameters'
             (Width    => Widget.all.Upper.Minor_Ticks.all.Get_Line.Width,
              Color    =>
                Gtk.Widget.Styles.Style_Get
                  (Widget        => Widget,
                   Property_Name => "minor-tick-color",
                   Default       => Minor_Tick_Color),
              Line_Cap =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "minor-tick-line-cap")));
      Widget.all.Upper.Major_Ticks.all.Set
        (Inner  => Widget.all.Upper.Major_Ticks.all.Get_Inner,
         Outer  => Widget.all.Upper.Major_Ticks.all.Get_Outer,
         Ticks  => Widget.all.Upper.Major_Ticks.all.Get_Ticks,
         From   => Widget.all.Upper.Major_Ticks.all.Get_From,
         Length => Widget.all.Upper.Major_Ticks.all.Get_Length,
         Line   =>
           Gtk.Layered.Line_Parameters'
             (Width    => Widget.all.Upper.Major_Ticks.all.Get_Line.Width,
              Color    =>
                Gtk.Widget.Styles.Style_Get
                  (Widget        => Widget,
                   Property_Name => "major-tick-color",
                   Default       => Major_Tick_Color),
              Line_Cap =>
                Gtk.Widget.Styles.Line_Cap_Property.Style_Get
                  (Widget        => Widget,
                   Property_Name => "major-tick-line-cap")));
      Widget.all.Upper.Annotation.all.Set
        (Ellipse => Widget.all.Upper.Annotation.all.Get_Ellipse,
         Ticks   => Widget.all.Upper.Annotation.all.Get_Ticks,
         From    => Widget.all.Upper.Annotation.all.Get_From,
         Length  => Widget.all.Upper.Annotation.all.Get_Length,
         Face    => Widget.all.Upper.Annotation.all.Get_Face,
         Mode    => Widget.all.Upper.Annotation.all.Get_Mode,
         Height  => Widget.all.Upper.Annotation.all.Get_Height,
         Stretch => Widget.all.Upper.Annotation.all.Get_Stretch,
         Color   =>
           Gtk.Widget.Styles.Style_Get (Widget        => Widget,
                                        Property_Name => "text-color",
                                        Default       => Text_Color));
      Widget.all.Lower.Annotation.all.Set
        (Ellipse => Widget.all.Lower.Annotation.all.Get_Ellipse,
         Ticks   => Widget.all.Lower.Annotation.all.Get_Ticks,
         From    => Widget.all.Lower.Annotation.all.Get_From,
         Length  => Widget.all.Lower.Annotation.all.Get_Length,
         Face    => Widget.all.Lower.Annotation.all.Get_Face,
         Mode    => Widget.all.Lower.Annotation.all.Get_Mode,
         Height  => Widget.all.Lower.Annotation.all.Get_Height,
         Stretch => Widget.all.Lower.Annotation.all.Get_Stretch,
         Color   =>
           Gtk.Widget.Styles.Style_Get (Widget        => Widget,
                                        Property_Name => "text-color",
                                        Default       => Text_Color));
   end Style_Changed;

   pragma Warnings (On, "declaration hides ""Widget""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Valve.Round_90;
