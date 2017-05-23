--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Meter.Thermo_Dual                       Luebeck            --
--  Implementation                                 Summer, 2012       --
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
with Ada.Strings;               use Ada.Strings;

with Cairo;                     use Cairo;
with Cairo.Line_Cap_Property;   use Cairo.Line_Cap_Property;

with Gdk.Color.IHLS;            use Gdk.Color.IHLS;

with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Types;                use Glib.Types;

with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Line_Cap_Property; use  Gtk.Widget.Styles.Line_Cap_Property;

with Pango.Cairo.Fonts;         use Pango.Cairo.Fonts;

with Strings_Edit;

package body Gtk.Meter.Thermo_Dual is

   type Bar_Mode is (Celsius_Mode, Fahrenheit_Mode);

   Background_Color  : constant Gdk_Color := RGB (1.0, 1.0, 1.0);
   Major_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Middle_Tick_Color : constant Gdk_Color := RGB (1.0, 0.0, 0.0);
   Minor_Tick_Color  : constant Gdk_Color := RGB (0.0, 0.0, 0.0);
   Text_Color        : constant Gdk_Color := RGB (0.0, 0.0, 0.0);

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

--     Celsius      : constant String := Character'Val (16#C2#) &
--                                       Character'Val (16#B0#) & 'C';
--     Fahrenheit   : constant String := Character'Val (16#C2#) &
--                                       Character'Val (16#B0#) & 'F';

   Corner                  : constant := 1.0 / 10.0;
   Length                  : constant := 2.5;
   First                   : constant := Length * 0.5;
   Bar_Width               : constant := 0.09;
   Reflection_Offset       : constant := Bar_Width * 0.35;
   Bar_Offset              : constant := 0.00;
   Left_Annotation_Offset  : constant :=-0.19;
   Left_Tick_Offset        : constant :=-0.07;
   Left_Label_Offset       : constant :=-0.30;
   Right_Annotation_Offset : constant := 0.23;
   Right_Tick_Offset       : constant := 0.07;
   Right_Label_Offset      : constant := 0.30;
   Stem_Length             : constant := 0.15;
   Label_Height            : constant :=-1.45;

   Reflection_Shift : constant Gdk_Luminance := Gdk_Luminance'Last / 2;

   Color_Span : constant Gdk_Luminance := Gdk_Luminance'Last / 2;

   function Celsius (T : Gdouble) return Gdouble is
      F : constant := 5.0 / 9.0;
   begin
      return (T - 32.0) * F;
   exception
      when Constraint_Error =>
         if T < 0.0 then
            return -273.16;
         else
            return Gdouble'Last;
         end if;
   end Celsius;

   function Fahrenheit (T : Gdouble) return Gdouble is
      F : constant := 9.0 / 5.0;
   begin
      return T * F + 32.0;
   exception
      when Constraint_Error =>
         if T < 0.0 then
            return -459.67;
         else
            return Gdouble'Last;
         end if;
   end Fahrenheit;

   function Get_Background
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Rectangular_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Bar
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Bar_Layer is
   begin
      return Widget.Bar;
   end Get_Bar;

   function Get_Bar_Color
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return Gdk_Color is
   begin
      return Widget.Bar.Get_Line.Color;
   end Get_Bar_Color;

   function Get_Cache
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Celsius_Label
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Label_Layer is
   begin
      return Widget.Celsius.Label;
   end Get_Celsius_Label;

   function Get_Celsius_Annotation
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Flat_Annotation_Layer is
   begin
      return Widget.Celsius.Annotation;
   end Get_Celsius_Annotation;

   function Get_Celsius_Value
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return Gdouble is
   begin
      return
      (  Widget.Bar.Get_Value * Widget.Celsius.Span
      +  Widget.Celsius.From
      );
   end Get_Celsius_Value;

   function Get_Fahrenheit_Annotation
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Flat_Annotation_Layer is
   begin
      return Widget.Fahrenheit.Annotation;
   end Get_Fahrenheit_Annotation;

   function Get_Fahrenheit_Label
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return not null access Label_Layer is
   begin
      return Widget.Fahrenheit.Label;
   end Get_Fahrenheit_Label;

   function Get_Fahrenheit_Value
            (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
            )  return Gdouble is
   begin
      return
      (  Widget.Bar.Get_Value * Widget.Fahrenheit.Span
      +  Widget.Fahrenheit.From
      );
   end Get_Fahrenheit_Value;

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
               Blurb      =>
                  "The color of the circle bounding the ticks"
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
            (  Name       => "text-color",
               Boxed_Type => Gdk_Color_Type,
               Nick       => "Text color",
               Blurb      => "Text color"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New_Celsius
             (  Widget     : out Gtk_Meter_Thermo_Dual;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive  := 8;
                Color      : Gdk_Color := RGB (1.0, 0.0, 0.0)
             )  is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Celsius (Widget, Adjustment, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Celsius;

   procedure Gtk_New_Celsius
             (  Widget  : out Gtk_Meter_Thermo_Dual;
                Lower   : Gdouble    := -40.0;
                Upper   : Gdouble    := 50.0;
                Sectors : Positive  := 8;
                Color   : Gdk_Color := RGB (1.0, 0.0, 0.0)
             )  is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Celsius (Widget, Lower, Upper, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Celsius;

   procedure Gtk_New_Fahrenheit
             (  Widget     : out Gtk_Meter_Thermo_Dual;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive  := 8;
                Color      : Gdk_Color := RGB (1.0, 0.0, 0.0)
             )  is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Fahrenheit (Widget, Adjustment, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Fahrenheit;

   procedure Gtk_New_Fahrenheit
             (  Widget  : out Gtk_Meter_Thermo_Dual;
                Lower   : Gdouble    := 20.0;
                Upper   : Gdouble    := 220.0;
                Sectors : Positive  := 8;
                Color   : Gdk_Color := RGB (1.0, 0.0, 0.0)
             )  is
   begin
      Widget := new Gtk_Meter_Thermo_Dual_Record;
      Initialize_Fahrenheit (Widget, Lower, Upper, Sectors, Color);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New_Fahrenheit;

   procedure Initialize
             (  Widget     : not null access
                             Gtk_Meter_Thermo_Dual_Record'Class;
                Mode       : Bar_Mode;
                Adjustment : Gtk_Adjustment;
                Lower      : Gdouble;
                Upper      : Gdouble;
                Count      : Positive;
                Color      : Gdk_Color
             )  is
      procedure Create_Annotation
                (  Scale   : in out Temperature_Scale;
                   Justify : Alignment;
                   Offset  : Gdouble
                )  is
         use Strings_Edit;
         Texts   : UTF8_String (1..2048);
         Pointer : Integer := Texts'First;
      begin
         for Index in 0..Scale.Sectors + 3 loop
            Gtk.Layered.Waveform.Edit.Put
            (  Destination => Texts,
               Pointer     => Pointer,
               AbsSmall    => Scale.Small,
               Value       =>
                  (  Scale.Lower
                  +  Gdouble (Index)
                  *  Gdouble (Scale.Step / Scale.Factor)
            )     );
            Put (Texts, Pointer, " ");
         end loop;
         Scale.Annotation :=
           Add_Flat_Annotation
             (Under       => Widget.Cache,
              From        => (Offset, First - Scale.First),
              Length      => Length - Scale.First,
              Scale_Angle => 3.0 * Ada.Numerics.Pi / 2.0,
              Texts       => Texts (1 .. Pointer - 1),
              Justify     => Justify,
              Step        => Scale.Step,
              Height      => 0.1,
              Color       => Text_Color,
              Scaled      => True,
              Face        =>
                Create_Toy
                  (Family => "arial",
                   Slant  => Cairo_Font_Slant_Normal,
                   Weight => Cairo_Font_Weight_Normal));
      end Create_Annotation;

      procedure Create_Ticks
                (  Scale   : in out Temperature_Scale;
                   Lower   : Gdouble;
                   Upper   : Gdouble;
                   Offset  : Gdouble;
                   Sign    : Gdouble;
                   Sectors : Positive
                )  is
         Raster : constant Gtk.Layered.Waveform.Rasters.Scale :=
                           Gtk.Layered.Waveform.Rasters.Create
                           (  Lower,
                              Upper,
                              Sectors
                           );
      begin
         Scale.Sectors := Sectors;
         Scale.From := Lower;
         Scale.Span := Upper - Lower;
         Scale.Factor := Length / Scale.Span;
         Scale.Small  := Integer'Min (Raster.Small, 0);
         Scale.Step :=
            (  Gdouble (Raster.Minor)
            *  Scale.Factor
            *  Gdouble (Raster.Ticks + 1)
            );
         if Raster.Low_Tick = 0 then
            Scale.Lower := Raster.Low_Value;
         else
            Scale.Lower :=
               (  Raster.Low_Value
               +  Raster.Minor
               *  Gdouble (Raster.Ticks + 1 - Raster.Low_Tick)
               );
         end if;
         Scale.First := Scale.Factor * Gdouble (Scale.Lower - Lower);
         Scale.Major_Ticks :=
           Add_Flat_Scale
             (Under   => Widget.Background.Get_Foreground,
              From    => (Offset + Sign * 0.07, First - Scale.First),
              Length  => Length - Scale.First,
              Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
              Breadth => 0.13,
              Color   => Major_Tick_Color,
              Width   => 4.0 / 600.0,
              Step    => Scale.Step,
              Scaled  => True,
              Widened => True);
         declare
            Start : Gdouble := Scale.First;
            Tick  : Tick_Number;
         begin
            if Start >= 0.5 * Scale.Step then
               Start := Start - 0.5 * Scale.Step;
               Tick := 1;
            else
               Tick := 2;
            end if;
            Scale.Middle_Ticks :=
              Add_Flat_Scale
                (Under   => Widget.Background.Get_Foreground,
                 From    => (Offset + Sign * 0.05, First - Start),
                 Length  => Length - Start,
                 Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
                 Breadth => 0.10,
                 Color   => Middle_Tick_Color,
                 Width   => 2.0 / 600.0,
                 Step    => 0.5 * Scale.Step,
                 Skipped => 2,
                 First   => Tick,
                 Scaled  => True,
                 Widened => True);
         end;
         declare
            Start : Gdouble := Scale.First;
            Tick  : Tick_Number := 10;
         begin
            if Start > 0.1 * Scale.Step then
               while Start >= 0.1 * Scale.Step loop
                  Start := Start - 0.1 * Scale.Step;
                  Tick  := Tick - 1;
               end loop;
               if Tick > 5 then
                  Tick := Tick - 5;
               end if;
            end if;
            Scale.Minor_Ticks :=
              Add_Flat_Scale
                (Under   => Widget.Background.Get_Foreground,
                 From    => (Offset + Sign * 0.04, First - Start),
                 Length  => Length - Start,
                 Angle   => 3.0 * Ada.Numerics.Pi / 2.0,
                 Breadth => 0.07,
                 Color   => Minor_Tick_Color,
                 Width   => 1.0 / 600.0,
                 Step    => 0.1 * Scale.Step,
                 Skipped => 5,
                 First   => Tick,
                 Scaled  => True,
                 Widened => True);
         end;
      end Create_Ticks;
   begin
      if Lower >= Upper then
         raise Constraint_Error with
               "Lower temperature is greater than the upper one";
      end if;
      case Mode is
         when Celsius_Mode =>
            if Lower < -273.16 then
               raise Constraint_Error with
                     "Lower temperature is less than absolute zero";
            elsif Upper > 10.0E8 then
               raise Constraint_Error with
                     "Upper temperature is too high";
            end if;
         when Fahrenheit_Mode =>
            if Lower < -459.67 then
               raise Constraint_Error with
                     "Lower temperature is less than absolute zero";
            elsif Upper > 10.0E8 then
               raise Constraint_Error with
                     "Upper temperature is too high";
            end if;
      end case;
      --
      -- Create background
      --
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Set_Aspect_Ratio (Widget, 1.0 / 3.0);
      Widget.Background :=
         Add_Rectangular_Background
         (  Under         => Widget,
            Height        => 3.0,
            Width         => 1.0,
            Center        => (0.0, 0.0),
            Corner_Radius => Corner,
            Color         => Background_Color,
            Border_Width  => 0.03,
            Border_Depth  => 0.01,
            Border_Shadow => Shadow_Etched_Out,
            Deepened      => True,
            Widened       => True,
            Scaled        => True
         );
      case Mode is
         when Celsius_Mode =>
            Create_Ticks
            (  Widget.Celsius,
               Lower,
               Upper,
               Right_Tick_Offset,
               1.0,
               Count
            );
            Create_Ticks
            (  Widget.Fahrenheit,
               Fahrenheit (Lower),
               Fahrenheit (Upper),
               Left_Tick_Offset,
               -1.0,
               Integer'Max (1, (Count * 9) / 5)
            );
         when Fahrenheit_Mode =>
            Create_Ticks
            (  Widget.Celsius,
               Celsius (Lower),
               Celsius (Upper),
               Left_Tick_Offset,
               1.0,
               Integer'Max (1, (Count * 5) / 9)
            );
            Create_Ticks
            (  Widget.Fahrenheit,
               Lower,
               Upper,
               Right_Tick_Offset,
               -1.0,
               Count
            );
      end case;
      Widget.Stem :=
         Add_Line
         (  Under    => Widget.Background.Get_Foreground,
            From     => (Bar_Offset, First + Stem_Length),
            To       => (Bar_Offset, First),
            Width    => Bar_Width,
            Color    => Color,
            Line_Cap => Cairo_Line_Cap_Butt,
            Scaled   => True,
            Widened  => True
         );
       Widget.Bulb :=
          Add_Line
          (  Under    => Widget.Background.Get_Foreground,
             From     => (Bar_Offset, First + Stem_Length),
             To       => (Bar_Offset, First + Stem_Length),
             Width    => Bar_Width * 2.0,
             Color    => Color,
             Line_Cap => Cairo_Line_Cap_Round,
             Scaled   => True,
             Widened  => True
          );
       Widget.Reflection :=
          Add_Line
          (  Under    => Widget.Background.Get_Foreground,
             From     => (  Bar_Offset          - Reflection_Offset,
                            First + Stem_Length - Reflection_Offset
                         ),
             To       => (  Bar_Offset          - Reflection_Offset,
                            First + Stem_Length - Reflection_Offset
                         ),
             Width    => Reflection_Offset * 2.0,
             Line_Cap => Cairo_Line_Cap_Round,
             Scaled   => True,
             Widened  => True,
             Color    => Lighten
                         (  Widget.Bulb.Get_Line.Color,
                            Reflection_Shift,
                            True
          )              );
      Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
      --
      -- Create annotation
      --
      Create_Annotation
      (  Widget.Fahrenheit,
         Right,
         Left_Annotation_Offset
      );
      Widget.Fahrenheit.Label :=
         Add_Label
         (  Under    => Widget.Get_Cache,
            Face     => Create_Toy
                        (  Family => "arial",
                           Slant  => Cairo_Font_Slant_Normal,
                           Weight => Cairo_Font_Weight_Bold
                        ),
            Color    => Text_Color,
            Height   => 0.12,
            Mode     => Moved_Centered,
            Scaled   => True,
            Text     => Character'Val (16#C2#) &
                        Character'Val (16#B0#) & 'F',
            Location => (Left_Label_Offset, Label_Height)
         );
      Create_Annotation
      (  Widget.Celsius,
         Left,
         Right_Annotation_Offset
      );
      Widget.Celsius.Label :=
         Add_Label
         (  Under    => Widget.Get_Cache,
            Face     => Create_Toy
                        (  Family => "arial",
                           Slant  => Cairo_Font_Slant_Normal,
                           Weight => Cairo_Font_Weight_Bold
                        ),
            Color    => Text_Color,
            Height   => 0.12,
            Mode     => Moved_Centered,
            Scaled   => True,
            Text     => Character'Val (16#C2#) &
                        Character'Val (16#B0#) & 'C',
            Location => (Right_Label_Offset, Label_Height)
         );
      --
      -- Create foreground
      --
      Widget.Bar :=
        Add_Bar
          (Under      => Widget.Background.Get_Foreground,
           From       => (Bar_Offset, First),
           Length     => Length,
           Angle      => 3.0 * Ada.Numerics.Pi / 2.0,
           Adjustment => Adjustment,
           Line_Cap   => Cairo_Line_Cap_Butt,
           Width      => Bar_Width,
           Color      => Color,
           Scaled     => True,
           Widened    => True);
   end Initialize;

   procedure Initialize_Celsius
             (  Widget     : not null access
                             Gtk_Meter_Thermo_Dual_Record'Class;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive;
                Color      : Gdk_Color
             )  is
   begin
      Initialize
      (  Widget,
         Celsius_Mode,
         Adjustment.all'Unchecked_Access,
         Adjustment.Get_Lower,
         Adjustment.Get_Upper,
         Sectors,
         Color
      );
   end Initialize_Celsius;

   procedure Initialize_Celsius
             (  Widget  : not null access
                          Gtk_Meter_Thermo_Dual_Record'Class;
                Lower   : Gdouble;
                Upper   : Gdouble;
                Sectors : Positive;
                Color   : Gdk_Color
             )  is
   begin
      Initialize
      (  Widget,
         Celsius_Mode,
         null,
         Lower,
         Upper,
         Sectors,
         Color
      );
   end Initialize_Celsius;

   procedure Initialize_Fahrenheit
             (  Widget     : not null access
                             Gtk_Meter_Thermo_Dual_Record'Class;
                Adjustment : not null access
                             Gtk_Adjustment_Record'Class;
                Sectors    : Positive;
                Color      : Gdk_Color
             )  is
   begin
      Initialize
      (  Widget,
         Fahrenheit_Mode,
         Adjustment.all'Unchecked_Access,
         Adjustment.Get_Lower,
         Adjustment.Get_Upper,
         Sectors,
         Color
      );
   end Initialize_Fahrenheit;

   procedure Initialize_Fahrenheit
             (  Widget  : not null access
                          Gtk_Meter_Thermo_Dual_Record'Class;
                Lower   : Gdouble;
                Upper   : Gdouble;
                Sectors : Positive;
                Color   : Gdk_Color
             )  is
   begin
      Initialize
      (  Widget,
         Fahrenheit_Mode,
         null,
         Lower,
         Upper,
         Sectors,
         Color
      );
   end Initialize_Fahrenheit;

   procedure Set_Bar_Color
             (  Widget : not null access Gtk_Meter_Thermo_Dual_Record;
                Color  : Gdk_Color
             )  is
   begin
      Widget.Bar.Set
      (  From => Widget.Bar.Get_From,
         To   => Widget.Bar.Get_To,
         Line => (  Width    => Widget.Bar.Get_Line.Width,
                    Color    => Color,
                    Line_Cap => Widget.Bar.Get_Line.Line_Cap
      )          );
      Widget.Bulb.Set
      (  From => Widget.Bulb.Get_From,
         To   => Widget.Bulb.Get_To,
         Line => (  Width    => Widget.Bulb.Get_Line.Width,
                    Color    => Color,
                    Line_Cap => Widget.Bulb.Get_Line.Line_Cap
      )          );
      Widget.Reflection.Set
      (  From => Widget.Reflection.Get_From,
         To   => Widget.Reflection.Get_To,
         Line => (  Width    => Widget.Reflection.Get_Line.Width,
                    Line_Cap => Widget.Reflection.Get_Line.Line_Cap,
                    Color    => Lighten
                                (  Color,
                                   Reflection_Shift,
                                   True
      )          )              );
      Widget.Stem.Set
      (  From   => Widget.Stem.Get_From,
         To     => Widget.Stem.Get_To,
         Line   => (  Width    => Widget.Stem.Get_Line.Width,
                      Color    => Color,
                      Line_Cap => Widget.Stem.Get_Line.Line_Cap
      )            );
   end Set_Bar_Color;

   procedure Set_Celsius_Value
             (  Widget : not null access Gtk_Meter_Thermo_Dual_Record;
                Value  : Gdouble
             )  is
   begin
      Widget.Bar.Set_Value
      (  (Value - Widget.Celsius.From)
      /  Widget.Celsius.Span
      );
   end Set_Celsius_Value;

   procedure Set_Fahrenheit_Value
             (  Widget : not null access Gtk_Meter_Thermo_Dual_Record;
                Value  : Gdouble
             )  is
   begin
      Widget.Bar.Set_Value
      (  (Value - Widget.Fahrenheit.From)
      /  Widget.Fahrenheit.Span
      );
   end Set_Fahrenheit_Value;

   procedure Style_Changed
             (  Widget : not null access Gtk_Meter_Thermo_Dual_Record
             )  is
      procedure Set (Scale : in out Temperature_Scale) is
      begin
         Scale.Minor_Ticks.Set
         (  From    => Scale.Minor_Ticks.Get_From,
            Length  => Scale.Minor_Ticks.Get_Length,
            Breadth => Scale.Minor_Ticks.Get_Breadth,
            Angle   => Scale.Minor_Ticks.Get_Angle,
            Ticks   => Scale.Minor_Ticks.Get_Ticks,
            Line =>
               (  Scale.Minor_Ticks.Get_Line.Width,
                  Style_Get
                  (  Widget,
                     "minor-tick-color",
                     Minor_Tick_Color
                  ),
                  Style_Get (Widget, "minor-tick-line-cap")
         )     );
         Scale.Middle_Ticks.Set
         (  From    => Scale.Middle_Ticks.Get_From,
            Length  => Scale.Middle_Ticks.Get_Length,
            Breadth => Scale.Middle_Ticks.Get_Breadth,
            Angle   => Scale.Middle_Ticks.Get_Angle,
            Ticks   => Scale.Middle_Ticks.Get_Ticks,
            Line =>
               (  Scale.Middle_Ticks.Get_Line.Width,
                  Style_Get
                  (  Widget,
                     "middle-tick-color",
                     Middle_Tick_Color
                  ),
                  Style_Get (Widget, "middle-tick-line-cap")
         )     );
         Scale.Major_Ticks.Set
         (  From    => Scale.Major_Ticks.Get_From,
            Length  => Scale.Major_Ticks.Get_Length,
            Breadth => Scale.Major_Ticks.Get_Breadth,
            Angle   => Scale.Major_Ticks.Get_Angle,
            Ticks   => Scale.Major_Ticks.Get_Ticks,
            Line =>
               (  Scale.Major_Ticks.Get_Line.Width,
                  Style_Get
                  (  Widget,
                     "major-tick-color",
                     Major_Tick_Color
                  ),
                  Style_Get (Widget, "major-tick-line-cap")
         )     );
         Scale.Annotation.Set
         (  Ticks       => Scale.Annotation.Get_Ticks,
            From        => Scale.Annotation.Get_From,
            Length      => Scale.Annotation.Get_Length,
            Face        => Scale.Annotation.Get_Face,
            Scale_Angle => Scale.Annotation.Get_Scale_Angle,
            Height      => Scale.Annotation.Get_Height,
            Stretch     => Scale.Annotation.Get_Stretch,
            Text_Angle  => Scale.Annotation.Get_Text_Angle,
            Justify     => Scale.Annotation.Get_Justify,
            Color       => Style_Get (Widget, "text-color", Text_Color)
         );
         Scale.Label.Set
         (  Location => Scale.Label.Get_Location,
            Face     => Scale.Label.Get_Face,
            Height   => Scale.Label.Get_Height,
            Stretch  => Scale.Label.Get_Stretch,
            Mode     => Scale.Label.Get_Mode,
            Angle    => Scale.Label.Get_Angle,
            Skew     => Scale.Label.Get_Skew,
            Color    => Style_Get (Widget, "text-color", Text_Color)
         );
      end Set;
   begin
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
         Color =>
            Style_Get (Widget, "backgound-color", Background_Color)
      );
      Set (Widget.Celsius);
      Set (Widget.Fahrenheit);
   end Style_Changed;

end Gtk.Meter.Thermo_Dual;
