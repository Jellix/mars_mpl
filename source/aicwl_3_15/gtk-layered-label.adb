--                                                                    --
--  package Gtk.Layered.Label       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2010       --
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

with Ada.Unchecked_Deallocation;

with Cairo.Elementary_Functions;
with Cairo.Font_Slant_Property;

with Glib.Properties.Creation;

with Gtk.Layered.Stream_IO;
with Gtk.Layered.Text_Transformation_Property;

with Pango.Cairo.Fonts.Font_Type_Property;
with Pango.Enums.Weight_Property;

package body Gtk.Layered.Label is

   Eps : constant := 1.0E-6;

   type Layer_Property is
     (Property_Scaled,
      Property_X,
      Property_Y,
      Property_Text,
      Property_Markup,
      Property_Font_Type,
      Property_Family,
      Property_Slant,
      Property_Font_Size,
      Property_Weight,
      Property_Height,
      Property_Stretch,
      Property_Angle,
      Property_Skew,
      Property_Mode,
      Property_Color);

   type Label_Ptr is access all Label_Layer;

   procedure Free is
      new Ada.Unchecked_Deallocation (Label_Layer, Label_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   protected Text_Mutex is
      function Get_Text (Layer : Label_Layer) return UTF8_String;
      procedure Set_Text
        (Layer  : in out Label_Layer;
         Text   : UTF8_String;
         Markup : Boolean);
   end Text_Mutex;

   protected body Text_Mutex is
      function Get_Text (Layer : Label_Layer) return UTF8_String is
      begin
         return Layer.Text.all;
      end Get_Text;

      procedure Set_Text
        (Layer  : in out Label_Layer;
         Text   : UTF8_String;
         Markup : Boolean)
      is
         Ptr : constant String_Ptr := new UTF8_String'(Text);
      begin
         Free (Layer.Text);
         Layer.Text    := Ptr;
         Layer.Markup  := Markup;
         Layer.Updated := True;
      end Set_Text;

   end Text_Mutex;

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Label_Layer
   is
      Ptr : Label_Ptr := new Label_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Label
     (Under    : not null access Layer_Location'Class;
      Text     : UTF8_String                        := "";
      Location : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Face     : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "sans",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height   : Gdouble                            := 12.0;
      Stretch  : Gdouble                            := 1.0;
      Mode     : Text_Transformation                := Moved_Centered;
      Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle    : Gdouble                            := 3.0 * Ada.Numerics.Pi / 2.0;
      Skew     : Gdouble                            := 0.0;
      Markup   : Boolean                            := False;
      Scaled   : Boolean                            := False)
   is
      Ptr   : Label_Ptr := new Label_Layer;
      Layer : Label_Layer renames Ptr.all;
   begin
      Layer.Text   := new UTF8_String'(Text);
      Layer.Markup := Markup;
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
        (Layer    => Layer,
         Location => Location,
         Face     => Face,
         Height   => Height,
         Stretch  => Stretch,
         Mode     => Mode,
         Color    => Color,
         Angle    => Angle,
         Skew     => Skew);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Label;

   function Add_Label
     (Under    : not null access Layer_Location'Class;
      Text     : UTF8_String                        := "";
      Location : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Face     : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "sans",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height   : Gdouble                            := 12.0;
      Stretch  : Gdouble                            := 1.0;
      Mode     : Text_Transformation                := Moved_Centered;
      Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle    : Gdouble                            := 3.0 * Ada.Numerics.Pi / 2.0;
      Skew     : Gdouble                            := 0.0;
      Markup   : Boolean                            := False;
      Scaled   : Boolean                            := False)
      return not null access Label_Layer
   is
      Ptr   : Label_Ptr := new Label_Layer;
      Layer : Label_Layer renames Ptr.all;
   begin
      Layer.Text   := new UTF8_String'(Text);
      Layer.Markup := Markup;
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
        (Layer    => Layer,
         Location => Location,
         Face     => Face,
         Height   => Height,
         Stretch  => Stretch,
         Mode     => Mode,
         Color    => Color,
         Angle    => Angle,
         Skew     => Skew);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Label;

   overriding procedure Draw
     (Layer   : in out Label_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      Size    : constant Gdouble             := Layer.Widget.all.Get_Size;
      Text    : constant UTF8_String         := Text_Mutex.Get_Text (Layer);
      Gain    : Gdouble                      := 1.0;
      State   : Cairo.Ellipses.Context_State := Cairo.Ellipses.Save (Context);
      pragma Unreferenced (State);
      X, Y    : aliased Gdouble;
      Extents : Cairo.Cairo_Text_Extents;
   begin
      if Layer.Scaled then
         declare
            Center : constant Cairo.Ellipses.Cairo_Tuple :=
                       Layer.Widget.all.Get_Center;
         begin
            X := Center.X + Layer.X * Size;
            Y := Center.Y + Layer.Y * Size;
         end;
      else
         X := Layer.X;
         Y := Layer.Y;
      end if;
      Cairo.Move_To (Context, X, Y);
      if Layer.Markup then
         Pango.Cairo.Fonts.Get_Markup_Extents
           (Layer.Face, Context, Text, Extents);
      else
         Pango.Cairo.Fonts.Get_Text_Extents
           (Layer.Face, Context, Text, Extents);
      end if;
      if Extents.Height > 0.0 and then Extents.Width > 0.0 then
         Gain := Layer.Height / Extents.Height;
         if Layer.Scaled then
            Gain := Gain * Size;
         end if;
         Cairo.Translate
           (Cr => Context,
            Tx => X,
            Ty => Y);
         --           Set_Source_RGB (Context, 1.0, 0.0, 0.0);
         --           Move_To (Context, -Extents.Width / 2.0, -Extents.Height / 2.0);
         --           Rel_Line_To (Context, Extents.Width, 0.0);
         --           Rel_Line_To (Context, 0.0, Extents.Height);
         --           Rel_Line_To (Context, -Extents.Width, 0.0);
         --           Close_Path (Context);
         --           Stroke (context);
         --
         --           Move_To
         --           (  Cr => Context,
         --              X  => -Extents.X_Bearing - Extents.Width / 2.0,
         --              Y  => -Extents.Y_Bearing - Extents.Height / 2.0
         --           );
         --           Show_Text (Layer.Face, Context, Text);
         --           Move_To (Context, 0.0, 0.0);

         case Layer.Mode is
            when Moved_Inside =>
               Cairo.Translate
                 (Cr => Context,
                  Tx => (Extents.Width * 0.5 * Gain * Layer.Stretch *
                             Cairo.Elementary_Functions.Cos (Layer.Angle)),
                  Ty => (-Extents.Height * 0.5 * Gain *
                             Cairo.Elementary_Functions.Sin (Layer.Angle)));
            when Moved_Outside =>
               Cairo.Translate
                 (Cr => Context,
                  Tx => (Extents.Width * 0.5 * Gain * Layer.Stretch *
                             Cairo.Elementary_Functions.Cos (Layer.Angle)),
                  Ty => (Extents.Height * 0.5 * Gain *
                             Cairo.Elementary_Functions.Sin (Layer.Angle)));
            when Moved_Centered =>
               null;
            when Rotated =>
               Cairo.Rotate (Context, Layer.Angle + Ada.Numerics.Pi / 2.0);
            when Skewed =>
               Cairo.Rotate (Context, Layer.Angle);
               declare
                  Matrix : aliased Cairo.Cairo_Matrix;
               begin
                  Matrix.Xx := 1.0;
                  Matrix.Xy :=
                    -1.0 * Cairo.Elementary_Functions.Tan (Layer.Skew);
                  Matrix.X0 := 0.0;
                  Matrix.Yx := 0.0;
                  Matrix.Yy := 1.0;
                  Matrix.Y0 := 0.0;
                  Cairo.Transform (Context, Matrix'Access);
               end;
         end case;
--           -- Bounding rectangle
--           Set_Source_RGB (Context, 0.0, 0.0, 1.0);
--           Move_To (Context, 0.0, 0.0);
--           Rel_Move_To
--           (  Cr => Context,
--              Dx => -Extents.Width  * 0.5 * Gain * Layer.Stretch,
--              Dy => -Extents.Height * 0.5 * Gain
--           );
--           Rel_Line_To
--           (  Cr => Context,
--              Dx => Extents.Width * Gain * Layer.Stretch,
--              Dy => 0.0
--           );
--           Rel_Line_To
--           (  Cr => Context,
--              Dx => 0.0,
--              Dy => Extents.Height * Gain
--           );
--           Rel_Line_To
--           (  Cr => Context,
--              Dx => -Extents.Width * Gain * Layer.Stretch,
--              Dy => 0.0
--           );
--           Close_Path (Context);
--           Stroke (Context);
--           Move_To (Context, 0.0, 0.0);

         Cairo.Scale
           (Cr => Context,
            Sx => Gain * Layer.Stretch,
            Sy => Gain);

--           -- Bounding rectangle
--           Set_Source_RGB (Context, 1.0, 1.0, 1.0);
--           Move_To (Context, 0.0, 0.0);
--           Rel_Move_To
--           (  Cr => Context,
--              Dx => -Extents.Width  * 0.5,
--              Dy => -Extents.Height * 0.5
--           );
--           Rel_Line_To (Context, Extents.Width, 0.0);
--           Rel_Line_To (Context, 0.0, Extents.Height);
--           Rel_Line_To (Context, -Extents.Width, 0.0);
--           Close_Path (Context);
--           Stroke (Context);

         Cairo.Set_Source_Rgb
           (Context,
            Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last));
         Cairo.Move_To
           (Cr => Context,
            X  => -(Extents.X_Bearing + Extents.Width  * 0.5),
            Y  => -(Extents.Y_Bearing + Extents.Height * 0.5));
         if Layer.Markup then
            Pango.Cairo.Fonts.Show_Markup (Layer.Face, Context, Text);
         else
            Pango.Cairo.Fonts.Show_Text (Layer.Face, Context, Text);
         end if;
      end if;
      Layer.Updated := False;
   end Draw;

   overriding procedure Finalize (Layer : in out Label_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      Free (Layer.Text);
   end Finalize;

   function Get_Angle (Layer : Label_Layer) return Gdouble is
   begin
      return Layer.Angle;
   end Get_Angle;

   function Get_Color (Layer : Label_Layer) return Gdk.Color.Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Face (Layer : Label_Layer)
                      return Pango.Cairo.Fonts.Pango_Cairo_Font is
   begin
      return Layer.Face;
   end Get_Face;

   function Get_Height (Layer : Label_Layer) return Gdouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Location
     (Layer : Label_Layer) return Cairo.Ellipses.Cairo_Tuple is
   begin
      return (X => Layer.X, Y => Layer.Y);
   end Get_Location;

   function Get_Markup (Layer : Label_Layer) return Boolean is
   begin
      return Layer.Markup;
   end Get_Markup;

   function Get_Mode (Layer : Label_Layer) return Text_Transformation is
   begin
      return Layer.Mode;
   end Get_Mode;

   overriding function Get_Properties_Number
     (Layer : Label_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         - Layer_Property'Pos (Layer_Property'First) + 1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Label_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x",
                    Nick    => "x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The x-coordinate of the label's center");
            when Property_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y",
                    Nick    => "y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The y-coordinate of the label's center");
            when Property_Stretch =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "stretch",
                    Nick    => "stretch",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                      "The relation of the rendered width " &
                      "of the text to its original width. " &
                      "The stretch value 1 keeps text " &
                      "unchanged");
            when Property_Height =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "height",
                    Nick    => "height",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 12.0,
                    Blurb   => "The text font height");
            when Property_Font_Type =>
               return
                  Pango.Cairo.Fonts.Font_Type_Property.Gnew_Enum
                   (Name    => "font-type",
                    Nick    => "font type",
                    Default => Pango.Cairo.Fonts.Pango_Font,
                    Blurb   => "The backend used for the font, " &
                               "e.g. toy font, pango font");
            when Property_Family =>
               return
                 Glib.Properties.Creation.Gnew_String
                   (Name    => "font-family",
                    Nick    => "font family",
                    Default => "arial",
                    Blurb   => "The text font family, " &
                               "e.g. courier");
            when Property_Mode =>
               return
                 Gtk.Layered.Text_Transformation_Property.Gnew_Enum
                   (Name    => "text-transformation-mode",
                    Nick    => "text transformation mode",
                    Default => Moved_Centered,
                    Blurb   => "The method how the text is " &
                               "transformed and aligned");
            when Property_Slant =>
               return
                 Cairo.Font_Slant_Property.Gnew_Enum
                   (Name    => "font-slant",
                    Nick    => "font slant",
                    Default => Cairo.Cairo_Font_Slant_Normal,
                    Blurb   => "The text font slant");
            when Property_Font_Size =>
               return
                  Glib.Properties.Creation.Gnew_Uint
                   (Name    => "font-size",
                    Nick    => "font size",
                    Minimum => 1,
                    Maximum => Guint (Gint'Last),
                    Default => 12,
                    Blurb   => "The font size in points. " &
                               "The value is only relevant for " &
                               "pango fonts. For cairo toy size " &
                               "is ignored");
            when Property_Text =>
               return
                 Glib.Properties.Creation.Gnew_String
                   (Name    => "text",
                    Nick    => "text",
                    Default => "",
                    Blurb   => "The label text");
            when Property_Markup =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "markup-flag",
                    Nick    => "text markups",
                    Default => False,
                    Blurb   => "The label text markup flag. " &
                               "False is for plain text or " &
                               "True for a text with pango markup");
            when Property_Weight =>
               return
                 Pango.Enums.Weight_Property.Gnew_Enum
                   (Name    => "font-weight",
                    Nick    => "font weight",
                    Default => Pango.Enums.Pango_Weight_Normal,
                    Blurb   => "The text font weight");
            when Property_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The text color");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   => "The text size is changed when " &
                               "the widget is resized");
            when Property_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "angle",
                    Nick    => "angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angle of the label text base " &
                               "line when the text transformation " &
                               "mode is rotated or skewed. " &
                               "Otherwise it is ignored");
            when Property_Skew =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "skew",
                    Nick    => "skew",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "When the text transformation mode " &
                               "is Rotated, skew is the angle " &
                               "between the base text line and the " &
                               "vertical axis of the text origin");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Label_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.X);
               when Property_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Y);
               when Property_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Angle);
               when Property_Skew =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Skew);
               when Property_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Color);
               when Property_Height =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Height);
               when Property_Mode =>
                  Gtk.Layered.Text_Transformation_Property.Set_Enum
                    (Value,
                     Layer.Mode);
               when Property_Font_Type =>
                  Pango.Cairo.Fonts.Font_Type_Property.Set_Enum
                    (Value,
                     Pango.Cairo.Fonts.Get_Type (Layer.Face));
               when Property_Family =>
                  Glib.Values.Init (Value, GType_String);
                  Glib.Values.Set_String
                    (Value, Pango.Cairo.Fonts.Get_Family (Layer.Face));
               when Property_Slant =>
                  Cairo.Font_Slant_Property.Set_Enum
                    (Value,
                     Pango.Cairo.Fonts.Get_Slant (Layer.Face));
               when Property_Font_Size =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint
                    (Value, Guint (Pango.Cairo.Fonts.Get_Size (Layer.Face)));
               when Property_Weight =>
                  Pango.Enums.Weight_Property.Set_Enum
                    (Value,
                     Pango.Cairo.Fonts.Get_Weight (Layer.Face));
               when Property_Stretch =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Stretch);
               when Property_Text =>
                  Glib.Values.Init (Value, GType_String);
                  if Layer.Text = null then
                     Glib.Values.Set_String (Value, "");
                  else
                     Glib.Values.Set_String (Value, Layer.Text.all);
                  end if;
               when Property_Markup =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Markup);
               when Property_Scaled =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   overriding function Get_Scaled (Layer : Label_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Skew (Layer : Label_Layer) return Gdouble is
   begin
      return Layer.Skew;
   end Get_Skew;

   function Get_Stretch (Layer : Label_Layer) return Gdouble is
   begin
      return Layer.Stretch;
   end Get_Stretch;

   function Get_Text (Layer : Label_Layer) return UTF8_String is
   begin
      return Text_Mutex.Get_Text (Layer);
   end Get_Text;

   overriding function Is_Updated (Layer : Label_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Label_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.X := Layer.X + Offset.X;
      Layer.Y := Layer.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Label_Layer)
   is
      X, Y    : Gdouble;
      Face    : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height  : Gdouble;
      Stretch : Gdouble;
      Mode    : Text_Transformation;
      Color   : Gdk.Color.Gdk_Color;
      Angle   : Gdouble;
      Skew    : Gdouble;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, X);
      Gtk.Layered.Stream_IO.Restore (Stream, Y);
      Pango.Cairo.Fonts.Restore (Stream, Face);
      Gtk.Layered.Stream_IO.Restore (Stream, Height);
      Gtk.Layered.Stream_IO.Restore (Stream, Stretch);
      Gtk.Layered.Stream_IO.Restore (Stream, Mode);
      Gtk.Layered.Stream_IO.Restore (Stream, Color);
      Gtk.Layered.Stream_IO.Restore (Stream, Angle);
      Gtk.Layered.Stream_IO.Restore (Stream, Skew);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled, Layer.Markup);
      Set
        (Layer    => Layer,
         Location => (X => X, Y => Y),
         Face     => Face,
         Height   => Height,
         Stretch  => Stretch,
         Mode     => Mode,
         Color    => Color,
         Angle    => Angle,
         Skew     => Skew);
      Layer.Set_Text
        (Gtk.Layered.Stream_IO.Restore (Stream'Access), Layer.Markup);
   end Restore;

   overriding procedure Scale
     (Layer  : in out Label_Layer;
      Factor : Gdouble)
   is
      Height : constant Gdouble := Layer.Height * Factor;
   begin
      if Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      end if;
      Layer.Height  := Height;
      Layer.Updated := True;
   end Scale;

   procedure Set
     (Layer    : in out Label_Layer;
      Location : Cairo.Ellipses.Cairo_Tuple;
      Face     : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height   : Gdouble;
      Stretch  : Gdouble;
      Mode     : Text_Transformation;
      Color    : Gdk.Color.Gdk_Color;
      Angle    : Gdouble;
      Skew     : Gdouble) is
   begin
      if abs Skew >= Ada.Numerics.Pi / 2.0 - Eps then
         raise Constraint_Error with "Skew is greater than half Pi";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      elsif Stretch <= 0.0 then
         raise Constraint_Error with "Non-positive stretch";
      end if;
      Layer.X       := Location.X;
      Layer.Y       := Location.Y;
      Layer.Face    := Face;
      Layer.Height  := Height;
      Layer.Stretch := Stretch;
      Layer.Mode    := Mode;
      Layer.Color   := Color;
      Layer.Angle   := Angle;
      Layer.Skew    := Skew;
      Layer.Updated := True;
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Label_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               Layer.X := Glib.Values.Get_Double (Value);
            when Property_Y =>
               Layer.Y := Glib.Values.Get_Double (Value);
            when Property_Angle =>
               Layer.Angle := Glib.Values.Get_Double (Value);
               if
                 Layer.Angle not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Angle :=
                     Gdouble'Remainder (Layer.Angle, 2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Skew =>
               Layer.Skew := Glib.Values.Get_Double (Value);
               if
                 Layer.Skew not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Skew :=
                     Gdouble'Remainder (Layer.Skew, 2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Stretch =>
               Layer.Stretch := Glib.Values.Get_Double (Value);
               if Layer.Stretch < 0.0 then
                  Layer.Stretch := 0.0;
               end if;
            when Property_Height =>
               Layer.Height := Glib.Values.Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Mode =>
               Layer.Mode :=
                 Gtk.Layered.Text_Transformation_Property.Get_Enum
                   (Value);
            when Property_Font_Type =>
               Pango.Cairo.Fonts.Set_Type
                 (Layer.Face,
                  Pango.Cairo.Fonts.Font_Type_Property.Get_Enum (Value));
            when Property_Family =>
               Pango.Cairo.Fonts.Set_Family
                 (Layer.Face, Glib.Values.Get_String (Value));
            when Property_Slant =>
               Pango.Cairo.Fonts.Set_Slant
                 (Layer.Face,
                  Cairo.Font_Slant_Property.Get_Enum (Value));
            when Property_Font_Size =>
               Pango.Cairo.Fonts.Set_Size
                 (Layer.Face,
                  Gint
                    (Guint'Max
                         (Guint'Min
                              (Glib.Values.Get_Uint (Value),
                               Guint (Gint'Last)),
                          1)));
            when Property_Weight =>
               Pango.Cairo.Fonts.Set_Weight
                 (Layer.Face,
                  Pango.Enums.Weight_Property.Get_Enum (Value));
            when Property_Text =>
               Set_Text (Layer, Glib.Values.Get_String (Value), Layer.Markup);
            when Property_Markup =>
               Layer.Markup := Glib.Values.Get_Boolean (Value);
            when Property_Color =>
               Layer.Color := Gdk.Color.Get_Value (Value);
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Label_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Text
     (Layer  : in out Label_Layer;
      Text   : UTF8_String;
      Markup : Boolean := False) is
   begin
      Text_Mutex.Set_Text (Layer, Text, Markup);
   end Set_Text;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Label_Layer) is
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.X);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Y);
      Pango.Cairo.Fonts.Store (Stream, Layer.Face);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Height);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Stretch);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Mode);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Color);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Angle);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Skew);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Scaled, Layer.Markup);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Get_Text);
   end Store;

end Gtk.Layered.Label;
