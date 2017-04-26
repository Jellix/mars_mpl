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
--____________________________________________________________________--

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with Glib.Messages;               use Glib.Messages;
with Glib.Properties.Creation;    use Glib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;
with Pango.Layout;                use Pango.Layout;

with Ada.Unchecked_Deallocation;
with Cairo.Font_Slant_Property;
with Gtk.Layered.Text_Transformation_Property;
with Pango.Enums.Weight_Property;
with Pango.Cairo.Fonts.Font_Type_Property;

package body Gtk.Layered.Label is

   Eps : constant := 1.0E-6;

   type Layer_Property is
        (  Property_Scaled,
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
           Property_Color
        );

   type Label_Ptr is access all Label_Layer;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Label." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation (Label_Layer, Label_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   protected Text_Mutex is
      function Get_Text (Layer : Label_Layer) return UTF8_String;
      procedure Set_Text
                (  Layer  : in out Label_Layer;
                   Text   : UTF8_String;
                   Markup : Boolean
                );
   end Text_Mutex;

   protected body Text_Mutex is
      function Get_Text (Layer : Label_Layer) return UTF8_String is
      begin
         return Layer.Text.all;
      end Get_Text;

      procedure Set_Text
                (  Layer  : in out Label_Layer;
                   Text   : UTF8_String;
                   Markup : Boolean
                )  is
         Ptr : constant String_Ptr := new UTF8_String'(Text);
      begin
         Free (Layer.Text);
         Layer.Text    := Ptr;
         Layer.Markup  := Markup;
         Layer.Updated := True;
      end Set_Text;

   end Text_Mutex;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Label_Layer is
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
             (  Under    : not null access Layer_Location'Class;
                Text     : UTF8_String := "";
                Location : Cairo_Tuple := (0.0, 0.0);
                Face     : Pango_Cairo_Font :=
                              Create_Toy
                              (  Family => "sans",
                                 Slant  => Cairo_Font_Slant_Normal,
                                 Weight => Cairo_Font_Weight_Normal
                              );
                Height   : Gdouble             := 12.0;
                Stretch  : Gdouble             := 1.0;
                Mode     : Text_Transformation := Moved_Centered;
                Color    : Gdk_Color           := RGB (0.0, 0.0, 0.0);
                Angle    : Gdouble             := 3.0 * Pi / 2.0;
                Skew     : Gdouble             := 0.0;
                Markup   : Boolean             := False;
                Scaled   : Boolean             := False
             )  is
      Ptr   : Label_Ptr := new Label_Layer;
      Layer : Label_Layer renames Ptr.all;
   begin
      Layer.Text   := new UTF8_String'(Text);
      Layer.Markup := Markup;
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer    => Layer,
         Location => Location,
         Face     => Face,
         Height   => Height,
         Stretch  => Stretch,
         Mode     => Mode,
         Color    => Color,
         Angle    => Angle,
         Skew     => Skew
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Label;

   function Add_Label
            (  Under    : not null access Layer_Location'Class;
               Text     : UTF8_String := "";
               Location : Cairo_Tuple := (0.0, 0.0);
               Face     : Pango_Cairo_Font :=
                             Create_Toy
                             (  Family => "sans",
                                Slant  => Cairo_Font_Slant_Normal,
                                Weight => Cairo_Font_Weight_Normal
                             );
               Height   : Gdouble             := 12.0;
               Stretch  : Gdouble             := 1.0;
               Mode     : Text_Transformation := Moved_Centered;
               Color    : Gdk_Color           := RGB (0.0, 0.0, 0.0);
               Angle    : Gdouble             := 3.0 * Pi / 2.0;
               Skew     : Gdouble             := 0.0;
               Markup   : Boolean             := False;
               Scaled   : Boolean             := False
            )  return not null access Label_Layer is
      Ptr   : Label_Ptr := new Label_Layer;
      Layer : Label_Layer renames Ptr.all;
   begin
      Layer.Text   := new UTF8_String'(Text);
      Layer.Markup := Markup;
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer    => Layer,
         Location => Location,
         Face     => Face,
         Height   => Height,
         Stretch  => Stretch,
         Mode     => Mode,
         Color    => Color,
         Angle    => Angle,
         Skew     => Skew
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Label;

   procedure Draw
             (  Layer   : in out Label_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Size    : constant Gdouble     := Layer.Widget.Get_Size;
      Text    : constant UTF8_String := Text_Mutex.Get_Text (Layer);
      Gain    : GDouble              := 1.0;
      State   : Context_State        := Save (Context);
      X, Y    : aliased GDouble;
      Extents : Cairo_Text_Extents;
   begin
      if Layer.Scaled then
         declare
            Center : constant Cairo_Tuple := Layer.Widget.Get_Center;
         begin
            X := Center.X + Layer.X * Size;
            Y := Center.Y + Layer.Y * Size;
         end;
      else
         X := Layer.X;
         Y := Layer.Y;
      end if;
      Move_To (Context, X, Y);
      if Layer.Markup then
         Get_Markup_Extents (Layer.Face, Context, Text, Extents);
      else
         Get_Text_Extents (Layer.Face, Context, Text, Extents);
      end if;
      if Extents.Height > 0.0 and then Extents.Width > 0.0 then
         Gain := Layer.Height / Extents.Height;
         if Layer.Scaled then
            Gain := Gain * Size;
         end if;
         Translate
         (  Cr => Context,
            Tx => X,
            Ty => Y
         );
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
               Translate
               (  Cr => Context,
                  Tx => ( -Extents.Width
                        *  0.5 * Gain * Layer.Stretch
                        *  cos (Layer.Angle)
                        ),
                  Ty => ( -Extents.Height
                        *  0.5 * Gain
                        *  sin (Layer.Angle)
               )        );
            when Moved_Outside =>
               Translate
               (  Cr => Context,
                  Tx => (  Extents.Width
                        *  0.5 * Gain * Layer.Stretch
                        *  cos (Layer.Angle)
                        ),
                  Ty => (  Extents.Height
                        *  0.5 * Gain
                        *  sin (Layer.Angle)
               )        );
            when Moved_Centered =>
               null;
            when Rotated =>
--                 declare
--                    Cos_Angle : constant GDouble := cos (Layer.Angle);
--                    Sin_Angle : constant GDouble := sin (Layer.Angle);
--                 begin
--                    Translate
--                    (  Cr => Context,
--                       Tx => (  Extents.Width  * Sin_Angle
--                             +  Extents.Height * Cos_Angle
--                             ),
--                       Ty => ( -Extents.Width  * Cos_Angle
--                             +  Extents.Height * Sin_Angle
--                    )        );
--                 end;
               Rotate (Context, Layer.Angle + Pi / 2.0);
            when Skewed =>
               Rotate (Context, Layer.Angle);
               declare
                  Matrix : aliased Cairo_Matrix;
               begin
                  Matrix.XX := 1.0;
                  Matrix.XY := -1.0 * tan (Layer.Skew);
                  Matrix.X0 := 0.0;
                  Matrix.YX := 0.0;
                  Matrix.YY := 1.0;
                  Matrix.Y0 := 0.0;
                  Transform (Context, Matrix'Access);
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

         Scale
         (  Cr => Context,
            Sx => Gain * Layer.Stretch,
            Sy => Gain
         );

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

         Set_Source_RGB
         (  Context,
            GDouble (Red   (Layer.Color)) / GDouble (Guint16'Last),
            GDouble (Green (Layer.Color)) / GDouble (Guint16'Last),
            GDouble (Blue  (Layer.Color)) / GDouble (Guint16'Last)
         );
         Move_To
         (  Cr => Context,
            X  => -(Extents.X_Bearing + Extents.Width  * 0.5),
            Y  => -(Extents.Y_Bearing + Extents.Height * 0.5)
         );
         if Layer.Markup then
            Show_Markup (Layer.Face, Context, Text);
         else
            Show_Text (Layer.Face, Context, Text);
         end if;
      end if;
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out Label_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      Free (Layer.Text);
   end Finalize;

   function Get_Angle (Layer : Label_Layer) return GDouble is
   begin
      return Layer.Angle;
   end Get_Angle;

   function Get_Color (Layer : Label_Layer) return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Face (Layer : Label_Layer) return Pango_Cairo_Font is
   begin
      return Layer.Face;
   end Get_Face;

   function Get_Height (Layer : Label_Layer) return GDouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Location (Layer : Label_Layer) return Cairo_Tuple is
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

   function Get_Properties_Number
            (  Layer : Label_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Label_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the label's " &
                                "center"
                  );
            when Property_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the label's " &
                                "center"
                  );
            when Property_Stretch =>
               return
                  Gnew_Double
                  (  Name    => "stretch",
                     Nick    => "stretch",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The relation of the rendered width " &
                                "of the text to its original width. " &
                                "The stretch value 1 keeps text " &
                                "unchanged"
                  );
            when Property_Height =>
               return
                  Gnew_Double
                  (  Name    => "height",
                     Nick    => "height",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 12.0,
                     Blurb   => "The text font height"
                  );
            when Property_Font_Type =>
               return
                  Pango.Cairo.Fonts.Font_Type_Property.Gnew_Enum
                  (  Name    => "font-type",
                     Nick    => "font type",
                     Default => Pango_Font,
                     Blurb   => "The backend used for the font, " &
                                "e.g. toy font, pango font"
                  );
            when Property_Family =>
               return
                  Gnew_String
                  (  Name    => "font-familiy",
                     Nick    => "font famility",
                     Default => "arial",
                     Blurb   => "The text font family, " &
                                "e.g. courier"
                  );
            when Property_Mode =>
               return
                  Gtk.Layered.Text_Transformation_Property.Gnew_Enum
                  (  Name    => "text-transformation-mode",
                     Nick    => "text transformation mode",
                     Default => Moved_Centered,
                     Blurb   => "The method how the text is " &
                                "transformed and aligned"
                  );
            when Property_Slant =>
               return
                  Cairo.Font_Slant_Property.Gnew_Enum
                  (  Name    => "font-slant",
                     Nick    => "font slant",
                     Default => CAIRO_FONT_SLANT_NORMAL,
                     Blurb   => "The text font slant"
                  );
            when Property_Font_Size =>
               return
                  Gnew_UInt
                  (  Name    => "font-size",
                     Nick    => "font size",
                     Minimum => 1,
                     Maximum => GUInt (GInt'Last),
                     Default => 12,
                     Blurb   => "The font size in points. " &
                                "The value is only relevant for " &
                                "pango fonts. For cairo toy size " &
                                "is ignored"
                  );
            when Property_Text =>
               return
                  Gnew_String
                  (  Name    => "text",
                     Nick    => "text",
                     Default => "",
                     Blurb   => "The label text"
                  );
            when Property_Markup =>
               return
                  Gnew_Boolean
                  (  Name    => "markup-flag",
                     Nick    => "text markups",
                     Default => False,
                     Blurb   => "The label text markup flag. " &
                                "False is for plain text or " &
                                "True for a text with pango markup"
                  );
            when Property_Weight =>
               return
                  Pango.Enums.Weight_Property.Gnew_Enum
                  (  Name    => "font-weight",
                     Nick    => "font weight",
                     Default => Pango.Enums.Pango_Weight_Normal,
                     Blurb   => "The text font weight"
                  );
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The text color"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The text size is changed when " &
                                "the widget is resized"
                  );
            when Property_Angle =>
               return
                  Gnew_Double
                  (  Name    => "angle",
                     Nick    => "angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the label text base " &
                                "line when the text transformation " &
                                "mode is rotated or skewed. " &
                                "Otherwise it is ignored"
                  );
            when Property_Skew =>
               return
                  Gnew_Double
                  (  Name    => "skew",
                     Nick    => "skew",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "When the text transformation mode " &
                                "is Rotated, skew is the angle " &
                                "between the base text line and the " &
                                "vertical axis of the text origin"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Label_Layer;
               Property : Positive
            )  return GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.X);
               when Property_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Y);
               when Property_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Angle);
               when Property_Skew =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Skew);
               when Property_Color =>
                  Set_Value (Value, Layer.Color);
               when Property_Height =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Height);
               when Property_Mode =>
                  Gtk.Layered.Text_Transformation_Property.Set_Enum
                  (  Value,
                     Layer.Mode
                  );
               when Property_Font_Type =>
                  Pango.Cairo.Fonts.Font_Type_Property.Set_Enum
                  (  Value,
                     Get_Type (Layer.Face)
                  );
               when Property_Family =>
                  Init (Value, GType_String);
                  Set_String (Value, Get_Family (Layer.Face));
               when Property_Slant =>
                  Cairo.Font_Slant_Property.Set_Enum
                  (  Value,
                     Get_Slant (Layer.Face)
                  );
               when Property_Font_Size =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Get_Size (Layer.Face)));
               when Property_Weight =>
                  Pango.Enums.Weight_Property.Set_Enum
                  (  Value,
                     Get_Weight (Layer.Face)
                  );
               when Property_Stretch =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Stretch);
               when Property_Text =>
                  Init (Value, GType_String);
                  if Layer.Text = null then
                     Set_String (Value, "");
                  else
                     Set_String (Value, Layer.Text.all);
                  end if;
               when Property_Markup =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Markup);
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Label_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Skew (Layer : Label_Layer) return GDouble is
   begin
      return Layer.Skew;
   end Get_Skew;

   function Get_Stretch (Layer : Label_Layer) return GDouble is
   begin
      return Layer.Stretch;
   end Get_Stretch;

   function Get_Text (Layer : Label_Layer) return UTF8_String is
   begin
      return Text_Mutex.Get_Text (Layer);
   end Get_Text;

   function Is_Updated (Layer : Label_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Label_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.X := Layer.X + Offset.X;
      Layer.Y := Layer.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Label_Layer
             )  is
      X, Y    : GDouble;
      Face    : Pango_Cairo_Font;
      Height  : GDouble;
      Stretch : GDouble;
      Mode    : Text_Transformation;
      Color   : Gdk_Color;
      Angle   : GDouble;
      Skew    : GDouble;
   begin
      Restore (Stream, X);
      Restore (Stream, Y);
      Restore (Stream, Face);
      Restore (Stream, Height);
      Restore (Stream, Stretch);
      Restore (Stream, Mode);
      Restore (Stream, Color);
      Restore (Stream, Angle);
      Restore (Stream, Skew);
      Restore (Stream, Layer.Scaled, Layer.Markup);
      Set
      (  Layer    => Layer,
         Location => (X => X, Y => Y),
         Face     => Face,
         Height   => Height,
         Stretch  => Stretch,
         Mode     => Mode,
         Color    => Color,
         Angle    => Angle,
         Skew     => Skew
      );
      Layer.Set_Text (Restore (Stream'Access), Layer.Markup);
   end Restore;

   procedure Scale
             (  Layer  : in out Label_Layer;
                Factor : GDouble
             )  is
      Height : constant GDouble := Layer.Height * Factor;
   begin
      if Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      end if;
      Layer.Height  := Height;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer    : in out Label_Layer;
                Location : Cairo_Tuple;
                Face     : Pango_Cairo_Font;
                Height   : GDouble;
                Stretch  : GDouble;
                Mode     : Text_Transformation;
                Color    : Gdk_Color;
                Angle    : GDouble;
                Skew     : GDouble
             )  is
   begin
      if abs Skew >= Pi / 2.0 - Eps then
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

   procedure Set_Property_Value
             (  Layer    : in out Label_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               Layer.X := Get_Double (Value);
            when Property_Y =>
               Layer.Y := Get_Double (Value);
            when Property_Angle =>
               Layer.Angle := Get_Double (Value);
               if Layer.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Angle :=
                     GDouble'Remainder (Layer.Angle, 2.0 * Pi);
               end if;
            when Property_Skew =>
               Layer.Skew := Get_Double (Value);
               if Layer.Skew not in -2.0 * Pi..2.0 * Pi then
                  Layer.Skew :=
                     GDouble'Remainder (Layer.Skew, 2.0 * Pi);
               end if;
            when Property_Stretch =>
               Layer.Stretch := Get_Double (Value);
               if Layer.Stretch < 0.0 then
                  Layer.Stretch := 0.0;
               end if;
            when Property_Height =>
               Layer.Height := Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Mode =>
               Layer.Mode :=
                  Gtk.Layered.Text_Transformation_Property.Get_Enum
                  (  Value
                  );
            when Property_Font_Type =>
               Set_Type
               (  Layer.Face,
                  Pango.Cairo.Fonts.Font_Type_Property.Get_Enum (Value)
               );
            when Property_Family =>
               Set_Family (Layer.Face, Get_String (Value));
            when Property_Slant =>
               Set_Slant
               (  Layer.Face,
                  Cairo.Font_Slant_Property.Get_Enum (Value)
               );
            when Property_Font_Size =>
               Set_Size
               (  Layer.Face,
                  GInt
                  (  GUInt'Max
                     (  GUInt'Min
                        (  Get_UInt (Value),
                           GUInt (GInt'Last)
                        ),
                        1
               )  )  );
            when Property_Weight =>
               Set_Weight
               (  Layer.Face,
                  Pango.Enums.Weight_Property.Get_Enum (Value)
               );
            when Property_Text =>
               Set_Text (Layer, Get_String (Value), Layer.Markup);
            when Property_Markup =>
               Layer.Markup := Get_Boolean (Value);
            when Property_Color =>
               Layer.Color := Get_Value (Value);
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Label_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Text
             (  Layer  : in out Label_Layer;
                Text   : UTF8_String;
                Markup : Boolean := False
             )  is
   begin
      Text_Mutex.Set_Text (Layer, Text, Markup);
   end Set_Text;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Label_Layer
             )  is
   begin
      Store (Stream, Layer.X);
      Store (Stream, Layer.Y);
      Store (Stream, Layer.Face);
      Store (Stream, Layer.Height);
      Store (Stream, Layer.Stretch);
      Store (Stream, Layer.Mode);
      Store (Stream, Layer.Color);
      Store (Stream, Layer.Angle);
      Store (Stream, Layer.Skew);
      Store (Stream, Layer.Scaled, Layer.Markup);
      Store (Stream, Layer.Get_Text);
   end Store;

end Gtk.Layered.Label;
