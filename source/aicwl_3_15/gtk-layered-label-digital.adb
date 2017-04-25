--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Label.Digital                   Luebeck            --
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
--____________________________________________________________________--

with Ada.Exceptions;            use Ada.Exceptions;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;     use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Font_Slant_Property;
with Gtk.Layered.Text_Transformation_Property;
with Gtk.Layered.Waveform;
with Pango.Enums.Weight_Property;
with Pango.Cairo.Fonts.Font_Type_Property;

package body Gtk.Layered.Label.Digital is

   type Layer_Property is
        (  Property_Scaled,
           Property_X,
           Property_Y,
           Property_Text,
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
           Property_Color,
           Property_Base,
           Property_Precision,
           Property_Put_Plus,
           Property_Absolute
        );

   type Digital_Ptr is access all Digital_Layer;

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Adjustment_Record,
             Digital_Ptr
          );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Digital." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation (Digital_Layer, Digital_Ptr);

   procedure Changed
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Layer      : Digital_Ptr
             );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Digital_Layer is
      Ptr : Digital_Ptr := new Digital_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Adjustment
             (  Layer      : in out Digital_Layer;
                Adjustment : not null access Gtk_Adjustment_Record'Class
             )  is
   begin
      Ref (Adjustment);
      Layer.Adjustment := Adjustment.all'Unchecked_Access;
      Layer.Changed :=
         Handlers.Connect
         (  Adjustment,
            "changed",
            Handlers.To_Marshaller (Changed'Access),
            Layer'Unchecked_Access
         );
      Layer.Value_Changed :=
         Handlers.Connect
         (  Adjustment,
            "value_changed",
            Handlers.To_Marshaller (Changed'Access),
            Layer'Unchecked_Access
         );
      Layer.Value := Adjustment.Get_Value;
   end Add_Adjustment;

   procedure Add_Digital
             (  Under      : not null access Layer_Location'Class;
                Location   : Cairo_Tuple := (0.0, 0.0);
                Face       : Pango_Cairo_Font :=
                                Create_Toy
                                (  Family => "sans",
                                   Slant  => CAIRO_FONT_SLANT_NORMAL,
                                   Weight => CAIRO_FONT_WEIGHT_NORMAL
                                );
                Height     : GDouble             := 12.0;
                Stretch    : GDouble             := 1.0;
                Mode       : Text_Transformation := Rotated;
                Color      : Gdk_Color           := RGB (0.0, 0.0, 0.0);
                Angle      : GDouble             := 0.0;
                Skew       : GDouble             := 0.0;
                Base       : NumberBase          := 10;
                Precision  : Integer             := 0;
                Absolute   : Boolean             := True;
                Put_Plus   : Boolean             := False;
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean := False
             )  is
      Ptr   : Digital_Ptr := new Digital_Layer;
      Layer : Digital_Layer renames Ptr.all;
   begin
      Layer.Text := new UTF8_String'("");
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer     => Layer,
         Location  => Location,
         Face      => Face,
         Height    => Height,
         Stretch   => Stretch,
         Mode      => Mode,
         Color     => Color,
         Angle     => Angle,
         Skew      => Skew,
         Base      => Base,
         Precision => Precision,
         Absolute  => Absolute,
         Put_Plus  => Put_Plus
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      begin
         Layer.Set_Text
         (  Digital_Layer'Class (Layer).Render (Layer.Value)
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Warning,
               (  "Fault rendering value: "
               &  Exception_Information (Error)
               &  Where ("Add_Digital")
            )  );
      end;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Digital;

   function Add_Digital
            (  Under      : not null access Layer_Location'Class;
               Location   : Cairo_Tuple := (0.0, 0.0);
               Face       : Pango_Cairo_Font :=
                               Create_Toy
                               (  Family => "sans",
                                  Slant  => CAIRO_FONT_SLANT_NORMAL,
                                  Weight => CAIRO_FONT_WEIGHT_NORMAL
                               );
               Height     : GDouble             := 12.0;
               Stretch    : GDouble             := 1.0;
               Mode       : Text_Transformation := Rotated;
               Color      : Gdk_Color           := RGB (0.0, 0.0, 0.0);
               Angle      : GDouble             := 0.0;
               Skew       : GDouble             := 0.0;
               Base       : NumberBase          := 10;
               Precision  : Integer             := 0;
               Absolute   : Boolean             := True;
               Put_Plus   : Boolean             := False;
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean := False
            )  return not null access Digital_Layer is
      Ptr   : Digital_Ptr := new Digital_Layer;
      Layer : Digital_Layer renames Ptr.all;
   begin
      Layer.Text := new UTF8_String'("");
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer     => Layer,
         Location  => Location,
         Face      => Face,
         Height    => Height,
         Stretch   => Stretch,
         Mode      => Mode,
         Color     => Color,
         Angle     => Angle,
         Skew      => Skew,
         Base      => Base,
         Precision => Precision,
         Absolute  => Absolute,
         Put_Plus  => Put_Plus
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      begin
         Layer.Set_Text
         (  Digital_Layer'Class (Layer).Render (Layer.Value)
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Warning,
               (  "Fault rendering value: "
               &  Exception_Information (Error)
               &  Where ("Add_Digital")
            )  );
      end;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Digital;

   function Add_Label
            (  Under    : not null access Layer_Location'Class;
               Text     : UTF8_String;
               Location : Cairo_Tuple;
               Face     : Pango_Cairo_Font;
               Height   : GDouble;
               Stretch  : GDouble;
               Mode     : Text_Transformation;
               Color    : Gdk_Color;
               Angle    : GDouble;
               Skew     : GDouble;
               Markup   : Boolean;
               Scaled   : Boolean
            )  return not null access Digital_Layer is
   begin
      raise Program_Error with "Add_Label is disallowed";
      return Add_Digital (Under);
   end Add_Label;

   procedure Changed
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Layer      : Digital_Ptr
             )  is
   begin
      Layer.Set_Value (Adjustment.Get_Value);
      if not Layer.Widget.Drawing and then Layer.Updated then
         Queue_Draw (Layer.Widget); -- Signal draw to the widget
      end if;
   end Changed;

   procedure Finalize (Layer : in out Digital_Layer) is
   begin
      Finalize (Label_Layer (Layer));
      if Layer.Adjustment /= null then
         Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   function Get_Absolute (Layer : Digital_Layer) return Boolean is
   begin
      return Layer.Absolute;
   end Get_Absolute;

   function Get_Adjustment (Layer : Digital_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Angle (Layer : Digital_Layer) return GDouble is
   begin
      return Label_Layer (Layer).Get_Angle;
   end Get_Angle;

   function Get_Base (Layer : Digital_Layer) return NumberBase is
   begin
      return Layer.Base;
   end Get_Base;

   function Get_Color (Layer : Digital_Layer) return Gdk_Color is
   begin
      return Label_Layer (Layer).Get_Color;
   end Get_Color;

   function Get_Face (Layer : Digital_Layer)
      return Pango_Cairo_Font is
   begin
      return Label_Layer (Layer).Get_Face;
   end Get_Face;

   function Get_Height (Layer : Digital_Layer) return GDouble is
   begin
      return Label_Layer (Layer).Get_Height;
   end Get_Height;

   function Get_Location (Layer : Digital_Layer) return Cairo_Tuple is
   begin
      return Label_Layer (Layer).Get_Location;
   end Get_Location;

   function Get_Mode (Layer : Digital_Layer)
      return Text_Transformation is
   begin
      return Label_Layer (Layer).Get_Mode;
   end Get_Mode;

   function Get_Precision (Layer : Digital_Layer) return Integer is
   begin
      return Layer.Precision;
   end Get_Precision;

   function Get_Properties_Number
            (  Layer : Digital_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Digital_Layer;
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
                     Blurb   => "The label text",
                     Flags   => Param_Readable
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
            when Property_Base =>
               return
                  Gnew_UInt
                  (  Name    => "base",
                     Nick    => "base",
                     Minimum => 2,
                     Maximum => 16,
                     Default => 10,
                     Blurb   => "The base used for the output"
                  );
            when Property_Precision =>
               return
                  Gnew_Int
                  (  Name    => "precision",
                     Nick    => "precision",
                     Minimum =>-1_000,
                     Maximum => 1_000,
                     Default => 0,
                     Blurb   => "The output precision relative or " &
                                "absolute"
                  );
            when Property_Absolute =>
               return
                  Gnew_Boolean
                  (  Name    => "absolute-precision",
                     Nick    => "absolute",
                     Default => True,
                     Blurb   => "The output precision is absolute " &
                                "when true and relative otherwise"
                  );
            when Property_Put_Plus =>
               return
                  Gnew_Boolean
                  (  Name    => "put-plus",
                     Nick    => "plus",
                     Default => False,
                     Blurb   => "The output renders plus " &
                                "when true and does not otherwise"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Digital_Layer;
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
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Base =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Layer.Base));
               when Property_Precision =>
                  Init (Value, GType_Int);
                  Set_Int (Value, GInt (Layer.Precision));
               when Property_Absolute =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Absolute);
               when Property_Put_Plus =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Put_Plus);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Put_Plus (Layer : Digital_Layer) return Boolean is
   begin
      return Layer.Put_Plus;
   end Get_Put_Plus;

   function Get_Stretch (Layer : Digital_Layer) return GDouble is
   begin
      return Label_Layer (Layer).Get_Stretch;
   end Get_Stretch;

   function Get_Skew (Layer : Digital_Layer) return GDouble is
   begin
      return Label_Layer (Layer).Get_Skew;
   end Get_Skew;

   function Get_Text (Layer : Digital_Layer) return UTF8_String is
   begin
      return Label_Layer (Layer).Get_Text;
   end Get_Text;

   function Get_Value (Layer : Digital_Layer) return GDouble is
   begin
      return Layer.Value;
   end Get_Value;

   function Render
            (  Layer  : Digital_Layer;
               Value  : GDouble
            )  return UTF8_String is
      use Gtk.Layered.Waveform.Edit;
   begin
      if Layer.Get_Absolute then
         return Image
                (  Value    => Value,
                   Base     => Layer.Get_Base,
                   PutPlus  => Layer.Get_Put_Plus,
                   AbsSmall => Layer.Get_Precision
                );
      else
         return Image
                (  Value    => Value,
                   Base     => Layer.Get_Base,
                   PutPlus  => Layer.Get_Put_Plus,
                   RelSmall => Layer.Get_Precision
                );
      end if;
   end Render;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Digital_Layer
             )  is
      Base       : GUInt;
      Precision  : GUInt;
      Adjustment : Boolean;
   begin
      Restore (Stream, Label_Layer (Layer));
      Restore (Stream, Base);
      Restore (Stream, Precision);
      Restore (Stream, Layer.Absolute, Layer.Put_Plus, Adjustment);
      if Base in 2..16 then
         Layer.Base := NumberBase (Base);
      else
         Layer.Base := 10;
      end if;
      if Precision > 2_000 then
         Precision := 2_000;
      end if;
      Layer.Precision := Integer (Precision) - 1_000;
      if not Layer.Absolute and then Layer.Precision <= 0 then
         Layer.Precision := 1;
      end if;
      if Adjustment then
         declare
            Adjustment : Gtk_Adjustment;
         begin
            Restore (Stream, Adjustment);
            Add_Adjustment (Layer, Adjustment);
         end;
      else
         declare
            Value : GDouble;
         begin
            Restore (Stream, Value);
            Set_Value (Layer, Value);
         end;
      end if;
   end Restore;

   procedure Set
             (  Layer     : in out Digital_Layer;
                Location  : Cairo_Tuple;
                Face      : Pango_Cairo_Font;
                Height    : GDouble;
                Stretch   : GDouble;
                Mode      : Text_Transformation;
                Color     : Gdk_Color;
                Angle     : GDouble;
                Skew      : GDouble;
                Base      : NumberBase;
                Precision : Integer;
                Absolute  : Boolean;
                Put_Plus  : Boolean
             )  is
   begin
      if not Absolute and then Precision <= 0 then
         raise Constraint_Error with
               "Relative precision is not positive";
      end if;
      if Precision not in -1_000..1_000 then
         raise Constraint_Error with
               "Precision is out of range";
      end if;
      Layer.Base      := Base;
      Layer.Precision := Precision;
      Layer.Absolute  := Absolute;
      Layer.Put_Plus  := Put_Plus;
      Label_Layer (Layer).Set
      (  Location => Location,
         Face     => Face,
         Height   => Height,
         Stretch  => Stretch,
         Mode     => Mode,
         Color    => Color,
         Angle    => Angle,
         Skew     => Skew
      );
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Digital_Layer;
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
                  Pango.Cairo.Fonts.Font_Type_Property.
                     Get_Enum (Value)
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
               null;
            when Property_Color =>
               Layer.Color := Get_Value (Value);
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
            when Property_Base =>
               declare
                  Base : constant GUInt := Get_UInt (Value);
               begin
                  if Base in 2..16 then
                     Layer.Base := NumberBase (Base);
                  end if;
               end;
            when Property_Precision =>
               Layer.Precision := Integer (Get_Int (Value));
               if not Layer.Absolute and then Layer.Precision <= 0 then
                  Layer.Precision := 1;
               end if;
            when Property_Absolute =>
               Layer.Absolute := Get_Boolean (Value);
               if not Layer.Absolute and then Layer.Precision <= 0 then
                  Layer.Precision := 1;
               end if;
            when Property_Put_Plus =>
               Layer.Put_Plus := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Value
             (  Layer : in out Digital_Layer;
                Value : GDouble
             )  is
   begin
      if Layer.Value /= Value then
         Layer.Value := Value;
         begin
            Layer.Set_Text
            (  Digital_Layer'Class (Layer).Render (Layer.Value)
            );
         exception
            when Error : others =>
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Warning,
                  (  "Fault rendering value: "
                  &  Exception_Information (Error)
                  &  Where ("Set_Value")
               )  );
         end;
      end if;
   end Set_Value;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Digital_Layer
             )  is
   begin
      Store (Stream, Label_Layer (Layer));
      Store (Stream, GUInt (Layer.Base));
      Store (Stream, GUInt (Layer.Precision + 1_000));
      Store
      (  Stream,
         Layer.Absolute,
         Layer.Put_Plus,
         Layer.Adjustment /= null
      );
      if Layer.Adjustment = null then
         Store (Stream, Layer.Value);
      else
         Store (Stream, Layer.Adjustment);
      end if;
   end Store;

end Gtk.Layered.Label.Digital;
