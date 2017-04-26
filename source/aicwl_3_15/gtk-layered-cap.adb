--                                                                    --
--  package Gtk.Layered.Cap         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2010       --
--                                                                    --
--                                Last revision :  07:54 21 Jul 2016  --
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

with Ada.Numerics;                use Ada.Numerics;
with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with Cairo.Pattern;               use Cairo.Pattern;
with Glib.Properties.Creation;    use Glib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.Cap is
   type Cap_Ptr is access all Cap_Layer;

   Cos_45 : constant Gdouble := Sqrt (2.0) * 0.5;

   type Layer_Property is
     (Property_Center_X,
      Property_Center_Y,
      Property_Radius,
      Property_From_Color,
      Property_To_Color);

   procedure Free is
     new Ada.Unchecked_Deallocation (Cap_Layer, Cap_Ptr);

   overriding
   function Add (Under  : not null access Layer_Location'Class;
                 Stream : not null access Root_Stream_Type'Class)
                 return not null access Cap_Layer
   is
      Ptr : Cap_Ptr := new Cap_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Cap
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo_Tuple               := (0.0, 0.0);
      Radius        : Gdouble                   := 1.0;
      From          : Gdk_Color                 := RGB (1.0, 1.0, 1.0);
      To            : Gdk_Color                 := RGB (0.5, 0.5, 0.5);
      Border_Width  : Gdouble                   := 0.0;
      Border_Depth  : Gdouble                   := 1.0;
      Border_Color  : Border_Color_Type         := Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_Out;
      Deepened      : Boolean                   := False;
      Scaled        : Boolean                   := False;
      Widened       : Boolean                   := False)
   is
      Ptr   : Cap_Ptr := new Cap_Layer;
      Layer : Cap_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set (Layer         => Layer,
           Center        => Center,
           Radius        => Radius,
           From          => From,
           To            => To,
           Border_Width  => Border_Width,
           Border_Depth  => Border_Depth,
           Border_Color  => Border_Color,
           Border_Shadow => Border_Shadow);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Cap;

   function Add_Cap
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo_Tuple               := (0.0, 0.0);
      Radius        : Gdouble                   := 1.0;
      From          : Gdk_Color                 := RGB (1.0, 1.0, 1.0);
      To            : Gdk_Color                 := RGB (0.5, 0.5, 0.5);
      Border_Width  : Gdouble                   := 0.0;
      Border_Depth  : Gdouble                   := 1.0;
      Border_Color  : Border_Color_Type         := Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_Out;
      Deepened      : Boolean                   := False;
      Scaled        : Boolean                   := False;
      Widened       : Boolean                   := False)
      return not null access Cap_Layer
   is
      Ptr   : Cap_Ptr := new Cap_Layer;
      Layer : Cap_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set (Layer         => Layer,
           Center        => Center,
           Radius        => Radius,
           From          => From,
           To            => To,
           Border_Width  => Border_Width,
           Border_Depth  => Border_Depth,
           Border_Color  => Border_Color,
           Border_Shadow => Border_Shadow);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Cap;

   procedure Create_Patterns (Layer : in out Cap_Layer);
   procedure Create_Patterns (Layer : in out Cap_Layer) is
   begin
      Layer.Pattern := Create_Linear (-Cos_45, -Cos_45, Cos_45, Cos_45);
      Add_Color_Stop_Rgb
        (Layer.Pattern,
         0.0,
         Gdouble (Red   (Layer.From)) / Gdouble (Guint16'Last),
         Gdouble (Green (Layer.From)) / Gdouble (Guint16'Last),
         Gdouble (Blue  (Layer.From)) / Gdouble (Guint16'Last));
      Add_Color_Stop_Rgb
      (Layer.Pattern,
         1.0,
         Gdouble (Red   (Layer.To)) / Gdouble (Guint16'Last),
         Gdouble (Green (Layer.To)) / Gdouble (Guint16'Last),
         Gdouble (Blue  (Layer.To)) / Gdouble (Guint16'Last)
      );
      Layer.Mask := Create_Radial (0.0, 0.0, 0.0,
                                   0.0, 0.0, 1.0);
      Add_Color_Stop_Rgba
        (Layer.Mask,
         0.0,
         0.0, 0.0, 0.0,
         1.0);
      Add_Color_Stop_Rgba
        (Layer.Mask,
         1.0,
         0.0, 0.0, 0.0,
         1.0);
      Add_Color_Stop_Rgba
        (Layer.Mask,
         Gdouble'Succ (1.0),
         0.0, 0.0, 0.0,
         0.0);
   end Create_Patterns;

   overriding
   procedure Draw_Contents (Layer   : in out Cap_Layer;
                            Context : Cairo_Context;
                            Area    : Gdk_Rectangle)
   is
      Box     : constant Cairo_Box := Get_Path_Extents (Context);
      Radius  : constant Gdouble   := 0.5 * (Box.X2 - Box.X1);
      Matrix  : aliased Cairo_Matrix;
   begin
      Get_Matrix (Context, Matrix'Access);
      Matrix.Xx := Radius;
--      Matrix.Xy := 0.0;
      Matrix.X0 := Matrix.X0 + 0.5 * (Box.X1 + Box.X2);
--      Matrix.Yx := 0.0;
      Matrix.Yy := Radius;
      Matrix.Y0 := Matrix.Y0 + 0.5 * (Box.Y1 + Box.Y2);
      Set_Matrix (Context, Matrix'Access);
      Set_Source (Context, Layer.Pattern);
      Mask (Context, Layer.Mask);
   end Draw_Contents;

   overriding
   procedure Finalize (Layer : in out Cap_Layer) is
   begin
      if Null_Pattern /= Layer.Mask then
         Destroy (Layer.Mask);
         Layer.Mask := Null_Pattern;
      end if;
      if Null_Pattern /= Layer.Pattern then
         Destroy (Layer.Pattern);
         Layer.Pattern := Null_Pattern;
      end if;
      Abstract_Bordered_Layer (Layer).Finalize;
   end Finalize;

   function Get_Center (Layer : Cap_Layer) return Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_From (Layer : Cap_Layer) return Gdk_Color is
   begin
      return Layer.From;
   end Get_From;

   overriding
   function Get_Properties_Number (Layer : Cap_Layer) return Natural is
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last) -
             Layer_Property'Pos (Layer_Property'First) +  1 +
             Get_Properties_Number (Abstract_Bordered_Layer (Layer)));
   end Get_Properties_Number;

   overriding
   function Get_Property_Specification (Layer    : Cap_Layer;
                                        Property : Positive) return Param_Spec
   is
      Inherited : constant Natural :=
         Get_Properties_Number (Abstract_Bordered_Layer (Layer));
   begin
      if Property <= Inherited then
         return
           Get_Property_Specification (Abstract_Bordered_Layer (Layer),
                                       Property);
      elsif Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - Inherited - 1) is
            when Property_Center_X =>
               return
                  Gnew_Double
                   (Name    => "x",
                    Nick    => "x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The x-coordinate of the cap's center");
            when Property_Center_Y =>
               return
                  Gnew_Double
                   (Name    => "outer-y",
                    Nick    => "outer y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The y-coordinate of the caps's center");
            when Property_Radius =>
               return
                  Gnew_Double
                   (Name    => "r",
                    Nick    => "r",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 0.5,
                    Blurb   => "The radius of the cap");
            when Property_From_Color =>
               return
                  Gnew_Boxed
                   (Name       => "from-color",
                    Boxed_Type => Gdk_Color_Type,
                    Nick       => "from color",
                    Blurb      => "The color in upper left corner of the cap");
            when Property_To_Color =>
               return
                  Gnew_Boxed
                   (Name       => "to-color",
                    Boxed_Type => Gdk_Color_Type,
                    Nick       => "to color",
                    Blurb      => "The color in lower right corner of the cap");
         end case;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Specification;

   overriding
   function Get_Property_Value (Layer    : Cap_Layer;
                                Property : Positive) return GValue
   is
      Inherited : constant Natural :=
         Get_Properties_Number (Abstract_Bordered_Layer (Layer));
   begin
      if Property <= Inherited then
         return Get_Property_Value (Abstract_Bordered_Layer (Layer), Property);
      elsif Property <= Get_Properties_Number (Layer) then
         declare
            Value : GValue;
         begin
            case Layer_Property'Val (Property - Inherited - 1) is
               when Property_Center_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.X);
               when Property_Center_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.Y);
               when Property_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Radius);
               when Property_From_Color =>
                  Set_Value (Value, Layer.From);
               when Property_To_Color =>
                  Set_Value (Value, Layer.To);
            end case;
            return Value;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Value;

   function Get_Radius (Layer : Cap_Layer) return Gdouble is
   begin
      return Layer.Radius;
   end Get_Radius;

   function Get_To (Layer : Cap_Layer) return Gdk_Color is
   begin
      return Layer.To;
   end Get_To;

   overriding
   procedure Move (Layer  : in out Cap_Layer;
                   Offset : Cairo_Tuple) is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Set_Updated (Layer);
   end Move;

   overriding
   procedure Restore (Stream : in out Root_Stream_Type'Class;
                      Layer  : in out Cap_Layer)
   is
      Center  : Cairo_Tuple;
      Radius  : Gdouble;
      From    : Gdk_Color;
      To      : Gdk_Color;
   begin
      Restore (Stream, Center);
      Restore (Stream, Radius);
      Restore (Stream, From);
      Restore (Stream, To);
      Restore (Stream, Abstract_Bordered_Layer (Layer));
      Layer.Center := Center;
      Layer.Radius := Radius;
      Layer.From   := From;
      Layer.To     := To;
      Create_Patterns (Layer);
   end Restore;

   overriding
   procedure Scale (Layer  : in out Cap_Layer;
                    Factor : Gdouble)
   is
      Radius : constant Gdouble := Layer.Radius * Factor;
   begin
      if Radius <= 0.0 then
         raise Constraint_Error with "Non-positive radius";
      end if;
      Layer.Radius := Radius;
      Set_Updated (Layer);
   end Scale;

   procedure Set (Layer         : in out Cap_Layer;
                  Center        : Cairo_Tuple;
                  Radius        : Gdouble;
                  From          : Gdk_Color;
                  To            : Gdk_Color;
                  Border_Width  : Gdouble;
                  Border_Depth  : Gdouble;
                  Border_Color  : Border_Color_Type;
                  Border_Shadow : Gtk.Enums.Gtk_Shadow_Type) is
   begin
      if Radius <= 0.0 then
         raise Constraint_Error with "Non-positive radius";
      end if;
      Layer.Center  := Center;
      Layer.Radius  := Radius;
      Layer.From    := From;
      Layer.To      := To;
      Create_Patterns (Layer);
      Set
        (Layer         => Layer,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
   end Set;

   overriding
   procedure Set_Contents_Path (Layer   : in out Cap_Layer;
                                Context : Cairo_Context;
                                Area    : Gdk_Rectangle) is
   begin
      if Get_Scaled (Layer) then
         declare
            Center : constant Cairo_Tuple :=
                              Layer.Get_Widget.all.Get_Center;
            Size   : constant Gdouble      := Layer.Get_Widget.all.Get_Size;
         begin
            Arc
              (Cr     => Context,
               Xc     => Layer.Center.X * Size + Center.X,
               Yc     => Layer.Center.Y * Size + Center.Y,
               Radius => Layer.Radius * Size,
               Angle1 => 0.0,
               Angle2 => 2.0 * Pi);
         end;
      else
         Arc
           (Cr     => Context,
            Xc     => Layer.Center.X,
            Yc     => Layer.Center.Y,
            Radius => Layer.Radius,
            Angle1 => 0.0,
            Angle2 => 2.0 * Pi);
      end if;
   end Set_Contents_Path;

   overriding
   procedure Set_Property_Value (Layer    : in out Cap_Layer;
                                 Property : Positive;
                                 Value    : GValue)
   is
      Inherited : constant Natural :=
         Get_Properties_Number (Abstract_Bordered_Layer (Layer));
   begin
      if Property <= Inherited then
         Set_Property_Value
           (Abstract_Bordered_Layer (Layer),
            Property,
            Value);
      elsif Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - Inherited - 1) is
            when Property_Center_X =>
               Layer.Center.X := Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Get_Double (Value);
            when Property_Radius =>
               Layer.Radius := Get_Double (Value);
               if Layer.Radius < 1.0E-6 then
                  Layer.Radius := 1.0E-6;
               end if;
            when Property_From_Color =>
               Layer.From := Get_Value (Value);
            when Property_To_Color =>
               Layer.To := Get_Value (Value);
         end case;
         Set_Updated (Layer);
      else
         raise Constraint_Error;
      end if;
   end Set_Property_Value;

   overriding
   procedure Store (Stream : in out Root_Stream_Type'Class;
                    Layer  : Cap_Layer) is
   begin
      Store (Stream, Layer.Center);
      Store (Stream, Layer.Radius);
      Store (Stream, Layer.From);
      Store (Stream, Layer.To);
      Store (Stream, Abstract_Bordered_Layer (Layer));
   end Store;

end Gtk.Layered.Cap;
