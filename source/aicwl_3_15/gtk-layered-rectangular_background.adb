--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Rectangular_Background          Luebeck            --
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
with Ada.Unchecked_Deallocation;

with Cairo.Transformations;

with Glib.Properties.Creation;

with Gtk.Layered.Stream_IO;

package body Gtk.Layered.Rectangular_Background is

   type Rectangular_Background_Ptr is
      access all Rectangular_Background_Layer;

   type Layer_Property is
     (Property_Height,
      Property_Width,
      Property_Center_X,
      Property_Center_Y,
      Property_Angle,
      Property_Radius,
      Property_Color);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Rectangular_Background_Layer,
        Rectangular_Background_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Rectangular_Background_Layer
   is
      Ptr : Rectangular_Background_Ptr :=
               new Rectangular_Background_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Rectangular_Background
     (Under          : not null access Layer_Location'Class;
      Height         : Gdouble                                         := 1.0;
      Width          : Gdouble                                         := 1.0;
      Center         : Cairo.Ellipses.Cairo_Tuple                      := (0.0, 0.0);
      Rotation_Angle : Gdouble                                         := 0.0;
      Corner_Radius  : Gdouble                                         := 0.0;
      Color          : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width   : Gdouble                                         := 0.0;
      Border_Depth   : Gdouble                                         := 1.0;
      Border_Color   : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow  : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened       : Boolean                                         := False;
      Scaled         : Boolean                                         := False;
      Widened        : Boolean                                         := False)
   is
      Ptr   : Rectangular_Background_Ptr :=
                 new Rectangular_Background_Layer;
      Layer : Rectangular_Background_Layer renames Ptr.all;
   begin
      Set_Aspected (Layer, True);
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer          => Layer,
         Height         => Height,
         Width          => Width,
         Center         => Center,
         Rotation_Angle => Rotation_Angle,
         Corner_Radius  => Corner_Radius,
         Color          => Color,
         Border_Width   => Border_Width,
         Border_Depth   => Border_Depth,
         Border_Color   => Border_Color,
         Border_Shadow  => Border_Shadow);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Rectangular_Background;

   function Add_Rectangular_Background
     (Under          : not null access Layer_Location'Class;
      Height         : Gdouble                                         := 1.0;
      Width          : Gdouble                                         := 1.0;
      Center         : Cairo.Ellipses.Cairo_Tuple                      := (0.0, 0.0);
      Rotation_Angle : Gdouble                                         := 0.0;
      Corner_Radius  : Gdouble                                         := 0.0;
      Color          : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width   : Gdouble                                         := 0.0;
      Border_Depth   : Gdouble                                         := 1.0;
      Border_Color   : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow  : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened       : Boolean                                         := False;
      Scaled         : Boolean                                         := False;
      Widened        : Boolean                                         := False)
      return not null access Rectangular_Background_Layer
   is
      Ptr   : Rectangular_Background_Ptr :=
                 new Rectangular_Background_Layer;
      Layer : Rectangular_Background_Layer renames Ptr.all;
   begin
      Set_Aspected (Layer, True);
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer          => Layer,
         Height         => Height,
         Width          => Width,
         Center         => Center,
         Rotation_Angle => Rotation_Angle,
         Corner_Radius  => Corner_Radius,
         Color          => Color,
         Border_Width   => Border_Width,
         Border_Depth   => Border_Depth,
         Border_Color   => Border_Color,
         Border_Shadow  => Border_Shadow);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Rectangular_Background;

   overriding procedure Draw_Contents
     (Layer   : in out Rectangular_Background_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
   begin
      Cairo.Set_Source_Rgb
        (Context,
         Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last));
      Cairo.Fill (Context);
   end Draw_Contents;

   function Get_Center (Layer : Rectangular_Background_Layer)
      return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_Color
     (Layer : Rectangular_Background_Layer) return Gdk.Color.Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Height (Layer : Rectangular_Background_Layer)
     return Gdouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Corner_Radius (Layer : Rectangular_Background_Layer)
      return Gdouble is
   begin
      return Layer.Radius;
   end Get_Corner_Radius;

   overriding function Get_Properties_Number
     (Layer : Rectangular_Background_Layer) return Natural is
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         -  Layer_Property'Pos (Layer_Property'First)
         +  1
         +  Gtk.Layered.Abstract_Bordered.Get_Properties_Number
              (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer)));
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Rectangular_Background_Layer;
      Property : Positive) return Param_Spec
   is
      Inherited : constant Natural :=
                    Gtk.Layered.Abstract_Bordered.Get_Properties_Number
                      (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer
                         (Layer));
   begin
      if Property <= Inherited then
         return
            Gtk.Layered.Abstract_Bordered.Get_Property_Specification
             (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer),
              Property);
      elsif Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - Inherited - 1) is
            when Property_Center_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x",
                    Nick    => "x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The x-coordinate of the rectangle's center");
            when Property_Center_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y",
                    Nick    => "y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The y-coordinate of the rectangle's center");
            when Property_Height =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "height",
                    Nick    => "height",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The rectangle's height");
            when Property_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "width",
                    Nick    => "width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The rectangle's width");
            when Property_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "angle",
                    Nick    => "angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The angle of the size " &
                       "corresponding to the width of the " &
                       "rectangle");
            when Property_Radius =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "corner-r",
                    Nick    => "corner r",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The radius of the circles rounding " &
                       "the corners of the rectangle");
            when Property_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The background color");
         end case;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Rectangular_Background_Layer;
      Property : Positive) return Glib.Values.GValue
   is
      Inherited : constant Natural :=
                    Gtk.Layered.Abstract_Bordered.Get_Properties_Number
                      (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer
                         (Layer));
   begin
      if Property <= Inherited then
         return
            Gtk.Layered.Abstract_Bordered.Get_Property_Value
             (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer),
              Property);
      elsif Property <= Get_Properties_Number (Layer) then
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - Inherited - 1) is
               when Property_Center_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Center.X);
               when Property_Center_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Center.Y);
               when Property_Height =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Height);
               when Property_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Width);
               when Property_Radius =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Radius);
               when Property_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Angle);
               when Property_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Color);
            end case;
            return Value;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Value;

   function Get_Rotation_Angle (Layer : Rectangular_Background_Layer)
      return Gdouble is
   begin
      return Layer.Angle;
   end Get_Rotation_Angle;

   function Get_Width (Layer : Rectangular_Background_Layer)
      return Gdouble is
   begin
      return Layer.Width;
   end Get_Width;

   overriding procedure Move
     (Layer  : in out Rectangular_Background_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Gtk.Layered.Abstract_Bordered.Set_Updated (Layer);
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Rectangular_Background_Layer)
   is
      Height : Gdouble;
      Width  : Gdouble;
      Center : Cairo.Ellipses.Cairo_Tuple;
      Angle  : Gdouble;
      Radius : Gdouble;
      Color  : Gdk.Color.Gdk_Color;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Height);
      Gtk.Layered.Stream_IO.Restore (Stream, Width);
      Gtk.Layered.Stream_IO.Restore (Stream, Center);
      Gtk.Layered.Stream_IO.Restore (Stream, Angle);
      Gtk.Layered.Stream_IO.Restore (Stream, Radius);
      Gtk.Layered.Stream_IO.Restore (Stream, Color);
      Gtk.Layered.Abstract_Bordered.Restore
        (Stream,
         Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer));
      if 2.0 * Radius > Height then
         raise Constraint_Error with
            "Corner radius is greater than the height";
      elsif 2.0 * Radius > Width then
         raise Constraint_Error with
            "Corner radius is greater than the width";
      elsif Height <= 0.0 then
         raise Constraint_Error with"Non-positive rectangle height";
      elsif Width <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle width";
      end if;
      Layer.Height := Height;
      Layer.Width  := Width;
      Layer.Center := Center;
      Layer.Angle  := Angle;
      Layer.Radius := Radius;
      Layer.Color  := Color;
   end Restore;

   overriding procedure Scale
     (Layer  : in out Rectangular_Background_Layer;
      Factor : Gdouble)
   is
      Radius : constant Gdouble := Layer.Radius * Factor;
      Height : constant Gdouble := Layer.Height * Factor;
      Width  : constant Gdouble := Layer.Width  * Factor;
   begin
      if 2.0 * Radius > Height then
         raise Constraint_Error with
            "Corner radius is greater then the height";
      elsif 2.0 * Radius > Width then
         raise Constraint_Error with
            "Corner radius is greater then the width";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle height";
      elsif Width <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle width";
      end if;
      Layer.Height := Height;
      Layer.Width  := Width;
      Layer.Radius := Radius;
      Gtk.Layered.Abstract_Bordered.Set_Updated (Layer);
   end Scale;

   procedure Set
     (Layer          : in out Rectangular_Background_Layer;
      Height         : Gdouble;
      Width          : Gdouble;
      Center         : Cairo.Ellipses.Cairo_Tuple;
      Rotation_Angle : Gdouble;
      Corner_Radius  : Gdouble;
      Color          : Gdk.Color.Gdk_Color;
      Border_Width   : Gdouble;
      Border_Depth   : Gdouble;
      Border_Color   : Gtk.Layered.Abstract_Bordered.Border_Color_Type;
      Border_Shadow  : Gtk.Enums.Gtk_Shadow_Type) is
   begin
      if 2.0 * Corner_Radius > Height then
         raise Constraint_Error with
            "Corner radius is greater than the height";
      elsif 2.0 * Corner_Radius > Width then
         raise Constraint_Error with
            "Corner radius is greater than the width";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle height";
      elsif Width <= 0.0 then
         raise Constraint_Error with "Non-positive rectangle width";
      end if;
      Layer.Height := Height;
      Layer.Width  := Width;
      Layer.Center := Center;
      Layer.Angle  := Rotation_Angle;
      Layer.Radius := Corner_Radius;
      Layer.Color  := Color;
      Set
        (Layer         => Layer,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
   end Set;

   overriding procedure Set_Contents_Path
     (Layer   : in out Rectangular_Background_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      Half_Width  : Gdouble;
      Half_Height : Gdouble;
      Radius      : Gdouble;
      T           : Cairo.Transformations.Translate_And_Rotate;
   begin
      if Get_Scaled (Layer) then
         declare
            Center : constant Cairo.Ellipses.Cairo_Tuple :=
                       Layer.Get_Widget.all.Get_Center;
            Size   : constant Gdouble := Layer.Get_Widget.all.Get_Size;
         begin
            Cairo.Transformations.Translate
              (T => T,
               X => Layer.Center.X * Size + Center.X,
               Y => Layer.Center.Y * Size + Center.Y);
            Half_Width  := Size * Layer.Width  / 2.0;
            Half_Height := Size * Layer.Height / 2.0;
            Radius      := Size * Layer.Radius;
         end;
      else
         Cairo.Transformations.Translate
           (T => T,
            X => Layer.Center.X,
            Y => Layer.Center.Y);
         Half_Width  := Layer.Width  / 2.0;
         Half_Height := Layer.Height / 2.0;
         Radius      := Layer.Radius;
      end if;
      Cairo.Transformations.Rotate (T, Layer.Angle);
      Cairo.Transformations.Move_To (Context, T, Radius - Half_Width, -Half_Height);
      Cairo.Transformations.Line_To (Context, T, Half_Width - Radius, -Half_Height);
      if Radius > 0.0 then
         Cairo.Transformations.Arc
           (Context => Context,
            T       => T,
            X       => Half_Width - Radius,
            Y       => Radius - Half_Height,
            Radius  => Radius,
            Angle1  => 3.0 * Ada.Numerics.Pi / 2.0,
            Angle2  => 2.0 * Ada.Numerics.Pi);
      end if;
      Cairo.Transformations.Line_To (Context, T, Half_Width, Half_Height - Radius);
      if Radius > 0.0 then
         Cairo.Transformations.Arc
           (Context => Context,
            T       => T,
            X       => Half_Width  - Radius,
            Y       => Half_Height - Radius,
            Radius  => Radius,
            Angle1  => 0.0,
            Angle2  => Ada.Numerics.Pi / 2.0);
      end if;
      Cairo.Transformations.Line_To (Context, T, Radius - Half_Width,  Half_Height);
      if Radius > 0.0 then
         Cairo.Transformations.Arc
           (Context => Context,
            T       => T,
            X       => Radius - Half_Width,
            Y       => Half_Height - Radius,
            Radius  => Radius,
            Angle1  => Ada.Numerics.Pi / 2.0,
            Angle2  => Ada.Numerics.Pi);
      end if;
      Cairo.Transformations.Line_To (Context, T, -Half_Width, Radius - Half_Height);
      if Radius > 0.0 then
         Cairo.Transformations.Arc
           (Context => Context,
            T       => T,
            X       => Radius - Half_Width,
            Y       => Radius - Half_Height,
            Radius  => Radius,
            Angle1  => Ada.Numerics.Pi,
            Angle2  => 3.0 * Ada.Numerics.Pi / 2.0);
      end if;
   end Set_Contents_Path;

   overriding procedure Set_Property_Value
     (Layer    : in out Rectangular_Background_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue)
   is
      Inherited : constant Natural :=
                    Gtk.Layered.Abstract_Bordered.Get_Properties_Number
                      (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer
                         (Layer));
   begin
      if Property <= Inherited then
         Gtk.Layered.Abstract_Bordered.Set_Property_Value
           (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer),
            Property,
            Value);
      elsif Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - Inherited - 1) is
            when Property_Center_X =>
               Layer.Center.X := Glib.Values.Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Glib.Values.Get_Double (Value);
            when Property_Height =>
               Layer.Height := Glib.Values.Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Width =>
               Layer.Width := Glib.Values.Get_Double (Value);
               if Layer.Width < 0.0 then
                  Layer.Width := 0.0;
               end if;
            when Property_Radius =>
               Layer.Radius := Glib.Values.Get_Double (Value);
               if Layer.Radius < 0.0 then
                  Layer.Radius := 0.0;
               elsif 2.0 * Layer.Radius > Layer.Height then
                  Layer.Radius := Layer.Height / 2.0;
               elsif 2.0 * Layer.Radius > Layer.Width then
                  Layer.Radius := Layer.Width / 2.0;
               end if;
            when Property_Angle =>
               Layer.Angle := Glib.Values.Get_Double (Value);
               if
                 Layer.Angle not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Angle :=
                     Gdouble'Remainder (Layer.Angle, 2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Color =>
               Layer.Color := Gdk.Color.Get_Value (Value);
         end case;
         Gtk.Layered.Abstract_Bordered.Set_Updated (Layer);
      else
         raise Constraint_Error;
      end if;
   end Set_Property_Value;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Rectangular_Background_Layer) is
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Height);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Width);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Center);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Angle);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Angle);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Radius);
      Gtk.Layered.Abstract_Bordered.Store
        (Stream,
         Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer));
   end Store;

end Gtk.Layered.Rectangular_Background;
