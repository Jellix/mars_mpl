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
--____________________________________________________________________--

with Ada.Numerics;              use Ada.Numerics;
with Cairo.Transformations;     use Cairo.Transformations;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;     use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.Rectangular_Background is
   type Rectangular_Background_Ptr is
      access all Rectangular_Background_Layer;

   type Layer_Property is
        (  Property_Height,
           Property_Width,
           Property_Center_X,
           Property_Center_Y,
           Property_Angle,
           Property_Radius,
           Property_Color
        );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Rectangular_Background_Layer,
             Rectangular_Background_Ptr
          );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Rectangular_Background_Layer is
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
             (  Under          : not null access Layer_Location'Class;
                Height         : GDouble      := 1.0;
                Width          : GDouble      := 1.0;
                Center         : Cairo_Tuple := (0.0, 0.0);
                Rotation_Angle : GDouble      := 0.0;
                Corner_Radius  : GDouble      := 0.0;
                Color          : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Border_Width   : GDouble            := 0.0;
                Border_Depth   : GDouble            := 1.0;
                Border_Color   : Border_Color_Type := Default_Color;
                Border_Shadow  : Gtk_Shadow_Type   := Shadow_In;
                Deepened       : Boolean           := False;
                Scaled         : Boolean           := False;
                Widened        : Boolean           := False
             )  is
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
      (  Layer          => Layer,
         Height         => Height,
         Width          => Width,
         Center         => Center,
         Rotation_Angle => Rotation_Angle,
         Corner_Radius  => Corner_Radius,
         Color          => Color,
         Border_Width   => Border_Width,
         Border_Depth   => Border_Depth,
         Border_Color   => Border_Color,
         Border_Shadow  => Border_Shadow
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Rectangular_Background;

   function Add_Rectangular_Background
            (  Under          : not null access Layer_Location'Class;
               Height         : GDouble      := 1.0;
               Width          : GDouble      := 1.0;
               Center         : Cairo_Tuple := (0.0, 0.0);
               Rotation_Angle : GDouble      := 0.0;
               Corner_Radius  : GDouble      := 0.0;
               Color          : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Border_Width   : GDouble            := 0.0;
               Border_Depth   : GDouble            := 1.0;
               Border_Color   : Border_Color_Type := Default_Color;
               Border_Shadow  : Gtk_Shadow_Type   := Shadow_In;
               Deepened       : Boolean           := False;
               Scaled         : Boolean           := False;
               Widened        : Boolean           := False
            )  return not null access Rectangular_Background_Layer is
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
      (  Layer          => Layer,
         Height         => Height,
         Width          => Width,
         Center         => Center,
         Rotation_Angle => Rotation_Angle,
         Corner_Radius  => Corner_Radius,
         Color          => Color,
         Border_Width   => Border_Width,
         Border_Depth   => Border_Depth,
         Border_Color   => Border_Color,
         Border_Shadow  => Border_Shadow
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Rectangular_Background;

   procedure Draw_Contents
             (  Layer   : in out Rectangular_Background_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
   begin
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Layer.Color)) / GDouble (GUInt16'Last),
         GDouble (Green (Layer.Color)) / GDouble (GUInt16'Last),
         GDouble (Blue  (Layer.Color)) / GDouble (GUInt16'Last)
      );
      Cairo.Fill (Context);
   end Draw_Contents;

   function Get_Center (Layer : Rectangular_Background_Layer)
      return Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_Color (Layer : Rectangular_Background_Layer)
      return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Height (Layer : Rectangular_Background_Layer)
     return GDouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Corner_Radius (Layer : Rectangular_Background_Layer)
      return GDouble is
   begin
      return Layer.Radius;
   end Get_Corner_Radius;

   function Get_Properties_Number
            (  Layer : Rectangular_Background_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      +  Get_Properties_Number (Abstract_Bordered_Layer (Layer))
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Rectangular_Background_Layer;
               Property : Positive
            )  return Param_Spec is
      Inherited : constant Natural :=
         Get_Properties_Number (Abstract_Bordered_Layer (Layer));
   begin
      if Property <= Inherited then
         return
            Get_Property_Specification
            (  Abstract_Bordered_Layer (Layer),
               Property
            );
      elsif Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - Inherited - 1) is
            when Property_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The x-coordinate of the rectangle's center"
                  );
            when Property_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The y-coordinate of the rectangle's center"
                  );
            when Property_Height =>
               return
                  Gnew_Double
                  (  Name    => "height",
                     Nick    => "height",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The rectnagle's height"
                  );
            when Property_Width =>
               return
                  Gnew_Double
                  (  Name    => "width",
                     Nick    => "width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The rectnagle's width"
                  );
            when Property_Angle =>
               return
                  Gnew_Double
                  (  Name    => "angle",
                     Nick    => "angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the size " &
                                "corresponding to the width of the " &
                                "rectangle"
                  );
            when Property_Radius =>
               return
                  Gnew_Double
                  (  Name    => "corner-r",
                     Nick    => "corner r",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The radius of the circles rounding " &
                                "the corners of the rectangle"
                  );
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The background color"
                  );
         end case;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Rectangular_Background_Layer;
               Property : Positive
            )  return GValue is
      Inherited : constant Natural :=
         Get_Properties_Number (Abstract_Bordered_Layer (Layer));
   begin
      if Property <= Inherited then
         return
            Get_Property_Value
            (  Abstract_Bordered_Layer (Layer),
               Property
            );
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
               when Property_Height =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Height);
               when Property_Width =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Width);
               when Property_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Radius);
               when Property_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Angle);
               when Property_Color =>
                  Set_Value (Value, Layer.Color);
            end case;
            return Value;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Value;

   function Get_Rotation_Angle (Layer : Rectangular_Background_Layer)
      return GDouble is
   begin
      return Layer.Angle;
   end Get_Rotation_Angle;

   function Get_Width (Layer : Rectangular_Background_Layer)
      return GDouble is
   begin
      return Layer.Width;
   end Get_Width;

   procedure Move
             (  Layer  : in out Rectangular_Background_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Set_Updated (Layer);
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Rectangular_Background_Layer
             )  is
      Height : GDouble;
      Width  : GDouble;
      Center : Cairo_Tuple;
      Angle  : GDouble;
      Radius : GDouble;
      Color  : Gdk_Color;
   begin
      Restore (Stream, Height);
      Restore (Stream, Width);
      Restore (Stream, Center);
      Restore (Stream, Angle);
      Restore (Stream, Radius);
      Restore (Stream, Color);
      Restore (Stream, Abstract_Bordered_Layer (Layer));
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

   procedure Scale
             (  Layer  : in out Rectangular_Background_Layer;
                Factor : GDouble
             )  is
      Radius : constant GDouble := Layer.Radius * Factor;
      Height : constant GDouble := Layer.Height * Factor;
      Width  : constant GDouble := Layer.Width  * Factor;
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
      Set_Updated (Layer);
   end Scale;

   procedure Set
             (  Layer          : in out Rectangular_Background_Layer;
                Height         : GDouble;
                Width          : GDouble;
                Center         : Cairo_Tuple;
                Rotation_Angle : GDouble;
                Corner_Radius  : GDouble;
                Color          : Gdk_Color;
                Border_Width   : GDouble;
                Border_Depth   : GDouble;
                Border_Color   : Border_Color_Type;
                Border_Shadow  : Gtk_Shadow_Type
             )  is
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
      (  Layer         => Layer,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
   end Set;

   procedure Set_Contents_Path
             (  Layer   : in out Rectangular_Background_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Half_Width  : GDouble;
      Half_Height : GDouble;
      Radius      : GDouble;
      T           : Translate_And_Rotate;
   begin
      if Get_Scaled (Layer) then
         declare
            Center : constant Cairo_Tuple :=
                              Layer.Get_Widget.Get_Center;
            Size   : constant GDouble := Layer.Get_Widget.Get_Size;
         begin
            Translate
            (  T => T,
               X => Layer.Center.X * Size + Center.X,
               Y => Layer.Center.Y * Size + Center.Y
            );
            Half_Width  := Size * Layer.Width  / 2.0;
            Half_Height := Size * Layer.Height / 2.0;
            Radius      := Size * Layer.Radius;
         end;
      else
         Translate
         (  T => T,
            X => Layer.Center.X,
            Y => Layer.Center.Y
         );
         Half_Width  := Layer.Width  / 2.0;
         Half_Height := Layer.Height / 2.0;
         Radius      := Layer.Radius;
      end if;
      Rotate (T, Layer.Angle);
      Move_To (Context, T, Radius - Half_Width, -Half_Height);
      Line_To (Context, T, Half_Width - Radius, -Half_Height);
      if Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Half_Width - Radius,
            Y       => Radius - Half_Height,
            Radius  => Radius,
            Angle1  => 3.0 * Pi / 2.0,
            Angle2  => 2.0 * Pi
         );
      end if;
      Line_To (Context, T, Half_Width, Half_Height - Radius);
      if Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Half_Width  - Radius,
            Y       => Half_Height - Radius,
            Radius  => Radius,
            Angle1  => 0.0,
            Angle2  => Pi / 2.0
         );
      end if;
      Line_To (Context, T, Radius - Half_Width,  Half_Height);
      if Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Radius - Half_Width,
            Y       => Half_Height - Radius,
            Radius  => Radius,
            Angle1  => Pi / 2.0,
            Angle2  => Pi
         );
      end if;
      Line_To (Context, T, -Half_Width, Radius - Half_Height);
      if Radius > 0.0 then
         Arc
         (  Context => Context,
            T       => T,
            X       => Radius - Half_Width,
            Y       => Radius - Half_Height,
            Radius  => Radius,
            Angle1  => Pi,
            Angle2  => 3.0 * Pi /2.0
         );
      end if;
   end Set_Contents_Path;

   procedure Set_Property_Value
             (  Layer    : in out Rectangular_Background_Layer;
                Property : Positive;
                Value    : GValue
             )  is
      Inherited : constant Natural :=
         Get_Properties_Number (Abstract_Bordered_Layer (Layer));
   begin
      if Property <= Inherited then
         Set_Property_Value
         (  Abstract_Bordered_Layer (Layer),
            Property,
            Value
         );
      elsif Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - Inherited - 1) is
            when Property_Center_X =>
               Layer.Center.X := Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Get_Double (Value);
            when Property_Height =>
               Layer.Height := Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Width =>
               Layer.Width := Get_Double (Value);
               if Layer.Width < 0.0 then
                  Layer.Width := 0.0;
               end if;
            when Property_Radius =>
               Layer.Radius := Get_Double (Value);
               if Layer.Radius < 0.0 then
                  Layer.Radius := 0.0;
               elsif 2.0 * Layer.Radius > Layer.Height then
                  Layer.Radius := Layer.Height / 2.0;
               elsif 2.0 * Layer.Radius > Layer.Width then
                  Layer.Radius := Layer.Width / 2.0;
               end if;
            when Property_Angle =>
               Layer.Angle := Get_Double (Value);
               if Layer.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Angle :=
                     GDouble'Remainder (Layer.Angle, 2.0 * Pi);
               end if;
            when Property_Color =>
               Layer.Color := Get_Value (Value);
         end case;
         Set_Updated (Layer);
      else
         raise Constraint_Error;
      end if;
   end Set_Property_Value;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Rectangular_Background_Layer
             )  is
   begin
      Store (Stream, Layer.Height);
      Store (Stream, Layer.Width);
      Store (Stream, Layer.Center);
      Store (Stream, Layer.Angle);
      Store (Stream, Layer.Angle);
      Store (Stream, Layer.Radius);
      Store (Stream, Abstract_Bordered_Layer (Layer));
   end Store;

end Gtk.Layered.Rectangular_Background;
