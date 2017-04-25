--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Abstract_Bordered               Luebeck            --
--  Implementation                                 Winter, 2010       --
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
--____________________________________________________________________--

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with Gdk.Color.IHLS;              use Gdk.Color.IHLS;
with Gdk.RGBA;                    use Gdk.RGBA;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Style_Context;           use Gtk.Style_Context;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;
with Gtk.Style;                   use Gtk.Style;

with Gtk.Enums.Shadow_Property;
with Gtk.Main.Router;

package body Gtk.Layered.Abstract_Bordered is

   type Drawing_Action is (Line, Region, Contents);

   Darken_By  : constant Gdk_Luminance := Gdk_Luminance'Last / 3;
   Lighten_By : constant Gdk_Luminance := Gdk_Luminance'Last / 3;
   Cos_45     : constant GDouble := sqrt (2.0) * 0.5;

   type Layer_Property is
        (  Property_Scaled,
           Property_Widened,
           Property_Default_Color,
           Property_Custom_Color,
           Property_Shadow,
           Property_Width,
           Property_Depth,
           Property_Aspected,
           Property_Deepened
        );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Foreground_Layer,
             Foreground_Layer_Ptr
          );

   procedure Add
             (  Layer : not null access Abstract_Bordered_Layer;
                Under : not null access Layer_Location'Class
             )  is
   begin
      Add (Abstract_Layer (Layer.all)'Unchecked_Access, Under);
      if Layer.Foreground = null then
         Layer.Foreground := new Foreground_Layer;
         Layer.Foreground.Border := Layer.all'Unchecked_Access;
         if Layer.Widget.Bottom = Layer.Next then
            -- The border is the topmost layer
            Add (Layer.Foreground, Layer.Widget);
         else
            -- Use the layer above it
            Add (Layer.Foreground, Layer.Next);
         end if;
      end if;
   end Add;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Foreground_Layer is
      Ptr : Foreground_Layer_Ptr := new Foreground_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Draw
             (  Layer   : in out Abstract_Bordered_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Background_Known : Boolean := False;
      Background_Color : Gdk_Color;

      function Get_Background return Gdk_Color is
      begin
         if not Background_Known then
            declare
               Widget : Gtk_Widget := Layer.Widget.all'Unchecked_Access;
               Parent : Gtk_Widget;
               Color  : Gdk_RGBA;
            begin
               loop
                  Get_Style_Context (Widget).Get_Background_Color
                  (  Gtk_State_Flag_Normal,
                     Color
                  );
                  exit when Color.Alpha > 0.0;
                  Parent := Widget.Get_Parent;
                  exit when Parent = null or else Parent = Widget;
                  Widget := Parent;
               end loop;
               Background_Color := From_RGBA (Color);
               Background_Known := True;
            end;
         end if;
        return Background_Color;
      end Get_Background;

      procedure Set_Dark is
         Color : Gdk_Color;
      begin
         if Layer.Border_Color.Style_Color then
            Color := Darken (Get_Background, Darken_By);
         else
            Color := Darken (Layer.Border_Color.Color, Darken_By);
         end if;
         Set_Source_RGB
         (  Context,
            GDouble (Red   (Color)) / GDouble (GUInt16'Last),
            GDouble (Green (Color)) / GDouble (GUInt16'Last),
            GDouble (Blue  (Color)) / GDouble (GUInt16'Last)
         );
      end Set_Dark;

      procedure Set_Light is
         Color : Gdk_Color;
      begin
         if Layer.Border_Color.Style_Color then
            Color := Lighten (Get_Background, Lighten_By);
         else
            Color := Lighten (Layer.Border_Color.Color, Lighten_By);
         end if;
         Set_Source_RGB
         (  Context,
            GDouble (Red   (Color)) / GDouble (GUInt16'Last),
            GDouble (Green (Color)) / GDouble (GUInt16'Last),
            GDouble (Blue  (Color)) / GDouble (GUInt16'Last)
         );
      end Set_Light;

      procedure Set_Normal is
         Color : Gdk_Color;
      begin
         if Layer.Border_Color.Style_Color then
            Color := Get_Background;
         else
            Color := Layer.Border_Color.Color;
         end if;
         Set_Source_RGB
         (  Context,
            GDouble (Red   (Color)) / GDouble (GUInt16'Last),
            GDouble (Green (Color)) / GDouble (GUInt16'Last),
            GDouble (Blue  (Color)) / GDouble (GUInt16'Last)
         );
      end Set_Normal;

      Shadow_Depth : GDouble; -- The width of the visible shadow
      Border_Width : GDouble;
      Extent       : GDouble;
      Scaling      : GDouble;
      Box_Center   : Cairo_Tuple;
      Path         : Cairo_Path_Access;
   --
   -- Draw -- Scaled path
   --
   --    Shift - The shift direction of the path (in line width)
   --    Bound - The desired outer size of the path
   --    Width - The width of the path's line (zero for areas)
   --
   --            Extent * FX + Width = Bound
   --            FX = (Bound - Width) / Extent
   --
      procedure Draw
                (  Shift, Bound, Width : GDouble;
                   Action : Drawing_Action := Line
                )  is
         F : constant GDouble := (Bound - Width) / Extent;
         S : constant GDouble := Shift * Shadow_Depth * 0.5;
         FX, FY : GDouble;
         State  : Context_State := Save (Context);
      begin
         if Layer.Aspected then
            -- Sized according to the widget's width
            FX := F;
            FY := (1.0 - 1.0 * Layer.Widget.Aspect_Ratio) + -- *Scaling+
                  F * Layer.Widget.Aspect_Ratio;
         else
            FX := F;
            FY := F;
         end if;
         New_Path (Context);
         Scale (Context, FX, FY);
         Translate
         (  Context,
            Box_Center.X * (1.0 / FX - 1.0) + S * Cos_45 / FX,
            Box_Center.Y * (1.0 / FY - 1.0) + S * Cos_45 / FY
         );
         Append_Path (Context, Path);
         Set_Line_Width (Context, Width / F);
         case Action is
            when Line =>
               Stroke (Context);
            when Region =>
               Cairo.Fill (Context);
            when Contents =>
               Draw_Contents
               (  Abstract_Bordered_Layer'Class (Layer),
                  Context,
                  Area
               );
         end case;
      end Draw;

      Outer_Size  : GDouble;
      Middle_Size : GDouble;
      Inner_Size  : GDouble;
      Box         : Cairo_Box := Get_Path_Extents (Context);
   begin
      if Layer.Foreground /= null then
         Layer.Foreground.Size := Layer.Widget.Size;
      end if;
      if Layer.Widened then
         Border_Width := Layer.Border_Width * Layer.Widget.Size;
      else
         Border_Width := Layer.Border_Width;
      end if;
      if Border_Width > 0.0 then
         if Layer.Deepened then
            Shadow_Depth := Layer.Border_Depth * Layer.Widget.Size;
         else
            Shadow_Depth := Layer.Border_Depth;
         end if;
      else
         Shadow_Depth := 0.0;
      end if;
      declare
         State : Context_State := Save (Context);
      begin
         New_Path (Context);
         Set_Contents_Path
         (  Abstract_Bordered_Layer'Class (Layer),
            Context,
            Area
         );
         Close_Path (Context);
         Box := Get_Path_Extents (Context);
--
--    Track the aspect ratio
--
--      Gtk.Main.Router.Trace
--      (  "Aspect ratio: Path ="
--      &  Double'Image (abs ((Box.X2 - Box.X1) / (Box.Y2 - Box.Y1)))
--      &  ", Widget ="
--      &  Double'Image
--         (  Double (Get_Allocation_Width  (Layer.Widget))
--         /  Double (Get_Allocation_Height (Layer.Widget))
--      )  );
         Extent := abs (Box.X2 - Box.X1);
--         Double'Max
--         (  abs (Box.X2 - Box.X1),
--            abs (Box.Y2 - Box.Y1)
--         );
         Box_Center.X := (Box.X2 + Box.X1) * 0.5;
         Box_Center.Y := (Box.Y2 + Box.Y1) * 0.5;
         if Layer.Scaled then
            -- Border inside the path's extent
            Outer_Size := Extent;
            Inner_Size := Extent - 2.0 * Border_Width;
            case Layer.Border_Shadow is
               when Shadow_None =>
                  Middle_Size := Outer_Size;
               when Shadow_In =>
                  Inner_Size  := Inner_Size - 2.0 * Shadow_Depth;
                  Middle_Size := Outer_Size;
               when Shadow_Out =>
                  Inner_Size  := Inner_Size - Shadow_Depth;
                  Middle_Size := Outer_Size - 2.0 * Shadow_Depth;
               when Shadow_Etched_In | Shadow_Etched_Out =>
                  Inner_Size  := Inner_Size - 4.0 * Shadow_Depth;
                  Middle_Size := Outer_Size - 2.0 * Shadow_Depth;
            end case;
            if Inner_Size > 0.0 then
               Layer.Widget.Size :=
                  Layer.Widget.Size * Inner_Size / Extent;
            end if;
         else
            -- Border around the path
            Inner_Size := Extent;
            Outer_Size := Extent + 2.0 * Border_Width;
            case Layer.Border_Shadow is
               when Shadow_None =>
                  Middle_Size := Outer_Size;
               when Shadow_In =>
                  Outer_Size  := Outer_Size + 2.0 * Shadow_Depth;
                  Middle_Size := Outer_Size;
               when Shadow_Out =>
                  Outer_Size  := Outer_Size + Shadow_Depth;
                  Middle_Size := Outer_Size - 2.0 * Shadow_Depth;
               when Shadow_Etched_In | Shadow_Etched_Out =>
                  Outer_Size  := Outer_Size + 2.0 * Shadow_Depth;
                  Middle_Size := Outer_Size - 2.0 * Shadow_Depth;
            end case;
         end if;
         if Inner_Size > 0.0 then
            Path := Copy_Path (Context);
            Scaling := Inner_Size / Extent;
            case Layer.Border_Shadow is
               when Shadow_None =>
                  if Border_Width > 0.0 then
                     Set_Normal;
                     Draw (0.0, Middle_Size, 0.0, Region);
                  end if;
               when Shadow_In =>
                  if Border_Width > 0.0 then
                     Set_Normal;
                     Draw (0.0, Middle_Size, 0.0, Region);
                  end if;
                  Set_Dark;
                  Draw (-1.0, Inner_Size + Shadow_Depth, Shadow_Depth);
                  Set_Light;
                  Draw ( 1.0, Inner_Size + Shadow_Depth, Shadow_Depth);
               when Shadow_Out =>
                  Set_Light;
                  Draw (-1.0, Outer_Size - Shadow_Depth, Shadow_Depth);
                  Set_Dark;
                  Draw ( 1.0, Outer_Size - Shadow_Depth, Shadow_Depth);
                  if Border_Width > 0.0 then
                     Set_Normal;
                     Draw (0.0, Middle_Size, 0.0, Region);
                  end if;
               when Shadow_Etched_In =>
                  Set_Dark;
                  Draw (-1.0, Outer_Size - Shadow_Depth, Shadow_Depth);
                  Set_Light;
                  Draw ( 1.0, Outer_Size - Shadow_Depth, Shadow_Depth);
                  if Border_Width > 0.0 then
                     Set_Normal;
                     Draw (0.0, Middle_Size, 0.0, Region);
                  end if;
                  Set_Light;
                  Draw (-1.0, Inner_Size + Shadow_Depth, Shadow_Depth);
                  Set_Dark;
                  Draw ( 1.0, Inner_Size + Shadow_Depth, Shadow_Depth);
               when Shadow_Etched_Out =>
                  Set_Light;
                  Draw (-1.0, Outer_Size - Shadow_Depth, Shadow_Depth);
                  Set_Dark;
                  Draw ( 1.0, Outer_Size - Shadow_Depth, Shadow_Depth);
                  if Border_Width > 0.0 then
                     Set_Normal;
                     Draw (0.0, Middle_Size, 0.0, Region);
                  end if;
                  Set_Dark;
                  Draw (-1.0, Inner_Size + Shadow_Depth, Shadow_Depth);
                  Set_Light;
                  Draw ( 1.0, Inner_Size + Shadow_Depth, Shadow_Depth);
            end case;
            Draw (0.0, Inner_Size, 0.0, Contents);
         end if;
         if Path /= null then
            Path_Destroy (Path);
         end if;
      end;
      Layer.Updated := False;
   exception
      when others =>
         if Path /= null then
            Path_Destroy (Path);
         end if;
         raise;
   end Draw;

   procedure Draw
             (  Layer   : in out Foreground_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
   begin
      Layer.Widget.Size := Layer.Size;
   end Draw;

   procedure Finalize (Layer : in out Abstract_Bordered_Layer) is
      Ptr : Foreground_Layer_Ptr := Layer.Foreground;
   begin
      if Ptr /= null then
         Free (Ptr); -- This also sets Layer.Foreground to null
      end if;
      Finalize (Abstract_Layer (Layer));
   end Finalize;

   procedure Finalize (Layer : in out Foreground_Layer) is
   begin
      if Layer.Border /= null then
         Layer.Border.Foreground := null;
         Layer.Border := null;
      end if;
      Finalize (Abstract_Layer (Layer));
   end Finalize;

   function Get_Aspected (Layer : Abstract_Bordered_Layer)
      return Boolean is
   begin
      return Layer.Aspected;
   end Get_Aspected;

   function Get_Border_Color (Layer : Abstract_Bordered_Layer)
      return Border_Color_Type is
   begin
      return Layer.Border_Color;
   end Get_Border_Color;

   function Get_Border_Depth (Layer : Abstract_Bordered_Layer)
      return GDouble is
   begin
      return Layer.Border_Depth;
   end Get_Border_Depth;

   function Get_Border_Shadow (Layer : Abstract_Bordered_Layer)
      return Gtk_Shadow_Type is
   begin
      return Layer.Border_Shadow;
   end Get_Border_Shadow;

   function Get_Border_Width (Layer : Abstract_Bordered_Layer)
      return GDouble is
   begin
      return Layer.Border_Width;
   end Get_Border_Width;

   function Get_Deepened (Layer : Abstract_Bordered_Layer)
      return Boolean is
   begin
      return Layer.Deepened;
   end Get_Deepened;

   function Get_Foreground (Layer : Abstract_Bordered_Layer)
      return access Foreground_Layer'Class is
   begin
      return Layer.Foreground;
   end Get_Foreground;

   function Get_Properties_Number
            (  Layer : Abstract_Bordered_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Properties_Number
            (  Layer : Foreground_Layer
            )  return Natural is
   begin
      return 0;
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Abstract_Bordered_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Default_Color =>
               return
                  Gnew_Boolean
                  (  Name    => "border-color-type",
                     Nick    => "border color type",
                     Default => True,
                     Blurb   => "If true the color is taken from the " &
                                "widget's styles. Otherwise the " &
                                "border-color property defines the " &
                                "color"
                  );
            when Property_Custom_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "border-color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "border color",
                     Blurb      => "The border color. This property " &
                                   "when written is ignored if the " &
                                   "property border-color-type is " &
                                   "set to true"
                  );
            when Property_Shadow =>
               return
                  Gtk.Enums.Shadow_Property.Gnew_Enum
                  (  Name    => "border-shadow-style",
                     Nick    => "border shadow",
                     Default => Shadow_None,
                     Blurb   => "The type of the border's shadow"
                  );
            when Property_Width =>
               return
                  Gnew_Double
                  (  Name    => "border-width",
                     Nick    => "border width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The border line width"
                  );
            when Property_Depth =>
               return
                  Gnew_Double
                  (  Name    => "border-depth",
                     Nick    => "border depth",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The visual width of the shadows " &
                                "dropped by the border"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The size of the contents inside " &
                                "the border is changed when the " &
                                "widget is resized"
                  );
            when Property_Deepened =>
               return
                  Gnew_Boolean
                  (  Name    => "deepened",
                     Nick    => "deepened",
                     Default => False,
                     Blurb   => "The width of the shadows dropped " &
                                "by the border is changed when " &
                                "the widget is resized"
                  );
            when Property_Aspected =>
               return
                  Gnew_Boolean
                  (  Name    => "aspected",
                     Nick    => "aspected",
                     Default => False,
                     Blurb   => "If is set to false, when the widget " &
                                "is resized the border width's " &
                                "aspect remains constant. The " &
                                "property has no visible effect " &
                                "when the widget's aspect ratio is " &
                                "1. For widgets having rectangular " &
                                "border this option should be true, " &
                                "otherwise vertical and horizontal " &
                                "borders would have different " &
                                "widths. The widgets having circular " &
                                "borders should have this set to false."
                  );
            when Property_Widened =>
               return
                  Gnew_Boolean
                  (  Name    => "widened",
                     Nick    => "widened",
                     Default => False,
                     Blurb   => "The border's line width is changed " &
                                "when the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Specification
            (  Layer    : Foreground_Layer;
               Property : Positive
            )  return Param_Spec is
      Result : Param_Spec;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Abstract_Bordered_Layer;
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
               when Property_Shadow =>
                  Gtk.Enums.Shadow_Property.Set_Enum
                  (  Value,
                     Layer.Border_Shadow
                  );
               when Property_Depth =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Border_Depth);
               when Property_Width =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Border_Width);
               when Property_Default_Color =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Border_Color.Style_Color);
               when Property_Custom_Color =>
                  if Layer.Border_Color.Style_Color then
                     declare
                        Color : Gdk_RGBA;
                     begin
                        Get_Style_Context
                        (  Layer.Widget
                        ) .Get_Background_Color
                           (  Gtk_State_Flag_Normal,
                              Color
                           );
                        Set_Value (Value, From_RGBA (Color));
                     end;
                  else
                     Set_Value (Value, Layer.Border_Color.Color);
                  end if;
               when Property_Aspected =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Aspected);
               when Property_Deepened =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Deepened);
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Widened =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Widened);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Property_Value
            (  Layer    : Foreground_Layer;
               Property : Positive
            )  return GValue is
      Result : GValue;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Value;

   function Get_Scaled (Layer : Abstract_Bordered_Layer)
      return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Widened (Layer : Abstract_Bordered_Layer)
      return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Is_Updated (Layer : Abstract_Bordered_Layer)
      return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   function Is_Updated (Layer : Foreground_Layer) return Boolean is
   begin
      return False;
   end Is_Updated;

   procedure Remove (Layer : in out Abstract_Bordered_Layer) is
   begin
      if Layer.Foreground /= null then
         Free (Layer.Foreground);
      end if;
      Remove (Abstract_Layer (Layer));
   end Remove;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Abstract_Bordered_Layer
             )  is
      Color      : Gdk_Color;
      Shadow     : Gtk_Shadow_Type;
      Width      : GDouble;
      Depth      : GDouble;
      User_Color : Boolean;
   begin
      Restore
      (  Stream,
         Layer.Aspected,
         Layer.Scaled,
         Layer.Widened,
         Layer.Deepened,
         User_Color
      );
      Restore (Stream, Shadow);
      Restore (Stream, Width);
      Restore (Stream, Depth);
      if User_Color then
         Set
         (  Layer         => Layer,
            Border_Width  => Width,
            Border_Depth  => Depth,
            Border_Color  => (Style_Color => True),
            Border_Shadow => Shadow
         );
      else
         Restore (Stream, Color);
         Set
         (  Layer         => Layer,
            Border_Width  => Width,
            Border_Depth  => Depth,
            Border_Color  => (Style_Color => False, Color => Color),
            Border_Shadow => Shadow
         );
      end if;
   end Restore;

   procedure Set
             (  Layer         : in out Abstract_Bordered_Layer;
                Border_Width  : GDouble;
                Border_Depth  : GDouble;
                Border_Color  : Border_Color_Type;
                Border_Shadow : Gtk_Shadow_Type
             )  is
   begin
      Layer.Border_Width  := Border_Width;
      Layer.Border_Depth  := Border_Depth;
      Layer.Border_Color  := Border_Color;
      Layer.Border_Shadow := Border_Shadow;
      Layer.Updated       := True;
   end Set;

   procedure Set_Aspected
             (  Layer    : in out Abstract_Bordered_Layer;
                Aspected : Boolean
             )  is
   begin
      Layer.Aspected := Aspected;
      Layer.Updated  := True;
   end Set_Aspected;

   procedure Set_Deepened
             (  Layer    : in out Abstract_Bordered_Layer;
                Deepened : Boolean
             )  is
   begin
      Layer.Deepened := Deepened;
      Layer.Updated  := True;
   end Set_Deepened;

   procedure Set_Property_Value
             (  Layer    : in out Abstract_Bordered_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Shadow =>
               Layer.Border_Shadow :=
                  Gtk.Enums.Shadow_Property.Get_Enum (Value);
            when Property_Depth =>
               Layer.Border_Width := Get_Double (Value);
               if Layer.Border_Depth < 0.0 then
                  Layer.Border_Depth := 0.0;
               end if;
            when Property_Width =>
               Layer.Border_Width := Get_Double (Value);
               if Layer.Border_Width < 0.0 then
                  Layer.Border_Width := 0.0;
               end if;
            when Property_Default_Color =>
               if Get_Boolean (Value) then
                  Layer.Border_Color := (Style_Color => True);
               else
                  declare
                     Color : Gdk_RGBA;
                  begin
                     Get_Style_Context
                     (  Layer.Widget
                     ) .Get_Background_Color
                        (  Gtk_State_Flag_Normal,
                           Color
                        );
                     Layer.Border_Color :=
                     (  Style_Color => False,
                        Color       => From_RGBA (Color)
                     );
                  end;
               end if;
            when Property_Custom_Color =>
               if not Layer.Border_Color.Style_Color then
                  Layer.Border_Color.Color := Get_Value (Value);
               end if;
            when Property_Aspected =>
               Layer.Aspected := Get_Boolean (Value);
            when Property_Deepened =>
               Layer.Deepened := Get_Boolean (Value);
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Property_Value
             (  Layer    : in out Foreground_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      raise Constraint_Error;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Abstract_Bordered_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Updated
             (  Layer : in out Abstract_Bordered_Layer'Class
             )  is
   begin
      Layer.Updated := True;
   end Set_Updated;

   procedure Set_Widened
             (  Layer   : in out Abstract_Bordered_Layer;
                Widened : Boolean
             )  is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Abstract_Bordered_Layer
             )  is
   begin
      Store
      (  Stream,
         Layer.Aspected,
         Layer.Scaled,
         Layer.Widened,
         Layer.Deepened,
         Layer.Border_Color.Style_Color
      );
      Store (Stream, Layer.Border_Shadow);
      Store (Stream, Layer.Border_Width);
      Store (Stream, Layer.Border_Depth);
      if not Layer.Border_Color.Style_Color then
         Store (Stream, Layer.Border_Color.Color);
      end if;
   end Store;

end Gtk.Layered.Abstract_Bordered;
