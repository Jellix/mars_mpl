--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Needle                          Luebeck            --
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

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;

package body Gtk.Layered.Needle is

   Sqrt_2 : constant GDouble := sqrt (2.0);

   type Needle_Ptr is access all Needle_Layer;

   type Layer_Property is
        (  Property_Scaled,
           Property_Center_X,
           Property_Center_Y,
           Property_From,
           Property_Length,
           Property_Tip_Length,
           Property_Tip_Width,
           Property_Tip_Cap,
           Property_Rear_Length,
           Property_Rear_Width,
           Property_Rear_Cap,
           Property_Color,
           Property_Value
        );

   package Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Needle_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Needle_Layer, Needle_Ptr);

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle      : Needle_Ptr
             );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Needle_Layer is
      Ptr : Needle_Ptr := new Needle_Layer;
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
             (  Layer      : in out Needle_Layer;
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
      declare
         Lower : constant GDouble := Adjustment.Get_Lower;
         Upper : constant GDouble := Adjustment.Get_Upper;
         Value : constant GDouble := Adjustment.Get_Value;
      begin
         if Upper <= Lower or else Value <= Lower then
            Layer.Set_Value (0.0);
         elsif Value >= Upper then
            Layer.Set_Value (1.0);
         else
            Layer.Set_Value ((Value - Lower) / (Upper - Lower));
         end if;
      end;
   end Add_Adjustment;

   procedure Add_Needle
             (  Under       : not null access Layer_Location'Class;
                Center      : Cairo_Tuple    := (0.0, 0.0);
                From        : GDouble        := 3.0 * Pi / 4.0;
                Length      : GDouble        := 3.0 * Pi / 2.0;
                Tip_Length  : GDouble        := 20.0;
                Tip_Width   : GDouble        := 2.0;
                Tip_Cap     : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
                Rear_Length : GDouble        := 3.0;
                Rear_Width  : GDouble        := 3.0;
                Rear_Cap    : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
                Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0);
                Adjustment  : access Gtk_Adjustment_Record'Class :=
                                     null;
                Scaled      : Boolean        := False
             )  is
      Ptr   : Needle_Ptr := new Needle_Layer;
      Layer : Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Center => Center,
         From   => From,
         Length => Length,
         Tip    => (  Length => Tip_Length,
                      Width  => Tip_Width,
                      Cap    => Tip_Cap
                   ),
         Rear   => (  Length => Rear_Length,
                      Width  => Rear_Width,
                      Cap    => Rear_Cap
                   ),
         Color  => Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Needle;

   function Add_Needle
            (  Under       : not null access Layer_Location'Class;
               Center      : Cairo_Tuple    := (0.0, 0.0);
               From        : GDouble        := 3.0 * Pi / 4.0;
               Length      : GDouble        := 3.0 * Pi / 2.0;
               Tip_Length  : GDouble        := 20.0;
               Tip_Width   : GDouble        := 2.0;
               Tip_Cap     : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
               Rear_Length : GDouble        := 3.0;
               Rear_Width  : GDouble        := 3.0;
               Rear_Cap    : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
               Color       : Gdk_Color      := RGB (1.0, 0.0, 0.0);
               Adjustment  : access Gtk_Adjustment_Record'Class := null;
               Scaled      : Boolean        := False
            )  return not null access Needle_Layer is
      Ptr   : Needle_Ptr := new Needle_Layer;
      Layer : Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Center => Center,
         From   => From,
         Length => Length,
         Tip    => (  Length => Tip_Length,
                      Width  => Tip_Width,
                      Cap    => Tip_Cap
                   ),
         Rear   => (  Length => Rear_Length,
                      Width  => Rear_Width,
                      Cap    => Rear_Cap
                   ),
         Color  => Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Needle;

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle      : Needle_Ptr
             )  is
      Lower : constant GDouble := Get_Lower (Needle.Adjustment);
      Upper : constant GDouble := Get_Upper (Needle.Adjustment);
      Value : constant GDouble := Get_Value (Needle.Adjustment);
   begin
      if Upper <= Lower or else Value <= Lower then
         Needle.Set_Value (0.0);
      elsif Value >= Upper then
         Needle.Set_Value (1.0);
      else
         Needle.Set_Value ((Value - Lower) / (Upper - Lower));
      end if;
      if not Needle.Widget.Drawing and then Needle.Updated then
         Queue_Draw (Needle.Widget); -- Signal draw to the widget
      end if;
   end Changed;

   procedure Draw
             (  Layer   : in out Needle_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Tip_Length  : GDouble;
      Tip_Radius  : GDouble;
      Rear_Length : GDouble;
      Rear_Radius : GDouble;
      State       : Context_State := Save (Context);
   begin
      New_Path (Context);
      if Layer.Scaled then
         declare
            Center : constant Cairo_Tuple := Layer.Widget.Get_Center;
            Size   : constant GDouble      := Layer.Widget.Get_Size;
         begin
            Tip_Length  := Size * Layer.Tip.Length;
            Tip_Radius  := Size * Layer.Tip.Width  / 2.0;
            Rear_Length := Size * Layer.Rear.Length;
            Rear_Radius := Size * Layer.Rear.Width / 2.0;
            Translate
            (  Context,
               Center.X + Layer.Center.X * Size,
               Center.Y + Layer.Center.Y * Size
            );
         end;
      else
         Tip_Length  := Layer.Tip.Length;
         Tip_Radius  := Layer.Tip.Width  / 2.0;
         Rear_Length := Layer.Rear.Length;
         Rear_Radius := Layer.Rear.Width / 2.0;
         Translate (Context, Layer.Center.X, Layer.Center.Y);
      end if;
      Rotate (Context, Layer.From + Layer.Value * Layer.Length);
      case Layer.Tip.Cap is
         when CAIRO_LINE_CAP_BUTT =>
            Move_To (Context, Tip_Length, -Tip_Radius);
            Line_To (Context, Tip_Length,  Tip_Radius);
         when CAIRO_LINE_CAP_ROUND =>
            declare
               Angle : constant GDouble :=
                          arctan
                          (  X => Tip_Length + Rear_Length,
                             Y => Tip_Radius - Rear_Radius
                          );
               Cos_Angle : constant GDouble := cos (Angle);
               Sin_Angle : constant GDouble := sin (Angle);
            begin
               Move_To
               (  Context,
                  Tip_Length + Tip_Radius * Sin_Angle,
                 -Tip_Radius * Cos_Angle
               );
               Arc
               (  Cr     => Context,
                  Xc     => Tip_Length,
                  Yc     => 0.0,
                  Radius => Tip_Radius,
                  Angle1 => Angle - Pi / 2.0,
                  Angle2 => Pi / 2.0 - Angle
               );
               Line_To
               (  Context,
                  Tip_Length + Tip_Radius * Sin_Angle,
                  Tip_Radius * Cos_Angle
               );
            end;
         when CAIRO_LINE_CAP_SQUARE =>
            Move_To (Context, Tip_Length, -Tip_Radius);
            Line_To
            (  Context,
               Tip_Length + Tip_Radius * Sqrt_2,
               0.0
            );
            Line_To (Context, Tip_Length,  Tip_Radius);
      end case;
      case Layer.Rear.Cap is
         when CAIRO_LINE_CAP_BUTT =>
            Line_To (Context, -Rear_Length,  Rear_Radius);
            Line_To (Context, -Rear_Length, -Rear_Radius);
         when CAIRO_LINE_CAP_ROUND =>
            declare
               Angle : constant GDouble :=
                          arctan
                          (  X => Tip_Length + Rear_Length,
                             Y => Rear_Radius - Tip_Radius
                          );
               Cos_Angle : constant GDouble := cos (Angle);
               Sin_Angle : constant GDouble := sin (Angle);
            begin
               Line_To
               (  Context,
                 -Rear_Length - Rear_Radius * Sin_Angle,
                  Rear_Radius * Cos_Angle
               );
               Arc
               (  Cr     => Context,
                  Xc     => -Rear_Length,
                  Yc     => 0.0,
                  Radius => Rear_Radius,
                  Angle1 => Pi / 2.0 - Angle,
                  Angle2 => Angle - Pi / 2.0
               );
               Line_To
               (  Context,
                 -Rear_Length - Rear_Radius * Sin_Angle,
                 -Rear_Radius * Cos_Angle
               );
            end;
         when CAIRO_LINE_CAP_SQUARE =>
            Line_To (Context, -Rear_Length, Rear_Radius);
            Line_To
            (  Context,
              -Rear_Length - Rear_Radius * Sqrt_2,
               0.0
            );
            Line_To (Context, -Rear_Length, -Rear_Radius);
      end case;
      Close_Path (Context);
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Green (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Blue  (Layer.Color)) / GDouble (Guint16'Last)
      );
      Fill (Context);
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out Needle_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Disconnect (Layer.Adjustment, Layer.Changed);
         Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   function Get_Adjustment (Layer : Needle_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Center (Layer : Needle_Layer) return Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_Color (Layer : Needle_Layer) return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_From (Layer : Needle_Layer) return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Needle_Layer) return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Properties_Number
            (  Layer : Needle_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Needle_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the needle's " &
                                "center"
                  );
            when Property_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the needle's " &
                                "center"
                  );
            when Property_Value =>
               return
                  Gnew_Double
                  (  Name    => "value",
                     Nick    => "value",
                     Minimum => 0.0,
                     Maximum => 1.0,
                     Default => 0.0,
                     Blurb   => "The indicated value"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of corresponding to the " &
                                "value 0"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The length added to the value of " &
                                "the property from is the angle " &
                                "coresponding to the value 1"
                  );
            when Property_Tip_Length =>
               return
                  Gnew_Double
                  (  Name    => "tip-length",
                     Nick    => "tip length",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The length of the needle's tip"
                  );
            when Property_Rear_Length =>
               return
                  Gnew_Double
                  (  Name    => "rear-length",
                     Nick    => "rear length",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The length of the rear needle's end"
                  );
            when Property_Tip_Width =>
               return
                  Gnew_Double
                  (  Name    => "tip-width",
                     Nick    => "tip width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The needle width at its tip"
                  );
            when Property_Rear_Width =>
               return
                  Gnew_Double
                  (  Name    => "rear-width",
                     Nick    => "read width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The needle width at its rear end"
                  );
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The needle color"
                  );
            when Property_Tip_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                  (  Name    => "tip-cap",
                     Nick    => "tip cap",
                     Default => CAIRO_LINE_CAP_BUTT,
                     Blurb   => "The cap style of the needle's tip"
                  );
            when Property_Rear_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                  (  Name    => "rear-cap",
                     Nick    => "rear cap",
                     Default => CAIRO_LINE_CAP_BUTT,
                     Blurb   => "The cap style of the needle's rear end"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The needle size is changed when " &
                                "the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Needle_Layer;
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
               when Property_Center_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.X);
               when Property_Center_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Center.Y);
               when Property_Value =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Value);
               when Property_Tip_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Tip.Length);
               when Property_Rear_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Rear.Length);
               when Property_Tip_Width =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Tip.Width);
               when Property_Rear_Width =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Rear.Width);
               when Property_From =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From);
               when Property_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Length);
               when Property_Color =>
                  Set_Value (Value, Layer.Color);
               when Property_Tip_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                  (  Value,
                     Layer.Tip.Cap
                  );
               when Property_Rear_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                  (  Value,
                     Layer.Rear.Cap
                  );
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Rear (Layer : Needle_Layer) return End_Parameters is
   begin
      return Layer.Rear;
   end Get_Rear;

   function Get_Scaled (Layer : Needle_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Tip (Layer : Needle_Layer) return End_Parameters is
   begin
      return Layer.Tip;
   end Get_Tip;

   function Get_Value (Layer : Needle_Layer) return GDouble is
   begin
      return Layer.Value;
   end Get_Value;

   function Is_Updated (Layer : Needle_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Needle_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Layer.Updated  := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Needle_Layer
             )  is
      Center     : Cairo_Tuple;
      From       : GDouble;
      Length     : GDouble;
      Tip        : End_Parameters;
      Rear       : End_Parameters;
      Color      : Gdk_Color;
      Adjustment : Boolean;
   begin
      Restore (Stream, Center);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Tip);
      Restore (Stream, Rear);
      Restore (Stream, Color);
      Restore (Stream, Layer.Scaled, Adjustment);
      Set
      (  Layer  => Layer,
         Center => Center,
         From   => From,
         Length => Length,
         Tip    => Tip,
         Rear   => Rear,
         Color  => Color
      );
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

   procedure Scale
             (  Layer  : in out Needle_Layer;
                Factor : GDouble
             )  is
      Tip_Length  : GDouble := Layer.Tip.Length  * Factor;
      Rear_Length : GDouble := Layer.Rear.Length * Factor;
   begin
      Set
      (  Layer  => Layer,
         Center => Layer.Center,
         From   => Layer.From,
         Length => Layer.Length,
         Tip    => (  Length => Layer.Tip.Length * Factor,
                      Width  => Layer.Tip.Width  * Factor,
                      Cap    => Layer.Tip.Cap
                   ),
         Rear   => (  Length => Layer.Rear.Length * Factor,
                      Width  => Layer.Rear.Width  * Factor,
                      Cap    => Layer.Rear.Cap
                   ),
         Color  => Layer.Color
      );
   end Scale;

   procedure Set
             (  Layer  : in out Needle_Layer;
                Center : Cairo_Tuple;
                From   : GDouble;
                Length : GDouble;
                Tip    : End_Parameters;
                Rear   : End_Parameters;
                Color  : Gdk_Color
             )  is
   begin
      if Tip.Width < 0.0 then
         raise Constraint_Error with "Negative tip width";
      elsif Rear.Width < 0.0 then
         raise Constraint_Error with "Negative rear end width";
      elsif Tip.Length <= 0.0 then
         raise Constraint_Error with "Non-positive tip length";
      elsif -Rear.Length >= Tip.Length then
         raise Constraint_Error with "Non-positive arrow length";
      end if;
      Layer.Center  := Center;
      Layer.From    := From;
      Layer.Length  := Length;
      Layer.Tip     := Tip;
      Layer.Rear    := Rear;
      Layer.Color   := Color;
      Layer.Updated := True;
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Needle_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Center.X := Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Get_Double (Value);
            when Property_Value =>
               Set_Value (Layer, Get_Double (Value));
            when Property_Tip_Width =>
               Layer.Tip.Width := Get_Double (Value);
               if Layer.Tip.Width < 0.0 then
                  Layer.Tip.Width := 0.0;
               end if;
            when Property_Rear_Width =>
               Layer.Rear.Width := Get_Double (Value);
               if Layer.Rear.Width < 0.0 then
                  Layer.Rear.Width := 0.0;
               end if;
            when Property_Tip_Length =>
               Layer.Tip.Length := Get_Double (Value);
               if Layer.Tip.Length < 0.0 then
                  Layer.Tip.Length := 0.0;
               end if;
            when Property_Rear_Length =>
               Layer.Rear.Length := Get_Double (Value);
               if -Layer.Rear.Length >= Layer.Tip.Length then
                  Layer.Rear.Length := -Layer.Tip.Length;
               end if;
            when Property_From =>
               Layer.From := Get_Double (Value);
               if Layer.From not in -2.0 * Pi..2.0 * Pi then
                  Layer.From := GDouble'Remainder (Layer.From, 2.0 * Pi);
               end if;
            when Property_Length =>
               Layer.Length := Get_Double (Value);
               if Layer.Length not in -2.0 * Pi..2.0 * Pi then
                  Layer.Length :=
                     GDouble'Remainder (Layer.Length, 2.0 * Pi);
               end if;
            when Property_Color =>
               Layer.Color := Get_Value (Value);
            when Property_Tip_Cap =>
               Layer.Tip.Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Rear_Cap =>
               Layer.Rear.Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Needle_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Value
             (  Layer : in out Needle_Layer;
                Value : GDouble
             )  is
   begin
      if Value <= 0.0 then
         if Layer.Value /= 0.0 then
            Layer.Value   := 0.0;
            Layer.Updated := True;
         end if;
      elsif Value >= 1.0 then
         if Layer.Value /= 1.0 then
            Layer.Value   := 1.0;
            Layer.Updated := True;
         end if;
      else
         if Layer.Value /= Value then
            Layer.Value   := Value;
            Layer.Updated := True;
         end if;
      end if;
   end Set_Value;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Needle_Layer
             )  is
   begin
      Store (Stream, Layer.Center);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Tip);
      Store (Stream, Layer.Rear);
      Store (Stream, Layer.Color);
      Store (Stream, Layer.Scaled, Layer.Adjustment /= null);
      if Layer.Adjustment = null then
         Store (Stream, Layer.Value);
      else
         Store (Stream, Layer.Adjustment);
      end if;
   end Store;

end Gtk.Layered.Needle;
