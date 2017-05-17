--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Flat_Needle                     Luebeck            --
--  Implementation                                 Winter, 2011       --
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

with Ada.Numerics;                use Ada.Numerics;
with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with Glib.Properties.Creation;    use Glib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;

package body Gtk.Layered.Flat_Needle is

   Sqrt_2 : constant Gdouble := Sqrt (2.0);

   type Flat_Needle_Ptr is access all Flat_Needle_Layer;

   type Layer_Property is
     (Property_Scaled,
      Property_From_X,
      Property_From_Y,
      Property_To_X,
      Property_To_Y,
      Property_Tip_Length,
      Property_Tip_Width,
      Property_Tip_Cap,
      Property_Rear_Length,
      Property_Rear_Width,
      Property_Rear_Cap,
      Property_Color,
      Property_Value);

   package Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Flat_Needle_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Flat_Needle_Layer, Flat_Needle_Ptr);

   procedure Changed
     (Adjustment : access GObject_Record'Class;
      Needle     : Flat_Needle_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Flat_Needle_Layer
   is
      Ptr : Flat_Needle_Ptr := new Flat_Needle_Layer;
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
     (Layer      : in out Flat_Needle_Layer;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Gtk.Adjustment.Ref (Adjustment);
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
         Lower : constant Gdouble := Adjustment.all.Get_Lower;
         Upper : constant Gdouble := Adjustment.all.Get_Upper;
         Value : constant Gdouble := Adjustment.all.Get_Value;
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

   procedure Add_Flat_Needle
     (Under       : not null access Layer_Location'Class;
      From        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      Angle       : Gdouble                                           := 0.0;
      Length      : Gdouble                                           := 1.0;
      Tip_Length  : Gdouble                                           := 20.0;
      Tip_Width   : Gdouble                                           := 2.0;
      Tip_Cap     : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length : Gdouble                                           := 3.0;
      Rear_Width  : Gdouble                                           := 3.0;
      Rear_Cap    : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Color       : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled      : Boolean                                           := False)
   is
      Ptr   : Flat_Needle_Ptr := new Flat_Needle_Layer;
      Layer : Flat_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         From   => From,
         Angle  => Angle,
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
   end Add_Flat_Needle;

   procedure Add_Flat_Needle
     (Under       : not null access Layer_Location'Class;
      From        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      To          : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 1.0);
      Tip_Length  : Gdouble                                           := 20.0;
      Tip_Width   : Gdouble                                           := 2.0;
      Tip_Cap     : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length : Gdouble                                           := 3.0;
      Rear_Width  : Gdouble                                           := 3.0;
      Rear_Cap    : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Color       : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled      : Boolean                                           := False)
   is
      Ptr   : Flat_Needle_Ptr := new Flat_Needle_Layer;
      Layer : Flat_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer => Layer,
         From  => From,
         To    => To,
         Tip   => (  Length => Tip_Length,
                     Width  => Tip_Width,
                     Cap    => Tip_Cap
                  ),
         Rear  => (  Length => Rear_Length,
                     Width  => Rear_Width,
                     Cap    => Rear_Cap
                  ),
         Color => Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Flat_Needle;

   function Add_Flat_Needle
     (Under       : not null access Layer_Location'Class;
      From        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      Angle       : Gdouble                                           := 0.0;
      Length      : Gdouble                                           := 1.0;
      Tip_Length  : Gdouble                                           := 20.0;
      Tip_Width   : Gdouble                                           := 2.0;
      Tip_Cap     : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length : Gdouble                                           := 3.0;
      Rear_Width  : Gdouble                                           := 3.0;
      Rear_Cap    : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Color       : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled      : Boolean                                           := False)
      return not null access Flat_Needle_Layer
   is
      Ptr   : Flat_Needle_Ptr := new Flat_Needle_Layer;
      Layer : Flat_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         From   => From,
         Angle  => Angle,
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
   end Add_Flat_Needle;

   function Add_Flat_Needle
     (Under       : not null access Layer_Location'Class;
      From        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      To          : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 1.0);
      Tip_Length  : Gdouble                                           := 20.0;
      Tip_Width   : Gdouble                                           := 2.0;
      Tip_Cap     : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length : Gdouble                                           := 3.0;
      Rear_Width  : Gdouble                                           := 3.0;
      Rear_Cap    : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Color       : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment  : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled      : Boolean                                           := False)
      return not null access Flat_Needle_Layer
   is
      Ptr   : Flat_Needle_Ptr := new Flat_Needle_Layer;
      Layer : Flat_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer => Layer,
         From  => From,
         To    => To,
         Tip   => (  Length => Tip_Length,
                     Width  => Tip_Width,
                     Cap    => Tip_Cap
                  ),
         Rear  => (  Length => Rear_Length,
                     Width  => Rear_Width,
                     Cap    => Rear_Cap
                  ),
         Color => Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Flat_Needle;

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle     : Flat_Needle_Ptr
             )  is
      Lower : constant Gdouble := Gtk.Adjustment.Get_Lower (Needle.all.Adjustment);
      Upper : constant Gdouble := Gtk.Adjustment.Get_Upper (Needle.all.Adjustment);
      Value : constant Gdouble := Gtk.Adjustment.Get_Value (Needle.all.Adjustment);
   begin
      if Upper <= Lower or else Value <= Lower then
         Needle.all.Set_Value (0.0);
      elsif Value >= Upper then
         Needle.all.Set_Value (1.0);
      else
         Needle.all.Set_Value ((Value - Lower) / (Upper - Lower));
      end if;
      if not Needle.all.Widget.all.Drawing and then Needle.all.Updated then
         Queue_Draw (Needle.all.Widget); -- Signal draw to the widget
      end if;
   end Changed;

   overriding procedure Draw
     (Layer   : in out Flat_Needle_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      B : constant Gdouble := Layer.Value;
      A : constant Gdouble := 1.0 - B;
      Tip_Length  : Gdouble;
      Tip_Radius  : Gdouble;
      Rear_Length : Gdouble;
      Rear_Radius : Gdouble;
      State       : Cairo.Ellipses.Context_State :=
                      Cairo.Ellipses.Save (Context);
   begin
      Cairo.New_Path (Context);
      if Layer.Scaled then
         declare
            Center : constant Cairo.Ellipses.Cairo_Tuple :=
                       Layer.Widget.all.Get_Center;
            Size   : constant Gdouble      := Layer.Widget.all.Get_Size;
         begin
            Tip_Length  := Size * Layer.Tip.Length;
            Tip_Radius  := Size * Layer.Tip.Width  / 2.0;
            Rear_Length := Size * Layer.Rear.Length;
            Rear_Radius := Size * Layer.Rear.Width / 2.0;
            Cairo.Translate
              (Context,
               Center.X + (Layer.From.X * A + Layer.To.X * B) * Size,
               Center.Y + (Layer.From.Y * A + Layer.To.Y * B) * Size);
         end;
      else
         Tip_Length  := Layer.Tip.Length;
         Tip_Radius  := Layer.Tip.Width  / 2.0;
         Rear_Length := Layer.Rear.Length;
         Rear_Radius := Layer.Rear.Width / 2.0;
         Cairo.Translate
           (Context,
            Layer.From.X * A + Layer.To.X * B,
            Layer.From.Y * A + Layer.To.Y * B);
      end if;
      Cairo.Rotate (Context, Layer.Angle - Pi / 2.0);
      case Layer.Tip.Cap is
         when Cairo.Cairo_Line_Cap_Butt =>
            Cairo.Move_To (Context, Tip_Length, -Tip_Radius);
            Cairo.Line_To (Context, Tip_Length,  Tip_Radius);
         when Cairo.Cairo_Line_Cap_Round =>
            declare
               Angle : constant Gdouble :=
                          Arctan
                          (  X => Tip_Length + Rear_Length,
                             Y => Tip_Radius - Rear_Radius
                          );
               Cos_Angle : constant Gdouble := Cos (Angle);
               Sin_Angle : constant Gdouble := Sin (Angle);
            begin
               Cairo.Move_To
                 (Context,
                  Tip_Length + Tip_Radius * Sin_Angle,
                  -Tip_Radius * Cos_Angle);
               Cairo.Arc
                 (Cr     => Context,
                  Xc     => Tip_Length,
                  Yc     => 0.0,
                  Radius => Tip_Radius,
                  Angle1 => Angle - Pi / 2.0,
                  Angle2 => Pi / 2.0 - Angle);
               Cairo.Line_To
                 (Context,
                  Tip_Length + Tip_Radius * Sin_Angle,
                  Tip_Radius * Cos_Angle);
            end;
         when Cairo.Cairo_Line_Cap_Square =>
            Cairo.Move_To (Context, Tip_Length, -Tip_Radius);
            Cairo.Line_To
              (Context,
               Tip_Length + Tip_Radius * Sqrt_2,
               0.0);
            Cairo.Line_To (Context, Tip_Length, Tip_Radius);
      end case;
      case Layer.Rear.Cap is
         when Cairo.Cairo_Line_Cap_Butt =>
            Cairo.Line_To (Context, -Rear_Length,  Rear_Radius);
            Cairo.Line_To (Context, -Rear_Length, -Rear_Radius);
         when Cairo.Cairo_Line_Cap_Round =>
            declare
               Angle : constant Gdouble :=
                          Arctan
                          (  X => Tip_Length + Rear_Length,
                             Y => Rear_Radius - Tip_Radius
                          );
               Cos_Angle : constant Gdouble := Cos (Angle);
               Sin_Angle : constant Gdouble := Sin (Angle);
            begin
               Cairo.Line_To
                 (Context,
                  -Rear_Length - Rear_Radius * Sin_Angle,
                  Rear_Radius * Cos_Angle);
               Cairo.Arc
                 (Cr     => Context,
                  Xc     => -Rear_Length,
                  Yc     => 0.0,
                  Radius => Rear_Radius,
                  Angle1 => Pi / 2.0 - Angle,
                  Angle2 => Angle - Pi / 2.0);
               Cairo.Line_To
                 (Context,
                  -Rear_Length - Rear_Radius * Sin_Angle,
                  -Rear_Radius * Cos_Angle);
            end;
         when Cairo.Cairo_Line_Cap_Square =>
            Cairo.Line_To (Context, -Rear_Length, Rear_Radius);
            Cairo.Line_To
              (Context,
               -Rear_Length - Rear_Radius * Sqrt_2,
               0.0);
            Cairo.Line_To (Context, -Rear_Length, -Rear_Radius);
      end case;
      Cairo.Close_Path (Context);
      Cairo.Set_Source_Rgb
        (Context,
         Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last));
      Cairo.Fill (Context);
      Layer.Updated := False;
   end Draw;

   overriding procedure Finalize (Layer : in out Flat_Needle_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Disconnect (Layer.Adjustment, Layer.Changed);
         Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Gtk.Adjustment.Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   overriding function Get_Adjustment
     (Layer : Flat_Needle_Layer) return Gtk.Adjustment.Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Angle (Layer : Flat_Needle_Layer) return Gdouble is
   begin
      return Layer.Angle;
   end Get_Angle;

   function Get_Color (Layer : Flat_Needle_Layer) return Gdk.Color.Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_From
     (Layer : Flat_Needle_Layer) return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Flat_Needle_Layer) return Gdouble is
   begin
      return
         Sqrt
         (  (Layer.To.X - Layer.From.X)**2
         +  (Layer.To.Y - Layer.From.Y)**2
         );
   end Get_Length;

   overriding function Get_Properties_Number
            (  Layer : Flat_Needle_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   overriding function Get_Property_Specification
            (  Layer    : Flat_Needle_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_From_X =>
               return
                  Gnew_Double
                  (  Name    => "x0",
                     Nick    => "x0",
                     Minimum => Gdouble'First,
                     Maximum => Gdouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the point " &
                                "corresponding to the value 0"
                  );
            when Property_From_Y =>
               return
                  Gnew_Double
                  (  Name    => "y0",
                     Nick    => "y0",
                     Minimum => Gdouble'First,
                     Maximum => Gdouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the point " &
                                "corresponding to the value 0"
                  );
            when Property_To_X =>
               return
                  Gnew_Double
                  (  Name    => "x1",
                     Nick    => "x1",
                     Minimum => Gdouble'First,
                     Maximum => Gdouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the point " &
                                "corresponding to the value 1"
                  );
            when Property_To_Y =>
               return
                  Gnew_Double
                  (  Name    => "y1",
                     Nick    => "y1",
                     Minimum => Gdouble'First,
                     Maximum => Gdouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the point " &
                                "corresponding to the value 1"
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
            when Property_Tip_Length =>
               return
                  Gnew_Double
                  (  Name    => "tip-length",
                     Nick    => "tip length",
                     Minimum => 0.0,
                     Maximum => Gdouble'Last,
                     Default => 1.0,
                     Blurb   => "The length of the needle's tip"
                  );
            when Property_Rear_Length =>
               return
                  Gnew_Double
                  (  Name    => "rear-length",
                     Nick    => "rear length",
                     Minimum => Gdouble'First,
                     Maximum => Gdouble'Last,
                     Default => 0.0,
                     Blurb   => "The length of the rear needle's end"
                  );
            when Property_Tip_Width =>
               return
                  Gnew_Double
                  (  Name    => "tip-width",
                     Nick    => "tip width",
                     Minimum => 0.0,
                     Maximum => Gdouble'Last,
                     Default => 1.0,
                     Blurb   => "The needle width at its tip"
                  );
            when Property_Rear_Width =>
               return
                  Gnew_Double
                  (  Name    => "rear-width",
                     Nick    => "read width",
                     Minimum => 0.0,
                     Maximum => Gdouble'Last,
                     Default => 1.0,
                     Blurb   => "The needle width at its rear end"
                  );
            when Property_Color =>
               return
                 Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The needle color");
            when Property_Tip_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "tip-cap",
                    Nick    => "tip cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the needle's tip");
            when Property_Rear_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "rear-cap",
                    Nick    => "rear cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the needle's rear end");
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

   overriding function Get_Property_Value
     (Layer    : Flat_Needle_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_From_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From.X);
               when Property_From_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From.Y);
               when Property_To_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.To.X);
               when Property_To_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.To.Y);
               when Property_Value =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Value);
               when Property_Tip_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Tip.Length);
               when Property_Rear_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Rear.Length);
               when Property_Tip_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Tip.Width);
               when Property_Rear_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Rear.Width);
               when Property_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Color);
               when Property_Tip_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Tip.Cap);
               when Property_Rear_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Rear.Cap);
               when Property_Scaled =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Rear (Layer : Flat_Needle_Layer)
      return End_Parameters is
   begin
      return Layer.Rear;
   end Get_Rear;

   overriding function Get_Scaled (Layer : Flat_Needle_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Tip (Layer : Flat_Needle_Layer) return End_Parameters is
   begin
      return Layer.Tip;
   end Get_Tip;

   function Get_To
     (Layer : Flat_Needle_Layer) return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.To;
   end Get_To;

   overriding function Get_Value (Layer : Flat_Needle_Layer) return Gdouble is
   begin
      return Layer.Value;
   end Get_Value;

   overriding function Is_Updated (Layer : Flat_Needle_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Flat_Needle_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.From.X  := Layer.From.X + Offset.X;
      Layer.From.Y  := Layer.From.Y + Offset.Y;
      Layer.To.X    := Layer.To.X + Offset.X;
      Layer.To.Y    := Layer.To.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Flat_Needle_Layer)
   is
      From       : Cairo.Ellipses.Cairo_Tuple;
      To         : Cairo.Ellipses.Cairo_Tuple;
      Tip        : End_Parameters;
      Rear       : End_Parameters;
      Color      : Gdk.Color.Gdk_Color;
      Adjustment : Boolean;
   begin
      Restore (Stream, From);
      Restore (Stream, To);
      Restore (Stream, Tip);
      Restore (Stream, Rear);
      Restore (Stream, Color);
      Restore (Stream, Layer.Scaled, Adjustment);
      Set
      (  Layer => Layer,
         From  => From,
         To    => To,
         Tip   => Tip,
         Rear  => Rear,
         Color => Color
      );
      if Adjustment then
         declare
            Adjustment : Gtk.Adjustment.Gtk_Adjustment;
         begin
            Restore (Stream, Adjustment);
            Add_Adjustment (Layer, Adjustment);
         end;
      else
         declare
            Value : Gdouble;
         begin
            Restore (Stream, Value);
            Set_Value (Layer, Value);
         end;
      end if;
   end Restore;

   overriding procedure Scale
             (  Layer  : in out Flat_Needle_Layer;
                Factor : Gdouble
             )  is
      Tip_Length  : Gdouble := Layer.Tip.Length  * Factor;
      Rear_Length : Gdouble := Layer.Rear.Length * Factor;
   begin
      Set
      (  Layer  => Layer,
         From   => Layer.From,
         To     => Layer.To,
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
     (Layer  : in out Flat_Needle_Layer;
      From   : Cairo.Ellipses.Cairo_Tuple;
      Angle  : Gdouble;
      Length : Gdouble;
      Tip    : End_Parameters;
      Rear   : End_Parameters;
      Color  : Gdk.Color.Gdk_Color) is
   begin
      Set
      (  Layer => Layer,
         Color => Color,
         Rear  => Rear,
         Tip   => Tip,
         From  => From,
         To => (  X => From.X + Length * Cos (Angle),
                  Y => From.Y + Length * Sin (Angle)
      )        );
   end Set;

   procedure Set
     (Layer : in out Flat_Needle_Layer;
      From  : Cairo.Ellipses.Cairo_Tuple;
      To    : Cairo.Ellipses.Cairo_Tuple;
      Tip   : End_Parameters;
      Rear  : End_Parameters;
      Color : Gdk.Color.Gdk_Color) is
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
      Layer.Angle   := Arctan (X => To.X - From.X, Y => To.Y - From.Y);
      Layer.From    := From;
      Layer.To      := To;
      Layer.Tip     := Tip;
      Layer.Rear    := Rear;
      Layer.Color   := Color;
      Layer.Updated := True;
      Set_Value (Layer, Layer.Value);
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Flat_Needle_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_From_X =>
               Layer.From.X := Glib.Values.Get_Double (Value);
            when Property_From_Y =>
               Layer.From.Y := Glib.Values.Get_Double (Value);
            when Property_To_X =>
               Layer.To.X := Glib.Values.Get_Double (Value);
            when Property_To_Y =>
               Layer.To.Y := Glib.Values.Get_Double (Value);
            when Property_Value =>
               Set_Value (Layer, Glib.Values.Get_Double (Value));
            when Property_Tip_Width =>
               Layer.Tip.Width := Glib.Values.Get_Double (Value);
               if Layer.Tip.Width < 0.0 then
                  Layer.Tip.Width := 0.0;
               end if;
            when Property_Rear_Width =>
               Layer.Rear.Width := Glib.Values.Get_Double (Value);
               if Layer.Rear.Width < 0.0 then
                  Layer.Rear.Width := 0.0;
               end if;
            when Property_Tip_Length =>
               Layer.Tip.Length := Glib.Values.Get_Double (Value);
               if Layer.Tip.Length < 0.0 then
                  Layer.Tip.Length := 0.0;
               end if;
            when Property_Rear_Length =>
               Layer.Rear.Length := Glib.Values.Get_Double (Value);
               if -Layer.Rear.Length >= Layer.Tip.Length then
                  Layer.Rear.Length := -Layer.Tip.Length;
               end if;
            when Property_Color =>
               Layer.Color := Gdk.Color.Get_Value (Value);
            when Property_Tip_Cap =>
               Layer.Tip.Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Rear_Cap =>
               Layer.Rear.Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
             (  Layer  : in out Flat_Needle_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Value
             (  Layer : in out Flat_Needle_Layer;
                Value : Gdouble
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

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Flat_Needle_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Store (Stream, Layer.From);
      Store (Stream, Layer.To);
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

end Gtk.Layered.Flat_Needle;
