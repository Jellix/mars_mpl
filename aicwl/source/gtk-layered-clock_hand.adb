--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Clock_Hand                    Luebeck            --
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

with Ada.Unchecked_Deallocation;

with Cairo.Elementary_Functions;
with Cairo.Line_Cap_Property;

with Glib.Properties.Creation;

with Gtk.Layered.Stream_IO;

package body Gtk.Layered.Clock_Hand is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Center""");
   pragma Warnings (Off, "declaration hides ""Handlers""");

   Sqrt_2 : constant Gdouble := Cairo.Elementary_Functions.Sqrt (2.0);

   type Clock_Hand_Ptr is access all Clock_Hand_Layer;

   type Layer_Property is
     (Property_Scaled,
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
      Property_Bulb_Position,
      Property_Bulb_Radius,
      Property_Bulb_Width,
      Property_Color,
      Property_Value);

   package Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Clock_Hand_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Clock_Hand_Layer, Clock_Hand_Ptr);

   procedure Changed
     (Adjustment : access GObject_Record'Class;
      Needle     : Clock_Hand_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Clock_Hand_Layer
   is
      Ptr : Clock_Hand_Ptr := new Clock_Hand_Layer;
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
     (Layer      : in out Clock_Hand_Layer;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   procedure Add_Adjustment
     (Layer      : in out Clock_Hand_Layer;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Gtk.Adjustment.Ref (Adjustment);
      Layer.Adjustment := Adjustment.all'Unchecked_Access;
      Layer.Changed :=
        Handlers.Connect
          (Adjustment,
           "changed",
           Handlers.To_Marshaller (Changed'Access),
           Layer'Unchecked_Access);
      Layer.Value_Changed :=
        Handlers.Connect
          (Adjustment,
           "value_changed",
           Handlers.To_Marshaller (Changed'Access),
           Layer'Unchecked_Access);
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

   procedure Add_Clock_Hand
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      From          : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 4.0;
      Length        : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 2.0;
      Tip_Length    : Gdouble                                           := 20.0;
      Tip_Width     : Gdouble                                           := 2.0;
      Tip_Cap       : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length   : Gdouble                                           := 3.0;
      Rear_Width    : Gdouble                                           := 3.0;
      Rear_Cap      : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Bulb_Position : Gdouble                                           := 13.0;
      Bulb_Radius   : Gdouble                                           := 5.0;
      Bulb_Width    : Gdouble                                           := 2.0;
      Color         : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment    : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled        : Boolean                                           := False)
   is
      Ptr   : Clock_Hand_Ptr := new Clock_Hand_Layer;
      Layer : Clock_Hand_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Center        => Center,
         From          => From,
         Length        => Length,
         Tip           => (Length => Tip_Length,
                           Width  => Tip_Width,
                           Cap    => Tip_Cap),
         Rear          => (Length => Rear_Length,
                           Width  => Rear_Width,
                           Cap    => Rear_Cap),
         Bulb_Position => Bulb_Position,
         Bulb_Radius   => Bulb_Radius,
         Bulb_Width    => Bulb_Width,
         Color         => Color);
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Clock_Hand;

   function Add_Clock_Hand
     (Under         : not null access Layer_Location'Class;
      Center        : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      From          : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 4.0;
      Length        : Gdouble                                           := 3.0 * Ada.Numerics.Pi / 2.0;
      Tip_Length    : Gdouble                                           := 20.0;
      Tip_Width     : Gdouble                                           := 2.0;
      Tip_Cap       : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Rear_Length   : Gdouble                                           := 3.0;
      Rear_Width    : Gdouble                                           := 3.0;
      Rear_Cap      : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Bulb_Position : Gdouble                                           := 13.0;
      Bulb_Radius   : Gdouble                                           := 5.0;
      Bulb_Width    : Gdouble                                           := 2.0;
      Color         : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Adjustment    : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled        : Boolean                                           := False)
      return not null access Clock_Hand_Layer
   is
      Ptr   : Clock_Hand_Ptr := new Clock_Hand_Layer;
      Layer : Clock_Hand_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Center        => Center,
         From          => From,
         Length        => Length,
         Tip           => (Length => Tip_Length,
                           Width  => Tip_Width,
                           Cap    => Tip_Cap),
         Rear          => (Length => Rear_Length,
                           Width  => Rear_Width,
                           Cap    => Rear_Cap),
         Bulb_Position => Bulb_Position,
         Bulb_Radius   => Bulb_Radius,
         Bulb_Width    => Bulb_Width,
         Color         => Color);
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Clock_Hand;

   procedure Changed
     (Adjustment : access GObject_Record'Class;
      Needle     : Clock_Hand_Ptr)
   is
      pragma Unreferenced (Adjustment);
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
     (Layer   : in out Clock_Hand_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      Tip_Length     : Gdouble;
      Tip_Radius     : Gdouble;
      Rear_Length    : Gdouble;
      Rear_Radius    : Gdouble;
      Bulb_Position  : Gdouble;
      Bulb_Radius    : Gdouble;
      Bulb_Width     : Gdouble;
      X1, X2, Y1, Y2 : Gdouble;
   begin
      Cairo.New_Path (Context);
      declare
         State : Cairo.Ellipses.Context_State := Cairo.Ellipses.Save (Context);
         pragma Unreferenced (State);
      begin
         Cairo.Set_Source_Rgb
           (Context,
            Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last));
         if Layer.Scaled then
            declare
               Center : constant Cairo.Ellipses.Cairo_Tuple :=
                          Layer.Widget.all.Get_Center;
               Size   : constant Gdouble     := Layer.Widget.all.Get_Size;
            begin
               Tip_Length    := Size * Layer.Tip.Length;
               Tip_Radius    := Size * Layer.Tip.Width  / 2.0;
               Rear_Length   := Size * Layer.Rear.Length;
               Rear_Radius   := Size * Layer.Rear.Width / 2.0;
               Bulb_Position := Size * Layer.Bulb_Position;
               Bulb_Radius   := Size * Layer.Bulb_Radius;
               Bulb_Width    := Size * Layer.Bulb_Width;
               Cairo.Translate
                 (Context,
                  Center.X + Layer.Center.X * Size,
                  Center.Y + Layer.Center.Y * Size);
            end;
         else
            Tip_Length    := Layer.Tip.Length;
            Tip_Radius    := Layer.Tip.Width  / 2.0;
            Rear_Length   := Layer.Rear.Length;
            Rear_Radius   := Layer.Rear.Width / 2.0;
            Bulb_Position := Layer.Bulb_Position;
            Bulb_Radius   := Layer.Bulb_Radius;
            Bulb_Width    := Layer.Bulb_Width;
            Cairo.Translate (Context, Layer.Center.X, Layer.Center.Y);
         end if;
         Cairo.Rotate (Context, Layer.From + Layer.Value * Layer.Length);
         Cairo.Clip_Extents
           (Cr => Context,
            X1 => X1,
            X2 => X2,
            Y1 => Y1,
            Y2 => Y2);
         Cairo.Rectangle
           (Cr     => Context,
            X      => X1,
            Y      => Y1,
            Width  => X2 - X1,
            Height => Y2 - Y1);
         Cairo.Arc_Negative
           (Cr     => Context,
            Xc     => Bulb_Position,
            Yc     => 0.0,
            Radius => Bulb_Radius,
            Angle1 => 2.0 * Ada.Numerics.Pi,
            Angle2 => 0.0);
         Cairo.Close_Path (Context);
         declare
            Dummy : Cairo.Ellipses.Context_State := Cairo.Ellipses.Save (Context);
            pragma Unreferenced (Dummy);
         begin
            Cairo.Clip (Context);
            case Layer.Tip.Cap is
               when Cairo.Cairo_Line_Cap_Butt =>
                  Cairo.Move_To (Context, Tip_Length, -Tip_Radius);
                  Cairo.Line_To (Context, Tip_Length,  Tip_Radius);
               when Cairo.Cairo_Line_Cap_Round =>
                  declare
                     Angle : constant Gdouble :=
                               Cairo.Elementary_Functions.Arctan
                                 (X => Tip_Length + Rear_Length,
                                  Y => Tip_Radius - Rear_Radius);
                     Cos_Angle : constant Gdouble :=
                                   Cairo.Elementary_Functions.Cos (Angle);
                     Sin_Angle : constant Gdouble :=
                                   Cairo.Elementary_Functions.Sin (Angle);
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
                        Angle1 => Angle - Ada.Numerics.Pi / 2.0,
                        Angle2 => Ada.Numerics.Pi / 2.0 - Angle);
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
                  Cairo.Line_To (Context, Tip_Length,  Tip_Radius);
            end case;
            case Layer.Rear.Cap is
               when Cairo.Cairo_Line_Cap_Butt =>
                  Cairo.Line_To (Context, -Rear_Length,  Rear_Radius);
                  Cairo.Line_To (Context, -Rear_Length, -Rear_Radius);
               when Cairo.Cairo_Line_Cap_Round =>
                  declare
                     Angle : constant Gdouble :=
                               Cairo.Elementary_Functions.Arctan
                                 (X => Tip_Length + Rear_Length,
                                  Y => Rear_Radius - Tip_Radius);
                     Cos_Angle : constant Gdouble :=
                                   Cairo.Elementary_Functions.Cos (Angle);
                     Sin_Angle : constant Gdouble :=
                                   Cairo.Elementary_Functions.Sin (Angle);
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
                        Angle1 => Ada.Numerics.Pi / 2.0 - Angle,
                        Angle2 => Angle - Ada.Numerics.Pi / 2.0);
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
            Cairo.Fill (Context);
         end;
         Cairo.Set_Line_Width (Context, Bulb_Width);
         Cairo.Arc
           (Cr     => Context,
            Xc     => Bulb_Position,
            Yc     => 0.0,
            Radius => Bulb_Radius,
            Angle1 => 0.0,
            Angle2 => 2.0 * Ada.Numerics.Pi);
         Cairo.Stroke (Context);
      end;
      Layer.Updated := False;
   end Draw;

   overriding procedure Finalize (Layer : in out Clock_Hand_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Gtk.Handlers.Disconnect (Layer.Adjustment, Layer.Changed);
         Gtk.Handlers.Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Gtk.Adjustment.Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   overriding function Get_Adjustment
     (Layer : Clock_Hand_Layer) return Gtk.Adjustment.Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Bulb_Position (Layer : Clock_Hand_Layer)
      return Gdouble is
   begin
      return Layer.Bulb_Position;
   end Get_Bulb_Position;

   function Get_Bulb_Radius (Layer : Clock_Hand_Layer)
      return Gdouble is
   begin
      return Layer.Bulb_Radius;
   end Get_Bulb_Radius;

   function Get_Bulb_Width (Layer : Clock_Hand_Layer) return Gdouble is
   begin
      return Layer.Bulb_Width;
   end Get_Bulb_Width;

   function Get_Center (Layer : Clock_Hand_Layer)
      return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_Color (Layer : Clock_Hand_Layer) return Gdk.Color.Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_From (Layer : Clock_Hand_Layer) return Gdouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Clock_Hand_Layer) return Gdouble is
   begin
      return Layer.Length;
   end Get_Length;

   overriding function Get_Properties_Number
     (Layer : Clock_Hand_Layer) return Natural is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         -  Layer_Property'Pos (Layer_Property'First)
         +  1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Clock_Hand_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x",
                    Nick    => "x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The x-coordinate of the needle's center");
            when Property_Center_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y",
                    Nick    => "y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The y-coordinate of the needle's center");
            when Property_Value =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "value",
                    Nick    => "value",
                    Minimum => 0.0,
                    Maximum => 1.0,
                    Default => 0.0,
                    Blurb   => "The indicated value");
            when Property_From =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "from",
                    Nick    => "from",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angle of corresponding to the value 0");
            when Property_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "length",
                    Nick    => "length",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The length added to the value of " &
                       "the property from is the angle " &
                       "corresponding to the value 1");
            when Property_Tip_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "tip-length",
                    Nick    => "tip length",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The length of the needle's tip");
            when Property_Rear_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "rear-length",
                    Nick    => "rear length",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The length of the rear needle's end");
            when Property_Tip_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "tip-width",
                    Nick    => "tip width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The needle width at its tip");
            when Property_Rear_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "rear-width",
                    Nick    => "read width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The needle width at its rear end");
            when Property_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
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
            when Property_Bulb_Position =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "bulb-position",
                    Nick    => "bulb position",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 0.5,
                    Blurb   =>
                       "The position of the needle's bulb, " &
                       "its distance from the center " &
                       "towards the needle's tip");
            when Property_Bulb_Radius =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "bulb-radius",
                    Nick    => "bulb radius",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The radius of the needle's bulb. " &
                       "The radius corresponds to the " &
                       "center of the bulb's line");
            when Property_Bulb_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "bulb-width",
                    Nick    => "bulb width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The line width of the needle's bulb");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The needle size is changed when " &
                       "the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Clock_Hand_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Center_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Center.X);
               when Property_Center_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Center.Y);
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
               when Property_From =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From);
               when Property_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Length);
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
               when Property_Bulb_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Bulb_Width);
               when Property_Bulb_Position =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Bulb_Position);
               when Property_Bulb_Radius =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Bulb_Radius);
               when Property_Scaled =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Scaled);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Rear (Layer : Clock_Hand_Layer)
      return End_Parameters is
   begin
      return Layer.Rear;
   end Get_Rear;

   overriding function Get_Scaled (Layer : Clock_Hand_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Tip (Layer : Clock_Hand_Layer)
      return End_Parameters is
   begin
      return Layer.Tip;
   end Get_Tip;

   overriding function Get_Value (Layer : Clock_Hand_Layer) return Gdouble is
   begin
      return Layer.Value;
   end Get_Value;

   overriding function Is_Updated (Layer : Clock_Hand_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Clock_Hand_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Layer.Updated  := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Clock_Hand_Layer)
   is
      Center        : Cairo.Ellipses.Cairo_Tuple;
      From          : Gdouble;
      Length        : Gdouble;
      Tip           : End_Parameters;
      Rear          : End_Parameters;
      Bulb_Position : Gdouble;
      Bulb_Radius   : Gdouble;
      Bulb_Width    : Gdouble;
      Color         : Gdk.Color.Gdk_Color;
      Adjustment    : Boolean;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Center);
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, Length);
      Gtk.Layered.Stream_IO.Restore (Stream, Tip);
      Gtk.Layered.Stream_IO.Restore (Stream, Rear);
      Gtk.Layered.Stream_IO.Restore (Stream, Bulb_Position);
      Gtk.Layered.Stream_IO.Restore (Stream, Bulb_Radius);
      Gtk.Layered.Stream_IO.Restore (Stream, Bulb_Width);
      Gtk.Layered.Stream_IO.Restore (Stream, Color);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled, Adjustment);
      Set
        (Layer         => Layer,
         Center        => Center,
         From          => From,
         Length        => Length,
         Tip           => Tip,
         Rear          => Rear,
         Bulb_Position => Bulb_Position,
         Bulb_Radius   => Bulb_Radius,
         Bulb_Width    => Bulb_Width,
         Color         => Color);
      if Adjustment then
         declare
            Adjustment : Gtk.Adjustment.Gtk_Adjustment;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Adjustment);
            Add_Adjustment (Layer, Adjustment);
         end;
      else
         declare
            Value : Gdouble;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Value);
            Set_Value (Layer, Value);
         end;
      end if;
   end Restore;

   overriding procedure Scale
     (Layer  : in out Clock_Hand_Layer;
      Factor : Gdouble) is
   begin
      Set
        (Layer         => Layer,
         Center        => Layer.Center,
         From          => Layer.From,
         Length        => Layer.Length,
         Tip           => (Length => Layer.Tip.Length * Factor,
                           Width  => Layer.Tip.Width  * Factor,
                           Cap    => Layer.Tip.Cap),
         Rear          => (Length => Layer.Rear.Length * Factor,
                           Width  => Layer.Rear.Width  * Factor,
                           Cap    => Layer.Rear.Cap),
         Bulb_Position => Layer.Bulb_Position * Factor,
         Bulb_Radius   => Layer.Bulb_Radius   * Factor,
         Bulb_Width    => Layer.Bulb_Width    * Factor,
         Color         => Layer.Color);
   end Scale;

   procedure Set
     (Layer         : in out Clock_Hand_Layer;
      Center        : Cairo.Ellipses.Cairo_Tuple;
      From          : Gdouble;
      Length        : Gdouble;
      Tip           : End_Parameters;
      Rear          : End_Parameters;
      Bulb_Position : Gdouble;
      Bulb_Radius   : Gdouble;
      Bulb_Width    : Gdouble;
      Color         : Gdk.Color.Gdk_Color) is
   begin
      if Tip.Width < 0.0 then
         raise Constraint_Error with "Negative tip width";
      elsif Rear.Width < 0.0 then
         raise Constraint_Error with "Negative rear end width";
      elsif Tip.Length <= 0.0 then
         raise Constraint_Error with "Non-positive tip length";
      elsif -Rear.Length >= Tip.Length then
         raise Constraint_Error with "Non-positive needle length";
      elsif Bulb_Width < 0.0 then
         raise Constraint_Error with "Negative bulb width";
      elsif Bulb_Radius <= 0.0 then
         raise Constraint_Error with "Non-positive bulb radius";
      end if;
      Layer.Center        := Center;
      Layer.From          := From;
      Layer.Length        := Length;
      Layer.Tip           := Tip;
      Layer.Rear          := Rear;
      Layer.Bulb_Position := Bulb_Position;
      Layer.Bulb_Radius   := Bulb_Radius;
      Layer.Bulb_Width    := Bulb_Width;
      Layer.Color         := Color;
      Layer.Updated       := True;
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Clock_Hand_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Center.X := Glib.Values.Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Glib.Values.Get_Double (Value);
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
            when Property_From =>
               Layer.From := Glib.Values.Get_Double (Value);
               if
                 Layer.From not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.From :=
                    Gdouble'Remainder (Layer.From, 2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Length =>
               Layer.Length := Glib.Values.Get_Double (Value);
               if
                 Layer.Length not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Length :=
                     Gdouble'Remainder (Layer.Length, 2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Color =>
               Layer.Color := Gdk.Color.Get_Value (Value);
            when Property_Tip_Cap =>
               Layer.Tip.Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Rear_Cap =>
               Layer.Rear.Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Bulb_Width =>
               Layer.Bulb_Width := Glib.Values.Get_Double (Value);
               if Layer.Bulb_Width < 0.0 then
                  Layer.Bulb_Width := 0.0;
               end if;
            when Property_Bulb_Position =>
               Layer.Bulb_Position := Glib.Values.Get_Double (Value);
               if
                 Layer.Bulb_Position > Layer.Tip.Length - Layer.Bulb_Radius
               then
                  Layer.Bulb_Position :=
                     Layer.Tip.Length - Layer.Bulb_Radius;
               end if;
               if Layer.Bulb_Position < 0.0 then
                  Layer.Bulb_Position := 0.0;
               end if;
            when Property_Bulb_Radius =>
               Layer.Bulb_Position := Glib.Values.Get_Double (Value);
               if
                 Layer.Bulb_Position + Layer.Bulb_Radius > Layer.Tip.Length
               then
                  Layer.Bulb_Radius :=
                     Layer.Tip.Length - Layer.Bulb_Position;
               end if;
               if Layer.Bulb_Radius < 0.0 then
                  Layer.Bulb_Radius := 0.0;
               end if;
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Clock_Hand_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Value
     (Layer : in out Clock_Hand_Layer;
      Value : Gdouble) is
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
      Layer  : Clock_Hand_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Center);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.From);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Length);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Tip);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Rear);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Bulb_Position);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Bulb_Radius);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Bulb_Width);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Color);
      Gtk.Layered.Stream_IO.Store
        (Stream,
         Layer.Scaled,
         Layer.Adjustment /= null);
      if Layer.Adjustment = null then
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Value);
      else
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Adjustment);
      end if;
   end Store;

   pragma Warnings (On, "declaration hides ""Handlers""");
   pragma Warnings (On, "declaration hides ""Center""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Layered.Clock_Hand;
