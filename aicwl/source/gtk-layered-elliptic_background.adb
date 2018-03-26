--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Background             Luebeck            --
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

with Cairo;

with Glib.Properties.Creation;

with Gtk.Layered.Elliptic_Shape_Property;
with Gtk.Layered.Stream_IO;

package body Gtk.Layered.Elliptic_Background is
   type Elliptic_Background_Ptr is access Elliptic_Background_Layer;

   type Layer_Property is
     (Property_Outer_Center_X,
      Property_Outer_Center_Y,
      Property_Outer_Curvature,
      Property_Outer_Radius,
      Property_Outer_Angle,
      Property_Shape,
      Property_Inner_Center_X,
      Property_Inner_Center_Y,
      Property_Inner_Curvature,
      Property_Inner_Radius,
      Property_Inner_Angle,
      Property_From,
      Property_Length,
      Property_Color);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Elliptic_Background_Layer,
        Elliptic_Background_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Elliptic_Background_Layer
   is
      Ptr : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Elliptic_Background
     (Under         : not null access Layer_Location'Class;
      Outer         : Cairo.Ellipses.Ellipse_Parameters               := Cairo.Ellipses.Unit_Circle;
      Inner         : Cairo.Ellipses.Ellipse_Parameters;
      From          : Gdouble                                         := 0.0;
      Length        : Gdouble                                         := 2.0 * Ada.Numerics.Pi;
      Color         : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width  : Gdouble                                         := 0.0;
      Border_Depth  : Gdouble                                         := 1.0;
      Border_Color  : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened      : Boolean                                         := False;
      Scaled        : Boolean                                         := False;
      Widened       : Boolean                                         := False)
   is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Outer         => Outer,
         Inner         => (Bagel, Inner),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   procedure Add_Elliptic_Background
     (Under         : not null access Layer_Location'Class;
      Outer         : Cairo.Ellipses.Ellipse_Parameters               := Cairo.Ellipses.Unit_Circle;
      Center        : Cairo.Ellipses.Cairo_Tuple;
      From          : Gdouble                                         := 0.0;
      Length        : Gdouble                                         := 2.0 * Ada.Numerics.Pi;
      Color         : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width  : Gdouble                                         := 0.0;
      Border_Depth  : Gdouble                                         := 1.0;
      Border_Color  : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened      : Boolean                                         := False;
      Scaled        : Boolean                                         := False;
      Widened       : Boolean                                         := False)
   is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Outer         => Outer,
         Inner         => (Sector, Center),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   procedure Add_Elliptic_Background
     (Under         : not null access Layer_Location'Class;
      Outer         : Cairo.Ellipses.Ellipse_Parameters               := Cairo.Ellipses.Unit_Circle;
      From          : Gdouble                                         := 0.0;
      Length        : Gdouble                                         := 2.0 * Ada.Numerics.Pi;
      Color         : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width  : Gdouble                                         := 0.0;
      Border_Depth  : Gdouble                                         := 1.0;
      Border_Color  : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened      : Boolean                                         := False;
      Scaled        : Boolean                                         := False;
      Widened       : Boolean                                         := False)
   is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Outer         => Outer,
         Inner         => (Shape => Segment),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   function Add_Elliptic_Background
     (Under         : not null access Layer_Location'Class;
      Outer         : Cairo.Ellipses.Ellipse_Parameters               := Cairo.Ellipses.Unit_Circle;
      Inner         : Cairo.Ellipses.Ellipse_Parameters;
      From          : Gdouble                                         := 0.0;
      Length        : Gdouble                                         := 2.0 * Ada.Numerics.Pi;
      Color         : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width  : Gdouble                                         := 0.0;
      Border_Depth  : Gdouble                                         := 1.0;
      Border_Color  : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened      : Boolean                                         := False;
      Scaled        : Boolean                                         := False;
      Widened       : Boolean                                         := False)
      return not null access Elliptic_Background_Layer
   is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Outer         => Outer,
         Inner         => (Bagel, Inner),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   function Add_Elliptic_Background
     (Under         : not null access Layer_Location'Class;
      Outer         : Cairo.Ellipses.Ellipse_Parameters               := Cairo.Ellipses.Unit_Circle;
      Center        : Cairo.Ellipses.Cairo_Tuple;
      From          : Gdouble                                         := 0.0;
      Length        : Gdouble                                         := 2.0 * Ada.Numerics.Pi;
      Color         : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width  : Gdouble                                         := 0.0;
      Border_Depth  : Gdouble                                         := 1.0;
      Border_Color  : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened      : Boolean                                         := False;
      Scaled        : Boolean                                         := False;
      Widened       : Boolean                                         := False)
      return not null access Elliptic_Background_Layer
   is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Outer         => Outer,
         Inner         => (Sector, Center),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   function Add_Elliptic_Background
     (Under         : not null access Layer_Location'Class;
      Outer         : Cairo.Ellipses.Ellipse_Parameters               := Cairo.Ellipses.Unit_Circle;
      From          : Gdouble                                         := 0.0;
      Length        : Gdouble                                         := 2.0 * Ada.Numerics.Pi;
      Color         : Gdk.Color.Gdk_Color                             := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Border_Width  : Gdouble                                         := 0.0;
      Border_Depth  : Gdouble                                         := 1.0;
      Border_Color  : Gtk.Layered.Abstract_Bordered.Border_Color_Type := Gtk.Layered.Abstract_Bordered.Default_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type                       := Gtk.Enums.Shadow_In;
      Deepened      : Boolean                                         := False;
      Scaled        : Boolean                                         := False;
      Widened       : Boolean                                         := False)
      return not null access Elliptic_Background_Layer
   is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Outer         => Outer,
         Inner         => (Shape => Segment),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   overriding procedure Draw_Contents
     (Layer   : in out Elliptic_Background_Layer;
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

   function Get_Color
     (Layer : Elliptic_Background_Layer) return Gdk.Color.Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_From (Layer : Elliptic_Background_Layer)
      return Gdouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Inner (Layer : Elliptic_Background_Layer)
      return Elliptic_Arc_Closure is
   begin
      return Layer.Inner;
   end Get_Inner;

   function Get_Length (Layer : Elliptic_Background_Layer)
      return Gdouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Outer (Layer : Elliptic_Background_Layer)
      return Cairo.Ellipses.Ellipse_Parameters is
   begin
      return Layer.Outer;
   end Get_Outer;

   overriding function Get_Properties_Number
     (Layer : Elliptic_Background_Layer) return Natural is
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         - Layer_Property'Pos (Layer_Property'First) + 1
         + Gtk.Layered.Abstract_Bordered.Get_Properties_Number
             (Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer)));
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Elliptic_Background_Layer;
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
            when Property_Outer_Center_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "outer-x",
                    Nick    => "outer x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the outer ellipse's center");
            when Property_Outer_Center_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "outer-y",
                    Nick    => "outer y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The y-coordinate of the outer ellipse's center");
            when Property_Outer_Curvature =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "outer-k",
                    Nick    => "outer k",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The curvature of the major axis of the outer ellipse");
            when Property_Outer_Radius =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "outer-r",
                    Nick    => "outer r",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 0.5,
                    Blurb   =>
                       "The radius of the minor axis of outer ellipse");
            when Property_Outer_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "outer-angle",
                    Nick    => "outer angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The angle of the major axis of the outer ellipse");
            when Property_Shape =>
               return
                 Gtk.Layered.Elliptic_Shape_Property.Gnew_Enum
                   (Name    => "background-shape",
                    Nick    => "background shape",
                    Default => Bagel,
                    Blurb   => "The shape of the background");
            when Property_Inner_Center_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-x",
                    Nick    => "inner x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "When background-shape is bagel, then this is the " &
                       "x-coordinate of the inner elliptic arc. When " &
                       "background-shape is sector, then this is the " &
                       "x-coordinate of the sector's center. Otherwise the " &
                       "property value is ignored");
            when Property_Inner_Center_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-y",
                    Nick    => "inner y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "When background-shape is bagel, then this is the " &
                       "y-coordinate of the inner elliptic arc. When " &
                       "background-shape is sector, then this is the " &
                       "y-coordinate of the sector's center. Otherwise the " &
                       "property value is ignored.");
            when Property_Inner_Curvature =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-k",
                    Nick    => "inner k",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "When background-shape is bagel, then this is the " &
                       "curvature of the major axis of the inner elliptic " &
                       "arc. Otherwise the property value is ignored.");
            when Property_Inner_Radius =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-r",
                    Nick    => "inner r",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 0.5,
                    Blurb   =>
                       "When background-shape is bagel, then this is the " &
                       "radius of the minor axis of the inner elliptic arc. " &
                       "Otherwise the property value is ignored.");
            when Property_Inner_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-angle",
                    Nick    => "inner angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "When background-shape is bagel, then this is the " &
                       "angle of the major axis of the inner elliptic arc. " &
                       "Otherwise the property value is ignored");
            when Property_From =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "from",
                    Nick    => "from",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angle of the outer elliptic arc beginning");
            when Property_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "length",
                    Nick    => "length",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angular length of the outer elliptic arc");
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
     (Layer    : Elliptic_Background_Layer;
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
               when Property_Outer_Center_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Outer.Center.X);
               when Property_Outer_Center_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Outer.Center.Y);
               when Property_Outer_Curvature =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Outer.Major_Curvature);
               when Property_Outer_Radius =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Outer.Minor_Radius);
               when Property_Outer_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Outer.Angle);
               when Property_Shape =>
                  Gtk.Layered.Elliptic_Shape_Property.Set_Enum
                    (Value,
                     Layer.Inner.Shape);
               when Property_Inner_Center_X =>
                  Glib.Values.Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Sector =>
                        Glib.Values.Set_Double (Value, Layer.Inner.Center.X);
                     when Bagel =>
                        Glib.Values.Set_Double (Value, Layer.Inner.Arc.Center.X);
                     when Segment =>
                        Glib.Values.Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Center_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Sector =>
                        Glib.Values.Set_Double (Value, Layer.Inner.Center.Y);
                     when Bagel =>
                        Glib.Values.Set_Double (Value, Layer.Inner.Arc.Center.Y);
                     when Segment =>
                        Glib.Values.Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Curvature =>
                  Glib.Values.Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Bagel =>
                        Glib.Values.Set_Double
                          (Value,
                           Layer.Inner.Arc.Major_Curvature);
                     when Segment | Sector =>
                        Glib.Values.Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Radius =>
                  Glib.Values.Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Bagel =>
                        Glib.Values.Set_Double
                          (Value,
                           Layer.Inner.Arc.Minor_Radius);
                     when Segment | Sector =>
                        Glib.Values.Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Bagel =>
                        Glib.Values.Set_Double (Value, Layer.Inner.Arc.Angle);
                     when Segment | Sector =>
                        Glib.Values.Set_Double (Value, 0.0);
                  end case;
               when Property_From =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From);
               when Property_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Length);
               when Property_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Color);
            end case;
            return Value;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Value;

   overriding procedure Move
     (Layer  : in out Elliptic_Background_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      Layer.Outer := Layer.Outer + Offset;
      case Layer.Inner.Shape is
         when Sector =>
            Layer.Inner.Center.X := Layer.Inner.Center.X + Offset.X;
            Layer.Inner.Center.Y := Layer.Inner.Center.Y + Offset.Y;
         when Bagel =>
            Layer.Inner.Arc := Layer.Inner.Arc + Offset;
         when Segment =>
            null;
      end case;
      Gtk.Layered.Abstract_Bordered.Set_Updated (Layer);
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Elliptic_Background_Layer)
   is
      Outer  : Cairo.Ellipses.Ellipse_Parameters;
      Inner  : Elliptic_Arc_Closure;
      From   : Gdouble;
      Length : Gdouble;
      Color  : Gdk.Color.Gdk_Color;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Outer);
      Gtk.Layered.Stream_IO.Restore (Stream, Inner);
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, Length);
      Gtk.Layered.Stream_IO.Restore (Stream, Color);
      Gtk.Layered.Abstract_Bordered.Restore
        (Stream, Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer));
      if Outer.Minor_Radius <= 0.0 then
         raise Constraint_Error with
               "Non-positive outer ellipse radius";
      elsif Outer.Major_Curvature < 0.0 then
         raise Constraint_Error with
               "Negative outer ellipse curvature";
      elsif Inner.Shape = Bagel then
         if Inner.Arc.Minor_Radius < 0.0 then
            raise Constraint_Error with
                  "Negative inner ellipse radius";
         elsif Inner.Arc.Major_Curvature < 0.0 then
            raise Constraint_Error with
                  "Negative inner ellipse curvature";
         end if;
      end if;
      Layer.Outer  := Outer;
      Layer.Inner  := Inner;
      Layer.From   := From;
      Layer.Length := Length;
      Layer.Color  := Color;
   end Restore;

   overriding procedure Scale
     (Layer  : in out Elliptic_Background_Layer;
      Factor : Gdouble)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
      Outer : constant Cairo.Ellipses.Ellipse_Parameters := Layer.Outer * Factor;
      Inner : Elliptic_Arc_Closure := Layer.Inner;
   begin
      case Inner.Shape is
         when Sector | Segment =>
            null;
         when Bagel =>
            Inner.Arc := Inner.Arc * Factor;
      end case;
      if Outer.Minor_Radius <= 0.0 then
         raise Constraint_Error with
               "Non-positive outer ellipse radius";
      elsif Outer.Major_Curvature < 0.0 then
         raise Constraint_Error with
               "Negative outer ellipse curvature";
      elsif Inner.Shape = Bagel then
         if Inner.Arc.Minor_Radius < 0.0 then
            raise Constraint_Error with
                  "Negative inner ellipse radius";
         elsif Inner.Arc.Major_Curvature < 0.0 then
            raise Constraint_Error with
                  "Negative inner ellipse curvature";
         end if;
      end if;
      Layer.Outer := Outer;
      Layer.Inner := Inner;
      Gtk.Layered.Abstract_Bordered.Set_Updated (Layer);
   end Scale;

   procedure Set
     (Layer         : in out Elliptic_Background_Layer;
      Outer         : Cairo.Ellipses.Ellipse_Parameters;
      Inner         : Elliptic_Arc_Closure;
      From          : Gdouble;
      Length        : Gdouble;
      Color         : Gdk.Color.Gdk_Color;
      Border_Width  : Gdouble;
      Border_Depth  : Gdouble;
      Border_Color  : Gtk.Layered.Abstract_Bordered.Border_Color_Type;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type) is
   begin
      if Outer.Minor_Radius <= 0.0 then
         raise Constraint_Error with
               "Non-positive outer ellipse radius";
      elsif Outer.Major_Curvature < 0.0 then
         raise Constraint_Error with "Negative outer ellipse curvature";
      elsif Inner.Shape = Bagel then
         if Inner.Arc.Minor_Radius < 0.0 then
            raise Constraint_Error with "Negative inner ellipse radius";
         elsif Inner.Arc.Major_Curvature < 0.0 then
            raise Constraint_Error with
                  "Negative inner ellipse curvature";
         end if;
      end if;
      Layer.Outer  := Outer;
      Layer.Inner  := Inner;
      Layer.From   := From;
      Layer.Length := Length;
      Layer.Color  := Color;
      Set
        (Layer         => Layer,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow);
   end Set;

   overriding procedure Set_Contents_Path
     (Layer   : in out Elliptic_Background_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
   begin
      if Get_Scaled (Layer) then
         declare
            Center : constant Cairo.Ellipses.Cairo_Tuple :=
                       Layer.Get_Widget.all.Get_Center;
            Size   : constant Gdouble := Layer.Get_Widget.all.Get_Size;
            use type Cairo.Ellipses.Ellipse_Parameters;
         begin
            Cairo.Ellipses.Elliptic_Arc_Abs
              (Context,
               Layer.Outer * Size + Center,
               Layer.From,
               Layer.Length);
            if abs Layer.Length < 2.0 * Ada.Numerics.Pi then
               case Layer.Inner.Shape is
                  when Segment =>
                     null;
                  when Sector =>
                     Cairo.Line_To
                       (Cr => Context,
                        X  => Layer.Inner.Center.X * Size + Center.X,
                        Y  => Layer.Inner.Center.Y * Size + Center.Y);
                  when Bagel =>
                     Cairo.Ellipses.Elliptic_Arc_Abs
                       (Context,
                        Layer.Inner.Arc * Size + Center,
                        Layer.From + Layer.Length,
                        -Layer.Length);
               end case;
            end if;
         end;
      else
         Cairo.Ellipses.Elliptic_Arc_Abs
           (Context,
            Layer.Outer,
            Layer.From,
            Layer.Length);
         if abs Layer.Length < 2.0 * Ada.Numerics.Pi then
            case Layer.Inner.Shape is
               when Segment =>
                  null;
               when Sector =>
                  Cairo.Line_To
                    (Cr => Context,
                     X  => Layer.Inner.Center.X,
                     Y  => Layer.Inner.Center.Y);
               when Bagel =>
                  Cairo.Ellipses.Elliptic_Arc_Abs
                    (Context,
                     Layer.Inner.Arc,
                     Layer.From + Layer.Length,
                     -Layer.Length);
            end case;
         end if;
      end if;
   end Set_Contents_Path;

   overriding procedure Set_Property_Value
     (Layer    : in out Elliptic_Background_Layer;
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
            when Property_Outer_Center_X =>
               Layer.Outer.Center.X := Glib.Values.Get_Double (Value);
            when Property_Outer_Center_Y =>
               Layer.Outer.Center.Y := Glib.Values.Get_Double (Value);
            when Property_Outer_Curvature =>
               Layer.Outer.Major_Curvature := Glib.Values.Get_Double (Value);
               if Layer.Outer.Major_Curvature < 0.0 then
                  Layer.Outer.Major_Curvature := 0.0;
               end if;
            when Property_Outer_Radius =>
               Layer.Outer.Minor_Radius := Glib.Values.Get_Double (Value);
               if Layer.Outer.Minor_Radius < 1.0E-6 then
                  Layer.Outer.Minor_Radius := 1.0E-6;
               end if;
            when Property_Outer_Angle =>
               Layer.Outer.Angle := Glib.Values.Get_Double (Value);
               if
                 Layer.Outer.Angle not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Outer.Angle :=
                    Gdouble'Remainder
                      (Layer.Outer.Angle, 2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Shape =>
               case Gtk.Layered.
                       Elliptic_Shape_Property.Get_Enum (Value) is
                  when Bagel =>
                     Layer.Inner :=
                       (Shape => Bagel,
                        Arc   => ((0.0, 0.0), 1.0, 1.0, 0.0));
                  when Sector =>
                     Layer.Inner :=
                       (Shape  => Sector,
                        Center => (0.0, 0.0));
                  when Segment =>
                     Layer.Inner := (Shape => Segment);
               end case;
            when Property_Inner_Center_X =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Center.X := Glib.Values.Get_Double (Value);
                  when Sector =>
                     Layer.Inner.Center.X := Glib.Values.Get_Double (Value);
                  when Segment =>
                     null;
               end case;
            when Property_Inner_Center_Y =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Center.Y := Glib.Values.Get_Double (Value);
                  when Sector =>
                     Layer.Inner.Center.Y := Glib.Values.Get_Double (Value);
                  when Segment =>
                     null;
               end case;
            when Property_Inner_Curvature =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Major_Curvature :=
                        Glib.Values.Get_Double (Value);
                     if Layer.Inner.Arc.Major_Curvature < 0.0 then
                        Layer.Inner.Arc.Major_Curvature := 0.0;
                     end if;
                  when Segment | Sector =>
                     null;
               end case;
            when Property_Inner_Radius =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Minor_Radius := Glib.Values.Get_Double (Value);
                     if Layer.Inner.Arc.Minor_Radius < 1.0E-6 then
                        Layer.Inner.Arc.Minor_Radius := 1.0E-6;
                     end if;
                  when Segment | Sector =>
                     null;
               end case;
            when Property_Inner_Angle =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Angle := Glib.Values.Get_Double (Value);
                     if
                       Layer.Inner.Arc.Angle not in
                         -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
                     then
                        Layer.Inner.Arc.Angle :=
                           Gdouble'Remainder
                            (Layer.Inner.Arc.Angle, 2.0 * Ada.Numerics.Pi);
                     end if;
                  when Segment | Sector =>
                     null;
               end case;
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
         end case;
         Gtk.Layered.Abstract_Bordered.Set_Updated (Layer);
      else
         raise Constraint_Error;
      end if;
   end Set_Property_Value;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Elliptic_Background_Layer) is
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Outer);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Inner);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.From);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Length);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Color);
      Gtk.Layered.Abstract_Bordered.Store
        (Stream, Gtk.Layered.Abstract_Bordered.Abstract_Bordered_Layer (Layer));
   end Store;

end Gtk.Layered.Elliptic_Background;
