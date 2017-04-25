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
--____________________________________________________________________--

with Cairo;                     use Cairo;
with Gdk.Color.IHLS;            use Gdk.Color.IHLS;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;     use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Gtk.Layered.Elliptic_Shape_Property;

package body Gtk.Layered.Elliptic_Background is
   type Elliptic_Background_Ptr is access Elliptic_Background_Layer;

   type Layer_Property is
        (  Property_Outer_Center_X,
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
           Property_Color
        );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Elliptic_Background_Layer,
             Elliptic_Background_Ptr
          );

   procedure Add_Elliptic_Background
             (  Under  : not null access Layer_Location'Class;
                Outer  : Ellipse_Parameters := Unit_Circle;
                Inner  : Ellipse_Parameters;
                From          : GDouble    := 0.0;
                Length        : GDouble    := 2.0 * Pi;
                Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
                Border_Width  : GDouble    := 0.0;
                Border_Depth  : GDouble    := 1.0;
                Border_Color  : Border_Color_Type := Default_Color;
                Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
                Deepened      : Boolean           := False;
                Scaled        : Boolean := False;
                Widened       : Boolean := False
             )  is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
      (  Layer         => Layer,
         Outer         => Outer,
         Inner         => (Bagel, Inner),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   procedure Add_Elliptic_Background
             (  Under  : not null access Layer_Location'Class;
                Outer  : Ellipse_Parameters := Unit_Circle;
                Center : Cairo_Tuple;
                From          : GDouble    := 0.0;
                Length        : GDouble    := 2.0 * Pi;
                Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
                Border_Width  : GDouble    := 0.0;
                Border_Depth  : GDouble    := 1.0;
                Border_Color  : Border_Color_Type := Default_Color;
                Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
                Deepened      : Boolean           := False;
                Scaled        : Boolean := False;
                Widened       : Boolean := False
             )  is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
      (  Layer         => Layer,
         Outer         => Outer,
         Inner         => (Sector, Center),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   procedure Add_Elliptic_Background
             (  Under  : not null access Layer_Location'Class;
                Outer  : Ellipse_Parameters := Unit_Circle;
                From          : GDouble      := 0.0;
                Length        : GDouble      := 2.0 * Pi;
                Color         : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Border_Width  : GDouble    := 0.0;
                Border_Depth  : GDouble    := 1.0;
                Border_Color  : Border_Color_Type := Default_Color;
                Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
                Deepened      : Boolean           := False;
                Scaled        : Boolean := False;
                Widened       : Boolean := False
             )  is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
      (  Layer         => Layer,
         Outer         => Outer,
         Inner         => (Shape => Segment),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   function Add_Elliptic_Background
            (  Under  : not null access Layer_Location'Class;
               Outer  : Ellipse_Parameters := Unit_Circle;
               Inner  : Ellipse_Parameters;
               From          : GDouble    := 0.0;
               Length        : GDouble    := 2.0 * Pi;
               Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
               Border_Width  : GDouble    := 0.0;
               Border_Depth  : GDouble    := 1.0;
               Border_Color  : Border_Color_Type := Default_Color;
               Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
               Deepened      : Boolean           := False;
               Scaled        : Boolean := False;
               Widened       : Boolean := False
            )  return not null access Elliptic_Background_Layer is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
      (  Layer         => Layer,
         Outer         => Outer,
         Inner         => (Bagel, Inner),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   function Add_Elliptic_Background
            (  Under  : not null access Layer_Location'Class;
               Outer  : Ellipse_Parameters := Unit_Circle;
               Center : Cairo_Tuple;
               From          : GDouble    := 0.0;
               Length        : GDouble    := 2.0 * Pi;
               Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
               Border_Width  : GDouble    := 0.0;
               Border_Depth  : GDouble    := 1.0;
               Border_Color  : Border_Color_Type := Default_Color;
               Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
               Deepened      : Boolean           := False;
               Scaled        : Boolean := False;
               Widened       : Boolean := False
            )  return not null access Elliptic_Background_Layer is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
      (  Layer         => Layer,
         Outer         => Outer,
         Inner         => (Sector, Center),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   function Add_Elliptic_Background
            (  Under  : not null access Layer_Location'Class;
               Outer  : Ellipse_Parameters := Unit_Circle;
               From          : GDouble    := 0.0;
               Length        : GDouble    := 2.0 * Pi;
               Color         : Gdk_Color := RGB (0.0, 0.0, 0.0);
               Border_Width  : GDouble    := 0.0;
               Border_Depth  : GDouble    := 1.0;
               Border_Color  : Border_Color_Type := Default_Color;
               Border_Shadow : Gtk_Shadow_Type   := Shadow_In;
               Deepened      : Boolean           := False;
               Scaled        : Boolean := False;
               Widened       : Boolean := False
            )  return not null access Elliptic_Background_Layer is
      Ptr   : Elliptic_Background_Ptr := new Elliptic_Background_Layer;
      Layer : Elliptic_Background_Layer renames Ptr.all;
   begin
      Set_Deepened (Layer, Deepened);
      Set_Scaled   (Layer, Scaled);
      Set_Widened  (Layer, Widened);
      Add (Ptr, Under);
      Set
      (  Layer         => Layer,
         Outer         => Outer,
         Inner         => (Shape => Segment),
         From          => From,
         Length        => Length,
         Color         => Color,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Background;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Elliptic_Background_Layer is
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

   procedure Draw_Contents
             (  Layer   : in out Elliptic_Background_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
   begin
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Green (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Blue  (Layer.Color)) / GDouble (Guint16'Last)
      );
      Cairo.Fill (Context);
   end Draw_Contents;

   function Get_Color (Layer : Elliptic_Background_Layer)
      return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_From (Layer : Elliptic_Background_Layer)
      return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Inner (Layer : Elliptic_Background_Layer)
      return Elliptic_Arc_Closure is
   begin
      return Layer.Inner;
   end Get_Inner;

   function Get_Length (Layer : Elliptic_Background_Layer)
      return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Outer (Layer : Elliptic_Background_Layer)
      return Ellipse_Parameters is
   begin
      return Layer.Outer;
   end Get_Outer;

   function Get_Properties_Number
            (  Layer : Elliptic_Background_Layer
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
            (  Layer    : Elliptic_Background_Layer;
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
            when Property_Outer_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "outer-x",
                     Nick    => "outer x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The x-coordinate of the outer ellipse's center"
                  );
            when Property_Outer_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "outer-y",
                     Nick    => "outer y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The y-coordinate of the outer ellipse's center"
                  );
            when Property_Outer_Curvature =>
               return
                  Gnew_Double
                  (  Name    => "outer-k",
                     Nick    => "outer k",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The curvature of the major axis of " &
                                "the outer ellipse"
                  );
            when Property_Outer_Radius =>
               return
                  Gnew_Double
                  (  Name    => "outer-r",
                     Nick    => "outer r",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 0.5,
                     Blurb   => "The radius of the minor axis of " &
                                "outer ellipse"
                  );
            when Property_Outer_Angle =>
               return
                  Gnew_Double
                  (  Name    => "outer-angle",
                     Nick    => "outer angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the major axis of the " &
                                "outer ellipse"
                  );
            when Property_Shape =>
               return
                  Gtk.Layered.Elliptic_Shape_Property.Gnew_Enum
                  (  Name    => "backround-shape",
                     Nick    => "backround shape",
                     Default => Bagel,
                     Blurb   => "The shape of the background"
                  );
            when Property_Inner_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "inner-x",
                     Nick    => "inner x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "When background-shape is bagel, then this " &
                        "is the x-coordinate of the inner elliptic " &
                        "arc. When background-shape is sector, then " &
                        "this is the x-coordinate of the sector's " &
                        "center. Otherwise the property value is " &
                        "ignored"
                  );
            when Property_Inner_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "inner-y",
                     Nick    => "inner y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "When background-shape is bagel, then this " &
                        "is the y-coordinate of the inner elliptic " &
                        "arc. When background-shape is sector, then " &
                        "this is the y-coordinate of the sector's " &
                        "center. Otherwise the property value is " &
                        "ignored"
                  );
            when Property_Inner_Curvature =>
               return
                  Gnew_Double
                  (  Name    => "inner-k",
                     Nick    => "inner k",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "When background-shape is bagel, then this " &
                        "is the curvature of the major axis of the " &
                        "inner elliptic arc. Otherwise the property " &
                        "value is ignored"
                  );
            when Property_Inner_Radius =>
               return
                  Gnew_Double
                  (  Name    => "inner-r",
                     Nick    => "inner r",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 0.5,
                     Blurb =>
                        "When background-shape is bagel, then this " &
                        "is the radius of the minor axis of the " &
                        "inner elliptic arc. Otherwise the property " &
                        "value is ignored"
                  );
            when Property_Inner_Angle =>
               return
                  Gnew_Double
                  (  Name    => "inner-angle",
                     Nick    => "inner angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb =>
                        "When background-shape is bagel, then this " &
                        "is the angle of the major axis of the inner " &
                        "elliptic arc. Otherwise the property " &
                        "value is ignored"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the outer elliptic " &
                                "arc beginning"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angular length of the outer " &
                                "elliptic arc"
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
            (  Layer    : Elliptic_Background_Layer;
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
               when Property_Outer_Center_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Outer.Center.X);
               when Property_Outer_Center_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Outer.Center.Y);
               when Property_Outer_Curvature =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Outer.Major_Curvature);
               when Property_Outer_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Outer.Minor_Radius);
               when Property_Outer_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Outer.Angle);
               when Property_Shape =>
                  Gtk.Layered.Elliptic_Shape_Property.Set_Enum
                  (  Value,
                     Layer.Inner.Shape
                  );
               when Property_Inner_Center_X =>
                  Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Sector =>
                        Set_Double (Value, Layer.Inner.Center.X);
                     when Bagel =>
                        Set_Double (Value, Layer.Inner.Arc.Center.X);
                     when Segment =>
                        Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Center_Y =>
                  Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Sector =>
                        Set_Double (Value, Layer.Inner.Center.Y);
                     when Bagel =>
                        Set_Double (Value, Layer.Inner.Arc.Center.Y);
                     when Segment =>
                        Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Curvature =>
                  Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Bagel =>
                        Set_Double
                        (  Value,
                           Layer.Inner.Arc.Major_Curvature
                        );
                     when Segment | Sector =>
                        Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Radius =>
                  Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Bagel =>
                        Set_Double
                        (  Value,
                           Layer.Inner.Arc.Minor_Radius
                        );
                     when Segment | Sector =>
                        Set_Double (Value, 0.0);
                  end case;
               when Property_Inner_Angle =>
                  Init (Value, GType_Double);
                  case Layer.Inner.Shape is
                     when Bagel =>
                        Set_Double (Value, Layer.Inner.Arc.Angle);
                     when Segment | Sector =>
                        Set_Double (Value, 0.0);
                  end case;
               when Property_From =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From);
               when Property_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Length);
               when Property_Color =>
                  Set_Value (Value, Layer.Color);
            end case;
            return Value;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Value;

   procedure Move
             (  Layer  : in out Elliptic_Background_Layer;
                Offset : Cairo_Tuple
             )  is
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
      Set_Updated (Layer);
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Elliptic_Background_Layer
             )  is
      Outer  : Ellipse_Parameters;
      Inner  : Elliptic_Arc_Closure;
      From   : GDouble;
      Length : GDouble;
      Color  : Gdk_Color;
   begin
      Restore (Stream, Outer);
      Restore (Stream, Inner);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Color);
      Restore (Stream, Abstract_Bordered_Layer (Layer));
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

   procedure Scale
             (  Layer  : in out Elliptic_Background_Layer;
                Factor : GDouble
             )  is
      Outer : constant Ellipse_Parameters := Layer.Outer * Factor;
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
      Set_Updated (Layer);
   end Scale;

   procedure Set
             (  Layer         : in out Elliptic_Background_Layer;
                Outer         : Ellipse_Parameters;
                Inner         : Elliptic_Arc_Closure;
                From          : GDouble;
                Length        : GDouble;
                Color         : Gdk_Color;
                Border_Width  : GDouble;
                Border_Depth  : GDouble;
                Border_Color  : Border_Color_Type;
                Border_Shadow : Gtk_Shadow_Type
             )  is
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
      (  Layer         => Layer,
         Border_Width  => Border_Width,
         Border_Depth  => Border_Depth,
         Border_Color  => Border_Color,
         Border_Shadow => Border_Shadow
      );
   end Set;

   procedure Set_Contents_Path
             (  Layer   : in out Elliptic_Background_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
   begin
      if Get_Scaled (Layer) then
         declare
            Center : constant Cairo_Tuple :=
                              Layer.Get_Widget.Get_Center;
            Size   : constant GDouble := Layer.Get_Widget.Get_Size;
         begin
            Elliptic_Arc_Abs
            (  Context,
               Layer.Outer * Size + Center,
               Layer.From,
               Layer.Length
            );
            if abs Layer.Length < 2.0 * Pi then
               case Layer.Inner.Shape is
                  when Segment =>
                     null;
                  when Sector =>
                     Line_To
                     (  Cr => Context,
                        X  => Layer.Inner.Center.X * Size + Center.X,
                        Y  => Layer.Inner.Center.Y * Size + Center.Y
                     );
                  when Bagel =>
                     Elliptic_Arc_Abs
                     (  Context,
                        Layer.Inner.Arc * Size + Center,
                        Layer.From + Layer.Length,
                       -Layer.Length
                      );
               end case;
            end if;
         end;
      else
         Elliptic_Arc_Abs
         (  Context,
            Layer.Outer,
            Layer.From,
            Layer.Length
         );
         if abs Layer.Length < 2.0 * Pi then
            case Layer.Inner.Shape is
               when Segment =>
                  null;
               when Sector =>
                  Line_To
                  (  Cr => Context,
                     X  => Layer.Inner.Center.X,
                     Y  => Layer.Inner.Center.Y
                  );
               when Bagel =>
                  Elliptic_Arc_Abs
                  (  Context,
                     Layer.Inner.Arc,
                     Layer.From + Layer.Length,
                    -Layer.Length
                   );
            end case;
         end if;
      end if;
   end Set_Contents_Path;

   procedure Set_Property_Value
             (  Layer    : in out Elliptic_Background_Layer;
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
            when Property_Outer_Center_X =>
               Layer.Outer.Center.X := Get_Double (Value);
            when Property_Outer_Center_Y =>
               Layer.Outer.Center.Y := Get_Double (Value);
            when Property_Outer_Curvature =>
               Layer.Outer.Major_Curvature := Get_Double (Value);
               if Layer.Outer.Major_Curvature < 0.0 then
                  Layer.Outer.Major_Curvature := 0.0;
               end if;
            when Property_Outer_Radius =>
               Layer.Outer.Minor_Radius := Get_Double (Value);
               if Layer.Outer.Minor_Radius < 1.0E-6 then
                  Layer.Outer.Minor_Radius := 1.0E-6;
               end if;
            when Property_Outer_Angle =>
               Layer.Outer.Angle := Get_Double (Value);
               if Layer.Outer.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Outer.Angle :=
                     GDouble'Remainder (Layer.Outer.Angle, 2.0 * Pi);
               end if;
            when Property_Shape =>
               case Gtk.Layered.
                       Elliptic_Shape_Property.Get_Enum (Value) is
                  when Bagel =>
                     Layer.Inner :=
                        (  Shape => Bagel,
                           Arc   => ((0.0, 0.0), 1.0, 1.0, 0.0)
                        );
                  when Sector =>
                     Layer.Inner :=
                        (  Shape  => Sector,
                           Center => (0.0, 0.0)
                        );
                  when Segment =>
                     Layer.Inner := (Shape => Segment);
               end case;
            when Property_Inner_Center_X =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Center.X := Get_Double (Value);
                  when Sector =>
                     Layer.Inner.Center.X := Get_Double (Value);
                  when Segment =>
                     null;
               end case;
            when Property_Inner_Center_Y =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Center.Y := Get_Double (Value);
                  when Sector =>
                     Layer.Inner.Center.Y := Get_Double (Value);
                  when Segment =>
                     null;
               end case;
            when Property_Inner_Curvature =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Major_Curvature :=
                        Get_Double (Value);
                     if Layer.Inner.Arc.Major_Curvature < 0.0 then
                        Layer.Inner.Arc.Major_Curvature := 0.0;
                     end if;
                  when Segment | Sector =>
                     null;
               end case;
            when Property_Inner_Radius =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Minor_Radius := Get_Double (Value);
                     if Layer.Inner.Arc.Minor_Radius < 1.0E-6 then
                        Layer.Inner.Arc.Minor_Radius := 1.0E-6;
                     end if;
                  when Segment | Sector =>
                     null;
               end case;
            when Property_Inner_Angle =>
               case Layer.Inner.Shape is
                  when Bagel =>
                     Layer.Inner.Arc.Angle := Get_Double (Value);
                     if Layer.Inner.Arc.Angle not in -2.0 * Pi..2.0 * Pi
                     then
                        Layer.Inner.Arc.Angle :=
                           GDouble'Remainder
                           (  Layer.Inner.Arc.Angle, 2.0 * Pi
                           );
                     end if;
                  when Segment | Sector =>
                     null;
               end case;
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
         end case;
         Set_Updated (Layer);
      else
         raise Constraint_Error;
      end if;
   end Set_Property_Value;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Elliptic_Background_Layer
             )  is
   begin
      Store (Stream, Layer.Outer);
      Store (Stream, Layer.Inner);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Color);
      Store (Stream, Abstract_Bordered_Layer (Layer));
   end Store;

end Gtk.Layered.Elliptic_Background;
