--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Sector_Needle                   Luebeck            --
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
--____________________________________________________________________--

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Gtk.Layered.Elliptic_Shape_Property;

package body Gtk.Layered.Sector_Needle is

   Sqrt_2 : constant GDouble := sqrt (2.0);

   type Needle_Ptr is access all Sector_Needle_Layer;

   type Layer_Property is
        (  Property_Scaled,
           Property_Outer_Center_X,
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
           Property_Color,
           Property_Value
        );

   package Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Needle_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Sector_Needle_Layer, Needle_Ptr);

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle      : Needle_Ptr
             );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Sector_Needle_Layer is
      Ptr : Needle_Ptr := new Sector_Needle_Layer;
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
             (  Layer      : in out Sector_Needle_Layer;
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

   procedure Add_Sector_Needle
             (  Under      : not null access Layer_Location'Class;
                Outer      : Ellipse_Parameters := Unit_Circle;
                Inner      : Ellipse_Parameters;
                From       : GDouble   := 3.0 * Pi / 4.0;
                Length     : GDouble   := 3.0 * Pi / 2.0;
                Color      : Gdk_Color := RGB (1.0, 0.0, 0.0);
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean   := False
             )  is
      Ptr   : Needle_Ptr := new Sector_Needle_Layer;
      Layer : Sector_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => (Bagel, Inner),
         From   => From,
         Length => Length,
         Color  => Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Sector_Needle;

   procedure Add_Sector_Needle
             (  Under      : not null access Layer_Location'Class;
                Outer      : Ellipse_Parameters := Unit_Circle;
                Center     : Cairo_Tuple;
                From       : GDouble   := 3.0 * Pi / 4.0;
                Length     : GDouble   := 3.0 * Pi / 2.0;
                Color      : Gdk_Color := RGB (1.0, 0.0, 0.0);
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean   := False
             )  is
      Ptr   : Needle_Ptr := new Sector_Needle_Layer;
      Layer : Sector_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => (Sector, Center),
         From   => From,
         Length => Length,
         Color  => Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Sector_Needle;

   procedure Add_Sector_Needle
             (  Under      : not null access Layer_Location'Class;
                Outer      : Ellipse_Parameters := Unit_Circle;
                From       : GDouble   := 3.0 * Pi / 4.0;
                Length     : GDouble   := 3.0 * Pi / 2.0;
                Color      : Gdk_Color := RGB (1.0, 0.0, 0.0);
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean   := False
             )  is
      Ptr   : Needle_Ptr := new Sector_Needle_Layer;
      Layer : Sector_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => (Shape => Segment),
         From   => From,
         Length => Length,
         Color  => Color
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Sector_Needle;

   function Add_Sector_Needle
            (  Under      : not null access Layer_Location'Class;
               Outer      : Ellipse_Parameters := Unit_Circle;
               Inner      : Ellipse_Parameters;
               From       : GDouble   := 3.0 * Pi / 4.0;
               Length     : GDouble   := 3.0 * Pi / 2.0;
               Color      : Gdk_Color := RGB (1.0, 0.0, 0.0);
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean   := False
            )  return not null access Sector_Needle_Layer is
      Ptr   : Needle_Ptr := new Sector_Needle_Layer;
      Layer : Sector_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => (Bagel, Inner),
         From   => From,
         Length => Length,
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
   end Add_Sector_Needle;

   function Add_Sector_Needle
            (  Under      : not null access Layer_Location'Class;
               Outer      : Ellipse_Parameters := Unit_Circle;
               Center     : Cairo_Tuple;
               From       : GDouble   := 3.0 * Pi / 4.0;
               Length     : GDouble   := 3.0 * Pi / 2.0;
               Color      : Gdk_Color := RGB (1.0, 0.0, 0.0);
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean   := False
            )  return not null access Sector_Needle_Layer is
      Ptr   : Needle_Ptr := new Sector_Needle_Layer;
      Layer : Sector_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => (Sector, Center),
         From   => From,
         Length => Length,
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
   end Add_Sector_Needle;

   function Add_Sector_Needle
            (  Under      : not null access Layer_Location'Class;
               Outer      : Ellipse_Parameters := Unit_Circle;
               From       : GDouble   := 3.0 * Pi / 4.0;
               Length     : GDouble   := 3.0 * Pi / 2.0;
               Color      : Gdk_Color := RGB (1.0, 0.0, 0.0);
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean   := False
            )  return not null access Sector_Needle_Layer is
      Ptr   : Needle_Ptr := new Sector_Needle_Layer;
      Layer : Sector_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => (Shape => Segment),
         From   => From,
         Length => Length,
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
   end Add_Sector_Needle;

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
         Queue_Draw (Needle.Widget);  -- Signal draw to the widget
      end if;
   end Changed;

   procedure Draw
             (  Layer   : in out Sector_Needle_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Length : constant GDouble := Layer.Length * Layer.Value;
   begin
      New_Path (Context);
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
               Length
            );
            if abs Length < 2.0 * Pi then
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
                        Layer.From + Length,
                       -Length
                      );
               end case;
            end if;
         end;
      else
         Elliptic_Arc_Abs
         (  Context,
            Layer.Outer,
            Layer.From,
            Length
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
                     Layer.From + Length,
                    -Length
                   );
            end case;
         end if;
      end if;
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

   procedure Finalize (Layer : in out Sector_Needle_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Disconnect (Layer.Adjustment, Layer.Changed);
         Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   function Get_Adjustment (Layer : Sector_Needle_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Color (Layer : Sector_Needle_Layer) return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_From (Layer : Sector_Needle_Layer) return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Inner (Layer : Sector_Needle_Layer)
      return Elliptic_Arc_Closure is
   begin
      return Layer.Inner;
   end Get_Inner;

   function Get_Length  (Layer : Sector_Needle_Layer) return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Outer (Layer : Sector_Needle_Layer)
      return Ellipse_Parameters is
   begin
      return Layer.Outer;
   end Get_Outer;

   function Get_Properties_Number
            (  Layer : Sector_Needle_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Sector_Needle_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
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
                  (  Name    => "shape",
                     Nick    => "shape",
                     Default => Bagel,
                     Blurb   => "The shape of the needle"
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
                        "When shape is bagel, then this " &
                        "is the x-coordinate of the inner elliptic " &
                        "arc. When shape is sector, then " &
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
                        "When shape is bagel, then this " &
                        "is the y-coordinate of the inner elliptic " &
                        "arc. When shape is sector, then " &
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
                        "When shape is bagel, then this " &
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
                        "When shape is bagel, then this " &
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
                        "When shape is bagel, then this " &
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
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The needle color"
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
            (  Layer    : Sector_Needle_Layer;
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
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Value =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Value);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Sector_Needle_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Value (Layer : Sector_Needle_Layer) return GDouble is
   begin
      return Layer.Value;
   end Get_Value;

   function Is_Updated (Layer : Sector_Needle_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Sector_Needle_Layer;
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
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Sector_Needle_Layer
             )  is
      Outer      : Ellipse_Parameters;
      Inner      : Elliptic_Arc_Closure;
      From       : GDouble;
      Length     : GDouble;
      Color      : Gdk_Color;
      Adjustment : Boolean;
   begin
      Restore (Stream, Outer);
      Restore (Stream, Inner);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Color);
      Restore (Stream, Layer.Scaled, Adjustment);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => Inner,
         From   => From,
         Length => Length,
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
             (  Layer  : in out Sector_Needle_Layer;
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
      Layer.Outer   := Outer;
      Layer.Inner   := Inner;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer  : in out Sector_Needle_Layer;
                Outer  : Ellipse_Parameters;
                Inner  : Elliptic_Arc_Closure;
                From   : GDouble;
                Length : GDouble;
                Color  : Gdk_Color
             )  is
   begin
      if Outer.Minor_Radius <= 0.0 then
         raise Constraint_Error with
            "Non-positive outer ellipse radius";
      elsif Outer.Major_Curvature < 0.0 then
         raise Constraint_Error with
            "Negative outer ellipse curvature";
      elsif Inner.Shape = Sector then
         if Inner.Arc.Minor_Radius < 0.0 then
            raise Constraint_Error with
               "Negative inner ellipse radius";
         elsif Inner.Arc.Major_Curvature < 0.0 then
            raise Constraint_Error with
               "Negative inner ellipse curvature";
         end if;
      end if;
      Layer.Outer   := Outer;
      Layer.Inner   := Inner;
      Layer.From    := From;
      Layer.Length  := Length;
      Layer.Color   := Color;
      Layer.Updated := True;
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Sector_Needle_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
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
            when Property_Value =>
               Set_Value (Layer, Get_Double (Value));
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Sector_Needle_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Value
             (  Layer : in out Sector_Needle_Layer;
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
                Layer  : Sector_Needle_Layer
             )  is
   begin
      Store (Stream, Layer.Outer);
      Store (Stream, Layer.Inner);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Color);
      Store (Stream, Layer.Scaled, Layer.Adjustment /= null);
      if Layer.Adjustment = null then
         Store (Stream, Layer.Value);
      else
         Store (Stream, Layer.Adjustment);
      end if;
   end Store;

end Gtk.Layered.Sector_Needle;
