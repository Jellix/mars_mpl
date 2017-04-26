--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Scale                  Luebeck            --
--  Implementation                                 Autumn, 2010       --
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

with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Gtk.Layered.Stream_IO;     use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;

package body Gtk.Layered.Elliptic_Scale is
   type Scale_Ptr is access all Elliptic_Scale_Layer;

   type Layer_Property is
        (  Property_Scaled,
           Property_Widened,
           Property_Center_X,
           Property_Center_Y,
           Property_Outer_Curvature,
           Property_Outer_Radius,
           Property_Outer_Angle,
           Property_Inner_Curvature,
           Property_Inner_Radius,
           Property_Inner_Angle,
           Property_From,
           Property_Length,
           Property_Tick_Step,
           Property_Tick_First,
           Property_Tick_Skipped,
           Property_Line_Width,
           Property_Line_Color,
           Property_Line_Cap
        );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Elliptic_Scale." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation (Elliptic_Scale_Layer, Scale_Ptr);

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Elliptic_Scale_Layer is
      Ptr : Scale_Ptr := new Elliptic_Scale_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Elliptic_Scale
             (  Under    : not null access Layer_Location'Class;
                Step     : Gdouble;
                First    : Tick_Number        := Tick_Number'Last;
                Skipped  : Tick_Number        := Tick_Number'Last;
                Outer    : Ellipse_Parameters := Unit_Circle;
                Inner    : Ellipse_Parameters := Unit_Circle / 2.0;
                From     : Gdouble            := 0.0;
                Length   : Gdouble            := 2.0 * Pi;
                Width    : Gdouble            := 1.0;
                Color    : Gdk_Color          := RGB (0.0, 0.0, 0.0);
                Line_Cap : Cairo_Line_Cap     := Cairo_Line_Cap_Butt;
                Scaled   : Boolean            := False;
                Widened  : Boolean            := False
             )  is
      Ptr   : Scale_Ptr := new Elliptic_Scale_Layer;
      Layer : Elliptic_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => Inner,
         Line   => (Width, Color, Line_Cap),
         Ticks  => (Step, Get_First_Tick (First, Skipped), Skipped),
         From   => From,
         Length => Length
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Scale;

   function Add_Elliptic_Scale
            (  Under    : not null access Layer_Location'Class;
               Step     : Gdouble;
               First    : Tick_Number        := Tick_Number'Last;
               Skipped  : Tick_Number        := Tick_Number'Last;
               Outer    : Ellipse_Parameters := Unit_Circle;
               Inner    : Ellipse_Parameters := Unit_Circle / 2.0;
               From     : Gdouble            := 0.0;
               Length   : Gdouble            := 2.0 * Pi;
               Width    : Gdouble            := 1.0;
               Color    : Gdk_Color          := RGB (0.0, 0.0, 0.0);
               Line_Cap : Cairo_Line_Cap     := Cairo_Line_Cap_Butt;
               Scaled   : Boolean            := False;
               Widened  : Boolean            := False
            )  return not null access Elliptic_Scale_Layer is
      Ptr   : Scale_Ptr := new Elliptic_Scale_Layer;
      Layer : Elliptic_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => Inner,
         Line   => (Width, Color, Line_Cap),
         Ticks  => (Step, Get_First_Tick (First, Skipped), Skipped),
         From   => From,
         Length => Length
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Scale;

   procedure Draw
             (  Layer   : in out Elliptic_Scale_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      This     : Gdouble;
      Thick    : Natural := Layer.Ticks.First;
      From, To : Cairo_Tuple;
      Length   : constant Gdouble := abs Layer.Length +
                                     abs Layer.Ticks.Step * 0.05;
      State    : Context_State := Save (Context);
   begin
      New_Path (Context);
      Set_Source_Rgb
      (  Context,
         Gdouble (Red   (Layer.Line.Color)) / Gdouble (Guint16'Last),
         GDouble (Green (Layer.Line.Color)) / GDouble (Guint16'Last),
         GDouble (Blue  (Layer.Line.Color)) / GDouble (Guint16'Last)
      );
      Set_Line_Cap (Context, Layer.Line.Line_Cap);
      if Layer.Widened then
         Set_Line_Width
         (  Context,
            Layer.Line.Width * Layer.Widget.Get_Size
         );
      else
         Set_Line_Width (Context, Layer.Line.Width);
      end if;
      if Layer.Scaled then
         declare
            Center : constant Cairo_Tuple := Layer.Widget.Get_Center;
            Size   : constant GDouble      := Layer.Widget.Get_Size;
            Inner  : constant Ellipse_Parameters :=
                        Layer.Inner * Size + Center;
            Outer  : constant Ellipse_Parameters :=
                        Layer.Outer * Size + Center;
         begin
            for Index in Natural'Range loop
               This := Layer.Ticks.Step * GDouble (Index);
               exit when abs This > Length;
               if Thick = Layer.Ticks.Skipped then
                  Thick := 1;
               else
                  Thick := Thick + 1;
                  begin
                     From :=
                        Get_Point (Inner, Inner * (Layer.From + This));
                     To :=
                        Get_Point (Outer, Outer * (Layer.From + This));
                     Move_To (Context, From.X, From.Y);
                     Line_To (Context, To.X,   To.Y);
                  exception
                     when Constraint_Error =>
                        null;
                  end;
               end if;
            end loop;
         end;
      else
         for Index in Natural'Range loop
            This := Layer.Ticks.Step * GDouble (Index);
            exit when abs This > Length;
            if Thick = Layer.Ticks.Skipped then
               Thick := 1;
            else
               Thick := Thick + 1;
               begin
                  From :=
                     Get_Point
                     (  Layer.Inner,
                        Layer.Inner * (Layer.From + This)
                     );
                  To :=
                     Get_Point
                     (  Layer.Outer,
                        Layer.Outer * (Layer.From + This)
                     );
                  Move_To (Context, From.X, From.Y);
                  Line_To (Context, To.X,   To.Y);
               exception
                  when Constraint_Error =>
                     null;
               end;
            end if;
         end loop;
      end if;
      Stroke (Context);
      Layer.Updated := False;
   end Draw;

   function Get_From (Layer : Elliptic_Scale_Layer) return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Inner (Layer : Elliptic_Scale_Layer)
      return Ellipse_Parameters is
   begin
      return Layer.Inner;
   end Get_Inner;

   function Get_Length (Layer : Elliptic_Scale_Layer) return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Line (Layer : Elliptic_Scale_Layer)
      return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   function Get_Outer (Layer : Elliptic_Scale_Layer)
      return Ellipse_Parameters is
   begin
      return Layer.Outer;
   end Get_Outer;

   function Get_Properties_Number
            (  Layer : Elliptic_Scale_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Elliptic_Scale_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the outer and " &
                                "inner ellipses' center"
                  );
            when Property_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the outer and " &
                                "inner ellipses' center"
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
                                "the outer ellipse"
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
            when Property_Inner_Curvature =>
               return
                  Gnew_Double
                  (  Name    => "inner-k",
                     Nick    => "inner k",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The curvature of the major axis of " &
                                "the inner ellipse"
                  );
            when Property_Inner_Radius =>
               return
                  Gnew_Double
                  (  Name    => "inner-r",
                     Nick    => "inner r",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 0.5,
                     Blurb   => "The radius of the minor axis of " &
                                "the inner ellipse"
                  );
            when Property_Inner_Angle =>
               return
                  Gnew_Double
                  (  Name    => "inner-angle",
                     Nick    => "inner angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the major axis of the " &
                                "outer ellipse"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the scale beginning, " &
                                "which corresponds to the first tick"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angular length of the scale"
                  );
            when Property_Tick_Step =>
               return
                  Gnew_Double
                  (  Name    => "step",
                     Nick    => "step",
                     Minimum => 1.0E-6,
                     Maximum => 2.0 * Pi,
                     Default => Pi / 12.0,
                     Blurb   => "The angular distance between two " &
                                "consequent ticks"
                  );
            when Property_Tick_First =>
               return
                  Gnew_UInt
                  (  Name    => "first-tick",
                     Nick    => "first tick",
                     Minimum => GUInt (Tick_Number'First),
                     Maximum => GUInt (Tick_Number'Last),
                     Default => 1,
                     Blurb   => "The number of the first tick. " &
                                "The first tick is located at " &
                                "the beginning of the scale"
                  );
            when Property_Tick_Skipped =>
               return
                  Gnew_UInt
                  (  Name    => "skipped-tick",
                     Nick    => "skipped tick",
                     Minimum => 2,
                     Maximum => GUInt (Tick_Number'Last),
                     Default => GUInt (Tick_Number'Last),
                     Blurb   => "The number of the skipped tick. " &
                                "The ticks are numbered from 1 to " &
                                "skipped-tick. The ticks with this " &
                                "number are not drawn"
                  );
            when Property_Line_Width =>
               return
                  Gnew_Double
                  (  Name    => "line-width",
                     Nick    => "line width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The tick line's width"
                  );
            when Property_Line_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The tick line's color"
                  );
            when Property_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                  (  Name    => "line-cap",
                     Nick    => "line cap",
                     Default => CAIRO_LINE_CAP_BUTT,
                     Blurb   => "The cap style of the tick lines"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The scale size is changed when " &
                                "the widget is resized"
                  );
            when Property_Widened =>
               return
                  Gnew_Boolean
                  (  Name    => "widened",
                     Nick    => "widened",
                     Default => False,
                     Blurb   => "The tick's line width is changed " &
                                "when the widget is resized"
                  );
         end case;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Elliptic_Scale_Layer;
               Property : Positive
            )  return GValue is
   begin
      if Property <= Get_Properties_Number (Layer) then
         declare
            Value : GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Center_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Outer.Center.X);
               when Property_Center_Y =>
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
               when Property_Inner_Curvature =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Inner.Major_Curvature);
               when Property_Inner_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Inner.Minor_Radius);
               when Property_Inner_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Inner.Angle);
               when Property_From =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From);
               when Property_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Length);
               when Property_Line_Width =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Line.Width);
               when Property_Line_Color =>
                  Set_Value (Value, Layer.Line.Color);
               when Property_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                  (  Value,
                     Layer.Line.Line_Cap
                  );
               when Property_Tick_Step =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ticks.Step);
               when Property_Tick_First =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Layer.Ticks.First));
               when Property_Tick_Skipped =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Layer.Ticks.Skipped));
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Widened =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Widened);
            end case;
            return Value;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Elliptic_Scale_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Ticks (Layer : Elliptic_Scale_Layer)
      return Tick_Parameters is
   begin
      return Layer.Ticks;
   end Get_Ticks;

   function Get_Widened (Layer : Elliptic_Scale_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Is_Updated (Layer : Elliptic_Scale_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Elliptic_Scale_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Outer   := Layer.Outer + Offset;
      Layer.Inner   := Layer.Inner + Offset;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Elliptic_Scale_Layer
             )  is
      Outer  : Ellipse_Parameters;
      Inner  : Ellipse_Parameters;
      Ticks  : Tick_Parameters;
      Line   : Line_Parameters;
      From   : GDouble;
      Length : GDouble;
   begin
      Restore (Stream, Outer);
      Restore (Stream, Inner);
      Restore (Stream, Ticks);
      Restore (Stream, Line);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Layer.Scaled, Layer.Widened);
      Set
      (  Layer  => Layer,
         Outer  => Outer,
         Inner  => Inner,
         Line   => Line,
         Ticks  => Ticks,
         From   => From,
         Length => Length
      );
   end Restore;

   procedure Scale
             (  Layer  : in out Elliptic_Scale_Layer;
                Factor : GDouble
             )  is
      Outer : constant Ellipse_Parameters := Layer.Outer * Factor;
      Inner : constant Ellipse_Parameters := Layer.Inner * Factor;
   begin
      if Inner.Minor_Radius <= 0.0 then
         raise Constraint_Error with
            "Non-positive inner ellipse radius";
      elsif Inner.Major_Curvature < 0.0 then
         raise Constraint_Error with
            "Negative inner ellipse curvature";
      elsif Outer.Minor_Radius <= 0.0 then
         raise Constraint_Error with
            "Non-positive outer ellipse radius";
      elsif Outer.Major_Curvature < 0.0 then
         raise Constraint_Error with
            "Negative outer ellipse curvature";
      end if;
      Layer.Outer   := Outer;
      Layer.Inner   := Inner;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer  : in out Elliptic_Scale_Layer;
                Outer  : Ellipse_Parameters;
                Inner  : Ellipse_Parameters;
                Line   : Line_Parameters;
                Ticks  : Tick_Parameters;
                From   : GDouble;
                Length : GDouble
             )  is
   begin
      if Ticks.Step < Min_Step then
         raise Constraint_Error with "Step is too small";
      elsif Ticks.First > Ticks.Skipped then
         raise Constraint_Error with
            "First tick is greater than skipped";
      elsif Inner.Minor_Radius <= 0.0 then
         raise Constraint_Error with
            "Non-positive inner ellipse radius";
      elsif Inner.Major_Curvature < 0.0 then
         raise Constraint_Error with
            "Negative inner ellipse curvature";
      elsif Outer.Minor_Radius <= 0.0 then
         raise Constraint_Error with
            "Non-positive outer ellipse radius";
      elsif Outer.Major_Curvature < 0.0 then
         raise Constraint_Error with
            "Negative outer ellipse curvature";
      elsif Line.Width <= 0.0 then
         raise Constraint_Error with "Non-positive line width";
      end if;
      Layer.Outer        := Outer;
      Layer.Inner        := Inner;
      Layer.Inner.Center := Layer.Outer.Center;
      Layer.From         := From;
      Layer.Length       := Length;
      Layer.Ticks        := Ticks;
      Layer.Line         := Line;
      Layer.Updated      := True;
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Elliptic_Scale_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Outer.Center.X := Get_Double (Value);
               Layer.Inner.Center.X := Layer.Outer.Center.X;
            when Property_Center_Y =>
               Layer.Outer.Center.Y := Get_Double (Value);
               Layer.Inner.Center.Y := Layer.Outer.Center.Y;
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
            when Property_Inner_Curvature =>
               Layer.Inner.Major_Curvature := Get_Double (Value);
               if Layer.Inner.Major_Curvature < 0.0 then
                  Layer.Inner.Major_Curvature := 0.0;
               end if;
            when Property_Inner_Radius =>
               Layer.Inner.Minor_Radius := Get_Double (Value);
               if Layer.Inner.Minor_Radius < 1.0E-6 then
                  Layer.Inner.Minor_Radius := 1.0E-6;
               end if;
            when Property_Inner_Angle =>
               Layer.Inner.Angle := Get_Double (Value);
               if Layer.Inner.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Inner.Angle :=
                     GDouble'Remainder (Layer.Inner.Angle, 2.0 * Pi);
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
            when Property_Line_Width =>
               Layer.Line.Width := Get_Double (Value);
               if Layer.Line.Width < 0.0 then
                  Layer.Line.Width := 0.0;
               end if;
            when Property_Line_Color =>
               Layer.Line.Color := Get_Value (Value);
            when Property_Line_Cap =>
               Layer.Line.Line_Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Tick_Step =>
               Layer.Ticks.Step := Get_Double (Value);
               if Layer.Ticks.Step < 1.0E-6 then
                  Layer.Ticks.Step := 1.0E-6;
               end if;
            when Property_Tick_First =>
               if Get_UInt (Value) < 1 then
                  Layer.Ticks.First := 1;
               elsif Get_UInt (Value) > GUInt (Tick_Number'Last) then
                  Layer.Ticks.First := Tick_Number'Last;
               else
                  Layer.Ticks.First := Tick_Number (Get_UInt (Value));
               end if;
            when Property_Tick_Skipped =>
               if Get_UInt (Value) < 2 then
                  Layer.Ticks.Skipped := 2;
               elsif Get_UInt (Value) > GUInt (Tick_Number'Last) then
                  Layer.Ticks.Skipped := Tick_Number'Last;
               else
                  Layer.Ticks.Skipped := Tick_Number (Get_UInt (Value));
               end if;
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Get_Boolean (Value);
         end case;
         Layer.Updated := True;
      else
         raise Constraint_Error;
      end if;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Elliptic_Scale_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Widened
             (  Layer   : in out Elliptic_Scale_Layer;
                Widened : Boolean
             )  is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Elliptic_Scale_Layer
             )  is
   begin
      Store (Stream, Layer.Outer);
      Store (Stream, Layer.Inner);
      Store (Stream, Layer.Ticks);
      Store (Stream, Layer.Line);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Scaled, Layer.Widened);
   end Store;

end Gtk.Layered.Elliptic_Scale;
