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
-- __________________________________________________________________ --

with Ada.Unchecked_Deallocation;

with Cairo.Line_Cap_Property;

with Glib.Properties.Creation;

with Gtk.Layered.Stream_IO;

package body Gtk.Layered.Elliptic_Scale is

   pragma Warnings (Off, "declaration hides ""Center""");

   type Scale_Ptr is access all Elliptic_Scale_Layer;

   type Layer_Property is
     (Property_Scaled,
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
      Property_Line_Cap);

   procedure Free is
      new Ada.Unchecked_Deallocation (Elliptic_Scale_Layer, Scale_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Elliptic_Scale_Layer
   is
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
     (Under    : not null access Layer_Location'Class;
      Step     : Gdouble;
      First    : Tick_Number                       := Tick_Number'Last;
      Skipped  : Tick_Number                       := Tick_Number'Last;
      Outer    : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle;
      Inner    : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle / 2.0;
      From     : Gdouble                           := 0.0;
      Length   : Gdouble                           := 2.0 * Ada.Numerics.Pi;
      Width    : Gdouble                           := 1.0;
      Color    : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap              := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                           := False;
      Widened  : Boolean                           := False)
   is
      Ptr   : Scale_Ptr := new Elliptic_Scale_Layer;
      Layer : Elliptic_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer  => Layer,
         Outer  => Outer,
         Inner  => Inner,
         Line   => (Width, Color, Line_Cap),
         Ticks  => (Step, Get_First_Tick (First, Skipped), Skipped),
         From   => From,
         Length => Length);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Scale;

   function Add_Elliptic_Scale
     (Under    : not null access Layer_Location'Class;
      Step     : Gdouble;
      First    : Tick_Number                       := Tick_Number'Last;
      Skipped  : Tick_Number                       := Tick_Number'Last;
      Outer    : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle;
      Inner    : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle / 2.0;
      From     : Gdouble                           := 0.0;
      Length   : Gdouble                           := 2.0 * Ada.Numerics.Pi;
      Width    : Gdouble                           := 1.0;
      Color    : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap              := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                           := False;
      Widened  : Boolean                           := False)
      return not null access Elliptic_Scale_Layer
   is
      Ptr   : Scale_Ptr := new Elliptic_Scale_Layer;
      Layer : Elliptic_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer  => Layer,
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

   overriding procedure Draw
     (Layer   : in out Elliptic_Scale_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      This     : Gdouble;
      Thick    : Natural := Layer.Ticks.First;
      From, To : Cairo.Ellipses.Cairo_Tuple;
      Length   : constant Gdouble := abs Layer.Length +
                                     abs Layer.Ticks.Step * 0.05;
      State    : Cairo.Ellipses.Context_State := Cairo.Ellipses.Save (Context);
      pragma Unreferenced (State);
   begin
      Cairo.New_Path (Context);
      Cairo.Set_Source_Rgb
        (Context,
         Gdouble (Gdk.Color.Red   (Layer.Line.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Green (Layer.Line.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Blue  (Layer.Line.Color)) / Gdouble (Guint16'Last));
      Cairo.Set_Line_Cap (Context, Layer.Line.Line_Cap);
      if Layer.Widened then
         Cairo.Set_Line_Width
           (Context,
            Layer.Line.Width * Layer.Widget.all.Get_Size);
      else
         Cairo.Set_Line_Width (Context, Layer.Line.Width);
      end if;
      if Layer.Scaled then
         declare
            Center : constant Cairo.Ellipses.Cairo_Tuple := Layer.Widget.all.Get_Center;
            Size   : constant Gdouble     := Layer.Widget.all.Get_Size;
            Inner  : constant Cairo.Ellipses.Ellipse_Parameters :=
                        Layer.Inner * Size + Center;
            Outer  : constant Cairo.Ellipses.Ellipse_Parameters :=
                        Layer.Outer * Size + Center;
         begin
            for Index in Natural'Range loop
               This := Layer.Ticks.Step * Gdouble (Index);
               exit when abs This > Length;
               if Thick = Layer.Ticks.Skipped then
                  Thick := 1;
               else
                  Thick := Thick + 1;
                  begin
                     From :=
                        Cairo.Ellipses.Get_Point (Inner, Inner * (Layer.From + This));
                     To :=
                        Cairo.Ellipses.Get_Point (Outer, Outer * (Layer.From + This));
                     Cairo.Move_To (Context, From.X, From.Y);
                     Cairo.Line_To (Context, To.X,   To.Y);
                  exception
                     when Constraint_Error =>
                        null;
                  end;
               end if;
            end loop;
         end;
      else
         for Index in Natural'Range loop
            This := Layer.Ticks.Step * Gdouble (Index);
            exit when abs This > Length;
            if Thick = Layer.Ticks.Skipped then
               Thick := 1;
            else
               Thick := Thick + 1;
               begin
                  From :=
                    Cairo.Ellipses.Get_Point
                      (Layer.Inner,
                       Layer.Inner * (Layer.From + This));
                  To :=
                    Cairo.Ellipses.Get_Point
                      (Layer.Outer,
                       Layer.Outer * (Layer.From + This));
                  Cairo.Move_To (Context, From.X, From.Y);
                  Cairo.Line_To (Context, To.X,   To.Y);
               exception
                  when Constraint_Error =>
                     null;
               end;
            end if;
         end loop;
      end if;
      Cairo.Stroke (Context);
      Layer.Updated := False;
   end Draw;

   function Get_From (Layer : Elliptic_Scale_Layer) return Gdouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Inner (Layer : Elliptic_Scale_Layer)
      return Cairo.Ellipses.Ellipse_Parameters is
   begin
      return Layer.Inner;
   end Get_Inner;

   function Get_Length (Layer : Elliptic_Scale_Layer) return Gdouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Line (Layer : Elliptic_Scale_Layer)
      return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   function Get_Outer (Layer : Elliptic_Scale_Layer)
      return Cairo.Ellipses.Ellipse_Parameters is
   begin
      return Layer.Outer;
   end Get_Outer;

   overriding function Get_Properties_Number
     (Layer : Elliptic_Scale_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last) -
             Layer_Property'Pos (Layer_Property'First) + 1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Elliptic_Scale_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x",
                    Nick    => "x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the outer and inner ellipses' " &
                       "center");
            when Property_Center_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y",
                    Nick    => "y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The y-coordinate of the outer and inner ellipses' " &
                       "center");
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
                       "The radius of the minor axis of the outer ellipse");
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
            when Property_Inner_Curvature =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-k",
                    Nick    => "inner k",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The curvature of the major axis of the inner ellipse");
            when Property_Inner_Radius =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-r",
                    Nick    => "inner r",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 0.5,
                    Blurb   =>
                       "The radius of the minor axis of the inner ellipse");
            when Property_Inner_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "inner-angle",
                    Nick    => "inner angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The angle of the major axis of the outer ellipse");
            when Property_From =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "from",
                    Nick    => "from",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The angle of the scale beginning, which corresponds " &
                       "to the first tick");
            when Property_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "length",
                    Nick    => "length",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angular length of the scale");
            when Property_Tick_Step =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "step",
                    Nick    => "step",
                    Minimum => 1.0E-6,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => Ada.Numerics.Pi / 12.0,
                    Blurb   =>
                       "The angular distance between two consequent ticks");
            when Property_Tick_First =>
               return
                 Glib.Properties.Creation.Gnew_Uint
                   (Name    => "first-tick",
                    Nick    => "first tick",
                    Minimum => Guint (Tick_Number'First),
                    Maximum => Guint (Tick_Number'Last),
                    Default => 1,
                    Blurb   =>
                       "The number of the first tick. The first tick is " &
                       "located at the beginning of the scale");
            when Property_Tick_Skipped =>
               return
                 Glib.Properties.Creation.Gnew_Uint
                   (Name    => "skipped-tick",
                    Nick    => "skipped tick",
                    Minimum => 2,
                    Maximum => Guint (Tick_Number'Last),
                    Default => Guint (Tick_Number'Last),
                    Blurb   =>
                       "The number of the skipped tick. The ticks are " &
                       "numbered from 1 to skipped-tick. The ticks with this " &
                       "number are not drawn");
            when Property_Line_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "line-width",
                    Nick    => "line width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The tick line's width");
            when Property_Line_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The tick line's color");
            when Property_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "line-cap",
                    Nick    => "line cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the tick lines");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The scale size is changed when the widget is resized");
            when Property_Widened =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "widened",
                    Nick    => "widened",
                    Default => False,
                    Blurb   =>
                       "The tick's line width is changed when the widget is " &
                       "resized");
         end case;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Elliptic_Scale_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property <= Get_Properties_Number (Layer) then
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Center_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Outer.Center.X);
               when Property_Center_Y =>
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
               when Property_Inner_Curvature =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Inner.Major_Curvature);
               when Property_Inner_Radius =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Inner.Minor_Radius);
               when Property_Inner_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Inner.Angle);
               when Property_From =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From);
               when Property_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Length);
               when Property_Line_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Line.Width);
               when Property_Line_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Line.Color);
               when Property_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Line.Line_Cap);
               when Property_Tick_Step =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Ticks.Step);
               when Property_Tick_First =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint (Value, Guint (Layer.Ticks.First));
               when Property_Tick_Skipped =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint (Value, Guint (Layer.Ticks.Skipped));
               when Property_Scaled =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Scaled);
               when Property_Widened =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Widened);
            end case;
            return Value;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Property_Value;

   overriding function Get_Scaled (Layer : Elliptic_Scale_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Ticks (Layer : Elliptic_Scale_Layer)
      return Tick_Parameters is
   begin
      return Layer.Ticks;
   end Get_Ticks;

   overriding function Get_Widened (Layer : Elliptic_Scale_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   overriding function Is_Updated (Layer : Elliptic_Scale_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Elliptic_Scale_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.Outer   := Layer.Outer + Offset;
      Layer.Inner   := Layer.Inner + Offset;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Elliptic_Scale_Layer)
   is
      Outer  : Cairo.Ellipses.Ellipse_Parameters;
      Inner  : Cairo.Ellipses.Ellipse_Parameters;
      Ticks  : Tick_Parameters;
      Line   : Line_Parameters;
      From   : Gdouble;
      Length : Gdouble;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Outer);
      Gtk.Layered.Stream_IO.Restore (Stream, Inner);
      Gtk.Layered.Stream_IO.Restore (Stream, Ticks);
      Gtk.Layered.Stream_IO.Restore (Stream, Line);
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, Length);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled, Layer.Widened);
      Set
        (Layer  => Layer,
         Outer  => Outer,
         Inner  => Inner,
         Line   => Line,
         Ticks  => Ticks,
         From   => From,
         Length => Length);
   end Restore;

   overriding procedure Scale
     (Layer  : in out Elliptic_Scale_Layer;
      Factor : Gdouble)
   is
      Outer : constant Cairo.Ellipses.Ellipse_Parameters := Layer.Outer * Factor;
      Inner : constant Cairo.Ellipses.Ellipse_Parameters := Layer.Inner * Factor;
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
     (Layer  : in out Elliptic_Scale_Layer;
      Outer  : Cairo.Ellipses.Ellipse_Parameters;
      Inner  : Cairo.Ellipses.Ellipse_Parameters;
      Line   : Line_Parameters;
      Ticks  : Tick_Parameters;
      From   : Gdouble;
      Length : Gdouble) is
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

   overriding procedure Set_Property_Value
     (Layer    : in out Elliptic_Scale_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property <= Get_Properties_Number (Layer) then
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Outer.Center.X := Glib.Values.Get_Double (Value);
               Layer.Inner.Center.X := Layer.Outer.Center.X;
            when Property_Center_Y =>
               Layer.Outer.Center.Y := Glib.Values.Get_Double (Value);
               Layer.Inner.Center.Y := Layer.Outer.Center.Y;
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
            when Property_Inner_Curvature =>
               Layer.Inner.Major_Curvature := Glib.Values.Get_Double (Value);
               if Layer.Inner.Major_Curvature < 0.0 then
                  Layer.Inner.Major_Curvature := 0.0;
               end if;
            when Property_Inner_Radius =>
               Layer.Inner.Minor_Radius := Glib.Values.Get_Double (Value);
               if Layer.Inner.Minor_Radius < 1.0E-6 then
                  Layer.Inner.Minor_Radius := 1.0E-6;
               end if;
            when Property_Inner_Angle =>
               Layer.Inner.Angle := Glib.Values.Get_Double (Value);
               if
                 Layer.Inner.Angle not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Inner.Angle :=
                    Gdouble'Remainder
                      (Layer.Inner.Angle, 2.0 * Ada.Numerics.Pi);
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
            when Property_Line_Width =>
               Layer.Line.Width := Glib.Values.Get_Double (Value);
               if Layer.Line.Width < 0.0 then
                  Layer.Line.Width := 0.0;
               end if;
            when Property_Line_Color =>
               Layer.Line.Color := Gdk.Color.Get_Value (Value);
            when Property_Line_Cap =>
               Layer.Line.Line_Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
            when Property_Tick_Step =>
               Layer.Ticks.Step := Glib.Values.Get_Double (Value);
               if Layer.Ticks.Step < 1.0E-6 then
                  Layer.Ticks.Step := 1.0E-6;
               end if;
            when Property_Tick_First =>
               if Glib.Values.Get_Uint (Value) < 1 then
                  Layer.Ticks.First := 1;
               elsif Glib.Values.Get_Uint (Value) > Guint (Tick_Number'Last) then
                  Layer.Ticks.First := Tick_Number'Last;
               else
                  Layer.Ticks.First := Tick_Number (Glib.Values.Get_Uint (Value));
               end if;
            when Property_Tick_Skipped =>
               if Glib.Values.Get_Uint (Value) < 2 then
                  Layer.Ticks.Skipped := 2;
               elsif Glib.Values.Get_Uint (Value) > Guint (Tick_Number'Last) then
                  Layer.Ticks.Skipped := Tick_Number'Last;
               else
                  Layer.Ticks.Skipped := Tick_Number (Glib.Values.Get_Uint (Value));
               end if;
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Glib.Values.Get_Boolean (Value);
         end case;
         Layer.Updated := True;
      else
         raise Constraint_Error;
      end if;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Elliptic_Scale_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Widened
     (Layer   : in out Elliptic_Scale_Layer;
      Widened : Boolean) is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Elliptic_Scale_Layer) is
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Outer);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Inner);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Ticks);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Line);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.From);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Length);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Scaled, Layer.Widened);
   end Store;

   pragma Warnings (On, "declaration hides ""Center""");

end Gtk.Layered.Elliptic_Scale;
