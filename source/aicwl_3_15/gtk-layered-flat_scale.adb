--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Flat_Scale                      Luebeck            --
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

package body Gtk.Layered.Flat_Scale is
   type Scale_Ptr is access all Flat_Scale_Layer;

   type Layer_Property is
     (Property_Scaled,
      Property_Widened,
      Property_From_X,
      Property_From_Y,
      Property_Length,
      Property_Angle,
      Property_Tick_Length,
      Property_Tick_Step,
      Property_Tick_First,
      Property_Tick_Skipped,
      Property_Line_Width,
      Property_Line_Color,
      Property_Line_Cap);

   procedure Free is
     new Ada.Unchecked_Deallocation (Flat_Scale_Layer, Scale_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Flat_Scale_Layer
   is
      Ptr : Scale_Ptr := new Flat_Scale_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Flat_Scale
     (Under    : not null access Layer_Location'Class;
      Step     : Gdouble;
      First    : Tick_Number                := Tick_Number'Last;
      Skipped  : Tick_Number                := Tick_Number'Last;
      From     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      Length   : Gdouble                    := 1.0;
      Breadth  : Gdouble                    := 1.0;
      Angle    : Gdouble                    := 0.0;
      Width    : Gdouble                    := 1.0;
      Color    : Gdk.Color.Gdk_Color        := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap       := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                    := False;
      Widened  : Boolean                    := False)
   is
      Ptr   : Scale_Ptr := new Flat_Scale_Layer;
      Layer : Flat_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer   => Layer,
         Line    => (Width, Color, Line_Cap),
         Ticks   => (Step, Get_First_Tick (First, Skipped), Skipped),
         From    => From,
         Length  => Length,
         Breadth => Breadth,
         Angle   => Angle);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Flat_Scale;

   function Add_Flat_Scale
     (Under    : not null access Layer_Location'Class;
      Step     : Gdouble;
      First    : Tick_Number                := Tick_Number'Last;
      Skipped  : Tick_Number                := Tick_Number'Last;
      From     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      Length   : Gdouble                    := 1.0;
      Breadth  : Gdouble                    := 1.0;
      Angle    : Gdouble                    := 0.0;
      Width    : Gdouble                    := 1.0;
      Color    : Gdk.Color.Gdk_Color        := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap       := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                    := False;
      Widened  : Boolean                    := False)
      return not null access Flat_Scale_Layer
   is
      Ptr   : Scale_Ptr := new Flat_Scale_Layer;
      Layer : Flat_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer   => Layer,
         Line    => (Width, Color, Line_Cap),
         Ticks   => (Step, Get_First_Tick (First, Skipped), Skipped),
         From    => From,
         Length  => Length,
         Breadth => Breadth,
         Angle   => Angle);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Flat_Scale;

   overriding procedure Draw
     (Layer   : in out Flat_Scale_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      X_Size : Gdouble := Cos (Layer.Angle);
      Y_Size : Gdouble := Sin (Layer.Angle);
      This   : Gdouble;
      From   : Cairo.Ellipses.Cairo_Tuple;
      Width  : constant Gdouble := Layer.Breadth * 0.5;
      Thick  : Natural := Layer.Ticks.First;
      Length : constant Gdouble := Layer.Length
                 + Layer.Ticks.Step * 0.05;
      State  : Cairo.Ellipses.Context_State := Cairo.Ellipses.Save (Context);
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
            Size : constant Gdouble := Layer.Widget.all.Get_Size;
         begin
            X_Size := X_Size * Size;
            Y_Size := Y_Size * Size;
            From.X := Layer.From.X * Size + Layer.Widget.all.Get_Center.X;
            From.Y := Layer.From.Y * Size + Layer.Widget.all.Get_Center.Y;
         end;
      else
         From := Layer.From;
      end if;
      for Index in Natural'Range loop
         This := Layer.Ticks.Step * Gdouble (Index);
         exit when abs This > Length;
         if Thick = Layer.Ticks.Skipped then
            Thick := 1;
         else
            Thick := Thick + 1;
            begin
               Cairo.Move_To
                 (Cr => Context,
                  X  => From.X + This * X_Size + Width * Y_Size,
                  Y  => From.Y + This * Y_Size - Width * X_Size);
               Cairo.Line_To
                 (Cr => Context,
                  X  => From.X + This * X_Size - Width * Y_Size,
                  Y  => From.Y + This * Y_Size + Width * X_Size);
            exception
               when Constraint_Error =>
                  null;
            end;
         end if;
      end loop;
      Cairo.Stroke (Context);
      Layer.Updated := False;
   end Draw;

   function Get_Angle (Layer : Flat_Scale_Layer) return Gdouble is
   begin
      return Layer.Angle;
   end Get_Angle;

   function Get_Breadth (Layer : Flat_Scale_Layer) return Gdouble is
   begin
      return Layer.Breadth;
   end Get_Breadth;

   function Get_From
     (Layer : Flat_Scale_Layer) return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length (Layer : Flat_Scale_Layer) return Gdouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Line (Layer : Flat_Scale_Layer)
                      return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   overriding function Get_Properties_Number
     (Layer : Flat_Scale_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         -  Layer_Property'Pos (Layer_Property'First)
         +  1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Flat_Scale_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_From_X =>
               return
                 Gnew_Double
                   (Name    => "x0",
                    Nick    => "x0",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the point " &
                       "corresponding to the value 0");
            when Property_From_Y =>
               return
                 Gnew_Double
                   (Name    => "y0",
                    Nick    => "y0",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The y-coordinate of the point " &
                       "corresponding to the value 0");
            when Property_Length =>
               return
                 Gnew_Double
                   (Name    => "length",
                    Nick    => "length",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The scale length. The end of the" &
                       "scale corresponds to the value 1");
            when Property_Angle =>
               return
                 Gnew_Double
                   (Name    => "angle",
                    Nick    => "angle",
                    Minimum => -2.0 * Pi,
                    Maximum => 2.0 * Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The angle of the scale line. The " &
                       "ticks are drawn perpendicular to " &
                       "this line");
            when Property_Tick_Length =>
               return
                 Gnew_Double
                   (Name    => "tick-length",
                    Nick    => "tick length",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The length of the ticks. The ticks" &
                       "are drawn perpendicular to the " &
                       "scale line");
            when Property_Tick_Step =>
               return
                 Gnew_Double
                   (Name    => "step",
                    Nick    => "step",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The distance between two consecutive ticks");
            when Property_Tick_First =>
               return
                 Gnew_Uint
                   (Name    => "first-tick",
                    Nick    => "first tick",
                    Minimum => Guint (Tick_Number'First),
                    Maximum => Guint (Tick_Number'Last),
                    Default => 1,
                    Blurb   =>
                       "The number of the first tick. " &
                       "The first tick is located at " &
                       "the beginning of the scale");
            when Property_Tick_Skipped =>
               return
                 Gnew_Uint
                   (Name    => "skipped-tick",
                    Nick    => "skipped tick",
                    Minimum => 2,
                    Maximum => Guint (Tick_Number'Last),
                    Default => Guint (Tick_Number'Last),
                    Blurb   =>
                       "The number of the skipped tick. " &
                       "The ticks are numbered from 1 to " &
                       "skipped-tick. The ticks with this " &
                       "number are not drawn");
            when Property_Line_Width =>
               return
                 Gnew_Double
                   (Name    => "line-width",
                    Nick    => "line width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The tick line's width");
            when Property_Line_Color =>
               return
                 Gnew_Boxed
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
                 Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The scale size is changed when the widget is resized");
            when Property_Widened =>
               return
                 Gnew_Boolean
                   (Name    => "widened",
                    Nick    => "widened",
                    Default => False,
                    Blurb   =>
                       "The tick's line width is changed " &
                       "when the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Flat_Scale_Layer;
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
               when Property_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Length);
               when Property_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Angle);
               when Property_Line_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Line.Width);
               when Property_Line_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Line.Color);
               when Property_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Line.Line_Cap);
               when Property_Tick_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Breadth);
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
      end if;
   end Get_Property_Value;

   overriding function Get_Scaled (Layer : Flat_Scale_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Ticks (Layer : Flat_Scale_Layer)
                       return Tick_Parameters is
   begin
      return Layer.Ticks;
   end Get_Ticks;

   overriding function Get_Widened (Layer : Flat_Scale_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   overriding function Is_Updated (Layer : Flat_Scale_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Flat_Scale_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.From.X  := Layer.From.X + Offset.X;
      Layer.From.Y  := Layer.From.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Flat_Scale_Layer)
   is
      Ticks   : Tick_Parameters;
      Line    : Line_Parameters;
      From    : Cairo.Ellipses.Cairo_Tuple;
      Length  : Gdouble;
      Breadth : Gdouble;
      Angle   : Gdouble;
   begin
      Restore (Stream, Ticks);
      Restore (Stream, Line);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Breadth);
      Restore (Stream, Angle);
      Restore (Stream, Layer.Scaled, Layer.Widened);
      Set
        (Layer   => Layer,
         Line    => Line,
         Ticks   => Ticks,
         From    => From,
         Length  => Length,
         Breadth => Breadth,
         Angle   => Angle);
   end Restore;

   overriding procedure Scale
     (Layer  : in out Flat_Scale_Layer;
      Factor : Gdouble)
   is
      Ticks   : Tick_Parameters  := Layer.Ticks;
      Breadth : constant Gdouble := Layer.Breadth * Factor;
   begin
      Ticks.Step := Ticks.Step * Factor;
      if Ticks.Step < Min_Step then
         raise Constraint_Error with "Step is too small";
      elsif Breadth < 0.0 then
         raise Constraint_Error with "Negative tick length";
      end if;
      Layer.Length  := Layer.Length * Factor;
      Layer.Ticks   := Ticks;
      Layer.Breadth := Breadth;
      Layer.Updated := True;
   end Scale;

   procedure Set
     (Layer   : in out Flat_Scale_Layer;
      Line    : Line_Parameters;
      Ticks   : Tick_Parameters;
      From    : Cairo.Ellipses.Cairo_Tuple;
      Length  : Gdouble;
      Breadth : Gdouble;
      Angle   : Gdouble) is
   begin
      if Ticks.Step < Min_Step then
         raise Constraint_Error with "Step is too small";
      elsif Ticks.First > Ticks.Skipped then
         raise Constraint_Error with
           "First tick is greater than the skipped tick";
      elsif Breadth < 0.0 then
         raise Constraint_Error with "Negative tick length";
      elsif Length < 0.0 then
         raise Constraint_Error with "Negative scale length";
      elsif Line.Width <= 0.0 then
         raise Constraint_Error with "Non-positive line width";
      end if;
      Layer.Line    := Line;
      Layer.Ticks   := Ticks;
      Layer.From    := From;
      Layer.Length  := Length;
      Layer.Breadth := Breadth;
      Layer.Angle   := Angle;
      Layer.Updated := True;
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Flat_Scale_Layer;
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
            when Property_Length =>
               Layer.Length := Glib.Values.Get_Double (Value);
            when Property_Angle =>
               Layer.Angle := Glib.Values.Get_Double (Value);
               if Layer.Angle not in -2.0 * Pi .. 2.0 * Pi then
                  Layer.Angle :=
                    Gdouble'Remainder (Layer.Angle, 2.0 * Pi);
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
            when Property_Tick_Length =>
               Layer.Breadth := Glib.Values.Get_Double (Value);
               if Layer.Breadth < 0.0 then
                  Layer.Breadth := 0.0;
               end if;
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
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Flat_Scale_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Widened
     (Layer   : in out Flat_Scale_Layer;
      Widened : Boolean) is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Flat_Scale_Layer) is
   begin
      Store (Stream, Layer.Ticks);
      Store (Stream, Layer.Line);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Breadth);
      Store (Stream, Layer.Angle);
      Store (Stream, Layer.Scaled, Layer.Widened);
   end Store;

end Gtk.Layered.Flat_Scale;
