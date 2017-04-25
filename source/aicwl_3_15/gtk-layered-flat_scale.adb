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
--____________________________________________________________________--

with Ada.Numerics;                use Ada.Numerics;
with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;

package body Gtk.Layered.Flat_Scale is
   type Scale_Ptr is access all Flat_Scale_Layer;

   type Layer_Property is
        (  Property_Scaled,
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
           Property_Line_Cap
        );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Flat_Scale." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation (Flat_Scale_Layer, Scale_Ptr);

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Flat_Scale_Layer is
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
             (  Under    : not null access Layer_Location'Class;
                Step     : GDouble;
                First    : Tick_Number    := Tick_Number'Last;
                Skipped  : Tick_Number    := Tick_Number'Last;
                From     : Cairo_Tuple    := (0.0, 0.0);
                Length   : GDouble        := 1.0;
                Breadth  : GDouble        := 1.0;
                Angle    : GDouble        := 0.0;
                Width    : GDouble        := 1.0;
                Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
                Line_Cap : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
                Scaled   : Boolean        := False;
                Widened  : Boolean        := False
             )  is
      Ptr   : Scale_Ptr := new Flat_Scale_Layer;
      Layer : Flat_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Line    => (Width, Color, Line_Cap),
         Ticks   => (Step, Get_First_Tick (First, Skipped), Skipped),
         From    => From,
         Length  => Length,
         Breadth => Breadth,
         Angle   => Angle
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Flat_Scale;

   function Add_Flat_Scale
            (  Under    : not null access Layer_Location'Class;
               Step     : GDouble;
               First    : Tick_Number    := Tick_Number'Last;
               Skipped  : Tick_Number    := Tick_Number'Last;
               From     : Cairo_Tuple    := (0.0, 0.0);
               Length   : GDouble        := 1.0;
               Breadth  : GDouble        := 1.0;
               Angle    : GDouble        := 0.0;
               Width    : GDouble        := 1.0;
               Color    : Gdk_Color      := RGB (0.0, 0.0, 0.0);
               Line_Cap : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
               Scaled   : Boolean        := False;
               Widened  : Boolean        := False
            )  return not null access Flat_Scale_Layer is
      Ptr   : Scale_Ptr := new Flat_Scale_Layer;
      Layer : Flat_Scale_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Line    => (Width, Color, Line_Cap),
         Ticks   => (Step, Get_First_Tick (First, Skipped), Skipped),
         From    => From,
         Length  => Length,
         Breadth => Breadth,
         Angle   => Angle
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Flat_Scale;

   procedure Draw
             (  Layer   : in out Flat_Scale_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      X_Size : GDouble := cos (Layer.Angle);
      Y_Size : GDouble := sin (Layer.Angle);
      This   : GDouble;
      From   : Cairo_Tuple;
      Width  : constant GDouble := Layer.Breadth * 0.5;
      Thick  : Natural := Layer.Ticks.First;
      Length : constant GDouble := Layer.Length
                                 + Layer.Ticks.Step * 0.05;
      State  : Context_State := Save (Context);
   begin
      New_Path (Context);
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Layer.Line.Color)) / GDouble (Guint16'Last),
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
            Size : constant GDouble := Layer.Widget.Get_Size;
         begin
            X_Size := X_Size * Size;
            Y_Size := Y_Size * Size;
            From.X := Layer.From.X * Size + Layer.Widget.Get_Center.X;
            From.Y := Layer.From.Y * Size + Layer.Widget.Get_Center.Y;
         end;
      else
         From := Layer.From;
      end if;
      for Index in Natural'Range loop
         This := Layer.Ticks.Step * GDouble (Index);
         exit when abs This > Length;
         if Thick = Layer.Ticks.Skipped then
            Thick := 1;
         else
            Thick := Thick + 1;
            begin
               Move_To
               (  Cr => Context,
                  X  => From.X + This * X_Size + Width * Y_Size,
                  Y  => From.Y + This * Y_Size - Width * X_Size
               );
               Line_To
               (  Cr => Context,
                  X  => From.X + This * X_Size - Width * Y_Size,
                  Y  => From.Y + This * Y_Size + Width * X_Size
               );
            exception
               when Constraint_Error =>
                  null;
            end;
         end if;
      end loop;
      Stroke (Context);
      Layer.Updated := False;
   end Draw;

   function Get_Angle (Layer : Flat_Scale_Layer) return GDouble is
   begin
      return Layer.Angle;
   end Get_Angle;

   function Get_Breadth (Layer : Flat_Scale_Layer) return GDouble is
   begin
      return Layer.Breadth;
   end Get_Breadth;

   function Get_From (Layer : Flat_Scale_Layer) return Cairo_Tuple is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length (Layer : Flat_Scale_Layer) return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Line (Layer : Flat_Scale_Layer)
      return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   function Get_Properties_Number
            (  Layer : Flat_Scale_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Flat_Scale_Layer;
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
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the point " &
                                "corresponding to the value 0"
                  );
            when Property_From_Y =>
               return
                  Gnew_Double
                  (  Name    => "y0",
                     Nick    => "y0",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the point " &
                                "corresponding to the value 0"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The scale length. The end of the" &
                                "scale corresponds to the value 1"
                  );
            when Property_Angle =>
               return
                  Gnew_Double
                  (  Name    => "angle",
                     Nick    => "angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the scale line. The " &
                                "ticks are drawn perpendicular to " &
                                "this line"
                  );
            when Property_Tick_Length =>
               return
                  Gnew_Double
                  (  Name    => "tick-length",
                     Nick    => "tick length",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The length of the ticks. The ticks" &
                                "are drawn perpendicular to the " &
                                "scale line"
                  );
            when Property_Tick_Step =>
               return
                  Gnew_Double
                  (  Name    => "step",
                     Nick    => "step",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The distance between two " &
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
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Flat_Scale_Layer;
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
               when Property_From_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From.X);
               when Property_From_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From.Y);
               when Property_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Length);
               when Property_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Angle);
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
               when Property_Tick_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Breadth);
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
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Flat_Scale_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Ticks (Layer : Flat_Scale_Layer)
      return Tick_Parameters is
   begin
      return Layer.Ticks;
   end Get_Ticks;

   function Get_Widened (Layer : Flat_Scale_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Is_Updated (Layer : Flat_Scale_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Flat_Scale_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.From.X  := Layer.From.X + Offset.X;
      Layer.From.Y  := Layer.From.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Flat_Scale_Layer
             )  is
      Ticks   : Tick_Parameters;
      Line    : Line_Parameters;
      From    : Cairo_Tuple;
      Length  : GDouble;
      Breadth : GDouble;
      Angle   : GDouble;
   begin
      Restore (Stream, Ticks);
      Restore (Stream, Line);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Breadth);
      Restore (Stream, Angle);
      Restore (Stream, Layer.Scaled, Layer.Widened);
      Set
      (  Layer   => Layer,
         Line    => Line,
         Ticks   => Ticks,
         From    => From,
         Length  => Length,
         Breadth => Breadth,
         Angle   => Angle
      );
   end Restore;

   procedure Scale
             (  Layer  : in out Flat_Scale_Layer;
                Factor : GDouble
             )  is
      Ticks   : Tick_Parameters  := Layer.Ticks;
      Breadth : constant GDouble := Layer.Breadth * Factor;
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
             (  Layer   : in out Flat_Scale_Layer;
                Line    : Line_Parameters;
                Ticks   : Tick_Parameters;
                From    : Cairo_Tuple;
                Length  : GDouble;
                Breadth : GDouble;
                Angle   : GDouble
             )  is
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

   procedure Set_Property_Value
             (  Layer    : in out Flat_Scale_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_From_X =>
               Layer.From.X := Get_Double (Value);
            when Property_From_Y =>
               Layer.From.Y := Get_Double (Value);
            when Property_Length =>
               Layer.Length := Get_Double (Value);
            when Property_Angle =>
               Layer.Angle := Get_Double (Value);
               if Layer.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Angle :=
                     GDouble'Remainder (Layer.Angle, 2.0 * Pi);
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
            when Property_Tick_Length =>
               Layer.Breadth := Get_Double (Value);
               if Layer.Breadth < 0.0 then
                  Layer.Breadth := 0.0;
               end if;
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
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Flat_Scale_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Widened
             (  Layer   : in out Flat_Scale_Layer;
                Widened : Boolean
             )  is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Flat_Scale_Layer
             )  is
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
