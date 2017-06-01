--                                                                    --
--  package Gtk.Layered.Line        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2011       --
--                                                                    --
--                                Last revision :  16:49 28 Feb 2016  --
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

package body Gtk.Layered.Line is

   pragma Warnings (Off, "declaration hides ""Center""");
   pragma Warnings (Off, "declaration hides ""Line""");

   type Line_Ptr is access all Line_Layer;

   type Layer_Property is
     (Property_Scaled,
      Property_Widened,
      Property_From_X,
      Property_From_Y,
      Property_To_X,
      Property_To_Y,
      Property_Line_Width,
      Property_Line_Color,
      Property_Line_Cap);

   procedure Free is
     new Ada.Unchecked_Deallocation (Line_Layer, Line_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Line_Layer
   is
      Ptr : Line_Ptr := new Line_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Line
     (Under    : not null access Layer_Location'Class;
      From     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      Angle    : Gdouble                    := 0.0;
      Length   : Gdouble                    := 1.0;
      Width    : Gdouble                    := 1.0;
      Color    : Gdk.Color.Gdk_Color        := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap       := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                    := False;
      Widened  : Boolean                    := False)
   is
      Ptr   : Line_Ptr := new Line_Layer;
      Layer : Line_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer   => Layer,
         From    => From,
         Angle   => Angle,
         Length  => Length,
         Line    => (Width, Color, Line_Cap));
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Line;

   procedure Add_Line
     (Under    : not null access Layer_Location'Class;
      From     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      To       : Cairo.Ellipses.Cairo_Tuple := (0.0, 1.0);
      Width    : Gdouble                    := 1.0;
      Color    : Gdk.Color.Gdk_Color        := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap       := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                    := False;
      Widened  : Boolean                    := False)
   is
      Ptr   : Line_Ptr := new Line_Layer;
      Layer : Line_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer => Layer,
         From  => From,
         To    => To,
         Line  => (Width, Color, Line_Cap));
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Line;

   function Add_Line
     (Under    : not null access Layer_Location'Class;
      From     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      Angle    : Gdouble                    := 0.0;
      Length   : Gdouble                    := 1.0;
      Width    : Gdouble                    := 1.0;
      Color    : Gdk.Color.Gdk_Color        := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap       := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                    := False;
      Widened  : Boolean                    := False)
      return not null access Line_Layer
   is
      Ptr   : Line_Ptr := new Line_Layer;
      Layer : Line_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer  => Layer,
         From   => From,
         Angle  => Angle,
         Length => Length,
         Line   => (Width, Color, Line_Cap));
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Line;

   function Add_Line
     (Under    : not null access Layer_Location'Class;
      From     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      To       : Cairo.Ellipses.Cairo_Tuple := (0.0, 1.0);
      Width    : Gdouble                    := 1.0;
      Color    : Gdk.Color.Gdk_Color        := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap       := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                    := False;
      Widened  : Boolean                    := False)
      return not null access Line_Layer
   is
      Ptr   : Line_Ptr := new Line_Layer;
      Layer : Line_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer => Layer,
         From  => From,
         To    => To,
         Line  => (Width, Color, Line_Cap));
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Line;

   overriding procedure Draw
     (Layer   : in out Line_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
   begin
      Cairo.New_Path (Context);
      if Layer.Widened then
         Cairo.Set_Line_Width
           (Context,
            Layer.Line.Width * Layer.Widget.all.Get_Size);
      else
         Cairo.Set_Line_Width (Context, Layer.Line.Width);
      end if;
      Cairo.Set_Source_Rgb
        (Context,
         Gdouble (Gdk.Color.Red   (Layer.Line.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Green (Layer.Line.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Blue  (Layer.Line.Color)) / Gdouble (Guint16'Last));
      Cairo.Set_Line_Cap (Context, Layer.Line.Line_Cap);
      if Layer.Scaled then
         declare
            Size   : constant Gdouble := Layer.Widget.all.Get_Size;
            Center : constant Cairo.Ellipses.Cairo_Tuple :=
                       Layer.Widget.all.Get_Center;
         begin
            Cairo.Move_To
              (Cr => Context,
               X  => Layer.From.X * Size + Center.X,
               Y  => Layer.From.Y * Size + Center.Y);
            Cairo.Line_To
              (Cr => Context,
               X  => Layer.To.X * Size + Center.X,
               Y  => Layer.To.Y * Size + Center.Y);
         end;
      else
         Cairo.Move_To
           (Cr => Context,
            X  => Layer.From.X,
            Y  => Layer.From.Y);
         Cairo.Line_To
           (Cr => Context,
            X  => Layer.To.X,
            Y  => Layer.To.Y);
      end if;
      Cairo.Stroke  (Context);
      Layer.Updated := False;
   end Draw;

   function Get_Angle (Layer : Line_Layer) return Gdouble is
   begin
      return
        Cairo.Elementary_Functions.Arctan
          (X => Layer.To.X - Layer.From.X,
           Y => Layer.To.Y - Layer.From.Y);
   end Get_Angle;

   function Get_From (Layer : Line_Layer) return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Line_Layer) return Gdouble is
   begin
      return
        Cairo.Elementary_Functions.Sqrt
          ((Layer.To.X - Layer.From.X) ** 2 +
           (Layer.To.Y - Layer.From.Y) ** 2);
   end Get_Length;

   function Get_Line (Layer : Line_Layer) return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   overriding function Get_Properties_Number (Layer : Line_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last) -
             Layer_Property'Pos (Layer_Property'First) + 1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Line_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_From_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x0",
                    Nick    => "x0",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the point corresponding to the " &
                       "value 0");
            when Property_From_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y0",
                    Nick    => "y0",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The y-coordinate of the point corresponding to the " &
                       "value 0");
            when Property_To_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x1",
                    Nick    => "x1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the point corresponding to the " &
                       "value 1");
            when Property_To_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y1",
                    Nick    => "y1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The y-coordinate of the point corresponding to the " &
                       "value 1");
            when Property_Line_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "width",
                    Nick    => "width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The line width");
            when Property_Line_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The line color");
            when Property_Line_Cap =>
               return
                 Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "line-cap",
                    Nick    => "line cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the line");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The line size is changed when the widget is resized");
            when Property_Widened =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "widened",
                    Nick    => "widened",
                    Default => False,
                    Blurb   =>
                       "The line width is changed when the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Line_Layer;
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
               when Property_Line_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Line.Width);
               when Property_Line_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Line.Color);
               when Property_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Line.Line_Cap);
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

   overriding function Get_Scaled (Layer : Line_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_To (Layer : Line_Layer) return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.To;
   end Get_To;

   overriding function Get_Widened (Layer : Line_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   overriding function Is_Updated (Layer : Line_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Line_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.From.X  := Layer.From.X + Offset.X;
      Layer.From.Y  := Layer.From.Y + Offset.Y;
      Layer.To.X    := Layer.To.X   + Offset.X;
      Layer.To.Y    := Layer.To.Y   + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Line_Layer)
   is
      From : Cairo.Ellipses.Cairo_Tuple;
      To   : Cairo.Ellipses.Cairo_Tuple;
      Line : Line_Parameters;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, To);
      Gtk.Layered.Stream_IO.Restore (Stream, Line);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled, Layer.Widened);
      Set
        (Layer => Layer,
         From  => From,
         To    => To,
         Line  => Line);
   end Restore;

   overriding procedure Scale
     (Layer  : in out Line_Layer;
      Factor : Gdouble) is
   begin
      Layer.From.X  := Layer.From.X * Factor;
      Layer.From.Y  := Layer.From.Y * Factor;
      Layer.To.X    := Layer.To.X   * Factor;
      Layer.To.Y    := Layer.To.Y   * Factor;
      Layer.Updated := True;
   end Scale;

   procedure Set
     (Layer  : in out Line_Layer;
      From   : Cairo.Ellipses.Cairo_Tuple;
      Angle  : Gdouble;
      Length : Gdouble;
      Line   : Line_Parameters) is
   begin
      Set
        (Layer => Layer,
         Line  => Line,
         From  => From,
         To    =>
           (X => From.X + Length * Cairo.Elementary_Functions.Cos (Angle),
            Y => From.Y + Length * Cairo.Elementary_Functions.Sin (Angle)));
   end Set;

   procedure Set
     (Layer : in out Line_Layer;
      From  : Cairo.Ellipses.Cairo_Tuple;
      To    : Cairo.Ellipses.Cairo_Tuple;
      Line  : Line_Parameters) is
   begin
      if Line.Width <= 0.0 then
         raise Constraint_Error with "Non-positive line width";
      end if;
      Layer.From    := From;
      Layer.To      := To;
      Layer.Line    := Line;
      Layer.Updated := True;
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Line_Layer;
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
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Glib.Values.Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Line_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Widened
     (Layer   : in out Line_Layer;
      Widened : Boolean) is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Line_Layer) is
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.From);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.To);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Line);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Scaled, Layer.Widened);
   end Store;

   pragma Warnings (On, "declaration hides ""Center""");
   pragma Warnings (On, "declaration hides ""Line""");

end Gtk.Layered.Line;
