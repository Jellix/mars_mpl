--                                                                    --
--  package Gtk.Layered.Arc         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2010       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

package body Gtk.Layered.Arc is
   type Arc_Ptr is access all Arc_Layer;

   type Layer_Property is
     (Property_Scaled,
      Property_Widened,
      Property_Center_X,
      Property_Center_Y,
      Property_Curvature,
      Property_Radius,
      Property_Angle,
      Property_From,
      Property_Length,
      Property_Line_Width,
      Property_Line_Color,
      Property_Line_Cap);

   procedure Free is
      new Ada.Unchecked_Deallocation (Arc_Layer, Arc_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Arc_Layer
   is
      Ptr : Arc_Ptr := new Arc_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Arc
     (Under    : not null access Layer_Location'Class;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle;
      From     : Gdouble                           := 0.0;
      Length   : Gdouble                           := 2.0 * Ada.Numerics.Pi;
      Width    : Gdouble                           := 1.0;
      Color    : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap              := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                           := False;
      Widened  : Boolean                           := False)
   is
      Ptr   : Arc_Ptr := new Arc_Layer;
      Layer : Arc_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer   => Layer,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Line    => (Width, Color, Line_Cap));
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Arc;

   function Add_Arc
     (Under    : not null access Layer_Location'Class;
      Ellipse  : Cairo.Ellipses.Ellipse_Parameters := Cairo.Ellipses.Unit_Circle;
      From     : Gdouble                           := 0.0;
      Length   : Gdouble                           := 2.0 * Ada.Numerics.Pi;
      Width    : Gdouble                           := 1.0;
      Color    : Gdk.Color.Gdk_Color               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Cap : Cairo.Cairo_Line_Cap              := Cairo.Cairo_Line_Cap_Butt;
      Scaled   : Boolean                           := False;
      Widened  : Boolean                           := False)
      return not null access Arc_Layer
   is
      Ptr   : Arc_Ptr := new Arc_Layer;
      Layer : Arc_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer   => Layer,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Line    => (Width, Color, Line_Cap));
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Arc;

   overriding procedure Draw
     (Layer   : in out Arc_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      use type Cairo.Ellipses.Ellipse_Parameters;
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
         Cairo.Ellipses.Elliptic_Arc_Abs
           (Context,
            Layer.Ellipse * Layer.Widget.all.Get_Size +
              Layer.Widget.all.Get_Center,
            Layer.From,
            Layer.Length);
      else
         Cairo.Ellipses.Elliptic_Arc_Abs
           (Context,
            Layer.Ellipse,
            Layer.From,
            Layer.Length);
      end if;
      Cairo.Stroke  (Context);
      Layer.Updated := False;
   end Draw;

   function Get_Ellipse
     (Layer : Arc_Layer) return Cairo.Ellipses.Ellipse_Parameters is
   begin
      return Layer.Ellipse;
   end Get_Ellipse;

   function Get_From (Layer : Arc_Layer) return Gdouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Arc_Layer) return Gdouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Line (Layer : Arc_Layer) return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   overriding function Get_Properties_Number (Layer : Arc_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         -  Layer_Property'Pos (Layer_Property'First)
         +  1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Arc_Layer;
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
                    Blurb   => "The x-coordinate of the arc's ellipse center");
            when Property_Center_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y",
                    Nick    => "y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The y-coordinate of the arc's ellipse center");
            when Property_Curvature =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "k",
                    Nick    => "k",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The curvature of the arc's ellipse major axis");
            when Property_Radius =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "r",
                    Nick    => "r",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 0.5,
                    Blurb   => "The radius of the arc's ellipse minor axis");
            when Property_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "angle",
                    Nick    => "angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The angle of the major ellipse axis of the arc");
            when Property_From =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "from",
                    Nick    => "from",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angle of the arc beginning");
            when Property_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "length",
                    Nick    => "length",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angular length of the arc");
            when Property_Line_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "width",
                    Nick    => "width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The arc line width");
            when Property_Line_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The arc's line color");
            when Property_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "line-cap",
                    Nick    => "line cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the arc's line");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The arc size is changed when " &
                       "the widget is resized");
            when Property_Widened =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "widened",
                    Nick    => "widened",
                    Default => False,
                    Blurb   =>
                       "The arc's line width is changed " &
                       "when the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Arc_Layer;
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
                  Glib.Values.Set_Double (Value, Layer.Ellipse.Center.X);
               when Property_Center_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Ellipse.Center.Y);
               when Property_Curvature =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Ellipse.Major_Curvature);
               when Property_Radius =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Ellipse.Minor_Radius);
               when Property_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Ellipse.Angle);
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

   overriding function Get_Scaled (Layer : Arc_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   overriding function Get_Widened (Layer : Arc_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   overriding function Is_Updated (Layer : Arc_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Arc_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      Layer.Ellipse := Layer.Ellipse + Offset;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Arc_Layer)
   is
      Ellipse : Cairo.Ellipses.Ellipse_Parameters;
      From    : Gdouble;
      Length  : Gdouble;
      Line    : Line_Parameters;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Ellipse);
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, Length);
      Gtk.Layered.Stream_IO.Restore (Stream, Line);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled, Layer.Widened);
      Set
        (Layer   => Layer,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Line    => Line);
   end Restore;

   overriding procedure Scale
     (Layer  : in out Arc_Layer;
      Factor : Gdouble)
   is
      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      Set
        (Layer   => Layer,
         Ellipse => Layer.Ellipse * Factor,
         From    => Layer.From,
         Length  => Layer.Length,
         Line    => Layer.Line);
   end Scale;

   procedure Set
     (Layer   : in out Arc_Layer;
      Ellipse : Cairo.Ellipses.Ellipse_Parameters;
      From    : Gdouble;
      Length  : Gdouble;
      Line    : Line_Parameters) is
   begin
      if Ellipse.Minor_Radius <= 0.0 then
         raise Constraint_Error with "Non-positive ellipse radius";
      elsif Ellipse.Major_Curvature < 0.0 then
         raise Constraint_Error with "Negative ellipse curvature";
      elsif Line.Width <= 0.0 then
         raise Constraint_Error with "Non-positive line width";
      end if;
      Layer.Ellipse := Ellipse;
      Layer.From    := From;
      Layer.Length  := Length;
      Layer.Line    := Line;
      Layer.Updated := True;
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Arc_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Ellipse.Center.X := Glib.Values.Get_Double (Value);
            when Property_Center_Y =>
               Layer.Ellipse.Center.Y := Glib.Values.Get_Double (Value);
            when Property_Curvature =>
               Layer.Ellipse.Major_Curvature := Glib.Values.Get_Double (Value);
               if Layer.Ellipse.Major_Curvature < 0.0 then
                  Layer.Ellipse.Major_Curvature := 0.0;
               end if;
            when Property_Radius =>
               Layer.Ellipse.Minor_Radius := Glib.Values.Get_Double (Value);
               if Layer.Ellipse.Minor_Radius < 1.0E-6 then
                  Layer.Ellipse.Minor_Radius := 1.0E-6;
               end if;
            when Property_Angle =>
               Layer.Ellipse.Angle := Glib.Values.Get_Double (Value);
               if
                 Layer.Ellipse.Angle not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Ellipse.Angle :=
                    Gdouble'Remainder (Layer.Ellipse.Angle,
                                       2.0 * Ada.Numerics.Pi);
               end if;
            when Property_From =>
               Layer.From := Glib.Values.Get_Double (Value);
               if
                 Layer.From not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.From := Gdouble'Remainder (Layer.From,
                                                   2.0 * Ada.Numerics.Pi);
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
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Glib.Values.Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Arc_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Widened
     (Layer   : in out Arc_Layer;
      Widened : Boolean) is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Arc_Layer) is
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Ellipse);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.From);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Length);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Line);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Scaled, Layer.Widened);
   end Store;

end Gtk.Layered.Arc;
