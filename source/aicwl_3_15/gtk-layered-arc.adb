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
--____________________________________________________________________--

with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;     use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;

package body Gtk.Layered.Arc is
   type Arc_Ptr is access all Arc_Layer;

   type Layer_Property is
        (  Property_Scaled,
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
           Property_Line_Cap
        );

   procedure Free is
      new Ada.Unchecked_Deallocation (Arc_Layer, Arc_Ptr);

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Arc_Layer is
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
             (  Under    : not null access Layer_Location'Class;
                Ellipse  : Ellipse_Parameters := Unit_Circle;
                From     : GDouble            := 0.0;
                Length   : GDouble            := 2.0 * Pi;
                Width    : GDouble            := 1.0;
                Color    : Gdk_Color          := RGB (0.0, 0.0, 0.0);
                Line_Cap : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
                Scaled   : Boolean            := False;
                Widened  : Boolean            := False
             )  is
      Ptr   : Arc_Ptr := new Arc_Layer;
      Layer : Arc_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Line    => (Width, Color, Line_Cap)
      );
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Arc;

   function Add_Arc
            (  Under    : not null access Layer_Location'Class;
               Ellipse  : Ellipse_Parameters := Unit_Circle;
               From     : GDouble            := 0.0;
               Length   : GDouble            := 2.0 * Pi;
               Width    : GDouble            := 1.0;
               Color    : Gdk_Color          := RGB (0.0, 0.0, 0.0);
               Line_Cap : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
               Scaled   : Boolean            := False;
               Widened  : Boolean            := False
            )  return not null access Arc_Layer is
      Ptr   : Arc_Ptr := new Arc_Layer;
      Layer : Arc_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Line    => (Width, Color, Line_Cap)
      );
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Arc;

   procedure Draw
             (  Layer   : in out Arc_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
   begin
      New_Path (Context);
      if Layer.Widened then
         Set_Line_Width
         (  Context,
            Layer.Line.Width * Layer.Widget.Get_Size
         );
      else
         Set_Line_Width (Context, Layer.Line.Width);
      end if;
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Layer.Line.Color)) / GDouble (Guint16'Last),
         GDouble (Green (Layer.Line.Color)) / GDouble (Guint16'Last),
         GDouble (Blue  (Layer.Line.Color)) / GDouble (Guint16'Last)
      );
      Set_Line_Cap (Context, Layer.Line.Line_Cap);
      if Layer.Scaled then
         Elliptic_Arc_Abs
         (  Context,
            (  Layer.Ellipse * Layer.Widget.Get_Size
            +  Layer.Widget.Get_Center
            ),
            Layer.From,
            Layer.Length
         );
      else
         Elliptic_Arc_Abs
         (  Context,
            Layer.Ellipse,
            Layer.From,
            Layer.Length
         );
      end if;
      Stroke  (Context);
      Layer.Updated := False;
   end Draw;

   function Get_Ellipse (Layer : Arc_Layer) return Ellipse_Parameters is
   begin
      return Layer.Ellipse;
   end Get_Ellipse;

   function Get_From (Layer : Arc_Layer) return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Arc_Layer) return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Line (Layer : Arc_Layer) return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   function Get_Properties_Number (Layer : Arc_Layer) return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Arc_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                  Gnew_Double
                  (  Name    => "x",
                     Nick    => "x",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The x-coordinate of the arc's ellipse center"
                  );
            when Property_Center_Y =>
               return
                  Gnew_Double
                  (  Name    => "y",
                     Nick    => "y",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The y-coordinate of the arc's ellipse center"
                  );
            when Property_Curvature =>
               return
                  Gnew_Double
                  (  Name    => "k",
                     Nick    => "k",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb =>
                        "The curvature of the arc's ellipse major axis"
                  );
            when Property_Radius =>
               return
                  Gnew_Double
                  (  Name    => "r",
                     Nick    => "r",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 0.5,
                     Blurb =>
                        "The radius of the arc's ellipse minor axis"
                  );
            when Property_Angle =>
               return
                  Gnew_Double
                  (  Name    => "angle",
                     Nick    => "angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb =>
                        "The angle of the major ellipse axis of the arc"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the arc beginning"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angular length of the arc"
                  );
            when Property_Line_Width =>
               return
                  Gnew_Double
                  (  Name    => "width",
                     Nick    => "width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The arc line width"
                  );
            when Property_Line_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The arc's line color"
                  );
            when Property_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                  (  Name    => "line-cap",
                     Nick    => "line cap",
                     Default => CAIRO_LINE_CAP_BUTT,
                     Blurb   => "The cap style of the arc's line"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The arc size is changed when " &
                                "the widget is resized"
                  );
            when Property_Widened =>
               return
                  Gnew_Boolean
                  (  Name    => "widened",
                     Nick    => "widened",
                     Default => False,
                     Blurb   => "The arc's line width is changed " &
                                "when the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Arc_Layer;
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
               when Property_Center_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Center.X);
               when Property_Center_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Center.Y);
               when Property_Curvature =>
                  Init (Value, GType_Double);
                  Set_Double
                  (  Value,
                     Layer.Ellipse.Major_Curvature
                  );
               when Property_Radius =>
                  Init (Value, GType_Double);
                  Set_Double
                  (  Value,
                     Layer.Ellipse.Minor_Radius
                  );
               when Property_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Angle);
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

   function Get_Scaled (Layer : Arc_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Widened (Layer : Arc_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Is_Updated (Layer : Arc_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Arc_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Ellipse := Layer.Ellipse + Offset;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Arc_Layer
             )  is
      Ellipse : Ellipse_Parameters;
      From    : GDouble;
      Length  : GDouble;
      Line    : Line_Parameters;
   begin
      Restore (Stream, Ellipse);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Line);
      Restore (Stream, Layer.Scaled, Layer.Widened);
      Set
      (  Layer   => Layer,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Line    => Line
      );
   end Restore;

   procedure Scale
             (  Layer  : in out Arc_Layer;
                Factor : GDouble
             )  is
   begin
      Set
      (  Layer   => Layer,
         Ellipse => Layer.Ellipse * Factor,
         From    => Layer.From,
         Length  => Layer.Length,
         Line    => Layer.Line
      );
   end Scale;

   procedure Set
             (  Layer   : in out Arc_Layer;
                Ellipse : Ellipse_Parameters;
                From    : GDouble;
                Length  : GDouble;
                Line    : Line_Parameters
             )  is
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

   procedure Set_Property_Value
             (  Layer    : in out Arc_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Ellipse.Center.X := Get_Double (Value);
            when Property_Center_Y =>
               Layer.Ellipse.Center.Y := Get_Double (Value);
            when Property_Curvature =>
               Layer.Ellipse.Major_Curvature := Get_Double (Value);
               if Layer.Ellipse.Major_Curvature < 0.0 then
                  Layer.Ellipse.Major_Curvature := 0.0;
               end if;
            when Property_Radius =>
               Layer.Ellipse.Minor_Radius := Get_Double (Value);
               if Layer.Ellipse.Minor_Radius < 1.0E-6 then
                  Layer.Ellipse.Minor_Radius := 1.0E-6;
               end if;
            when Property_Angle =>
               Layer.Ellipse.Angle := Get_Double (Value);
               if Layer.Ellipse.Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Ellipse.Angle :=
                     GDouble'Remainder (Layer.Ellipse.Angle, 2.0 * Pi);
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
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Arc_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Widened
             (  Layer   : in out Arc_Layer;
                Widened : Boolean
             )  is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Arc_Layer
             )  is
   begin
      Store (Stream, Layer.Ellipse);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Line);
      Store (Stream, Layer.Scaled, Layer.Widened);
   end Store;

end Gtk.Layered.Arc;
