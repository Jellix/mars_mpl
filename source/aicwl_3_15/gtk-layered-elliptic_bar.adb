--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Elliptic_Bar                    Luebeck            --
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

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with Glib.Properties.Creation;    use Glib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;

package body Gtk.Layered.Elliptic_Bar is
   type Elliptic_Bar_Ptr is access all Elliptic_Bar_Layer;

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
           Property_Line_Cap,
           Property_Value
        );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Elliptic_Bar_Ptr
          );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Elliptic_Bar_Layer,
             Elliptic_Bar_Ptr
          );

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle     : Elliptic_Bar_Ptr
             );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Elliptic_Bar_Layer is
      Ptr : Elliptic_Bar_Ptr := new Elliptic_Bar_Layer;
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
             (  Layer      : in out Elliptic_Bar_Layer;
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
         Lower : constant Gdouble := Adjustment.Get_Lower;
         Upper : constant Gdouble := Adjustment.Get_Upper;
         Value : constant Gdouble := Adjustment.Get_Value;
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

   procedure Add_Elliptic_Bar
             (  Under      : not null access Layer_Location'Class;
                Ellipse    : Ellipse_Parameters := Unit_Circle;
                From       : Gdouble        := 3.0 * Pi / 4.0;
                Length     : Gdouble        := 3.0 * Pi / 2.0;
                Width      : Gdouble        := 1.0;
                Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
                Line_Cap   : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean        := False;
                Widened    : Boolean        := False
             )  is
      Ptr   : Elliptic_Bar_Ptr := new Elliptic_Bar_Layer;
      Layer : Elliptic_Bar_Layer renames Ptr.all;
   begin
      Layer.Widened := Widened;
      Layer.Scaled  := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Line    => (Width, Color, Line_Cap),
         Ellipse => Ellipse,
         From    => From,
         Length  => Length
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Bar;

   function Add_Elliptic_Bar
            (  Under      : not null access Layer_Location'Class;
               Ellipse    : Ellipse_Parameters := Unit_Circle;
               From       : Gdouble        := 3.0 * Pi / 4.0;
               Length     : Gdouble        := 3.0 * Pi / 2.0;
               Width      : Gdouble        := 1.0;
               Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
               Line_Cap   : Cairo_Line_Cap := Cairo_Line_Cap_Butt;
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean        := False;
               Widened    : Boolean        := False
            )  return not null access Elliptic_Bar_Layer is
      Ptr   : Elliptic_Bar_Ptr := new Elliptic_Bar_Layer;
      Layer : Elliptic_Bar_Layer renames Ptr.all;
   begin
      Layer.Widened := Widened;
      Layer.Scaled  := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Line    => (Width, Color, Line_Cap),
         Ellipse => Ellipse,
         From    => From,
         Length  => Length
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Elliptic_Bar;

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Needle     : Elliptic_Bar_Ptr
             )  is
      Lower : constant Gdouble := Get_Lower (Needle.Adjustment);
      Upper : constant Gdouble := Get_Upper (Needle.Adjustment);
      Value : constant Gdouble := Get_Value (Needle.Adjustment);
   begin
      if Upper <= Lower or else Value <= Lower then
         Needle.Set_Value (0.0);
      elsif Value >= Upper then
         Needle.Set_Value (1.0);
      else
         Needle.Set_Value ((Value - Lower) / (Upper - Lower));
      end if;
      if not Needle.Widget.Drawing and then Needle.Updated then
         Queue_Draw (Needle.Widget); -- Signal draw to the widget
      end if;
   end Changed;

   procedure Draw
             (  Layer   : in out Elliptic_Bar_Layer;
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
      Set_Source_Rgb
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
            Layer.Value * Layer.Length
         );
      else
         Elliptic_Arc_Abs
         (  Context,
            Layer.Ellipse,
            Layer.From,
            Layer.Value * Layer.Length
         );
      end if;
      Stroke  (Context);
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out Elliptic_Bar_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Disconnect (Layer.Adjustment, Layer.Changed);
         Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   function Get_Adjustment (Layer : Elliptic_Bar_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Ellipse (Layer : Elliptic_Bar_Layer)
      return Ellipse_Parameters is
   begin
      return Layer.Ellipse;
   end Get_Ellipse;

   function Get_From (Layer : Elliptic_Bar_Layer) return GDouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Elliptic_Bar_Layer) return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Line (Layer : Elliptic_Bar_Layer)
      return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   function Get_Properties_Number
            (  Layer : Elliptic_Bar_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Elliptic_Bar_Layer;
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
                        "The x-coordinate of the bar's ellipse center"
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
                        "The y-coordinate of the bar's ellipse center"
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
                        "The curvature of the bar's ellipse major axis"
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
                        "The radius of the bar's ellipse minor axis"
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
                        "The angle of the major ellipse axis of the bar"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the bar beginning " &
                                "corresponding to the value 0"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angular length of the bar, " &
                                "the length added to the value of " &
                                "the property from is the angle " &
                                "coresponding to the value 1"
                  );
            when Property_Line_Width =>
               return
                  Gnew_Double
                  (  Name    => "width",
                     Nick    => "width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The width of the bar's line"
                  );
            when Property_Line_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The color of the bar"
                  );
            when Property_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                  (  Name    => "line-cap",
                     Nick    => "line cap",
                     Default => CAIRO_LINE_CAP_BUTT,
                     Blurb   => "The cap style of the line of the bar"
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
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The bar size is changed when " &
                                "the widget is resized"
                  );
            when Property_Widened =>
               return
                  Gnew_Boolean
                  (  Name    => "widened",
                     Nick    => "widened",
                     Default => False,
                     Blurb   => "The bar's line width is changed " &
                                "when the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Elliptic_Bar_Layer;
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
                  Set_Double (Value, Layer.Ellipse.Major_Curvature);
               when Property_Radius =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ellipse.Minor_Radius);
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
               when Property_Value =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Value);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Elliptic_Bar_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Value (Layer : Elliptic_Bar_Layer) return GDouble is
   begin
      return Layer.Value;
   end Get_Value;

   function Get_Widened (Layer : Elliptic_Bar_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Is_Updated (Layer : Elliptic_Bar_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Elliptic_Bar_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Ellipse := Layer.Ellipse + Offset;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Elliptic_Bar_Layer
             )  is
      Ellipse    : Ellipse_Parameters;
      From       : GDouble;
      Length     : GDouble;
      Line       : Line_Parameters;
      Adjustment : Boolean;
   begin
      Restore (Stream, Ellipse);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Line);
      Restore (Stream, Layer.Scaled, Layer.Widened, Adjustment);
      Set
      (  Layer   => Layer,
         Ellipse => Ellipse,
         From    => From,
         Length  => Length,
         Line    => Line
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
             (  Layer  : in out Elliptic_Bar_Layer;
                Factor : GDouble
             )  is
      Ellipse : constant Ellipse_Parameters := Layer.Ellipse * Factor;
   begin
      if Ellipse.Minor_Radius <= 0.0 then
         raise Constraint_Error with "Non-positive ellipse radius";
      elsif Ellipse.Major_Curvature < 0.0 then
         raise Constraint_Error with "Negative ellipse curvature";
      end if;
      Layer.Ellipse := Ellipse;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer   : in out Elliptic_Bar_Layer;
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
      Set_Value (Layer, Layer.Value);
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Elliptic_Bar_Layer;
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
            when Property_Value =>
               Set_Value (Layer, Get_Double (Value));
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Elliptic_Bar_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Value
             (  Layer : in out Elliptic_Bar_Layer;
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

   procedure Set_Widened
             (  Layer   : in out Elliptic_Bar_Layer;
                Widened : Boolean
             )  is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Elliptic_Bar_Layer
             )  is
   begin
      Store (Stream, Layer.Ellipse);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Line);
      Store
      (  Stream,
         Layer.Scaled,
         Layer.Widened,
         Layer.Adjustment /= null
      );
      if Layer.Adjustment = null then
         Store (Stream, Layer.Value);
      else
         Store (Stream, Layer.Adjustment);
      end if;
   end Store;

end Gtk.Layered.Elliptic_Bar;
