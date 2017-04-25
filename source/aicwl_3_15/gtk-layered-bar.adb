--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Bar                    Luebeck            --
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
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;

package body Gtk.Layered.Bar is
   type Bar_Ptr is access all Bar_Layer;

   type Layer_Property is
        (  Property_Scaled,
           Property_Widened,
           Property_From_X,
           Property_From_Y,
           Property_To_X,
           Property_To_Y,
           Property_Line_Width,
           Property_Line_Color,
           Property_Line_Cap,
           Property_Value
        );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Bar_Ptr
          );

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Bar_Layer,
             Bar_Ptr
          );

   procedure Changed
             (  Adjustment   : access GObject_Record'Class;
                Bar : Bar_Ptr
             );

   procedure Add_Adjustment
             (  Layer      : in out Bar_Layer;
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

   procedure Add_Bar
             (  Under      : not null access Layer_Location'Class;
                From       : Cairo_Tuple    := (0.0, 0.0);
                Angle      : GDouble        := 0.0;
                Length     : GDouble        := 1.0;
                Width      : GDouble        := 1.0;
                Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
                Line_Cap   : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean        := False;
                Widened    : Boolean        := False
             )  is
      Ptr   : Bar_Ptr := new Bar_Layer;
      Layer : Bar_Layer renames Ptr.all;
   begin
      Layer.Widened := Widened;
      Layer.Scaled  := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Line   => (Width, Color, Line_Cap),
         From   => From,
         Angle  => Angle,
         Length => Length
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Bar;

   procedure Add_Bar
             (  Under      : not null access Layer_Location'Class;
                From       : Cairo_Tuple    := (0.0, 0.0);
                To         : Cairo_Tuple    := (0.0, 1.0);
                Width      : GDouble        := 1.0;
                Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
                Line_Cap   : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
                Adjustment : access Gtk_Adjustment_Record'Class := null;
                Scaled     : Boolean        := False;
                Widened    : Boolean        := False
             )  is
      Ptr   : Bar_Ptr := new Bar_Layer;
      Layer : Bar_Layer renames Ptr.all;
   begin
      Layer.Widened := Widened;
      Layer.Scaled  := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer => Layer,
         Line  => (Width, Color, Line_Cap),
         From  => From,
         To    => To
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Bar;

   function Add_Bar
            (  Under      : not null access Layer_Location'Class;
               From       : Cairo_Tuple    := (0.0, 0.0);
               Angle      : GDouble        := 0.0;
               Length     : GDouble        := 1.0;
               Width      : GDouble        := 1.0;
               Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
               Line_Cap   : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean        := False;
               Widened    : Boolean        := False
            )  return not null access Bar_Layer is
      Ptr   : Bar_Ptr := new Bar_Layer;
      Layer : Bar_Layer renames Ptr.all;
   begin
      Layer.Widened := Widened;
      Layer.Scaled  := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Line   => (Width, Color, Line_Cap),
         From   => From,
         Angle  => Angle,
         Length => Length
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Bar;

   function Add_Bar
            (  Under      : not null access Layer_Location'Class;
               From       : Cairo_Tuple    := (0.0, 0.0);
               To         : Cairo_Tuple    := (0.0, 1.0);
               Width      : GDouble        := 1.0;
               Color      : Gdk_Color      := RGB (1.0, 0.0, 0.0);
               Line_Cap   : Cairo_Line_Cap := CAIRO_LINE_CAP_BUTT;
               Adjustment : access Gtk_Adjustment_Record'Class := null;
               Scaled     : Boolean        := False;
               Widened    : Boolean        := False
            )  return not null access Bar_Layer is
      Ptr   : Bar_Ptr := new Bar_Layer;
      Layer : Bar_Layer renames Ptr.all;
   begin
      Layer.Widened := Widened;
      Layer.Scaled  := Scaled;
      Add (Ptr, Under);
      Set
      (  Layer => Layer,
         Line  => (Width, Color, Line_Cap),
         From  => From,
         To    => To
      );
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Bar;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Bar_Layer is
      Ptr : Bar_Ptr := new Bar_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Changed
             (  Adjustment : access GObject_Record'Class;
                Bar        : Bar_Ptr
             )  is
      Lower : constant GDouble := Get_Lower (Bar.Adjustment);
      Upper : constant GDouble := Get_Upper (Bar.Adjustment);
      Value : constant GDouble := Get_Value (Bar.Adjustment);
   begin
      if Upper <= Lower or else Value <= Lower then
         Bar.Set_Value (0.0);
      elsif Value >= Upper then
         Bar.Set_Value (1.0);
      else
         Bar.Set_Value ((Value - Lower) / (Upper - Lower));
      end if;
      if not Bar.Widget.Drawing and then Bar.Updated then
         Queue_Draw (Bar.Widget); -- Signal draw to the widget
      end if;
   end Changed;

   procedure Draw
             (  Layer   : in out Bar_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      From, To : Cairo_Tuple;
      B : constant GDouble := Layer.Value;
      A : constant GDouble := 1.0 - B;
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
         declare
            Size   : constant GDouble := Layer.Widget.Get_Size;
            Center : constant Cairo_Tuple := Layer.Widget.Get_Center;
         begin
            From.X := Center.X + Layer.From.X * Size;
            From.Y := Center.Y + Layer.From.Y * Size;
            To.X   := Center.X + Layer.To.X   * Size;
            To.Y   := Center.Y + Layer.To.Y   * Size;
         end;
      else
         From := Layer.From;
         To   := Layer.To;
      end if;
      Move_To (Context, From.X, From.Y);
      Line_To
      (  Cr => Context,
         X  => From.X * A + To.X * B,
         Y  => From.Y * A + To.Y * B
      );
      Stroke  (Context);
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out Bar_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Disconnect (Layer.Adjustment, Layer.Changed);
         Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   function Get_Adjustment (Layer : Bar_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Angle (Layer : Bar_Layer) return GDouble is
   begin
      return
         arctan
         (  X => Layer.To.X - Layer.From.X,
            Y => Layer.To.Y - Layer.From.Y
         );
   end Get_Angle;

   function Get_From (Layer : Bar_Layer) return Cairo_Tuple is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Bar_Layer) return GDouble is
   begin
      return
         sqrt
         (  (Layer.To.X - Layer.From.X)**2
         +  (Layer.To.Y - Layer.From.Y)**2
         );
   end Get_Length;

   function Get_Line (Layer : Bar_Layer)
      return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   function Get_Properties_Number (Layer : Bar_Layer) return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Bar_Layer;
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
            when Property_To_X =>
               return
                  Gnew_Double
                  (  Name    => "x1",
                     Nick    => "x1",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the point " &
                                "corresponding to the value 1"
                  );
            when Property_To_Y =>
               return
                  Gnew_Double
                  (  Name    => "y1",
                     Nick    => "y1",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the point " &
                                "corresponding to the value 1"
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
            when Property_Line_Width =>
               return
                  Gnew_Double
                  (  Name    => "width",
                     Nick    => "width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The bar's line width"
                  );
            when Property_Line_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The bar's line color"
                  );
            when Property_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                  (  Name    => "line-cap",
                     Nick    => "line cap",
                     Default => CAIRO_LINE_CAP_BUTT,
                     Blurb   => "The cap style of the bar's line"
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
            (  Layer    : Bar_Layer;
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
               when Property_To_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.To.X);
               when Property_To_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.To.Y);
               when Property_Value =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Value);
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

   function Get_Scaled (Layer : Bar_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_To (Layer : Bar_Layer) return Cairo_Tuple is
   begin
      return Layer.To;
   end Get_To;

   function Get_Value (Layer : Bar_Layer) return GDouble is
   begin
      return Layer.Value;
   end Get_Value;

   function Get_Widened (Layer : Bar_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Is_Updated (Layer : Bar_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Bar_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.From.X  := Layer.From.X + Offset.X;
      Layer.From.Y  := Layer.From.Y + Offset.Y;
      Layer.To.X    := Layer.To.X   + Offset.X;
      Layer.To.Y    := Layer.To.Y   + Offset.Y;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Bar_Layer
             )  is
      From       : Cairo_Tuple;
      To         : Cairo_Tuple;
      Line       : Line_Parameters;
      Adjustment : Boolean;
   begin
      Restore (Stream, From);
      Restore (Stream, To);
      Restore (Stream, Line);
      Restore (Stream, Layer.Scaled, Layer.Widened, Adjustment);
      Set
      (  Layer => Layer,
         From  => From,
         To    => To,
         Line  => Line
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
             (  Layer  : in out Bar_Layer;
                Factor : GDouble
             )  is
   begin
      Layer.From.X  := Layer.From.X * Factor;
      Layer.From.Y  := Layer.From.Y * Factor;
      Layer.To.X    := Layer.To.X   * Factor;
      Layer.To.Y    := Layer.To.Y   * Factor;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer  : in out Bar_Layer;
                From   : Cairo_Tuple;
                Angle  : GDouble;
                Length : GDouble;
                Line   : Line_Parameters
             )  is
   begin
      Set
      (  Layer => Layer,
         Line  => Line,
         From  => From,
         To => (  X => From.X + Length * cos (Angle),
                  Y => From.Y + Length * sin (Angle)
      )        );
   end Set;

   procedure Set
             (  Layer : in out Bar_Layer;
                From  : Cairo_Tuple;
                To    : Cairo_Tuple;
                Line  : Line_Parameters
             )  is
   begin
      if Line.Width <= 0.0 then
         raise Constraint_Error with "Non-positive line width";
      end if;
      Layer.From    := From;
      Layer.To      := To;
      Layer.Line    := Line;
      Layer.Updated := True;
      Set_Value (Layer, Layer.Value);
   end Set;

   procedure Set_Property_Value
             (  Layer    : in out Bar_Layer;
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
            when Property_To_X =>
               Layer.To.X := Get_Double (Value);
            when Property_To_Y =>
               Layer.To.Y := Get_Double (Value);
            when Property_Value =>
               Set_Value (Layer, Get_Double (Value));
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
             (  Layer  : in out Bar_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Value
             (  Layer : in out Bar_Layer;
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
             (  Layer   : in out Bar_Layer;
                Widened : Boolean
             )  is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Bar_Layer
             )  is
   begin
      Store (Stream, Layer.From);
      Store (Stream, Layer.To);
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

end Gtk.Layered.Bar;
