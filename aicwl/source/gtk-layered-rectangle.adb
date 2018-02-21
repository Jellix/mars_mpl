--                                                                    --
--  package Gtk.Layered.Rectangle   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2011       --
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

with Glib.Properties.Creation;

with Gtk.Layered.Stream_IO;

package body Gtk.Layered.Rectangle is
   type Rectangle_Ptr is access all Rectangle_Layer;

   type Layer_Property is
     (Property_Scaled,
      Property_Widened,
      Property_X,
      Property_Y,
      Property_Height,
      Property_Width,
      Property_Line_Width,
      Property_Color,
      Property_Opacity);

   procedure Free is
     new Ada.Unchecked_Deallocation (Rectangle_Layer, Rectangle_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Rectangle_Layer
   is
      Ptr : Rectangle_Ptr := new Rectangle_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Rectangle
     (Under      : not null access Layer_Location'Class;
      Box        : Cairo.Ellipses.Cairo_Box;
      Color      : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Width : Gdouble             := 0.0;
      Opacity    : Fill_Opacity        := 1.0;
      Scaled     : Boolean             := False;
      Widened    : Boolean             := False)
   is
      Ptr   : Rectangle_Ptr := new Rectangle_Layer;
      Layer : Rectangle_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer      => Layer,
         Box        => Box,
         Color      => Color,
         Line_Width => Line_Width,
         Opacity    => Opacity);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Rectangle;

   function Add_Rectangle
     (Under      : not null access Layer_Location'Class;
      Box        : Cairo.Ellipses.Cairo_Box;
      Color      : Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Line_Width : Gdouble             := 0.0;
      Opacity    : Fill_Opacity        := 1.0;
      Scaled     : Boolean             := False;
      Widened    : Boolean             := False)
      return not null access Rectangle_Layer
   is
      Ptr   : Rectangle_Ptr := new Rectangle_Layer;
      Layer : Rectangle_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer      => Layer,
         Box        => Box,
         Color      => Color,
         Line_Width => Line_Width,
         Opacity    => Opacity);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Rectangle;

   overriding procedure Draw
     (Layer   : in out Rectangle_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      X      : Gdouble := Layer.Box.X1;
      Y      : Gdouble := Layer.Box.Y1;
      Width  : Gdouble := Layer.Box.X2 - Layer.Box.X1;
      Height : Gdouble := Layer.Box.Y2 - Layer.Box.Y1;
   begin
      Cairo.New_Path (Context);
      if Layer.Scaled then
         declare
            pragma Warnings (Off, "declaration hides ""Center""");
            Size_X : constant Gdouble :=
                       Gdouble (Layer.Widget.all.Get_Allocated_Width);
            Size_Y : constant Gdouble :=
                       Gdouble (Layer.Widget.all.Get_Allocated_Height);
            Center : constant Cairo.Ellipses.Cairo_Tuple :=
                       Layer.Widget.all.Get_Center;
            pragma Warnings (On, "declaration hides ""Center""");
         begin
            X      := X * Size_X + Center.X;
            Width  := Width * Size_X;
            Y      := Y * Size_Y + Center.Y;
            Height := Height * Size_Y;
         end;
      end if;
      Cairo.Rectangle
        (Cr     => Context,
         X      => X,
         Y      => Y,
         Width  => Width,
         Height => Height);
      if Layer.Opacity > 0.0 then
         if Layer.Opacity = 1.0 then
            Cairo.Set_Source_Rgb
              (Context,
               Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
               Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
               Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last));
         else
            Cairo.Set_Source_Rgba
              (Context,
               Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
               Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
               Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last),
               Layer.Opacity);
         end if;
         if Layer.Width > 0.0 then
            Cairo.Fill_Preserve (Context);
         else
            Cairo.Fill (Context);
         end if;
      end if;
      if Layer.Width > 0.0 then
         if Layer.Widened then
            Cairo.Set_Line_Width
              (Context,
               Layer.Width * Layer.Widget.all.Get_Size);
         else
            Cairo.Set_Line_Width (Context, Layer.Width);
         end if;
         Cairo.Set_Source_Rgb
           (Context,
            Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last));
         Cairo.Stroke (Context);
      end if;
      Layer.Updated := False;
   end Draw;

   function Get_Box (Layer : Rectangle_Layer) return Cairo.Ellipses.Cairo_Box is
   begin
      return Layer.Box;
   end Get_Box;

   function Get_Color (Layer : Rectangle_Layer) return Gdk.Color.Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Line_Width  (Layer : Rectangle_Layer)
                             return Gdouble is
   begin
      return Layer.Width;
   end Get_Line_Width;

   function Get_Opacity  (Layer : Rectangle_Layer)
                          return Fill_Opacity is
   begin
      return Layer.Opacity;
   end Get_Opacity;

   overriding function Get_Properties_Number (Layer : Rectangle_Layer)
                                              return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last) -
             Layer_Property'Pos (Layer_Property'First) + 1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Rectangle_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x",
                    Nick    => "x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The x-coordinate of the left " &
                      "margin of the rectangle");
            when Property_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y",
                    Nick    => "y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The y-coordinate of the top " &
                      "margin of the rectangle");
            when Property_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "width",
                    Nick    => "width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The width of the rectangle");
            when Property_Height =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "height",
                    Nick    => "height",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The height of the rectangle");
            when Property_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The rectangle color");
            when Property_Opacity =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "fill-opacity",
                    Nick    => "opacity",
                    Minimum => 0.0,
                    Maximum => 1.0,
                    Default => 1.0,
                    Blurb   => "The opacity of filling. 0.0 " &
                      "is transparent, 1.0 is opaque");
            when Property_Line_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "width",
                    Nick    => "width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The rectangle's line width");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   => "The bar size is changed when " &
                      "the widget is resized");
            when Property_Widened =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "widened",
                    Nick    => "widened",
                    Default => False,
                    Blurb   => "The rectangle's line width is " &
                      "changed when the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Rectangle_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Box.X1);
               when Property_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Box.Y1);
               when Property_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double
                    (Value,
                     Gdouble (Layer.Box.X2 - Layer.Box.X1));
               when Property_Height =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double
                    (Value,
                     Gdouble (Layer.Box.Y2 - Layer.Box.Y1));
               when Property_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Color);
               when Property_Opacity =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Gdouble (Layer.Opacity));
               when Property_Line_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Width);
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

   overriding function Get_Scaled (Layer : Rectangle_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   overriding function Get_Widened (Layer : Rectangle_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   overriding function Is_Updated (Layer : Rectangle_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Rectangle_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.Box.X1 := Layer.Box.X1 + Offset.X;
      Layer.Box.Y1 := Layer.Box.Y1 + Offset.Y;
      Layer.Box.X2 := Layer.Box.X2 + Offset.X;
      Layer.Box.Y2 := Layer.Box.Y2 + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Rectangle_Layer)
   is
      From    : Cairo.Ellipses.Cairo_Tuple;
      To      : Cairo.Ellipses.Cairo_Tuple;
      Color   : Gdk.Color.Gdk_Color;
      Width   : Gdouble;
      Opacity : Gdouble;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, To);
      Gtk.Layered.Stream_IO.Restore (Stream, Color);
      Gtk.Layered.Stream_IO.Restore (Stream, Width);
      Gtk.Layered.Stream_IO.Restore (Stream, Opacity);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled);
      Set
        (Layer    => Layer,
         Color      => Color,
         Line_Width => Gdouble'Max (0.0, Width),
         Opacity    => Gdouble'Min (1.0, Gdouble'Max (0.0, Opacity)),
         Box        => (X1 => Gdouble'Min (From.X, To.X),
                        Y1 => Gdouble'Min (From.Y, To.Y),
                        X2 => Gdouble'Max (From.X, To.X),
                        Y2 => Gdouble'Max (From.Y, To.Y)));
   end Restore;

   overriding procedure Scale
     (Layer  : in out Rectangle_Layer;
      Factor : Gdouble) is
   begin
      Layer.Box.X1 := Layer.Box.X1 * Factor;
      Layer.Box.Y1 := Layer.Box.Y1 * Factor;
      Layer.Box.X2 := Layer.Box.X2 * Factor;
      Layer.Box.Y2 := Layer.Box.Y2 * Factor;
      Layer.Updated := True;
   end Scale;

   procedure Set
     (Layer      : in out Rectangle_Layer;
      Box        : Cairo.Ellipses.Cairo_Box;
      Color      : Gdk.Color.Gdk_Color;
      Line_Width : Gdouble;
      Opacity    : Fill_Opacity) is
   begin
      if Box.X2 < Box.X1 then
         raise Constraint_Error with "Negative box width";
      elsif Box.Y2 < Box.Y1 then
         raise Constraint_Error with "Negative box height";
      elsif Line_Width < 0.0 then
         raise Constraint_Error with "Negative line width";
      end if;
      Layer.Box     := Box;
      Layer.Color   := Color;
      Layer.Width   := Line_Width;
      Layer.Opacity := Opacity;
      Layer.Updated := True;
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Rectangle_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X =>
               Layer.Box.X2 := Layer.Box.X2 - Layer.Box.X1;
               Layer.Box.X1 := Glib.Values.Get_Double (Value);
               Layer.Box.X2 := Layer.Box.X2 + Layer.Box.X1;
            when Property_Y =>
               Layer.Box.Y2 := Layer.Box.Y2 - Layer.Box.Y1;
               Layer.Box.Y1 := Glib.Values.Get_Double (Value);
               Layer.Box.Y2 := Layer.Box.Y2 + Layer.Box.Y1;
            when Property_Width =>
               Layer.Box.X2 :=
                 Layer.Box.X1 + Glib.Values.Get_Double (Value);
            when Property_Height =>
               Layer.Box.Y2 :=
                 Layer.Box.Y1 + Glib.Values.Get_Double (Value);
            when Property_Color =>
               Layer.Color := Gdk.Color.Get_Value (Value);
            when Property_Opacity =>
               declare
                  New_Value : Gdouble := Glib.Values.Get_Double (Value);
               begin
                  New_Value :=
                    Gdouble'Min (1.0, Gdouble'Max (0.0, New_Value));
                  if New_Value /= Gdouble (Layer.Opacity) then
                     Layer.Opacity := New_Value;
                     Layer.Updated := True;
                  end if;
               end;
            when Property_Line_Width =>
               Layer.Width := Glib.Values.Get_Double (Value);
               if Layer.Width < 0.0 then
                  Layer.Width := 0.0;
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
     (Layer  : in out Rectangle_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Widened
     (Layer   : in out Rectangle_Layer;
      Widened : Boolean) is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Rectangle_Layer) is
   begin
      Gtk.Layered.Stream_IO.Store
        (Stream,
         Cairo.Ellipses.Cairo_Tuple'(X => Layer.Box.X1, Y => Layer.Box.Y1));
      Gtk.Layered.Stream_IO.Store
        (Stream,
         Cairo.Ellipses.Cairo_Tuple'(X => Layer.Box.X2, Y => Layer.Box.Y2));
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Color);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Width);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Opacity);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Scaled, Layer.Widened);
   end Store;

end Gtk.Layered.Rectangle;
