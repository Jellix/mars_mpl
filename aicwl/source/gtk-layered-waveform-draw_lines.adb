--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.                       Luebeck            --
--        Draw_Lines                               Winter, 2011       --
--  Separate body                                                     --
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

separate (Gtk.Layered.Waveform)
procedure Draw_Lines
  (Layer   : in out Waveform_Layer;
   Context : Cairo.Cairo_Context;
   Data    : in out Line_Method_Data)
is
   pragma Warnings (Off, "declaration hides ""Point""");

   procedure Line_To (Point : Cairo.Ellipses.Cairo_Tuple)
     with Inline => True
   is
   begin
      Cairo.Line_To (Context, Point.X, Point.Y);
   end Line_To;

   procedure Move_To (Point : Cairo.Ellipses.Cairo_Tuple)
     with Inline => True
   is
   begin
      Cairo.Move_To (Context, Point.X, Point.Y);
   end Move_To;

   Antialias : constant Cairo.Cairo_Antialias := Cairo.Get_Antialias (Context);
   X1, X2    : Gdouble;
   Y1, Y2    : Gdouble;
begin
   if Data.Count > 0 then
      if Layer.Scaled then
         declare
            X_Size : constant Gdouble :=
                     Gdouble (Layer.Widget.all.Get_Allocated_Width);
            Y_Size : constant Gdouble :=
                     Gdouble (Layer.Widget.all.Get_Allocated_Height);
         begin
            X1 := Layer.Box.X1 * X_Size + Layer.Widget.all.Get_Center.X;
            X2 := Layer.Box.X2 * X_Size + Layer.Widget.all.Get_Center.X;
            Y1 := Layer.Box.Y1 * Y_Size + Layer.Widget.all.Get_Center.Y;
            Y2 := Layer.Box.Y2 * Y_Size + Layer.Widget.all.Get_Center.Y;
         end;
      else
         X1 := Layer.Box.X1;
         X2 := Layer.Box.X2;
         Y1 := Layer.Box.Y1;
         Y2 := Layer.Box.Y2;
      end if;
      Layer.Set_Y_Conversion (Y1, Y2);
      declare
         State : Cairo.Ellipses.Context_State := Cairo.Ellipses.Save (Context);
         pragma Unreferenced (State);
      begin
         Cairo.Set_Antialias (Context, Cairo.Cairo_Antialias_None);
         --
         -- Clipping the bounding rectangle of the waveform
         --
         Cairo.New_Path (Context);
         Cairo.Move_To (Context, X1, Y1);
         Cairo.Line_To (Context, X2, Y1);
         Cairo.Line_To (Context, X2, Y2);
         Cairo.Line_To (Context, X1, Y2);
         Cairo.Close_Path (Context);
         Cairo.Clip (Context);
         declare
            Points : Points_Array renames Data.Points.all;

            function Line_Point
              (Offset : Natural) return Cairo.Ellipses.Cairo_Tuple
              with Inline => True
            is
               Result : constant Point_Data :=
                  Points ((Data.First + Offset) mod Points'Length);
            begin
               return
                 (Gdouble (Result.X) + 0.5,
                  Layer.Y0 - Layer.YY * Gdouble (Result.Y) + 0.5);
            end Line_Point;

            function Point
              (Offset : Natural) return Cairo.Ellipses.Cairo_Tuple
              with Inline => True;
            function Point
              (Offset : Natural) return Cairo.Ellipses.Cairo_Tuple
            is
               Result : constant  Point_Data :=
                  Points ((Data.First + Offset) mod Points'Length);
            begin
               return
                 (Gdouble (Result.X),
                  Layer.Y0 - Layer.YY * Gdouble (Result.Y));
            end Point;
         begin
            Move_To (Line_Point (0));
            for Index in 1 .. Data.Count - 1 loop
               Line_To (Line_Point (Index));
            end loop;
            if Layer.Opacity = 0.0 then -- No filling
               if Layer.Widened then
                  Cairo.Set_Line_Width
                    (Context,
                     Layer.Line.Width * Layer.Widget.all.Get_Size);
               else
                  Cairo.Set_Line_Width (Context, Layer.Line.Width);
               end if;
               Cairo.Set_Line_Cap (Context, Layer.Line.Line_Cap);
               Cairo.Set_Source_Rgb
                 (Cr    => Context,
                  Red   => Gdouble (Gdk.Color.Red (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Green => Gdouble (Gdk.Color.Green (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Blue  => Gdouble (Gdk.Color.Blue (Layer.Line.Color)) /
                           Gdouble (Guint16'Last));
               Cairo.Stroke (Context);
            elsif Layer.Opacity = 1.0 then -- Opaque filling
               Cairo.Line_To (Context, Point (Data.Count - 1).X, Y2);
               Cairo.Line_To (Context, Point (0)             .X, Y2);
               Cairo.Close_Path (Context);
               Cairo.Set_Source_Rgb
                 (Cr    => Context,
                  Red   => Gdouble (Gdk.Color.Red (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Green => Gdouble (Gdk.Color.Green (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Blue  => Gdouble (Gdk.Color.Blue (Layer.Line.Color)) /
                           Gdouble (Guint16'Last)
               );
               Cairo.Fill (Context);
            else -- Transparent filling
               Cairo.Line_To (Context, Point (Data.Count - 1).X, Y2);
               Cairo.Line_To (Context, Point (0)             .X, Y2);
               Cairo.Close_Path (Context);
               Cairo.Set_Source_Rgba
                 (Cr    => Context,
                  Red   => Gdouble (Gdk.Color.Red   (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Green => Gdouble (Gdk.Color.Green (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Blue  => Gdouble (Gdk.Color.Blue  (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Alpha => 1.0 - Layer.Opacity
               );
               Cairo.Fill (Context);
                  -- Drawing the line over it
               Move_To (Point (0));
               for Index in 1 .. Data.Count - 1 loop
                  Line_To (Point (Index));
               end loop;
               if Layer.Widened then
                  Cairo.Set_Line_Width
                    (Context,
                     Layer.Line.Width * Layer.Widget.all.Get_Size);
               else
                  Cairo.Set_Line_Width (Context, Layer.Line.Width);
               end if;
               Cairo.Set_Line_Cap (Context, Layer.Line.Line_Cap);
               Cairo.Set_Source_Rgb
                 (Cr    => Context,
                  Red   => Gdouble (Gdk.Color.Red (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Green => Gdouble (Gdk.Color.Green (Layer.Line.Color)) /
                           Gdouble (Guint16'Last),
                  Blue  => Gdouble (Gdk.Color.Blue (Layer.Line.Color)) /
                           Gdouble (Guint16'Last)
               );
               Cairo.Stroke (Context);
            end if;
         end;
         Cairo.Set_Antialias (Context, Antialias);
      end;
   end if;
   pragma Warnings (On, "declaration hides ""Point""");
exception
   when Error : others =>
      Glib.Messages.Log
        (Gtk.Missed.GtkAda_Contributions_Domain,
         Glib.Messages.Log_Level_Critical,
         "Fault: "
         & Ada.Exceptions.Exception_Information (Error)
         & Where ("Draw_Lines"));
end Draw_Lines;