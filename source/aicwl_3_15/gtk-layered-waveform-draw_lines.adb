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
--____________________________________________________________________--

separate (Gtk.Layered.Waveform)
   procedure Draw_Lines
             (  Layer   : in out Waveform_Layer;
                Context : Cairo_Context;
                Data    : in out Line_Method_Data
             )  is
   procedure Move_To (Point : Cairo_Tuple) is
      pragma Inline (Move_To);
   begin
      Move_To (Context, Point.X, Point.Y);
   end Move_To;

   procedure Line_To (Point : Cairo_Tuple) is
      pragma Inline (Line_To);
   begin
      Line_To (Context, Point.X, Point.Y);
   end Line_To;

   Antialias : constant Cairo_Antialias := Get_Antialias (Context);
   X1, X2    : GDouble;
   Y1, Y2    : GDouble;
begin
   if Data.Count > 0 then
      if Layer.Scaled then
         declare
            X_Size : constant GDouble :=
                     GDouble (Layer.Widget.Get_Allocated_Width);
            Y_Size : constant GDouble :=
                     GDouble (Layer.Widget.Get_Allocated_Height);
         begin
            X1 := Layer.Box.X1 * X_Size + Layer.Widget.Get_Center.X;
            X2 := Layer.Box.X2 * X_Size + Layer.Widget.Get_Center.X;
            Y1 := Layer.Box.Y1 * Y_Size + Layer.Widget.Get_Center.Y;
            Y2 := Layer.Box.Y2 * Y_Size + Layer.Widget.Get_Center.Y;
         end;
      else
         X1 := Layer.Box.X1;
         X2 := Layer.Box.X2;
         Y1 := Layer.Box.Y1;
         Y2 := Layer.Box.Y2;
      end if;
      Layer.Set_Y_Conversion (Y1, Y2);
      declare
         State : Context_State := Save (Context);
      begin
         Set_Antialias (Context, Cairo_Antialias_None);
         --
         -- Clipping the bounding rectangle of the waveform
         --
         New_Path (Context);
         Move_To (Context, X1, Y1);
         Line_To (Context, X2, Y1);
         Line_To (Context, X2, Y2);
         Line_To (Context, X1, Y2);
         Close_Path (Context);
         Clip (Context);
         declare
            Points : Points_Array renames Data.Points.all;

            function Point (Offset : Natural) return Cairo_Tuple is
               pragma Inline (Point);
               Result : constant  Point_Data :=
                  Points ((Data.First + Offset) mod Points'Length);
            begin
               return
               (  GDouble (Result.X),
                  Layer.Y0 - Layer.YY * GDouble (Result.Y)
               );
            end Point;

            function Line_Point (Offset : Natural) return Cairo_Tuple is
               pragma Inline (Line_Point);
               Result : constant Point_Data :=
                  Points ((Data.First + Offset) mod Points'Length);
            begin
               return
               (  GDouble (Result.X) + 0.5,
                  Layer.Y0 - Layer.YY * GDouble (Result.Y) + 0.5
               );
            end Line_Point;
         begin
            Move_To (Line_Point (0));
            for Index in 1..Data.Count - 1 loop
               Line_To (Line_Point (Index));
            end loop;
            if Layer.Opacity = 0.0 then -- No filling
               if Layer.Widened then
                  Set_Line_Width
                  (  Context,
                     Layer.Line.Width * Layer.Widget.Get_Size
                  );
               else
                  Set_Line_Width (Context, Layer.Line.Width);
               end if;
               Set_Line_Cap (Context, Layer.Line.Line_Cap);
               Set_Source_RGB
               (  Cr    => Context,
                  Red   => GDouble (Red (Layer.Line.Color)) /
                           GDouble (GUint16'Last),
                  Green => GDouble (Green (Layer.Line.Color)) /
                           GDouble (GUInt16'Last),
                  Blue  => GDouble (Blue (Layer.Line.Color)) /
                           GDouble (GUInt16'Last)
               );
               Stroke (Context);
            elsif Layer.Opacity = 1.0 then -- Opaque filling
               Line_To (Context, Point (Data.Count - 1).X, Y2);
               Line_To (Context, Point (0             ).X, Y2);
               Close_Path (Context);
               Set_Source_RGB
               (  Cr    => Context,
                  Red   => GDouble (Red (Layer.Line.Color)) /
                           GDouble (GUInt16'Last),
                  Green => GDouble (Green (Layer.Line.Color)) /
                           GDouble (GUInt16'Last),
                  Blue  => GDouble (Blue (Layer.Line.Color)) /
                           GDouble (GUint16'Last)
               );
               Fill (Context);
            else -- Transparent filling
               Line_To (Context, Point (Data.Count - 1).X, Y2);
               Line_To (Context, Point (0             ).X, Y2);
               Close_Path (Context);
               Set_Source_RGBA
               (  Cr    => Context,
                  Red   => GDouble (Red   (Layer.Line.Color)) /
                           GDouble (GUInt16'Last),
                  Green => GDouble (Green (Layer.Line.Color)) /
                           GDouble (GUint16'Last),
                  Blue  => GDouble (Blue  (Layer.Line.Color)) /
                           GDouble (GUint16'Last),
                  Alpha => 1.0 - Layer.Opacity
               );
               Fill (Context);
                  -- Drawing the line over it
               Move_To (Point (0));
               for Index in 1..Data.Count - 1 loop
                  Line_To (Point (Index));
               end loop;
               if Layer.Widened then
                  Set_Line_Width
                  (  Context,
                     Layer.Line.Width * Layer.Widget.Get_Size
                  );
               else
                  Set_Line_Width (Context, Layer.Line.Width);
               end if;
               Set_Line_Cap (Context, Layer.Line.Line_Cap);
               Set_Source_RGB
               (  Cr    => Context,
                  Red   => GDouble (Red (Layer.Line.Color)) /
                           GDouble (GUInt16'Last),
                  Green => GDouble (Green (Layer.Line.Color)) /
                           GDouble (GUInt16'Last),
                  Blue  => GDouble (Blue (Layer.Line.Color)) /
                           GDouble (GUInt16'Last)
               );
               Stroke (Context);
            end if;
         end;
         Set_Antialias (Context, Antialias);
      end;
   end if;
exception
   when Error : others =>
      Log
      (  GtkAda_Contributions_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("Draw_Lines")
      )  );
end Draw_Lines;
