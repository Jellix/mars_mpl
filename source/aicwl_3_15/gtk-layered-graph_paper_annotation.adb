--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Graph_Paper_Annotation          Luebeck            --
-- Implementation                                  Summer, 2011       --
--                                                                    --
--                                Last revision :  09:08 05 Mar 2017  --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Ada.Numerics;                use Ada.Numerics;
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with GLib.Messages;               use GLib.Messages;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Label;           use Gtk.Layered.Label;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;
with Strings_Edit.Integers;       use Strings_Edit.Integers;

with Cairo.Font_Slant_Property;
with Pango.Enums.Weight_Property;
with Gtk.Layered.Alignment_Property;
with Gtk.Layered.Vertical_Alignment_Property;
with Pango.Cairo.Fonts.Font_Type_Property;
with Strings_Edit.Integers.Superscript;

package body Gtk.Layered.Graph_Paper_Annotation is
   use Gtk.Layered.Waveform.Edit;

   type Layer_Property is
        (  Property_Scaled,
           Property_Enlarged,
           Property_Orientation,
           Property_Alignment,
           Property_From,
           Property_To,
           Property_Value,
           Property_Font_Type,
           Property_Family,
           Property_Slant,
           Property_Font_Size,
           Property_Weight,
           Property_Height,
           Property_Stretch,
           Property_Use_Superscript,
           Property_Text_Angle,
           Property_X_Justify,
           Property_Y_Justify,
           Property_Color,
           Property_Background_Color,
           Property_Border,
           Property_Overlap,
           Property_Opacity
        );

   function "+" (Value : Annotation_Text_Ptr) return UTF8_String is
   begin
      if Value = null then
         return "";
      else
         return Value.Buffer (1..Value.Length);
      end if;
   end "+";

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Graph_Paper_Annotation." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Graph_Paper_Annotation_Layer,
             Graph_Paper_Annotation_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Graph_Paper_Time_Annotation_Layer,
             Graph_Paper_Time_Annotation_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Annotation_Text,
             Annotation_Text_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Annotation_List,
             Annotation_List_Ptr
          );

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Graph_Paper_Annotation_Layer is
      Ptr : Graph_Paper_Annotation_Ptr :=
               new Graph_Paper_Annotation_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access
                      Graph_Paper_Time_Annotation_Layer is
      Ptr : Graph_Paper_Time_Annotation_Ptr :=
               new Graph_Paper_Time_Annotation_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   function Add_Annotation_Implementation
            (  Under       : not null access Layer_Location'Class;
               Paper       : access Graph_Paper_Layer'Class;
               Location    : Axis_Location;
               Face        : Pango_Cairo_Font;
               Height      : GDouble;
               Stretch     : GDouble;
               Color       : Gdk_Color;
               Text_Angle  : GDouble;
               Justify_X   : Alignment;
               Justify_Y   : Vertical_Alignment;
               Superscript : Boolean;
               Background  : Gdk_Color;
               Border      : GDouble;
               Overlap     : GDouble;
               Opacity     : Fill_Opacity;
               Scaled      : Boolean;
               Enlarged    : Boolean
            )  return Graph_Paper_Annotation_Ptr is
      Ptr   : Graph_Paper_Annotation_Ptr :=
                 new Graph_Paper_Annotation_Layer;
      Layer : Graph_Paper_Annotation_Layer renames Ptr.all;
   begin
      Layer.Scaled   := Scaled;
      Layer.Enlarged := Enlarged;
      Add (Ptr, Under);
      Set
      (  Layer       => Layer,
         Location    => Location,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Color       => Color,
         Text_Angle  => Text_Angle,
         Justify_X   => Justify_X,
         Justify_Y   => Justify_Y,
         Superscript => Superscript,
         Background  => Background,
         Border      => Border,
         Overlap     => Overlap,
         Opacity     => Opacity
      );
      if Paper /= null then
         Paper.Attach (Layer);
         Layer.Paper := Paper.all'Unchecked_Access;
      end if;
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Annotation_Implementation;

   function Add_Annotation_Implementation
            (  Under       : not null access Layer_Location'Class;
               Paper       : access Graph_Paper_Layer'Class;
               Location    : Axis_Location;
               Face        : Pango_Cairo_Font;
               Height      : GDouble;
               Stretch     : GDouble;
               Color       : Gdk_Color;
               Text_Angle  : GDouble;
               Justify_X   : Alignment;
               Justify_Y   : Vertical_Alignment;
               Superscript : Boolean;
               Background  : Gdk_Color;
               Border      : GDouble;
               Overlap     : GDouble;
               Opacity     : Fill_Opacity;
               Scaled      : Boolean;
               Enlarged    : Boolean
            )  return Graph_Paper_Time_Annotation_Ptr is
      Ptr   : Graph_Paper_Time_Annotation_Ptr :=
                 new Graph_Paper_Time_Annotation_Layer;
      Layer : Graph_Paper_Time_Annotation_Layer renames Ptr.all;
   begin
      Layer.Scaled   := Scaled;
      Layer.Enlarged := Enlarged;
      Add (Ptr, Under);
      Set
      (  Layer       => Layer,
         Location    => Location,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Color       => Color,
         Text_Angle  => Text_Angle,
         Justify_X   => Justify_X,
         Justify_Y   => Justify_Y,
         Superscript => Superscript,
         Background  => Background,
         Border      => Border,
         Overlap     => Overlap,
         Opacity     => Opacity
      );
      if Paper /= null then
         Paper.Attach (Layer);
         Layer.Paper := Paper.all'Unchecked_Access;
      end if;
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Annotation_Implementation;

   procedure Add_Graph_Paper_Annotation
             (  Under       : not null access Layer_Location'Class;
                Paper       : not null access Graph_Paper_Layer'Class;
                Location    : Axis_Location :=
                                 (  Orientation => Vertical,
                                    Alignment   => Absolute,
                                    Top         => -0.5,
                                    Bottom      => -0.5,
                                    X_Position  =>  0.0
                                 );
                Face        : Pango_Cairo_Font :=
                                 Create_Pango ("arial unicode ms");
                Height      : GDouble            := 12.0;
                Stretch     : GDouble            := 1.0;
                Color       : Gdk_Color          := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble            := 0.0;
                Justify_X   : Alignment          := Center;
                Justify_Y   : Vertical_Alignment := Center;
                Superscript : Boolean            := True;
                Background  : Gdk_Color          := RGB (1.0, 1.0, 1.0);
                Border      : GDouble            := 2.0;
                Overlap     : GDouble            :=-2.0;
                Opacity     : Fill_Opacity       := 1.0;
                Scaled      : Boolean            := False;
                Enlarged    : Boolean            := False
             )  is
      Ptr : Graph_Paper_Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
            Paper       => Paper,
            Location    => Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Text_Angle  => Text_Angle,
            Justify_X   => Justify_X,
            Justify_Y   => Justify_Y,
            Color       => Color,
            Superscript => Superscript,
            Background  => Background,
            Border      => Border,
            Overlap     => Overlap,
            Opacity     => Opacity,
            Scaled      => Scaled,
            Enlarged    => Enlarged
         );
   end Add_Graph_Paper_Annotation;

   function Add_Graph_Paper_Annotation
            (  Under       : not null access Layer_Location'Class;
               Paper       : not null access Graph_Paper_Layer'Class;
               Location    : Axis_Location :=
                                (  Orientation => Vertical,
                                   Alignment   => Absolute,
                                   Top         => -0.5,
                                   Bottom      =>  0.5,
                                   X_Position  =>  0.0
                                );
               Face        : Pango_Cairo_Font :=
                                Create_Pango ("arial unicode ms");
               Height      : GDouble            := 12.0;
               Stretch     : GDouble            := 1.0;
               Color       : Gdk_Color          := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble            := 0.0;
               Justify_X   : Alignment          := Center;
               Justify_Y   : Vertical_Alignment := Center;
               Superscript : Boolean            := True;
               Background  : Gdk_Color          := RGB (1.0, 1.0, 1.0);
               Border      : GDouble            := 2.0;
               Overlap     : GDouble            :=-2.0;
               Opacity     : Fill_Opacity       := 1.0;
               Scaled      : Boolean            := False;
               Enlarged    : Boolean            := False
            )  return not null access Graph_Paper_Annotation_Layer is
      Ptr : Graph_Paper_Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
            Paper       => Paper,
            Location    => Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Text_Angle  => Text_Angle,
            Justify_X   => Justify_X,
            Justify_Y   => Justify_Y,
            Color       => Color,
            Superscript => Superscript,
            Background  => Background,
            Border      => Border,
            Overlap     => Overlap,
            Opacity     => Opacity,
            Scaled      => Scaled,
            Enlarged    => Enlarged
         );
      return Ptr.all'Unchecked_Access;
   end Add_Graph_Paper_Annotation;

   procedure Add_Graph_Paper_Time_Annotation
             (  Under       : not null access Layer_Location'Class;
                Paper       : not null access Graph_Paper_Layer'Class;
                Location    : Axis_Location :=
                                 (  Orientation => Horizontal,
                                    Alignment   => Absolute,
                                    Left        => -0.5,
                                    Right       => -0.5,
                                    Y_Position  =>  0.0
                                 );
                Face        : Pango_Cairo_Font :=
                                 Create_Pango ("arial unicode ms");
                Height      : GDouble            := 12.0;
                Stretch     : GDouble            := 1.0;
                Color       : Gdk_Color          := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble            := 0.0;
                Justify_X   : Alignment          := Center;
                Justify_Y   : Vertical_Alignment := Center;
                Superscript : Boolean            := True;
                Background  : Gdk_Color          := RGB (1.0, 1.0, 1.0);
                Border      : GDouble            := 2.0;
                Overlap     : GDouble            :=-2.0;
                Opacity     : Fill_Opacity       := 1.0;
                Scaled      : Boolean            := False;
                Enlarged    : Boolean            := False
             )  is
      Ptr : Graph_Paper_Time_Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
            Paper       => Paper,
            Location    => Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Text_Angle  => Text_Angle,
            Justify_X   => Justify_X,
            Justify_Y   => Justify_Y,
            Color       => Color,
            Superscript => Superscript,
            Background  => Background,
            Border      => Border,
            Overlap     => Overlap,
            Opacity     => Opacity,
            Scaled      => Scaled,
            Enlarged    => Enlarged
         );
   end Add_Graph_Paper_Time_Annotation;

   function Add_Graph_Paper_Time_Annotation
            (  Under       : not null access Layer_Location'Class;
               Paper       : not null access Graph_Paper_Layer'Class;
               Location    : Axis_Location :=
                                (  Orientation => Horizontal,
                                   Alignment   => Absolute,
                                   Left        => -0.5,
                                   Right       => -0.5,
                                   Y_Position  =>  0.0
                                );
               Face        : Pango_Cairo_Font :=
                                Create_Pango ("arial unicode ms");
               Height      : GDouble            := 12.0;
               Stretch     : GDouble            := 1.0;
               Color       : Gdk_Color          := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble            := 0.0;
               Justify_X   : Alignment          := Center;
               Justify_Y   : Vertical_Alignment := Center;
               Superscript : Boolean            := True;
               Background  : Gdk_Color          := RGB (1.0, 1.0, 1.0);
               Border      : GDouble            := 2.0;
               Overlap     : GDouble            :=-2.0;
               Opacity     : Fill_Opacity       := 1.0;
               Scaled      : Boolean            := False;
               Enlarged    : Boolean            := False
            )  return not null access
                      Graph_Paper_Time_Annotation_Layer is
      Ptr : Graph_Paper_Time_Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
            Paper       => Paper,
            Location    => Location,
            Face        => Face,
            Height      => Height,
            Stretch     => Stretch,
            Text_Angle  => Text_Angle,
            Justify_X   => Justify_X,
            Justify_Y   => Justify_Y,
            Color       => Color,
            Superscript => Superscript,
            Background  => Background,
            Border      => Border,
            Overlap     => Overlap,
            Opacity     => Opacity,
            Scaled      => Scaled,
            Enlarged    => Enlarged
         );
      return Ptr.all'Unchecked_Access;
   end Add_Graph_Paper_Time_Annotation;

   procedure Detached
             (  Annotation : in out Graph_Paper_Annotation_Layer
             )  is
   begin
      Annotation.Paper := null;
   end Detached;

   procedure Changed
             (  Layer  : in out Graph_Paper_Annotation_Layer;
                Paper  : Graph_Paper_Layer'Class;
                Box    : Cairo_Box;
                X1, X2 : X_Axis;
                Y1, Y2 : Y_Axis
             )  is
      use Gtk.Layered.Waveform.Rasters;
      Changed : Boolean := Layer.Updated;
   begin
      case Layer.Location.Orientation is
         when Horizontal =>
            if Layer.Raster /= Paper.Get_X_Raster then
               Layer.Raster := Paper.Get_X_Raster;
               Changed := True;
            end if;
         when Vertical =>
            if Layer.Raster /= Paper.Get_Y_Raster then
               Layer.Raster := Paper.Get_Y_Raster;
               Changed := True;
            end if;
      end case;
      if Layer.T1 /= GDouble (X1) or else Layer.T2 /= GDouble (X2) then
         Layer.T1 := GDouble (X1);
         Layer.T2 := GDouble (X2);
         Changed := True;
      end if;
      if Layer.V1 /= GDouble (Y1) or else Layer.V2 /= GDouble (Y2) then
         Layer.V1 := GDouble (Y1);
         Layer.V2 := GDouble (Y2);
         Changed := True;
      end if;
      if Layer.Box /= Box then
         Layer.Box := Box;
         Changed := True;
      end if;
      Layer.Updated := Changed;
      if Changed and not Layer.Widget.Drawing then
         Queue_Draw (Layer.Widget); -- Signal draw to the widget
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed")
         )  );
   end Changed;

   procedure Delete (List : in out Annotation_List_Ptr) is
   begin
      if List /= null then
         for Index in List'Range loop
            Free (List (Index));
         end loop;
         Free (List);
      end if;
   end Delete;

   procedure Draw
             (  Layer   : in out Graph_Paper_Annotation_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      dX : constant GDouble := Layer.Box.X2 - Layer.Box.X1 + 1.0;
      dY : constant GDouble := Layer.Box.Y2 - Layer.Box.Y1 + 1.0;
      dT : constant GDouble := Layer.T2 - Layer.T1;
      dV : constant GDouble := Layer.V2 - Layer.V1;
      FX, FY : Long_Double;
      X0, Y0 : Long_Double;

      function To_X (T : GDouble) return GDouble is
         pragma Inline (To_X);
      begin
         return
            GDouble (Long_Double'Rounding (FX * Long_Double (T)) + X0);
      end To_X;

      function To_Y (V : GDouble) return GDouble is
         pragma Inline (To_Y);
      begin
         return
            GDouble (Y0 - Long_Double'Rounding (FY * Long_Double (V)));
      end To_Y;

      Cos_A   : constant GDouble := abs cos (Layer.Text_Angle);
      Sin_A   : constant GDouble := abs sin (Layer.Text_Angle);
      Old_Box : Cairo_Box := (others => GDouble'First);
      New_Box : Cairo_Box;
      Border  : GDouble := Layer.Border;
      Overlap : GDouble := Layer.Overlap;
      From    : GDouble; -- The first point of the annotation
      To      : GDouble; -- The last point of the annotation

      function Draw_Text (X, Y : GDouble; Index : Positive)
         return Boolean is
         Extents : Cairo_Text_Extents;
         Gain    : GDouble;
         Box     : Cairo_Tuple;
         Width   : GDouble;
         Height  : GDouble;
      begin
         if (  Layer.Texts = null
            or else
               Index > Layer.Texts'Length
            or else
               Layer.Texts (Index) = null
            or else
               Layer.Texts (Index).Length = 0
            )
         then
            return False;
         end if;
         Get_Markup_Extents
         (  Layer.Face,
            Context,
            +Layer.Texts (Index),
            Extents
         );
         if Extents.Height <= 0.0 or else Extents.Width <= 0.0 then
            return False;
         end if;
         Gain := Layer.Height / Extents.Height;
         if Layer.Enlarged then
            Gain := Gain * Layer.Widget.Get_Size;
         end if;
         Box :=
            (  Gain * Extents.Width * Layer.Stretch + Border * 2.0,
               Gain * Extents.Height                + Border * 2.0
            );
         Width  := Cos_A * Box.X + Sin_A * Box.Y;
         Height := Sin_A * Box.X + Cos_A * Box.Y;
         case Layer.Justify_X is
            when Left   => New_Box.X1 := X;
            when Center => New_Box.X1 := X - Width / 2.0;
            when Right  => New_Box.X1 := X - Width;
         end case;
         New_Box.X2 := New_Box.X1 + Width;
         case Layer.Justify_Y is
            when Top    => New_Box.Y1 := Y;
            when Center => New_Box.Y1 := Y - Height / 2.0;
            when Bottom => New_Box.Y1 := Y - Height;
         end case;
         New_Box.Y2 := New_Box.Y1 + Height;
         case Layer.Location.Orientation is
            when Horizontal =>
               if New_Box.X1 < From or else New_Box.X2 > To then
                  return False;
               end if;
            when Vertical =>
               if New_Box.Y1 < From or else New_Box.Y2 > To then
                  return False;
               end if;
         end case;
         if (  (  New_Box.X1 in Old_Box.X1..Old_Box.X2
               or else
                  New_Box.X2 in Old_Box.X1..Old_Box.X2
               )
            and then
               (  New_Box.Y1 in Old_Box.Y1..Old_Box.Y2
               or else
                  New_Box.Y2 in Old_Box.Y1..Old_Box.Y2
            )  )
         then
            return False;
         else
            Old_Box :=
               (  X1 => New_Box.X1 + Overlap,
                  X2 => New_Box.X2 - Overlap,
                  Y1 => New_Box.Y1 + Overlap,
                  Y2 => New_Box.Y2 - Overlap
               );
         end if;
         declare
            State : Context_State := Save (Context);
         begin
            Translate
            (  Context,
               New_Box.X1 + Width / 2.0,
               New_Box.Y1 + Height / 2.0
            );
            Rotate (Context, Layer.Text_Angle);
            if Layer.Opacity < 1.0 then
               Move_To (Context, -Box.X / 2.0, -Box.Y / 2.0);
               Rel_Line_To (Context, Box.X, 0.0);
               Rel_Line_To (Context, 0.0, Box.Y);
               Rel_Line_To (Context, -Box.X, 0.0);
               Close_Path (Context);
               Set_Source_RGBA
               (  Cr    => Context,
                  Red   => GDouble (Red   (Layer.Background)) /
                           GDouble (GUInt16'Last),
                  Green => GDouble (Green (Layer.Background)) /
                           GDouble (GUInt16'Last),
                  Blue  => GDouble (Blue  (Layer.Background)) /
                           GDouble (GUInt16'Last),
                  Alpha => 1.0 - Layer.Opacity
               );
               Fill (Context);
               Set_Source_RGB
               (  Cr    => Context,
                  Red   => GDouble (Red   (Layer.Color)) /
                           GDouble (GUInt16'Last),
                  Green => GDouble (Green (Layer.Color)) /
                           GDouble (GUInt16'Last),
                  Blue  => GDouble (Blue  (Layer.Color)) /
                           GDouble (GUInt16'Last)
               );
            end if;
            Scale (Context, Gain * Layer.Stretch, Gain);
            Move_To
            (  Cr => Context,
               X  => -Extents.X_Bearing - Extents.Width  * 0.5,
               Y  => -Extents.Y_Bearing - Extents.Height * 0.5
            );
            Show_Markup (Layer.Face, Context, +Layer.Texts (Index));
         end;
         return True;
      end Draw_Text;

      Onto : GDouble; -- The line location

      Step : constant GDouble :=
                      (  GDouble (Layer.Raster.Minor)
                      *  GDouble (Layer.Raster.Ticks + 1)
                      );
      V0   : constant GDouble :=
                      (  GDouble (Layer.Raster.Low_Value)
                      +  GDouble (Layer.Raster.Minor)
                      *  GDouble
                         (  Layer.Raster.Ticks
                         -  Layer.Raster.Low_Tick
                         +  1
                      )  );

      procedure Draw_X is
         Start  : Integer;
         Box    : Cairo_Box;
         Median : constant GDouble := V0 + dT * 0.5;
         function Draw (Index : Integer) return Boolean is
            pragma Inline (Draw);
            T, X : GDouble;
         begin
            T := V0 + Step * GDouble (Index);
            X := To_X (T);
            if X not in From..To then
               return False;
            else
               if Draw_Text (X, Onto, Index + 1) then
                  if (  abs (Median - T)
                     <  abs (Median - Layer.Middle_Value)
                     )
                  then
                     Layer.Middle_Value := T;
                  end if;
                  if Index = Start then
                     Box := Old_Box;
                  end if;
               end if;
               return True;
            end if;
         end Draw;
         V1 : GDouble;
      begin
         if dX > 0.0 and then dT > 0.0 then
            V1 := V0 + dT;
            if Layer.Middle_Value in V0..V1 then
               Start := Integer ((Layer.Middle_Value - V0) / Step);
            else
               Start := Integer (0.5 * dT / Step);
            end if;
            Layer.Middle_Value := GDouble'First;
            if Draw (Start) then
               for Index in reverse 0..Start - 1 loop
                  exit when not Draw (Index);
               end loop;
               Old_Box := Box;
               for Index in Start + 1..Natural'Last loop
                  exit when not Draw (Index);
               end loop;
            end if;
         end if;
      end Draw_X;

      procedure Draw_Y is
         Start  : Integer;
         Box    : Cairo_Box;
         Median : constant GDouble := V0 + dV * 0.5;
         function Draw (Index : Integer) return Boolean is
            pragma Inline (Draw);
            V, Y : GDouble;
         begin
            V := V0 + Step * GDouble (Index);
            Y := To_Y (V);
            if Y not in From..To then
               return False;
            else
               if Draw_Text (Onto, Y, Index + 1) then
                  if (  abs (Median - V)
                     <  abs (Median - Layer.Middle_Value)
                     )
                  then
                     Layer.Middle_Value := V;
                  end if;
                  if Index = Start then
                     Box := Old_Box;
                  end if;
               end if;
               return True;
            end if;
         end Draw;
         V1 : GDouble;
      begin
         if dY > 0.0 and then dV > 0.0 then
            V1 := V0 + dV;
            if Layer.Middle_Value in V0..V1 then
               Start := Integer ((Layer.Middle_Value - V0) / Step);
            else
               Start := Integer (0.5 * dV / Step);
            end if;
            Layer.Middle_Value := GDouble'First;
            if Draw (Start) then
               for Index in reverse 0..Start - 1 loop
                  exit when not Draw (Index);
               end loop;
               Old_Box := Box;
               for Index in Start + 1..Natural'Last loop
                  exit when not Draw (Index);
               end loop;
            end if;
         end if;
      end Draw_Y;

      use Gtk.Layered.Waveform.Rasters;

      function Set_X return Boolean is
      begin
         if dT <= 0.0 then
            Layer.Updated := False;
            return True;
         else
            FX := Long_Double (dX) / Long_Double (dT);
            X0 := (  Long_Double'Rounding (Long_Double (Layer.Box.X1))
                  -  Long_Double'Rounding (FX * Long_Double (Layer.T1))
                  );
            return False;
         end if;
      end Set_X;

      function Set_Y return Boolean is
      begin
         if dV <= 0.0 then
            Layer.Updated := False;
            return True;
         else
            FY := Long_Double (dY) / Long_Double (dV);
            Y0 := (  Long_Double'Rounding (Long_Double (Layer.Box.Y2))
                  +  Long_Double'Rounding (FY * Long_Double (Layer.V1))
                  );
            return False;
         end if;
      end Set_Y;
   begin
      if Layer.Scaled then
         declare
            X_Size : constant GDouble :=
                     GDouble (Layer.Widget.Get_Allocated_Width);
            Y_Size : constant GDouble :=
                     GDouble (Layer.Widget.Get_Allocated_Height);
         begin
            case Layer.Location.Orientation is
               when Horizontal =>
                  if Set_X then return; end if;
                  From :=
                     (  Layer.Location.Left * X_Size
                     +  Layer.Widget.Get_Center.X
                     );
                  To :=
                     (  Layer.Location.Right * X_Size
                     +  Layer.Widget.Get_Center.X
                     );
                  case Layer.Location.Alignment is
                     when Absolute =>
                        Onto :=
                           (  Layer.Location.Y_Position * Y_Size
                           +  Layer.Widget.Get_Center.Y
                           );
                     when Relative =>
                        if Set_Y then return; end if;
                        Onto := GDouble (Layer.Location.Y_Value);
                        if Onto < Layer.V1 or else Onto > Layer.V2 then
                           Layer.Updated := False;
                           return;
                        end if;
                        Onto := To_Y (Onto);
                  end case;
               when Vertical =>
                  if Set_Y then return; end if;
                  From :=
                     (  Layer.Location.Top * Y_Size
                     +  Layer.Widget.Get_Center.Y
                     );
                  To :=
                     (  Layer.Location.Bottom * Y_Size
                     +  Layer.Widget.Get_Center.Y
                     );
                  case Layer.Location.Alignment is
                     when Absolute =>
                        Onto :=
                           (  Layer.Location.X_Position * X_Size
                           +  Layer.Widget.Get_Center.X
                           );
                     when Relative =>
                        if Set_X then return; end if;
                        Onto := GDouble (Layer.Location.X_Value);
                        if Onto < Layer.T1 or else Onto > Layer.T2 then
                           Layer.Updated := False;
                           return;
                        end if;
                        Onto := To_X (Onto);
                  end case;
            end case;
         end;
      else
         case Layer.Location.Orientation is
            when Horizontal =>
               if Set_X then return; end if;
               From := Layer.Location.Left;
               To   := Layer.Location.Right;
               case Layer.Location.Alignment is
                  when Absolute =>
                     Onto := Layer.Location.Y_Position;
                  when Relative =>
                     if Set_Y then return; end if;
                     Onto := GDouble (Layer.Location.Y_Value);
                     if Onto < Layer.V1 or else Onto > Layer.V2 then
                        Layer.Updated := False;
                        return;
                     end if;
                     Onto := To_Y (Onto);
               end case;
            when Vertical =>
               if Set_Y then return; end if;
               From := Layer.Location.Top;
               To   := Layer.Location.Bottom;
               case Layer.Location.Alignment is
                  when Absolute =>
                     Onto := Layer.Location.X_Position;
                  when Relative =>
                     if Set_X then return; end if;
                     Onto := GDouble (Layer.Location.X_Value);
                     if Onto < Layer.T1 or else Onto > Layer.T2 then
                        Layer.Updated := False;
                        return;
                     end if;
                     Onto := To_X (Onto);
               end case;
         end case;
      end if;
      declare
         State : Context_State := Save (Context);
      begin
         New_Path (Context);
         Set_Source_RGB
         (  Context,
            GDouble (Red   (Layer.Color)) / GDouble (GUInt16'Last),
            GDouble (Green (Layer.Color)) / GDouble (GUInt16'Last),
            GDouble (Blue  (Layer.Color)) / GDouble (GUInt16'Last)
         );
         if Layer.Enlarged then
            Border  := Border  * Layer.Widget.Get_Size;
            Overlap := Overlap * Layer.Widget.Get_Size;
         end if;
         if Layer.Updated then -- Renewing texts
            declare
               V, V2  : GDouble;
               Suffix : constant String := +Layer.Suffix;
            begin
               if Step > 0.0 then
                  case Layer.Location.Orientation is
                     when Horizontal =>
                        V2 := Layer.T2;
                     when Vertical =>
                        V2 := Layer.V2;
                  end case;
                  for Index in Natural'Range loop
                     V := V0 + Step * GDouble (Index);
                     exit when V > V2;
                     if Layer.Renderer = null then
                        Layer.Set_Text
                        (  Index + 1,
                           (  Graph_Paper_Annotation_Layer'Class
                              (  Layer
                              ) .Render (V, Layer.Raster)
                           &  Suffix
                        )  );
                     else
                        Layer.Set_Text
                        (  Index + 1,
                           (  Layer.Renderer
                              (  Layer,
                                 V,
                                 Layer.Raster
                              )
                           &  Suffix
                        )  );
                     end if;
                  end loop;
               end if;
            end;
         end if;
         if Step > 0.0 then
            case Layer.Location.Orientation is
               when Horizontal =>
                  Draw_X;
               when Vertical =>
                  Draw_Y;
            end case;
         end if;
      end;
      Layer.Updated := False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Draw")
         )  );
   end Draw;

   procedure Finalize (Layer : in out Graph_Paper_Annotation_Layer) is
   begin
      if Layer.Paper /= null then
         Layer.Paper.Detach (Layer);
         Layer.Paper := null;
      end if;
      Finalize (Abstract_Layer (Layer));
      Delete (Layer.Texts);
      Free (Layer.Suffix);
   end Finalize;

   function Get_Background_Color (Layer : Graph_Paper_Annotation_Layer)
      return Gdk_Color is
   begin
      return Layer.Background;
   end Get_Background_Color;

   function Get_Border (Layer : Graph_Paper_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Border;
   end Get_Border;

   function Get_Color (Layer : Graph_Paper_Annotation_Layer)
      return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Enlarged (Layer : Graph_Paper_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Enlarged;
   end Get_Enlarged;

  function Get_Face (Layer : Graph_Paper_Annotation_Layer)
      return Pango_Cairo_Font is
   begin
      return Layer.Face;
   end Get_Face;

   function Get_Height (Layer : Graph_Paper_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Justify_X (Layer : Graph_Paper_Annotation_Layer)
      return Alignment is
   begin
      return Layer.Justify_X;
   end Get_Justify_X;

   function Get_Justify_Y (Layer : Graph_Paper_Annotation_Layer)
      return Vertical_Alignment is
   begin
      return Layer.Justify_Y;
   end Get_Justify_Y;

   function Get_Location (Layer : Graph_Paper_Annotation_Layer)
      return Axis_Location is
   begin
      return Layer.Location;
   end Get_Location;

   function Get_Opacity (Layer : Graph_Paper_Annotation_Layer)
      return Fill_Opacity is
   begin
      return Layer.Opacity;
   end Get_Opacity;

   function Get_Overlap (Layer : Graph_Paper_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Overlap;
   end Get_Overlap;

   function Get_Properties_Number
            (  Layer : Graph_Paper_Annotation_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Graph_Paper_Annotation_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Font_Type =>
               return
                  Pango.Cairo.Fonts.Font_Type_Property.Gnew_Enum
                  (  Name    => "font-type",
                     Nick    => "font type",
                     Default => Pango_Font,
                     Blurb   => "The backend used for the font, " &
                                "e.g. toy font, pango font"
                  );
            when Property_Border =>
               return
                  Gnew_Double
                  (  Name    => "border-width",
                     Nick    => "border",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 2.0,
                     Blurb   => "The amount added to the four sides" &
                                "of the annotation text extents box " &
                                "in order to get the corresponding " &
                                "background box"
                  );
            when Property_Overlap =>
               return
                  Gnew_Double
                  (  Name    => "border-overlap",
                     Nick    => "overlap",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default =>-2.0,
                     Blurb   => "The amount inside the box allowed" &
                                "to overlap other boxes"
                  );
            when Property_From =>
               return
                  Gnew_Double
                  (  Name    => "from",
                     Nick    => "from",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the point " &
                                "of the line along which annotation " &
                                "texts are arranged when the " &
                                "annotation is horizontal. When " &
                                "annotation is vertical it is the " &
                                "y-coordinate. Annotation text is " &
                                "shown only the corresponding " &
                                "coordinate of the box is no less " &
                                "than the property value"
                  );
            when Property_To =>
               return
                  Gnew_Double
                  (  Name    => "to",
                     Nick    => "to",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the point " &
                                "of the line along which annotation " &
                                "texts are arranged when the " &
                                "annotation is horizontal. When " &
                                "annotation is vertical it is the " &
                                "y-coordinate. Annotation text is " &
                                "shown only the corresponding " &
                                "coordinate of the box is no greater " &
                                "than the property value"
                  );
            when Property_Value =>
               return
                  Gnew_Double
                  (  Name    => "value",
                     Nick    => "value",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the line along " &
                                "which annotation texts are " &
                                "arranged when the annotation is " &
                                "horizontal. When annotation is " &
                                "vertical it is the x-coordinate. " &
                                "When annotation position is " &
                                "relative, the value is the y- or " &
                                "x-value. In the latter case an " &
                                "annotation text is shown only if " &
                                "in the corresponding range of the " &
                                "graph paper box"
                  );
            when Property_Text_Angle =>
               return
                  Gnew_Double
                  (  Name    => "text-angle",
                     Nick    => "text angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the annotation texts " &
                                "base line"
                  );
            when Property_Stretch =>
               return
                  Gnew_Double
                  (  Name    => "stretch",
                     Nick    => "stretch",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The relation of the rendered width " &
                                "of an annotation text to its " &
                                "original width. The stretch value " &
                                "1 keeps texts unchanged"
                  );
            when Property_Height =>
               return
                  Gnew_Double
                  (  Name    => "height",
                     Nick    => "height",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 12.0,
                     Blurb   => "The annotation text font height"
                  );
            when Property_Family =>
               return
                  Gnew_String
                  (  Name    => "font-familiy",
                     Nick    => "font famility",
                     Default => "arial",
                     Blurb   => "The annotation text font family, " &
                                "e.g. courier"
                  );
            when Property_Opacity =>
               return
                  Gnew_Double
                  (  Name    => "background-opacity",
                     Nick    => "opacity",
                     Minimum => 0.0,
                     Maximum => 1.0,
                     Default => 1.0,
                     Blurb   => "The opacity of the text background " &
                                "boxes"
                  );
            when Property_X_Justify =>
               return
                  Gtk.Layered.Alignment_Property.Gnew_Enum
                  (  Name    => "text-alignment",
                     Nick    => "text X-alignment",
                     Default => Center,
                     Blurb   => "The text horizontal alignment " &
                                "relatively to the annotation " &
                                "line"
                  );
            when Property_Y_Justify =>
               return
                  Gtk.Layered.Vertical_Alignment_Property.Gnew_Enum
                  (  Name    => "text-vertical-alignment",
                     Nick    => "text Y-alignment",
                     Default => Center,
                     Blurb   => "The vertical horizontal alignment " &
                                "relatively to the annotation " &
                                "line"
                  );
            when Property_Slant =>
               return
                  Cairo.Font_Slant_Property.Gnew_Enum
                  (  Name    => "font-slant",
                     Nick    => "font slant",
                     Default => CAIRO_FONT_SLANT_NORMAL,
                     Blurb   => "The annotation text font slant"
                  );
            when Property_Font_Size =>
               return
                  Gnew_UInt
                  (  Name    => "font-size",
                     Nick    => "font size",
                     Minimum => 1,
                     Maximum => GUInt (GInt'Last),
                     Default => 12,
                     Blurb   => "The font size in points. " &
                                "The value is only relevant for " &
                                "pango fonts. For cairo toy size " &
                                "is ignored"
                  );
            when Property_Weight =>
               return
                  Pango.Enums.Weight_Property.Gnew_Enum
                  (  Name    => "font-weight",
                     Nick    => "font weight",
                     Default => Pango.Enums.Pango_Weight_Normal,
                     Blurb   => "The annotation text font weight"
                  );
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The annotation texts color"
                  );
            when Property_Background_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "background-color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "background",
                     Blurb      => "The annotation texts background " &
                                   "color"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The annotation size is changed when " &
                                "the widget is resized"
                  );
            when Property_Use_Superscript =>
               return
                  Gnew_Boolean
                  (  Name    => "use-superscript",
                     Nick    => "superscript",
                     Default => False,
                     Blurb   => "The annotation text may use " &
                                "superscript characters for " &
                                "the exponent part. When false, " &
                                "the Eyy notation is used instead"
                  );
            when Property_Enlarged =>
               return
                  Gnew_Boolean
                  (  Name    => "enlarged",
                     Nick    => "enlarged",
                     Default => False,
                     Blurb   => "The annotation texts sizes are " &
                                "changed when the widget is resized"
                  );
            when Property_Orientation =>
               return
                  Gnew_Boolean
                  (  Name    => "orientation",
                     Nick    => "orientation",
                     Default => False,
                     Blurb   => "The annotation corresponds to the " &
                                "x-axis when false or to the y-axis " &
                                "otherwise"
                  );
            when Property_Alignment =>
               return
                  Gnew_Boolean
                  (  Name    => "alignment",
                     Nick    => "alignment",
                     Default => False,
                     Blurb   => "The annotation line position " &
                                "is a coordinate when this property " &
                                "is false. Otherwise it is a " &
                                "value, e.g. horizontal axis located" &
                                "where the signal value would appear"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Graph_Paper_Annotation_Layer;
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
               when Property_Border =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Border);
               when Property_Font_Type =>
                  Pango.Cairo.Fonts.Font_Type_Property.Set_Enum
                  (  Value,
                     Get_Type (Layer.Face)
                  );
               when Property_Overlap =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Overlap);
               when Property_Opacity =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Opacity);
               when Property_From =>
                  Init (Value, GType_Double);
                  case Layer.Location.Orientation is
                     when Horizontal =>
                        Set_Double (Value, Layer.Location.Left);
                     when Vertical =>
                        Set_Double (Value, Layer.Location.Top);
                  end case;
               when Property_To =>
                  Init (Value, GType_Double);
                  case Layer.Location.Orientation is
                     when Horizontal =>
                        Set_Double (Value, Layer.Location.Right);
                     when Vertical =>
                        Set_Double (Value, Layer.Location.Bottom);
                  end case;
               when Property_Value =>
                  Init (Value, GType_Double);
                  case Layer.Location.Orientation is
                     when Horizontal =>
                        case Layer.Location.Alignment is
                           when Absolute =>
                              Set_Double
                              (  Value,
                                 Layer.Location.Y_Position
                              );
                           when Relative =>
                              Set_Double
                              (  Value,
                                 GDouble (Layer.Location.Y_Value)
                              );
                        end case;
                     when Vertical =>
                        case Layer.Location.Alignment is
                           when Absolute =>
                              Set_Double
                              (  Value,
                                 Layer.Location.X_Position
                              );
                           when Relative =>
                              Set_Double
                              (  Value,
                                 GDouble (Layer.Location.X_Value)
                              );
                        end case;
                  end case;
               when Property_Text_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Text_Angle);
               when Property_Background_Color =>
                  Set_Value (Value, Layer.Background);
               when Property_Color =>
                  Set_Value (Value, Layer.Color);
               when Property_Height =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Height);
               when Property_Family =>
                  Init (Value, GType_String);
                  Set_String (Value, Get_Family (Layer.Face));
               when Property_Slant =>
                  Cairo.Font_Slant_Property.Set_Enum
                  (  Value,
                     Get_Slant (Layer.Face)
                  );
               when Property_Font_Size =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Get_Size (Layer.Face)));
               when Property_Weight =>
                  Pango.Enums.Weight_Property.Set_Enum
                  (  Value,
                     Get_Weight (Layer.Face)
                  );
               when Property_X_Justify =>
                  Gtk.Layered.Alignment_Property.Set_Enum
                  (  Value,
                     Layer.Justify_X
                  );
               when Property_Y_Justify =>
                  Gtk.Layered.Vertical_Alignment_Property.Set_Enum
                  (  Value,
                     Layer.Justify_Y
                  );
               when Property_Stretch =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Stretch);
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Enlarged =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Enlarged);
               when Property_Orientation =>
                  Init (Value, GType_Boolean);
                  Set_Boolean
                  (  Value,
                     Layer.Location.Orientation = Vertical
                  );
               when Property_Use_Superscript =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Superscript);
               when Property_Alignment =>
                  Init (Value, GType_Boolean);
                  Set_Boolean
                  (  Value,
                     Layer.Location.Alignment = Relative
                  );
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scaled (Layer : Graph_Paper_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Stretch (Layer : Graph_Paper_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Stretch;
   end Get_Stretch;

   function Get_Suffix (Layer : Graph_Paper_Annotation_Layer)
      return UTF8_String is
   begin
      return +Layer.Suffix;
   end Get_Suffix;

   function Get_Superscript (Layer : Graph_Paper_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Superscript;
   end Get_Superscript;

   function Get_Text_Angle (Layer : Graph_Paper_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Text_Angle;
   end Get_Text_Angle;

   function Image
            (  Layer : Graph_Paper_Annotation_Layer;
               Value : GDouble
            )  return UTF8_String is
   begin
      if Layer.Renderer = null then
         return
         (  Graph_Paper_Annotation_Layer'Class
            (  Layer
            ) .Render (Value, Layer.Raster)
         &  (+Layer.Suffix)
         );
      else
         return
         (  Layer.Renderer (Layer, Value, Layer.Raster)
         &  (+Layer.Suffix)
         );
      end if;
   end Image;

   function Image (Interval : Duration) return String is
      use Strings_Edit;
   begin
      if Interval > 10.0 then
         return Image (GDouble (Interval), AbsSmall => 0);
      elsif Interval > 1.0 then
         return Image (GDouble (Interval), AbsSmall =>-3);
      else
         return Image (GDouble (Interval), AbsSmall =>-6);
      end if;
   end Image;

   function Image (Stamp : Ada.Calendar.Time) return String is
      use Ada.Calendar;
      use Gtk.Layered.Waveform.Edit;
      use Strings_Edit;
      Now     : constant Ada.Calendar.Time := Clock;
      Text    : String (1..80);
      Value   : constant GDouble := GDouble (Seconds (Stamp));
      Second  : constant Integer := Integer (GDouble'Floor (Value));
      Pointer : Integer := Text'First;
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Second / 3600,
         Justify     => Strings_Edit.Right,
         Field       => 2,
         Fill        => '0'
      );
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => ':'
      );
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => (Second / 60) mod 60,
         Justify     => Strings_Edit.Right,
         Field       => 2,
         Fill        => '0'
      );
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => ':'
      );
      if Second mod 60 < 10 then
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => '0'
         );
      end if;
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Value - GDouble (60 * (Second / 60)),
         AbsSmall    =>-3
      );
      if (  Year (Now) /= Year (Stamp)
         or else
            Month (Now) /= Month (Stamp)
         or else
            Day (Now) /= Day (Stamp)
         )
      then
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => " "
         );
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => Integer (Year (Stamp))
         );
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => "."
         );
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => Integer (Month (Stamp)),
            Field       => 2,
            Fill        => '0',
            Justify     => Strings_Edit.Right
         );
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => "."
         );
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => Integer (Day (Stamp)),
            Field       => 2,
            Fill        => '0',
            Justify     => Strings_Edit.Right
         );
      end if;
      return Text (Text'First..Pointer - 1);
   end Image;

   function Is_Updated (Layer : Graph_Paper_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Graph_Paper_Annotation_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      case Layer.Location.Orientation is
         when Horizontal =>
            Layer.Location.Left  := Layer.Location.Left  + Offset.X;
            Layer.Location.Right := Layer.Location.Right + Offset.X;
            case Layer.Location.Alignment is
               when Absolute =>
                  Layer.Location.Y_Position :=
                     Layer.Location.Y_Position + Offset.Y;
               when Relative =>
                  null;
            end case;
         when Vertical =>
            Layer.Location.Top    := Layer.Location.Top    + Offset.Y;
            Layer.Location.Bottom := Layer.Location.Bottom + Offset.Y;
            case Layer.Location.Alignment is
               when Absolute =>
                  Layer.Location.X_Position :=
                     Layer.Location.X_Position + Offset.X;
               when Relative =>
                  null;
            end case;
      end case;
      Layer.Updated := True;
   end Move;

   function Render
            (  Layer  : Graph_Paper_Annotation_Layer;
               Value  : GDouble;
               Raster : Gtk.Layered.Waveform.Rasters.Scale
            )  return UTF8_String is
      Small : constant Integer := 0;
      Power : Integer;
   begin
       if Raster.Small > 0 then
          Power := (Raster.Small / 3) * 3;
       elsif Raster.Small < -3 then
          Power := -3 * ((2 - Raster.Small) / 3);
       else
          return Image (Value, AbsSmall => Raster.Small);
       end if;
       declare
          Text : constant String :=
                 Image (Value / 10.0 ** Power, AbsSmall => Small);
       begin
          if Text = "0" or else Power = 0 then
             return Text;
          else
             if Layer.Superscript then
                return
                (  Text
                &  Character'Val (16#C2#)
                &  Character'Val (16#B7#)
                &  "10"
                &  Strings_Edit.Integers.Superscript.Image (Power)
                );
             else
                return Text & "E" & Image (Power);
             end if;
          end if;
      end;
   end Render;

   function Render
            (  Layer  : Graph_Paper_Time_Annotation_Layer;
               Value  : GDouble;
               Raster : Gtk.Layered.Waveform.Rasters.Scale
            )  return UTF8_String is
      use Ada.Calendar;
      use Strings_Edit;
      Text    : String (1..80);
      Stamp   : constant GDouble :=
                GDouble (Seconds (To_Time (Value)));
      Second  : constant Integer := Integer (GDouble'Floor (Stamp));
      Pointer : Integer := Text'First;
   begin
      Put
      (  Destination => Text,
         Pointer     => Pointer,
         Value       => Second / 3600,
         Justify     => Strings_Edit.Right,
         Field       => 2,
         Fill        => '0'
      );
      if Layer.Raster.Small < 4 then
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => ':'
         );
         Put
         (  Destination => Text,
            Pointer     => Pointer,
            Value       => (Second / 60) mod 60,
            Justify     => Strings_Edit.Right,
            Field       => 2,
            Fill        => '0'
         );
         if Layer.Raster.Small < 2 then
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => ':'
            );
            Put
            (  Destination => Text,
               Pointer     => Pointer,
               Value       => Second mod 60,
               Justify     => Strings_Edit.Right,
               Field       => 2,
               Fill        => '0'
            );
            if Layer.Raster.Small < 0 then
               declare
                  Fraction : constant String :=
                     Image
                     (  Value    => Stamp - GDouble (Second),
                        AbsSmall => Layer.Raster.Small
                     );
               begin
                  Put
                  (  Destination => Text,
                     Pointer     => Pointer,
                     Value  =>
                        Fraction (Fraction'First + 1..Fraction'Last)
                  );
               end;
            end if;
         end if;
      end if;
      return Text (Text'First..Pointer - 1);
   end Render;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Graph_Paper_Annotation_Layer
             )  is
      Face        : Pango_Cairo_Font;
      Height      : GDouble;
      Stretch     : GDouble;
      From        : GDouble;
      To          : GDouble;
      Value       : GDouble;
      Text_Angle  : GDouble;
      Color       : Gdk_Color;
      Justify_X   : Alignment;
      Justify_Y   : Vertical_Alignment;
      Location    : Axis_Location;
      Background  : Gdk_Color;
      Border      : GDouble;
      Overlap     : GDouble;
      Opacity     : GDouble;
      Orientation : Boolean;
      Alignment   : Boolean;
      Superscript : Boolean;
   begin
      Restore (Stream, Face);
      Restore (Stream, Height);
      Restore (Stream, Stretch);
      Restore (Stream, From);
      Restore (Stream, To);
      Restore (Stream, Value);
      Restore (Stream, Text_Angle);
      Restore (Stream, Color);
      Restore (Stream, Justify_X);
      Restore (Stream, Justify_Y);
      Restore (Stream, Background);
      Restore (Stream, Border);
      Restore (Stream, Overlap);
      Restore (Stream, Opacity);
      Restore
      (  Stream,
         Layer.Scaled,
         Layer.Enlarged,
         Orientation,
         Alignment,
         Superscript
      );
      case Orientation is
         when False =>
            case Alignment is
               when False =>
                  Location := (Horizontal, Absolute, From, To, Value);
               when True =>
                  Location :=
                     (Horizontal, Relative, From, To, Y_Axis (Value));
            end case;
         when True =>
            case Alignment is
               when False =>
                  Location := (Vertical, Absolute, From, To, Value);
               when True =>
                  Location :=
                     (Vertical, Relative, From, To, X_Axis (Value));
            end case;
      end case;
      Set
      (  Layer       => Layer,
         Location    => Location,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Color       => Color,
         Text_Angle  => Text_Angle,
         Justify_X   => Justify_X,
         Justify_Y   => Justify_Y,
         Background  => Background,
         Border      => Border,
         Overlap     => Overlap,
         Opacity     => Opacity,
         Superscript => Superscript
      );
      Layer.Updated := True;
   end Restore;

   procedure Scale
             (  Layer  : in out Graph_Paper_Annotation_Layer;
                Factor : GDouble
             )  is
      Height : constant GDouble := Layer.Height * Factor;
      Length : GDouble;
   begin
      if Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      end if;
      case Layer.Location.Orientation is
         when Horizontal =>
            Length :=
               (Layer.Location.Right - Layer.Location.Left) * Factor;
            Layer.Location.Left :=
               (  (  Layer.Location.Left
                  +  Layer.Location.Right
                  -  Length
                  )
               /  2.0
               );
            Layer.Location.Right := Layer.Location.Left + Length;
         when Vertical =>
            Length :=
               (Layer.Location.Bottom - Layer.Location.Top) * Factor;
            Layer.Location.Top :=
               (  (  Layer.Location.Top
                  +  Layer.Location.Bottom
                  -  Length
                  )
               /  2.0
               );
            Layer.Location.Bottom := Layer.Location.Top + Length;
      end case;
      Layer.Height  := Height;
      Layer.Updated := True;
   end Scale;

   procedure Set
             (  Layer       : in out Graph_Paper_Annotation_Layer;
                Location    : Axis_Location;
                Face        : Pango_Cairo_Font;
                Height      : GDouble;
                Stretch     : GDouble;
                Color       : Gdk_Color;
                Text_Angle  : GDouble;
                Justify_X   : Alignment;
                Justify_Y   : Vertical_Alignment;
                Superscript : Boolean;
                Background  : Gdk_Color;
                Border      : GDouble;
                Overlap     : GDouble;
                Opacity     : Fill_Opacity
             )  is
   begin
      if Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      elsif Stretch <= 0.0 then
         raise Constraint_Error with "Non-positive stretch";
      end if;
      case Location.Orientation is
         when Horizontal =>
            if Location.Left > Location.Right then
               raise Constraint_Error with "Negative annotation width";
            end if;
         when Vertical =>
            if Location.Top > Location.Bottom then
               raise Constraint_Error with "Negative annotation height";
            end if;
      end case;
      Layer.Location    := Location;
      Layer.Face        := Face;
      Layer.Height      := Height;
      Layer.Stretch     := Stretch;
      Layer.Color       := Color;
      Layer.Text_Angle  := Text_Angle;
      Layer.Justify_X   := Justify_X;
      Layer.Justify_Y   := Justify_Y;
      Layer.Background  := Background;
      Layer.Border      := Border;
      Layer.Overlap     := Overlap;
      Layer.Opacity     := Opacity;
      Layer.Superscript := Superscript;
      Layer.Updated     := True;
   end Set;

   procedure Set_Enlarged
             (  Layer    : in out Graph_Paper_Annotation_Layer;
                Enlarged : Boolean
             )  is
   begin
      Layer.Enlarged := Enlarged;
      Layer.Updated  := True;
   end Set_Enlarged;

   procedure Set_Face
             (  Layer : in out Graph_Paper_Annotation_Layer;
                Face  : Pango_Cairo_Font
             )  is
   begin
      Layer.Face    := Face;
      Layer.Updated := True;
   end Set_Face;

   procedure Set_Property_Value
             (  Layer    : in out Graph_Paper_Annotation_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Border =>
               Layer.Border := Get_Double (Value);
            when Property_Font_Type =>
               Set_Type
               (  Layer.Face,
                  Pango.Cairo.Fonts.Font_Type_Property.Get_Enum (Value)
               );
            when Property_Overlap =>
               Layer.Overlap := Get_Double (Value);
            when Property_Opacity =>
               Layer.Opacity :=
                  GDouble'Max
                  (  0.0,
                     GDouble'Min
                     (  1.0,
                        Get_Double (Value)
                  )  );
            when Property_From =>
               case Layer.Location.Orientation is
                  when Horizontal =>
                     Layer.Location.Left := Get_Double (Value);
                     if Layer.Location.Left > Layer.Location.Right then
                        Layer.Location.Right := Layer.Location.Left;
                     end if;
                  when Vertical =>
                     Layer.Location.Top := Get_Double (Value);
                     if Layer.Location.Top > Layer.Location.Bottom then
                        Layer.Location.Bottom := Layer.Location.Top;
                     end if;
               end case;
            when Property_To =>
               case Layer.Location.Orientation is
                  when Horizontal =>
                     Layer.Location.Right := Get_Double (Value);
                     if Layer.Location.Left > Layer.Location.Right then
                        Layer.Location.Left := Layer.Location.Right;
                     end if;
                  when Vertical =>
                     Layer.Location.Bottom := Get_Double (Value);
                     if Layer.Location.Top > Layer.Location.Bottom then
                        Layer.Location.Top := Layer.Location.Bottom;
                     end if;
               end case;
            when Property_Value =>
               case Layer.Location.Orientation is
                  when Horizontal =>
                     case Layer.Location.Alignment is
                        when Absolute =>
                           Layer.Location.Y_Position :=
                              Get_Double (Value);
                        when Relative =>
                           Layer.Location.Y_Value :=
                              Y_Axis (Get_Double (Value));
                     end case;
                  when Vertical =>
                     case Layer.Location.Alignment is
                        when Absolute =>
                           Layer.Location.X_Position :=
                              Get_Double (Value);
                        when Relative =>
                           Layer.Location.X_Value :=
                              X_Axis (Get_Double (Value));
                     end case;
               end case;
            when Property_Background_Color =>
               Layer.Background := Get_Value (Value);
            when Property_Color =>
               Layer.Color := Get_Value (Value);
            when Property_Stretch =>
               Layer.Stretch := Get_Double (Value);
               if Layer.Stretch < 0.0 then
                  Layer.Stretch := 0.0;
               end if;
            when Property_Text_Angle =>
               Layer.Text_Angle := Get_Double (Value);
               if Layer.Text_Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Text_Angle :=
                     GDouble'Remainder (Layer.Text_Angle, 2.0 * Pi);
               end if;
            when Property_Height =>
               Layer.Height := Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_X_Justify =>
               Layer.Justify_X :=
                  Gtk.Layered.Alignment_Property.Get_Enum (Value);
            when Property_Y_Justify =>
               Layer.Justify_Y :=
                  Gtk.Layered.Vertical_Alignment_Property.Get_Enum
                  (  Value
                  );
            when Property_Family =>
               Set_Family (Layer.Face, Get_String (Value));
            when Property_Slant =>
               Set_Slant
               (  Layer.Face,
                  Cairo.Font_Slant_Property.Get_Enum (Value)
               );
            when Property_Font_Size =>
               Set_Size
               (  Layer.Face,
                  GInt
                  (  GUInt'Max
                     (  GUInt'Min
                        (  Get_UInt (Value),
                           GUInt (GInt'Last)
                        ),
                        1
               )  )  );
            when Property_Weight =>
               Set_Weight
               (  Layer.Face,
                  Pango.Enums.Weight_Property.Get_Enum (Value)
               );
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
            when Property_Enlarged =>
               Layer.Enlarged := Get_Boolean (Value);
            when Property_Use_Superscript =>
               Layer.Superscript := Get_Boolean (Value);
            when Property_Orientation =>
               case Get_Boolean (Value) is
                  when False =>
                     case Layer.Location.Orientation is
                        when Horizontal =>
                           null;
                        when Vertical =>
                           case Layer.Location.Alignment is
                              when Absolute =>
                                 Layer.Location :=
                                    (  Horizontal,
                                       Absolute,
                                       Layer.Location.Top,
                                       Layer.Location.Bottom,
                                       Layer.Location.Y_Position
                                    );
                              when Relative =>
                                 Layer.Location :=
                                    (  Horizontal,
                                       Relative,
                                       Layer.Location.Top,
                                       Layer.Location.Bottom,
                                       Layer.Location.Y_Value
                                    );
                           end case;
                     end case;
                  when True =>
                     case Layer.Location.Orientation is
                        when Horizontal =>
                           case Layer.Location.Alignment is
                              when Absolute =>
                                 Layer.Location :=
                                    (  Vertical,
                                       Absolute,
                                       Layer.Location.Left,
                                       Layer.Location.Right,
                                       Layer.Location.X_Position
                                    );
                              when Relative =>
                                 Layer.Location :=
                                    (  Vertical,
                                       Relative,
                                       Layer.Location.Left,
                                       Layer.Location.Right,
                                       Layer.Location.X_Value
                                    );
                           end case;
                        when Vertical =>
                           null;
                     end case;
               end case;
            when Property_Alignment =>
               case Get_Boolean (Value) is
                  when False =>
                     case Layer.Location.Alignment is
                        when Absolute =>
                           null;
                        when Relative =>
                           case Layer.Location.Orientation is
                              when Horizontal =>
                                 Layer.Location :=
                                    (  Horizontal,
                                       Absolute,
                                       Layer.Location.Left,
                                       Layer.Location.Right,
                                       GDouble (Layer.Location.Y_Value)
                                    );
                              when Vertical =>
                                 Layer.Location :=
                                    (  Vertical,
                                       Absolute,
                                       Layer.Location.Top,
                                       Layer.Location.Bottom,
                                       GDouble (Layer.Location.X_Value)
                                    );
                           end case;
                     end case;
                  when True =>
                     case Layer.Location.Alignment is
                        when Absolute =>
                           case Layer.Location.Orientation is
                              when Horizontal =>
                                 Layer.Location :=
                                    (  Horizontal,
                                       Relative,
                                       Layer.Location.Left,
                                       Layer.Location.Right,
                                       Y_Axis
                                       (  Layer.Location.Y_Position
                                    )  );
                              when Vertical =>
                                 Layer.Location :=
                                    (  Vertical,
                                       Relative,
                                       Layer.Location.Top,
                                       Layer.Location.Bottom,
                                       X_Axis
                                       (  Layer.Location.X_Position
                                    )  );
                           end case;
                        when Relative =>
                           null;
                     end case;
               end case;
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Renderer
             (  Layer    : in out Graph_Paper_Annotation_Layer;
                Renderer : Renderer_Function
             )  is
   begin
      Layer.Renderer := Renderer;
   end Set_Renderer;

   procedure Set_Scaled
             (  Layer  : in out Graph_Paper_Annotation_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Suffix
             (  Layer  : in out Graph_Paper_Annotation_Layer;
                Suffix : UTF8_String
             )  is
   begin
      if Layer.Suffix = null then
         Layer.Suffix := new Annotation_Text'
                             (  Suffix'Length,
                                Suffix'Length,
                                Suffix
                             );
      elsif Layer.Suffix.Size < Suffix'Length then
         Free (Layer.Suffix);
         Layer.Suffix := new Annotation_Text'
                             (  Suffix'Length,
                                Suffix'Length,
                                Suffix
                             );
      else
         Layer.Suffix.Length := Suffix'Length;
         Layer.Suffix.Buffer (1..Suffix'Length) := Suffix;
      end if;
      Layer.Updated := True;
   end Set_Suffix;

   procedure Set_Text
             (  Layer    : in out Graph_Paper_Annotation_Layer;
                Position : Positive;
                Text     : UTF8_String
             )  is
   begin
      if Layer.Texts = null then
         if Position = 1 then
            Layer.Texts :=
               new Annotation_List'
                   (  1..1 => new Annotation_Text'
                                  (  Text'Length,
                                     Text'Length,
                                     Text
                   )              );
            Layer.Updated := True;
            return;
         end if;
      elsif Position > Layer.Texts'Last then
         if Position = Layer.Texts'Last + 1 then
            declare
               Old_Texts : Annotation_List_Ptr := Layer.Texts;
            begin
               Layer.Texts := new Annotation_List (1..Position);
               Layer.Texts (Old_Texts'Range) := Old_Texts.all;
               Layer.Texts (Position) :=
                  new Annotation_Text'
                      (  Text'Length,
                         Text'Length,
                         Text
                      );
               Free (Old_Texts);
               Layer.Updated := True;
               return;
            end;
         end if;
      else
         if (  Layer.Texts (Position) /= null
            and then
               Layer.Texts (Position).Size >= Text'Length
            )
         then
            Layer.Texts (Position).Buffer (1..Text'Length) := Text;
            Layer.Texts (Position).Length := Text'Length;
         else
            Free (Layer.Texts (Position));
            Layer.Texts (Position) :=
               new Annotation_Text'
                   (  Text'Length,
                      Text'Length,
                      Text
                   );
         end if;
         Layer.Updated := True;
         return;
      end if;
      raise Constraint_Error with "No such text";
   end Set_Text;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Graph_Paper_Annotation_Layer
             )  is
   begin
      Store (Stream, Layer.Face);
      Store (Stream, Layer.Height);
      Store (Stream, Layer.Stretch);
      case Layer.Location.Orientation is
         when Horizontal =>
            Store (Stream, Layer.Location.Left);
            Store (Stream, Layer.Location.Right);
            case Layer.Location.Alignment is
               when Absolute =>
                  Store (Stream, Layer.Location.Y_Position);
               when Relative =>
                  Store (Stream, GDouble (Layer.Location.Y_Value));
            end case;
         when Vertical =>
            Store (Stream, Layer.Location.Top);
            Store (Stream, Layer.Location.Bottom);
            case Layer.Location.Alignment is
               when Absolute =>
                  Store (Stream, Layer.Location.X_Position);
               when Relative =>
                  Store (Stream, GDouble (Layer.Location.X_Value));
            end case;
      end case;
      Store (Stream, Layer.Text_Angle);
      Store (Stream, Layer.Color);
      Store (Stream, Layer.Justify_X);
      Store (Stream, Layer.Justify_Y);
      Store (Stream, Layer.Background);
      Store (Stream, Layer.Border);
      Store (Stream, Layer.Opacity);
      Store
      (  Stream,
         Layer.Scaled,
         Layer.Enlarged,
         Layer.Location.Orientation = Vertical,
         Layer.Location.Alignment   = Relative,
         Layer.Superscript
      );
   end Store;

end Gtk.Layered.Graph_Paper_Annotation;
