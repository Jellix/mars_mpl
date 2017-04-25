--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform                        Luebeck            --
--  Implementation                                 Winter, 2011       --
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
with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with Cairo.Surface;               use Cairo.Surface;
with GLib.Messages;               use GLib.Messages;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Line_Cap_Property;
with Gtk.Layered.Interpolation_Mode_Property;
with Gtk.Layered.Waveform_Drawing_Method_Property;

package body Gtk.Layered.Waveform is
   ---------------------------------------------------------------------
   Dump_New_Line : constant Boolean := False;
   Dump_X_Value  : constant Boolean := True;
   Dump_Y_Value  : constant Boolean := True;

   procedure Dump
             (  Layer : Waveform_Layer;
                X     : Horizontal_Offset;
                V     : Y_Axis
             )  is
   begin
      if Dump_X_Value then
         Trace (Layer'Address, Horizontal_Offset'Image (X) & " :");
      end if;
      Trace (Layer'Address, Edit.Image (GDouble (V)));
      if Dump_Y_Value then
         Trace
         (  Layer'Address,
            " =" & GInt'Image (GInt (Layer.To_Y (V)))
         );
      end if;
   exception
      when others =>
         null;
   end Dump;

   procedure Dump
             (  Layer : Waveform_Layer;
                Data  : Points_Array;
                Index : Integer
             )  is
   begin
      if Dump_New_Line then
         Trace_Line (Layer'Address, Integer'Image (Index) & '=');
      else
         Trace (Layer'Address, Integer'Image (Index) & '=');
      end if;
      Dump (Layer, Data (Index).X, Data (Index).Y);
   end Dump;

   procedure Dump
             (  Layer  : Waveform_Layer;
                Prefix : String;
                Data   : Points_Array
             )  is
   begin
      Trace_Line (Layer'Address, Prefix);
      for Index in Data'Range loop
         Dump (Layer, Data, Index);
      end loop;
   end Dump;
   ---------------------------------------------------------------------
   use type Ada.Calendar.Time;

   Epoch          : Time;
   Calendar_Epoch : Ada.Calendar.Time;

   type Point is record
      T : X_Axis;
      V : Y_Axis;
   end record;

   type Layer_Property is
        (  Property_Scaled,
           Property_Widened,
           Property_Opacity,
           Property_Extrapolate_Left,
           Property_Extrapolate_Right,
           Property_Interpolation_Mode,
           Property_Preferred_Method,
           Property_X1,
           Property_X2,
           Property_Y1,
           Property_Y2,
           Property_T1,
           Property_T2,
           Property_V1,
           Property_V2,
           Property_Visible,
           Property_Width,
           Property_Line_Cap,
           Property_Color
        );

   package Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Waveform_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Points_Array, Points_Array_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Waveform_Layer, Waveform_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Waveform_Data_Scanner'Class,
             Waveform_Data_Scanner_Ptr
          );
   function Interpolate (T : X_Axis; L, R : Point) return Y_Axis;
      pragma Inline (Interpolate);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Waveform." & Name;
   end Where;

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Waveform_Layer is
      Ptr : Waveform_Ptr := new Waveform_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Waveform
             (  Under     : not null access Layer_Location'Class;
                Box       : Cairo_Box;
                Width     : GDouble            := 1.0;
                Color     : Gdk_Color          := RGB (1.0, 0.0, 0.0);
                Line_Cap  : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
                Sweeper   : access Gtk_Adjustment_Record'Class := null;
                Amplifier : access Gtk_Adjustment_Record'Class := null;
                Mode      : Interpolation_Mode := Linear;
                Left      : Boolean            := False;
                Right     : Boolean            := False;
                Opacity   : Fill_Opacity       := 1.0;
                Scaled    : Boolean            := False;
                Widened   : Boolean            := False
             )  is
      Ptr   : Waveform_Ptr := new Waveform_Layer;
      Layer : Waveform_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Box     => Box,
         Mode    => Mode,
         Left    => Left,
         Right   => Right,
         Opacity => Opacity,
         Line    => (  Color    => Color,
                       Width    => Width,
                       Line_Cap => Line_Cap
      )             );
      Ptr.Set_Amplifier (Amplifier);
      Ptr.Set_Sweeper (Sweeper);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Waveform;

   function Add_Waveform
            (  Under     : not null access Layer_Location'Class;
               Box       : Cairo_Box;
               Width     : GDouble            := 1.0;
               Color     : Gdk_Color          := RGB (1.0, 0.0, 0.0);
               Line_Cap  : Cairo_Line_Cap     := CAIRO_LINE_CAP_BUTT;
               Sweeper   : access Gtk_Adjustment_Record'Class := null;
               Amplifier : access Gtk_Adjustment_Record'Class := null;
               Mode      : Interpolation_Mode := Linear;
               Left      : Boolean            := False;
               Right     : Boolean            := False;
               Opacity   : Fill_Opacity       := 1.0;
               Scaled    : Boolean            := False;
               Widened   : Boolean            := False
            )  return not null access Waveform_Layer is
      Ptr   : Waveform_Ptr := new Waveform_Layer;
      Layer : Waveform_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
      (  Layer   => Layer,
         Box     => Box,
         Mode    => Mode,
         Left    => Left,
         Right   => Right,
         Opacity => Opacity,
         Line    => (  Color    => Color,
                       Width    => Width,
                       Line_Cap => Line_Cap
      )             );
      Ptr.Set_Amplifier (Amplifier);
      Ptr.Set_Sweeper (Sweeper);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Waveform;

   procedure Amplify
             (  Layer : in out Waveform_Layer;
                Lower : Y_Axis;
                Upper : Y_Axis
             )  is
   begin
      if Lower >= Upper then
         raise Constraint_Error with
            "Lower value is greater or equal to the upper value";
      end if;
      if Layer.V1 /= Upper or else Layer.V2 /= Lower then
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            if Layer.V1 /= Upper then
               Trace_Line
               (  Layer'Address,
                  (  "Amplify V1 ="
                  &  Edit.Image (GDouble (Layer.V1))
                  &  " /="
                  &  Edit.Image (GDouble (Upper))
                  &  " = upper"
               )  );
            end if;
            if Layer.V2 /= Lower then
               Trace_Line
               (  Layer'Address,
                  (  "Amplify V2 ="
                  &  Edit.Image (GDouble (Layer.V2))
                  &  " /="
                  &  Edit.Image (GDouble (Lower))
                  &  " = lower"
               )  );
            end if;
         end if; -------------------------------------------------------
         Layer.V1 := Upper;
         Layer.V2 := Lower;
         Layer.Updated := True;
         Layer.Valid   := False;
      end if;
   end Amplify;

   procedure Changed
             (  Layer : in out Waveform_Layer;
                From  : X_Axis;
                To    : X_Axis
             )  is
   begin
      if To >= Layer.T1 and then From <= Layer.T2 then
         -- The change occurred within visible range
         case Layer.Method is
            when Resample_New_And_Stroke | Resample_All_And_Stroke =>
               if From <= Layer.Line_Data.Last_T then
                  Layer.Valid := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Changed contents");
                  end if; ----------------------------------------------
               end if;
         end case;
         Layer.Sampled := False;
         Layer.Updated := True;
      end if;
   end Changed;

   procedure Changed_Amplifier
             (  Adjustment : access GObject_Record'Class;
                Layer      : Waveform_Ptr
             )  is
   begin
      Layer.Query_Amplifier;
      if not Layer.Widget.Drawing and then Layer.Updated then
         Queue_Draw (Layer.Widget); -- Signal draw to the widget
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_Amplifier")
         )  );
   end Changed_Amplifier;

   procedure Changed_Sweeper
             (  Adjustment : access GObject_Record'Class;
                Layer      : Waveform_Ptr
             )  is
   begin
      Layer.Query_Sweeper;
      if (  not Layer.Widget.Drawing
         and then
            Layer.Updated
         and then
            (  Adjustment.all not in Waveform_Sweeper'Class
            or else
               not Waveform_Sweeper'Class (Adjustment.all).Is_Active
         )  )
      then
         Queue_Draw (Layer.Widget); -- Signal draw to the widget
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_Sweeper")
         )  );
   end Changed_Sweeper;

   procedure Draw
             (  Layer   : in out Waveform_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
   begin
      Layer.Query_Amplifier;
      if Layer.Visible then
         case Layer.Method is
            when Resample_New_And_Stroke | Resample_All_And_Stroke =>
               Layer.Draw_Lines (Context, Layer.Line_Data);
         end case;
      end if;
      Layer.Updated := False;
   end Draw;

   procedure Draw_Lines
             (  Layer   : in out Waveform_Layer;
                Context : Cairo_Context;
                Data    : in out Line_Method_Data
             )  is separate;

   procedure Finalize (Layer : in out Waveform_Layer) is
   begin
      Set_Source (Layer);
      Free (Layer.Line_Data.Points);
      if Layer.Amplifier_Adjustment /= null then
         Unref (Layer.Amplifier_Adjustment);
      end if;
      if Layer.Sweeper_Adjustment /= null then
         Unref (Layer.Sweeper_Adjustment);
      end if;
      Finalize (Abstract_Layer (Layer));
   end Finalize;

   procedure Get
             (  Source : in out Waveform_Data_Scanner'Class;
                T      : X_Axis;
                Mode   : Interpolation_Mode;
                V      : out Y_Axis
             )  is
      T1 : X_Axis := X_Axis'Succ (T);
      T2 : X_Axis;
      V1 : Y_Axis;
      V2 : Y_Axis;
   begin
      Source.Backward (T1, V1);
      if T1 = T then
         V := V1;
      else
         T2 := T1; -- Check if the value is well-defined
         Source.Forward (T2, V2);
         case Mode is
            when Left =>
               V := V1;
            when Linear =>
               V :=
                  (  (  Y_Axis (T - T1) * V2
                     +  Y_Axis (T2 - T) * V1
                     )
                  /  Y_Axis (T2 - T1)
                  );
         end case;
      end if;
   end Get;

   procedure Get
             (  Source : in out Waveform_Data_Scanner'Class;
                T      : X_Axis;
                Mode   : Interpolation_Mode;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
      T1 : X_Axis := X_Axis'Succ (T);
      T2 : X_Axis;
      V1 : Y_Axis;
      V2 : Y_Axis;
   begin
      Source.Backward (T1, V1, Got_It);
      if not Got_It then
         return;
      end if;
      if T1 = T then
         V := V1;
      else
         T2 := T1; -- Check if the value is well-defined
         Source.Forward (T2, V2, Got_It);
         if not Got_It then
            return;
         end if;
         case Mode is
            when Left =>
               V := V1;
            when Linear =>
               V :=
                  (  (  Y_Axis (T - T1) * V2
                     +  Y_Axis (T2 - T) * V1
                     )
                  /  Y_Axis (T2 - T1)
                  );
         end case;
      end if;
   end Get;

   function Get (Layer : Waveform_Layer; X : GDouble) return Y_Axis is
      Got_It : Boolean;
      V      : Y_Axis;
   begin
      Layer.Get (X, V, Got_It);
      if Got_It then
         return V;
      else
         raise End_Error;
      end if;
   end Get;

   procedure Get
             (  Layer  : Waveform_Layer;
                X      : GDouble;
                Y      : out Y_Axis;
                Got_It : out Boolean
             )  is
   begin
      if Layer.Data = null then
         Got_It := False;
      else
         declare
            T : constant X_Axis := Get_T (Layer, X);
         begin
            Layer.Data.Get (T, Layer.Mode, Y, Got_It);
            if (  not Got_It
               and then
                  (  Layer.Extrapolate_Left
                  or else
                     Layer.Extrapolate_Right
               )  )
            then
               declare
                  T1, T2 : X_Axis;
                  V1, V2 : Y_Axis;
               begin
                  Layer.Data.First (T1, V1, Got_It);
                  Layer.Data.Last  (T2, V2, Got_It);
                  if Got_It then
                     if T <= T1 and then Layer.Extrapolate_Left then
                        Y := V1;
                        Got_It := True;
                     elsif T >= T2 and then Layer.Extrapolate_Right then
                        Y := V2;
                        Got_It := True;
                     end if;
                  end if;
               end;
            end if;
         end;
      end if;
   end Get;

   function Get_Amplifier (Layer : Waveform_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Amplifier_Adjustment;
   end Get_Amplifier;

   function Get_Box (Layer : Waveform_Layer) return Cairo_Box is
   begin
      return Layer.Box;
   end Get_Box;

   function Get_Epoch return Ada.Calendar.Time is
   begin
      return Calendar_Epoch;
   end Get_Epoch;

   function Get_Epoch return Time is
   begin
      return Epoch;
   end Get_Epoch;

   function Get_Interpolation_Mode (Layer : Waveform_Layer)
      return Interpolation_Mode is
   begin
      return Layer.Mode;
   end Get_Interpolation_Mode;

   function Get_Left_Extrapolation_Mode (Layer : Waveform_Layer)
      return Boolean is
   begin
      return Layer.Extrapolate_Left;
   end Get_Left_Extrapolation_Mode;

   function Get_Line (Layer : Waveform_Layer) return Line_Parameters is
   begin
      return Layer.Line;
   end Get_Line;

   function Get_Method (Layer : Waveform_Layer)
      return Waveform_Drawing_Method is
   begin
      return Layer.Method;
   end Get_Method;

   function Get_Opacity (Layer : Waveform_Layer) return Fill_Opacity is
   begin
      return Layer.Opacity;
   end Get_Opacity;

   procedure Get_Point
             (  Layer : Waveform_Layer;
                X     : GDouble;
                T     : out X_Axis;
                V     : out Y_Axis
             )  is
      Got_It : Boolean;
   begin
      Layer.Get_Point (X, T, V, Got_It);
      if not Got_It then
         raise End_Error;
      end if;
   end Get_Point;

   procedure Get_Point
             (  Layer  : Waveform_Layer;
                X      : GDouble;
                T      : out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
   begin
      if Layer.Data = null then
         Got_It := False;
      else
         T := Get_T (Layer, X);
         Layer.Data.Get (T, Layer.Mode, V, Got_It);
         if (  not Got_It
            and then
               (Layer.Extrapolate_Left or else Layer.Extrapolate_Right)
            )
         then
            declare
               T1, T2 : X_Axis;
               V1, V2 : Y_Axis;
            begin
               Layer.Data.First (T1, V1, Got_It);
               Layer.Data.Last  (T2, V2, Got_It);
               if Got_It then
                  if T <= T1 and then Layer.Extrapolate_Left then
                     V := V1;
                     Got_It := True;
                  elsif T >= T2 and then Layer.Extrapolate_Right then
                     V := V2;
                     Got_It := True;
                  end if;
               end if;
            end;
         end if;
      end if;
   end Get_Point;

   function Get_Preferred_Method (Layer : Waveform_Layer)
      return Waveform_Drawing_Method is
   begin
      return Layer.Preferred;
   end Get_Preferred_Method;

   function Get_Properties_Number
            (  Layer : Waveform_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Waveform_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Opacity =>
               return
                  Gnew_Double
                  (  Name    => "fill-opacity",
                     Nick    => "opacity",
                     Minimum => 0.0,
                     Maximum => 1.0,
                     Default => 1.0,
                     Blurb   => "The opacity of filling. 0.0 " &
                                "is transparent, 1.0 is opaque"
                  );
            when Property_X1 =>
               return
                  Gnew_Double
                  (  Name    => "x1",
                     Nick    => "x1",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the waveform's " &
                                "box left margin"
                  );
            when Property_X2 =>
               return
                  Gnew_Double
                  (  Name    => "x2",
                     Nick    => "x2",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The x-coordinate of the waveform's " &
                                "box right margin"
                  );
            when Property_Y1 =>
               return
                  Gnew_Double
                  (  Name    => "y1",
                     Nick    => "y1",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the waveform's " &
                                "box top margin"
                  );
            when Property_Y2 =>
               return
                  Gnew_Double
                  (  Name    => "y2",
                     Nick    => "y2",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The y-coordinate of the waveform's " &
                                "box bottom margin"
                  );
            when Property_T1 =>
               return
                  Gnew_Double
                  (  Name    => "t1",
                     Nick    => "t1",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The time at the waveform's box " &
                                "left margin in seconds since the epoch"
                  );
            when Property_T2 =>
               return
                  Gnew_Double
                  (  Name    => "t2",
                     Nick    => "t2",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The time at the waveform's box " &
                                "right margin in seconds since the " &
                                "epoch"
                  );
            when Property_V1 =>
               return
                  Gnew_Double
                  (  Name    => "v1",
                     Nick    => "v1",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The value at the waveform's box " &
                                "top margin"
                  );
            when Property_V2 =>
               return
                  Gnew_Double
                  (  Name    => "v2",
                     Nick    => "v2",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The value at the waveform's box " &
                                "bottom margin"
                  );
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The waveform color"
                  );
            when Property_Extrapolate_Left =>
               return
                  Gnew_Boolean
                  (  Name    => "extrapolate-left",
                     Nick    => "extrapolate-left",
                     Default => False,
                     Blurb   => "Extrapolation of waveform data " &
                                "to the left is allowed when is set " &
                                "to true"
                  );
            when Property_Extrapolate_Right =>
               return
                  Gnew_Boolean
                  (  Name    => "extrapolate-right",
                     Nick    => "extrapolate-right",
                     Default => False,
                     Blurb   => "Extrapolation of waveform data " &
                                "to the right is allowed when is set " &
                                "to true"
                  );
            when Property_Interpolation_Mode =>
               return
                  Gtk.Layered.Interpolation_Mode_Property.Gnew_Enum
                  (  Name    => "interpolation-mode",
                     Nick    => "interpolation",
                     Default => Linear,
                     Blurb   => "The interpolation mode used between " &
                                "points of the waveform"
                  );
            when Property_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                  (  Name    => "line-cap",
                     Nick    => "line cap",
                     Default => CAIRO_LINE_CAP_BUTT,
                     Blurb   => "The cap style of the waveform line"
                  );
            when Property_Preferred_Method =>
               return
                  Gtk.Layered.Waveform_Drawing_Method_Property.Gnew_Enum
                  (  Name    => "preferred-mode",
                     Nick    => "preferred mode",
                     Default => Resample_New_And_Stroke,
                     Blurb   => "The preferred method of drawing and " &
                                "sampling the waveform"
                  );
            when Property_Width =>
               return
                  Gnew_Double
                  (  Name    => "width",
                     Nick    => "width",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The waveform line width"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The waveform size is changed when " &
                                "the widget is resized"
                  );
            when Property_Visible =>
               return
                  Gnew_Boolean
                  (  Name    => "visible",
                     Nick    => "visible",
                     Default => True,
                     Blurb   => "The waveform is visible when this " &
                                "property is set to true"
                  );
            when Property_Widened =>
               return
                  Gnew_Boolean
                  (  Name    => "widened",
                     Nick    => "widened",
                     Default => False,
                     Blurb   => "The waveform line is thickened when " &
                                "the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Waveform_Layer;
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
               when Property_Opacity =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Opacity);
               when Property_X1 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Box.X1);
               when Property_X2 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Box.X2);
               when Property_Y1 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Box.Y1);
               when Property_Y2 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Box.Y2);
               when Property_T1 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, GDouble (Layer.T1));
               when Property_T2 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, GDouble (Layer.T2));
               when Property_V1 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, GDouble (Layer.V1));
               when Property_V2 =>
                  Init (Value, GType_Double);
                  Set_Double (Value, GDouble (Layer.V2));
               when Property_Width =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Line.Width);
               when Property_Color =>
                  Set_Value (Value, Layer.Line.Color);
               when Property_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                  (  Value,
                     Layer.Line.Line_Cap
                  );
               when Property_Extrapolate_Left =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Extrapolate_Left);
               when Property_Extrapolate_Right =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Extrapolate_Right);
               when Property_Interpolation_Mode =>
                  Gtk.Layered.Interpolation_Mode_Property.Set_Enum
                  (  Value,
                     Layer.Mode
                  );
               when Property_Preferred_Method =>
                  Gtk.Layered.Waveform_Drawing_Method_Property.Set_Enum
                  (  Value,
                     Layer.Preferred
                  );
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Visible =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Visible);
               when Property_Widened =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Widened);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Right_Extrapolation_Mode (Layer : Waveform_Layer)
      return Boolean is
   begin
      return Layer.Extrapolate_Right;
   end Get_Right_Extrapolation_Mode;

   function Get_Scaled (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Source
            (  Layer : Waveform_Layer
            )  return access Waveform_Data_Source'Class is
   begin
      if Layer.Data = null then
         return null;
      else
         return Layer.Data.Get_Source;
      end if;
   end Get_Source;

   function Get_Sweeper (Layer : Waveform_Layer)
      return Gtk_Adjustment is
   begin
      return Layer.Sweeper_Adjustment;
   end Get_Sweeper;

   function Get_T (Layer : Waveform_Layer; X : GDouble) return X_Axis is
      X1 : X_Axis := X_Axis (Layer.Box.X1);
      dX : X_Axis := X_Axis (Layer.Box.X2 - Layer.Box.X1 + 1.0);
   begin
      if Layer.Scaled then
         declare
            X_Size : constant X_Axis :=
                     X_Axis (Layer.Widget.Get_Allocated_Width);
         begin
            X1 := X1 * X_Size + X_Axis (Layer.Widget.Get_Center.X);
            dX := dX * X_Size;
         end;
      end if;
      return (X_Axis (X) - X1) * (Layer.T2 - Layer.T1) / dX + Layer.T1;
   end Get_T;

   function Get_T1 (Layer : Waveform_Layer) return X_Axis is
   begin
      return Layer.T1;
   end Get_T1;

   function Get_T2 (Layer : Waveform_Layer) return X_Axis is
   begin
      return Layer.T2;
   end Get_T2;

   function Get_V1 (Layer : Waveform_Layer) return Y_Axis is
   begin
      return Layer.V1;
   end Get_V1;

   function Get_V2 (Layer : Waveform_Layer) return Y_Axis is
   begin
      return Layer.V2;
   end Get_V2;

   function Get_V (Layer : Waveform_Layer; Y : GDouble) return Y_Axis is
      Y2 : Y_Axis := Y_Axis (Layer.Box.Y2);
      dY : Y_Axis := Y_Axis (Layer.Box.Y2 - Layer.Box.Y1);
   begin
      if Layer.Scaled then
         declare
            Y_Size : constant Y_Axis :=
                     Y_Axis (Layer.Widget.Get_Allocated_Height);
         begin
            Y2 := Y2 * Y_Size + Y_Axis (Layer.Widget.Get_Center.Y);
            dY := dY * Y_Size;
         end;
      end if;
      return Layer.V2 - (Y2 - Y_Axis (Y)) * (Layer.V2 - Layer.V1) / dY;
   end Get_V;

   function Get_Widened (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Get_X (Layer : Waveform_Layer; T : X_Axis) return GDouble is
      X1 : X_Axis := X_Axis (Layer.Box.X1);
      dX : X_Axis := X_Axis (Layer.Box.X2 - Layer.Box.X1 + 1.0);
   begin
      if Layer.Scaled then
         declare
            X_Size : constant X_Axis :=
                     X_Axis (Layer.Widget.Get_Allocated_Width);
         begin
            X1 := X1 * X_Size + X_Axis (Layer.Widget.Get_Center.X);
            dX := dX * X_Size;
         end;
      end if;
      return
         GDouble
         (  (T - Layer.T1) * dX / (Layer.T2 - Layer.T1)
         +  X1
         );
   end Get_X;

   function Get_Y (Layer : Waveform_Layer; V : Y_Axis) return GDouble is
      Y2 : Y_Axis := Y_Axis (Layer.Box.Y2);
      dY : Y_Axis := Y_Axis (Layer.Box.Y2 - Layer.Box.Y1 + 1.0);
   begin
      if Layer.Scaled then
         declare
            Y_Size : constant Y_Axis :=
                     Y_Axis (Layer.Widget.Get_Allocated_Height);
         begin
            Y2 := Y2 * Y_Size + Y_Axis (Layer.Widget.Get_Center.Y);
            dY := dY * Y_Size;
         end;
      end if;
      return
         GDouble
         (  Y2
         -  (Layer.V2 - V) * dY / (Layer.V2 - Layer.V1)
         );
   end Get_Y;

   function Interpolate (T : X_Axis; L, R : Point) return Y_Axis is
   begin
      if L.V = R.V then
         return L.V;
      else
         return
         (  (  R.V * Y_Axis (T - L.T) + L.V * Y_Axis (R.T - T)
            )
         /  Y_Axis (R.T - L.T)
         );
      end if;
   end Interpolate;

   function Is_Updated (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   function Is_Visible (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Visible;
   end Is_Visible;

   procedure Move
             (  Layer  : in out Waveform_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.Box.X1  := Layer.Box.X1 + Offset.X;
      Layer.Box.X2  := Layer.Box.X2 + Offset.X;
      Layer.Box.Y1  := Layer.Box.Y1 + Offset.Y;
      Layer.Box.Y2  := Layer.Box.Y2 + Offset.Y;
      Layer.Updated := True;
   end Move;

   procedure Prepare
             (  Layer   : in out Waveform_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      X1, X2 : GDouble;
      Y1, Y2 : GDouble;
   begin
      if Layer.Sweeper_Object /= null then
         Layer.Sweeper_Object.Set_Current_Time
         (  Layer.Widget.Get_Drawing_Time,
            True
         );
         Layer.Query_Sweeper;
      end if;
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
      if X1 >= X2 or else Layer.T1 >= Layer.T2 then
         Layer.Sampled := True;
         Layer.Valid   := False;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace_Line (Layer'Address, "Null width or span");
         end if; -------------------------------------------------------
         return;
      end if;
      if not Layer.Sampled then
         case Layer.Method is
            when Resample_New_And_Stroke | Resample_All_And_Stroke =>
               Layer.Sample_Lines
               (  Layer.Line_Data,
                  Horizontal_Offset (X1),
                  Horizontal_Offset (X2)
               );
         end case;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Prepare")
         )  );
   end Prepare;

   procedure Query_Amplifier (Layer : in out Waveform_Layer) is
   begin
      if Layer.Amplifier_Adjustment /= null then
         declare
            Value : constant Y_Axis :=
                    Y_Axis (Get_Value (Layer.Amplifier_Adjustment));
            Size  : constant Y_Axis :=
                    Y_Axis (Get_Page_Size (Layer.Amplifier_Adjustment));
         begin
            if Size <= 0.0 then
               Layer.Amplify (Value, Value + 1.0);
            else
               Layer.Amplify (Value, Value + Size);
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Query_Amplifier")
         )  );
   end Query_Amplifier;

   procedure Query_Sweeper (Layer : in out Waveform_Layer) is
   begin
      if Layer.Sweeper_Adjustment /= null then
         declare
            Value : constant X_Axis :=
                    X_Axis (Layer.Sweeper_Adjustment.Get_Value);
            Size  : constant X_Axis :=
                    X_Axis (Layer.Sweeper_Adjustment.Get_Page_Size);
         begin
            if Size <= 0.0 then
               Layer.Sweep (Value, Value + 1.0);
            else
               Layer.Sweep (Value, Value + Size);
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Query_Sweeper")
         )  );
   end Query_Sweeper;

   procedure Resized
             (  Layer : in out Waveform_Layer;
                Area  : Gdk_Rectangle
             )  is
   begin
      Layer.Sampled := False;
      Layer.Valid   := False;
      if 0 /= (Tracing_Mode and Trace_Waveform) then -------------------
         Trace_Line (Layer'Address, "Resized");
      end if; ----------------------------------------------------------
   end Resized;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Waveform_Layer
             )  is
      Box       : Cairo_Box;
      Line      : Line_Parameters;
      Mode      : Interpolation_Mode;
      Preferred : Waveform_Drawing_Method;
      Opacity   : GDouble;
      Amplifier : Boolean;
      Sweeper   : Boolean;
      Left      : Boolean;
      Right     : Boolean;
   begin
      Restore (Stream, Box.X1);
      Restore (Stream, Box.X2);
      Restore (Stream, Box.Y1);
      Restore (Stream, Box.Y2);
      Restore (Stream, Line);
      Restore (Stream, Mode);
      Restore (Stream, Preferred);
      Restore (Stream, Opacity);
      Restore
      (  Stream,
         Layer.Scaled,
         Layer.Widened,
         Amplifier,
         Sweeper,
         Left,
         Right
      );
      Opacity := GDouble'Min (1.0, GDouble'Max (0.0, Opacity));
      Set
      (  Layer   => Layer,
         Box     => Box,
         Line    => Line,
         Mode    => Mode,
         Left    => Left,
         Right   => Right,
         Opacity => Opacity
      );
      Set_Preferred_Method (Layer, Preferred);
      if Amplifier then
         declare
            Adjustment : Gtk_Adjustment;
         begin
            Restore (Stream, Adjustment);
            Layer.Set_Amplifier (Adjustment);
         end;
      else
         declare
            V1, V2 : GDouble;
         begin
            Restore (Stream, V1);
            Restore (Stream, V2);
            Amplify (Layer, Y_Axis (V1), Y_Axis (V2));
         end;
      end if;
      if Sweeper then
         declare
            Adjustment : Gtk_Adjustment;
         begin
            Restore (Stream, Adjustment);
            Layer.Set_Sweeper (Adjustment);
         end;
      else
         declare
            T1, T2 : GDouble;
         begin
            Restore (Stream, T1);
            Restore (Stream, T2);
            Sweep (Layer, X_Axis (T1), X_Axis (T2));
         end;
      end if;
      Layer.Sampled := False;
      Layer.Valid   := False;
   end Restore;

   procedure Sample_Lines
             (  Layer  : in out Waveform_Layer;
                Data   : in out Line_Method_Data;
                X1, X2 : Horizontal_Offset
             )  is separate;
-- procedure Sample_Surface
--           (  Layer : in out Waveform_Layer;
--              Data  : in out Surface_Method_Data
--           )  is separate;

   procedure Scale
             (  Layer  : in out Waveform_Layer;
                Factor : GDouble
             )  is
      Center_X    : constant GDouble :=
                       (Layer.Box.X1 + Layer.Box.X2) * 0.5;
      Center_Y    : constant GDouble :=
                       (Layer.Box.Y1 + Layer.Box.Y2) * 0.5;
      Half_Width  : constant GDouble :=
                       (Layer.Box.X2 - Layer.Box.X1 + 1.0) * 0.5;
      Half_Height : constant GDouble :=
                       (Layer.Box.Y2 - Layer.Box.Y1 + 1.0) * 0.5;
   begin
      Set
      (  Layer   => Layer,
         Line    => Layer.Line,
         Mode    => Layer.Mode,
         Left    => Layer.Extrapolate_Left,
         Right   => Layer.Extrapolate_Right,
         Opacity => Layer.Opacity,
         Box     => (  X1 => Center_X - Half_Width,
                       X2 => Center_X + Half_Width,
                       Y1 => Center_Y - Half_Height,
                       Y2 => Center_Y + Half_Height
      )             );
   end Scale;

   procedure Set
             (  Layer   : in out Waveform_Layer;
                Box     : Cairo_Box;
                Line    : Line_Parameters;
                Mode    : Interpolation_Mode;
                Left    : Boolean;
                Right   : Boolean;
                Opacity : Fill_Opacity
             )  is
   begin
      if Line.Width < 0.0 then
         raise Constraint_Error with "Negative line width";
      elsif Box.X1 > Box.X2 then
         raise Constraint_Error with "Negative box width";
      elsif Box.Y1 > Box.Y2 then
         raise Constraint_Error with "Negative box height";
      end if;
      if Layer.Box /= Box then
         Layer.Box   := Box;
         Layer.Valid := False;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace_Line (Layer'Address, "Set box");
         end if; -------------------------------------------------------
      end if;
      Layer.Line              := Line;
      Layer.Mode              := Mode;
      Layer.Extrapolate_Left  := Left;
      Layer.Extrapolate_Right := Right;
      Layer.Updated           := True;
      Layer.Set_Opacity (Opacity);
   end Set;

   procedure Set_Amplifier
             (  Layer     : in out Waveform_Layer;
                Amplifier : access Gtk_Adjustment_Record'Class := null
             )  is
   begin
      if Amplifier /= Layer.Amplifier_Adjustment then
         if Layer.Amplifier_Adjustment /= null then
            Set (Layer.Handlers (1));
            Set (Layer.Handlers (2));
            Unref (Layer.Amplifier_Adjustment);
            Layer.Amplifier_Adjustment := null;
         end if;
         if Amplifier /= null then
            Ref (Amplifier);
            Layer.Amplifier_Adjustment :=
               Amplifier.all'Unchecked_Access;
            if Amplifier.all in Waveform_Amplifier'Class then
               Layer.Amplifier_Object :=
                  Waveform_Amplifier'Class
                  (  Amplifier.all
                  ) 'Unchecked_Access;
            end if;
            Set
            (  Layer.Handlers (1),
               Handlers.Connect
               (  Amplifier,
                  "changed",
                  Handlers.To_Marshaller (Changed_Amplifier'Access),
                  Layer'Unchecked_Access
            )  );
            Set
            (  Layer.Handlers (2),
               Handlers.Connect
               (  Amplifier,
                  "value_changed",
                  Handlers.To_Marshaller (Changed_Amplifier'Access),
                  Layer'Unchecked_Access
            )  );
         end if;
      end if;
   end Set_Amplifier;

   procedure Set_Color
             (  Layer : in out Waveform_Layer;
                Color : Gdk_Color
             )  is
   begin
      Layer.Line.Color := Color;
      Layer.Updated := True;
   end Set_Color;

   procedure Set_Extrapolation_Mode
             (  Layer : in out Waveform_Layer;
                Left  : Boolean;
                Right : Boolean
             )  is
   begin
      if (  Layer.Extrapolate_Left /= Left
         or else
            Layer.Extrapolate_Right /= Right
         )
      then
         Layer.Extrapolate_Left  := Left;
         Layer.Extrapolate_Right := Right;
         Layer.Sampled := False;
         Layer.Updated := True;
         Layer.Valid   := False;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace_Line (Layer'Address, "Set_Extrapolation_Mode");
         end if; -------------------------------------------------------
      end if;
   end Set_Extrapolation_Mode;

   procedure Set_Interpolation_Mode
             (  Layer : in out Waveform_Layer;
                Mode  : Interpolation_Mode
             )  is
   begin
      if Layer.Mode /= Mode then
         Layer.Mode    := Mode;
         Layer.Sampled := False;
         Layer.Updated := True;
         Layer.Valid   := False;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace_Line (Layer'Address, "Set_Interpolation_Mode");
         end if; -------------------------------------------------------
      end if;
   end Set_Interpolation_Mode;

   procedure Set_Preferred_Method
             (  Layer  : in out Waveform_Layer;
                Method : Waveform_Drawing_Method
             )  is
   begin
      if Layer.Preferred /= Method then
         Layer.Preferred := Method;
         if Layer.Method /= Method then
            case Method is
               when Resample_New_And_Stroke | Resample_All_And_Stroke =>
                  Layer.Method  := Method;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Preferred_Method");
                  end if; ----------------------------------------------
            end case;
         end if;
      end if;
   end Set_Preferred_Method;

   procedure Set_Opacity
             (  Layer   : in out Waveform_Layer;
                Opacity : Fill_Opacity
             )  is
   begin
      if Opacity /= Layer.Opacity then
         Layer.Opacity := Opacity;
         Layer.Updated := True;
         Layer.Sampled := False;
         Layer.Valid   := False;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace_Line (Layer'Address, "Set_Opacity");
         end if; -------------------------------------------------------
      end if;
   end Set_Opacity;

   procedure Set_Property_Value
             (  Layer    : in out Waveform_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Opacity =>
               Layer.Set_Opacity
               (  GDouble
                  (  GDouble'Min
                     (  1.0,
                        GDouble'Max (0.0, Get_Double (Value))
               )  )  );
            when Property_T1 =>
               declare
                  New_Value : constant X_Axis :=
                              X_Axis (Get_Double (Value));
               begin
                  if New_Value >= Layer.T2 then
                     Layer.T2 := New_Value + 1.0;
                  end if;
                  Layer.T1 := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property T1");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_T2 =>
               declare
                  New_Value : constant X_Axis :=
                              X_Axis (Get_Double (Value));
               begin
                  if Layer.T1 >= New_Value then
                     Layer.T1 := New_Value - 1.0;
                  end if;
                  Layer.T2 := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property T2");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_V1 =>
               declare
                  New_Value : constant Y_Axis :=
                              Y_Axis (Get_Double (Value));
               begin
                  if New_Value >= Layer.V2 then
                     Layer.V2 := New_Value + 1.0;
                  end if;
                  Layer.V1 := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property V1");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_V2 =>
               declare
                  New_Value : constant Y_Axis :=
                              Y_Axis (Get_Double (Value));
               begin
                  if Layer.V1 >= New_Value then
                     Layer.V1 := New_Value - 1.0;
                  end if;
                  Layer.V2 := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property V2");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_X1 =>
               declare
                  New_Value : constant GDouble := Get_Double (Value);
               begin
                  if New_Value >= Layer.Box.X2 then
                     Layer.Box.X2 := New_Value + 1.0;
                  end if;
                  Layer.Box.X1  := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property X1");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_X2 =>
               declare
                  New_Value : constant GDouble := Get_Double (Value);
               begin
                  if Layer.Box.X1 > New_Value then
                     Layer.Box.X1 := New_Value - 1.0;
                  end if;
                  Layer.Box.X2  := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property X2");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_Y1 =>
               declare
                  New_Value : constant GDouble := Get_Double (Value);
               begin
                  if New_Value >= Layer.Box.Y2 then
                     Layer.Box.Y2 := New_Value + 1.0;
                  end if;
                  Layer.Box.Y1  := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property Y1");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_Y2 =>
               declare
                  New_Value : constant GDouble := Get_Double (Value);
               begin
                  if Layer.Box.Y1 >= New_Value then
                     Layer.Box.Y1 := New_Value - 1.0;
                  end if;
                  Layer.Box.Y2  := New_Value;
                  Layer.Sampled := False;
                  Layer.Valid   := False;
                  if 0 /= (Tracing_Mode and Trace_Waveform) then -------
                     Trace_Line (Layer'Address, "Set_Property Y2");
                  end if; ----------------------------------------------
               end;
               Layer.Updated := True;
            when Property_Width =>
               Layer.Line.Width := Get_Double (Value);
               if Layer.Line.Width < 0.0 then
                  Layer.Line.Width := 0.0;
               end if;
               Layer.Updated := True;
            when Property_Color =>
               Layer.Line.Color := Get_Value (Value);
               Layer.Updated    := True;
            when Property_Extrapolate_Left =>
               Set_Extrapolation_Mode
               (  Layer => Layer,
                  Left  => Get_Boolean (Value),
                  Right => Layer.Extrapolate_Right
               );
            when Property_Extrapolate_Right =>
               Set_Extrapolation_Mode
               (  Layer => Layer,
                  Left  => Layer.Extrapolate_Left,
                  Right => Get_Boolean (Value)
               );
            when Property_Interpolation_Mode =>
               Set_Interpolation_Mode
               (  Layer,
                  Gtk.Layered.Interpolation_Mode_Property.
                  Get_Enum (Value)
               );
            when Property_Preferred_Method =>
               Set_Preferred_Method
               (  Layer,
                  Gtk.Layered.Waveform_Drawing_Method_Property.
                  Get_Enum (Value)
               );
            when Property_Line_Cap =>
               Layer.Line.Line_Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
               Layer.Updated := True;
            when Property_Scaled =>
               Layer.Scaled  := Get_Boolean (Value);
               Layer.Updated := True;
            when Property_Visible =>
               Layer.Visible := Get_Boolean (Value);
               Layer.Updated := True;
            when Property_Widened =>
               Layer.Scaled  := Get_Boolean (Value);
               Layer.Updated := True;
         end case;
      end if;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Waveform_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Source (Layer : in out Waveform_Layer) is
   begin
      if Layer.Data /= null then
         Layer.Data.Get_Source.Disconnected (Layer);
         Free (Layer.Data);
         Layer.Sampled := False;
         Layer.Updated := True;
         Layer.Valid   := False;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace_Line (Layer'Address, "Set_Source");
         end if; -------------------------------------------------------
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Source")
         )  );
   end Set_Source;

   procedure Set_Source
             (  Layer  : in out Waveform_Layer;
                Source : in out Waveform_Data_Source'Class
             )  is
   begin
      Set_Source (Layer);
      Layer.Data :=
         new Waveform_Data_Scanner'Class'
             (  Create (Source'Unchecked_Access)
             );
      Layer.Data.Get_Source.Connected (Layer);
      Layer.Sampled := False;
      Layer.Updated := True;
      Layer.Valid   := False;
      if 0 /= (Tracing_Mode and Trace_Waveform) then -------------------
         Trace_Line (Layer'Address, "Set_Source");
      end if; ----------------------------------------------------------
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Source")
         )  );
   end Set_Source;

   procedure Set_Sweeper
             (  Layer   : in out Waveform_Layer;
                Sweeper : access Gtk_Adjustment_Record'Class := null
             )  is
   begin
      if Sweeper /= Layer.Sweeper_Adjustment then
         if Layer.Sweeper_Adjustment /= null then
            Set (Layer.Handlers (3));
            Set (Layer.Handlers (4));
            Unref (Layer.Sweeper_Adjustment);
            Layer.Sweeper_Adjustment := null;
         end if;
         if Sweeper /= null then
            Ref (Sweeper);
            Layer.Sweeper_Adjustment := Sweeper.all'Unchecked_Access;
            if Sweeper.all in Waveform_Sweeper'Class then
               Layer.Sweeper_Object :=
               Waveform_Sweeper'Class (Sweeper.all)'Unchecked_Access;
            end if;
            Set
            (  Layer.Handlers (3),
               Handlers.Connect
               (  Sweeper,
                  "changed",
                  Handlers.To_Marshaller (Changed_Sweeper'Access),
                  Layer'Unchecked_Access
            )  );
            Set
            (  Layer.Handlers (4),
               Handlers.Connect
               (  Sweeper,
                  "value_changed",
                  Handlers.To_Marshaller (Changed_Sweeper'Access),
                  Layer'Unchecked_Access
            )  );
         end if;
      end if;
   end Set_Sweeper;

   procedure Set_Visible
             (  Layer   : in out Waveform_Layer;
                Visible : Boolean
             )  is
   begin
      if Layer.Visible /= Visible then
         Layer.Visible := Visible;
         Layer.Updated := True;
      end if;
   end Set_Visible;

   procedure Set_Widened
             (  Layer   : in out Waveform_Layer;
                Widened : Boolean
             )  is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Set_Y_Conversion
             (  Layer  : in out Waveform_Layer;
                Y1, Y2 : GDouble
             )  is
   begin
      Layer.YY :=
         (  (Y2 - Y1 + GDouble'Model_Epsilon)
         /  (GDouble (Layer.V1 - Layer.V2) + GDouble'Model_Epsilon * 2.0)
         );
      Layer.Y0 := Y2 + Layer.YY * GDouble (Layer.V2);
   end Set_Y_Conversion;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Waveform_Layer
             )  is
      Store_Amplifier : constant Boolean :=
         (  Layer.Amplifier_Adjustment /= null
         and then
            Layer.Amplifier_Object = null
         );
      Store_Sweeper : constant Boolean :=
         (  Layer.Sweeper_Adjustment /= null
         and then
            Layer.Sweeper_Object = null
         );
   begin
      Store (Stream, Layer.Box.X1);
      Store (Stream, Layer.Box.X2);
      Store (Stream, Layer.Box.Y1);
      Store (Stream, Layer.Box.Y2);
      Store (Stream, Layer.Line);
      Store (Stream, Layer.Mode);
      Store (Stream, Layer.Preferred);
      Store (Stream, Layer.Opacity);
      Store
      (  Stream,
         Layer.Scaled,
         Layer.Widened,
         Store_Amplifier,
         Store_Sweeper,
         Layer.Extrapolate_Left,
         Layer.Extrapolate_Right
      );
      if Store_Amplifier then
         Store (Stream, Layer.Amplifier_Adjustment);
      else
         Store (Stream, GDouble (Layer.V1));
         Store (Stream, GDouble (Layer.V2));
      end if;
      if Store_Sweeper then
         Store (Stream, Layer.Sweeper_Adjustment);
      else
         Store (Stream, GDouble (Layer.T1));
         Store (Stream, GDouble (Layer.T2));
      end if;
   end Store;

   procedure Sweep (Layer : in out Waveform_Layer; To : X_Axis) is
      dT : constant X_Axis := Layer.T2 - Layer.T1;
      dX : X_Axis := X_Axis'Truncation
                     (  X_Axis (Layer.Box.X2 - Layer.Box.X1 + 1.0)
                     );
   begin
      Layer.T2 := To;
      Layer.T1 := Layer.T2 - dT;
      Layer.Sampled := False;
      Layer.Updated := True;
   end Sweep;

   procedure Sweep
             (  Layer : in out Waveform_Layer;
                From  : X_Axis;
                To    : X_Axis
             )  is
   begin
      if From >= To then
         raise Constraint_Error with
            "Start time is not before the end time";
      end if;
      if Layer.T1 /= From or else Layer.T2 /= To then
         declare
            Span : constant X_Axis := To - From;
         begin
            --
            -- Resampling is forced when the page width is changed, when
            -- the page is moved backwards for more than one step,  when
            -- the page is moved forward beyound its right margin
            --
            if (  abs (Layer.T2 - Layer.T1 - Span) > Layer.dT / 10.0
               or else
                  Layer.T2 - To >= Layer.dT
               or else
                  From >= Layer.T2
               )
            then
               Layer.Valid := False;
               if 0 /= (Tracing_Mode and Trace_Waveform) then ----------
                  if abs (Layer.T2 - Layer.T1 - Span) > Layer.dT / 10.0
                  then
                     Trace_Line
                     (  Layer'Address,
                        "Sweep changed page span"
                     );
                  elsif Layer.T2 - To >= Layer.dT then
                     Trace_Line (Layer'Address, "Sweep backward");
                  else
                     Trace_Line (Layer'Address, "Sweep forward");
                  end if;
               end if; -------------------------------------------------
            end if;
         end;
         Layer.T1 := From;
         Layer.T2 := To;
         Layer.Sampled := False;
         Layer.Updated := True;
      end if;
   end Sweep;

   function To_Double (Value : Time) return GDouble is
   begin
      return GDouble (To_Duration (Value - Epoch));
   end To_Double;

   function To_Double (Value : Ada.Calendar.Time) return GDouble is
   begin
      return GDouble (Value - Calendar_Epoch);
   end To_Double;

   function To_Time (Value : GDouble) return Time is
   begin
      return Epoch + To_Time_Span (Duration (Value));
   end To_Time;

   function To_Time (Value : GDouble) return Ada.Calendar.Time is
   begin
      return Calendar_Epoch + Duration (Value);
   end To_Time;

   function To_Y (Layer : Waveform_Layer; V : Y_Axis) return GDouble is
   begin
      return Layer.Y0 - Layer.YY * GDouble (V) + 0.5;
   end To_Y;

begin
   declare
      use Ada.Calendar;
      type Seconds_Count is range 0..86_400;
      type Time_Data is record
         Time    : Ada.Calendar.Time;
         Year    : Year_Number;
         Month   : Month_Number;
         Day     : Day_Number;
         Seconds : Day_Duration;
      end record;
      T1, T2  : Time_Data;
      Seconds : Seconds_Count;
   begin
      loop
         T1.Time := Ada.Calendar.Clock;
         Epoch := Clock;
         T2.Time := Ada.Calendar.Clock;
         Split (T1.Time, T1.Year, T1.Month, T1.Day, T1.Seconds);
         Split (T2.Time, T2.Year, T2.Month, T2.Day, T2.Seconds);
         exit when
              (  T1.Year = T2.Year
              and then
                 T1.Month = T2.Month
              and then
                 T1.Day = T2.Day
              );
      end loop;
      T1.Seconds := (T1.Seconds + T2.Seconds) / 2;
      Seconds    := Seconds_Count (T1.Seconds) mod 60;
      Calendar_Epoch :=
         Time_Of
         (  T1.Year,
            T1.Month,
            T1.Day,
            Duration (Seconds)
         );
      Epoch := Epoch - To_Time_Span (T1.Seconds - Duration (Seconds));
   end;
end Gtk.Layered.Waveform;
