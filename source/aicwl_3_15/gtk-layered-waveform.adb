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
-- __________________________________________________________________ --

with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with Cairo.Line_Cap_Property;

with Glib.Messages;
with Glib.Properties.Creation;

with Gtk.Layered.Interpolation_Mode_Property;
with Gtk.Layered.Stream_IO;
with Gtk.Layered.Waveform_Drawing_Method_Property;

package body Gtk.Layered.Waveform is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Handlers""");

   Dump_New_Line : constant Boolean := False;
   Dump_X_Value  : constant Boolean := True;
   Dump_Y_Value  : constant Boolean := True;
   -----------------------------------------------------------------------------
   function Where (Name : String) return String;

   procedure Dump
     (Layer : Waveform_Layer;
      X     : Horizontal_Offset;
      V     : Y_Axis);

   procedure Dump
     (Layer : Waveform_Layer;
      Data  : Points_Array;
      Index : Integer);
   -----------------------------------------------------------------------------

   ---------------------------------------------------------------------
   use type Ada.Calendar.Time;

   Epoch          : Ada.Real_Time.Time;
   Calendar_Epoch : Ada.Calendar.Time;

   type Point is record
      T : X_Axis;
      V : Y_Axis;
   end record;

   type Layer_Property is
     (Property_Scaled,
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
      Property_Color);

   package Handlers is
     new Gtk.Handlers.User_Callback (GObject_Record, Waveform_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation (Points_Array, Points_Array_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation (Waveform_Layer, Waveform_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Waveform_Data_Scanner'Class,
        Waveform_Data_Scanner_Ptr);
   function Interpolate (T : X_Axis; L, R : Point) return Y_Axis;
   pragma Inline (Interpolate);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Waveform_Layer
   is
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
     (Under     : not null access Layer_Location'Class;
      Box       : Cairo.Ellipses.Cairo_Box;
      Width     : Gdouble                                           := 1.0;
      Color     : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Line_Cap  : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Sweeper   : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Amplifier : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Mode      : Interpolation_Mode                                := Linear;
      Left      : Boolean                                           := False;
      Right     : Boolean                                           := False;
      Opacity   : Fill_Opacity                                      := 1.0;
      Scaled    : Boolean                                           := False;
      Widened   : Boolean                                           := False)
   is
      Ptr   : Waveform_Ptr := new Waveform_Layer;
      Layer : Waveform_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer   => Layer,
         Box     => Box,
         Mode    => Mode,
         Left    => Left,
         Right   => Right,
         Opacity => Opacity,
         Line    => (Color    => Color,
                     Width    => Width,
                     Line_Cap => Line_Cap));
      Ptr.all.Set_Amplifier (Amplifier);
      Ptr.all.Set_Sweeper (Sweeper);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Waveform;

   function Add_Waveform
     (Under     : not null access Layer_Location'Class;
      Box       : Cairo.Ellipses.Cairo_Box;
      Width     : Gdouble                                           := 1.0;
      Color     : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (1.0, 0.0, 0.0);
      Line_Cap  : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Sweeper   : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Amplifier : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Mode      : Interpolation_Mode                                := Linear;
      Left      : Boolean                                           := False;
      Right     : Boolean                                           := False;
      Opacity   : Fill_Opacity                                      := 1.0;
      Scaled    : Boolean                                           := False;
      Widened   : Boolean                                           := False)
      return not null access Waveform_Layer
   is
      Ptr   : Waveform_Ptr := new Waveform_Layer;
      Layer : Waveform_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer   => Layer,
         Box     => Box,
         Mode    => Mode,
         Left    => Left,
         Right   => Right,
         Opacity => Opacity,
         Line    => (Color    => Color,
                     Width    => Width,
                     Line_Cap => Line_Cap));
      Ptr.all.Set_Amplifier (Amplifier);
      Ptr.all.Set_Sweeper (Sweeper);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Waveform;

   procedure Amplify
     (Layer : in out Waveform_Layer;
      Lower : Y_Axis;
      Upper : Y_Axis) is
   begin
      if Lower >= Upper then
         raise Constraint_Error with
           "Lower value is greater or equal to the upper value";
      end if;
      if Layer.V1 /= Upper or else Layer.V2 /= Lower then
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            if Layer.V1 /= Upper then
               Trace_Line
                 (Layer'Address,
                  ("Amplify V1 ="
                   &  Edit.Image (Gdouble (Layer.V1))
                   &  " /="
                   &  Edit.Image (Gdouble (Upper))
                   &  " = upper"));
            end if;
            if Layer.V2 /= Lower then
               Trace_Line
                 (Layer'Address,
                  ("Amplify V2 ="
                   &  Edit.Image (Gdouble (Layer.V2))
                   &  " /="
                   &  Edit.Image (Gdouble (Lower))
                   &  " = lower"));
            end if;
         end if; -------------------------------------------------------
         Layer.V1 := Upper;
         Layer.V2 := Lower;
         Layer.Updated := True;
         Layer.Valid   := False;
      end if;
   end Amplify;

   procedure Changed
     (Layer : in out Waveform_Layer;
      From  : X_Axis;
      To    : X_Axis) is
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
     (Adjustment : access GObject_Record'Class;
      Layer      : Waveform_Ptr)
   is
      pragma Unreferenced (Adjustment);
   begin
      Layer.all.Query_Amplifier;
      if not Layer.all.Widget.all.Drawing and then Layer.all.Updated then
         Queue_Draw (Layer.all.Widget); -- Signal draw to the widget
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Changed_Amplifier"));
   end Changed_Amplifier;

   procedure Changed_Sweeper
     (Adjustment : access GObject_Record'Class;
      Layer      : Waveform_Ptr) is
   begin
      Layer.all.Query_Sweeper;
      if
        not Layer.all.Widget.all.Drawing and then
        Layer.all.Updated and then
        (Adjustment.all not in Waveform_Sweeper'Class or else
           not Waveform_Sweeper'Class (Adjustment.all).Is_Active)
      then
         Queue_Draw (Layer.all.Widget); -- Signal draw to the widget
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Changed_Sweeper"));
   end Changed_Sweeper;

   overriding procedure Draw
     (Layer   : in out Waveform_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
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
     (Layer   : in out Waveform_Layer;
      Context : Cairo.Cairo_Context;
      Data    : in out Line_Method_Data) is separate;

   procedure Dump
     (Layer : Waveform_Layer;
      X     : Horizontal_Offset;
      V     : Y_Axis) is
   begin
      if Dump_X_Value then
         Trace (Layer'Address, Horizontal_Offset'Image (X) & " :");
      end if;
      Trace (Layer'Address, Edit.Image (Gdouble (V)));
      if Dump_Y_Value then
         Trace
           (Layer'Address,
            " =" & Gint'Image (Gint (Layer.To_Y (V))));
      end if;
   exception
      when others =>
         null;
   end Dump;

   procedure Dump
     (Layer : Waveform_Layer;
      Data  : Points_Array;
      Index : Integer) is
   begin
      if Dump_New_Line then
         Trace_Line (Layer'Address, Integer'Image (Index) & '=');
      else
         Trace (Layer'Address, Integer'Image (Index) & '=');
      end if;
      Dump (Layer, Data (Index).X, Data (Index).Y);
   end Dump;

   overriding procedure Finalize (Layer : in out Waveform_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Source (Layer);
      Free (Layer.Line_Data.Points);
      if Layer.Amplifier_Adjustment /= null then
         Gtk.Adjustment.Unref (Layer.Amplifier_Adjustment);
      end if;
      if Layer.Sweeper_Adjustment /= null then
         Gtk.Adjustment.Unref (Layer.Sweeper_Adjustment);
      end if;
      Finalize (Abstract_Layer (Layer));
   end Finalize;

   procedure Get
     (Source : in out Waveform_Data_Scanner'Class;
      T      : X_Axis;
      Mode   : Interpolation_Mode;
      V      : out Y_Axis)
   is
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
                 ((Y_Axis (T - T1) * V2
                  +  Y_Axis (T2 - T) * V1)
                  /  Y_Axis (T2 - T1));
         end case;
      end if;
   end Get;

   procedure Get
     (Source : in out Waveform_Data_Scanner'Class;
      T      : X_Axis;
      Mode   : Interpolation_Mode;
      V      : out Y_Axis;
      Got_It : out Boolean)
   is
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
                 ((Y_Axis (T - T1) * V2
                  +  Y_Axis (T2 - T) * V1)
                  /  Y_Axis (T2 - T1));
         end case;
      end if;
   end Get;

   function Get (Layer : Waveform_Layer; X : Gdouble) return Y_Axis is
      Got_It : Boolean;
      V      : Y_Axis;
   begin
      Layer.Get (X, V, Got_It);
      if Got_It then
         return V;
      else
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Get;

   procedure Get
     (Layer  : Waveform_Layer;
      X      : Gdouble;
      Y      : out Y_Axis;
      Got_It : out Boolean) is
   begin
      if Layer.Data = null then
         Got_It := False;
      else
         declare
            T : constant X_Axis := Get_T (Layer, X);
         begin
            Layer.Data.all.Get (T, Layer.Mode, Y, Got_It);
            if
              not Got_It and then
              (Layer.Extrapolate_Left or else
               Layer.Extrapolate_Right)
            then
               declare
                  T1, T2 : X_Axis;
                  V1, V2 : Y_Axis;
               begin
                  Layer.Data.all.First (T1, V1, Got_It);
                  Layer.Data.all.Last  (T2, V2, Got_It);
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

   function Get_Amplifier
     (Layer : Waveform_Layer) return Gtk.Adjustment.Gtk_Adjustment is
   begin
      return Layer.Amplifier_Adjustment;
   end Get_Amplifier;

   function Get_Box (Layer : Waveform_Layer) return Cairo.Ellipses.Cairo_Box is
   begin
      return Layer.Box;
   end Get_Box;

   function Get_Epoch return Ada.Calendar.Time is
   begin
      return Calendar_Epoch;
   end Get_Epoch;

   function Get_Epoch return Ada.Real_Time.Time is
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
     (Layer : Waveform_Layer;
      X     : Gdouble;
      T     : out X_Axis;
      V     : out Y_Axis)
   is
      Got_It : Boolean;
   begin
      Layer.Get_Point (X, T, V, Got_It);
      if not Got_It then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Get_Point;

   procedure Get_Point
     (Layer  : Waveform_Layer;
      X      : Gdouble;
      T      : out X_Axis;
      V      : out Y_Axis;
      Got_It : out Boolean) is
   begin
      if Layer.Data = null then
         Got_It := False;
      else
         T := Get_T (Layer, X);
         Layer.Data.all.Get (T, Layer.Mode, V, Got_It);
         if
           not Got_It and then
           (Layer.Extrapolate_Left or else Layer.Extrapolate_Right)
         then
            declare
               T1, T2 : X_Axis;
               V1, V2 : Y_Axis;
            begin
               Layer.Data.all.First (T1, V1, Got_It);
               Layer.Data.all.Last  (T2, V2, Got_It);
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

   overriding function Get_Properties_Number
     (Layer : Waveform_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         -  Layer_Property'Pos (Layer_Property'First)
         +  1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Waveform_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Opacity =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "fill-opacity",
                    Nick    => "opacity",
                    Minimum => 0.0,
                    Maximum => 1.0,
                    Default => 1.0,
                    Blurb   =>
                       "The opacity of filling. 0.0 " &
                       "is transparent, 1.0 is opaque");
            when Property_X1 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x1",
                    Nick    => "x1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the waveform's " &
                       "box left margin");
            when Property_X2 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x2",
                    Nick    => "x2",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The x-coordinate of the waveform's " &
                       "box right margin");
            when Property_Y1 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y1",
                    Nick    => "y1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the waveform's " &
                       "box top margin");
            when Property_Y2 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y2",
                    Nick    => "y2",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The y-coordinate of the waveform's " &
                       "box bottom margin");
            when Property_T1 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "t1",
                    Nick    => "t1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The time at the waveform's box " &
                       "left margin in seconds since the epoch");
            when Property_T2 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "t2",
                    Nick    => "t2",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The time at the waveform's box " &
                       "right margin in seconds since the " &
                       "epoch");
            when Property_V1 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "v1",
                    Nick    => "v1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The value at the waveform's box " &
                       "top margin");
            when Property_V2 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "v2",
                    Nick    => "v2",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The value at the waveform's box " &
                       "bottom margin");
            when Property_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The waveform color");
            when Property_Extrapolate_Left =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "extrapolate-left",
                    Nick    => "extrapolate-left",
                    Default => False,
                    Blurb   =>
                       "Extrapolation of waveform data " &
                       "to the left is allowed when is set " &
                       "to true");
            when Property_Extrapolate_Right =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "extrapolate-right",
                    Nick    => "extrapolate-right",
                    Default => False,
                    Blurb   =>
                       "Extrapolation of waveform data " &
                       "to the right is allowed when is set " &
                       "to true");
            when Property_Interpolation_Mode =>
               return
                 Gtk.Layered.Interpolation_Mode_Property.Gnew_Enum
                   (Name    => "interpolation-mode",
                    Nick    => "interpolation",
                    Default => Linear,
                    Blurb   =>
                       "The interpolation mode used between " &
                       "points of the waveform");
            when Property_Line_Cap =>
               return
                 Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "line-cap",
                    Nick    => "line cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the waveform line");
            when Property_Preferred_Method =>
               return
                 Gtk.Layered.Waveform_Drawing_Method_Property.Gnew_Enum
                   (Name    => "preferred-mode",
                    Nick    => "preferred mode",
                    Default => Resample_New_And_Stroke,
                    Blurb   =>
                       "The preferred method of drawing and " &
                       "sampling the waveform");
            when Property_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "width",
                    Nick    => "width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   => "The waveform line width");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The waveform size is changed when " &
                       "the widget is resized");
            when Property_Visible =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "visible",
                    Nick    => "visible",
                    Default => True,
                    Blurb   =>
                       "The waveform is visible when this " &
                       "property is set to true");
            when Property_Widened =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "widened",
                    Nick    => "widened",
                    Default => False,
                    Blurb   =>
                       "The waveform line is thickened when " &
                       "the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Waveform_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Opacity =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Opacity);
               when Property_X1 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Box.X1);
               when Property_X2 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Box.X2);
               when Property_Y1 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Box.Y1);
               when Property_Y2 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Box.Y2);
               when Property_T1 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Gdouble (Layer.T1));
               when Property_T2 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Gdouble (Layer.T2));
               when Property_V1 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Gdouble (Layer.V1));
               when Property_V2 =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Gdouble (Layer.V2));
               when Property_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Line.Width);
               when Property_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Line.Color);
               when Property_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Line.Line_Cap);
               when Property_Extrapolate_Left =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Extrapolate_Left);
               when Property_Extrapolate_Right =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Extrapolate_Right);
               when Property_Interpolation_Mode =>
                  Gtk.Layered.Interpolation_Mode_Property.Set_Enum
                    (Value,
                     Layer.Mode);
               when Property_Preferred_Method =>
                  Gtk.Layered.Waveform_Drawing_Method_Property.Set_Enum
                    (Value,
                     Layer.Preferred);
               when Property_Scaled =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Scaled);
               when Property_Visible =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Visible);
               when Property_Widened =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Widened);
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

   overriding function Get_Scaled (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Source
     (Layer : Waveform_Layer) return access Waveform_Data_Source'Class is
   begin
      if Layer.Data = null then
         return null;
      else
         return Layer.Data.all.Get_Source;
      end if;
   end Get_Source;

   function Get_Sweeper
     (Layer : Waveform_Layer) return Gtk.Adjustment.Gtk_Adjustment is
   begin
      return Layer.Sweeper_Adjustment;
   end Get_Sweeper;

   function Get_T (Layer : Waveform_Layer; X : Gdouble) return X_Axis is
      X1 : X_Axis := X_Axis (Layer.Box.X1);
      dX : X_Axis := X_Axis (Layer.Box.X2 - Layer.Box.X1 + 1.0);
   begin
      if Layer.Scaled then
         declare
            X_Size : constant X_Axis :=
                       X_Axis (Layer.Widget.all.Get_Allocated_Width);
         begin
            X1 := X1 * X_Size + X_Axis (Layer.Widget.all.Get_Center.X);
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

   function Get_V (Layer : Waveform_Layer; Y : Gdouble) return Y_Axis is
      Y2 : Y_Axis := Y_Axis (Layer.Box.Y2);
      dY : Y_Axis := Y_Axis (Layer.Box.Y2 - Layer.Box.Y1);
   begin
      if Layer.Scaled then
         declare
            Y_Size : constant Y_Axis :=
                       Y_Axis (Layer.Widget.all.Get_Allocated_Height);
         begin
            Y2 := Y2 * Y_Size + Y_Axis (Layer.Widget.all.Get_Center.Y);
            dY := dY * Y_Size;
         end;
      end if;
      return Layer.V2 - (Y2 - Y_Axis (Y)) * (Layer.V2 - Layer.V1) / dY;
   end Get_V;

   function Get_V1 (Layer : Waveform_Layer) return Y_Axis is
   begin
      return Layer.V1;
   end Get_V1;

   function Get_V2 (Layer : Waveform_Layer) return Y_Axis is
   begin
      return Layer.V2;
   end Get_V2;

   overriding function Get_Widened (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Get_X (Layer : Waveform_Layer; T : X_Axis) return Gdouble is
      X1 : X_Axis := X_Axis (Layer.Box.X1);
      dX : X_Axis := X_Axis (Layer.Box.X2 - Layer.Box.X1 + 1.0);
   begin
      if Layer.Scaled then
         declare
            X_Size : constant X_Axis :=
                       X_Axis (Layer.Widget.all.Get_Allocated_Width);
         begin
            X1 := X1 * X_Size + X_Axis (Layer.Widget.all.Get_Center.X);
            dX := dX * X_Size;
         end;
      end if;
      return
        Gdouble ((T - Layer.T1) * dX / (Layer.T2 - Layer.T1) + X1);
   end Get_X;

   function Get_Y (Layer : Waveform_Layer; V : Y_Axis) return Gdouble is
      Y2 : Y_Axis := Y_Axis (Layer.Box.Y2);
      dY : Y_Axis := Y_Axis (Layer.Box.Y2 - Layer.Box.Y1 + 1.0);
   begin
      if Layer.Scaled then
         declare
            Y_Size : constant Y_Axis :=
                       Y_Axis (Layer.Widget.all.Get_Allocated_Height);
         begin
            Y2 := Y2 * Y_Size + Y_Axis (Layer.Widget.all.Get_Center.Y);
            dY := dY * Y_Size;
         end;
      end if;
      return
        Gdouble
          (Y2 - (Layer.V2 - V) * dY / (Layer.V2 - Layer.V1));
   end Get_Y;

   function Interpolate (T : X_Axis; L, R : Point) return Y_Axis is
   begin
      if L.V = R.V then
         return L.V;
      else
         return
           ((R.V * Y_Axis (T - L.T) + L.V * Y_Axis (R.T - T))
            /  Y_Axis (R.T - L.T));
      end if;
   end Interpolate;

   overriding function Is_Updated (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   function Is_Visible (Layer : Waveform_Layer) return Boolean is
   begin
      return Layer.Visible;
   end Is_Visible;

   overriding procedure Move
     (Layer  : in out Waveform_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.Box.X1  := Layer.Box.X1 + Offset.X;
      Layer.Box.X2  := Layer.Box.X2 + Offset.X;
      Layer.Box.Y1  := Layer.Box.Y1 + Offset.Y;
      Layer.Box.Y2  := Layer.Box.Y2 + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Prepare
     (Layer   : in out Waveform_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      pragma Unreferenced (Context);
      X1, X2 : Gdouble;
      --  Y1, Y2 : Gdouble;
   begin
      if Layer.Sweeper_Object /= null then
         Layer.Sweeper_Object.all.Set_Current_Time
           (Layer.Widget.all.Get_Drawing_Time,
            True);
         Layer.Query_Sweeper;
      end if;
      if Layer.Scaled then
         declare
            X_Size : constant Gdouble :=
                       Gdouble (Layer.Widget.all.Get_Allocated_Width);
            --  Y_Size : constant Gdouble :=
            --             Gdouble (Layer.Widget.all.Get_Allocated_Height);
         begin
            X1 := Layer.Box.X1 * X_Size + Layer.Widget.all.Get_Center.X;
            X2 := Layer.Box.X2 * X_Size + Layer.Widget.all.Get_Center.X;
            --  Y1 := Layer.Box.Y1 * Y_Size + Layer.Widget.all.Get_Center.Y;
            --  Y2 := Layer.Box.Y2 * Y_Size + Layer.Widget.all.Get_Center.Y;
         end;
      else
         X1 := Layer.Box.X1;
         X2 := Layer.Box.X2;
         --  Y1 := Layer.Box.Y1;
         --  Y2 := Layer.Box.Y2;
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
                 (Layer.Line_Data,
                  Horizontal_Offset (X1),
                  Horizontal_Offset (X2));
         end case;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Prepare"));
   end Prepare;

   procedure Query_Amplifier (Layer : in out Waveform_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Layer.Amplifier_Adjustment /= null then
         declare
            Value : constant Y_Axis :=
                      Y_Axis (Gtk.Adjustment.Get_Value (Layer.Amplifier_Adjustment));
            Size  : constant Y_Axis :=
                      Y_Axis (Gtk.Adjustment.Get_Page_Size (Layer.Amplifier_Adjustment));
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
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Query_Amplifier"));
   end Query_Amplifier;

   procedure Query_Sweeper (Layer : in out Waveform_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Layer.Sweeper_Adjustment /= null then
         declare
            Value : constant X_Axis :=
                      X_Axis (Layer.Sweeper_Adjustment.all.Get_Value);
            Size  : constant X_Axis :=
                      X_Axis (Layer.Sweeper_Adjustment.all.Get_Page_Size);
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
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Query_Sweeper"));
   end Query_Sweeper;

   overriding procedure Resized
     (Layer : in out Waveform_Layer;
      Area  : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
   begin
      Layer.Sampled := False;
      Layer.Valid   := False;
      if 0 /= (Tracing_Mode and Trace_Waveform) then -------------------
         Trace_Line (Layer'Address, "Resized");
      end if; ----------------------------------------------------------
   end Resized;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Waveform_Layer)
   is
      Box       : Cairo.Ellipses.Cairo_Box;
      Line      : Line_Parameters;
      Mode      : Interpolation_Mode;
      Preferred : Waveform_Drawing_Method;
      Opacity   : Gdouble;
      Amplifier : Boolean;
      Sweeper   : Boolean;
      Left      : Boolean;
      Right     : Boolean;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Box.X1);
      Gtk.Layered.Stream_IO.Restore (Stream, Box.X2);
      Gtk.Layered.Stream_IO.Restore (Stream, Box.Y1);
      Gtk.Layered.Stream_IO.Restore (Stream, Box.Y2);
      Gtk.Layered.Stream_IO.Restore (Stream, Line);
      Gtk.Layered.Stream_IO.Restore (Stream, Mode);
      Gtk.Layered.Stream_IO.Restore (Stream, Preferred);
      Gtk.Layered.Stream_IO.Restore (Stream, Opacity);
      Gtk.Layered.Stream_IO.Restore
        (Stream,
         Layer.Scaled,
         Layer.Widened,
         Amplifier,
         Sweeper,
         Left,
         Right);
      Opacity := Gdouble'Min (1.0, Gdouble'Max (0.0, Opacity));
      Set
        (Layer   => Layer,
         Box     => Box,
         Line    => Line,
         Mode    => Mode,
         Left    => Left,
         Right   => Right,
         Opacity => Opacity);
      Set_Preferred_Method (Layer, Preferred);
      if Amplifier then
         declare
            Adjustment : Gtk.Adjustment.Gtk_Adjustment;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Adjustment);
            Layer.Set_Amplifier (Adjustment);
         end;
      else
         declare
            V1, V2 : Gdouble;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, V1);
            Gtk.Layered.Stream_IO.Restore (Stream, V2);
            Amplify (Layer, Y_Axis (V1), Y_Axis (V2));
         end;
      end if;
      if Sweeper then
         declare
            Adjustment : Gtk.Adjustment.Gtk_Adjustment;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Adjustment);
            Layer.Set_Sweeper (Adjustment);
         end;
      else
         declare
            T1, T2 : Gdouble;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, T1);
            Gtk.Layered.Stream_IO.Restore (Stream, T2);
            Sweep (Layer, X_Axis (T1), X_Axis (T2));
         end;
      end if;
      Layer.Sampled := False;
      Layer.Valid   := False;
   end Restore;

   procedure Sample_Lines
     (Layer  : in out Waveform_Layer;
      Data   : in out Line_Method_Data;
      X1, X2 : Horizontal_Offset) is separate;

   overriding procedure Scale
     (Layer  : in out Waveform_Layer;
      Factor : Gdouble)
   is
      pragma Unreferenced (Factor);

      Center_X    : constant Gdouble :=
                      (Layer.Box.X1 + Layer.Box.X2) * 0.5;
      Center_Y    : constant Gdouble :=
                      (Layer.Box.Y1 + Layer.Box.Y2) * 0.5;
      Half_Width  : constant Gdouble :=
                      (Layer.Box.X2 - Layer.Box.X1 + 1.0) * 0.5;
      Half_Height : constant Gdouble :=
                      (Layer.Box.Y2 - Layer.Box.Y1 + 1.0) * 0.5;
   begin
      Set
        (Layer   => Layer,
         Line    => Layer.Line,
         Mode    => Layer.Mode,
         Left    => Layer.Extrapolate_Left,
         Right   => Layer.Extrapolate_Right,
         Opacity => Layer.Opacity,
         Box     => (X1 => Center_X - Half_Width,
                     X2 => Center_X + Half_Width,
                     Y1 => Center_Y - Half_Height,
                     Y2 => Center_Y + Half_Height));
   end Scale;

   procedure Set
     (Layer   : in out Waveform_Layer;
      Box     : Cairo.Ellipses.Cairo_Box;
      Line    : Line_Parameters;
      Mode    : Interpolation_Mode;
      Left    : Boolean;
      Right   : Boolean;
      Opacity : Fill_Opacity)
   is
      use type Cairo.Ellipses.Cairo_Box;
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
     (Layer     : in out Waveform_Layer;
      Amplifier : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Amplifier /= Layer.Amplifier_Adjustment then
         if Layer.Amplifier_Adjustment /= null then
            Gtk.Handlers.References.Set (Layer.Handlers (1));
            Gtk.Handlers.References.Set (Layer.Handlers (2));
            Gtk.Adjustment.Unref (Layer.Amplifier_Adjustment);
            Layer.Amplifier_Adjustment := null;
         end if;
         if Amplifier /= null then
            Gtk.Adjustment.Ref (Amplifier);
            Layer.Amplifier_Adjustment :=
              Amplifier.all'Unchecked_Access;
            if Amplifier.all in Waveform_Amplifier'Class then
               Layer.Amplifier_Object :=
                 Waveform_Amplifier'Class
                   (Amplifier.all)'Unchecked_Access;
            end if;
            Gtk.Handlers.References.Set
              (Layer.Handlers (1),
               Handlers.Connect
                 (Amplifier,
                  "changed",
                  Handlers.To_Marshaller (Changed_Amplifier'Access),
                  Layer'Unchecked_Access));
            Gtk.Handlers.References.Set
              (Layer.Handlers (2),
               Handlers.Connect
                 (Amplifier,
                  "value_changed",
                  Handlers.To_Marshaller (Changed_Amplifier'Access),
                  Layer'Unchecked_Access));
         end if;
      end if;
   end Set_Amplifier;

   procedure Set_Color
     (Layer : in out Waveform_Layer;
      Color : Gdk.Color.Gdk_Color) is
   begin
      Layer.Line.Color := Color;
      Layer.Updated := True;
   end Set_Color;

   procedure Set_Extrapolation_Mode
     (Layer : in out Waveform_Layer;
      Left  : Boolean;
      Right : Boolean) is
   begin
      if
        Layer.Extrapolate_Left /= Left or else Layer.Extrapolate_Right /= Right
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
     (Layer : in out Waveform_Layer;
      Mode  : Interpolation_Mode) is
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

   procedure Set_Opacity
     (Layer   : in out Waveform_Layer;
      Opacity : Fill_Opacity) is
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

   procedure Set_Preferred_Method
     (Layer  : in out Waveform_Layer;
      Method : Waveform_Drawing_Method) is
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

   overriding procedure Set_Property_Value
     (Layer    : in out Waveform_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Opacity =>
               Layer.Set_Opacity
                 (Gdouble
                    (Gdouble'Min
                         (1.0,
                          Gdouble'Max (0.0, Glib.Values.Get_Double (Value)))));
            when Property_T1 =>
               declare
                  New_Value : constant X_Axis :=
                                X_Axis (Glib.Values.Get_Double (Value));
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
                                X_Axis (Glib.Values.Get_Double (Value));
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
                                Y_Axis (Glib.Values.Get_Double (Value));
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
                                Y_Axis (Glib.Values.Get_Double (Value));
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
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
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
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
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
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
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
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
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
               Layer.Line.Width := Glib.Values.Get_Double (Value);
               if Layer.Line.Width < 0.0 then
                  Layer.Line.Width := 0.0;
               end if;
               Layer.Updated := True;
            when Property_Color =>
               Layer.Line.Color := Gdk.Color.Get_Value (Value);
               Layer.Updated    := True;
            when Property_Extrapolate_Left =>
               Set_Extrapolation_Mode
                 (Layer => Layer,
                  Left  => Glib.Values.Get_Boolean (Value),
                  Right => Layer.Extrapolate_Right);
            when Property_Extrapolate_Right =>
               Set_Extrapolation_Mode
                 (Layer => Layer,
                  Left  => Layer.Extrapolate_Left,
                  Right => Glib.Values.Get_Boolean (Value));
            when Property_Interpolation_Mode =>
               Set_Interpolation_Mode
                 (Layer,
                  Gtk.Layered.Interpolation_Mode_Property.Get_Enum (Value));
            when Property_Preferred_Method =>
               Set_Preferred_Method
                 (Layer,
                  Gtk.Layered.Waveform_Drawing_Method_Property.
                    Get_Enum (Value));
            when Property_Line_Cap =>
               Layer.Line.Line_Cap :=
                 Cairo.Line_Cap_Property.Get_Enum (Value);
               Layer.Updated := True;
            when Property_Scaled =>
               Layer.Scaled  := Glib.Values.Get_Boolean (Value);
               Layer.Updated := True;
            when Property_Visible =>
               Layer.Visible := Glib.Values.Get_Boolean (Value);
               Layer.Updated := True;
            when Property_Widened =>
               Layer.Scaled  := Glib.Values.Get_Boolean (Value);
               Layer.Updated := True;
         end case;
      end if;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Waveform_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Source (Layer : in out Waveform_Layer) is
   begin
      if Layer.Data /= null then
         Layer.Data.all.Get_Source.all.Disconnected (Layer);
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
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Source"));
   end Set_Source;

   procedure Set_Source
     (Layer  : in out Waveform_Layer;
      Source : in out Waveform_Data_Source'Class) is
   begin
      Set_Source (Layer);
      Layer.Data :=
        new Waveform_Data_Scanner'Class'
          (Create (Source'Unchecked_Access));
      Layer.Data.all.Get_Source.all.Connected (Layer);
      Layer.Sampled := False;
      Layer.Updated := True;
      Layer.Valid   := False;
      if 0 /= (Tracing_Mode and Trace_Waveform) then -------------------
         Trace_Line (Layer'Address, "Set_Source");
      end if; ----------------------------------------------------------
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Source"));
   end Set_Source;

   procedure Set_Sweeper
     (Layer   : in out Waveform_Layer;
      Sweeper : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Sweeper /= Layer.Sweeper_Adjustment then
         if Layer.Sweeper_Adjustment /= null then
            Gtk.Handlers.References.Set (Layer.Handlers (3));
            Gtk.Handlers.References.Set (Layer.Handlers (4));
            Gtk.Adjustment.Unref (Layer.Sweeper_Adjustment);
            Layer.Sweeper_Adjustment := null;
         end if;
         if Sweeper /= null then
            Gtk.Adjustment.Ref (Sweeper);
            Layer.Sweeper_Adjustment := Sweeper.all'Unchecked_Access;
            if Sweeper.all in Waveform_Sweeper'Class then
               Layer.Sweeper_Object :=
                 Waveform_Sweeper'Class (Sweeper.all)'Unchecked_Access;
            end if;
            Gtk.Handlers.References.Set
              (Layer.Handlers (3),
               Handlers.Connect
                 (Sweeper,
                  "changed",
                  Handlers.To_Marshaller (Changed_Sweeper'Access),
                  Layer'Unchecked_Access));
            Gtk.Handlers.References.Set
              (Layer.Handlers (4),
               Handlers.Connect
                 (Sweeper,
                  "value_changed",
                  Handlers.To_Marshaller (Changed_Sweeper'Access),
                  Layer'Unchecked_Access));
         end if;
      end if;
   end Set_Sweeper;

   procedure Set_Visible
     (Layer   : in out Waveform_Layer;
      Visible : Boolean) is
   begin
      if Layer.Visible /= Visible then
         Layer.Visible := Visible;
         Layer.Updated := True;
      end if;
   end Set_Visible;

   overriding procedure Set_Widened
     (Layer   : in out Waveform_Layer;
      Widened : Boolean) is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   procedure Set_Y_Conversion
     (Layer  : in out Waveform_Layer;
      Y1, Y2 : Gdouble) is
   begin
      Layer.YY :=
        ((Y2 - Y1 + Gdouble'Model_Epsilon)
         / (Gdouble (Layer.V1 - Layer.V2) + Gdouble'Model_Epsilon * 2.0));
      Layer.Y0 := Y2 + Layer.YY * Gdouble (Layer.V2);
   end Set_Y_Conversion;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Waveform_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;

      Store_Amplifier : constant Boolean :=
                          Layer.Amplifier_Adjustment /= null and then
                              Layer.Amplifier_Object = null;
      Store_Sweeper   : constant Boolean :=
                          Layer.Sweeper_Adjustment /= null and then
                              Layer.Sweeper_Object = null;
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.X1);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.X2);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.Y1);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.Y2);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Line);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Mode);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Preferred);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Opacity);
      Gtk.Layered.Stream_IO.Store
        (Stream,
         Layer.Scaled,
         Layer.Widened,
         Store_Amplifier,
         Store_Sweeper,
         Layer.Extrapolate_Left,
         Layer.Extrapolate_Right);
      if Store_Amplifier then
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Amplifier_Adjustment);
      else
         Gtk.Layered.Stream_IO.Store (Stream, Gdouble (Layer.V1));
         Gtk.Layered.Stream_IO.Store (Stream, Gdouble (Layer.V2));
      end if;
      if Store_Sweeper then
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Sweeper_Adjustment);
      else
         Gtk.Layered.Stream_IO.Store (Stream, Gdouble (Layer.T1));
         Gtk.Layered.Stream_IO.Store (Stream, Gdouble (Layer.T2));
      end if;
   end Store;

   procedure Sweep (Layer : in out Waveform_Layer; To : X_Axis) is
      dT : constant X_Axis := Layer.T2 - Layer.T1;
      dX : X_Axis := X_Axis'Truncation
        (X_Axis (Layer.Box.X2 - Layer.Box.X1 + 1.0));
      pragma Unreferenced (dX);
   begin
      Layer.T2 := To;
      Layer.T1 := Layer.T2 - dT;
      Layer.Sampled := False;
      Layer.Updated := True;
   end Sweep;

   procedure Sweep
     (Layer : in out Waveform_Layer;
      From  : X_Axis;
      To    : X_Axis) is
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
            if
              abs (Layer.T2 - Layer.T1 - Span) > Layer.dT / 10.0 or else
              Layer.T2 - To >= Layer.dT or else
              From >= Layer.T2
            then
               Layer.Valid := False;
               if 0 /= (Tracing_Mode and Trace_Waveform) then ----------
                  if abs (Layer.T2 - Layer.T1 - Span) > Layer.dT / 10.0
                  then
                     Trace_Line
                       (Layer'Address,
                        "Sweep changed page span");
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

   function To_Double (Value : Ada.Real_Time.Time) return Gdouble
   is
      use type Ada.Real_Time.Time;
   begin
      return Gdouble (Ada.Real_Time.To_Duration (Value - Epoch));
   end To_Double;

   function To_Double (Value : Ada.Calendar.Time) return Gdouble is
   begin
      return Gdouble (Value - Calendar_Epoch);
   end To_Double;

   function To_Time (Value : Gdouble) return Ada.Real_Time.Time
   is
      use type Ada.Real_Time.Time;
   begin
      return Epoch + Ada.Real_Time.To_Time_Span (Duration (Value));
   end To_Time;

   function To_Time (Value : Gdouble) return Ada.Calendar.Time is
   begin
      return Calendar_Epoch + Duration (Value);
   end To_Time;

   function To_Y (Layer : Waveform_Layer; V : Y_Axis) return Gdouble is
   begin
      return Layer.Y0 - Layer.YY * Gdouble (V) + 0.5;
   end To_Y;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Waveform." & Name;
   end Where;

begin
   declare
      type Seconds_Count is range 0 .. 86_400;
      type Time_Data is record
         Time    : Ada.Calendar.Time;
         Year    : Ada.Calendar.Year_Number;
         Month   : Ada.Calendar.Month_Number;
         Day     : Ada.Calendar.Day_Number;
         Seconds : Ada.Calendar.Day_Duration;
      end record;
      T1, T2  : Time_Data;
      Seconds : Seconds_Count;

      use type Ada.Real_Time.Time;
   begin
      loop
         T1.Time := Ada.Calendar.Clock;
         Epoch   := Ada.Real_Time.Clock;
         T2.Time := Ada.Calendar.Clock;
         Ada.Calendar.Split (T1.Time, T1.Year, T1.Month, T1.Day, T1.Seconds);
         Ada.Calendar.Split (T2.Time, T2.Year, T2.Month, T2.Day, T2.Seconds);
         exit when
           T1.Year = T2.Year and then
           T1.Month = T2.Month and then
           T1.Day = T2.Day;
      end loop;
      T1.Seconds := (T1.Seconds + T2.Seconds) / 2;
      Seconds    := Seconds_Count (T1.Seconds) mod 60;
      Calendar_Epoch :=
        Ada.Calendar.Time_Of
          (T1.Year,
           T1.Month,
           T1.Day,
           Duration (Seconds));
      Epoch := Epoch - Ada.Real_Time.To_Time_Span (T1.Seconds - Duration (Seconds));
   end;

   pragma Warnings (On, "declaration hides ""Adjustment""");
   pragma Warnings (On, "declaration hides ""Handlers""");

end Gtk.Layered.Waveform;
