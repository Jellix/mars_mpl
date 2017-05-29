--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Graph_Paper                     Luebeck            --
--  Implementation                                 Spring, 2011       --
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

with Cairo.Line_Cap_Property;

with Glib.Messages;
with Glib.Properties.Creation;

with Gtk.Layered.Stream_IO;

with Interfaces.C;

package body Gtk.Layered.Graph_Paper is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Handlers""");

   Max_Tick : constant := 1_000_000;

   type Layer_Property is
     (Property_Scaled,
      Property_Widened,
      Property_X1,
      Property_X2,
      Property_Y1,
      Property_Y2,
      Property_X_Tick_Length,
      Property_Y_Tick_Length,
      Property_Major_Line_Width,
      Property_Major_Line_Color,
      Property_Major_Line_Cap,
      Property_Minor_Line_Width,
      Property_Minor_Line_Color,
      Property_Minor_Line_Cap);

   package Handlers is
      new Gtk.Handlers.User_Callback
       (GObject_Record,
        Graph_Paper_Ptr);

   overriding procedure Finalize (Layer : in out Graph_Paper_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      while Layer.Annotations /= null loop
         Detach (Layer, Layer.Annotations.all.Annotation.all);
      end loop;
      if Layer.X_Axis /= null then
         Layer.X_Axis.all.Unref;
      end if;
      if Layer.Y_Axis /= null then
         Layer.Y_Axis.all.Unref;
      end if;
      Finalize (Abstract_Layer (Layer));
   end Finalize;

   overriding procedure Set_Widened
     (Layer   : in out Graph_Paper_Layer;
      Widened : Boolean) is
   begin
      Layer.Widened := Widened;
      Layer.Updated := True;
   end Set_Widened;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Graph_Paper." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
       (Graph_Paper_Layer,
        Graph_Paper_Ptr);

   procedure Changed_X
     (Adjustment : access GObject_Record'Class;
      Layer      : Graph_Paper_Ptr);

   procedure Changed_Y
     (Adjustment : access GObject_Record'Class;
      Layer      : Graph_Paper_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Graph_Paper_Layer
   is
      Ptr : Graph_Paper_Ptr := new Graph_Paper_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_X_Adjustment
     (Layer      : in out Graph_Paper_Layer;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Adjustment.all.Ref;
      Layer.X_Axis := Adjustment.all'Unchecked_Access;
      if Adjustment.all in Gtk.Layered.Waveform.Waveform_Sweeper'Class then
         Layer.X_Sweeper :=
           Gtk.Layered.Waveform.Waveform_Sweeper'Class
             (Adjustment.all)'Unchecked_Access;
      end if;
      Gtk.Handlers.References.Set
        (Layer.Handlers (1),
         Handlers.Connect
           (Adjustment,
            "changed",
            Handlers.To_Marshaller (Changed_X'Access),
            Layer'Unchecked_Access));
      Gtk.Handlers.References.Set
        (Layer.Handlers (2),
         Handlers.Connect
           (Adjustment,
            "value_changed",
            Handlers.To_Marshaller (Changed_X'Access),
            Layer'Unchecked_Access));
   end Add_X_Adjustment;

   procedure Add_Y_Adjustment
     (Layer      : in out Graph_Paper_Layer;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Adjustment.all.Ref;
      Layer.Y_Axis := Adjustment.all'Unchecked_Access;
      if Adjustment.all in Gtk.Layered.Waveform.Waveform_Sweeper'Class then
         Layer.Y_Sweeper :=
           Gtk.Layered.Waveform.Waveform_Sweeper'Class
             (Adjustment.all)'Unchecked_Access;
      end if;
      Gtk.Handlers.References.Set
        (Layer.Handlers (3),
         Handlers.Connect
           (Adjustment,
            "changed",
            Handlers.To_Marshaller (Changed_Y'Access),
            Layer'Unchecked_Access));
      Gtk.Handlers.References.Set
        (Layer.Handlers (4),
         Handlers.Connect
           (Adjustment,
            "value_changed",
            Handlers.To_Marshaller (Changed_Y'Access),
            Layer'Unchecked_Access));
   end Add_Y_Adjustment;

   procedure Add_Graph_Paper
     (Under          : not null access Layer_Location'Class;
      Box            : Cairo.Ellipses.Cairo_Box;
      X_Tick_Length  : Positive                                          := 50;
      Y_Tick_Length  : Positive                                          := 50;
      Major_Width    : Gdouble                                           := 1.0;
      Minor_Width    : Gdouble                                           := 1.0;
      Major_Color    : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Minor_Color    : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (0.5, 0.5, 0.5);
      Major_Line_Cap : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Minor_Line_Cap : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      X_Axis         : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Y_Axis         : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled         : Boolean                                           := False;
      Widened        : Boolean                                           := False)
   is
      Ptr   : Graph_Paper_Ptr := new Graph_Paper_Layer;
      Layer : Graph_Paper_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Box           => Box,
         X_Tick_Length => X_Tick_Length,
         Y_Tick_Length => Y_Tick_Length,
         Major_Line    => (Major_Width, Major_Color, Major_Line_Cap),
         Minor_Line    => (Minor_Width, Minor_Color, Minor_Line_Cap));
      if X_Axis /= null then
         Add_X_Adjustment (Ptr.all, X_Axis);
      end if;
      if Y_Axis /= null then
         Add_Y_Adjustment (Ptr.all, Y_Axis);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Graph_Paper;

   function Add_Graph_Paper
     (Under          : not null access Layer_Location'Class;
      Box            : Cairo.Ellipses.Cairo_Box;
      X_Tick_Length  : Positive                                          := 50;
      Y_Tick_Length  : Positive                                          := 50;
      Major_Width    : Gdouble                                           := 1.0;
      Minor_Width    : Gdouble                                           := 1.0;
      Major_Color    : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Minor_Color    : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (0.5, 0.5, 0.5);
      Major_Line_Cap : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      Minor_Line_Cap : Cairo.Cairo_Line_Cap                              := Cairo.Cairo_Line_Cap_Butt;
      X_Axis         : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Y_Axis         : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled         : Boolean                                           := False;
      Widened        : Boolean                                           := False)
      return not null access Graph_Paper_Layer
   is
      Ptr   : Graph_Paper_Ptr := new Graph_Paper_Layer;
      Layer : Graph_Paper_Layer renames Ptr.all;
   begin
      Layer.Scaled  := Scaled;
      Layer.Widened := Widened;
      Add (Ptr, Under);
      Set
        (Layer         => Layer,
         Box           => Box,
         X_Tick_Length => X_Tick_Length,
         Y_Tick_Length => Y_Tick_Length,
         Major_Line    => (Major_Width, Major_Color, Major_Line_Cap),
         Minor_Line    => (Minor_Width, Minor_Color, Minor_Line_Cap));
      if X_Axis /= null then
         Add_X_Adjustment (Ptr.all, X_Axis);
      end if;
      if Y_Axis /= null then
         Add_Y_Adjustment (Ptr.all, Y_Axis);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Graph_Paper;

   procedure Attach
     (Layer      : in out Graph_Paper_Layer;
      Annotation : in out Graph_Paper_Annotation_Interface'Class) is
   begin
      if Layer.Annotations = null then
         Layer.Annotations := new Item (Annotation'Unchecked_Access);
      elsif Find (Layer, Annotation) = null then
         Layer.Annotations :=
            new Item'
             (Annotation => Annotation'Unchecked_Access,
              Next       => Layer.Annotations,
              Previous   => Layer.Annotations.all.Previous);
         Layer.Annotations.all.Next.all.Previous := Layer.Annotations;
         Layer.Annotations.all.Previous.all.Next := Layer.Annotations;
      end if;
   end Attach;

   procedure Changed_X
     (Adjustment : access GObject_Record'Class;
      Layer      : Graph_Paper_Ptr) is
   begin
      Layer.all.Changed := True;
      Layer.all.Updated := True;
      if
        not Layer.all.Widget.all.Drawing and then
        Layer.all.Updated and then
        (Adjustment.all not in Gtk.Layered.Waveform.Waveform_Sweeper'Class
         or else
           not Gtk.Layered.Waveform.Waveform_Sweeper'Class
             (Adjustment.all).Is_Active)
      then
         Queue_Draw (Layer.all.Widget); -- Signal draw to the widget
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Changed_X"));
   end Changed_X;

   procedure Changed_Y
     (Adjustment : access GObject_Record'Class;
      Layer      : Graph_Paper_Ptr)
   is
      pragma Unreferenced (Adjustment);
   begin
      Layer.all.Changed := True;
      Layer.all.Updated := True;
      if not Layer.all.Widget.all.Drawing then
         Queue_Draw (Layer.all.Widget); -- Signal draw to the widget
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Changed_Y"));
   end Changed_Y;

   procedure Detach
     (Layer      : in out Graph_Paper_Layer;
      Annotation : in out Graph_Paper_Annotation_Interface'Class)
   is
      procedure Free is new Ada.Unchecked_Deallocation (Item, Item_Ptr);
      This : Item_Ptr := Find (Layer, Annotation);
   begin
      if This /= null then
         if This = Layer.Annotations then
            if This.all.Next = This then
               Layer.Annotations := null;
            else
               Layer.Annotations := This.all.Next;
            end if;
         end if;
         This.all.Next.all.Previous := This.all.Previous;
         This.all.Previous.all.Next := This.all.Next;
         begin
            This.all.Annotation.all.Detached;
         exception
            when Error : others =>
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Detach notification fault: "
                  & Ada.Exceptions.Exception_Information (Error)
                  & Where ("Detach"));
         end;
         Free (This);
      end if;
   end Detach;

   overriding procedure Draw
     (Layer   : in out Graph_Paper_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      X1, X2 : Gdouble;
      Y1, Y2 : Gdouble;
      T1, T2 : Gdouble := 0.0;
      V1, V2 : Gdouble := 0.0;
      FX, FY : Interfaces.C.long_double;

      procedure Draw_X (Major : Boolean) is
         use type Interfaces.C.long_double;
         function Abs_X (T : Gdouble) return Interfaces.C.long_double is
            pragma Inline (Abs_X);
         begin
            return Interfaces.C.long_double'Rounding (FX * Interfaces.C.long_double (T));
         end Abs_X;
         Minor : Natural := Layer.X_Raster.Low_Tick;
         Shift : constant Interfaces.C.long_double :=
            Interfaces.C.long_double'Rounding (Interfaces.C.long_double (X1)) + 0.5 - Abs_X (T1);
         X : Gdouble;
      begin
         for Index in Natural'Range loop
            X := Gdouble
              (Abs_X
                 (Layer.X_Raster.Low_Value +
                      Layer.X_Raster.Minor * Gdouble (Index)) +
                   Shift);
            exit when X > X2;
            if Minor = 0 or else Minor > Layer.X_Raster.Ticks then
               -- Major tick
               if Major then
                  Cairo.Move_To (Context, X, Y1);
                  Cairo.Line_To (Context, X, Y2);
               end if;
               Minor := 1;
            else
               -- Minor tick
               if not Major then
                  Cairo.Move_To (Context, X, Y1);
                  Cairo.Line_To (Context, X, Y2);
               end if;
               Minor := Minor + 1;
            end if;
         end loop;
      end Draw_X;

      procedure Draw_Y (Major : Boolean) is
         use type Interfaces.C.long_double;
         function Abs_Y (V : Gdouble) return Interfaces.C.long_double is
            pragma Inline (Abs_Y);
         begin
            return Interfaces.C.long_double'Rounding (FY * Interfaces.C.long_double (V));
         end Abs_Y;
         Minor : Natural := Layer.Y_Raster.Low_Tick;
         Shift : constant Interfaces.C.long_double :=
            Interfaces.C.long_double'Rounding (Interfaces.C.long_double (Y2)) + 0.5 + Abs_Y (V1);
         Y : Gdouble;
      begin
         for Index in Natural'Range loop
            Y := Gdouble
              (Shift -
                 Abs_Y
                   (Layer.Y_Raster.Low_Value +
                        Layer.Y_Raster.Minor * Gdouble (Index)));
            exit when Y < Y1;
            if Minor = 0 or else Minor > Layer.Y_Raster.Ticks then
               -- Major tick
               if Major then
                  Cairo.Move_To (Context, X1, Y);
                  Cairo.Line_To (Context, X2, Y);
               end if;
               Minor := 1;
            else
               -- Minor tick
               if not Major then
                  Cairo.Move_To (Context, X1, Y);
                  Cairo.Line_To (Context, X2, Y);
               end if;
               Minor := Minor + 1;
            end if;
         end loop;
      end Draw_Y;

      use type Interfaces.C.long_double;
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
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
            X1 := Gdouble'Floor   (X1) + 0.5;
            X2 := Gdouble'Ceiling (X2) - 0.5;
            Y1 := Gdouble'Floor   (Y1) + 0.5;
            Y2 := Gdouble'Ceiling (Y2) - 0.5;
         end;
      else
         X1 := Layer.Box.X1;
         X2 := Layer.Box.X2;
         Y1 := Layer.Box.Y1;
         Y2 := Layer.Box.Y2;
      end if;
      if Layer.X_Axis = null then
         if Layer.Y_Axis = null then
            return;
         end if;
      else
         T1 := Gtk.Adjustment.Get_Value (Layer.X_Axis);
         T2 := T1 + Gtk.Adjustment.Get_Page_Size (Layer.X_Axis);
         if X2 <= X1 or else T2 <= T1 then
            return;
         end if;
         if Layer.Changed then
            Layer.X_Raster :=
              Gtk.Layered.Waveform.Rasters.Create
                (T1,
                 T2,
                 Natural
                   ((X2 - X1 + 1.0) / Gdouble (Layer.X_Tick_Length)));
         end if;
         FX := Interfaces.C.long_double (X2 - X1 + 1.0) / Interfaces.C.long_double (T2 - T1);
      end if;
      if Layer.Y_Axis /= null then
         V1 := Gtk.Adjustment.Get_Value (Layer.Y_Axis);
         V2 := V1 + Gtk.Adjustment.Get_Page_Size (Layer.Y_Axis);
         if Y2 <= Y1 or else V2 <= V1 then
            return;
         end if;
         if Layer.Changed then
            Layer.Y_Raster :=
              Gtk.Layered.Waveform.Rasters.Create
                (V1,
                 V2,
                 Natural
                   ((Y2 - Y1 + 1.0) / Gdouble (Layer.Y_Tick_Length)));
         end if;
         FY := Interfaces.C.long_double (Y2 - Y1 + 1.0) / Interfaces.C.long_double (V2 - V1);
      end if;
      if Layer.Changed then
         Layer.Changed := False;
         declare -- Notifying all annotations
            This : access Item := Layer.Annotations;
         begin
            if This /= null then
               loop
                  begin
                     This.all.Annotation.all.Changed
                       (Layer => Layer,
                        From  => Gtk.Layered.Waveform.X_Axis (T1),
                        To    => Gtk.Layered.Waveform.X_Axis (T2),
                        Lower => Gtk.Layered.Waveform.Y_Axis (V1),
                        Upper => Gtk.Layered.Waveform.Y_Axis (V2),
                        Box   => (X1 => X1, X2 => X2,
                                  Y1 => Y1, Y2 => Y2));
                  exception
                     when Error : others =>
                        Glib.Messages.Log
                          (Gtk.Missed.GtkAda_Contributions_Domain,
                           Glib.Messages.Log_Level_Critical,
                           "Change notification fault: "
                           & Ada.Exceptions.Exception_Information (Error)
                           & Where ("Draw"));
                  end;
                  This := This.all.Next;
                  exit when This = Layer.Annotations;
               end loop;
            end if;
         end;
      end if;
      if Layer.Major_Line.Width > 0.0 then -- Major ticks
         Cairo.Set_Source_Rgb
           (Context,
            Gdouble (Gdk.Color.Red (Layer.Major_Line.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Green (Layer.Major_Line.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Blue (Layer.Major_Line.Color)) / Gdouble (Guint16'Last));
         Cairo.Set_Line_Cap (Context, Layer.Major_Line.Line_Cap);
         if Layer.Widened then
            Cairo.Set_Line_Width
              (Context,
               Layer.Major_Line.Width * Layer.Widget.all.Get_Size);
         else
            Cairo.Set_Line_Width (Context, Layer.Major_Line.Width);
         end if;
         Cairo.New_Path (Context);
         if Layer.X_Axis /= null then
            Draw_X (True); -- Vertical major ticks
         end if;
         if Layer.Y_Axis /= null then
            Draw_Y (True); -- Horizontal major ticks
         end if;
         Cairo.Stroke (Context);
      end if;
      if Layer.Minor_Line.Width > 0.0 then -- Minor ticks
         Cairo.Set_Source_Rgb
           (Context,
            Gdouble (Gdk.Color.Red (Layer.Minor_Line.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Green (Layer.Minor_Line.Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Blue (Layer.Minor_Line.Color)) / Gdouble (Guint16'Last));
         Cairo.Set_Line_Cap (Context, Layer.Minor_Line.Line_Cap);
         if Layer.Widened then
            Cairo.Set_Line_Width
              (Context,
               Layer.Minor_Line.Width * Layer.Widget.all.Get_Size);
         else
            Cairo.Set_Line_Width (Context, Layer.Minor_Line.Width);
         end if;
         Cairo.New_Path (Context);
         if Layer.X_Axis /= null then
            Draw_X (False); -- Vertical minor ticks
         end if;
         if Layer.Y_Axis /= null then
            Draw_Y (False); -- Horizontal minor ticks
         end if;
         Cairo.Stroke (Context);
      end if;
      Layer.Updated := False;
   end Draw;

   function Find
     (Layer      : Graph_Paper_Layer;
      Annotation : Graph_Paper_Annotation_Interface'Class) return Item_Ptr
   is
      type Reference is access constant
                        Graph_Paper_Annotation_Interface'Class;
      Ptr  : constant Reference := Annotation'Access;
      This : access Item := Layer.Annotations;
   begin
      if This = null then
         return null;
      else
         loop
            if This.all.Annotation = Ptr then
               return This.all'Unchecked_Access;
            end if;
            This := This.all.Next;
            if This = Layer.Annotations then
               return null;
            end if;
         end loop;
      end if;
   end Find;

   function Get_Box
     (Layer : Graph_Paper_Layer) return Cairo.Ellipses.Cairo_Box is
   begin
      if Layer.Scaled then
         declare
            X_Size : constant Gdouble :=
                     Gdouble (Layer.Widget.all.Get_Allocated_Width);
            Y_Size : constant Gdouble :=
                     Gdouble (Layer.Widget.all.Get_Allocated_Height);
         begin
            return
              (X1 => Layer.Box.X1 * X_Size + Layer.Widget.all.Get_Center.X,
               X2 => Layer.Box.X2 * X_Size + Layer.Widget.all.Get_Center.X,
               Y1 => Layer.Box.Y1 * Y_Size + Layer.Widget.all.Get_Center.Y,
               Y2 => Layer.Box.Y2 * Y_Size + Layer.Widget.all.Get_Center.Y);
         end;
      else
         return Layer.Box;
      end if;
   end Get_Box;

   function Get_Major_Line (Layer : Graph_Paper_Layer)
      return Line_Parameters is
   begin
      return Layer.Major_Line;
   end Get_Major_Line;

   function Get_Minor_Line (Layer : Graph_Paper_Layer)
      return Line_Parameters is
   begin
      return Layer.Minor_Line;
   end Get_Minor_Line;

   overriding function Get_Properties_Number
     (Layer : Graph_Paper_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last) -
             Layer_Property'Pos (Layer_Property'First) + 1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Graph_Paper_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X1 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x1",
                    Nick    => "x1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the waveform's box left margin");
            when Property_X2 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x2",
                    Nick    => "x2",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The x-coordinate of the waveform's box right margin");
            when Property_Y1 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y1",
                    Nick    => "y1",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the waveform's box top margin");
            when Property_Y2 =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y2",
                    Nick    => "y2",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The y-coordinate of the waveform's box bottom margin");
            when Property_X_Tick_Length =>
               return
                 Glib.Properties.Creation.Gnew_Uint
                   (Name    => "x-tick-width",
                    Nick    => "x tick",
                    Minimum => 1,
                    Maximum => Max_Tick,
                    Default => 50,
                    Blurb   =>
                       "The approximate distance between two major x-axis " &
                       "ticks in pixels.");
            when Property_Y_Tick_Length =>
               return
                 Glib.Properties.Creation.Gnew_Uint
                   (Name    => "y-tick-width",
                    Nick    => "y tick",
                    Minimum => 1,
                    Maximum => Max_Tick,
                    Default => 50,
                    Blurb   =>
                       "The approximate distance between two major y-axis " &
                       "ticks in pixels.");
            when Property_Major_Line_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "major-tick-line-width",
                    Nick    => "major width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The major tick line's width, when 0 the line is not " &
                       "drawn");
            when Property_Major_Line_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "major-tick-line-color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "major tick line color",
                    Blurb      => "The major tick line's color");
            when Property_Major_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "major-tick-line-cap",
                    Nick    => "major tick line cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the major tick lines");
            when Property_Minor_Line_Width =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "minor-tick-line-width",
                    Nick    => "minor width",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The minor tick line's width, when 0 the line is not " &
                       "drawn");
            when Property_Minor_Line_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "minor-tick-line-color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "minor tick line color",
                    Blurb      => "The minor tick line's color");
            when Property_Minor_Line_Cap =>
               return
                  Cairo.Line_Cap_Property.Gnew_Enum
                   (Name    => "minor-tick-line-cap",
                    Nick    => "minor tick line cap",
                    Default => Cairo.Cairo_Line_Cap_Butt,
                    Blurb   => "The cap style of the minor tick lines");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The scale size is changed when the widget is resized");
            when Property_Widened =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "widened",
                    Nick    => "widened",
                    Default => False,
                    Blurb   =>
                       "The tick's line width is changed  when the widget is " &
                       "resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Graph_Paper_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
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
               when Property_Major_Line_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Major_Line.Width);
               when Property_Major_Line_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Major_Line.Color);
               when Property_Major_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Major_Line.Line_Cap);
               when Property_Minor_Line_Width =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Minor_Line.Width);
               when Property_Minor_Line_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Minor_Line.Color);
               when Property_Minor_Line_Cap =>
                  Cairo.Line_Cap_Property.Set_Enum
                    (Value,
                     Layer.Minor_Line.Line_Cap);
               when Property_X_Tick_Length =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint (Value, Layer.X_Tick_Length);
               when Property_Y_Tick_Length =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint (Value, Layer.Y_Tick_Length);
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

   overriding function Get_Scaled (Layer : Graph_Paper_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   overriding function Get_Widened (Layer : Graph_Paper_Layer) return Boolean is
   begin
      return Layer.Widened;
   end Get_Widened;

   function Get_X_Axis
     (Layer : Graph_Paper_Layer) return Gtk.Adjustment.Gtk_Adjustment is
   begin
      return Layer.X_Axis;
   end Get_X_Axis;

   function Get_X_Raster
     (Layer : Graph_Paper_Layer) return Gtk.Layered.Waveform.Rasters.Scale
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Layer.Changed then
         raise
           Ada.IO_Exceptions.Use_Error with
             "The graph paper is not yet drawn";
      elsif Layer.X_Axis = null then
         raise
           Ada.IO_Exceptions.Use_Error with
             "The graph paper has no vertical axis";
      end if;
      return Layer.X_Raster;
   end Get_X_Raster;

   function Get_X_Tick_Length (Layer : Graph_Paper_Layer)
      return Positive is
   begin
      return Positive (Layer.X_Tick_Length);
   end Get_X_Tick_Length;

   function Get_Y_Axis
     (Layer : Graph_Paper_Layer) return Gtk.Adjustment.Gtk_Adjustment is
   begin
      return Layer.Y_Axis;
   end Get_Y_Axis;

   function Get_Y_Raster
     (Layer : Graph_Paper_Layer) return Gtk.Layered.Waveform.Rasters.Scale
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Layer.Changed then
         raise
           Ada.IO_Exceptions.Use_Error with
             "The graph paper is not yet drawn";
      elsif Layer.Y_Axis = null then
         raise
           Ada.IO_Exceptions.Use_Error with
             "The graph paper has no vertical axis";
      end if;
      return Layer.Y_Raster;
   end Get_Y_Raster;

   function Get_Y_Tick_Length (Layer : Graph_Paper_Layer)
      return Positive is
   begin
      return Positive (Layer.Y_Tick_Length);
   end Get_Y_Tick_Length;

   overriding function Is_Updated (Layer : Graph_Paper_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Graph_Paper_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.Box.X1  := Layer.Box.X1 + Offset.X;
      Layer.Box.X2  := Layer.Box.X2 + Offset.X;
      Layer.Box.Y1  := Layer.Box.Y1 + Offset.Y;
      Layer.Box.Y2  := Layer.Box.Y2 + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Prepare
     (Layer   : in out Graph_Paper_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      pragma Unreferenced (Context);
   begin
      if Layer.X_Sweeper /= null then
         Layer.X_Sweeper.all.Set_Current_Time
           (Layer.Widget.all.Get_Drawing_Time,
            True);
      end if;
--        if Layer.Y_Sweeper /= null then
--           Layer.Y_Sweeper.Set_Current_Time
--           (  Layer.Widget.Get_Drawing_Time
--           );
--        end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Prepare"));
   end Prepare;

   overriding procedure Resized
     (Layer : in out Graph_Paper_Layer;
      Area  : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
   begin
      Layer.Changed := True;
   end Resized;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Graph_Paper_Layer)
   is
      Box           : Cairo.Ellipses.Cairo_Box;
      Major_Line    : Line_Parameters;
      Minor_Line    : Line_Parameters;
      X_Tick_Length : Guint;
      Y_Tick_Length : Guint;
      X_Axis        : Boolean;
      Y_Axis        : Boolean;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Box.X1);
      Gtk.Layered.Stream_IO.Restore (Stream, Box.X2);
      Gtk.Layered.Stream_IO.Restore (Stream, Box.Y1);
      Gtk.Layered.Stream_IO.Restore (Stream, Box.Y2);
      Gtk.Layered.Stream_IO.Restore (Stream, Major_Line);
      Gtk.Layered.Stream_IO.Restore (Stream, Minor_Line);
      Gtk.Layered.Stream_IO.Restore (Stream, X_Tick_Length);
      Gtk.Layered.Stream_IO.Restore (Stream, Y_Tick_Length);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled, Layer.Widened, X_Axis, Y_Axis);
      Set
        (Layer         => Layer,
         Box           => Box,
         Major_Line    => Major_Line,
         Minor_Line    => Minor_Line,
         X_Tick_Length =>
           Positive (Guint'Min (Max_Tick, X_Tick_Length)),
         Y_Tick_Length =>
           Positive (Guint'Min (Max_Tick, Y_Tick_Length)));
      if X_Axis then
         declare
            Adjustment : Gtk.Adjustment.Gtk_Adjustment;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Adjustment);
            Add_X_Adjustment (Layer, Adjustment);
         end;
      end if;
      if Y_Axis then
         declare
            Adjustment : Gtk.Adjustment.Gtk_Adjustment;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Adjustment);
            Add_Y_Adjustment (Layer, Adjustment);
         end;
      end if;
   end Restore;

   overriding procedure Scale
     (Layer  : in out Graph_Paper_Layer;
      Factor : Gdouble)
   is
      pragma Unreferenced (Factor);
      Center_X    : constant Gdouble :=
                       (Layer.Box.X1 + Layer.Box.X2) * 0.5;
      Center_Y    : constant Gdouble :=
                       (Layer.Box.Y1 + Layer.Box.Y2) * 0.5;
      Half_Width  : constant Gdouble :=
                       (Layer.Box.X2 - Layer.Box.X1) * 0.5;
      Half_Height : constant Gdouble :=
                       (Layer.Box.Y2 - Layer.Box.Y1) * 0.5;
   begin
      Set
        (Layer         => Layer,
         Major_Line    => Layer.Major_Line,
         Minor_Line    => Layer.Minor_Line,
         X_Tick_Length => Positive (Layer.X_Tick_Length),
         Y_Tick_Length => Positive (Layer.Y_Tick_Length),
         Box           => (X1 => Center_X - Half_Width,
                           X2 => Center_X + Half_Width,
                           Y1 => Center_Y - Half_Height,
                           Y2 => Center_Y + Half_Height));
   end Scale;

   procedure Set
     (Layer         : in out Graph_Paper_Layer;
      Box           : Cairo.Ellipses.Cairo_Box;
      X_Tick_Length : Positive;
      Y_Tick_Length : Positive;
      Major_Line    : Line_Parameters;
      Minor_Line    : Line_Parameters) is
   begin
      if Major_Line.Width < 0.0 then
         raise Constraint_Error with "Negative major tick lines width";
      elsif Minor_Line.Width < 0.0 then
         raise Constraint_Error with "Negative minor tick lines width";
      elsif Box.X1 > Box.X2 then
         raise Constraint_Error with "Negative box width";
      elsif Box.Y1 > Box.Y2 then
         raise Constraint_Error with "Negative box height";
--        elsif X_Tick_Length <= 0 then
--           raise Constraint_Error with "Non-positive x-axis tick length";
--        elsif Y_Tick_Length <= 0 then
--           raise Constraint_Error with "Non-positive y-axis tick length";
      end if;
      Layer.Box           := Box;
      Layer.X_Tick_Length := Guint (X_Tick_Length);
      Layer.Y_Tick_Length := Guint (Y_Tick_Length);
      Layer.Major_Line    := Major_Line;
      Layer.Minor_Line    := Minor_Line;
      Layer.Changed       := True;
      Layer.Updated       := True;
   end Set;

   overriding procedure Set_Property_Value
     (Layer    : in out Graph_Paper_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_X1 =>
               declare
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
               begin
                  if New_Value >= Layer.Box.X2 then
                     Layer.Box.X2 := New_Value + 1.0;
                  end if;
                  Layer.Box.X1  := New_Value;
                  Layer.Changed := True;
               end;
            when Property_X2 =>
               declare
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
               begin
                  if Layer.Box.X1 > New_Value then
                     Layer.Box.X1 := New_Value - 1.0;
                  end if;
                  Layer.Box.X2  := New_Value;
                  Layer.Changed := True;
               end;
            when Property_Y1 =>
               declare
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
               begin
                  if New_Value >= Layer.Box.Y2 then
                     Layer.Box.Y2 := New_Value + 1.0;
                  end if;
                  Layer.Box.Y1  := New_Value;
                  Layer.Changed := True;
               end;
            when Property_Y2 =>
               declare
                  New_Value : constant Gdouble :=
                                Glib.Values.Get_Double (Value);
               begin
                  if Layer.Box.Y1 >= New_Value then
                     Layer.Box.Y1 := New_Value - 1.0;
                  end if;
                  Layer.Box.Y2  := New_Value;
                  Layer.Changed := True;
               end;
            when Property_Major_Line_Width =>
               Layer.Major_Line.Width := Glib.Values.Get_Double (Value);
               if Layer.Major_Line.Width < 0.0 then
                  Layer.Major_Line.Width := 0.0;
               end if;
               Layer.Changed := True;
            when Property_Major_Line_Color =>
               Layer.Major_Line.Color := Gdk.Color.Get_Value (Value);
               Layer.Changed := True;
            when Property_Major_Line_Cap =>
               Layer.Major_Line.Line_Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
               Layer.Changed := True;
            when Property_Minor_Line_Width =>
               Layer.Minor_Line.Width := Glib.Values.Get_Double (Value);
               if Layer.Minor_Line.Width < 0.0 then
                  Layer.Minor_Line.Width := 0.0;
               end if;
               Layer.Changed := True;
            when Property_Minor_Line_Color =>
               Layer.Minor_Line.Color := Gdk.Color.Get_Value (Value);
               Layer.Changed := True;
            when Property_Minor_Line_Cap =>
               Layer.Minor_Line.Line_Cap :=
                  Cairo.Line_Cap_Property.Get_Enum (Value);
               Layer.Changed := True;
            when Property_X_Tick_Length =>
               declare
                  Tick_Length : constant Guint := Glib.Values.Get_Uint (Value);
               begin
                  if Tick_Length < 1 then
                     Layer.X_Tick_Length := 1;
                  elsif Tick_Length > Max_Tick then
                     Layer.X_Tick_Length := Max_Tick;
                  else
                     Layer.X_Tick_Length := Glib.Values.Get_Uint (Value);
                  end if;
               end;
            when Property_Y_Tick_Length =>
               declare
                  Tick_Length : constant Guint := Glib.Values.Get_Uint (Value);
               begin
                  if Tick_Length < 1 then
                     Layer.Y_Tick_Length := 1;
                  elsif Tick_Length > Max_Tick then
                     Layer.Y_Tick_Length := Max_Tick;
                  else
                     Layer.Y_Tick_Length := Glib.Values.Get_Uint (Value);
                  end if;
               end;
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
            when Property_Widened =>
               Layer.Widened := Glib.Values.Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Graph_Paper_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_X_Axis
     (Layer      : not null access Graph_Paper_Layer;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Reset_Axis is
      begin
         Gtk.Handlers.References.Set (Layer.all.Handlers (1));
         Gtk.Handlers.References.Set (Layer.all.Handlers (2));
         Layer.all.X_Axis.all.Unref;
         Layer.all.X_Axis := null;
      end Reset_Axis;

      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         if Layer.all.X_Axis /= null then
            Reset_Axis;
            Layer.all.Changed := True;
            Layer.all.Updated := True;
         end if;
      else
         if Layer.all.X_Axis /= Adjustment then
            if Layer.all.X_Axis /= null then
               Reset_Axis;
            end if;
            Add_X_Adjustment (Layer.all, Adjustment);
            Layer.all.Changed := True;
            Layer.all.Updated := True;
         end if;
      end if;
   end Set_X_Axis;

   procedure Set_X_Tick_Length
     (Layer  : not null access Graph_Paper_Layer;
      Length : Positive) is
   begin
      if Length > Max_Tick then
         Layer.all.X_Tick_Length := Max_Tick;
      else
         Layer.all.X_Tick_Length := Guint (Length);
      end if;
      Layer.all.Changed := True;
      Layer.all.Updated := True;
   end Set_X_Tick_Length;

   procedure Set_Y_Axis
     (Layer      : not null access Graph_Paper_Layer;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Reset_Axis is
      begin
         Gtk.Handlers.References.Set (Layer.all.Handlers (3));
         Gtk.Handlers.References.Set (Layer.all.Handlers (4));
         Layer.all.Y_Axis.all.Unref;
         Layer.all.Y_Axis := null;
      end Reset_Axis;

      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      if Adjustment = null then
         if Layer.all.Y_Axis /= null then
            Reset_Axis;
            Layer.all.Changed := True;
            Layer.all.Updated := True;
         end if;
      else
         if Layer.all.Y_Axis /= Adjustment then
            if Layer.all.Y_Axis /= null then
               Reset_Axis;
            end if;
            Add_Y_Adjustment (Layer.all, Adjustment);
            Layer.all.Changed := True;
            Layer.all.Updated := True;
         end if;
      end if;
   end Set_Y_Axis;

   procedure Set_Y_Tick_Length
     (Layer  : not null access Graph_Paper_Layer;
      Length : Positive) is
   begin
      if Length > Max_Tick then
         Layer.all.Y_Tick_Length := Max_Tick;
      else
         Layer.all.Y_Tick_Length := Guint (Length);
      end if;
      Layer.all.Changed := True;
      Layer.all.Updated := True;
   end Set_Y_Tick_Length;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Graph_Paper_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.X1);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.X2);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.Y1);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Box.Y2);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Major_Line);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Minor_Line);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.X_Tick_Length);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Y_Tick_Length);
      Gtk.Layered.Stream_IO.Store
        (Stream,
         Layer.Scaled,
         Layer.Widened,
         Layer.X_Axis /= null,
         Layer.Y_Axis /= null);
      if Layer.X_Axis /= null then
         Gtk.Layered.Stream_IO.Store (Stream, Layer.X_Axis);
      end if;
      if Layer.Y_Axis /= null then
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Y_Axis);
      end if;
   end Store;

   pragma Warnings (On, "declaration hides ""Handlers""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Layered.Graph_Paper;
