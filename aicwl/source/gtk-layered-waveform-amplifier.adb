--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.Amplifier              Luebeck            --
--  Implementation                                 Spring, 2011       --
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

with Gtkada.Types;
with Interfaces.C.Strings;

package body Gtk.Layered.Waveform.Amplifier is

   pragma Warnings (Off, "declaration hides ""Amplifier""");
   pragma Warnings (Off, "declaration hides ""Params""");

   Class_Record           : Ada_GObject_Class := Uninitialized_Class;
   Signal_Names           : constant Gtkada.Types.Chars_Ptr_Array
     := (0 => Interfaces.C.Strings.New_String ("autoscaling-changed"),
         1 => Interfaces.C.Strings.New_String ("raster-mode-changed"));
   Autoscaling_Changed_ID : Signal_Id := Invalid_Signal_Id;
   Raster_Mode_Changed_ID : Signal_Id := Invalid_Signal_Id;

   overriding procedure Add_Range
     (Amplifier    : not null access Gtk_Waveform_Amplifier_Record;
      Layer        : Waveform_Layer'Class;
      From,  To    : X_Axis;
      Lower, Upper : Y_Axis) is
      pragma Unreferenced (From, To);
   begin
      if Amplifier.all.Setting then
         if 0 /= (Tracing_Mode and Trace_Amplifier) then ---------------
            Trace_Line
              (Amplifier'Address,
               "Set to "
               & Edit.Image (Gdouble (Amplifier.all.Y1))
               & ".."
               & Edit.Image (Gdouble (Amplifier.all.Y2))
               & " to "
               & Edit.Image (Gdouble (Lower))
               & ".."
               & Edit.Image (Gdouble (Upper)));
         end if; -------------------------------------------------------
         Amplifier.all.Y1 := Y_Axis'Min (Amplifier.all.Y1, Lower);
         Amplifier.all.Y2 := Y_Axis'Max (Amplifier.all.Y2, Upper);
      else
         Amplifier.all.Y1 := Lower;
         Amplifier.all.Y2 := Upper;
         Amplifier.all.Setting := True;
         if 0 /= (Tracing_Mode and Trace_Amplifier) then ---------------
            Trace_Line
              (Amplifier'Address,
               "Reset to "
               &  Edit.Image (Gdouble (Lower))
               &  ".."
               &  Edit.Image (Gdouble (Upper)));
         end if; -------------------------------------------------------
      end if;
      if Layer.Scaled then
         Amplifier.all.Width :=
           Gdouble (Layer.Widget.all.Get_Allocated_Height) *
           (Layer.Box.Y2 - Layer.Box.Y1);
      else
         Amplifier.all.Width := Layer.Box.Y2 - Layer.Box.Y1;
      end if;
   end Add_Range;

   procedure EmitV
     (Params : System.Address;
      Signal : Signal_Id;
      Quark  : GQuark;
      Result : System.Address);
   pragma Import (C, EmitV, "g_signal_emitv");

   procedure Emit
     (Amplifier : not null access Gtk_Waveform_Amplifier_Record'Class;
      Signal    : Signal_Id);
   procedure Emit
     (Amplifier : not null access Gtk_Waveform_Amplifier_Record'Class;
      Signal    : Signal_Id)
   is
--        procedure Set_Object
--          (Value  : in out Glib.Values.GValue;
--           Object : System.Address);
--
--        pragma Import (C, Set_Object, "g_value_set_object");
      Params : Glib.Values.GValue_Array (0 .. 0);
      Result : Glib.Values.GValue;
   begin
      if Class_Record /= Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Glib.Values.Init (Params (0), This);
            Glib.Values.Set_Object (Params (0), Amplifier);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Glib.Values.Unset (Params (0));
         end;
      end if;
   end Emit;

   function Get_Auto_Scaling
     (Amplifier : not null access constant Gtk_Waveform_Amplifier_Record)
      return Boolean is
   begin
      return Amplifier.all.Auto;
   end Get_Auto_Scaling;

   overriding function Get_Lower
     (Amplifier : access Gtk_Waveform_Amplifier_Record) return Gdouble is
   begin
      Set_Range (Amplifier.all);
      return Gtk.Adjustment.Gtk_Adjustment_Record (Amplifier.all).Get_Lower;
   end Get_Lower;

   overriding function Get_Page_Size
     (Amplifier : access Gtk_Waveform_Amplifier_Record) return Gdouble is
   begin
      Set_Range (Amplifier.all);
      return Gtk.Adjustment.Gtk_Adjustment_Record (Amplifier.all).Get_Page_Size;
   end Get_Page_Size;

   function Get_Raster_Scaling
     (Amplifier : not null access constant Gtk_Waveform_Amplifier_Record)
      return Boolean is
   begin
      return Amplifier.all.Raster;
   end Get_Raster_Scaling;

   function Get_Scaling
     (Amplifier : not null access constant Gtk_Waveform_Amplifier_Record)
      return Waveform_Scaling is
   begin
      return Amplifier.all.Scaling;
   end Get_Scaling;

   function Get_Tick_Length
     (Amplifier : not null access constant Gtk_Waveform_Amplifier_Record)
      return Positive is
   begin
      return Amplifier.all.Tick;
   end Get_Tick_Length;

   function Get_Type return GType is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Adjustment.Get_Type,
         Signals      => Signal_Names,
         Class_Record => Class_Record,
         Type_Name    => Class_Name,
         Parameters   => (0 => (0 => GType_None),
                          1 => (0 => GType_None)));
      return Class_Record.all.The_Type;
   end Get_Type;

   overriding function Get_Upper
     (Amplifier : access Gtk_Waveform_Amplifier_Record) return Gdouble is
   begin
      Set_Range (Amplifier.all);
      return Gtk.Adjustment.Gtk_Adjustment_Record (Amplifier.all).Get_Upper;
   end Get_Upper;

   overriding function Get_Value
     (Amplifier : access Gtk_Waveform_Amplifier_Record) return Gdouble is
   begin
      Set_Range (Amplifier.all);
      return Gtk.Adjustment.Gtk_Adjustment_Record (Amplifier.all).Get_Value;
   end Get_Value;

   procedure Gtk_New (Amplifier : out Gtk_Waveform_Amplifier) is
   begin
      Amplifier := new Gtk_Waveform_Amplifier_Record;
      Gtk.Layered.Waveform.Amplifier.Initialize (Amplifier);
   end Gtk_New;

   procedure Initialize
     (Amplifier : not null access Gtk_Waveform_Amplifier_Record'Class) is
   begin
      G_New (Amplifier, Get_Type);
      Gtk.Adjustment.Initialize
        (Adjustment     => Amplifier,
         Value          => 0.0,
         Lower          => 0.0,
         Upper          => 1.0,
         Step_Increment => 0.1,
         Page_Increment => 1.0,
         Page_Size      => 1.0);
      --***
      -- This is a bug in GTK, which manifests itself as not setting all
      -- adjustment parameters upon Inilialize (gtk_adjustment_new). The
      -- workaround is to call Configure yet again in order to force the
      -- parameters. Normally Configure would be not necessary
      --
      Amplifier.all.Configure
        (Value          => 0.0,
         Lower          => 0.0,
         Upper          => 1.0,
         Step_Increment => 0.1,
         Page_Increment => 1.0,
         Page_Size      => 1.0);
      if Autoscaling_Changed_ID = Invalid_Signal_Id then
         declare
            Widget_Type : constant GType := Get_Type (Amplifier);
         begin
            Autoscaling_Changed_ID :=
              Lookup (Widget_Type, "autoscaling-changed");
         end;
      end if;
      if Raster_Mode_Changed_ID = Invalid_Signal_Id then
         declare
            Widget_Type : constant GType := Get_Type (Amplifier);
         begin
            Raster_Mode_Changed_ID :=
              Lookup (Widget_Type, "raster-mode-changed");
         end;
      end if;
   end Initialize;

   procedure Set_Auto_Scaling
     (Amplifier : not null access Gtk_Waveform_Amplifier_Record;
      Auto      : Boolean) is
   begin
      if Amplifier.all.Auto /= Auto then
         Amplifier.all.Auto := Auto;
         Emit (Amplifier, Autoscaling_Changed_ID);
      end if;
   end Set_Auto_Scaling;

   procedure Set_Range
     (Amplifier : in out Gtk_Waveform_Amplifier_Record)
   is
      function Get_Count return Positive;
      function Get_Count return Positive is
         Length : Gdouble := Amplifier.Width;
      begin
         Length := Length / Gdouble (Amplifier.Tick);
         if Length < 1.0 then
            return 1;
         elsif Length > Gdouble (Integer'Last) then
            return Integer'Last;
         else
            return Integer (Length);
         end if;
      end Get_Count;
   begin
      if not Amplifier.Setting then
         return;
      end if;
      if Amplifier.Auto then
         declare
            type Adjustment_Type is mod 2 ** 7;
            Undeflow_V1 : constant Adjustment_Type := 2 ** 0;
            Overflow_V1 : constant Adjustment_Type := 2 ** 1;
            Undeflow_V2 : constant Adjustment_Type := 2 ** 2;
            Overflow_V2 : constant Adjustment_Type := 2 ** 3;
            Raster_V1   : constant Adjustment_Type := 2 ** 4;
            Raster_V2   : constant Adjustment_Type := 2 ** 5;
            Empty       : constant Adjustment_Type := 2 ** 6;
            Changed : Adjustment_Type := 0;
            Parent  : constant not null access Gtk.Adjustment.Gtk_Adjustment_Record :=
                        Gtk.Adjustment.Gtk_Adjustment_Record
                          (Amplifier)'Unchecked_Access;
            From  : constant Gdouble := Gdouble (Amplifier.Y1);
            To    : constant Gdouble := Gdouble (Amplifier.Y2);
            Span  : constant Gdouble :=
                      (To - From) * Gdouble (Amplifier.Scaling);
            Size  : Gdouble;
            V1    : Gdouble := Gtk.Adjustment.Get_Value (Parent);
            V2    : Gdouble := V1 + Gtk.Adjustment.Get_Page_Size (Parent);
            Lower : Gdouble := Gtk.Adjustment.Get_Lower (Parent);
            Upper : Gdouble := Gtk.Adjustment.Get_Upper (Parent);
         begin
            if From < V1 then
               V1 := From - Span;
               Changed := Undeflow_V1;
            elsif From > V1 + Span then
               V1 := From - Span;
               Changed := Overflow_V1;
            end if;
            if To > V2 then
               V2 := To + Span;
               Changed := Changed or Undeflow_V2;
            elsif To < V2 - Span then
               V2 := To + Span;
               Changed := Changed or Overflow_V2;
            end if;
            if Amplifier.Raster then
               if V2 <= V1 then
                  V2 := V1 + 1.0;
                  Changed := Changed or Empty;
               end if;
               declare
                  V     : Gdouble;
                  Scale : constant Rasters.Scale :=
                            Rasters.Create (V1, V2, Get_Count);
               begin
                  V := V1 - Gdouble'Remainder (V1, Scale.Minor);
                  declare
                     Bound : constant Gdouble := From - Scale.Minor;
                  begin
                     while V > Bound loop
                        V := V - Scale.Minor;
                     end loop;
                  end;
                  if V /= V1 then
                     V1 := V;
                     Changed := Changed or Raster_V1;
                  end if;
                  V := V2 - Gdouble'Remainder (V2, Scale.Minor);
                  declare
                     Bound : constant Gdouble := To + Scale.Minor;
                  begin
                     while V < Bound loop
                        V := V + Scale.Minor;
                     end loop;
                  end;
                  if V /= V2 then
                     V2 := V;
                     Changed := Changed or Raster_V2;
                  end if;
               end;
            end if;
            if 0 /= Changed then
               Size  := V2 - V1;
               Lower := Gdouble'Min (From, Gdouble'Min (Lower, V1));
               Upper := Gdouble'Max (To,   Gdouble'Max (Upper, V2));
               if 0 /= (Tracing_Mode and Trace_Amplifier) then ---------
                  if 0 /= (Changed and Undeflow_V1) then
                     Trace_Line
                       (Amplifier'Address,
                        Edit.Image (Gdouble (Amplifier.Y1))
                        & " = V1 < ["
                        & Edit.Image (Gtk.Adjustment.Get_Value (Parent))
                        & " set to "
                        & Edit.Image (V1)
                        & ".."
                        & Edit.Image (V2)
                        & " ["
                        & Edit.Image (Gdouble (Amplifier.Y1))
                        & ".."
                        & Edit.Image (Gdouble (Amplifier.Y2))
                        & "]");
                  end if;
                  if 0 /= (Changed and Overflow_V1) then
                     Trace_Line
                       (Amplifier'Address,
                        (Edit.Image (Gtk.Adjustment.Get_Value (Parent))
                         &  "]>>>"
                         &  Edit.Image (Span)
                         &  " < V1 = "
                         &  Edit.Image (Gdouble (Amplifier.Y1))
                         &  " set to "
                         &  Edit.Image (V1)
                         &  ".."
                         &  Edit.Image (V2)
                         &  " ["
                         &  Edit.Image (Gdouble (Amplifier.Y1))
                         &  ".."
                         &  Edit.Image (Gdouble (Amplifier.Y2))
                         &  " ]"));
                  end if;
                  if 0 /= (Changed and Undeflow_V2) then
                     Trace_Line
                       (Amplifier'Address,
                        (Edit.Image
                             (Gtk.Adjustment.Get_Value (Parent)
                              + Gtk.Adjustment.Get_Page_Size (Parent))
                         &  "] < V2 = "
                         &  Edit.Image (Gdouble (Amplifier.Y2))
                         &  " set to "
                         &  Edit.Image (V1)
                         &  ".."
                         &  Edit.Image (V2)
                         &  " ["
                         &  Edit.Image (Gdouble (Amplifier.Y1))
                         &  ".."
                         &  Edit.Image (Gdouble (Amplifier.Y2))
                         &  " ]"));
                  end if;
                  if 0 /= (Changed and Overflow_V2) then
                     Trace_Line
                       (Amplifier'Address,
                        (Edit.Image (Gdouble (Amplifier.Y2))
                         &  " = V2 < "
                         &  Edit.Image (Span)
                         &  " <<<["
                         &  Edit.Image
                           (Gtk.Adjustment.Get_Value (Parent)
                            + Gtk.Adjustment.Get_Page_Size (Parent))
                         &  " set to "
                         &  Edit.Image (V1)
                         &  " .."
                         &  Edit.Image (V2)
                         &  " ["
                         &  Edit.Image (Gdouble (Amplifier.Y1))
                         &  " .."
                         &  Edit.Image (Gdouble (Amplifier.Y2))
                         &  " ]"));
                  end if;
                  if 0 /= (Changed and Raster_V1) then
                     Trace_Line
                       (Amplifier'Address,
                        "Set lower at raster "
                        & Edit.Image (V1)
                        & ".."
                        & Edit.Image (V2)
                        & " ["
                        & Edit.Image (Gdouble (Amplifier.Y1))
                        & ".."
                        & Edit.Image (Gdouble (Amplifier.Y2))
                        & " ]");
                  end if;
                  if 0 /= (Changed and Raster_V2) then
                     Trace_Line
                       (Amplifier'Address,
                        "Set upper at raster "
                        & Edit.Image (V1)
                        & ".."
                        & Edit.Image (V2)
                        & " ["
                        & Edit.Image (Gdouble (Amplifier.Y1))
                        & ".."
                        & Edit.Image (Gdouble (Amplifier.Y2))
                        & " ]");
                  end if;
                  if 0 /= (Changed and Empty) then
                     Trace_Line
                       (Amplifier'Address,
                        "Empty range "
                        & Edit.Image (V1)
                        & ".."
                        & Edit.Image (V2)
                        & " ["
                        & Edit.Image (Gdouble (Amplifier.Y1))
                        & ".."
                        & Edit.Image (Gdouble (Amplifier.Y2))
                        & " ]");
                  end if;
                  Trace_Line
                    (Amplifier'Address,
                     "Configured value "
                     & Edit.Image (V1)
                     & " page "
                     & Edit.Image (Size)
                     & " range "
                     & Edit.Image (Lower)
                     & ".."
                     & Edit.Image (Upper));
               end if; -------------------------------------------------
               Amplifier.Setting := False;
               Gtk.Adjustment.Configure
                 (Adjustment     => Parent,
                  Step_Increment => Size * 0.2,
                  Page_Increment => Size * 0.8,
                  Page_Size      => Size,
                  Value          => V1,
                  Lower          => Lower,
                  Upper          => Upper);
            end if;
         end;
      end if;
   end Set_Range;

   procedure Set_Raster_Scaling
     (Amplifier : not null access Gtk_Waveform_Amplifier_Record;
      Raster    : Boolean) is
   begin
      if Amplifier.all.Raster /= Raster then
         Amplifier.all.Raster := Raster;
         Emit (Amplifier, Raster_Mode_Changed_ID);
      end if;
   end Set_Raster_Scaling;

   procedure Set_Scaling
     (Amplifier : not null access Gtk_Waveform_Amplifier_Record;
      Scaling   : Waveform_Scaling) is
   begin
      Amplifier.all.Scaling := Scaling;
   end Set_Scaling;

   procedure Set_Tick_Length
     (Amplifier : not null access Gtk_Waveform_Amplifier_Record;
      Length    : Positive) is
   begin
      Amplifier.all.Tick := Length;
   end Set_Tick_Length;

   pragma Warnings (On, "declaration hides ""Params""");
   pragma Warnings (On, "declaration hides ""Amplifier""");

end Gtk.Layered.Waveform.Amplifier;