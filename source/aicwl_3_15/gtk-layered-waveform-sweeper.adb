--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.Sweeper                Luebeck            --
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

package body Gtk.Layered.Waveform.Sweeper is

   Max_Page : constant Duration :=
                (Duration'Last - Duration'First) / 10.0;
   Min_Page : constant Duration := Duration'Small * 10.0;
   Def_Page : constant Duration := 20.0;

   Class_Record        : Ada_GObject_Class := Uninitialized_Class;
   Signal_Names        : constant Gtkada.Types.Chars_Ptr_Array :=
                           (  0 => Interfaces.C.Strings.New_String ("freezing-changed"),
                              1 => Interfaces.C.Strings.New_String ("offset-changed")
                             );
   Freezing_Changed_ID : Signal_Id := Invalid_Signal_Id;

   procedure EmitV
     (  Params : System.Address;
        Signal : Signal_Id;
        Quark  : GQuark;
        Result : System.Address
       );
   pragma Import (C, EmitV, "g_signal_emitv");

   procedure Emit
     (  Sweeper : not null access
          Gtk_Waveform_Sweeper_Record'Class;
        Signal  : Signal_Id
       )
   is
      procedure Set_Object
        (Value  : in out Glib.Values.GValue;
         Object : System.Address);
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : Glib.Values.GValue_Array (0 .. 0);
      Result : Glib.Values.GValue;
   begin
      if Class_Record /= Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Glib.Values.Init (Params (0), This);
            Glib.Values.Set_Object (Params (0), Sweeper);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Glib.Values.Unset (Params (0));
         end;
      end if;
   end Emit;

   function Get_Frozen
     (  Sweeper : not null access constant
          Gtk_Waveform_Sweeper_Record
       )  return Boolean is
   begin
      return Sweeper.all.Frozen;
   end Get_Frozen;

   function Get_From
     (Sweeper : not null access Gtk_Waveform_Sweeper_Record)
      return Ada.Real_Time.Time is
   begin
      return To_Time (Get_Lower (Sweeper));
   end Get_From;

   function Get_From
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
       )  return Ada.Calendar.Time is
   begin
      return To_Time (Get_Lower (Sweeper));
   end Get_From;

   function Get_Offset
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
       )  return Duration is
   begin
      return
        Duration
          (  Sweeper.all.Get_Upper
             -  Sweeper.all.Get_Value
             -  Sweeper.all.Get_Page_Size
            );
   end Get_Offset;

   function Get_Time
     (Sweeper : not null access Gtk_Waveform_Sweeper_Record)
      return Ada.Real_Time.Time is
   begin
      return To_Time
        (  Gdouble
             (  Get_Value (Sweeper)
              +  Sweeper.all.Get_Page_Size
             )  );
   end Get_Time;

   function Get_Time
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
       )  return Ada.Calendar.Time is
   begin
      return To_Time
        (  Gdouble
             (  Get_Value (Sweeper)
              +  Sweeper.all.Get_Page_Size
             )  );
   end Get_Time;

   function Get_To
     (Sweeper : not null access Gtk_Waveform_Sweeper_Record)
      return Ada.Real_Time.Time is
   begin
      return To_Time (Sweeper.all.Get_Upper);
   end Get_To;

   function Get_To
     (Sweeper : not null access Gtk_Waveform_Sweeper_Record)
      return Ada.Calendar.Time is
   begin
      return To_Time (Sweeper.all.Get_Upper);
   end Get_To;

   function Get_Type return GType is
   begin
      Initialize_Class_Record
        (  Ancestor     => Gtk.Adjustment.Get_Type,
           Signals      => Signal_Names,
           Class_Record => Class_Record,
           Type_Name    => Class_Name,
           Parameters   => (  0 => (0 => GType_None),
                              1 => (0 => GType_None)
                             )                  );
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New (Sweeper : out Gtk_Waveform_Sweeper) is
   begin
      Sweeper := new Gtk_Waveform_Sweeper_Record;
      Gtk.Layered.Waveform.Sweeper.Initialize (Sweeper);
   end Gtk_New;

   procedure Initialize
     (  Sweeper : not null access
          Gtk_Waveform_Sweeper_Record'Class
       )  is
      To : constant Gdouble := To_Double (Ada.Real_Time.Clock);
   begin
      G_New (Sweeper, Get_Type);
      Gtk.Adjustment.Initialize
        (  Adjustment     => Sweeper,
           Value          => To - Gdouble (Def_Page),
           Lower          => To - Gdouble (Def_Page),
           Upper          => To,
           Step_Increment => Gdouble (Def_Page) / 10.0,
           Page_Increment => Gdouble (Def_Page) / 3.0,
           Page_Size      => Gdouble (Def_Page)
          );
      --***
      -- This is a bug in GTK, which manifests itself as not setting all
      -- adjustment parameters upon Inilialize (gtk_adjustment_new). The
      -- workaround is to call Configure yet again in order to force the
      -- parameters. Normally Configure would be not necessary
      --
      Sweeper.all.Configure
        (  Value          => To - Gdouble (Def_Page),
           Lower          => To - Gdouble (Def_Page),
           Upper          => To,
           Step_Increment => Gdouble (Def_Page) / 10.0,
           Page_Increment => Gdouble (Def_Page) / 3.0,
           Page_Size      => Gdouble (Def_Page)
          );
      if Freezing_Changed_ID = Invalid_Signal_Id then
         declare
            Widget_Type : constant GType := Get_Type (Sweeper);
         begin
            Freezing_Changed_ID :=
              Lookup (Widget_Type, "freezing-changed");
         end;
      end if;
   end Initialize;

   function Get_Page_Span
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
       )  return Duration is
   begin
      return Duration (Sweeper.all.Get_Page_Size);
   end Get_Page_Span;

   procedure Set
     (Sweeper      : not null access Gtk_Waveform_Sweeper_Record;
      Date           : Ada.Real_Time.Time;
      From           : Ada.Real_Time.Time;
      To             : Ada.Real_Time.Time;
      Step_Increment : Duration;
      Page_Increment : Duration;
      Page_Span      : Duration) is
   begin
      Sweeper.all.Configure
        (  Value          => To_Double (Date) - Gdouble (Page_Span),
           Lower          => To_Double (From),
           Upper          => To_Double (To),
           Page_Size      => Gdouble (Page_Span),
           Step_Increment => Gdouble (Step_Increment),
           Page_Increment => Gdouble (Page_Increment)
          );
   end Set;

   procedure Set
     (  Sweeper      : not null access Gtk_Waveform_Sweeper_Record;
        Date           : Ada.Calendar.Time;
        From           : Ada.Calendar.Time;
        To             : Ada.Calendar.Time;
        Step_Increment : Duration;
        Page_Increment : Duration;
        Page_Span      : Duration
       )  is
   begin
      Sweeper.all.Configure
        (  Value          => To_Double (Date) - Gdouble (Page_Span),
           Lower          => To_Double (From),
           Upper          => To_Double (To),
           Page_Size      => Gdouble (Page_Span),
           Step_Increment => Gdouble (Step_Increment),
           Page_Increment => Gdouble (Page_Increment)
          );
   end Set;

   procedure Set_Current_Time
     (Sweeper : not null access Gtk_Waveform_Sweeper_Record;
      Stamp   : Ada.Real_Time.Time;
      Active  : Boolean := False)
   is
      Value : constant Gdouble := To_Double (Stamp);
      Upper : constant Gdouble := Sweeper.all.Get_Upper;
      Lower : constant Gdouble := Sweeper.all.Get_Lower;
   begin
      if Active then
         Sweeper.all.Active := Sweeper.all.Active + 1;
      end if;
      if Sweeper.all.Frozen then
         if Lower > Value then
            Sweeper.all.Set_Lower (Value);
         end if;
         if Upper < Value then
            Sweeper.all.Set_Upper (Value);
         end if;
      else
         if Lower > Value then
            Sweeper.all.Set_Lower (Value);
         end if;
         if Upper < Value then
            Sweeper.all.Set_Upper (Value);
            Set_Value (Sweeper, Value - Upper + Sweeper.all.Get_Value);
         end if;
      end if;
      if Active then
         Sweeper.all.Active := Sweeper.all.Active - 1;
      end if;
   end Set_Current_Time;

   overriding function Is_Active
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
       )  return Boolean is
   begin
      return Sweeper.all.Active > 0;
   end Is_Active;

   procedure Set_Frozen
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
        Frozen  : Boolean
       )  is
   begin
      if Sweeper.all.Frozen /= Frozen then
         Sweeper.all.Frozen := Frozen;
         Emit (Sweeper, Freezing_Changed_ID);
      end if;
   end Set_Frozen;

   procedure Set_Page_Span
     (  Sweeper   : not null access Gtk_Waveform_Sweeper_Record;
        Page_Span : Duration
       )  is
      Upper : Gdouble := Sweeper.all.Get_Upper;
      Lower : Gdouble := Sweeper.all.Get_Lower;
      Value : Gdouble := Sweeper.all.Get_Value + Sweeper.all.Get_Page_Size;
      Page  : Gdouble;
   begin
      if Page_Span < Min_Page then
         Page := Gdouble (Min_Page);
      elsif Page_Span > Max_Page then
         Page := Gdouble (Max_Page);
      else
         Page := Gdouble (Page_Span);
      end if;
      Value := Value - Page;
      if Value < Lower then
         Lower := Value;
      end if;
      if Value > Upper then
         Upper := Value;
      end if;
      Sweeper.all.Configure
        (  Value          => Value,
           Lower          => Lower,
           Upper          => Upper,
           Page_Size      => Page,
           Step_Increment => Sweeper.all.Get_Step_Increment,
           Page_Increment => Sweeper.all.Get_Page_Increment
          );
   end Set_Page_Span;

   procedure Set_Time
     (Sweeper : not null access Gtk_Waveform_Sweeper_Record;
      Stamp   : Ada.Real_Time.Time) is
   begin
      Sweeper.Set_Time (To_Double (Stamp));
   end Set_Time;

   procedure Set_Time
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
        Stamp   : Ada.Calendar.Time
       )  is
   begin
      Sweeper.Set_Time (To_Double (Stamp));
   end Set_Time;

   procedure Set_Time
     (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
        Stamp   : Gdouble
       )  is
      Upper : constant Gdouble := Sweeper.all.Get_Upper;
      Lower : constant Gdouble := Sweeper.all.Get_Lower;
      Page  : constant Gdouble := Sweeper.all.Get_Page_Size;
   begin
      if Stamp > Upper then
         Sweeper.all.Set_Value (Upper - Page);
      elsif Stamp < Lower + Page then
         Sweeper.all.Set_Value (Lower);
      else
         Sweeper.all.Set_Value (Stamp - Page);
      end if;
   end Set_Time;

end Gtk.Layered.Waveform.Sweeper;
