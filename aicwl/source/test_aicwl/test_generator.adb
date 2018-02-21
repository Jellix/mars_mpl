--                                                                    --
--  package Test_Generator          Copyright (c)  Dmitry A. Kazakov  --
--  Test waveform generator                        Luebeck            --
--                                                 Spring, 2011       --
--                                                                    --
--                                Last revision :  09:44 08 Oct 2016  --
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

with Ada.Real_Time;         use Ada.Real_Time;
with GLib;                  use GLib;
with GLib.Properties;       use GLib.Properties;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Label;             use Gtk.Label;
with Gtk.Layered.Waveform;  use Gtk.Layered.Waveform;
with Gtk.Missed;            use Gtk.Missed;
with Interfaces.C;          use Interfaces.C;
with Strings_Edit.Floats;   use Strings_Edit.Floats;

with Ada.Numerics.Long_Elementary_Functions;
with Ada.Unchecked_Deallocation;

package body Test_Generator is
   use Ada.Numerics.Long_Elementary_Functions;

   procedure Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Wave   : Wave_Generator
             )  is
   begin
      Wave.Worker.Set
      (  Duration (Value (Get_Text (Wave.Period_Edit))),
         Duration (Value (Get_Text (Wave.Refresh_Edit)))
      );
   exception
      when others =>
         null;
   end Changed;

   procedure Changed_Shape
             (  Widget : access Gtk_Widget_Record'Class;
                Wave   : Wave_Generator
             )  is
   begin
      Wave.Shape :=
         Wave_Shape'Val (Wave.Shape_Combo.Get_Active);
   exception
      when others =>
         null;
   end Changed_Shape;

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Wave   : Wave_Generator
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Generator,
                Generator_Ptr
             );
   begin
      if Wave.Worker /= null then
         Wave.Worker.Stop;
         while not Wave.Worker'Terminated loop
            delay 0.001;
         end loop;
         Free (Wave.Worker);
      end if;
      Wave.Buffer.Unref;
   end Destroy;

   function Get_Source
            (  Wave : not null access Wave_Generator_Record
            )  return not null access Waveform_Data_Source'Class is
   begin
      return Wave.Buffer.all'Unchecked_Access;
   end Get_Source;

   function Get_Buffer
            (  Wave : not null access Wave_Generator_Record
            )  return not null access
                      Gtk_Wavefrom_Ring_Data_Buffer_Record'Class is
   begin
      return Wave.Buffer;
   end Get_Buffer;

   procedure Gtk_New
             (  Wave   : out Wave_Generator;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Size   : Positive := 10_100;
                Period : Duration := 0.01
             )  is
   begin
      Wave := new Wave_Generator_Record;
      Initialize (Wave, Shape, Cycle, Size, Period);
   end Gtk_New;

   procedure Gtk_New
             (  Wave   : out Wave_Generator;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Buffer : not null access
                         Gtk_Wavefrom_Ring_Data_Buffer_Record'Class;
                Period : Duration := 0.01
             )  is
   begin
      Wave := new Wave_Generator_Record;
      Initialize (Wave, Shape, Cycle, Buffer, Period);
   end Gtk_New;

   procedure Init
             (  Wave   : not null access Wave_Generator_Record'Class;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Period : Duration
             )  is
      Label : Gtk_Label;
   begin
      Initialize (Wave, 3, 3, False);
      Wave.Set_Row_Spacings (3);
      Wave.Set_Col_Spacings (3);
         -- Row 1
      Gtk_New (Label, "Shape");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Attach
      (  Wave,
         Label,
         0, 1, 0, 1,
         XOptions => Fill,
         YOptions => Shrink
      );
      Gtk_New (Wave.Shape_Combo);
      Wave.Shape_Combo.Append_Text ("Constant");
      Wave.Shape_Combo.Append_Text ("Modulated");
      Wave.Shape_Combo.Append_Text ("Sine");
      Wave.Shape_Combo.Append_Text ("Saw");
      Wave.Shape_Combo.Append_Text ("Square");
      Wave.Shape_Combo.Append_Text ("Triangle");
      Wave.Shape_Combo.Set_Active (Wave_Shape'Pos (Shape));
      Attach
      (  Wave,
         Wave.Shape_Combo,
         1, 3, 0, 1,
         XOptions => Fill,
         YOptions => Shrink
      );
         -- Row 2
      Gtk_New (Label, "Cycle");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Attach
      (  Wave,
         Label,
         0, 1, 1, 2,
         XOptions => Fill,
         YOptions => Shrink
      );
      Gtk_New (Wave.Period_Edit);
      Wave.Period_Edit.Set_Width_Chars (6);
      if Find_Property (Wave.Period_Edit, "max-width-chars") /= null
      then
         Set_Property
         (  Wave.Period_Edit,
            Build ("max-width-chars"),
            GInt'(6)
         );
      end if;
      Set_Text (Wave.Period_Edit, Image (Float (Cycle)));
      Attach
      (  Wave,
         Wave.Period_Edit,
         1, 2, 1, 2,
         YOptions => Shrink
      );
      Gtk_New (Label, "s");
      Attach
      (  Wave,
         Label,
         2, 3, 1, 2,
         XOptions => Shrink,
         YOptions => Shrink
      );
         -- Row 3
      Gtk_New (Label, "Rate");
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
      Attach
      (  Wave,
         Label,
         0, 1, 2, 3,
         XOptions => Fill,
         YOptions => Shrink
      );
      Gtk_New (Wave.Refresh_Edit);
      Wave.Refresh_Edit.Set_Width_Chars (6);
      if Find_Property (Wave.Refresh_Edit, "max-width-chars") /= null
      then
         Set_Property
         (  Wave.Refresh_Edit,
            Build ("max-width-chars"),
            GInt'(6)
         );
      end if;
      Set_Text (Wave.Refresh_Edit, Image (Float (Period)));
      Attach
      (  Wave,
         Wave.Refresh_Edit,
         1, 2, 2, 3,
         YOptions => Shrink
      );
      Gtk_New (Label, "s");
      Attach
      (  Wave,
         Label,
         2, 3, 2, 3,
         XOptions => Shrink,
         YOptions => Shrink
      );

      Handlers.Connect
      (  Wave,
         "destroy",
         Destroy'Access,
         Wave.all'Unchecked_Access
      );
      Handlers.Connect
      (  Wave.Period_Edit,
         "changed",
         Changed'Access,
         Wave.all'Unchecked_Access
      );
      Handlers.Connect
      (  Wave.Refresh_Edit,
         "changed",
         Changed'Access,
         Wave.all'Unchecked_Access
      );
      Handlers.Connect
      (  Wave.Shape_Combo,
         "changed",
         Changed_Shape'Access,
         Wave.all'Unchecked_Access
      );
      Wave.Shape := Shape;
   end Init;

   procedure Initialize
             (  Wave   : not null access Wave_Generator_Record'Class;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Size   : Positive;
                Period : Duration
             )  is
   begin
      Init (Wave, Shape, Cycle, Period);
      Gtk_New (Wave.Buffer, Size);
      Wave.Worker := new Generator (Wave.all'Unchecked_Access);
      Wave.Worker.Set (Cycle, Period);
   end Initialize;

   procedure Initialize
             (  Wave   : not null access Wave_Generator_Record'Class;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Buffer : not null access
                         Gtk_Wavefrom_Ring_Data_Buffer_Record'Class;
                Period : Duration
             )  is
   begin
      Init (Wave, Shape, Cycle, Period);
      Wave.Buffer := Buffer.all'Unchecked_Access;
      Wave.Buffer.Ref;
      Wave.Worker := new Generator (Wave.all'Unchecked_Access);
      Wave.Worker.Set (Cycle, Period);
   end Initialize;

   task body Generator is
      Cycle  : Long_Float := 1.0;
      Period : Duration   := 100.0;
      X      : GDouble;
   begin
      loop
         select
            accept Stop;
            exit;
         or accept Set (Cycle, Period : Duration) do
               Generator.Cycle  := Long_Float (Cycle);
               Generator.Period := Period;
            end Set;
            if Period < 0.001 then
               Period := 0.001;
            end if;
            if Cycle < 0.001 then
               Cycle := 0.001;
            end if;
         or delay Period;
            X := To_Double (Clock);
            case Wave.Shape is
               when Modulated =>
                  Wave.Buffer.Put
                  (  T => X_Axis (X),
                     V => Y_Axis
                          (  sin (Long_Float (X), Cycle / 5.7)
                          *  sin (Long_Float (X), Cycle * 4.3)
                  )       );
               when Sine =>
                  Wave.Buffer.Put
                  (  T => X_Axis (X),
                     V => Y_Axis (sin (Long_Float (X), Cycle))
                  );
               when Saw =>
                  Wave.Buffer.Put
                  (  T => X_Axis (X),
                     V => (  2.0
                          *  Y_Axis'Remainder
                             (  Y_Axis (X),
                                Y_Axis (Cycle)
                             )
                          /  Y_Axis (Cycle)
                  )       );
               when Triangle =>
                  Wave.Buffer.Put
                  (  T => X_Axis (X),
                     V => (  4.0
                          *  abs Y_Axis'Remainder
                                 (  Y_Axis (X),
                                    Y_Axis (Cycle)
                                 )
                          /  Y_Axis (Cycle)
                          -  1.0
                  )       );
               when Square =>
                  if (  Y_Axis'Remainder (Y_Axis (X), Y_Axis (Cycle))
                     >= 0.0
                     )
                  then
                     Wave.Buffer.Put (T => X_Axis (X), V => 1.0);
                  else
                     Wave.Buffer.Put (T => X_Axis (X), V => 0.0);
                  end if;
               when Const =>
                  Wave.Buffer.Put
                  (  T => X_Axis (X),
                     V => Y_Axis (0.0)
                  );
            end case;
         end select;
      end loop;
   end Generator;

   procedure Set_Shape
             (  Wave  : not null access Wave_Generator_Record;
                Shape : Wave_Shape
             )  is
   begin
      Wave.Shape_Combo.Set_Active (Wave_Shape'Pos (Shape));
   end Set_Shape;

end Test_Generator;
