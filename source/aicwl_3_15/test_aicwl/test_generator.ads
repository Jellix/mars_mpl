--                                                                    --
--  package Test_Generator          Copyright (c)  Dmitry A. Kazakov  --
--  Test waveform generator                        Luebeck            --
--                                                 Spring, 2011       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Table;           use Gtk.Table;
with Gtk.Widget;          use Gtk.Widget;

with Gtk.Layered.Waveform.Ring_Data_Buffer;
with Gtk.Handlers;

package Test_Generator is
   use Gtk.Layered.Waveform;
   use Gtk.Layered.Waveform.Ring_Data_Buffer;

   type Wave_Generator_Record is new Gtk_Table_Record with private;
   type Wave_Generator is access all Wave_Generator_Record'Class;
   type Wave_Shape is (Const, Modulated, Sine, Saw, Square, Triangle);

   function Get_Buffer
            (  Wave : not null access Wave_Generator_Record
            )  return not null access
                      Gtk_Wavefrom_Ring_Data_Buffer_Record'Class;
   function Get_Source
            (  Wave : not null access Wave_Generator_Record
            )  return not null access Waveform_Data_Source'Class;
   procedure Gtk_New
             (  Wave   : out Wave_Generator;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Size   : Positive := 10_100;
                Period : Duration := 0.01
             );
   procedure Gtk_New
             (  Wave   : out Wave_Generator;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Buffer : not null access
                         Gtk_Wavefrom_Ring_Data_Buffer_Record'Class;
                Period : Duration := 0.01
             );
   procedure Initialize
             (  Wave   : not null access Wave_Generator_Record'Class;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Size   : Positive;
                Period : Duration
             );
   procedure Initialize
             (  Wave   : not null access Wave_Generator_Record'Class;
                Shape  : Wave_Shape;
                Cycle  : Duration;
                Buffer : not null access
                         Gtk_Wavefrom_Ring_Data_Buffer_Record'Class;
                Period : Duration
             );
   procedure Set_Shape
             (  Wave  : not null access Wave_Generator_Record;
                Shape : Wave_Shape
             );
private
   task type Generator
             (  Wave : not null access Wave_Generator_Record'Class
             )  is
      entry Set (Cycle, Period : Duration);
      entry Stop;
   end Generator;
   type Generator_Ptr is access Generator;

   type Wave_Generator_Record is new Gtk_Table_Record with record
      Shape        : Wave_Shape;
      Buffer       : Gtk_Wavefrom_Ring_Data_Buffer;
      Worker       : Generator_Ptr;
      Period_Edit  : Gtk_Entry;
      Refresh_Edit : Gtk_Entry;
      Shape_Combo  : Gtk_Combo_Box_Text;
      pragma Atomic (Shape);
   end record;

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Wave_Generator
          );

end Test_Generator;
