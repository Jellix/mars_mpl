--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--      Test_Oscilloscope_Stacked                  Luebeck            --
--  Test oscilloscopes stacked up                  Summer, 2011       --
--  into a rack                                                       --
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
with Ada.Numerics.Float_Random;   use Ada.Numerics.Float_Random;
with Ada.Text_IO;                 use Ada.Text_IO;
with Gdk.Color;                   use Gdk.Color;
with Gdk.Color.IHLS;              use Gdk.Color.IHLS;
with Gdk.Event;                   use Gdk.Event;
with GLib;                        use GLib;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Drawing_Area;            use Gtk.Drawing_Area;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Label;                   use Gtk.Label;
with Gtk.Layered.Refresh_Engine;  use Gtk.Layered.Refresh_Engine;
with Gtk.Missed;                  use Gtk.Missed;
with Gtk.Notebook;                use Gtk.Notebook;
with Gtk.Oscilloscope;            use Gtk.Oscilloscope;
with Gtk.Table;                   use Gtk.Table;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Window;                  use Gtk.Window;
with Test_Generator;              use Test_Generator;

with Gtk.Layered.Waveform;
with Gtk.Handlers;
with Gtk.Main.Router.GNAT_Stack;

procedure Test_Oscilloscope_Stack is
   Stack_Height : constant := 5;  -- Number of oscilloscopes
   Axis_Height  : constant := 20; -- Lower horizontal axis height
   Axis_Width   : constant := 35; -- Left vertical axis width
   Period       : constant Duration  := 0.02; -- Refresh period
   Rate         : constant Duration  := 0.01; -- Generator rate
   First        : constant Gdk_Color := RGB (1.00, 0.00, 0.00);
   Background   : constant Gdk_Color := RGB (0.00, 0.00, 0.00);
   Major_Line   : constant Gdk_Color := RGB (0.22, 0.22, 0.22);
   Minor_Line   : constant Gdk_Color := RGB (0.12, 0.12, 0.12);
   Window       : Gtk_Window;
   Engine       : aliased Layered_Refresh_Engine;

begin
   Gtk.Main.Init;
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("Gtk");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GLib-GObject");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GtkAda+");
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Title ("Test Ada industrial control widget library");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

   Engine.Set_Period (Period);
   declare
      Dice   : Ada.Numerics.Float_Random.Generator;
      Pages  : Gtk_Notebook;
      Grid   : Gtk_Table;
      Scaler : Gtk_Oscilloscope;
   begin
      Gtk_New (Grid, Stack_Height + 1, 2, False);
      Grid.Set_Col_Spacings (3);
      Grid.Set_Row_Spacings (3);
      if True then
         Gtk_New (Pages);
         Window.Add (Pages);
         declare
            Label : Gtk_Label;
            Box   : Gtk_HBox;
         begin
            Gtk_New (Box, Orientation_Horizontal, 3);
            Label := Gtk_Label_New ("Test");
            Box.Pack_Start (Label);
            Box.Show_All;
            Pages.Append_Page (Grid, Box);
         end;
      else
         Window.Add (Grid);
      end if;
      --
      -- Create the lowest  oscilloscope  used to sync  all others,  but
      -- showing no signal by itself.  Left on the oscilloscope a box is
      -- placed to reserve Scale_Width space.
      --
      declare
         Box    : Gtk_HBox;
         Filler : Gtk_Drawing_Area;
      begin
         Gtk_New_HBox (Box);
         Box.Set_App_Paintable (True);
         Gtk_New (Filler);
         Filler.Set_Size_Request (Axis_Width, Axis_Height);
         Box.Pack_Start (Filler, False, False);
         Gtk_New
         (  Widget         => Scaler,
            Refresh_Engine => Engine'Access
         );
         Scaler.Set_Time_Axis_Height (Lower, Axis_Height);
         Scaler.Set_Time_Axis (Lower, True);
         Box.Pack_Start (Scaler);
         Grid.Attach
         (  Child         => Box,
            Left_Attach   => 0,
            Right_Attach  => 1,
            Top_Attach    => Stack_Height,
            Bottom_Attach => Stack_Height + 1,
            Yoptions      => Shrink
         );
      end;
      for Slot in GUInt range 1..Stack_Height loop
         declare
            Oscilloscope : Gtk_Oscilloscope;
            Generator    : Wave_Generator;
            Channel      : Channel_Number;
         begin
            -- Create a generator for this slot
            Gtk_New
            (  Wave   => Generator,
               Cycle  => 3.0 + 4.0 * Duration (Random (Dice)),
               Period => Rate,
               Shape  => Wave_Shape'Val
                         (  Slot
                         mod
                            (Wave_Shape'Pos (Wave_Shape'Last) + 1)
            )            );
            Grid.Attach
            (  Child         => Generator,
               Left_Attach   => 1,
               Right_Attach  => 2,
               Top_Attach    => Slot - 1,
               Bottom_Attach => Slot,
               Xoptions      => Shrink
            );
            -- Create an oscilloscope for this slot
            Gtk.Oscilloscope.Gtk_New
            (  Widget         => Oscilloscope,
               Background     => Background,
               Lower_Sweeper  => Scaler.Get_Sweeper (Lower),
               Refresh_Engine => Engine'Access
            );
            Grid.Attach
            (  Child         => Oscilloscope,
               Left_Attach   => 0,
               Right_Attach  => 1,
               Top_Attach    => Slot - 1,
               Bottom_Attach => Slot
            );
            Oscilloscope.Set_Size_Request (300, 50);
            Channel :=
               Oscilloscope.Add_Channel
               (  Buffer  => Generator.Get_Buffer,
                  Sweeper => Lower,
                  Color   =>
                     To_RGB
                     (  Val
                        (  First => To_IHLS (First),
                           Pos   => Natural (Slot - 1),
                           Cycle => 6
               )     )  );
            Oscilloscope.Set_Group
            (  Left,
               Oscilloscope.Get_Group (Channel)
            );
            Oscilloscope.Set_Values_Axis (Left, True);
            Oscilloscope.Set_Values_Axis_Width (Left, Axis_Width);
            -- Disable any scales
            Oscilloscope.Set_Time_Scale   (Lower, False);
            Oscilloscope.Set_Time_Scale   (Upper, False);
            Oscilloscope.Set_Values_Scale (Left,  False);
            Oscilloscope.Set_Values_Scale (Right, False);
            Oscilloscope.Set_Grid_Colors  (Major_Line, Minor_Line);
         end;
      end loop;
   end;
   Window.Show_All;
   Gtk.Main.Main;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Oscilloscope_Stack;
