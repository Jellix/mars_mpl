--                                                                    --
--  procedure Test_Oscilloscope     Copyright (c)  Dmitry A. Kazakov  --
--  Test oscilloscope                              Luebeck            --
--                                                 Summer, 2011       --
--                                                                    --
--                                Last revision :  18:50 05 Apr 2016  --
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

with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Text_IO;       use Ada.Text_IO;
with Gdk.Event;         use Gdk.Event;
with Gtk.Box;           use Gtk.Box;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Oscilloscope;  use Gtk.Oscilloscope;
with Gtk.Paned;         use Gtk.Paned;
with Gtk.Table;         use Gtk.Table;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Test_Generator;    use Test_Generator;

with Gtk.Layered.Waveform;
with Gtk.Handlers;
with Gtk.Main.Router.GNAT_Stack;
with Gtk.Missed;
with Gtk.Oscilloscope.Amplifier_Panel;
with Gtk.Oscilloscope.Channels_Panel;
with Gtk.Oscilloscope.Sweeper_Panel;

procedure Test_Oscilloscope is
   use Gtk.Oscilloscope.Amplifier_Panel;
   use Gtk.Oscilloscope.Channels_Panel;
   use Gtk.Oscilloscope.Sweeper_Panel;

   Refresh_Rate : constant Duration := 0.02;

   Window       : Gtk_Window;
   Oscilloscope : Gtk_Oscilloscope;

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

   declare
      Pane      : Gtk_HBox;
      Panels    : Gtk_Table;
      Frame     : Gtk_Frame;
      Generator : Wave_Generator;
      Channel_1 : Channel_Number;
      Channel_2 : Channel_Number;
      Amplifier : Gtk_Oscilloscope_Amplifier_Panel;
      Channels  : Gtk_Oscilloscope_Channels_Panel;
      Sweeper   : Gtk_Oscilloscope_Sweeper_Panel;
   begin
      Gtk_New_HBox (Pane);
      Pane.Set_Spacing (3);
      Window.Add (Pane);

      Gtk_New
      (  Widget         => Oscilloscope,
         Refresh_Period => Refresh_Rate
      );
      Oscilloscope.Set_Snapshot_File (SVG_Snapshot, "oscilloscope.svg");
--    Oscilloscope.Set_Snapshot_File (PDF_Snapshot, "oscilloscope.pdf");
--    Oscilloscope.Set_Selection_Mode (Zoom_In_Time);
      declare
         Channel : Channel_Number;
      begin
         Channel_1 :=
            Add_Channel
            (  Widget => Oscilloscope,
               Mode   => Gtk.Layered.Left,
               Name   => "Left generator"
            );
         Channel_2 :=
            Add_Channel
            (  Widget => Oscilloscope,
               Mode   => Gtk.Layered.Left,
               Name   => "Right generator"
            );
         Channel :=
            Add_Deviation_Channel
            (  Widget   => Oscilloscope,
               Measured => Refresh_Period
            );
--           Channel :=
--              Add_Shadow_Channel
--              (  Widget  => Oscilloscope,
--                 Channel => Channel
--              );
         Channel :=
            Add_Deviation_Channel
            (  Widget   => Oscilloscope,
               Group    => Oscilloscope.Get_Group (Channel),
               Measured => Drawing_Time
            );
--           Channel :=
--              Add_Shadow_Channel
--              (  Widget  => Oscilloscope,
--                 Channel => Channel
--              );
      end;
      Gtk_New (Frame);
      Pane.Pack_Start (Frame);
      Frame.Set_Border_Width (3);
      Frame.Add (Oscilloscope);
      Frame.Set_Size_Request (300, 210);

      Gtk_New (Panels, 4, 3, False);
      Panels.Set_Col_Spacings (3);
      Panels.Set_Row_Spacings (3);
      Pane.Pack_Start (Panels, False, False);

         -- Left generator
         Gtk_New (Frame, "Generator");
         Panels.Attach (Frame, 0, 1, 0, 1, YOptions => Shrink or Fill);
         Gtk_New
         (  Wave   => Generator,
            Shape  => Saw,
            Cycle  => 10.0,
            Period => 0.01,
            Buffer => Oscilloscope.Get_Buffer (Channel_1)
         );
         Generator.Set_Border_Width (3);
         Frame.Add (Generator);

         -- Right generator
         Gtk_New (Frame, "Generator");
         Panels.Attach (Frame, 2, 3, 0, 1, YOptions => Shrink or Fill);
         Gtk_New
         (  Wave   => Generator,
            Shape  => Sine,
            Cycle  => 7.0,
            Period => 0.01,
            Buffer => Oscilloscope.Get_Buffer (Channel_2)
         );
         Generator.Set_Border_Width (3);
         Frame.Add (Generator);

         -- Sweepers
         Gtk_New (Frame, "Upper sweeper");
         Panels.Attach (Frame, 1, 2, 0, 1, YOptions => Shrink or Fill);
         Gtk_New (Sweeper, Oscilloscope, Upper);
         Sweeper.Set_Border_Width (3);
         Frame.Add (Sweeper);

         Gtk_New (Frame, "Lower sweeper");
         Panels.Attach (Frame, 1, 2, 2, 3, YOptions => Shrink or Fill);
         Gtk_New (Sweeper, Oscilloscope, Lower);
         Sweeper.Set_Border_Width (3);
         Frame.Add (Sweeper);

         -- Amplifiers
         Gtk_New (Frame, "Left amplifier");
         Panels.Attach (Frame, 0, 1, 1, 2, YOptions => Shrink or Fill);
         Gtk_New (Amplifier, Oscilloscope, Left);
         Amplifier.Set_Border_Width (3);
         Frame.Add (Amplifier);

         Gtk_New (Frame, "Middle amplifier");
         Panels.Attach (Frame, 1, 2, 1, 2, YOptions => Shrink or Fill);
         Gtk_New (Amplifier, Oscilloscope, Middle);
         Amplifier.Set_Border_Width (3);
         Frame.Add (Amplifier);

         Gtk_New (Frame, "Right amplifier");
         Panels.Attach (Frame, 2, 3, 1, 2, YOptions => Shrink or Fill);
         Gtk_New (Amplifier, Oscilloscope, Right);
         Amplifier.Set_Border_Width (3);
         Frame.Add (Amplifier);

         -- Channels
         Gtk_New (Frame, "Channels");
         Panels.Attach (Frame, 0, 3, 3, 4);
         Gtk_New (Channels, Oscilloscope);
         Channels.Set_Border_Width (3);
         Frame.Add (Channels);

   end;
   Window.Show_All;
   Gtk.Main.Main;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Test_Oscilloscope;
