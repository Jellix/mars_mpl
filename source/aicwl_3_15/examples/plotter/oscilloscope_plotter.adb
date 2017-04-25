--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Oscilloscope_Plotter                        Luebeck            --
--  Plotting using oscilloscope                    Winter, 2012       --
--                                                                    --
--                                Last revision :  15:58 22 Jan 2012  --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Glib;                  use Glib;
with Gtk.Missed;            use Gtk.Missed;
with Gdk.Event;             use Gdk.Event;
with Gtk.Layered.Waveform;  use Gtk.Layered.Waveform;
with Gtk.Oscilloscope;      use Gtk.Oscilloscope;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;

with Ada.Numerics.Elementary_Functions;
with Gtk.Main;

procedure Oscilloscope_Plotter is
   Window : Gtk_Window;
begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title ("Test plotting");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

   declare
      Curve        : Channel_Number;
      Oscilloscope : Gtk_Oscilloscope;
   begin
      Gtk_New (Oscilloscope);
      Add (Window, Oscilloscope);
      Oscilloscope.Set_Manual_Sweep (False);
      --
      -- Configuring the lower axis
      --
      Oscilloscope.Set_Frozen     (Lower, True);  -- No sweeping
      Oscilloscope.Set_Time_Scale (Lower, False); -- No scale (slider)
      Oscilloscope.Set_Time_Grid  (Lower, True);  -- Grid
      Oscilloscope.Set_Time_Axis
      (  Lower,
         True,  -- Visible
         False  -- As plain numbers
      );
      Oscilloscope.Get_Sweeper (Lower).Configure
      (  Value => 0.0,
         Lower => 0.0,
         Upper => 20.0,
         Step_Increment => 0.1,
         Page_Increment => 5.0,
         Page_Size      => 10.0
      );
      --
      -- Adding the channel
      --
      Curve :=
         Add_Channel
         (  Widget  => Oscilloscope,
            Mode    => Gtk.Layered.Linear, -- Linear interpolation
            Color   => RGB (0.0, 0.0, 0.7),
            Sweeper => Lower
         );
      --
      -- Configuring the left axis for this channel (and its group)
      --
      Oscilloscope.Set_Group (Left, Oscilloscope.Get_Group (Curve));
      Oscilloscope.Set_Values_Axis  (Left, True);
      Oscilloscope.Set_Values_Scale (Left, False);
      Oscilloscope.Set_Values_Grid  (Left, True);
      Oscilloscope.Set_Values_Axis_Width (Left, 60);
      --
      -- Pushing the data into the channel's buffer
      --
      declare
         use Ada.Numerics.Elementary_Functions;
         X : Float := 0.0;
      begin
         loop
            Oscilloscope.Feed
            (  Channel => Curve,
               T => GDouble (X),
               V => GDouble (sin (X * 7.0) * exp (X))
            );
            X := X + 0.001;
            exit when X > 10.0;
         end loop;
      end;
   end;
   Window.Set_Size_Request (400, 300);
   Show_All (Window);
   Gtk.Main.Main;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Oscilloscope_Plotter;
