--                                                                    --
--  procedure Test_Gtk_Tasking      Copyright (c)  Dmitry A. Kazakov  --
--  Test for Gtk.Main.Router                       Luebeck            --
--                                                 Spring, 2006       --
--                                                                    --
--                                Last revision :  09:54 04 Feb 2017  --
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

with Ada.Exceptions;   use Ada.Exceptions;
with Gtk.Main.Router;  use Gtk.Main.Router;
with Gtk.Window;       use Gtk.Window;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Table;        use Gtk.Table;
with Gtk.Label;        use Gtk.Label;

with Ada.Unchecked_Conversion;
with Gtk.Missed;

procedure Test_Gtk_Tasking is
   --
   -- All data are global, for the sake of  simplicity
   --
   Window  : Gtk_Window;
   Grid    : Gtk_Table;
   Label   : Gtk_Label;
   Counter : Integer;

   -- Circumvention of access rules, don't do it, it is here only to
   -- simplify the test
   type Local_Callback is access procedure;
   function "+" is
      new Ada.Unchecked_Conversion (Local_Callback, Gtk_Callback);

   task type Process;

   -- Update will write the label
   procedure Update is
   begin
      Label.Set_Text ("Counter" & Integer'Image (Counter));
   end Update;

   -- The task that calls to Update
   task body Process is
   begin
      for Index in Positive'Range loop
         Counter := Index;
         Request (+Update'Access); -- Request execution of Update
         delay 0.5;
      end loop;
   exception
      when Quit_Error => -- Main loop was quitted, we follow
         null;
      when Error : others =>
         Say (Exception_Information (Error)); -- This is safe
   end Process;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window); -- This must be called once
   Window.Set_Title ("Test Tasking");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Gtk_New (Grid, 1, 1, False);
   Window.Add (Grid);
   Gtk_New (Label, "label");
   Grid.Attach (Label, 0, 1, 0, 1);

   Label.Show;
   Grid.Show;
   Window.Show;
   declare
      Worker : Process; -- Now the task is on
   begin
      -- Enter the events processing loop
      Gtk.Main.Main;
   end;
exception
   when Error : others =>
      Say (Exception_Information (Error)); -- This is safe
end Test_Gtk_Tasking;
