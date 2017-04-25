--                                                                    --
--  procedure Test_Gtk_Button       Copyright (c)  Dmitry A. Kazakov  --
--  Test for                                       Luebeck            --
--      Gtk.Generic_Style_Button                   Spring, 2007       --
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

with GtkAda.Handlers;         use GtkAda.Handlers;
with GtkAda.Style;            use GtkAda.Style;
with Gtk.Window;              use Gtk.Window;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Table;               use Gtk.Table;
with Gtk.Tooltip;             use Gtk.Tooltip;
with Test_Gtk_Custom_Button;  use Test_Gtk_Custom_Button;

with Ada.Text_IO;
with Gtk.Main;
with Gtk.Main.Router;
with Gtk.Missed;
with Gtk.Style_Provider;

procedure Test_Gtk_Button is
   --
   -- All data are global, for the sake of  simplicity.
   --
   Window : Gtk_Window;
   Grid   : Gtk_Table;
   Button : Gtk_Style_Button;

begin
   Gtk.Main.Init;
   Load_Css_File
   (  "test_gtk_button.css",
      Ada.Text_IO.Put_Line'Access,
      Gtk.Style_Provider.Priority_User
   );
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Title ("Test Style Buttons");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

   Gtk_New (Grid, 1, 2, False);
   Window.Add (Grid);
   Gtk_New (Button);
   Button.Set_Name ("Button1");
   Grid.Attach (Button, 0, 1, 0, 1);
   Button.Show;

   Gtk_New (Button);
   Button.Set_Name ("Button2");
   Grid.Attach (Button, 0, 1, 1, 2);
   Button.Show;

   Grid.Show;
   Window.Show;

   Gtk.Main.Main;
end Test_Gtk_Button;
