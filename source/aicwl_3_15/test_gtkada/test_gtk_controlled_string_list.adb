--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Controlled_String_List             Luebeck            --
--  A test for                                     Spring, 2007       --
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

with GLib;                    use GLib;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Window;              use Gtk.Window;
with Gtk.Combo_Box_Text;      use Gtk.Combo_Box_Text;
with Gtk.Enums.String_Lists;  use Gtk.Enums.String_Lists;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Table;               use Gtk.Table;

with Gtk.Handlers;
with Gtk.Main;
with Gtk.Missed;

procedure Test_Gtk_Controlled_String_List is
   --
   -- All data are global, for the sake of  simplicity
   --
   Window : Gtk_Window;
   Grid   : Gtk_Table;
   Combo  : Gtk_Combo_Box_Text;

   procedure Set_Texts
             (  Combo : not null access Gtk_Combo_Box_Text_Record'Class;
                List  : Gtk.Enums.String_List.GList
             )  is
      use Gtk.Enums.String_List;
      Item : Gtk.Enums.String_List.GList;
   begin
      for N in GUInt loop
         Item := Nth (List, N);
         exit when Item = Null_List;
         Combo.Append_Text (Get_Data (Item));
      end loop;
   end Set_Texts;
begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title ("Test Controlled String Lists");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Gtk_New (Grid, 1, 2, False);

   Gtk_New (Combo);
   Set_Texts (Combo, +"A"/"B"/"C");

   Window.Add (Grid);
   Grid.Attach (Combo, 0, 1, 0, 1);
   Window.Show_All;
   Gtk.Main.Main;
end Test_Gtk_Controlled_String_List;
