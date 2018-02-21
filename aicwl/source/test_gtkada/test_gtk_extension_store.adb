--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Extension_Store                    Luebeck            --
--  Test                                           Autumn, 2014       --
--                                                                    --
--                                Last revision :  10:06 22 Nov 2014  --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with GLib;                    use GLib;
with Glib.Main;               use Glib.Main;
with Glib.Values;             use Glib.Values;
with Gtk.Enums;               use Gtk.Enums;
with Gdk.Event;               use Gdk.Event;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Tree_View_Column;    use Gtk.Tree_View_Column;
with Gtk.Tree_Store;          use Gtk.Tree_Store;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_View;           use Gtk.Tree_View;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;

with Ada.Unchecked_Conversion;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Missed;

with Gtk.Tree_Model.Extension_Store;
use  Gtk.Tree_Model.Extension_Store;

procedure Test_Gtk_Extension_Store is

   Window   : Gtk_Window;
   Base     : Gtk_Tree_Store;
   Extended : Gtk_Extension_Store;
   View     : Gtk_Tree_View;
   Scroller : Gtk_Scrolled_Window;
   ID       : G_Source_Id;
   Action   : Natural := 0;

   -- Circumvention of access rules, don't do it, it is here only to
   -- simplify the test 
   type Local_Callback is access function return Boolean;
   function "+" is
      new Ada.Unchecked_Conversion (Local_Callback, G_Source_Func);

   function Test_Action return Boolean is
      Value  : GValue;
      Row    : Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
   begin
      Action := Action + 1;
      case Action is
         when 1 =>
            Append (Base, Row, Null_Iter);
            Init (Value, GType_String);
            Set_String (Value, "A");
            Set_Value (Base, Row, 0, Value);
            Unset (Value);
         when 2 =>
            Row := Get_Iter_First (To_Interface (Extended));
            Set_Extension (Extended, Row, 1, "B");
         when 3 =>
            Parent := Get_Iter_First (To_Interface (Base));
            Append (Base, Row, Parent);
            Set (Base, Row, 0, "A1");
         when 4 =>
            Parent := Get_Iter_First (To_Interface (Base));
            Row    := Children (Base, Parent);
            Set_Extension
            (  Extended,
               To_Extension (Extended, Row),
               1,
               "B1"
            );
         when 5 =>
            Path := Gtk_Tree_Path_New_From_String ("0:0");
            Row  := Get_Iter (Extended, Path);
            Path_Free (Path);
            Parent := Gtk.Tree_Model.Parent
                      (  To_Interface (Extended),
                         Row
                      );
            Path := Get_Path (Extended, Parent);
            if "0" /= To_String (Path) then
               raise Data_Error with "Wrong path extended";
            end if;
            Path_Free (Path);
             
            Parent := From_Extension (Extended, Parent);
            Path := Get_Path (Base, Parent);
            if "0" /= To_String (Path) then
               raise Data_Error with "Wrong path base";
            end if;
            Path_Free (Path);

            Append (Base, Row, Parent);
            Set (Base, Row, 0, "A2");
            Set_Extension
            (  Extended,
               To_Extension (Extended, Row),
               1,
               "B2"
            );
         when 6 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "D");
            Set_Extension
            (  Extended,
               To_Extension (Extended, Row),
               1,
               "E"
            );
         when 7 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "F");
            Set_Extension
            (  Extended,
               To_Extension (Extended, Row),
               1,
               "G"
            );
         when 8 =>
            Path := Gtk_Tree_Path_New_From_String ("1");
            Row  := Get_Iter (Base, Path);
            Remove (Base, Row);
            Path_Free (Path);
         when others =>
            return False;
      end case;
      return True;
   exception
      when Error : others =>
         Put_Line
         (  "Error in Test_Action"
         &  Integer'Image (Action)
         &  ": "
         &  Exception_Information (Error)
         );
         Action := Integer'Last;
         return False;
   end Test_Action;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title ("Test Extension Store");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   
   Gtk_New (Scroller);
   Gtk_New (View);
   
   -- Creating the store with some initial records
   Gtk_New (Base, (1 => GType_String));
   Gtk_New (Extended, Base,  (1 => GType_String));

   Set_Model (View, To_Interface (Extended));
   Unref (Base);
   Unref (Extended);

   -- Creating columns in the view
   declare
      Column_No : GInt;
      Column    : Gtk_Tree_View_Column;
      Text      : Gtk_Cell_Renderer_Text;
   begin
      Gtk_New (Column);
      Set_Title (Column, "Base");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 0);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Extension");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 1);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

   end;
   Set_Policy (Scroller, Policy_Automatic, Policy_Automatic);
   Scroller.Add (View);      
   Window.Add (Scroller);

   View.Show;
   Scroller.Show;
   Window.Show;
   ID := Timeout_Add (1000, +Test_Action'Access);
   Gtk.Main.Main;

end Test_Gtk_Extension_Store;
