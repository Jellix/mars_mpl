--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Columned_Store                     Luebeck            --
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
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
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

with Gtk.Tree_Model.Columned_Store;
use  Gtk.Tree_Model.Columned_Store;

procedure Test_Gtk_Columned_Store is

   Window   : Gtk_Window;
   Base     : Gtk_Tree_Store;
   Columned : Gtk_Columned_Store;
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
      Row  : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
   begin
      Action := Action + 1;
      case Action is
         when 1 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "A");
         when 2 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "B");
         when 3 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "C");
         when 4 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "D");
         when 5 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "E");
         when 6 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "F");
         when 7 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "G");
         when 8 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "H");
         when 9 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "I");
         when 10 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "J");
         when 11 =>
            Append (Base, Row, Null_Iter);
            Set (Base, Row, 0, "K");
         when 12 =>
            Path := Gtk_Tree_Path_New_From_String ("0");
            Row  := Get_Iter (Base, Path);
            Set (Base, Row, 0, "A1");
            Path_Free (Path);
         when 13 =>
            Path := Gtk_Tree_Path_New_From_String ("1");
            Row  := Get_Iter (Base, Path);
            Set (Base, Row, 0, "B1");
            Path_Free (Path);
         when 14 =>
            Path := Gtk_Tree_Path_New_From_String ("2");
            Row  := Get_Iter (Base, Path);
            Set (Base, Row, 0, "C1");
            Path_Free (Path);
         when 15 =>
            Path := Gtk_Tree_Path_New_From_String ("3");
            Row  := Get_Iter (Base, Path);
            Set (Base, Row, 0, "D1");
            Path_Free (Path);
         when 16 =>
            Path := Gtk_Tree_Path_New_From_String ("4");
            Row  := Get_Iter (Base, Path);
            Set (Base, Row, 0, "E1");
            Path_Free (Path);
         when 17 =>
            Path := Gtk_Tree_Path_New_From_String ("5");
            Row  := Get_Iter (Base, Path);
            Set (Base, Row, 0, "F1");
            Path_Free (Path);
         when 18 =>
            Path := Gtk_Tree_Path_New_From_String ("6");
            Row  := Get_Iter (Base, Path);
            Set (Base, Row, 0, "G1");
            Path_Free (Path);
         when 19 =>
            Insert_Before (Base, Row, Null_Iter, Get_Iter_First (Base));
            Set (Base, Row, 0, "8");
         when 20 =>
            Insert_After (Base, Row, Null_Iter, Get_Iter_First (Base));
            Set (Base, Row, 0, "9");
         when 21 =>
            Path := Gtk_Tree_Path_New_From_String ("0");
            Row  := Get_Iter (Base, Path);
            Remove (Base, Row);
            Path_Free (Path);            
         when 22 =>
            Path := Gtk_Tree_Path_New_From_String ("6");
            Row  := Get_Iter (Base, Path);
            Remove (Base, Row);
            Path_Free (Path);            
         when 23 =>
            Path := Gtk_Tree_Path_New_From_String ("7");
            Row  := Get_Iter (Base, Path);
            Row  := To_Columned (Columned, Row);
            Path_Free (Path);            
            declare
               Value  : GValue;
               Result : Unbounded_String;
            begin
               for Column in 1..5 loop
                  Get_Value (Columned, Row, GInt (Column) - 1, Value);
                  Append (Result, Integer'Image (Column));
                  Append (Result, "='");
                  Append (Result, Get_String (Value));
                  Append (Result, "'");
                  Unset (Value);
               end loop;
               Append (Base, Row, Null_Iter);
               Set (Base, Row, 0, To_String (Result));
            end;
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
   Gtk_New (Columned, Base, 5);

   Set_Model (View, To_Interface (Columned));

   Unref (Base);
   Unref (Columned);

   -- Creating columns in the view
   declare
      Column_No : GInt;
      Column    : Gtk_Tree_View_Column;
      Text      : Gtk_Cell_Renderer_Text;
   begin
      Gtk_New (Column);
      Set_Title (Column, "1");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 0);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "2");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 1);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "3");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 2);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "4");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 3);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "5");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 4);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);
   end;
   Set_Policy (Scroller, Policy_Automatic, Policy_Automatic);
   Scroller.Add (View);      
   Window.Add (Scroller);

   View.Show;
   Scroller.Show;
   Window.Show;
   ID := Timeout_Add (500, +Test_Action'Access);
   Gtk.Main.Main;

end Test_Gtk_Columned_Store;
