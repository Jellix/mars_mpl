--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Recent_Manager                     Luebeck            --
--  Test for Gtk.Recent_Manager                    Winter, 2008       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with GLib;                      use GLib;
with GLib.Error;                use GLib.Error;
with Gtk.Button;                use Gtk.Button;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Color_Selection;       use Gtk.Color_Selection;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Combo_Box;             use Gtk.Combo_Box;
with Gtk.Label;                 use Gtk.Label;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Main.Router;           use Gtk.Main.Router;
with Gtk.Recent_Manager_Alt;    use Gtk.Recent_Manager_Alt;
with Gtk.Recent_Manager_Keys;   use Gtk.Recent_Manager_Keys;
with Gtk.Window;                use Gtk.Window;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with GtkAda.Types;              use GtkAda.Types;

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with Gtk.Missed;

procedure Test_Gtk_Recent_Manager is
   Window     : Gtk_Window;
   Grid       : Gtk_Table;
   Label      : Gtk_Label;
   Add_Button : Gtk_Button;
   Del_Button : Gtk_Button;
   Edit       : Gtk_Combo_Box;
   Edit_List  : Gtk_List_Store;
   View       : Gtk_Tree_View;
   Scroller   : Gtk_Scrolled_Window;
   List_Store : Gtk_List_Store;

   type Local_Widget_Callback is access
      procedure (Widget : access Gtk_Widget_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Widget_Callback,
             Cb_Gtk_Widget_Void
          );
   type Local_Button_Callback is access
      procedure (Widget : access Gtk_Button_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Button_Callback,
             Cb_Gtk_Button_Void
          );

   procedure Added (Widget : access Gtk_Button_Record'Class) is
      URI  : constant String := Get_Active_Text (Edit);
      This : Gtk_Tree_Iter;
   begin
      if Add_Item (Get_Default, URI) then
         Insert (Edit_List, This, 0);
         Set (Edit_List, This, 0, URI);
      else
         Say ("Cannot add URI '" & URI & "'");
      end if;
   end Added;

   procedure Deleted (Widget : access Gtk_Button_Record'Class) is
      Row   : Gtk_Tree_Iter;
      Error : GError;
      Model : Gtk_Tree_Model := To_Interface (List_Store);
   begin
      Get_Selected (Get_Selection (View), Model, Row);
      if Row = Null_Iter then
         Say ("Nothing selected");
      else
         Remove_Item
         (  Get_Default,
            Get_String (List_Store, Row, 2),
            Error
         );
         if Error = null then
            Remove (List_Store, Row);
         else
            Say (Get_Message (Error));
            Error_Free (Error);
         end if;
      end if;
   end Deleted;

   procedure Destroy (Widget : access Gtk_Widget_Record'Class) is
   begin
      Store ("URI_Entry", Edit_List, 0);
      Unref (Edit_List);
      Gtk.Main.Main_Quit;
   end Destroy;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title ("Test Recent Manager");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (+Destroy'Access);

   Gtk_New (Grid, 2, 4, False);
   Add (Window, Grid);

   Gtk_New (Scroller);
   Gtk_New (View);
   Add (Scroller, View);
   Gtk_New (Label, "URI");
   Gtk_New (Edit_List, (1 => GType_String));
   Restore ("URI_Entry", Edit_List, 0);
   Gtk_New_With_Entry (Edit);
   Edit.Set_Model (To_Interface (Edit_List));
   Gtk_New (Add_Button, "Add");
   Gtk_New (Del_Button, "Delete");

   Add_Button.On_Clicked (+Added'Access);
   Del_Button.On_Clicked (+Deleted'Access);
--
-- Creating a list store of recent items
--
   declare
      Recent_List : constant Gtk_Recent_Info_Array :=
                             Get_Items (Get_Default);
   begin
      Gtk_New
      (  List_Store,
         (  Gdk.Pixbuf.Get_Type, -- Icon
            GType_String,        -- Short name
            GType_String,        -- URI
            GType_Int,           -- Year
            GType_String,      	 -- Month
            GType_Int,           -- Day
            GType_Int,           -- Hour
            GType_Int,           -- Minures
            GType_Int,           -- Seconds
            GType_String,        -- Registered by
            GType_String,        -- Description
            GType_String         -- Exists
      )  );
      for Index in Recent_List'Range loop
         declare
            Info  : constant Gtk_Recent_Info := Recent_List (Index);
            Row   : Gtk_Tree_Iter;
            Stamp : constant Time       := Get_Added (Info);
            Icon  : constant Gdk_Pixbuf := Get_Icon (Info, 16);
         begin
            Insert (List_Store, Row, GInt'Last);
            Set (List_Store, Row, 0, Icon);
            Unref (Icon);
            Set (List_Store, Row, 1, Get_Short_Name (Info));
            Set (List_Store, Row, 2, Get_URI (Info));
            Set (List_Store, Row, 3, GInt (Year (Stamp)));
            case Month (Stamp) is
               when  1 => Set (List_Store, Row, 4, "Jan");
               when  2 => Set (List_Store, Row, 4, "Feb");
               when  3 => Set (List_Store, Row, 4, "Mar");
               when  4 => Set (List_Store, Row, 4, "Apr");
               when  5 => Set (List_Store, Row, 4, "May");
               when  6 => Set (List_Store, Row, 4, "Jun");
               when  7 => Set (List_Store, Row, 4, "Jul");
               when  8 => Set (List_Store, Row, 4, "Aug");
               when  9 => Set (List_Store, Row, 4, "Sep");
               when 10 => Set (List_Store, Row, 4, "Oct");
               when 11 => Set (List_Store, Row, 4, "Nov");
               when 12 => Set (List_Store, Row, 4, "Dec");
            end case;
            Set (List_Store, Row, 5, GInt (Day (Stamp)));
            Set (List_Store, Row, 6, GInt (Seconds (Stamp) / 3600.0));
            Set (List_Store, Row, 7, GInt (Seconds (Stamp) / 24.0) mod 60);
            Set (List_Store, Row, 8, GInt (Seconds (Stamp)) mod 60);
            declare
               List : Chars_Ptr_Array := Get_Applications (Info);
               Text : Unbounded_String;
            begin
               for Index in List'Range loop
                  if Length (Text) > 0 then
                     Append (Text, ", ");
                  end if;
                  Append
                  (  Text,
                     Interfaces.C.Strings.Value (List (Index))
                  );
               end loop;
               Free (List);
               Set (List_Store, Row, 9, To_String (Text));
            end;
            Set (List_Store, Row, 10, Get_Description (Info));
            if Exists (Info) then
               Set (List_Store, Row, 11, "yes");
            else
               Set (List_Store, Row, 11, "no");
            end if;
            Unref (Info);
         end;
      end loop;
      Set_Model (View, To_Interface (List_Store));
   end;
   --
   -- Creating columns of the tree view
   --
   declare
      Column_No : GInt;
      Column    : Gtk_Tree_View_Column;
      Icon      : Gtk_Cell_Renderer_Pixbuf;
      Text      : Gtk_Cell_Renderer_Text;
   begin
      Gtk_New (Column);
      Set_Title (Column, "Icon");
      Gtk_New (Icon);
      Pack_Start (Column, Icon, True);
      Add_Attribute (Column, Icon, "pixbuf", 0);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Short");
      Gtk_New (Text);
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 1);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "URI");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 2);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Year");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 3);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Month");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 4);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Day");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 5);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Hour");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 6);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Min");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 7);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Sec");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 8);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Registered by");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 9);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Description");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 10);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);

      Gtk_New (Column);
      Set_Title (Column, "Exists");
      Pack_Start (Column, Text, True);
      Add_Attribute (Column, Text, "text", 11);
      Column_No := Append_Column (View, Column);
      Set_Resizable (Column, True);
   end;
   Attach (Grid, Scroller,   0, 4, 0, 1);
   Attach (Grid, Label,      0, 1, 1, 2, XOptions => 0, YOptions => 0);
   Attach (Grid, Edit,       1, 2, 1, 2, YOptions => 0);
   Attach (Grid, Add_Button, 2, 3, 1, 2, XOptions => 0, YOptions => 0);
   Attach (Grid, Del_Button, 3, 4, 1, 2, XOptions => 0, YOptions => 0);
   Show_All (Grid);
   Show (Window);
   -- Enter the events processing loop
   Init (Window);
   Gtk.Main.Main;
end Test_Gtk_Recent_Manager;
