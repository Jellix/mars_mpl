--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Persistent_Directory_Browser       Luebeck            --
--  Test for                                       Winter, 2008       --
--                                                                    --
--                                Last revision :  22:06 23 Jul 2014  --
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
--
--  This test uses Simple Components and GNADE ODBC bindings.
--
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Gdk.Event;               use Gdk.Event;
with GLib;                    use GLib;
with GLib.Messages;           use GLib.Messages;
with GLib.Values;             use GLib.Values;
with GNAT.Exception_Actions;  use GNAT.Exception_Actions;
with GtkAda.Dialogs;          use GtkAda.Dialogs;
with GtkAda.Handlers;         use GtkAda.Handlers;
with Gtk.Abstract_Browser;    use Gtk.Abstract_Browser;
with Gtk.Editable;            use Gtk.Editable;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Radio_Button;        use Gtk.Radio_Button;
with Gtk.Image_Button;        use Gtk.Image_Button;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main.Router;         use Gtk.Main.Router;
with Gtk.Paned;               use Gtk.Paned;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Window;              use Gtk.Window;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Persistent.Handle;       use Persistent.Handle;

with Ada.Unchecked_Conversion;
with Glib.Object;
with GNAT.Traceback.Symbolic;
with Gtk.Button;
with Gtk.Main.Router;
with Gtk.Missed;
with Gtk.RC;
with Gtk.Persistent_Storage_Credentials_Dialog.ODBC;

with Gtk.Persistent_Storage_Browser;
use  Gtk.Persistent_Storage_Browser;
--
-- Remove the following line if you do not use the GNAT Ada compiler.
--
with Gtk.Main.Router.GNAT_Stack;

procedure Test_Gtk_Persistent_Directory_Browser is
   --
   -- All data are global, for the sake of  simplicity.  Otherwise,  the
   -- test were impossible to keep in  just  one  body  due  to  Ada  95
   -- restriction on controlled types.
   --
   Window  : Gtk_Window;
   Browser : Gtk_Persistent_Storage_Browser;
   Errors  : Gtk_Label;
   Selection_Mode : array (Gtk_Selection_Mode) of Gtk_Radio_Button;

   type Local_Callback is access procedure
        (  Widget : access Gtk_Widget_Record'Class
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Callback,
             Gtk.Button.Cb_Gtk_Button_Void
          );
   type Change_Callback is access procedure (Cell : Gtk_Editable);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Change_Callback,
             Cb_Gtk_Editable_Void
          );
   type Error_Callback is access procedure
        (  Object : access Glib.Object.GObject_Record'Class;
           Params : GValues
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Error_Callback,
             Object_Callback.Handler
          );

   procedure Add (Widget : access Gtk_Widget_Record'Class) is
      Storage : Storage_Handle;
   begin
      Add_Storage (Get_Cache (Browser), Storage);
   end Add;

   procedure Refreshed (Widget : access Gtk_Widget_Record'Class) is
   begin
      Changed
      (  Get_Cache (Browser),
         Get_Current_Directory (Get_Tree_View (Browser))
      );
   end Refreshed;

   procedure Deleted (Widget : access Gtk_Widget_Record'Class) is
   begin
      if Get_Selection_Size (Get_Items_View (Browser)) = 0 then
         -- Deleting entire directory
         declare
            Name : Item_Path renames
                      Get_Current_Directory (Get_Tree_View (Browser));
         begin
            case Message_Dialog
                 (  "Do you want to delete directory " & String (Name),
                    Confirmation,
                    Button_OK or Button_Cancel,
                    Button_Cancel
                 )  is
               when Button_OK =>
                  Delete (Get_Cache (Browser), Name);
               when others =>
                  null;
            end case;
         exception
            when Error : others =>
               Say
               (  "Cannot delete "
               &  String (Name)
               &  ". "
               &  Exception_Message (Error)
               );
         end;
      else
         -- Deleting files
         declare
            Selected : constant Selection :=
                          Get_Selection (Get_Items_View (Browser));
            List : Unbounded_String;
         begin
            for Index in Selected'Range loop
               if Index /= 1 then
                  Append (List, ", ");
               end if;
               Append
               (  List,
                  String
                  (  Get_Name
                     (  Get_Items_View (Browser),
                        Selected (Index)
               )  )  );
            end loop;
            case Message_Dialog
                 (  (  "Do you want to delete "
                    &  To_String (List)
                    &  " from "
                    &  String (Get_Directory (Get_Items_View (Browser)))
                    ),
                    Confirmation,
                    Button_OK or Button_Cancel,
                    Button_Cancel
                 )  is
               when Button_OK =>
                  for Index in reverse Selected'Range loop
                     Delete
                     (  Get_Cache (Browser),
                        Get_Path
                        (  Get_Items_View (Browser),
                           Selected (Index)
                     )  );
                  end loop;
               when others =>
                   null;
            end case;
         exception
            when Error : others =>
               Say
               (  "Cannot delete some of the files. "
               &  Exception_Message (Error)
               );
         end;
      end if;
   end Deleted;

   procedure Selection_Mode_Set
             (  Widget : access Gtk_Widget_Record'Class
             )  is
   begin
      for Index in Selection_Mode'Range loop
         if Get_Active (Selection_Mode (Index)) then
            Set_Selection_Mode (Get_Items_View (Browser), Index);
            return;
         end if;
      end loop;
   end Selection_Mode_Set;

   function Mode_Label (Mode : Gtk_Selection_Mode) return String is
   begin
      case Mode is
         when Selection_None     => return "None";
         when Selection_Single   => return "Single";
         when Selection_Browse   => return "Browse";
         when Selection_Multiple => return "Multiple";
      end case;
   end Mode_Label;

   procedure Error
             (  Store  : access Glib.Object.GObject_Record'Class;
                Params : GValues
             )  is
      Text   : String  := Get_String (Nth (Params, 1));
      Where  : String  := Get_String (Nth (Params, 2));
   begin
      Set_Text (Errors, Text & " (while reading '" & Where & "')");
   end Error;

   procedure Program_Error_Tracer
             (  Occurence : Exception_Occurrence
             )  is
      use GNAT.Traceback, GNAT.Traceback.Symbolic;
   begin
      Put_Line
      (  "Traced Program_Error "
      &  Exception_Message (Occurence)
      &  " at"
      );
      Put_Line (Symbolic_Traceback (Occurence));
   end Program_Error_Tracer;

   type Exception_Tracer_Ptr is access
      procedure (Occurence : Exception_Occurrence);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Exception_Tracer_Ptr,
             Exception_Action
          );

begin
   Register_Id_Action
   (  Program_Error'Identity,
      +Program_Error_Tracer'Access
   );
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Window.Set_Title ("Test persistent browser");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   --
   -- The following lines are meant for debugging under GNAT. They cause
   -- stack  tracing upon errors in the libraries specified. Remove them
   -- if you are using another compiler.
   --
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("Gtk");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GLib-GObject");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GLib-GIO");
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace ("GtkAda+");

   declare
      Box     : Gtk_VBox;
      Buttons : Gtk_HBox;
      Button  : Gtk_Image_Button;
   begin
      Gtk_New_VBox (Box);
      -- Buttons
      Gtk_New_HBox (Buttons);

      Gtk_New (Button, Stock_Refresh, Icon_Size_Small_Toolbar);
      Button.On_Clicked (+Refreshed'Access);
      Buttons.Pack_Start (Button, False, False);

      Gtk_New (Button, Stock_New, Icon_Size_Small_Toolbar);
      Button.On_Clicked (+Add'Access);
      Buttons.Pack_Start (Button, False, False);

      Gtk_New (Button, Stock_Delete, Icon_Size_Small_Toolbar);
      Button.On_Clicked (+Deleted'Access);
      Buttons.Pack_Start (Button, False, False);

      -- Radio buttons
      for Index in Selection_Mode'Range loop
         if Index = Gtk_Selection_Mode'First then
            Gtk_New
            (  Selection_Mode (Index),
               Label => Mode_Label (Index)
            );
         else
            Gtk_New
            (  Selection_Mode (Index),
               Get_Group (Selection_Mode (Gtk_Selection_Mode'First)),
               Mode_Label (Index)
            );
         end if;
         Buttons.Pack_Start (Selection_Mode (Index), False, False);
      end loop;
      Selection_Mode (Selection_Multiple).Set_Active (True);
      for Index in Selection_Mode'Range loop
         Selection_Mode (Index).On_Clicked (+Selection_Mode_Set'Access);
      end loop;

      -- Packing buttons
      Box.Pack_Start (Buttons, False, False);
      -- Browser
      Gtk_New
      (  Browser,
         Gtk.Persistent_Storage_Credentials_Dialog.ODBC.Create
      );
      Browser.Get_Items_View.Set_Editable (True);
      Browser.Get_Tree_View.Set_Editable (True);
      Box.Pack_Start (Browser);
      -- Messages
      Gtk_New (Errors);
      Errors.Set_Justify (Justify_Left);
      Errors.Set_Alignment (0.0, 0.5);
      Box.Pack_Start (Errors, False, False);

      Object_Callback.Connect
      (  Get_Cache (Browser),
         "rewind-error",
         +Error'Access
      );
      Object_Callback.Connect
      (  Get_Cache (Browser),
         "read-error",
         +Error'Access
      );

      Window.Add (Box);
      Box.Show_All;
   end;
   Window.Show;

   Gtk.Main.Main;
end Test_Gtk_Persistent_Directory_Browser;
