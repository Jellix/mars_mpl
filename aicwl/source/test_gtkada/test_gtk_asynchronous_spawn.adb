--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Asynchronous_Spawn                 Luebeck            --
--  Test for Gtk.Spawn.Asynchronous                Spring, 2009       --
--                                                                    --
--                                Last revision :  09:44 08 Oct 2016  --
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

with Glib;                       use Glib;
with Glib.Error;                 use Glib.Error;
with GLib.Spawn.Asynchronous;    use GLib.Spawn.Asynchronous;
with GLib.Spawn.Text_Bufferred;  use GLib.Spawn.Text_Bufferred;
with GtkAda.Handlers;            use GtkAda.Handlers;
with Gtk.Button;                 use Gtk.Button;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.GEntry;                 use Gtk.GEntry;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Table;                  use Gtk.Table;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Window;                 use Gtk.Window;
with Gtk.Widget;                 use Gtk.Widget;

with Ada.Unchecked_Conversion;
with Gtk.Main.Router;
with Gtk.Missed;
with Gtk.RC;
with GNAT.OS_Lib;

procedure Test_Gtk_Asynchronous_Spawn is
   use Gtk.Enums.String_List;
   --
   -- All data are global, for the sake of  simplicity.  Otherwise,  the
   -- test were impossible to keep in  just  one  body  due  to  Ada  95
   -- restriction on controlled types.
   --
   Window  : Gtk_Window;
   Label   : Gtk_Label;
   Scroll  : Gtk_Scrolled_Window;
   Command : Gtk_Text_View;
   Input   : Gtk_Text_View;
   Output  : Gtk_Text_View;
   Error   : Gtk_Text_View;
   Grid    : Gtk_Table;
   Button  : Gtk_Button;
   Process : Text_Bufferred_Process;

   -- Circumvention of access rules, don't do it, it is here only to
   -- simplify the test
   type Local_Callback is
       access procedure (Widget : access Gtk_Widget_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Callback,
             Widget_Callback.Simple_Handler
          );

   function Get_Line (Buffer : Gtk_Text_Buffer; No : GInt)
      return String is
      Start  : Gtk_Text_Iter;
      Stop   : Gtk_Text_Iter;
      Got_It : Boolean;
   begin
      Get_Iter_At_Line (Buffer, Start, No);
      if Get_Line (Start) /= No then
         return "";
      end if;
      Copy (Start, Stop);
      Forward_To_Line_End (Stop, Got_It);
      if Got_It then
         return Get_Text (Buffer, Start, Stop);
      end if;
      Get_End_Iter (Buffer, Stop);
      if Is_End (Start) then
         return "";
      else
         return Get_Text (Buffer, Start, Stop);
      end if;
   end Get_Line;

   procedure Do_Run (Widget : access Gtk_Widget_Record'Class) is
      From, To : Gtk_Text_Iter;
      Name : constant String := Get_Line (Get_Buffer (Command), 0);
      ArgV : GList := Null_List;
   begin
      Get_Bounds (Get_Buffer (Output), From, To);
      Delete     (Get_Buffer (Output), From, To);
      Get_Bounds (Get_Buffer (Error),  From, To);
      Delete     (Get_Buffer (Error),  From, To);
      for Index in 1..GInt'Last loop
         declare
            Line : constant String :=
                      Get_Line (Get_Buffer (Command), Index);
         begin
            exit when Line = "";
            Append (ArgV, Line);
         end;
      end loop;
      Run
      (  Process => Process,
         Name    => Name,
         ArgV    => ArgV,
         Input   => Get_Buffer (Input),
         Output  => Get_Buffer (Output),
         Error   => Get_Buffer (Error)
      );
      if ArgV /= Null_List then
         Free_String_List (ArgV);
      end if;
      case Get_State (Process) is
         when Process_Running | Process_Completed =>
            null;
         when Process_Failed_To_Start =>
            Set_Text
            (  Get_Buffer (Error),
               Get_Message (Get_Error (Process))
            );
      end case;
   end Do_Run;

begin
   GNAT.OS_Lib.Setenv ("G_SPAWN_WIN32_DEBUG", "1");

   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window);
   Set_Title (Window, "Test Spawn");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

   Gtk_New (Grid, 4, 3, False);
   Set_Col_Spacings (Grid, 3);
   Set_Row_Spacings (Grid, 3);
   Add (Window, Grid);

   Gtk_New
   (  Label,
      (  "Command," & Character'Val (10)
      &  "Argument 1," & Character'Val (10)
      &  "Argument 2," & Character'Val (10)
      &  "...:"
   )  );
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 0, 1, 0, 1, XOptions => Fill);
   Gtk_New (Scroll);
   Gtk_New (Command);
   Add (Scroll, Command);
   Attach (Grid, Scroll, 1, 2, 0, 1);
   Gtk_New (Button, "Run");
   Attach (Grid, Button, 2, 3, 0, 1, XOptions => Shrink);

   Gtk_New (Label, "Input:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 0, 1, 1, 2, XOptions => Fill);
   Gtk_New (Scroll);
   Gtk_New (Input);
   Add (Scroll, Input);
   Attach (Grid, Scroll, 1, 3, 1, 2);

   Gtk_New (Label, "Output:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 0, 1, 2, 3, XOptions => Fill);
   Gtk_New (Scroll);
   Gtk_New (Output);
   Add (Scroll, Output);
   Attach (Grid, Scroll, 1, 3, 2, 3);

   Gtk_New (Label, "Error:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 0, 1, 3, 4, XOptions => Fill);
   Gtk_New (Scroll);
   Gtk_New (Error);
   Add (Scroll, Error);
   Attach (Grid, Scroll, 1, 3, 3, 4);

   Widget_Callback.Connect (Button, "clicked", +Do_Run'Access);

   Show_All (Grid);
   Show (Window);

   Gtk.Main.Main;
end Test_Gtk_Asynchronous_Spawn;
