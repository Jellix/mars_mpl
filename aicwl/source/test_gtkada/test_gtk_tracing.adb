--                                                                    --
--  procedure Test_Gtk_Tracing      Copyright (c)  Dmitry A. Kazakov  --
--  Test for Gtk.Main.Router                       Luebeck            --
--                                                 Spring, 2012       --
--                                                                    --
--                                Last revision :  19:09 09 Oct 2015  --
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
with Ada.Text_IO;      use Ada.Text_IO;
with Gdk.Event;        use Gdk.Event;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Window;       use Gtk.Window;
with Gtk.Widget;       use Gtk.Widget;

with Ada.Unchecked_Conversion;
with Glib.Messages;
with Gtk.Main.Router.GNAT_Stack;
with Gtk.Missed;

procedure Test_Gtk_Tracing is
   Window : Gtk_Window;
   Box    : Gtk_VBox;
   Button : Gtk_Button;
   LF     : constant Character := Character'Val (10);

   type Local_Callback is access
      procedure (Widget : access Gtk_Widget_Record'Class);
   function "+" is
      new Ada.Unchecked_Conversion (Local_Callback, Cb_Gtk_Button_Void);

   procedure Trace_This (Widget : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Router.Trace
      (  "   The text passed to Gtk.Main.Router.Trace appears " & LF &
         "in the trace window, which is popped as needed. " & LF &
         "When the window first appears it blocks the " & LF &
         "application. Pressing the RECORD button releases " & LF &
         "it, and further calls to Trace would not break " & LF &
         "if not explicitly required."
      );
   end Trace_This;

   procedure Trace_Stack (Widget : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Router.GNAT_Stack.Trace
      (  "   The text passed to Gtk.Main.Router.GNAT_Stack.Trace" & LF &
         "appears in trace window, which is popped as needed. " & LF &
         "The output is accompanied by the call stack trace."
      );
   end Trace_Stack;

   procedure Trace_Exception
             (  Widget : access Gtk_Widget_Record'Class
             )  is
      X : Integer := 0;
   begin
      X := X / X; -- Error here
   exception
      when Error : others =>
         Gtk.Main.Router.Trace
         (  "   Gtk.Main.Router.GNAT_Stack.Trace can be called " & LF &
            "with an exception occurence, which traces the " & LF &
            "exception stack"
         );
         Gtk.Main.Router.GNAT_Stack.Trace (Error);
   end Trace_Exception;

   procedure Trace_Log
             (  Widget : access Gtk_Widget_Record'Class
             )  is
      use GLib.Messages;
   begin
      Log ("GtkAda+", Log_Level_Message, "Test message");
   end Trace_Log;

   task Spammer is
      entry Start;
   end Spammer;

   task body Spammer is
      File : File_Type;
   begin
      select
         accept Start;
      or terminate;
      end select;
      Open (File, In_File, "test_gtk_tracing.adb");
      for Index in 1..1_000 loop
         begin
            Gtk.Main.Router.Trace (Get_Line (File));
         exception
            when End_Error =>
               Close (File);
               Open (File, In_File, "test_gtk_tracing.adb");
         end;
      end loop;
   exception
      when Tasking_Error =>
         null;
   end Spammer;

   procedure Trace_Flood
             (  Widget : access Gtk_Widget_Record'Class
             )  is
   begin
      Spammer.Start;
      Widget.Set_Sensitive (False);
   end Trace_Flood;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Gtk.Main.Router.Init (Window); -- This must be called once
   Gtk.Main.Router.GNAT_Stack.Set_Log_Trace
   (  "GtkAda+",
      GLib.Messages.Log_Level_Flags'Last
   );
--     Gtk.Main.Router.GNAT_Stack.Set_Log_Trace
--     (  "Gdk",
--        Glib.Messages.Log_Level_Warning
--     );
   Window.Set_Title ("Test Tracing");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Gtk_New_VBox (Box);
   Add (Window, Box);

   Gtk_New (Button, "Trace this");
   Button.On_Clicked (+Trace_This'Access);
   Pack_Start (Box, Button);

   Gtk_New (Button, "Trace stack");
   Button.On_Clicked (+Trace_Stack'Access);
   Pack_Start (Box, Button);

   Gtk_New (Button, "Trace exception");
   Button.On_Clicked (+Trace_Exception'Access);
   Pack_Start (Box, Button);

   Gtk_New (Button, "Trace log messages");
   Button.On_Clicked (+Trace_Log'Access);
   Pack_Start (Box, Button);

   Gtk_New (Button, "Trace flood");
   Button.On_Clicked (+Trace_Flood'Access);
   Pack_Start (Box, Button);

   Gtk.Main.Router.Trace
   (  "   This is the trace window which contains the output " & LF &
      "produced by Gtk.Main.Router.Trace calls. Press the " & LF &
      "RECORD button to release the application."
   );
   Gtk.Main.Router.Trace
   (  "   Note how separate calls to Trace cause the output of " & LF &
      "different colors."
   );
   Gtk.Main.Router.Trace
   (  "   When the project is opened in the GPS studio and the " & LF &
      "studio was called with the switch:"  & LF & LF &
      "   gps --server=50000" & LF & LF &
      "(in the server mode), you will be able to navigate " & LF &
      "the symbolic traceback using the right click mouse " & LF &
      "menu dropped down when the corresponding line of " & LF &
      "the traceback is clicked on."
   );

   Window.Show_All;
   Gtk.Main.Main;
exception
   when Error : others =>
      Put_Line ("Fatal error: " & Exception_Information (Error));
end Test_Gtk_Tracing;
