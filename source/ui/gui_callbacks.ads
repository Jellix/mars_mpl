with GNAT.OS_Lib;

with Gtk.Button;
with Gtk.Handlers;
with Gtk.Switch;
with Gtk.Window;

package GUI_Callbacks is

   Aborted : Boolean := False;

   SIM_Pid : GNAT.OS_Lib.Process_Id := GNAT.OS_Lib.Invalid_Pid;

   package Windows_CB is
     new Gtk.Handlers.Callback (Widget_Type => Gtk.Window.Gtk_Window_Record);

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure Exit_Main (Win    : access Gtk.Window.Gtk_Window_Record'Class);

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function Switch_Bug (Self  : access Gtk.Switch.Gtk_Switch_Record'Class;
                        State : Boolean) return Boolean;

end GUI_Callbacks;
