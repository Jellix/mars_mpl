with GNAT.OS_Lib;

with Gdk.Event;

with Gtk.Button;
with Gtk.Switch;
with Gtk.Widget;

package GUI_Callbacks is

   Aborted : Boolean := False;

   SIM_Pid : GNAT.OS_Lib.Process_Id := GNAT.OS_Lib.Invalid_Pid;

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function Exit_Main (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                       Event  : in     Gdk.Event.Gdk_Event) return Boolean;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function Switch_Bug (Self  : access Gtk.Switch.Gtk_Switch_Record'Class;
                        State : in Boolean) return Boolean;

   function Set_A_Initial (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
                           Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

   function Set_V_Initial (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
                           Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

end GUI_Callbacks;
