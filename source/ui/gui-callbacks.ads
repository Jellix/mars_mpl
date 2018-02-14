with Gdk.Event;
with Gtk.Button;
with Gtk.Switch;

private package GUI.Callbacks is

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function Exit_Main (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                       Event  : in     Gdk.Event.Gdk_Event) return Boolean;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function Switch_Bug (Self  : access Gtk.Switch.Gtk_Switch_Record'Class;
                        State : in Boolean) return Boolean;

   generic
      type T is delta <>;
      with function Image (Value        : in T;
                           Include_Unit : in Boolean) return String is <>;
      Target : in out T;
      Name   : String;
   function Set_GEntry_Value
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

end GUI.Callbacks;
