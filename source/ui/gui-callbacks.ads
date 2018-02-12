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

   function Set_Fuel_Flow_Rate
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

   function Set_Initial_Altitude
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

   function Set_Initial_Fuel_Mass
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

   function Set_Initial_Velocity
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

   function Set_Thruster_Acceleration
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean;

end GUI.Callbacks;
