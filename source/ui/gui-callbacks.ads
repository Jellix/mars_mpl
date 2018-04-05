with Gdk.Event;
with Gtk.Button;
with Gtk.Toggle_Button;
with Gtk.Widget;

private package GUI.Callbacks is

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class);

   function Exit_Main (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                       Event  : in     Gdk.Event.Gdk_Event) return Boolean;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class);

   procedure Switch_Bug
     (Self : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class);

   generic
      type T is delta <>;
      with function Image (Value        : in T;
                           Include_Unit : in Boolean) return String is <>;
      with function Read return T;
      with procedure Write (Value : in T);
      Name   : String;
   function Set_Spin_Button_Value
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in     Gdk.Event.Gdk_Event_Focus) return Boolean;

   generic
      type T is delta <>;
      with procedure Write (Value : in T) is <>;
      with function Default return T is <>;
      Text_Entry : in Text_Entries;
   procedure Reset_Value (Button : access Gtk.Button.Gtk_Button_Record'Class);

end GUI.Callbacks;
