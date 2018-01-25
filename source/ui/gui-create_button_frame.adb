with Gtk.Button;
with Gtk.Button_Box;

separate (GUI)
function Create_Button_Frame
  (Window : in out Main_Window_Record'Class) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Button_Box   : constant Gtk.Button_Box.Gtk_Button_Box :=
                    Gtk.Button_Box.Gtk_Button_Box_New
                      (Orientation => Gtk.Enums.Orientation_Horizontal);
   Start_Button : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button_New_With_Label
                      (Label => "Start");
   Abort_Button : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button_New_With_Label
                      (Label => "Abort");
   Exit_Button  : constant Gtk.Button.Gtk_Button :=
                    Gtk.Button.Gtk_Button_New_With_Label
                      (Label => "Exit");
begin
   Window.Start_Button := Start_Button;
   Window.Abort_Button := Abort_Button;

   Button_Box.all.Add (Widget => Start_Button);
   Button_Box.all.Add (Widget => Abort_Button);
   Button_Box.all.Add (Widget => Exit_Button);

   Start_Button.all.On_Clicked (Call  => GUI_Callbacks.SIM_Start'Access,
                                After => True);
   Abort_Button.all.On_Clicked (Call  => GUI_Callbacks.SIM_Abort'Access,
                                After => True);
   Exit_Button.all.On_Clicked (Call  => GUI_Callbacks.Exit_Main'Access,
                               After => True);

   return Button_Box;
end Create_Button_Frame;
