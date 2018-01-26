with Gtk.Button;
with Gtk.Button_Box;
with Gtk.Frame;
with Gtk.Label;
with Gtk.Switch;

separate (GUI)
function Create_Simulation_Frame
  (Window : in out Main_Window_Record'Class) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Simulation control");
begin
   declare
      Container : constant Gtk.Box.Gtk_Box :=
                    Gtk.Box.Gtk_Hbox_New (Homogeneous => False,
                                          Spacing     => 0);
   begin
      Frame.all.Add (Widget => Container);

      declare
         Bug_Switch : constant Gtk.Switch.Gtk_Switch :=
                        Gtk.Switch.Gtk_Switch_New;
      begin
         Bug_Switch.all.Set_State (State => Shared_Sensor_Data.Bug_Enabled);
         Bug_Switch.all.On_State_Set (Call  => GUI_Callbacks.Switch_Bug'Access,
                                      After => False);

         declare
            Widget_Box : constant Gtk.Box.Gtk_Box :=
                           Gtk.Box.Gtk_Hbox_New (Homogeneous => False,
                                                 Spacing     => 0);
            Label      : constant Gtk.Label.Gtk_Label :=
                           Gtk.Label.Gtk_Label_New (Str => "TDM Bug");
         begin
            Widget_Box.all.Pack_Start (Child  => Label,
                                       Expand => True);
            Widget_Box.all.Pack_End (Child  => Bug_Switch,
                                     Expand => False);

            Container.all.Pack_Start (Child  => Widget_Box,
                                      Expand => False);
         end;
      end;

      declare
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

         Start_Button.all.On_Clicked (Call  => GUI_Callbacks.SIM_Start'Access,
                                      After => True);
         Abort_Button.all.On_Clicked (Call  => GUI_Callbacks.SIM_Abort'Access,
                                      After => True);
         Exit_Button.all.On_Clicked (Call  => GUI_Callbacks.Exit_Main'Access,
                                     After => True);

         declare
            Button_Box : constant Gtk.Button_Box.Gtk_Button_Box :=
                           Gtk.Button_Box.Gtk_Button_Box_New
                             (Orientation => Gtk.Enums.Orientation_Horizontal);
         begin
            Button_Box.all.Pack_Start (Child  => Start_Button,
                                       Expand => False);
            Button_Box.all.Pack_Start (Child  => Abort_Button,
                                       Expand => False);
            Button_Box.all.Pack_Start (Child  => Exit_Button,
                                       Expand => False);
            Container.all.Pack_End (Child  => Button_Box,
                                    Expand => False);
         end;
      end;
   end;

   return Frame;
end Create_Simulation_Frame;
