with Gtk.Button;
with Gtk.Button_Box;
with Gtk.Frame;
with Gtk.Label;
with Gtk.Switch;
with Parametrization;
with Shared_Parameters;

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
         Bug_Switch       : constant Gtk.Switch.Gtk_Switch :=
                              Gtk.Switch.Gtk_Switch_New;
         V_Initial        : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         V_Safe_Landing   : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         V_Target_Landing :  constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         A_Initial        : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         F_Initial        : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         F_Rate           : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         A_Thruster       : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
      begin
         Bug_Switch.all.Set_State (State => Shared_Sensor_Data.Bug_Enabled);
         Bug_Switch.all.On_State_Set (Call  => Callbacks.Switch_Bug'Access,
                                      After => False);
         V_Initial.all.Set_Text
           (Text =>
              Shared_Types.IO.Image
                (Value        => Shared_Parameters.Initial_Velocity,
                 Include_Unit => False));
         V_Initial.all.On_Focus_Out_Event
           (Call  => Callbacks.Set_Initial_Velocity'Access,
            After => True);

         V_Safe_Landing.all.Set_Text
           (Text =>
              Shared_Types.IO.Image
                (Value        => Shared_Parameters.Safe_Landing_Velocity,
                 Include_Unit => False));
         V_Safe_Landing.all.Set_Editable (Is_Editable => False);

         V_Target_Landing.all.Set_Text
           (Text =>
              Shared_Types.IO.Image
                (Value        => Shared_Parameters.Target_Landing_Velocity,
                 Include_Unit => False));
         V_Target_Landing.all.Set_Editable (Is_Editable => False);

         A_Initial.all.Set_Text
           (Text =>
              Shared_Types.IO.Image
                (Value        => Shared_Parameters.Initial_Altitude,
                 Include_Unit => False));
         A_Initial.all.On_Focus_Out_Event
           (Call  => Callbacks.Set_Initial_Altitude'Access,
            After => True);

         F_Initial.all.Set_Text
           (Text =>
              Shared_Types.IO.Image
                (Value        => Shared_Parameters.Initial_Fuel_Mass,
                 Include_Unit => False));
         F_Initial.all.On_Focus_Out_Event
           (Call  => Callbacks.Set_Initial_Fuel_Mass'Access,
            After => True);

         F_Rate.all.Set_Text
           (Text =>
              Shared_Types.IO.Image
                (Value        => Shared_Parameters.Fuel_Flow_Rate,
                 Include_Unit => False));
         F_Rate.all.Set_Editable (Is_Editable => False);

         A_Thruster.all.Set_Text
           (Text =>
              Shared_Types.IO.Image
                (Value        => Parametrization.Thruster_Acceleration,
                 Include_Unit => False));
         A_Thruster.all.Set_Editable (Is_Editable => False);

         declare
            Widget_Box : constant Gtk.Box.Gtk_Box :=
                           Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                                 Spacing     => 0);
         begin
            Widget_Box.all.Pack_Start
              (Child => Labeled_Widget (Widget      => Bug_Switch,
                                        Description => "TDM Bug"));
            Widget_Box.all.Pack_Start
              (Child => Labeled_Widget (Widget      => V_Initial,
                                        Description => "Initial Velocity"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget (Widget      => V_Safe_Landing,
                                 Description => "Safe Landing Velocity"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget (Widget      => V_Target_Landing,
                                 Description => "Target Landing Velocity"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget (Widget      => A_Initial,
                                 Description => "Initial Altitude"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget (Widget      => F_Initial,
                                 Description => "Initial Fuel Mass"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget (Widget      => F_Rate,
                                 Description => "Fuel Flow Rate"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget (Widget      => A_Thruster,
                                 Description => "Thruster Acceleration"));

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

         Start_Button.all.On_Clicked (Call  => Callbacks.SIM_Start'Access,
                                      After => True);
         Abort_Button.all.On_Clicked (Call  => Callbacks.SIM_Abort'Access,
                                      After => True);
         Exit_Button.all.On_Clicked (Call  => Callbacks.Exit_Main'Access,
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