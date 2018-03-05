with Gtk.Button;
with Gtk.Button_Box;
with Gtk.Frame;
with Gtk.Label;
with Gtk.Scrolled_Window;
with Gtk.Switch;
with Gtk.Text_Buffer.With_End_Mark;
with Shared_Parameters.Read;

separate (GUI)
function Create_Simulation_Frame
  (Window : in out Main_Window_Record'Class) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Simulation control");
begin
   Add_Widgets_To_Frame :
   declare
      Container : constant Gtk.Box.Gtk_Box :=
                    Gtk.Box.Gtk_Hbox_New (Homogeneous => False,
                                          Spacing     => 0);
   begin
      Frame.all.Add (Widget => Container);

      Add_Controls :
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
         M_Dry            : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         F_Initial        : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         F_Rate           : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
         V_Thruster       : constant Gtk.GEntry.Gtk_Entry :=
                              Gtk.GEntry.Gtk_Entry_New;
      begin
         Bug_Switch.all.Set_State
           (State => Shared_Parameters.Read.TDM_Bug_Enabled);
         Bug_Switch.all.On_State_Set (Call  => Callbacks.Switch_Bug'Access,
                                      After => False);
         V_Initial.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Initial_Velocity,
                           With_Unit => False));
         V_Initial.all.On_Focus_Out_Event (Call  => Set_Initial_Velocity'Access,
                                           After => True);

         V_Safe_Landing.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Safe_Landing_Velocity,
                     With_Unit => False));
         V_Safe_Landing.all.Set_Editable (Is_Editable => False);

         V_Target_Landing.all.Set_Text
           (Text =>
              Image
                (Value     => Shared_Parameters.Read.Target_Landing_Velocity,
                 With_Unit => False));
         V_Target_Landing.all.Set_Editable (Is_Editable => False);

         A_Initial.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Initial_Altitude,
                           With_Unit => False));
         A_Initial.all.On_Focus_Out_Event (Call  => Set_Initial_Altitude'Access,
                                           After => True);

         M_Dry.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Dry_Mass,
                     With_Unit => False));
         M_Dry.all.On_Focus_Out_Event
           (Call  => Set_Dry_Mass'Access,
            After => True);

         F_Initial.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Initial_Fuel_Mass,
                     With_Unit => False));
         F_Initial.all.On_Focus_Out_Event
           (Call  => Set_Initial_Fuel_Mass'Access,
            After => True);

         F_Rate.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Fuel_Flow_Rate,
                           With_Unit => False));
         F_Rate.all.On_Focus_Out_Event (Call  => Set_Fuel_Flow_Rate'Access,
                                        After => True);

         V_Thruster.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Exhaust_Velocity,
                     With_Unit => False));
         V_Thruster.all.On_Focus_Out_Event
           (Call  => Set_Exhaust_Velocity'Access,
            After => True);

         Add_Labeled_Text_Entries :
         declare
            Widget_Box : constant Gtk.Box.Gtk_Box :=
                           Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                                 Spacing     => 0);

            function Labeled_Widget_With_Unit
              (Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
               Description : in String;
               Unit        : in String) return not null access
              Gtk.Widget.Gtk_Widget_Record'Class;

            function Labeled_Widget_With_Unit
              (Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
               Description : in String;
               Unit        : in String) return not null access
              Gtk.Widget.Gtk_Widget_Record'Class
            is
               Box : constant Gtk.Box.Gtk_Box :=
                       Gtk.Box.Gtk_Box
                         (Labeled_Widget (Widget      => Widget,
                                          Description => Description));
            begin
               Box.all.Pack_Start
                 (Child => Gtk.Label.Gtk_Label_New (Str => Unit));

               return Box;
            end Labeled_Widget_With_Unit;

         begin
            Widget_Box.all.Pack_Start
              (Child => Labeled_Widget (Widget      => Bug_Switch,
                                        Description => "TDM Bug"));

            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit (Widget      => V_Initial,
                                           Description => "Initial Velocity",
                                           Unit        => "m/s"));

            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit
                   (Widget      => V_Safe_Landing,
                    Description => "Safe Landing Velocity",
                    Unit        => "m/s"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit
                   (Widget      => V_Target_Landing,
                    Description => "Target Landing Velocity",
                    Unit        => "m/s"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit (Widget      => A_Initial,
                                           Description => "Initial Altitude",
                                           Unit        => "m"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit (Widget      => M_Dry,
                                           Description => "Spacecraft Dry Mass",
                                           Unit        => "kg"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit (Widget      => F_Initial,
                                           Description => "Initial Fuel Mass",
                                           Unit        => "kg"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit (Widget      => F_Rate,
                                           Description => "Fuel Flow Rate",
                                           Unit        => "kg/s"));
            Widget_Box.all.Pack_Start
              (Child =>
                 Labeled_Widget_With_Unit (Widget      => V_Thruster,
                                           Description => "Exhaust Velocity",
                                           Unit        => "m/s"));

            Container.all.Pack_Start (Child  => Widget_Box,
                                      Expand => False);
         end Add_Labeled_Text_Entries;
      end Add_Controls;

      --  Create the simulator output window.
      Create_SIM_Logger :
      declare
         Log_Frame  : constant Gtk.Frame.Gtk_Frame :=
                        Gtk.Frame.Gtk_Frame_New (Label => "SIMon says:");
         Log_Window : constant Gtk.Scrolled_Window.Gtk_Scrolled_Window :=
                         Gtk.Scrolled_Window.Gtk_Scrolled_Window_New;
         Log_View   : constant Gtk.Text_View.Gtk_Text_View :=
                        Gtk.Text_View.Gtk_Text_View_New_With_Buffer
                          (Buffer =>
                             Gtk.Text_Buffer.With_End_Mark.
                               Gtk_Text_Buffer_With_End_Mark_New);
      begin
         Log_View.all.Set_Editable (Setting => False);
         Log_Window.all.Add (Widget => Log_View);
         Log_Frame.all.Add (Widget => Log_Window);
         Container.all.Pack_Start (Child  => Log_Frame,
                                   Expand => True);
         Window.SIMon_Says := Log_View;
      end Create_SIM_Logger;

      Create_Buttons :
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

         Add_Buttons :
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
         end Add_Buttons;
      end Create_Buttons;
   end Add_Widgets_To_Frame;

   return Frame;
end Create_Simulation_Frame;
