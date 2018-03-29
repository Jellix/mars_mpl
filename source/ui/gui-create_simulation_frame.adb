with Gdk.RGBA;
with Gtk.Button_Box;
with Gtk.Frame;
with Gtk.Grid;
with Gtk.Label;
with Gtk.Style_Context;
with Gtk.Switch;
with Shared_Parameters.Read;

separate (GUI)
function Create_Simulation_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Container : constant Gtk.Box.Gtk_Box :=
                 Gtk.Box.Gtk_Hbox_New (Homogeneous => False,
                                       Spacing     => 0);
begin
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
      T_On             : constant Gtk.GEntry.Gtk_Entry :=
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
           Image (Value     => Shared_Parameters.Read.Target_Landing_Velocity,
                  With_Unit => False));
      V_Target_Landing.all.Set_Editable (Is_Editable => False);

      A_Initial.all.Set_Text
        (Text => Image (Value     => Shared_Parameters.Read.Initial_Altitude,
                        With_Unit => False));
      A_Initial.all.On_Focus_Out_Event (Call  => Set_Initial_Altitude'Access,
                                        After => True);

      M_Dry.all.Set_Text
        (Text => Image (Value     => Shared_Parameters.Read.Dry_Mass,
                        With_Unit => False));
      M_Dry.all.On_Focus_Out_Event
        (Call  => Set_Dry_Mass'Access,
         After => True);

      F_Initial.all.Set_Text
        (Text => Image (Value     => Shared_Parameters.Read.Initial_Fuel_Mass,
                        With_Unit => False));
      F_Initial.all.On_Focus_Out_Event
        (Call  => Set_Initial_Fuel_Mass'Access,
         After => True);

      F_Rate.all.Set_Text
        (Text => Image (Value     => Shared_Parameters.Read.Fuel_Flow_Rate,
                        With_Unit => False));
      F_Rate.all.On_Focus_Out_Event (Call  => Set_Fuel_Flow_Rate'Access,
                                     After => True);

      T_On.all.Set_Text
        (Text => Image (Value     => Shared_Parameters.Read.Shortest_On_Time,
                        With_Unit => False));
      T_On.all.On_Focus_Out_Event (Call  => Set_Shortest_On_Time'Access,
                                   After => True);

      V_Thruster.all.Set_Text
        (Text => Image (Value     => Shared_Parameters.Read.Exhaust_Velocity,
                        With_Unit => False));
      V_Thruster.all.On_Focus_Out_Event
        (Call  => Set_Exhaust_Velocity'Access,
         After => True);

      Add_Labeled_Text_Entries :
      declare
         Grid : constant Gtk.Grid.Gtk_Grid := Gtk.Grid.Gtk_Grid_New;
         Row  : Glib.Gint                  := 0;

         procedure Grid_Add_Row
           (Left   : in              Glib.UTF8_String := "";
            Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
            Right  : in              Glib.UTF8_String := "");

         procedure Grid_Add_Row
           (Left   : in              Glib.UTF8_String := "";
            Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
            Right  : in              Glib.UTF8_String := "")
         is
            use type Glib.Gint;
         begin
            Grid.all.Insert_Row (Position => Row);

            if Left /= "" then
               Grid.all.Attach (Child  => Gtk.Label.Gtk_Label_New (Str => Left),
                                Left   => 0,
                                Top    => Row);
            end if;

            Grid.all.Attach (Child => Widget,
                             Left  => 1,
                             Top   => Row);

            if Right /= "" then
               Grid.all.Attach (Child => Gtk.Label.Gtk_Label_New (Str => Right),
                                Left  => 2,
                                Top   => Row);
            end if;

            Row := Row + 1;
         end Grid_Add_Row;
      begin
         for I in Glib.Gint range 0 .. 2 loop
            Grid.all.Insert_Column (Position => I);
         end loop;

         Grid_Add_Row (Left   => "TDM Bug",
                       Widget => Bug_Switch);
         Grid_Add_Row (Left   => "Initial Velocity",
                       Widget => V_Initial,
                       Right  => "m/s");
         Grid_Add_Row (Left   => "Safe Landing Velocity",
                       Widget => V_Safe_Landing,
                       Right  => "m/s");
         Grid_Add_Row (Left   => "Target Landing Velocity",
                       Widget => V_Target_Landing,
                       Right  => "m/s");
         Grid_Add_Row (Left   => "Initial Altitude",
                       Widget => A_Initial,
                       Right  => "m");
         Grid_Add_Row (Left   => "Spacecraft Dry Mass",
                       Widget => M_Dry,
                       Right  => "kg");
         Grid_Add_Row (Left   => "Initial Fuel Mass",
                       Widget => F_Initial,
                       Right  => "kg");
         Grid_Add_Row (Left   => "Fuel Flow Rate",
                       Widget => F_Rate,
                       Right  => "kg/s");
         Grid_Add_Row (Left   => "Shortest on-time",
                       Widget => T_On,
                       Right  => "ms");
         Grid_Add_Row (Left   => "Exhaust Velocity",
                       Widget => V_Thruster,
                       Right  => "m/s");

         Add_Parameter_Frame :
         declare
            Parameter_Frame : constant Gtk.Frame.Gtk_Frame :=
                                Gtk.Frame.Gtk_Frame_New
                                  (Label => "Simulation Parameters");
         begin
            Parameter_Frame.all.Add (Widget => Grid);
            Container.all.Pack_Start (Child  => Parameter_Frame,
                                      Expand => False);
         end Add_Parameter_Frame;
      end Add_Labeled_Text_Entries;
   end Add_Controls;

   --  create SIM log viewer
   Window.SIMon_Says :=
     Gtk.Frame.Log_Viewer.Gtk_Frame_Log_Viewer_New
       (Label   => "SIMon says",
        Process => Window.SIM_Process);
   Window.SIMon_Says.all.Set_Size_Request (Width => 200);

   Container.all.Pack_Start (Child  => Window.SIMon_Says,
                             Expand => True);

   Add_Time_And_Sim_Control :
   declare
      Box : constant Gtk.Box.Gtk_Vbox :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);
   begin
      Container.all.Pack_Start (Child  => Box,
                                Expand => False);

      Add_Mission_Clock :
      declare
         Context : constant Gtk.Style_Context.Gtk_Style_Context :=
                     Gtk.Style_Context.Get_Style_Context
                       (Widget => Window'Access);
         BG_RGBA : Gdk.RGBA.Gdk_RGBA;
         FG_RGBA : Gdk.RGBA.Gdk_RGBA;
      begin
         Context.all.Get_Background_Color
           (State => Gtk.Enums.Gtk_State_Flag_Normal,
            Color => BG_RGBA);
         Context.all.Get_Color (State => Gtk.Enums.Gtk_State_Flag_Normal,
                                Color => FG_RGBA);

         Window.Mission_Clock :=
           Gtk.Frame.Digital_Clock.Gtk_New
             (Label     => "Mission Clock",
              BG_Color  => Gtk.Missed.From_RGBA (Color => BG_RGBA),
              On_Color  => Gtk.Missed.From_RGBA (Color => FG_RGBA),
              Off_Color => Gtk.Missed.From_RGBA (Color => BG_RGBA));
         --  Set background and off-color to the same value.

         Box.all.Pack_Start
           (Child  => Window.Mission_Clock,
            Expand => False);
      end Add_Mission_Clock;

      Create_Buttons :
      begin
         Window.Buttons :=
           Button_List'(Start_Button =>
                          Gtk.Button.Gtk_Button_New_With_Label
                            (Label => "Start"),
                        Abort_Button =>
                          Gtk.Button.Gtk_Button_New_With_Label
                            (Label => "Abort"),
                        Exit_Button =>
                          Gtk.Button.Gtk_Button_New_With_Label
                            (Label => "Exit"));

         --  Initially, the Abort button is disabled.
         Window.Buttons (Abort_Button).all.Set_Sensitive (Sensitive => False);

         Window.Buttons (Start_Button).all.On_Clicked
           (Call  => Callbacks.SIM_Start'Access,
            After => True);
         Window.Buttons (Abort_Button).all.On_Clicked
           (Call  => Callbacks.SIM_Abort'Access,
            After => True);
         Window.Buttons (Exit_Button).all.On_Clicked
           (Call  => Callbacks.Exit_Main'Access,
            After => True);

         Add_Buttons :
         declare
            Button_Box : constant Gtk.Button_Box.Gtk_Button_Box :=
                           Gtk.Button_Box.Gtk_Button_Box_New
                             (Orientation => Gtk.Enums.Orientation_Horizontal);
         begin
            for B of Window.Buttons loop
               Button_Box.all.Pack_Start (Child  => B,
                                          Expand => False);
            end loop;

            Box.all.Pack_End (Child  => Button_Box,
                              Expand => False);
         end Add_Buttons;
      end Create_Buttons;
   end Add_Time_And_Sim_Control;

   return Container;
end Create_Simulation_Frame;
