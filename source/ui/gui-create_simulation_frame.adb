with Gdk.RGBA;
with Gtk.Button_Box;
with Gtk.Check_Button;
with Gtk.Frame;
with Gtk.Grid;
with Gtk.Label;
with Gtk.Style_Context;
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
   Add_Labeled_Text_Entries :
   declare
      use type Glib.Gint;

      Grid : constant Gtk.Grid.Gtk_Grid := Gtk.Grid.Gtk_Grid_New;
      Row  : Glib.Gint                  := 0;

      procedure Grid_Add_Row
        (Left   : in              Glib.UTF8_String := "";
         Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Right  : in              Glib.UTF8_String := "";
         Reset  :          access Gtk.Button.Gtk_Button_Record'Class := null);

      procedure Grid_Add_Row
        (Left   : in              Glib.UTF8_String := "";
         Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Right  : in              Glib.UTF8_String := "";
         Reset  :          access Gtk.Button.Gtk_Button_Record'Class := null) is
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

         if Reset /= null then
            Grid.all.Attach (Child => Reset,
                             Left  => 3,
                             Top   => Row);
         end if;

         Row := Row + 1;
      end Grid_Add_Row;
   begin
      for I in Glib.Gint range 0 .. 3 loop
         Grid.all.Insert_Column (Position => I);
      end loop;

      declare
         B : constant Gtk.Check_Button.Gtk_Check_Button :=
               Gtk.Check_Button.Gtk_Check_Button_New_With_Label;
      begin
         B.all.Set_Active (Is_Active => Shared_Parameters.Read.TDM_Bug_Enabled);
         B.all.On_Toggled (Call  => Callbacks.Switch_Bug'Access,
                           After => False);
         Grid_Add_Row (Left   => "TDM Bug",
                       Widget => B);
      end;

      declare
         B : constant Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         Window.Text_Entries (Initial_Velocity) := T;

         B.all.On_Clicked (Call  => Reset_Initial_Velocity'Access,
                           After => True);
         T.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Initial_Velocity,
                           With_Unit => False));
         T.all.On_Focus_Out_Event (Call  => Set_Initial_Velocity'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Initial Velocity",
                       Widget => T,
                       Right  => "m/s",
                       Reset  => B);
      end;

      declare
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         T.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Safe_Landing_Velocity,
                     With_Unit => False));
         T.all.Set_Editable (Is_Editable => False);

         Grid_Add_Row (Left   => "Safe Landing Velocity",
                       Widget => T,
                       Right  => "m/s");
      end;

      declare
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         T.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Target_Landing_Velocity,
                     With_Unit => False));
         T.all.Set_Editable (Is_Editable => False);

         Grid_Add_Row (Left   => "Target Landing Velocity",
                       Widget => T,
                       Right  => "m/s");
      end;

      declare
         B : constant Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         Window.Text_Entries (Initial_Altitude) := T;

         B.all.On_Clicked (Call  => Reset_Initial_Altitude'Access,
                           After => True);

         T.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Initial_Altitude,
                           With_Unit => False));
         T.all.On_Focus_Out_Event (Call  => Set_Initial_Altitude'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Initial Altitude",
                       Widget => T,
                       Right  => "m",
                       Reset  => B);
      end;

      declare
         B : constant Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         Window.Text_Entries (Dry_Mass) := T;

         B.all.On_Clicked (Call  => Reset_Dry_Mass'Access,
                           After => True);

         T.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Dry_Mass,
                           With_Unit => False));
         T.all.On_Focus_Out_Event (Call  => Set_Dry_Mass'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Spacecraft Dry Mass",
                       Widget => T,
                       Right  => "kg",
                       Reset  => B);
      end;

      declare
         B : constant Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         Window.Text_Entries (Initial_Fuel_Mass) := T;

         B.all.On_Clicked (Call  => Reset_Initial_Fuel_Mass'Access,
                           After => True);

         T.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Initial_Fuel_Mass,
                     With_Unit => False));
         T.all.On_Focus_Out_Event (Call  => Set_Initial_Fuel_Mass'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Initial Fuel Mass",
                       Widget => T,
                       Right  => "kg",
                       Reset  => B);
      end;

      declare
         B : constant Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         Window.Text_Entries (Fuel_Flow_Rate) := T;

         B.all.On_Clicked (Call  => Reset_Fuel_Flow_Rate'Access,
                           After => True);

         T.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Fuel_Flow_Rate,
                           With_Unit => False));
         T.all.On_Focus_Out_Event (Call  => Set_Fuel_Flow_Rate'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Fuel Flow Rate",
                       Widget => T,
                       Right  => "kg/s",
                       Reset  => B);
      end;

      declare
         B : constant Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         Window.Text_Entries (Shortest_On_Time) := T;

         B.all.On_Clicked (Call  => Reset_Shortest_On_Time'Access,
                           After => True);

         T.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Shortest_On_Time,
                           With_Unit => False));
         T.all.On_Focus_Out_Event (Call  => Set_Shortest_On_Time'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Shortest on-time",
                       Widget => T,
                       Right  => "ms",
                       Reset  => B);
      end;

      declare
         B : constant Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         Window.Text_Entries (Exhaust_Velocity) := T;

         B.all.On_Clicked (Call  => Reset_Exhaust_Velocity'Access,
                           After => True);

         T.all.Set_Text
           (Text => Image (Value     => Shared_Parameters.Read.Exhaust_Velocity,
                           With_Unit => False));
         T.all.On_Focus_Out_Event (Call  => Set_Exhaust_Velocity'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Exhaust Velocity",
                       Widget => T,
                       Right  => "m/s",
                       Reset  => B);
      end;

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
