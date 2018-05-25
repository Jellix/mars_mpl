with Gdk.RGBA;
with Gtk.Adjustment;
with Gtk.Button_Box;
with Gtk.Check_Button;
with Gtk.Frame;
with Gtk.Grid;
with Gtk.Label;
with Gtk.Spin_Button;
with Gtk.Style_Context;
with Shared_Parameters.Read;

separate (GUI)
function Create_Simulation_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Container : constant not null Gtk.Box.Gtk_Box :=
                 Gtk.Box.Gtk_Hbox_New (Homogeneous => False,
                                       Spacing     => 0);
begin
   Add_Labeled_Text_Entries :
   declare
      use type Glib.Gint;

      Grid : constant not null Gtk.Grid.Gtk_Grid := Gtk.Grid.Gtk_Grid_New;
      Row  : Glib.Gint                           := 0;

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
         if Left /= "" then
            Add_Left_Label :
            declare
               L : constant not null Gtk.Label.Gtk_Label :=
                     Gtk.Label.Gtk_Label_New (Str => Left);
            begin
               L.all.Set_Halign (Align => Gtk.Widget.Align_Start);
               Grid.all.Attach (Child  => L,
                                Left   => 0,
                                Top    => Row);
            end Add_Left_Label;
         end if;

         Grid.all.Attach (Child => Widget,
                          Left  => 1,
                          Top   => Row);

         if Right /= "" then
            Add_Right_Label :
            declare
               L : constant not null Gtk.Label.Gtk_Label :=
                     Gtk.Label.Gtk_Label_New (Str => Right);
            begin
               L.all.Set_Halign (Align => Gtk.Widget.Align_Start);
               Grid.all.Attach (Child => L,
                                Left  => 2,
                                Top   => Row);
            end Add_Right_Label;
         end if;

         if Reset /= null then
            Grid.all.Attach (Child => Reset,
                             Left  => 3,
                             Top   => Row);
         end if;

         Row := Row + 1;
      end Grid_Add_Row;
   begin
      Bug_Switch_Fields :
      declare
         B : constant not null Gtk.Check_Button.Gtk_Check_Button :=
               Gtk.Check_Button.Gtk_Check_Button_New_With_Label;
      begin
         B.all.Set_Active (Is_Active => Shared_Parameters.Read.TDM_Bug_Enabled);
         B.all.On_Toggled (Call  => Callbacks.Switch_Bug'Access,
                           After => False);
         Grid_Add_Row (Left   => "TDM Bug",
                       Widget => B);
      end Bug_Switch_Fields;

      Initial_Velocity_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment => Gtk.Adjustment.Gtk_Adjustment_New
                    (Value          => Glib.Gdouble (Shared_Parameters.Read.Initial_Velocity),
                     Lower          => Glib.Gdouble (Shared_Types.Meter_Per_Second'First),
                     Upper          => Glib.Gdouble (Shared_Types.Meter_Per_Second'Last),
                     Step_Increment => 0.001,
                     Page_Increment => 1.0,
                     Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Initial_Velocity) := T;

         B.all.On_Clicked (Call  => Reset_Initial_Velocity'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Initial_Velocity'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Initial Velocity",
                       Widget => T,
                       Right  => "m/s",
                       Reset  => B);
      end Initial_Velocity_Fields;

      Initial_Attitude_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment => Gtk.Adjustment.Gtk_Adjustment_New
                    (Value          => Glib.Gdouble (Shared_Parameters.Read.Initial_Attitude),
                     Lower          => Glib.Gdouble (Shared_Types.Degree'First),
                     Upper          => Glib.Gdouble (Shared_Types.Degree'Last),
                     Step_Increment => 0.001,
                     Page_Increment => 1.0,
                     Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Initial_Attitude) := T;

         B.all.On_Clicked (Call  => Reset_Initial_Attitude'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Initial_Attitude'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Entry Attitude",
                       Widget => T,
                       Right  => "°",
                       Reset  => B);
      end Initial_Attitude_Fields;

      Initial_Altitude_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment =>
                    Gtk.Adjustment.Gtk_Adjustment_New
                      (Value          => Glib.Gdouble (Shared_Parameters.Read.Initial_Altitude),
                       Lower          => Glib.Gdouble (Shared_Types.Meter'First),
                       Upper          => Glib.Gdouble (Shared_Types.Meter'Last),
                       Step_Increment => 0.001,
                       Page_Increment => 1.0,
                       Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Initial_Altitude) := T;

         B.all.On_Clicked (Call  => Reset_Initial_Altitude'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Initial_Altitude'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Initial Altitude",
                       Widget => T,
                       Right  => "m",
                       Reset  => B);
      end Initial_Altitude_Fields;

      Dry_Mass_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment =>
                    Gtk.Adjustment.Gtk_Adjustment_New
                      (Value          => Glib.Gdouble (Shared_Parameters.Read.Dry_Mass),
                       Lower          => Glib.Gdouble (Shared_Types.Vehicle_Mass'First),
                       Upper          => Glib.Gdouble (Shared_Types.Vehicle_Mass'Last),
                       Step_Increment => 0.001,
                       Page_Increment => 1.0,
                       Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Dry_Mass) := T;

         B.all.On_Clicked (Call  => Reset_Dry_Mass'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Dry_Mass'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Spacecraft Dry Mass",
                       Widget => T,
                       Right  => "kg",
                       Reset  => B);
      end Dry_Mass_Fields;

      Initial_Fuel_Mass_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment =>
                    Gtk.Adjustment.Gtk_Adjustment_New
                      (Value          => Glib.Gdouble (Shared_Parameters.Read.Initial_Fuel_Mass),
                       Lower          => Glib.Gdouble (Shared_Types.Fuel_Mass'First),
                       Upper          => Glib.Gdouble (Shared_Types.Fuel_Mass'Last),
                       Step_Increment => 0.001,
                       Page_Increment => 1.0,
                       Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Initial_Fuel_Mass) := T;

         B.all.On_Clicked (Call  => Reset_Initial_Fuel_Mass'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Initial_Fuel_Mass'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Initial Fuel Mass",
                       Widget => T,
                       Right  => "kg",
                       Reset  => B);
      end Initial_Fuel_Mass_Fields;

      Fuel_Flow_Rate_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment =>
                    Gtk.Adjustment.Gtk_Adjustment_New
                      (Value          => Glib.Gdouble (Shared_Parameters.Read.Fuel_Flow_Rate),
                       Lower          => Glib.Gdouble (Shared_Types.Kilogram_Per_Second'First),
                       Upper          => Glib.Gdouble (Shared_Types.Kilogram_Per_Second'Last),
                       Step_Increment => 0.001,
                       Page_Increment => 1.0,
                       Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Fuel_Flow_Rate) := T;

         B.all.On_Clicked (Call  => Reset_Fuel_Flow_Rate'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Fuel_Flow_Rate'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Fuel Flow Rate",
                       Widget => T,
                       Right  => "kg/s",
                       Reset  => B);
      end Fuel_Flow_Rate_Fields;

      Shortest_On_Time_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment =>
                    Gtk.Adjustment.Gtk_Adjustment_New
                      (Value          => Glib.Gdouble (Shared_Parameters.Read.Shortest_On_Time),
                       Lower          => Glib.Gdouble (Shared_Types.On_Time'First),
                       Upper          => Glib.Gdouble (Shared_Types.On_Time'Last),
                       Step_Increment => 0.001,
                       Page_Increment => 1.0,
                       Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Shortest_On_Time) := T;

         B.all.On_Clicked (Call  => Reset_Shortest_On_Time'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Shortest_On_Time'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Shortest on-time",
                       Widget => T,
                       Right  => "ms",
                       Reset  => B);
      end Shortest_On_Time_Fields;

      Exhaust_Velocity_Fields :
      declare
         B : constant not null Gtk.Button.Gtk_Button :=
               Gtk.Button.Gtk_Button_New_With_Label (Label => "Reset");
         T : constant not null Gtk.Spin_Button.Gtk_Spin_Button :=
               Gtk.Spin_Button.Gtk_Spin_Button_New
                 (Adjustment =>
                    Gtk.Adjustment.Gtk_Adjustment_New
                      (Value          => Glib.Gdouble (Shared_Parameters.Read.Exhaust_Velocity),
                       Lower          => Glib.Gdouble (Shared_Types.Meter_Per_Second'First),
                       Upper          => Glib.Gdouble (Shared_Types.Meter_Per_Second'Last),
                       Step_Increment => 0.001,
                       Page_Increment => 1.0,
                       Page_Size      => 0.0),
                  Climb_Rate => 1.0,
                  The_Digits => 3);
      begin
         Window.Text_Entries (Exhaust_Velocity) := T;

         B.all.On_Clicked (Call  => Reset_Exhaust_Velocity'Access,
                           After => True);

         T.all.Set_Numeric (Numeric => True);
         T.all.On_Focus_Out_Event (Call  => Set_Exhaust_Velocity'Access,
                                   After => True);

         Grid_Add_Row (Left   => "Exhaust Velocity",
                       Widget => T,
                       Right  => "m/s",
                       Reset  => B);
      end Exhaust_Velocity_Fields;

      Safe_Landing_Velocity_Fields :
      declare
         T : constant not null Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         T.all.Set_Text
           (Text =>
              Image (Value     => Shared_Parameters.Read.Safe_Landing_Velocity,
                     With_Unit => False));
         T.all.Set_Editable (Is_Editable => False);

         Grid_Add_Row (Left   => "Safe Landing Velocity",
                       Widget => T,
                       Right  => "m/s");
      end Safe_Landing_Velocity_Fields;

      Target_Landing_Velocity_Fields :
      declare
         T : constant not null Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry_New;
      begin
         T.all.Set_Text
           (Text =>
              Image
                (Value     => Shared_Parameters.Read.Target_Landing_Velocity,
                 With_Unit => False));
         T.all.Set_Editable (Is_Editable => False);

         Grid_Add_Row (Left   => "Target Landing Velocity",
                       Widget => T,
                       Right  => "m/s");
      end Target_Landing_Velocity_Fields;

      Add_Parameter_Frame :
      declare
         Parameter_Frame : constant not null Gtk.Frame.Gtk_Frame :=
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

   Container.all.Pack_Start (Child  => Window.SIMon_Says,
                             Expand => True);

   Add_Time_And_Sim_Control :
   declare
      Box : constant not null Gtk.Box.Gtk_Vbox :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);
   begin
      Container.all.Pack_Start (Child  => Box,
                                Expand => False);

      Add_Mission_Clock :
      declare
         Context : constant not null Gtk.Style_Context.Gtk_Style_Context :=
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
            Button_Box : constant not null Gtk.Button_Box.Gtk_Button_Box :=
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
