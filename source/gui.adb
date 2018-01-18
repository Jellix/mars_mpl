with Ada.Exceptions;
with Ada.Real_Time;

with Global;
with Touchdown_Monitor;

with Cairo;

with Glib;

with Gtk.Box;
with Gtk.Button;
with Gtk.Enums.String_Lists;
with Gtk.Frame;
with Gtk.Gauge.Altimeter;
with Gtk.Gauge.Elliptic_180;
with Gtk.Gauge.LED_Round;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Hbutton_Box;
with Gtk.Label;
with Gtk.Layered.Label;
with Gtk.Main;
with Gtk.Meter.Angular_90;
with Gtk.Missed;
with Gtk.Oscilloscope;
with Gtk.Widget;
with Gtk.Window;

with Pango.Cairo.Fonts;

package body GUI is

   use type Altimeter.Altitude;
   use type Altimeter.Velocity;
   use type Glib.Gdouble;
   use type Gtk.Enums.String_Lists.Controlled_String_List;
   use type Landing_Legs.Leg_State;
   use type Thrusters.State;

   type Leg_Switches is
     array (Landing_Legs.Legs_Index) of Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;

   type Dynamic_Elements is
      record
         Leg_Led      : Leg_Switches;
         Thruster_Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
         Altitude     : Gtk.GEntry.Gtk_Entry;
         Velocity     : Gtk.GEntry.Gtk_Entry;
         Fuel         : Gtk.GEntry.Gtk_Entry;
      end record;

   type Legs_Channels is array (Landing_Legs.Legs_Index) of
     Gtk.Oscilloscope.Channel_Number;

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Elements          : Dynamic_Elements;
         Oscilloscope      : Gtk.Oscilloscope.Gtk_Oscilloscope;
         Altitude_Channel  : Gtk.Oscilloscope.Channel_Number;
         Velocity_Channel  : Gtk.Oscilloscope.Channel_Number;
         Touchdown_Channel : Legs_Channels;
         Thruster_Channel  : Gtk.Oscilloscope.Channel_Number;
         Tachometer        : Gtk.Gauge.Elliptic_180.Gtk_Gauge_Elliptic_180;
         Altimeter         : Gtk.Gauge.Altimeter.Gtk_Gauge_Altimeter;
         Fuel_Scale        : Gtk.Meter.Angular_90.Gtk_Meter_Angular_90;
      end record;
   type Main_Window is access all Main_Window_Record'Class;

   type Scaling is
      record
         Texts  : access Gtk.Enums.String_Lists.Controlled_String_List;
         Factor : Glib.Gdouble;
      end record;

   Altitude_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
           ("0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9"),
         Factor => 10000.0);
   Fuel_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
             ("0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / "10"),
         Factor => 1000.0);
   Velocity_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
             ("0" / "20" / "40" / "60" / "80" / "100" /
              "120" / "140" / "160" / "180" / "200"),
         Factor => 200.0);

   package Windows_CB is
     new Gtk.Handlers.Callback (Widget_Type => Main_Window_Record);

   package Buttons_CB is
     new Gtk.Handlers.Callback (Widget_Type => Gtk.Button.Gtk_Button_Record);

   protected State_Update is
      procedure Set_State (New_State : State);
      entry Wait_For_Update (New_State : out State);
   private
      State_Changed : Boolean := True;
      Current_State : State;
   end State_Update;

   task GUI_Task;

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class);
   procedure Exit_Main (Win    : access Main_Window_Record'Class);
   procedure Quit_GUI;

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Quit_GUI;
   end Exit_Main;

   procedure Exit_Main (Win : access Main_Window_Record'Class)
   is
      pragma Unreferenced (Win);
   begin
      Quit_GUI;
   end Exit_Main;

   procedure Feed_Values (Win          : in Main_Window_Record;
                          Update_State : in State);
   procedure Feed_Values (Win          : in Main_Window_Record;
                          Update_State : in State) is
      DE : Dynamic_Elements renames Win.Elements;
   begin
      --  LEDs
      for Leg in DE.Leg_Led'Range loop
         DE.Leg_Led (Leg).all.Set_State
           (State => Update_State.Legs (Leg) = Landing_Legs.Touched_Down);
         DE.Leg_Led (Leg).all.Queue_Draw;
      end loop;
      DE.Thruster_Led.all.Set_State
        (State => Update_State.Thruster = Thrusters.Enabled);
      DE.Thruster_Led.all.Queue_Draw;

      -- Altitude
      DE.Altitude.all.Set_Text
        (Text => Altimeter.Image (Update_State.Altitude));
      Win.Altimeter.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Altitude) / Altitude_Scale.Factor);
      Win.Altimeter.all.Queue_Draw;

      -- Velocity
      DE.Velocity.all.Set_Text
        (Text => Altimeter.Image (Update_State.Velocity));
      Win.Tachometer.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Velocity) / Velocity_Scale.Factor);
      Win.Tachometer.all.Queue_Draw;

      --  Fuel
      DE.Fuel.all.Set_Text (Text => Engine.Image (Update_State.Fuel));
      Win.Fuel_Scale.all.Set_Value
        (Value => Glib.Gdouble (Update_State.Fuel) / Fuel_Scale.Factor);
      Win.Fuel_Scale.all.Queue_Draw;

      --  Data plots
      declare
         Plotter : Gtk.Oscilloscope.Gtk_Oscilloscope_Record renames
                     Gtk.Oscilloscope.Gtk_Oscilloscope_Record
                       (Win.Oscilloscope.all);
      begin
         -- Data plot
         Plotter.Feed
           (Channel => Win.Altitude_Channel,
            V       => Glib.Gdouble (Update_State.Altitude));
         Plotter.Feed
           (Channel => Win.Velocity_Channel,
            V       => Glib.Gdouble (Update_State.Velocity));

         for Leg in Landing_Legs.Legs_Index loop
            Plotter.Feed
              (Channel => Win.Touchdown_Channel (Leg),
               V       =>
                 Glib.Gdouble
                   (Landing_Legs.Legs_Index'Pos (Leg) *
                    Boolean'Pos
                      (Update_State.Legs (Leg) = Landing_Legs.Touched_Down)));
         end loop;

         Plotter.Feed
           (Channel => Win.Thruster_Channel,
            V       =>
              Glib.Gdouble (Thrusters.State'Pos (Update_State.Thruster)));
      end;
   end Feed_Values;

   procedure Initialize (Window : in out Main_Window_Record'Class);
   procedure Initialize (Window : in out Main_Window_Record'Class)
   is

      function Create_LEDs return not null access
        Gtk.Widget.Gtk_Widget_Record'Class;

      --
      function Create_LEDs return not null access
        Gtk.Widget.Gtk_Widget_Record'Class
      is
         Container : Gtk.Box.Gtk_Vbox;

         function Labeled_LED
           (The_LED     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
            Description : String) return not null access
           Gtk.Widget.Gtk_Widget_Record'Class;

         function Labeled_LED
           (The_LED     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
            Description : String) return not null access
           Gtk.Widget.Gtk_Widget_Record'Class
         is
            LED_Box : Gtk.Box.Gtk_Hbox;
            Label   : Gtk.Label.Gtk_Label;
         begin
            Gtk.Box.Gtk_New_Hbox (LED_Box);
            LED_Box.all.Set_Homogeneous (Homogeneous => True);
            Gtk.Label.Gtk_New (Label => Label,
                               Str   => Description);
            LED_Box.all.Pack_Start (Label);
            LED_Box.all.Pack_Start (The_LED);

            return LED_Box;
         end Labeled_LED;

      begin
         Gtk.Box.Gtk_New_Vbox (Box => Container);
         Container.all.Set_Size_Request (Width  => 100,
                                         Height => 100);

         for Leg in Landing_Legs.Legs_Index loop
            declare
               Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
            begin
               Gtk.Gauge.LED_Round.Gtk_New
                 (Widget        => Led,
                  On_Color      => Gtk.Missed.RGB (0.0, 1.0, 0.0),
                  Off_Color     => Gtk.Missed.RGB (0.5, 0.5, 0.5),
                  Border_Shadow => Gtk.Enums.Shadow_Etched_Out);
               Container.all.Pack_Start
                 (Child =>
                    Labeled_LED
                      (The_LED     => Led,
                       Description =>
                         "Landing Leg"
                       & Natural'Image
                         (Landing_Legs.Legs_Index'Pos (Leg) + 1)));
               Window.Elements.Leg_Led (Leg) := Led;
            end;
         end loop;

         declare
            Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
         begin
            Gtk.Gauge.LED_Round.Gtk_New
              (Widget        => Led,
               On_Color      => Gtk.Missed.RGB (1.0, 1.0, 0.5),
               Off_Color     => Gtk.Missed.RGB (0.5, 0.5, 0.5),
               Border_Shadow => Gtk.Enums.Shadow_Etched_Out);
            Container.all.Pack_Start
              (Child => Labeled_LED (The_LED     => Led,
                                     Description => "Thruster"));
            Window.Elements.Thruster_Led := Led;
         end;

         declare
            Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
         begin
            Gtk.Gauge.LED_Round.Gtk_New
              (Widget        => Led,
               On_Color      => Gtk.Missed.RGB (1.0, 0.0, 0.0),
               Off_Color     => Gtk.Missed.RGB (0.0, 1.0, 0.0),
               Border_Shadow => Gtk.Enums.Shadow_Etched_Out);
            Container.all.Pack_Start
              (Child => Labeled_LED (The_LED     => Led,
                                     Description => "Bug"));
            Led.all.Set_State (Touchdown_Monitor.Bug_Enabled);
         end;

         return Container;
      end Create_LEDs;

      Label_Font        : constant Pango.Cairo.Fonts.Pango_Cairo_Font :=
                            Pango.Cairo.Fonts.Create_Toy
                              (Family => "arial",
                               Slant  => Cairo.Cairo_Font_Slant_Normal,
                               Weight => Cairo.Cairo_Font_Weight_Bold);
      Label_Font_Italic : constant Pango.Cairo.Fonts.Pango_Cairo_Font :=
                            Pango.Cairo.Fonts.Create_Toy
                              (Family => "arial",
                               Slant  => Cairo.Cairo_Font_Slant_Italic,
                               Weight => Cairo.Cairo_Font_Weight_Bold);
   begin
      Window.Initialize (The_Type => Gtk.Enums.Window_Toplevel);
      Window.Set_Title (Title => "Mars MPL simulation");

      declare
         VBox : Gtk.Box.Gtk_Box;
      begin
         Gtk.Box.Gtk_New_Vbox (Box => VBox, Homogeneous => False, Spacing => 0);
         Window.Add (Widget => VBox);

         declare
            Frame : Gtk.Frame.Gtk_Frame;
         begin
            Gtk.Frame.Gtk_New (Frame => Frame, Label => "Sensor Signals");
            VBox.all.Pack_Start (Frame);

            declare
               HBox : Gtk.Box.Gtk_Hbox;
            begin
               Gtk.Box.Gtk_New_Hbox (Box => HBox);
               Frame.all.Add (Widget => HBox);

               declare
                  VBox2 : Gtk.Box.Gtk_Vbox;
               begin
                  Gtk.Box.Gtk_New_Vbox (Box => VBox2);
                  HBox.all.Pack_Start (Child => VBox2);
                  VBox2.all.Pack_Start (Child => Create_LEDs);
               end;

               declare
                  Altitude_Frame : Gtk.Frame.Gtk_Frame;
               begin
                  Gtk.Frame.Gtk_New (Frame => Altitude_Frame,
                                     Label => "Altitude");
                  HBox.all.Pack_Start (Child => Altitude_Frame);
                  Altitude_Frame.all.Set_Size_Request (Width  => 400,
                                                       Height => 400);

                  declare
                     VBox2 : Gtk.Box.Gtk_Vbox;
                  begin
                     Gtk.Box.Gtk_New_Vbox (Box         => VBox2,
                                           Homogeneous => False,
                                           Spacing     => 0);
                     Altitude_Frame.all.Add (Widget => VBox2);

                     declare
                        Gauge : Gtk.Gauge.Altimeter.Gtk_Gauge_Altimeter;
                     begin
                        Gtk.Gauge.Altimeter.Gtk_New
                          (Widget  => Gauge,
                           Texts   => Altitude_Scale.Texts.all,
                           Sectors =>
                             Positive
                               (Gtk.Enums.String_List.Length
                                    (+Altitude_Scale.Texts.all)));
                        Gtk.Layered.Label.Add_Label
                          (Under    => Gauge.all.Get_Cache,
                           Text     => "x 1000 m",
                           Location => (0.0175, 0.175),
                           Face     => Label_Font,
                           Height   => 0.04,
                           Stretch  => 1.0,
                           Mode     => Gtk.Layered.Moved_Centered,
                           Color    => Gtk.Missed.RGB (1.0, 1.0, 1.0),
                           Angle    => 0.0,
                           Skew     => 0.0,
                           Markup   => False,
                           Scaled   => True);
                        Window.Altimeter := Gauge;

                        VBox2.all.Pack_Start (Child  => Gauge,
                                              Expand => True);
                     end;

                     declare
                        Text : Gtk.GEntry.Gtk_Entry;
                     begin
                        Gtk.GEntry.Gtk_New (The_Entry => Text);
                        VBox2.all.Pack_End (Child => Text,
                                            Expand => False);
                        Window.Elements.Altitude := Text;
                        Text.all.Set_Editable (Is_Editable => False);
                     end;
                  end;
               end;

               declare
                  Velocity_Frame : Gtk.Frame.Gtk_Frame;
               begin
                  Gtk.Frame.Gtk_New (Frame => Velocity_Frame,
                                     Label => "Velocity");
                  HBox.all.Pack_Start (Child => Velocity_Frame);
                  Velocity_Frame.all.Set_Size_Request (Width  => 400,
                                                       Height => 400);

                  declare
                     VBox2 : Gtk.Box.Gtk_Vbox;
                  begin
                     Gtk.Box.Gtk_New_Vbox (Box => VBox2);
                     Velocity_Frame.all.Add (Widget => VBox2);

                     declare
                        Gauge : Gtk.Gauge.Elliptic_180.Gtk_Gauge_Elliptic_180;
                     begin
                        Gtk.Gauge.Elliptic_180.Gtk_New
                          (Widget  => Gauge,
                           Texts   => Velocity_Scale.Texts.all,
                           Sectors =>
                             Positive
                               (Gtk.Enums.String_List.Length
                                    (+Velocity_Scale.Texts.all)) - 1);
                        Gtk.Layered.Label.Add_Label
                          (Under    => Gauge.all.Get_Cache,
                           Text     => "m/s",
                           Location => (0.0175, 0.1),
                           Face     => Label_Font_Italic,
                           Height   => 0.03,
                           Stretch  => 0.9,
                           Mode     => Gtk.Layered.Moved_Centered,
                           Color    => Gtk.Missed.RGB (1.0, 0.6, 0.0),
                           Angle    => 0.0,
                           Skew     => 0.0,
                           Markup   => False,
                           Scaled   => True);
                        VBox2.all.Pack_Start (Child  => Gauge,
                                              Expand => True);
                        Window.Tachometer := Gauge;
                     end;

                     declare
                        Text : Gtk.GEntry.Gtk_Entry;
                     begin
                        Gtk.GEntry.Gtk_New (The_Entry => Text);
                        VBox2.all.Pack_End (Child  => Text,
                                            Expand => False);
                        Window.Elements.Velocity := Text;
                        Text.all.Set_Editable (Is_Editable => False);
                     end;
                  end;
               end;

               declare
                  Fuel_Frame : Gtk.Frame.Gtk_Frame;
               begin
                  Gtk.Frame.Gtk_New (Frame => Fuel_Frame,
                                     Label => "Fuel");
                  HBox.all.Pack_Start (Child => Fuel_Frame);
                  Fuel_Frame.all.Set_Size_Request (Width  => 400,
                                                   Height => 400);

                  declare
                     VBox2 : Gtk.Box.Gtk_Vbox;
                  begin
                     Gtk.Box.Gtk_New_Vbox (Box => VBox2);
                     Fuel_Frame.all.Add (Widget => VBox2);

                     declare
                        Gauge : Gtk.Meter.Angular_90.Gtk_Meter_Angular_90;
                     begin
                        Gtk.Meter.Angular_90.Gtk_New
                          (Widget  => Gauge,
                           Texts   => Fuel_Scale.Texts.all,
                           Sectors =>
                             Positive
                               (Gtk.Enums.String_List.Length
                                    (+Fuel_Scale.Texts.all)) - 1);
                        Gtk.Layered.Label.Add_Label
                          (Under    => Gauge.all.Get_Cache,
                           Text     => "x 100 kg",
                           Location => (0.0175, 0.1),
                           Face     => Label_Font_Italic,
                           Height   => 0.03,
                           Stretch  => 0.9,
                           Mode     => Gtk.Layered.Moved_Centered,
                           Color    => Gtk.Missed.RGB (0.0, 0.0, 0.0),
                           Angle    => 0.0,
                           Skew     => 0.0,
                           Markup   => False,
                           Scaled   => True);
                        VBox2.all.Pack_Start (Child  => Gauge,
                                              Expand => True);
                        Window.Fuel_Scale := Gauge;
                     end;

                     declare
                        Text : Gtk.GEntry.Gtk_Entry;
                     begin
                        Gtk.GEntry.Gtk_New (The_Entry => Text);
                        VBox2.all.Pack_End (Child  => Text,
                                            Expand => False);
                        Window.Elements.Fuel := Text;
                        Text.all.Set_Editable (Is_Editable => False);
                     end;
                  end;
               end;
            end;
         end;

         declare
            Frame : Gtk.Frame.Gtk_Frame;
         begin
            Gtk.Frame.Gtk_New (Frame => Frame, Label => "Timeline");
            Frame.all.Set_Size_Request (Width => 800, Height => 200);
            VBox.all.Pack_Start (Child => Frame);

            declare
               Plot : Gtk.Oscilloscope.Gtk_Oscilloscope;
            begin
               Gtk.Oscilloscope.Gtk_New (Widget => Plot);
               Frame.all.Add (Widget => Plot);

               Window.Oscilloscope := Plot;

               Window.Altitude_Channel := Plot.all.Add_Channel;
               Window.Velocity_Channel := Plot.all.Add_Channel;

               declare
                  G : Gtk.Oscilloscope.Group_Number;
               begin
                  G := Plot.all.Add_Group (Name => "Signals");

                  for Leg in Landing_Legs.Legs_Index loop
                     Window.Touchdown_Channel (Leg) :=
                       Plot.all.Add_Channel (Group => G);
                  end loop;

                  Window.Thruster_Channel  := Plot.all.Add_Channel (Group => G);
               end;

               Plot.all.Set_Manual_Sweep (False);
               Plot.all.Set_Time_Axis (Sweeper => Gtk.Oscilloscope.Lower,
                                       Visible => True,
                                       As_Time => True);
            end;
         end;

         declare
            Button_Box   : Gtk.Hbutton_Box.Gtk_Hbutton_Box;
            Start_Button : Gtk.Button.Gtk_Button;
            Abort_Button : Gtk.Button.Gtk_Button;
            Exit_Button  : Gtk.Button.Gtk_Button;
         begin
            Gtk.Hbutton_Box.Gtk_New (Widget => Button_Box);

            Gtk.Button.Gtk_New (Button => Start_Button,
                                Label  => "Start");
            Button_Box.all.Add (Widget => Start_Button);

            Gtk.Button.Gtk_New (Button => Abort_Button,
                                Label  => "Abort");
            Button_Box.all.Add (Widget => Abort_Button);

            Gtk.Button.Gtk_New (Button => Exit_Button,
                                Label  => "Exit");
            Button_Box.all.Add (Widget => Exit_Button);

            VBox.all.Pack_End (Child => Button_Box);
            Buttons_CB.Connect
              (Widget => Exit_Button,
               Name   => "clicked",
               Marsh  => Buttons_CB.To_Marshaller (Exit_Main'Access));
         end;
      end;
   end Initialize;

   procedure Quit_GUI is
   begin
      if not Aborted then
         Aborted := True;
         Global.Log (Message => "Quitting GUI...");
      end if;
   end Quit_GUI;

   protected body State_Update is
      procedure Set_State (New_State : State) is
      begin
         Current_State := New_State;
         State_Changed := True;
      end Set_State;

      entry Wait_For_Update (New_State : out State) when State_Changed is
      begin
         New_State     := Current_State;
         State_Changed := False;
      end Wait_For_Update;
   end State_Update;

   procedure Update (New_State : State) is
   begin
      State_Update.Set_State (New_State);
   end Update;

   task body GUI_Task is
      Win          : Main_Window;
      Update_State : State;
   begin
      Aborted := False;

      Gtk.Main.Init;
      Win := new Main_Window_Record;
      Initialize (Window => Win.all);

      Windows_CB.Connect
        (Widget => Win,
         Name   => "destroy",
         Marsh  => Windows_CB.To_Marshaller (Exit_Main'Access));

      Win.all.Show_All;

      declare
         Last_Update : Ada.Real_Time.Time := Global.Start_Time;
      begin
         loop
            State_Update.Wait_For_Update (New_State => Update_State);

            if not Update_State.Terminated then
               Feed_Values (Win          => Main_Window_Record (Win.all),
                            Update_State => Update_State);
               Last_Update := Ada.Real_Time.Clock;
            else
               Win.all.Oscilloscope.all.Set_Time
                 (Sweeper => Gtk.Oscilloscope.Lower,
                  Stamp   => Last_Update);
            end if;

            while Gtk.Main.Events_Pending loop
               if Gtk.Main.Main_Iteration_Do (Blocking => False) then
                  null;
               end if;
            end loop;

            exit when Aborted;
         end loop;
      end;
   exception
      when E : others =>
         Global.Log (Ada.Exceptions.Exception_Information (E));
   end GUI_Task;

end GUI;
