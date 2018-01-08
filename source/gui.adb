with Ada.Exceptions;

with Global;
with Touchdown_Monitor;

with Cairo;

with Glib;

with Gtk.Box;
with Gtk.Enums.String_Lists;
with Gtk.Frame;
with Gtk.Gauge.Round_270_60s;
with Gtk.Gauge.Elliptic_180;
with Gtk.Gauge.LED_Round;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Layered.Needle;
with Gtk.Main;
with Gtk.Missed;
with Gtk.Oscilloscope;
with Gtk.Widget;
with Gtk.Window;

package body GUI is

   type Leg_Switches is
     array (Landing_Legs.Legs_Index) of Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;

   type Dynamic_Elements is
      record
         Leg_Led      : Leg_Switches;
         Thruster_Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
         Altitude     : Gtk.GEntry.Gtk_Entry;
         Velocity     : Gtk.GEntry.Gtk_Entry;
      end record;

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Elements          : Dynamic_Elements;
         Oscilloscope      : Gtk.Oscilloscope.Gtk_Oscilloscope;
         Altitude_Channel  : Gtk.Oscilloscope.Channel_Number;
         Velocity_Channel  : Gtk.Oscilloscope.Channel_Number;
         Touchdown_Channel : Gtk.Oscilloscope.Channel_Number;
         Thruster_Channel  : Gtk.Oscilloscope.Channel_Number;
         Tachometer        : Gtk.Gauge.Elliptic_180.Gtk_Gauge_Elliptic_180;
         Altimeter_1000    : Gtk.Gauge.Round_270_60s.Gtk_Gauge_Round_270_60s;
         Altimeter_100     : access Gtk.Layered.Needle.Needle_Layer;
         Altimeter_10      : access Gtk.Layered.Needle.Needle_Layer;
      end record;
   type Main_Window is access all Main_Window_Record'Class;

   type Scaling is
      record
         Texts  : access Gtk.Enums.String_Lists.Controlled_String_List;
         Factor : Glib.Gdouble;
      end record;

   use type Gtk.Enums.String_Lists.Controlled_String_List;
   Altitude_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
           ("0" / "1" / "2" / "3" / "4" / "5" / "6" / "7" / "8" / "9" / "10"),
         Factor => 10000.0);
   Velocity_Scale : constant Scaling
     := (Texts  =>
            new Gtk.Enums.String_Lists.Controlled_String_List'
             ("0" / "20" / "40" / "60" / "80" / "100" /
              "120" / "140" / "160" / "180" / "200"),
         Factor => 200.0);

   package Windows_CB is
     new Gtk.Handlers.Callback (Widget_Type => Main_Window_Record);

   task GUI_Task;

   procedure Exit_Main (Win : access Main_Window_Record'Class);
   procedure Exit_Main (Win : access Main_Window_Record'Class) is
   begin
      Global.Log (Message => "Main window destroyed.");
      abort GUI_Task;
      Win.all.Destroy;
      Gtk.Main.Main_Quit;
   end Exit_Main;

   procedure Feed_Values (Win          : in Main_Window_Record;
                          Update_State : in State);
   procedure Feed_Values (Win          : in Main_Window_Record;
                          Update_State : in State) is
      use type Altimeter.Altitude;
      use type Altimeter.Velocity;
      use type Glib.Gdouble;
      use type Landing_Legs.Leg_State;
      use type Thrusters.State;
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
      Win.Altimeter_1000.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Altitude) /
             Altitude_Scale.Factor);

      declare
         Hundreds : constant Glib.Gdouble :=
                      Glib.Gdouble'Remainder
                        (Glib.Gdouble (Update_State.Altitude), 1000.0);
      begin
         Win.Altimeter_100.all.Set_Value
           (Value =>
              10.0 *
                (if Hundreds < 0.0
                 then 1000.0 + Hundreds
                 else Hundreds) / Altitude_Scale.Factor);
      end;

      declare
         Tens : constant Glib.Gdouble :=
                  Glib.Gdouble'Remainder
                    (Glib.Gdouble (Update_State.Altitude), 100.0);
      begin
         Win.Altimeter_10.all.Set_Value
           (Value =>
              100.0 *
                (if Tens < 0.0
                 then 100.0 + Tens
                 else Tens) / Altitude_Scale.Factor);
      end;
      Win.Altimeter_1000.all.Queue_Draw;

      -- Velocity
      DE.Velocity.all.Set_Text
        (Text => Altimeter.Image (Update_State.Velocity));
      Win.Tachometer.all.Set_Value
        (Value =>
           Glib.Gdouble (abs Update_State.Velocity) /
             Velocity_Scale.Factor);
      Win.Tachometer.all.Queue_Draw;

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

         Plotter.Feed
           (Channel => Win.Touchdown_Channel,
            V       =>
              Glib.Gdouble
                (Boolean'Pos ((for some L of Update_State.Legs =>
                                   L = Landing_Legs.Touched_Down))));
         Plotter.Feed
           (Channel => Win.Thruster_Channel,
            V       =>
              Glib.Gdouble (Thrusters.State'Pos (Update_State.Thruster)));
      end;
   end Feed_Values;

   procedure Initialize (Window : in out Main_Window_Record'Class);
   procedure Initialize (Window : in out Main_Window_Record'Class)
   is

      function Create_Altimeter return not null access
        Gtk.Widget.Gtk_Widget_Record'Class;
      function Create_LEDs return not null access
        Gtk.Widget.Gtk_Widget_Record'Class;

      --
      function Create_Altimeter return not null access
        Gtk.Widget.Gtk_Widget_Record'Class
      is
         Gauge      : Gtk.Gauge.Round_270_60s.Gtk_Gauge_Round_270_60s;
         Needle_100 : access Gtk.Layered.Needle.Needle_Layer;
         Needle_10  : access Gtk.Layered.Needle.Needle_Layer;
         use type Glib.Gdouble;
      begin
         Gtk.Gauge.Round_270_60s.Gtk_New
           (Widget  => Gauge,
            Texts   => Altitude_Scale.Texts.all,
            Sectors =>
              Positive
                (Gtk.Enums.String_List.Length
                     (+Altitude_Scale.Texts.all)) - 1);

         Needle_100 :=
           Gtk.Layered.Needle.Add_Needle
             (Under       => Gauge,
              Center      => (0.0, 0.0),
              Tip_Cap     => Cairo.Cairo_Line_Cap_Round,
              Adjustment  => null,
              Tip_Length  => 0.39,
              Tip_Width   => 0.01,
              Rear_Length => -0.165,
              Rear_Width  => 0.01,
              Color       => Gtk.Missed.RGB (0.0, 0.5, 0.0),
              Scaled      => True);
         Needle_10 :=
           Gtk.Layered.Needle.Add_Needle
             (Under       => Gauge,
              Center      => (0.0, 0.0),
              Tip_Cap     => Cairo.Cairo_Line_Cap_Round,
              Adjustment  => null,
              Tip_Length  => 0.35,
              Tip_Width   => 0.01,
              Rear_Length => -0.165,
              Rear_Width  => 0.03,
              Color       => Gtk.Missed.RGB (0.5, 0.5, 0.0),
              Scaled      => True);
         Window.Altimeter_1000 := Gauge;
         Window.Altimeter_100  := Needle_100;
         Window.Altimeter_10   := Needle_10;

         return Gauge;
      end Create_Altimeter;

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
                     Gtk.Box.Gtk_New_Vbox (Box => VBox2);
                     Altitude_Frame.all.Add (Widget => VBox2);
                     VBox2.all.Pack_Start (Child => Create_Altimeter);

                     declare
                        Text : Gtk.GEntry.Gtk_Entry;
                     begin
                        Gtk.GEntry.Gtk_New (The_Entry => Text);
                        VBox2.all.Pack_End (Child => Text);
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
                        VBox2.all.Pack_Start (Child => Gauge);
                        Window.Tachometer := Gauge;
                     end;

                     declare
                        Text : Gtk.GEntry.Gtk_Entry;
                     begin
                        Gtk.GEntry.Gtk_New (The_Entry => Text);
                        VBox2.all.Pack_End (Child => Text);
                        Window.Elements.Velocity := Text;
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
                  Window.Touchdown_Channel := Plot.all.Add_Channel (Group => G);
                  Window.Thruster_Channel  := Plot.all.Add_Channel (Group => G);
               end;

               Plot.all.Set_Manual_Sweep (False);
               Plot.all.Set_Time_Axis (Sweeper => Gtk.Oscilloscope.Lower,
                                       Visible => True,
                                       As_Time => True);
            end;
         end;
      end;
   end Initialize;

   protected State_Update is
      procedure Set_State (New_State : State);
      entry Wait_For_Update (New_State : out State);
   private
      State_Changed : Boolean := True;
      Current_State : State;
   end State_Update;

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

   task body GUI_Task is
      Win          : Main_Window;
      Update_State : State;
   begin
      Gtk.Main.Init;
      Win := new Main_Window_Record;
      Initialize (Window => Win.all);
      Windows_CB.Connect
        (Widget => Win,
         Name   => "destroy",
         Marsh  => Windows_CB.To_Marshaller (Exit_Main'Access));

      Win.all.Show_All;

      loop
         State_Update.Wait_For_Update (New_State => Update_State);

         if not Update_State.Terminated then
            Feed_Values (Win          => Main_Window_Record (Win.all),
                         Update_State => Update_State);
         else
            Win.all.Oscilloscope.all.Set_Time
              (Sweeper => Gtk.Oscilloscope.Lower,
               Stamp   => Update_State.Time_Stamp);
         end if;

         while Gtk.Main.Events_Pending loop
            if Gtk.Main.Main_Iteration_Do (Blocking => False) then
               null;
            end if;
         end loop;
      end loop;
   exception
      when E : others =>
         Global.Log (Ada.Exceptions.Exception_Information (E));
   end GUI_Task;

   procedure Update (New_State : State) is
   begin
      State_Update.Set_State (New_State);
   end Update;
end GUI;
