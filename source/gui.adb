with Ada.Real_Time;

with Global;

with Glib;

with Gtk.Box;
with Gtk.Enums.String_Lists;
with Gtk.Frame;
with Gtk.GEntry;
with Gtk.Grid;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Switch;
with Gtk.Window;

with Gtk.Gauge.Round_270_60s;
with Gtk.Oscilloscope;

package body GUI is

   type Leg_Switches is array (Landing_Legs.Legs_Index) of Gtk.Switch.Gtk_Switch;

   type Dynamic_Elements is
      record
         Leg_Switch      : Leg_Switches;
         Thruster_Switch : Gtk.Switch.Gtk_Switch;
         Height          : Gtk.GEntry.Gtk_Entry;
         Velocity        : Gtk.GEntry.Gtk_Entry;
      end record;

   type Main_Window_Record is new Gtk.Window.Gtk_Window_Record with
      record
         Elements          : Dynamic_Elements;
         Oscilloscope      : Gtk.Oscilloscope.Gtk_Oscilloscope;
         Height_Channel    : Gtk.Oscilloscope.Channel_Number;
         Touchdown_Channel : Gtk.Oscilloscope.Channel_Number;
         Thruster_Channel  : Gtk.Oscilloscope.Channel_Number;
         Tachometer        : Gtk.Gauge.Round_270_60s.Gtk_Gauge_Round_270_60s;
      end record;
   type Main_Window is access all Main_Window_Record'Class;

   package Windows_CB is new Gtk.Handlers.Callback (Widget_Type => Main_Window_Record);

   task GUI_Task;

   procedure Exit_Main (Win : access Main_Window_Record'Class);
   procedure Exit_Main (Win : access Main_Window_Record'Class) is
   begin
      Global.Log (Message => "Main window destroyed.");
      abort GUI_Task;
      Gtk.Main.Main_Quit;
      Win.all.Destroy;
   end Exit_Main;

   procedure Initialize (Window : in out Main_Window_Record'Class);
   procedure Initialize (Window : in out Main_Window_Record'Class) is
   begin
      Window.Initialize (The_Type => Gtk.Enums.Window_Toplevel);
      Window.Set_Title (Title => "Mars MPL simulation");

      declare
         VBox : Gtk.Box.Gtk_Box; -- The global box
      begin
         Gtk.Box.Gtk_New_Vbox (Box => VBox, Homogeneous => False, Spacing => 0);
         Window.Add (Widget => VBox);

         declare
            Frame : Gtk.Frame.Gtk_Frame;
         begin
            Gtk.Frame.Gtk_New (Frame => Frame, Label => "Sensor Signals");
            VBox.all.Pack_Start (Frame);

            declare
               Grid : Gtk.Grid.Gtk_Grid;
            begin
               Gtk.Grid.Gtk_New (Self => Grid);
               Grid.all.Set_Column_Homogeneous (Homogeneous => True);
               Frame.all.Add (Widget => Grid);

               declare
                  Label : Gtk.Label.Gtk_Label;
               begin
                  Gtk.Label.Gtk_New (Label => Label,
                                     Str   => "Landing leg signals.");
                  Grid.all.Attach (Child  => Label,
                                   Left   => 0,
                                   Top    => 0,
                                   Width  => 1,
                                   Height => 1);
               end;

               for Leg in Landing_Legs.Legs_Index loop
                  declare
                     use type Glib.Gint;
                     Switch : Gtk.Switch.Gtk_Switch;
                  begin
                     Gtk.Switch.Gtk_New (Self => Switch);
                     Grid.all.Attach
                       (Child  => Switch,
                        Left   => 1 + Landing_Legs.Legs_Index'Pos (Leg) / 2,
                        Top    => 0 + Landing_Legs.Legs_Index'Pos (Leg) mod 2,
                        Width  => 1,
                        Height => 1);
                     Window.Elements.Leg_Switch (Leg) := Switch;
                  end;
               end loop;

               declare
                  Label : Gtk.Label.Gtk_Label;
               begin
                  Gtk.Label.Gtk_New (Label => Label,
                                     Str   => "Thruster signal");
                  Grid.all.Attach (Child  => Label,
                                   Left   => 0,
                                   Top    => 2,
                                   Width  => 1,
                                   Height => 1);
               end;

               declare
                  Switch : Gtk.Switch.Gtk_Switch;
               begin
                  Gtk.Switch.Gtk_New (Self => Switch);
                  Grid.all.Attach (Child  => Switch,
                                   Left   => 1,
                                   Top    => 2,
                                   Width  => 1,
                                   Height => 1);
                  Window.Elements.Thruster_Switch := Switch;
               end;

               declare
                  Label : Gtk.Label.Gtk_Label;
               begin
                  Gtk.Label.Gtk_New (Label => Label,
                                     Str   => "Height above ground");
                  Grid.all.Attach (Child  => Label,
                                   Left   => 0,
                                   Top    => 3,
                                   Width  => 1,
                                   Height => 1);
               end;

               declare
                  Text : Gtk.GEntry.Gtk_Entry;
               begin
                  Gtk.GEntry.Gtk_New (The_Entry => Text);
                  Grid.all.Attach (Child  => Text,
                                   Left   => 1,
                                   Top    => 3,
                                   Width  => 1,
                                   Height => 1);
                  Window.Elements.Height := Text;
                  Text.all.Set_Editable (Is_Editable => False);
               end;

               declare
                  Label : Gtk.Label.Gtk_Label;
               begin
                  Gtk.Label.Gtk_New (Label => Label, Str => "Velocity");
                  Grid.all.Attach (Child  => Label,
                                   Left   => 0,
                                   Top    => 4,
                                   Width  => 1,
                                   Height => 1);
               end;

               declare
                  Text : Gtk.GEntry.Gtk_Entry;
               begin
                  Gtk.GEntry.Gtk_New (The_Entry => Text);
                  Grid.all.Attach (Child  => Text,
                                   Left   => 1,
                                   Top    => 4,
                                   Width  => 1,
                                   Height => 1);
                  Window.Elements.Velocity := Text;
                  Text.all.Set_Editable (Is_Editable => False);
               end;
            end;
         end;

         declare Frame : Gtk.Frame.Gtk_Frame;
         begin
            Gtk.Frame.Gtk_New (Frame => Frame, Label => "Velocity");
            Frame.all.Set_Size_Request (Width => 400, Height => 400);
            VBox.all.Pack_Start (Child => Frame);

            declare
               use type Gtk.Enums.String_Lists.Controlled_String_List;
               Gauge : Gtk.Gauge.Round_270_60s.Gtk_Gauge_Round_270_60s;
               List  : constant Gtk.Enums.String_Lists.Controlled_String_List
                 := "0" / "20" / "40" / "60" / "80" / "100" / "120" / "140";
            begin
               Gtk.Gauge.Round_270_60s.Gtk_New
                 (Widget  => Gauge,
                  Texts   => List,
                  Sectors => 7);
               Frame.all.Add (Widget => Gauge);
               Window.Tachometer := Gauge;
            end;
         end;

         declare
            Frame : Gtk.Frame.Gtk_Frame;
         begin
            Gtk.Frame.Gtk_New (Frame => Frame, Label => "Timeline");
            Frame.all.Set_Size_Request (Width => 800, Height => 200);
            VBox.all.Pack_Start (Child => Frame);

            declare
               use type Ada.Real_Time.Time;
               Plot : Gtk.Oscilloscope.Gtk_Oscilloscope;
            begin
               Gtk.Oscilloscope.Gtk_New (Widget => Plot);
               Frame.all.Add (Widget => Plot);

               Window.Oscilloscope := Plot;
               Window.Height_Channel    := Plot.all.Add_Channel;

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
      use type Ada.Real_Time.Time;
      Win          : Main_Window;
      Update_State : State;
   begin
      Gtk.Main.Init;
      Win := new Main_Window_Record;
      Initialize (Window => Win.all);
      Windows_CB.Connect (Widget => Win,
                          Name   => "destroy",
                          Marsh  => Windows_CB.To_Marshaller (Exit_Main'Access));

      Win.all.Show_All;

      loop
         State_Update.Wait_For_Update (New_State => Update_State);

         declare
            use type Altimeter.Height;
            use type Altimeter.Velocity;
            use type Glib.Gdouble;
            use type Landing_Legs.Leg_State;
            DE : Dynamic_Elements renames Win.all.Elements;
         begin
            for Leg in DE.Leg_Switch'Range loop
               DE.Leg_Switch (Leg).all.Set_Active
                 (Is_Active =>
                    Update_State.Legs (Leg) = Landing_Legs.Touched_Down);
            end loop;
            DE.Thruster_Switch.all.Set_Active
              (Is_Active => Update_State.Thruster);
            DE.Height.all.Set_Text
              (Text => Altimeter.Image (Update_State.Height));
            DE.Velocity.all.Set_Text
              (Text => Altimeter.Image (Update_State.Velocity));

            Win.all.Tachometer.all.Set_Value
              (Value => Glib.Gdouble (abs Update_State.Velocity / 140.0));
            Win.all.Tachometer.all.Queue_Draw;

            Win.all.Oscilloscope.all.Feed
              (Channel => Win.all.Height_Channel,
               V       => Glib.Gdouble (Update_State.Height));
            Win.all.Oscilloscope.all.Feed
              (Channel => Win.all.Touchdown_Channel,
               V       => Glib.Gdouble (Boolean'Pos ((for some L of Update_State.Legs => L = Landing_Legs.Touched_Down))));
            Win.all.Oscilloscope.all.Feed
              (Channel => Win.all.Thruster_Channel,
               V       => Glib.Gdouble (Boolean'Pos (Update_State.Thruster)));
         end;

         while Gtk.Main.Events_Pending loop
            if Gtk.Main.Main_Iteration_Do (Blocking => False) then
               null;
            end if;
         end loop;
      end loop;
   end GUI_Task;

   procedure Update (New_State : State) is
   begin
      State_Update.Set_State (New_State);
   end Update;
end GUI;
