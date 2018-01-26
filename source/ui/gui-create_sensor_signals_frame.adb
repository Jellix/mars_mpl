with Gtk.Frame;
with Gtk.Label;
with Gtk.Switch;

separate (GUI)
function Create_Sensor_Signals_Frame
  (Window : in out Main_Window_Record'Class) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   function Create_LEDs return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_LEDs return not null access
     Gtk.Widget.Gtk_Widget_Record'Class
   is
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);

      function Labeled_Widget
        (Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Description : String) return not null access
        Gtk.Widget.Gtk_Widget_Record'Class;

      function Labeled_Widget
        (Widget      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
         Description : String) return not null access
        Gtk.Widget.Gtk_Widget_Record'Class
      is
         Widget_Box : constant Gtk.Box.Gtk_Box :=
                        Gtk.Box.Gtk_Hbox_New (Homogeneous => True,
                                              Spacing     => 0);
         Label      : constant Gtk.Label.Gtk_Label :=
                        Gtk.Label.Gtk_Label_New (Str => Description);
      begin
         Widget_Box.all.Pack_Start (Label);
         Widget_Box.all.Pack_Start (Widget);

         return Widget_Box;
      end Labeled_Widget;
   begin
      Box.all.Set_Size_Request (Width  => 100,
                                Height => 100);

      for Leg in Shared_Types.Legs_Index loop
         declare
            Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
         begin
            Gtk.Gauge.LED_Round.Gtk_New
              (Widget        => Led,
               On_Color      => Colors.Green,
               Off_Color     => Colors.Grey,
               Border_Shadow => Gtk.Enums.Shadow_Etched_Out);
            Box.all.Pack_Start
              (Child =>
                 Labeled_Widget
                   (Widget      => Led,
                    Description =>
                      "Landing Leg"
                    & Natural'Image
                      (Shared_Types.Legs_Index'Pos (Leg) + 1)));
            Window.Elements.Leg_Led (Leg) := Led;
         end;
      end loop;

      declare
         Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
      begin
         Gtk.Gauge.LED_Round.Gtk_New
           (Widget        => Led,
            On_Color      => Colors.Light_Yellow,
            Off_Color     => Colors.Grey,
            Border_Shadow => Gtk.Enums.Shadow_Etched_Out);
         Box.all.Pack_Start
           (Child => Labeled_Widget (Widget     => Led,
                                     Description => "Thruster"));
         Window.Elements.Thruster_Led := Led;
      end;

      declare
         Bug_Switch : Gtk.Switch.Gtk_Switch;
      begin
         Gtk.Switch.Gtk_New (Self => Bug_Switch);
         Box.all.Pack_Start
           (Child => Labeled_Widget (Widget      => Bug_Switch,
                                     Description => "Bug"));
         Bug_Switch.all.Set_State (State => Shared_Sensor_Data.Bug_Enabled);
         Bug_Switch.all.On_State_Set (Call  => GUI_Callbacks.Switch_Bug'Access,
                                      After => False);
      end;

      return Box;
   end Create_LEDs;

   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Sensor Signals");
begin
   declare
      HBox : constant Gtk.Box.Gtk_Box :=
               Gtk.Box.Gtk_Hbox_New (Homogeneous => False,
                                     Spacing     => 0);
   begin
      Frame.all.Add (Widget => HBox);

      declare
         VBox2 : Gtk.Box.Gtk_Vbox;
      begin
         Gtk.Box.Gtk_New_Vbox (Box => VBox2);
         HBox.all.Pack_Start (Child => VBox2);
         VBox2.all.Pack_Start (Child => Create_LEDs);
      end;

      HBox.all.Pack_Start (Child => Window.Create_Fuel_Frame);
      HBox.all.Pack_Start (Child => Window.Create_Velocity_Frame);
      HBox.all.Pack_Start (Child => Window.Create_Altitude_Frame);
   end;

   return Frame;
end Create_Sensor_Signals_Frame;
