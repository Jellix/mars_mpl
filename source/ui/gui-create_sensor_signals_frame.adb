with Gtk.Frame;
with Gtk.Label;
with Gtk.Switch;

separate (GUI)
function Create_Sensor_Signals_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   function Create_LEDs return not null access
     Gtk.Widget.Gtk_Widget_Record'Class;

   function Create_LEDs return not null access
     Gtk.Widget.Gtk_Widget_Record'Class
   is
      Frame : constant Gtk.Frame.Gtk_Frame :=
                Gtk.Frame.Gtk_Frame_New (Label => "Discrete Signals");
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);
   begin
      Frame.all.Add (Widget => Box);

      for Leg in Shared_Types.Legs_Index loop
         Add_Leg_LED :
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
         end Add_Leg_LED;
      end loop;

      Add_Thruster_LED :
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
      end Add_Thruster_LED;

      return Frame;
   end Create_LEDs;

   HBox : constant Gtk.Box.Gtk_Box :=
            Gtk.Box.Gtk_Hbox_New (Homogeneous => False,
                                  Spacing     => 0);
begin
   HBox.all.Pack_Start (Child  => Create_LEDs,
                        Expand => False);
   HBox.all.Pack_Start (Child => Window.Create_Fuel_Frame);
   HBox.all.Pack_Start (Child => Window.Create_Velocity_Frame);
   HBox.all.Pack_Start (Child => Window.Create_Altitude_Frame);

   return HBox;
end Create_Sensor_Signals_Frame;
