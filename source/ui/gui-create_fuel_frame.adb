with Cairo.Ellipses;
with Gtk.Colors;
with Gtk.Frame;
with Gtk.Layered.Label;

separate (GUI)
function Create_Fuel_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Fuel");
begin
   Add_Widgets_To_Box :
   declare
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);
   begin
      Box.all.Set_Size_Request (Height => 200);
      Frame.all.Add (Widget => Box);

      Add_Fuel_Gauge :
      declare
         Gauge : Gtk.Meter.Angular_90.Gtk_Meter_Angular_90;
      begin
         Gtk.Meter.Angular_90.Gtk_New (Widget  => Gauge,
                                       Texts   => Fuel_Scale.Texts.all,
                                       Sectors =>
                                         Positive
                                           (Gtk.Enums.String_List.Length
                                              (+Fuel_Scale.Texts.all)) - 1);
         Gtk.Layered.Label.Add_Label (Under    => Gauge.all.Get_Cache,
                                      Text     => "kg",
                                      Location =>
                                        Cairo.Ellipses.Cairo_Tuple'(X => 0.0175,
                                                                    Y => 0.1),
                                      Face     => Label_Font_Italic,
                                      Height   => 0.03,
                                      Stretch  => 0.9,
                                      Mode     => Gtk.Layered.Moved_Centered,
                                      Color    => Gtk.Colors.Black,
                                      Angle    => 0.0,
                                      Skew     => 0.0,
                                      Markup   => False,
                                      Scaled   => True);
         Box.all.Pack_Start (Child  => Gauge,
                             Expand => True);
         Window.Fuel_Scale := Gauge;
      end Add_Fuel_Gauge;

      Add_Text_Entry :
      declare
         Text : Gtk.GEntry.Gtk_Entry;
      begin
         Gtk.GEntry.Gtk_New (The_Entry => Text);
         Box.all.Pack_End (Child  => Text,
                           Expand => False);
         Window.Elements.Fuel := Text;
         Text.all.Set_Editable (Is_Editable => False);
      end Add_Text_Entry;
   end Add_Widgets_To_Box;

   return Frame;
end Create_Fuel_Frame;
