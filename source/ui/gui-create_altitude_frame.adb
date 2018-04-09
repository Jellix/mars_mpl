with Cairo.Ellipses;
with Gtk.Colors;
with Gtk.Frame;
with Gtk.Layered.Label;

separate (GUI)
function Create_Altitude_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant not null Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Altitude");
begin
   Add_Widgets_To_Box :
   declare
      Box : constant not null Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);
   begin
      Frame.all.Add (Widget => Box);

      Gtk.Gauge.Altimeter.Gtk_New
        (Widget  => Window.Altimeter,
         Texts   => Altitude_Scale.Texts.all,
         Sectors =>
           Positive (Gtk.Enums.String_List.Length (+Altitude_Scale.Texts.all)));
      Gtk.Layered.Label.Add_Label
        (Under    => Window.Altimeter.all.Get_Cache,
         Text     => "x 1000 m",
         Location => Cairo.Ellipses.Cairo_Tuple'(X => 0.0175,
                                                 Y => 0.175),
         Face     => Label_Font,
         Height   => 0.04,
         Stretch  => 1.0,
         Mode     => Gtk.Layered.Moved_Centered,
         Color    => Gtk.Colors.White,
         Angle    => 0.0,
         Skew     => 0.0,
         Markup   => False,
         Scaled   => True);
      Box.all.Pack_Start (Child  => Window.Altimeter,
                          Expand => True);

      Add_Text_Entry :
      declare
         Text : Gtk.GEntry.Gtk_Entry;
      begin
         Gtk.GEntry.Gtk_New (The_Entry => Text);
         Box.all.Pack_End (Child => Text,
                           Expand => False);
         Window.Elements.Altitude := Text;
         Text.all.Set_Editable (Is_Editable => False);
      end Add_Text_Entry;
   end Add_Widgets_To_Box;

   return Frame;
end Create_Altitude_Frame;
