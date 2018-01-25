with Gtk.Frame;
with Gtk.Layered.Label;

separate (GUI)
function Create_Altitude_Frame
  (Window : in out Main_Window_Record'Class) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Altitude");
begin
   Frame.all.Set_Size_Request (Width  => 400,
                               Height => 400);

   declare
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);
   begin
      Frame.all.Add (Widget => Box);

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
            Color    => Colors.White,
            Angle    => 0.0,
            Skew     => 0.0,
            Markup   => False,
            Scaled   => True);
         Window.Altimeter := Gauge;

         Box.all.Pack_Start (Child  => Gauge,
                             Expand => True);
      end;

      declare
         Text : Gtk.GEntry.Gtk_Entry;
      begin
         Gtk.GEntry.Gtk_New (The_Entry => Text);
         Box.all.Pack_End (Child => Text,
                           Expand => False);
         Window.Elements.Altitude := Text;
         Text.all.Set_Editable (Is_Editable => False);
      end;
   end;

   return Frame;
end Create_Altitude_Frame;
