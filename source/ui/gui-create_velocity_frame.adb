with Cairo.Ellipses;
with Gtk.Frame;
with Gtk.Layered.Label;

separate (GUI)
function Create_Velocity_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Velocity");
begin
   Frame.all.Set_Size_Request (Width  => 400,
                               Height => 400);

   Add_Widgets_To_Frame :
   declare
      Box : constant Gtk.Box.Gtk_Box :=
              Gtk.Box.Gtk_Vbox_New (Homogeneous => False,
                                    Spacing     => 0);
   begin
      Frame.all.Add (Widget => Box);

      Add_Velocity_Gauge :
      declare
         Gauge : Gtk.Gauge.Round_270.Gtk_Gauge_Round_270;
      begin
         Gtk.Gauge.Round_270.Gtk_New
           (Widget  => Gauge,
            Texts   => Velocity_Scale.Texts.all,
            Sectors =>
              Positive
                (Gtk.Enums.String_List.Length
                     (+Velocity_Scale.Texts.all)) - 1);
         Gtk.Layered.Label.Add_Label
           (Under    => Gauge.all.Get_Cache,
            Text     => "m/s",
            Location => Cairo.Ellipses.Cairo_Tuple'(X => 0.01,
                                                    Y => 0.15),
            Face     => Label_Font,
            Height   => 0.03,
            Stretch  => 0.9,
            Mode     => Gtk.Layered.Moved_Centered,
            Color    => Colors.White,
            Angle    => 0.0,
            Skew     => 0.0,
            Markup   => False,
            Scaled   => True);
         Box.all.Pack_Start (Child  => Gauge,
                             Expand => True);
         Window.Tachometer := Gauge;
      end Add_Velocity_Gauge;

      Add_Text_Entry :
      declare
         Text : Gtk.GEntry.Gtk_Entry;
      begin
         Gtk.GEntry.Gtk_New (The_Entry => Text);
         Box.all.Pack_End (Child  => Text,
                           Expand => False);
         Window.Elements.Velocity := Text;
         Text.all.Set_Editable (Is_Editable => False);
      end Add_Text_Entry;
   end Add_Widgets_To_Frame;

   return Frame;
end Create_Velocity_Frame;
