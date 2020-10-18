with Cairo.Ellipses;
with Gtk.Colors;
with Gtk.Grid;
with Gtk.Label;
with Gtk.Layered.Label;

separate (GUI)
function Create_Gauges_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   use type Glib.Gint;

   Gauge_Size  : constant Glib.Gint := 6;
   Header_Row  : constant Glib.Gint := 0;
   Gauge_Row   : constant Glib.Gint := 1;
   Text_Row    : constant Glib.Gint := Gauge_Row + Gauge_Size;

   Grid : constant not null Gtk.Grid.Gtk_Grid := Gtk.Grid.Gtk_Grid_New;

   procedure Add_Heading (Title  : in Glib.UTF8_String;
                          Column : in Glib.Gint;
                          Width  : in Glib.Gint);

   procedure Add_Heading (Title  : in Glib.UTF8_String;
                          Column : in Glib.Gint;
                          Width  : in Glib.Gint)
   is
      L : constant not null Gtk.Label.Gtk_Label :=
            Gtk.Label.Gtk_Label_New (Str => "<b>" & Title & "</b>");
   begin
      L.all.Set_Use_Markup (Setting => True);
      L.all.Set_Halign (Align => Gtk.Widget.Align_Center);
      Grid.all.Attach (Child  => L,
                       Left   => Column,
                       Top    => Header_Row,
                       Width  => Width,
                       Height => 1);
   end Add_Heading;

   Current_Column : Glib.Gint := 0;
begin
   Grid.all.Set_Column_Homogeneous (Homogeneous => True);
   Grid.all.Set_Row_Homogeneous (Homogeneous => True);

   Add_Attitude_Gauge :
   begin
      Add_Heading (Title  => "Attitude",
                   Column => Current_Column + 1,
                   Width  => Gauge_Size - 2);

      Gtk.Valve.Round_90.Gtk_New
        (Widget     => Window.Horizon,
         Texts      => Horizon_Scale.Texts.all,
         Sectors    =>
           Positive
             (Gtk.Enums.String_List.Length (+Horizon_Scale.Texts.all)) - 1);

      Grid.all.Attach (Child  => Window.Horizon,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Size,
                       Height => Gauge_Size);

      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Horizon);
      Window.Elements.Horizon.all.Set_Editable (Is_Editable => False);

      Grid.all.Attach (Child  => Window.Elements.Horizon,
                       Left   => Current_Column + 1,
                       Top    => Text_Row,
                       Width  => Gauge_Size - 2,
                       Height => 1);

      Current_Column := Current_Column + Gauge_Size;
   end Add_Attitude_Gauge;

   Add_Velocity_Gauge :
   begin
      Add_Heading (Title  => "Velocity",
                   Column => Current_Column + 1,
                   Width  => Gauge_Size - 2);

      Gtk.Gauge.Round_270.Gtk_New
        (Widget  => Window.Tachometer,
         Texts   => Velocity_Scale.Texts.all,
         Sectors =>
           Positive
             (Gtk.Enums.String_List.Length (+Velocity_Scale.Texts.all)) - 1);
      Gtk.Layered.Label.Add_Label
        (Under    => Window.Tachometer.all.Get_Cache,
         Text     => "m/s",
         Location => Cairo.Ellipses.Cairo_Tuple'(X => 0.01,
                                                 Y => 0.15),
         Face     => Label_Font,
         Height   => 0.03,
         Stretch  => 0.9,
         Mode     => Gtk.Layered.Moved_Centered,
         Color    => Gtk.Colors.White,
         Angle    => 0.0,
         Skew     => 0.0,
         Markup   => False,
         Scaled   => True);
      Grid.all.Attach (Child  => Window.Tachometer,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Size,
                       Height => Gauge_Size);

      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Velocity);
      Window.Elements.Velocity.all.Set_Editable (Is_Editable => False);

      Grid.all.Attach (Child  => Window.Elements.Velocity,
                       Left   => Current_Column + 1,
                       Top    => Text_Row,
                       Width  => Gauge_Size - 2,
                       Height => 1);

      Current_Column := Current_Column + Gauge_Size;
   end Add_Velocity_Gauge;

   Add_Altitude_Gauge :
   begin
      Add_Heading (Title  => "Altitude",
                   Column => Current_Column + 1,
                   Width  => Gauge_Size - 2);

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

      Grid.all.Attach (Child  => Window.Altimeter,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Size,
                       Height => Gauge_Size);

      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Altitude);
      Window.Elements.Altitude.all.Set_Editable (Is_Editable => False);

      Grid.all.Attach (Child  => Window.Elements.Altitude,
                       Left   => Current_Column + 1,
                       Top    => Text_Row,
                       Width  => Gauge_Size - 2,
                       Height => 1);
   end Add_Altitude_Gauge;

   return Grid;
end Create_Gauges_Frame;
