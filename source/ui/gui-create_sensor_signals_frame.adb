with Cairo.Ellipses;
with Gtk.Colors;
with Gtk.Grid;
with Gtk.Label;
with Gtk.Layered.Label;

separate (GUI)
function Create_Sensor_Signals_Frame
  (Window     : in out Main_Window_Record;
   Gauge_Size : in     Glib.Gint) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   use type Glib.Gint;

   Header_Row : constant Glib.Gint := 0;
   Gauge_Row  : constant Glib.Gint := 1;
   Text_Row   : constant Glib.Gint := Header_Row + Gauge_Size + 1;

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

   --  Discrete Signals signified by LEDs.
   Add_Heading (Title  => "Discrete Signals",
                Column => Current_Column,
                Width  => 2);

   for Leg in Shared_Types.Legs_Index loop
      Add_Leg_LED :
      declare
         Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
      begin
         Gtk.Gauge.LED_Round.Gtk_New
           (Widget        => Led,
            On_Color      => Gtk.Colors.Green,
            Off_Color     => Gtk.Colors.Grey,
            Border_Shadow => Gtk.Enums.Shadow_Etched_Out);

         Add_Leg_Label :
         declare
            L : constant not null Gtk.Label.Gtk_Label :=
                  Gtk.Label.Gtk_Label_New
                    (Str => "Landing Leg"
                     & Natural'Image (Shared_Types.Legs_Index'Pos (Leg) + 1));
         begin
            L.all.Set_Halign (Align => Gtk.Widget.Align_Start);
            Grid.all.Attach
              (Child  => L,
               Left   => Current_Column,
               Top    =>
                 Gauge_Row + Glib.Gint (Shared_Types.Legs_Index'Pos (Leg)),
               Width  => 1,
               Height => 1);
         end Add_Leg_Label;

         Grid.all.Attach
           (Child  => Led,
            Left   => Current_Column + 1,
            Top    => Gauge_Row + Glib.Gint (Shared_Types.Legs_Index'Pos (Leg)),
            Width  => 1,
            Height => 1);
         Window.Elements.Leg_Led (Leg) := Led;
      end Add_Leg_LED;
   end loop;

   Add_Thruster_LED :
   declare
      Led : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
   begin
      Gtk.Gauge.LED_Round.Gtk_New
        (Widget        => Led,
         On_Color      => Gtk.Colors.Dark_Yellow,
         Off_Color     => Gtk.Colors.Grey,
         Border_Shadow => Gtk.Enums.Shadow_Etched_Out);

      Add_Thruster_Label :
      declare
         L : constant not null Gtk.Label.Gtk_Label :=
               Gtk.Label.Gtk_Label_New (Str => "Thruster");
      begin
         L.all.Set_Halign (Align => Gtk.Widget.Align_Start);
         Grid.all.Attach (Child  => L,
                          Left   => Current_Column,
                          Top    => Gauge_Row + 3,
                          Width  => 1,
                          Height => 1);
      end Add_Thruster_Label;

      Grid.all.Attach (Child  => Led,
                       Left   => Current_Column + 1,
                       Top    => Gauge_Row + 3,
                       Width  => 1,
                       Height => 1);
      Window.Elements.Thruster_Led := Led;
   end Add_Thruster_LED;

   --  Fuel Mass
   Current_Column := Current_Column + 2;

   Add_Heading (Title  => "Fuel Mass",
                Column => Current_Column,
                Width  => Gauge_Size);

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
      Grid.all.Attach (Child  => Gauge,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Size,
                       Height => Gauge_Size);
      Window.Fuel_Scale := Gauge;
   end Add_Fuel_Gauge;

   Add_Fuel_Text :
   declare
      Text : Gtk.GEntry.Gtk_Entry;
   begin
      Gtk.GEntry.Gtk_New (The_Entry => Text);
      Grid.all.Attach (Child  => Text,
                       Left   => Current_Column + 1,
                       Top    => Text_Row,
                       Width  => Gauge_Size - 2,
                       Height => 1);
      Window.Elements.Fuel := Text;
      Text.all.Set_Editable (Is_Editable => False);
   end Add_Fuel_Text;

   --  Delta V gauge.
   Current_Column := Current_Column + Gauge_Size;

   Add_Heading (Title  => "Delta V",
                Column => Current_Column,
                Width  => 2);

   Add_Delta_V_Gauge :
   declare
      Gauge : Gtk.Gauge.Flat_Vertical.Gtk_Gauge_Flat_Vertical;
   begin
      Gtk.Gauge.Flat_Vertical.Gtk_New
        (Widget  => Gauge,
         Texts   => Delta_V_Scale.Texts.all,
         Sectors => Positive
           (Gtk.Enums.String_List.Length
                (+Delta_V_Scale.Texts.all)) - 1);
      Grid.all.Attach (Child  => Gauge,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Glib.Gint'Max (1, Gauge_Size / 3),
                       Height => Text_Row);
      Window.Delta_V_Scale := Gauge;
   end Add_Delta_V_Gauge;

   --  Velocity Gauge
   Current_Column := Current_Column + Glib.Gint'Max (1, Gauge_Size / 3);

   Add_Heading (Title  => "Velocity",
                Column => Current_Column,
                Width  => Gauge_Size);

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
         Color    => Gtk.Colors.White,
         Angle    => 0.0,
         Skew     => 0.0,
         Markup   => False,
         Scaled   => True);
      Grid.all.Attach (Child  => Gauge,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Size,
                       Height => Gauge_Size);
      Window.Tachometer := Gauge;
   end Add_Velocity_Gauge;

   Add_Velocity_Text :
   declare
      Text : Gtk.GEntry.Gtk_Entry;
   begin
      Gtk.GEntry.Gtk_New (The_Entry => Text);
      Grid.all.Attach (Child  => Text,
                       Left   => Current_Column + 1,
                       Top    => Text_Row,
                       Width  => Gauge_Size - 2,
                       Height => 1);
      Window.Elements.Velocity := Text;
      Text.all.Set_Editable (Is_Editable => False);
   end Add_Velocity_Text;

   --  Altitude gauge.
   Current_Column := Current_Column + Gauge_Size;

   Add_Heading (Title  => "Altitude",
                Column => Current_Column,
                Width  => Gauge_Size);

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

   Add_Altitude_Text :
   declare
      Text : Gtk.GEntry.Gtk_Entry;
   begin
      Gtk.GEntry.Gtk_New (The_Entry => Text);
      Grid.all.Attach (Child  => Text,
                       Left   => Current_Column + 1,
                       Top    => Text_Row,
                       Width  => Gauge_Size - 2,
                       Height => 1);
      Window.Elements.Altitude := Text;
      Text.all.Set_Editable (Is_Editable => False);
   end Add_Altitude_Text;

   return Grid;
end Create_Sensor_Signals_Frame;
