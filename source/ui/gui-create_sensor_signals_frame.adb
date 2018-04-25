with Cairo.Ellipses;
with Gdk.Color;
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

   procedure Add_Labeled_LED
     (Widget    :    out Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
      Label     : in     Glib.UTF8_String;
      On_Color  : in     Gdk.Color.Gdk_Color;
      Off_Color : in     Gdk.Color.Gdk_Color;
      Column    : in     Glib.Gint;
      Row       : in     Glib.Gint);

   procedure Add_Labeled_LED
     (Widget    :    out Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
      Label     : in     Glib.UTF8_String;
      On_Color  : in     Gdk.Color.Gdk_Color;
      Off_Color : in     Gdk.Color.Gdk_Color;
      Column    : in     Glib.Gint;
      Row       : in     Glib.Gint)
   is
      L : constant not null Gtk.Label.Gtk_Label :=
            Gtk.Label.Gtk_Label_New (Str => Label);
   begin
      L.all.Set_Halign (Align => Gtk.Widget.Align_Start);
      Grid.all.Attach (Child  => L,
                       Left   => Column,
                       Top    => Row,
                       Width  => 1,
                       Height => 1);

      Gtk.Gauge.LED_Round.Gtk_New
        (Widget        => Widget,
         On_Color      => On_Color,
         Off_Color     => Off_Color,
         Border_Shadow => Gtk.Enums.Shadow_Etched_Out);

      Grid.all.Attach (Child  => Widget,
                       Left   => Column + 1,
                       Top    => Row,
                       Width  => 1,
                       Height => 1);
   end Add_Labeled_LED;

   Current_Column : Glib.Gint := 0;
begin
   Grid.all.Set_Column_Homogeneous (Homogeneous => True);
   Grid.all.Set_Row_Homogeneous (Homogeneous => True);

   --  Discrete Signals signified by LEDs.
   Add_Heading (Title  => "Discrete Signals",
                Column => Current_Column,
                Width  => 2);

   Add_Leg_LEDs :
   for Leg in Shared_Types.Legs_Index loop
      Add_Row :
      declare
         Row_Offset : constant Glib.Gint :=
                        Glib.Gint (Shared_Types.Legs_Index'Pos (Leg));
      begin
         Add_Labeled_LED
           (Widget    => Window.Elements.Leg_Led (Leg),
            Label     => "Landing Leg" & Glib.Gint'Image (Row_Offset + 1),
            On_Color  => Gtk.Colors.Green,
            Off_Color => Gtk.Colors.Grey,
            Column    => Current_Column,
            Row       => Gauge_Row + Row_Offset);
      end Add_Row;
   end loop Add_Leg_LEDs;

   Add_Thruster_LED :
   declare
      Row_Offset : constant Glib.Gint :=
                     Glib.Gint
                       (Shared_Types.Legs_Index'Pos
                          (Shared_Types.Legs_Index'Last)) + 1;
   begin
      Add_Labeled_LED (Widget    => Window.Elements.Thruster_Led,
                       Label     => "Thruster",
                       On_Color  => Gtk.Colors.Dark_Yellow,
                       Off_Color => Gtk.Colors.Grey,
                       Column    => Current_Column,
                       Row       => Gauge_Row + Row_Offset);
   end Add_Thruster_LED;

   Current_Column := Current_Column + 2;

   Add_Fuel_Gauge :
   declare
      Gauge_Width : constant Glib.Gint := Glib.Gint'Max (2, Gauge_Size / 3);
   begin
      Add_Heading (Title  => "Fuel Mass",
                   Column => Current_Column,
                   Width  => Gauge_Width);

      Gtk.Gauge.Flat_Vertical.Gtk_New
        (Widget  => Window.Fuel_Scale,
         Texts   => Fuel_Scale.Texts.all,
         Sectors =>
           Positive (Gtk.Enums.String_List.Length (+Fuel_Scale.Texts.all)) - 1);
      Grid.all.Attach (Child  => Window.Fuel_Scale,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Width,
                       Height => Gauge_Size);

      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Fuel);
      Window.Elements.Fuel.all.Set_Editable (Is_Editable => False);

      Grid.all.Attach (Child  => Window.Elements.Fuel,
                       Left   => Current_Column,
                       Top    => Text_Row,
                       Width  => Gauge_Width,
                       Height => 1);

      Current_Column := Current_Column + Gauge_Width;
   end Add_Fuel_Gauge;

   Add_Delta_V_Gauge :
   declare
      Gauge_Width : constant Glib.Gint := Glib.Gint'Max (2, Gauge_Size / 3);
   begin
      Add_Heading (Title  => "Delta V",
                   Column => Current_Column,
                   Width  => Gauge_Width);

      Gtk.Gauge.Flat_Vertical.Gtk_New
        (Widget  => Window.Delta_V_Scale,
         Texts   => Delta_V_Scale.Texts.all,
         Sectors =>
           Positive
             (Gtk.Enums.String_List.Length (+Delta_V_Scale.Texts.all)) - 1);
      Grid.all.Attach (Child  => Window.Delta_V_Scale,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Width,
                       Height => Gauge_Size);
      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Delta_V);
      Window.Elements.Delta_V.all.Set_Editable (Is_Editable => False);

      Grid.all.Attach (Child  => Window.Elements.Delta_V,
                       Left   => Current_Column,
                       Top    => Text_Row,
                       Width  => Gauge_Width,
                       Height => 1);
      Current_Column := Current_Column + Gauge_Width;
   end Add_Delta_V_Gauge;

   Add_Drag_Gauge :
   declare
      Gauge_Width : constant Glib.Gint := Glib.Gint'Max (2, Gauge_Size / 3);
   begin
      Add_Heading (Title  => "Drag",
                   Column => Current_Column,
                   Width  => Gauge_Width);

      Gtk.Gauge.Flat_Vertical.Gtk_New
        (Widget  => Window.Drag_Scale,
         Texts   => Drag_Scale.Texts.all,
         Sectors =>
           Positive
             (Gtk.Enums.String_List.Length (+Delta_V_Scale.Texts.all)) - 1);
      Grid.all.Attach (Child  => Window.Drag_Scale,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Width,
                       Height => Gauge_Size);

      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Drag);
      Window.Elements.Drag.all.Set_Editable (Is_Editable => False);
      Grid.all.Attach (Child  => Window.Elements.Drag,
                       Left   => Current_Column,
                       Top    => Text_Row,
                       Width  => Gauge_Width,
                       Height => 1);
      Current_Column := Current_Column + Gauge_Width;
   end Add_Drag_Gauge;

   --  Velocity Gauge
   Add_Heading (Title  => "Velocity",
                Column => Current_Column,
                Width  => Gauge_Size);

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

   --  Altitude gauge.
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

   Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Altitude);
   Window.Elements.Altitude.all.Set_Editable (Is_Editable => False);
   Grid.all.Attach (Child  => Window.Elements.Altitude,
                    Left   => Current_Column + 1,
                    Top    => Text_Row,
                    Width  => Gauge_Size - 2,
                    Height => 1);

   return Grid;
end Create_Sensor_Signals_Frame;
