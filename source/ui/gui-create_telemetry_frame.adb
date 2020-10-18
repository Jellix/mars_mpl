with Gdk.Color;
with Gtk.Colors;
with Gtk.Grid;
with Gtk.Label;

separate (GUI)
function Create_Telemetry_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   use type Glib.Gint;

   Gauge_Size  : constant Glib.Gint := 6;
   Header_Row  : constant Glib.Gint := 0;
   Gauge_Row   : constant Glib.Gint := 1;
   Text_Row    : constant Glib.Gint := Header_Row + Gauge_Size + 1;
   Gauge_Width : constant Glib.Gint := Glib.Gint'Max (2, Gauge_Size / 3);

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
   Grid.all.Set_Column_Homogeneous (Homogeneous => False);
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

   Add_Core_Temperature_Gauge :
   begin
      Add_Heading (Title  => "Core Temperature",
                   Column => Current_Column,
                   Width  => Gauge_Width);

      Gtk.Meter.Thermo.Gtk_New
        (Widget  => Window.Core_Temp,
         Texts   => Temperature_Scale.Texts.all,
         Sectors =>
           Positive
             (Gtk.Enums.String_List.Length (+Temperature_Scale.Texts.all)) - 1,
         Label   => "K");
      Grid.all.Attach (Child  => Window.Core_Temp,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Width,
                       Height => Gauge_Size);

      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Core_Temp);
      Window.Elements.Core_Temp.all.Set_Editable (Is_Editable => False);

      Grid.all.Attach (Child  => Window.Elements.Core_Temp,
                       Left   => Current_Column,
                       Top    => Text_Row,
                       Width  => Gauge_Width,
                       Height => 1);
      Current_Column := Current_Column + Gauge_Width;
   end Add_Core_Temperature_Gauge;

   Add_Surface_Temperature_Gauge :
   begin
      Add_Heading (Title  => "Surface Temperature",
                   Column => Current_Column,
                   Width  => Gauge_Width);

      Gtk.Meter.Thermo.Gtk_New
        (Widget  => Window.Surface_Temp,
         Texts   => Temperature_Scale.Texts.all,
         Sectors =>
           Positive
             (Gtk.Enums.String_List.Length (+Temperature_Scale.Texts.all)) - 1,
         Label   => "K");
      Grid.all.Attach (Child  => Window.Surface_Temp,
                       Left   => Current_Column,
                       Top    => Gauge_Row,
                       Width  => Gauge_Width,
                       Height => Gauge_Size);

      Gtk.GEntry.Gtk_New (The_Entry => Window.Elements.Surface_Temp);
      Window.Elements.Surface_Temp.all.Set_Editable (Is_Editable => False);

      Grid.all.Attach (Child  => Window.Elements.Surface_Temp,
                       Left   => Current_Column,
                       Top    => Text_Row,
                       Width  => Gauge_Width,
                       Height => 1);
      Current_Column := Current_Column + Gauge_Width;
   end Add_Surface_Temperature_Gauge;

   return Grid;
end Create_Telemetry_Frame;
