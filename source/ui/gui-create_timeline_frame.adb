with Gtk.Frame;
with Gtk.Gauge.LED_Rectangular;
with Gtk.Grid;
with Gtk.Missed;

separate (GUI)
function Create_Timeline_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Timeline");
   Box   : constant Gtk.Box.Gtk_Hbox := Gtk.Box.Gtk_Hbox_New;
begin
   Frame.all.Add (Widget => Box);
   Frame.all.Set_Size_Request (Width  => 600,
                               Height => 200);

   Add_Scope :
   declare
      Scope : Gtk.Oscilloscope.Gtk_Oscilloscope;
   begin
      Gtk.Oscilloscope.Gtk_New (Widget     => Scope,
                                Background => Colors.Light_Grey);
      --  A typical simulation runs about 70s. With 100 datapoints/s this
      --  amounts to roughly 7000 distinct data points, thus the default buffer
      --  size of ~60_000 should easily be enough.
      Box.all.Pack_Start (Child  => Scope,
                          Expand => True);

      Scope.all.Set_Manual_Sweep (Enable => False);

      --  Lower axis.
      Scope.all.Set_Frozen (Sweeper => Gtk.Oscilloscope.Lower,
                            Frozen  => True);
      Scope.all.Set_Time_Scale (Sweeper => Gtk.Oscilloscope.Lower,
                                Visible => True);
      Scope.all.Set_Time_Axis (Sweeper => Gtk.Oscilloscope.Lower,
                               Visible => True,
                               As_Time => False);
      Window.Plot.Oscilloscope := Scope;
   end Add_Scope;

   Add_Plots :
   declare
      Plots : Plot_Elements renames Window.Plot;
      Scope : Gtk.Oscilloscope.Gtk_Oscilloscope_Record renames
                Gtk.Oscilloscope.Gtk_Oscilloscope_Record
                  (Window.Plot.Oscilloscope.all);
   begin
      Plots.Altitude_Channel :=
        Scope.Add_Channel (Color   => Colors.Red,
                           Mode    => Gtk.Layered.Linear,
                           Name    => "Altitude",
                           Sweeper => Gtk.Oscilloscope.Lower);
      Plots.Fuel_Channel     :=
        Scope.Add_Channel (Color => Colors.Blue,
                           Mode  => Gtk.Layered.Linear,
                           Name  => "Fuel",
                           Sweeper => Gtk.Oscilloscope.Lower);
      Plots.Velocity_Channel :=
        Scope.Add_Channel (Color => Colors.Purple,
                           Mode  => Gtk.Layered.Linear,
                           Name  => "Velocity",
                           Sweeper => Gtk.Oscilloscope.Lower);

      Add_Discretes :
      declare
         G : Gtk.Oscilloscope.Group_Number;
      begin
         G := Scope.Add_Group (Name => "Signals");

         for Leg in Shared_Types.Legs_Index loop
            Add_Leg_Discrete :
            declare
               Color_Offset : constant Glib.Gdouble :=
                                0.2 * Glib.Gdouble (Shared_Types.Legs_Index'Pos (Leg));
            begin
               Plots.Touchdown_Channel (Leg) :=
                 Scope.Add_Channel
                   (Group   => G,
                    Color   => Gtk.Missed.RGB (Red   => Color_Offset,
                                               Green => 1.0,
                                               Blue  => Color_Offset),
                    Mode    => Gtk.Layered.Left,
                    Name    =>
                      "Touchdown" & Shared_Types.Legs_Index'Image (Leg),
                    Sweeper => Gtk.Oscilloscope.Lower);
            end Add_Leg_Discrete;
         end loop;

         Plots.Thruster_Channel :=
           Scope.Add_Channel (Group   => G,
                              Color   => Colors.Blue,
                              Mode    => Gtk.Layered.Left,
                              Name    => "Thruster",
                              Sweeper => Gtk.Oscilloscope.Lower);
      end Add_Discretes;
   end Add_Plots;

   Reset_Timeline (Plot => Window.Plot);

   Add_Mission_Clock :
   declare
      type LED_Assignment is array (Glib.Gint range <>,
                                    Glib.Gint range <>) of Boolean;
      X : constant Boolean := True;
      O : constant Boolean := False;

      function New_LED return not null Gtk.Widget.Gtk_Widget;
      function New_LED return not null Gtk.Widget.Gtk_Widget is
         LED : Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular;
      begin
         Gtk.Gauge.LED_Rectangular.Gtk_New
           (Widget   => LED,
            On_Color => Gtk.Missed.RGB (Red   => 0.5,
                                        Green => 0.5,
                                        Blue  => 1.0),
            Off_Color => Colors.Black);
         return Gtk.Widget.Gtk_Widget (LED);
      end New_LED;

      procedure Write_Digit (Grid  : not null Gtk.Grid.Gtk_Grid;
                             Left  : in       Glib.Gint;
                             Top   : in       Glib.Gint;
                             Which : in       LED_Assignment);
      procedure Write_Digit (Grid  : not null Gtk.Grid.Gtk_Grid;
                             Left  : in       Glib.Gint;
                             Top   : in       Glib.Gint;
                             Which : in       LED_Assignment) is
         use type Glib.Gint;
      begin
         for X in Which'Range (2) loop
            for Y in Which'Range (1) loop
               Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
                 (Grid.all.Get_Child_At (Left => Left + X,
                                         Top  => Top  + Y)).all.
                 Set_State (State => Which (Y, X));
            end loop;
         end loop;
      end Write_Digit;

      LED_0 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (X, O, X),
                 2 => (X, O, X),
                 3 => (X, O, X),
                 4 => (X, X, X));

      LED_1 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (O, O, X),
                 1 => (O, O, X),
                 2 => (O, O, X),
                 3 => (O, O, X),
                 4 => (O, O, X));

      LED_2 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (O, O, X),
                 2 => (X, X, X),
                 3 => (X, O, O),
                 4 => (X, X, X));

      LED_3 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (O, O, X),
                 2 => (X, X, X),
                 3 => (O, O, X),
                 4 => (X, X, X));

      LED_4 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, O, X),
                 1 => (X, O, X),
                 2 => (X, X, X),
                 3 => (O, O, X),
                 4 => (O, O, X));

      LED_5 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (X, O, O),
                 2 => (X, X, X),
                 3 => (O, O, X),
                 4 => (X, X, X));

      LED_6 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (X, O, O),
                 2 => (X, X, X),
                 3 => (X, O, X),
                 4 => (X, X, X));

      LED_7 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (O, O, X),
                 2 => (O, O, X),
                 3 => (O, O, X),
                 4 => (O, O, X));

      LED_8 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (X, O, X),
                 2 => (X, X, X),
                 3 => (X, O, X),
                 4 => (X, X, X));

      LED_9 : constant LED_Assignment (0 .. 4, 0 .. 2) :=
                (0 => (X, X, X),
                 1 => (X, O, X),
                 2 => (X, X, X),
                 3 => (O, O, X),
                 4 => (X, X, X));

      Clk_Box : constant Gtk.Box.Gtk_Vbox      := Gtk.Box.Gtk_Vbox_New;
      Clk_Frame : constant Gtk.Frame.Gtk_Frame := Gtk.Frame.Gtk_Frame_New (Label => "Mission clock");
      Grid      : constant Gtk.Grid.Gtk_Grid   := Gtk.Grid.Gtk_Grid_New;
   begin
      Box.all.Pack_End (Child  => Clk_Box,
                        Expand => False);
      Clk_Box.all.Pack_Start (Child => Clk_Frame, Expand => False);
      Clk_Box.all.Pack_End (Child => Gtk.Box.Gtk_Hbox_New,
                        Expand => True);
      Clk_Frame.all.Add (Widget => Grid);
      Clk_Frame.all.Add (Widget => Gtk.Box.Gtk_Hbox_New);
      Grid.all.Set_Size_Request (135, 25);
      Grid.all.Set_Column_Homogeneous (Homogeneous => True);
      Grid.all.Set_Column_Spacing (Spacing => 0);
      Grid.all.Set_Row_Homogeneous (Homogeneous => True);
      Grid.all.Set_Row_Spacing (Spacing => 0);
      for X in Glib.Gint range 0 .. 26 loop
         Grid.all.Insert_Row (Position => X);
      end loop;

      for Y in Glib.Gint range 0 .. 4 loop
         Grid.all.Insert_Column (Position => Y);
      end loop;

      for X in Glib.Gint range 0 .. 26 loop
         for Y in Glib.Gint range 0 .. 4 loop
            Grid.all.Attach (Child => New_LED,
                             Left  => X,
                             Top   => Y);
         end loop;
      end loop;

      Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
        (Grid.all.Get_Child_At (8, 1)).all.Set_State (State => True);
      Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
        (Grid.all.Get_Child_At (8, 3)).all.Set_State (State => True);
      Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
        (Grid.all.Get_Child_At (18, 4)).all.Set_State (State => True);

      Write_Digit (Grid => Grid, Left =>  0, Top => 0, Which => LED_0);
      Write_Digit (Grid => Grid, Left =>  4, Top => 0, Which => LED_1);
      Write_Digit (Grid => Grid, Left => 10, Top => 0, Which => LED_2);
      Write_Digit (Grid => Grid, Left => 14, Top => 0, Which => LED_3);
      Write_Digit (Grid => Grid, Left => 20, Top => 0, Which => LED_4);
      Write_Digit (Grid => Grid, Left => 24, Top => 0, Which => LED_5);
   end Add_Mission_Clock;

   return Frame;
end Create_Timeline_Frame;
