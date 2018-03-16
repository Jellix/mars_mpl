with Gtk.Frame;
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

   return Frame;
end Create_Timeline_Frame;
