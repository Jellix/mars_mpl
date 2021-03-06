with Gtk.Colors;
with Gtk.Frame;
with Gtk.Layered.Refresh_Engine;
with Gtk.Layered.Waveform.Sweeper;
with Gtk.Missed;

separate (GUI)
function Create_Timeline_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame    : constant not null Gtk.Frame.Gtk_Frame :=
                Gtk.Frame.Gtk_Frame_New (Label => "Timeline");
   Plot_Box : constant not null Gtk.Box.Gtk_Vbox :=
                Gtk.Box.Gtk_Vbox_New (Homogeneous => True,
                                      Spacing     => 0);
begin
   Frame.all.Add (Widget => Plot_Box);

   Add_Scopes :
   declare
      function New_Scope
        (Lower_Sweeper  : in              Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
         Upper_Sweeper  : in              Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
         Refresh_Engine : not null access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine)
         return not null Gtk.Oscilloscope.Gtk_Oscilloscope;
      --  Compiler issue: I tried passing the Master scope as parameter, so that
      --  the appropriate sweepers can be gotten directly from there instead of
      --  passing them as parameters, but as soon as New_Scope has a call to
      --  Master.Get_Sweeper, compilation with GNAT CE 2020 hangs.

      function New_Scope
        (Lower_Sweeper  : in              Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
         Upper_Sweeper  : in              Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
         Refresh_Engine : not null access Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine)
         return not null Gtk.Oscilloscope.Gtk_Oscilloscope
      is
         use type Gtk.Oscilloscope.Gtk_Oscilloscope;
         use type Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper;
         Scope : Gtk.Oscilloscope.Gtk_Oscilloscope;
      begin
         Gtk.Oscilloscope.Gtk_New
           (Widget         => Scope,
            Background     => Gtk.Colors.Light_Grey,
            Lower_Sweeper  => Lower_Sweeper,
            Upper_Sweeper  => Upper_Sweeper,
            Refresh_Engine => Refresh_Engine);
         --  A standard simulation runs about 5 minutes. With 100 datapoints/s
         --  this amounts to roughly 30000 distinct data points, thus the
         --  default buffer size of ~60_000 should still be enough.

         if Lower_Sweeper = null or else Upper_Sweeper = null then
            Scope.all.Set_Manual_Sweep (Enable => False);

            --  Lower axis.
            Scope.all.Set_Frozen (Sweeper => Gtk.Oscilloscope.Lower,
                                  Frozen  => True);
            Scope.all.Set_Time_Scale (Sweeper => Gtk.Oscilloscope.Lower,
                                      Visible => True);
            Scope.all.Set_Time_Axis (Sweeper => Gtk.Oscilloscope.Lower,
                                     Visible => True,
                                     As_Time => False);
         end if;

         return Scope;
      end New_Scope;

      Refresh_Engine : constant not null access
        Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine :=
          new Gtk.Layered.Refresh_Engine.Layered_Refresh_Engine;
      Master         : constant not null Gtk.Oscilloscope.Gtk_Oscilloscope :=
        New_Scope (Lower_Sweeper  => null,
                   Upper_Sweeper  => null,
                   Refresh_Engine => Refresh_Engine);
      Lower_Sweeper : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
        Master.all.Get_Sweeper (Sweeper => Gtk.Oscilloscope.Lower);
      Upper_Sweeper : constant Gtk.Layered.Waveform.Sweeper.Gtk_Waveform_Sweeper :=
        Master.all.Get_Sweeper (Sweeper => Gtk.Oscilloscope.Upper);
   begin
      Plot_Box.all.Pack_End (Child => Master);

      Window.Plot.Altitude_Plot := Master;

      Window.Plot.Discretes_Plot := New_Scope (Lower_Sweeper  => Lower_Sweeper,
                                               Upper_Sweeper  => Upper_Sweeper,
                                               Refresh_Engine => Refresh_Engine);
      Plot_Box.all.Pack_Start (Child => Window.Plot.Discretes_Plot);

      Window.Plot.Drag_Plot := New_Scope (Lower_Sweeper  => Lower_Sweeper,
                                          Upper_Sweeper  => Upper_Sweeper,
                                          Refresh_Engine => Refresh_Engine);
      Plot_Box.all.Pack_Start (Child => Window.Plot.Drag_Plot);

      Window.Plot.Fuel_Plot := New_Scope (Lower_Sweeper  => Lower_Sweeper,
                                          Upper_Sweeper  => Upper_Sweeper,
                                          Refresh_Engine => Refresh_Engine);
      Plot_Box.all.Pack_Start (Child => Window.Plot.Fuel_Plot);

      Window.Plot.Velocity_Plot := New_Scope (Lower_Sweeper  => Lower_Sweeper,
                                              Upper_Sweeper  => Upper_Sweeper,
                                              Refresh_Engine => Refresh_Engine);
      Plot_Box.all.Pack_Start (Child => Window.Plot.Velocity_Plot);

      Refresh_Engine.all.Set_Period (Period => 0.02);
   end Add_Scopes;

   Add_Plots :
   declare
      Plots : Plot_Elements renames Window.Plot;
   begin
      Plots.Altitude_Channel :=
        Plots.Altitude_Plot.all.Add_Channel (Color   => Gtk.Colors.Red,
                                             Mode    => Gtk.Layered.Linear,
                                             Name    => "Altitude",
                                             Sweeper => Gtk.Oscilloscope.Lower);
      Plots.Drag_Channel :=
        Plots.Drag_Plot.all.Add_Channel (Color   => Gtk.Colors.Dark_Yellow,
                                         Mode    => Gtk.Layered.Linear,
                                         Name    => "Drag",
                                         Sweeper => Gtk.Oscilloscope.Lower);

      Plots.Fuel_Channel     :=
        Plots.Fuel_Plot.all.Add_Channel (Color => Gtk.Colors.Blue,
                                         Mode  => Gtk.Layered.Linear,
                                         Name  => "Fuel",
                                         Sweeper => Gtk.Oscilloscope.Lower);
      Plots.Velocity_Channel :=
        Plots.Velocity_Plot.all.Add_Channel (Color => Gtk.Colors.Purple,
                                             Mode  => Gtk.Layered.Linear,
                                             Name  => "Velocity",
                                             Sweeper => Gtk.Oscilloscope.Lower);

      Add_Discretes :
      declare
         G : constant Gtk.Oscilloscope.Group_Number :=
               Plots.Discretes_Plot.all.Add_Group (Name => "Signals");
      begin
         for Leg in Shared_Types.Legs_Index loop
            Add_Leg_Discrete :
            declare
               Color_Offset : constant Glib.Gdouble :=
                                0.2 * Glib.Gdouble (Shared_Types.Legs_Index'Pos (Leg));
            begin
               Plots.Touchdown_Channel (Leg) :=
                 Plots.Discretes_Plot.all.Add_Channel
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
           Plots.Discretes_Plot.all.Add_Channel
             (Group   => G,
              Color   => Gtk.Colors.Blue,
              Mode    => Gtk.Layered.Left,
              Name    => "Thruster",
              Sweeper => Gtk.Oscilloscope.Lower);
      end Add_Discretes;
   end Add_Plots;

   Reset_Timeline (Plot => Window.Plot);

   return Frame;
end Create_Timeline_Frame;
