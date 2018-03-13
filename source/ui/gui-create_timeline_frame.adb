with Gtk.Frame;
with Gtk.Missed;

separate (GUI)
function Create_Timeline_Frame
  (Window : in out Main_Window_Record) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Timeline");
begin
   Frame.all.Set_Size_Request (Width  => 800,
                               Height => 200);

   Add_Scope :
   declare
      Scope : Gtk.Oscilloscope.Gtk_Oscilloscope;
   begin
      Gtk.Oscilloscope.Gtk_New (Widget => Scope);
      Frame.all.Add (Widget => Scope);

      Window.Plot.Oscilloscope := Scope;
   end Add_Scope;

   Add_Plots :
   declare
      Plots : Plot_Elements renames Window.Plot;
      Scope : Gtk.Oscilloscope.Gtk_Oscilloscope_Record renames
                Gtk.Oscilloscope.Gtk_Oscilloscope_Record
                  (Window.Plot.Oscilloscope.all);
   begin
      Plots.Altitude_Channel := Scope.Add_Channel (Color => Colors.Red,
                                                   Name  => "Altitude");
      Plots.Fuel_Channel     := Scope.Add_Channel (Color => Colors.Blue,
                                                   Name  => "Fuel");
      Plots.Velocity_Channel := Scope.Add_Channel (Color => Colors.Purple,
                                                   Name  => "Velocity");

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
                   (Group => G,
                    Color => Gtk.Missed.RGB (Red   => Color_Offset,
                                             Green => 1.0,
                                             Blue  => Color_Offset),
                    Name  => "Touchdown" & Shared_Types.Legs_Index'Image (Leg));
            end Add_Leg_Discrete;
         end loop;

         Plots.Thruster_Channel := Scope.Add_Channel (Group => G,
                                                      Color => Colors.Blue,
                                                      Name  => "Thruster");
      end Add_Discretes;

      Scope.Set_Manual_Sweep (False);
      Scope.Set_Time_Axis (Sweeper => Gtk.Oscilloscope.Lower,
                           Visible => True,
                           As_Time => True);
   end Add_Plots;

   return Frame;
end Create_Timeline_Frame;
