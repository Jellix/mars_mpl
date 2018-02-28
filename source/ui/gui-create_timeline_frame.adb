with Gtk.Frame;
with Gtk.Missed;

separate (GUI)
function Create_Timeline_Frame
  (Window : in out Main_Window_Record'Class) return not null access
  Gtk.Widget.Gtk_Widget_Record'Class
is
   Frame : constant Gtk.Frame.Gtk_Frame :=
             Gtk.Frame.Gtk_Frame_New (Label => "Timeline");
begin
   Frame.all.Set_Size_Request (Width  => 800,
                               Height => 200);

   Add_Plots :
   declare
      Plot : Gtk.Oscilloscope.Gtk_Oscilloscope;
   begin
      Gtk.Oscilloscope.Gtk_New (Widget => Plot);
      Frame.all.Add (Widget => Plot);

      Window.Oscilloscope := Plot;

      Window.Altitude_Channel := Plot.all.Add_Channel (Color => Colors.Red,
                                                       Name  => "Altitude");
      Window.Fuel_Channel     := Plot.all.Add_Channel (Color => Colors.Blue,
                                                       Name  => "Fuel");
      Window.Velocity_Channel := Plot.all.Add_Channel (Color => Colors.Purple,
                                                       Name  => "Velocity");

      Add_Discretes :
      declare
         G : Gtk.Oscilloscope.Group_Number;
      begin
         G := Plot.all.Add_Group (Name => "Signals");

         for Leg in Shared_Types.Legs_Index loop
            Add_Leg_Discrete :
            declare
               Color_Offset : constant Glib.Gdouble :=
                                0.2 * Glib.Gdouble (Shared_Types.Legs_Index'Pos (Leg));
            begin
               Window.Touchdown_Channel (Leg) :=
                 Plot.all.Add_Channel
                   (Group => G,
                    Color => Gtk.Missed.RGB (Red   => Color_Offset,
                                             Green => 1.0,
                                             Blue  => Color_Offset),
                    Name  => "Touchdown" & Shared_Types.Legs_Index'Image (Leg));
            end Add_Leg_Discrete;
         end loop;

         Window.Thruster_Channel :=
           Plot.all.Add_Channel (Group => G,
                                 Color => Colors.Blue,
                                 Name  => "Thruster");
      end Add_Discretes;

      Plot.all.Set_Manual_Sweep (False);
      Plot.all.Set_Time_Axis (Sweeper => Gtk.Oscilloscope.Lower,
                              Visible => True,
                              As_Time => True);
   end Add_Plots;

   return Frame;
end Create_Timeline_Frame;
