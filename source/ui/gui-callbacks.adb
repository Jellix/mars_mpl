with Gtk.GEntry;
with Shared_Parameters;
with Shared_Sensor_Data;

package body GUI.Callbacks is

   procedure Quit_GUI;

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Quit_GUI;
   end Exit_Main;

   function Exit_Main (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                       Event  : in     Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      pragma Unreferenced (Widget);
   begin
      Quit_GUI;
      return False; --  continue event processing chain
   end Exit_Main;

   procedure Quit_GUI is
   begin
      if not Aborted then
         Trace (Message => "Quitting GUI...");
         Aborted := True;
      end if;
   end Quit_GUI;

   function Set_Fuel_Flow_Rate
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      GEntry : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry (Self);
   begin
      declare
         function Range_Image return String;
         function Range_Image return String is
         begin
            return
              Image (Value        => Shared_Types.Fuel_Mass'First,
                     Include_Unit => False)
              & " .. "
              & Image (Value        => Shared_Types.Fuel_Mass'Last,
                       Include_Unit => False);
         end Range_Image;
      begin
         Shared_Parameters.Fuel_Flow_Rate :=
           Shared_Types.Fuel_Mass'Value (GEntry.all.Get_Text);
      exception
         when Constraint_Error =>
            Trace (Message =>
                      "Bad input for fuel flow rate. Must be in range "
                   & Range_Image & ".");
      end;

      GEntry.all.Set_Text
        (Image (Value        => Shared_Parameters.Fuel_Flow_Rate,
                Include_Unit => False));

      return False;
   end Set_Fuel_Flow_Rate;

   function Set_Initial_Altitude
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      GEntry : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry (Self);
   begin
      declare
         function Range_Image return String;
         function Range_Image return String is
         begin
            return
              Image (Value        => Shared_Types.Altitude'First,
                     Include_Unit => False)
              & " .. "
              & Image (Value        => Shared_Types.Altitude'Last,
                       Include_Unit => False);
         end Range_Image;
      begin
         Shared_Parameters.Initial_Altitude :=
           Shared_Types.Altitude'Value (GEntry.all.Get_Text);
      exception
         when Constraint_Error =>
            Trace (Message =>
                      "Bad input for initial altitude. Must be in range "
                   & Range_Image & ".");
      end;

      GEntry.all.Set_Text
        (Image (Value        => Shared_Parameters.Initial_Altitude,
                Include_Unit => False));

      return False;
   end Set_Initial_Altitude;

   function Set_Initial_Fuel_Mass
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      GEntry : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry (Self);
   begin
      declare
         function Range_Image return String;
         function Range_Image return String is
         begin
            return
              Image (Value        => Shared_Types.Fuel_Mass'First,
                     Include_Unit => False)
              & " .. "
              & Image (Value        => Shared_Types.Fuel_Mass'Last,
                       Include_Unit => False);
         end Range_Image;
      begin
         Shared_Parameters.Initial_Fuel_Mass :=
           Shared_Types.Fuel_Mass'Value (GEntry.all.Get_Text);
      exception
         when Constraint_Error =>
            Trace (Message =>
                      "Bad input for initial altitude. Must be in range "
                   & Range_Image & ".");
      end;

      GEntry.all.Set_Text
        (Image (Value        => Shared_Parameters.Initial_Fuel_Mass,
                Include_Unit => False));

      return False;
   end Set_Initial_Fuel_Mass;

   function Set_Initial_Velocity
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      GEntry : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry (Self);
   begin
      declare
         function Range_Image return String;
         function Range_Image return String is
         begin
            return
              Image (Value        => Shared_Types.Velocity'First,
                     Include_Unit => False)
              & " .. "
              & Image (Value        => Shared_Types.Velocity'Last,
                       Include_Unit => False);
         end Range_Image;
      begin
         Shared_Parameters.Initial_Velocity :=
           Shared_Types.Velocity'Value (GEntry.all.Get_Text);
      exception
         when Constraint_Error =>
            Trace (Message =>
                      "Bad input for initial velocity. Must be in range "
                   & Range_Image & ".");
      end;

      GEntry.all.Set_Text
        (Image (Value        => Shared_Parameters.Initial_Velocity,
                Include_Unit => False));

      return False;
   end Set_Initial_Velocity;

   function Set_Thruster_Acceleration
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      GEntry : constant Gtk.GEntry.Gtk_Entry := Gtk.GEntry.Gtk_Entry (Self);
   begin
      declare
         function Range_Image return String;
         function Range_Image return String is
         begin
            return
              Image (Value        => Shared_Types.Acceleration'First,
                     Include_Unit => False)
              & " .. "
              & Image (Value        => Shared_Types.Acceleration'Last,
                       Include_Unit => False);
         end Range_Image;
      begin
         Shared_Parameters.Thruster_Acceleration :=
           Shared_Types.Acceleration'Value (GEntry.all.Get_Text);
      exception
         when Constraint_Error =>
            Trace (Message =>
                      "Bad input for thruster acceleration. Must be in range "
                   & Range_Image & ".");
      end;

      GEntry.all.Set_Text
        (Image (Value        => Shared_Parameters.Thruster_Acceleration,
                Include_Unit => False));

      return False;
   end Set_Thruster_Acceleration;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      if SIM_Pid /= GNAT.OS_Lib.Invalid_Pid then
         Trace
           (Message =>
              "Aborting simulator.exe... (PID ="
            & Integer'Image (GNAT.OS_Lib.Pid_To_Integer (Pid => SIM_Pid))
            & ")");
      end if;

      GNAT.OS_Lib.Kill (Pid => SIM_Pid);
      SIM_Pid := GNAT.OS_Lib.Invalid_Pid;
   end SIM_Abort;

   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
      Pid : GNAT.OS_Lib.Process_Id;
   begin
      Pid := GNAT.OS_Lib.Non_Blocking_Spawn (Program_Name => "simulator.exe",
                                             Args         => (1 .. 0 => null),
                                             Output_File  => "CON");

      if Pid = GNAT.OS_Lib.Invalid_Pid then
         Trace (Message => "Failed to start simulator.exe!");
      else
         Trace
           (Message =>
              "Simulator.exe started (PID ="
            & Integer'Image (GNAT.OS_Lib.Pid_To_Integer (Pid => Pid)) & ")");
      end if;

      SIM_Pid := Pid;
   end SIM_Start;

   function Switch_Bug (Self  : access Gtk.Switch.Gtk_Switch_Record'Class;
                        State : Boolean) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Shared_Sensor_Data.Bug_Enabled := State;
      Trace (Message =>
               "TDM bug " & (if State then "en" else "dis") & "abled.");
      return False;
   end Switch_Bug;

end GUI.Callbacks;
