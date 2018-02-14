with Gtk.GEntry;
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
         Log.Trace (Message => "Quitting GUI...");
         Aborted := True;
      end if;
   end Quit_GUI;

   function Set_GEntry_Value
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
              Image (Value        => T'First,
                     Include_Unit => False)
              & " .. "
              & Image (Value        => T'Last,
                       Include_Unit => False);
         end Range_Image;
      begin
         Target := T'Value (GEntry.all.Get_Text);
      exception
         when Constraint_Error =>
            Log.Trace (Message =>
                          "Bad input for " & Name & ". Must be in range "
                       & Range_Image & ".");
      end;

      GEntry.all.Set_Text (Image (Value        => Target,
                                  Include_Unit => False));

      return False;
   end Set_GEntry_Value;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      if SIM_Pid /= GNAT.OS_Lib.Invalid_Pid then
         Log.Trace
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
         Log.Trace (Message => "Failed to start simulator.exe!");
      else
         Log.Trace
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
      Log.Trace (Message =>
                   "TDM bug " & (if State then "en" else "dis") & "abled.");
      return False;
   end Switch_Bug;

end GUI.Callbacks;
