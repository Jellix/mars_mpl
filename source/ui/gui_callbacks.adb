with GNATCOLL.Traces;

with Global;
with Shared_Sensor_Data;

package body GUI_Callbacks is

   Logger : constant GNATCOLL.Traces.Trace_Handle
     := GNATCOLL.Traces.Create (Unit_Name => "GUI",
                                Default   => GNATCOLL.Traces.On,
                                Stream    => Global.Standard_Error);

   use type GNAT.OS_Lib.Process_Id;

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
         Logger.all.Trace
           (Message => "[" & Global.Clock_Image & "] Quitting GUI...");
         Aborted := True;
      end if;
   end Quit_GUI;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      if SIM_Pid /= GNAT.OS_Lib.Invalid_Pid then
         Logger.all.Trace
           (Message =>
              "[" & Global.Clock_Image & "] Aborting simulator.exe... (PID ="
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
         Logger.all.Trace
           (Message =>
              "[" & Global.Clock_Image & "] Failed to start simulator.exe!");
      else
         Logger.all.Trace
           (Message =>
              "[" & Global.Clock_Image & "] Simulator.exe started (PID ="
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
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image & "] TDM bug "
         & (if State then "en" else "dis") & "abled.");
      return False;
   end Switch_Bug;

end GUI_Callbacks;
