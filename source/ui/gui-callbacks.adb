with GNAT.OS_Lib;
with Gtk.GEntry;
with Shared_Parameters.Write;

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
      Handle_Invalid_Data :
      begin
         Write (Value => T'Value (GEntry.all.Get_Text));
      exception
         when Constraint_Error =>
            Log.Trace (Message =>
                          "Bad input for " & Name & ". Must be in range "
                       & Range_Image & ".");
      end Handle_Invalid_Data;

      GEntry.all.Set_Text (Image (Value        => Read,
                                  Include_Unit => False));

      return False;
   end Set_GEntry_Value;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      if SIM_Pid /= GNAT.Expect.Invalid_Pid then
         Log.Trace
           (Message =>
              "Aborting simulator.exe... (PID ="
            & GNAT.Expect.Process_Id'Image (SIM_Pid)
            & ")");
         GNAT.Expect.Close (Descriptor => SIM_Process.all);
      end if;

      SIM_Pid := GNAT.Expect.Invalid_Pid;
   end SIM_Abort;

   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
      Pid : GNAT.Expect.Process_Id;
   begin
      Handle_Exception :
      begin
         GNAT.Expect.Non_Blocking_Spawn
           (Descriptor   => SIM_Process.all,
            Command      => "simulator.exe",
            Args         => GNAT.OS_Lib.Argument_List'(1 .. 0 => null));

         Pid := GNAT.Expect.Get_Pid (Descriptor => SIM_Process.all);

         Log.Trace
           (Message =>
              "Simulator.exe started (PID ="
            & GNAT.Expect.Process_Id'Image (Pid) & ")");
      exception
         when GNAT.Expect.Invalid_Process =>
            Log.Trace (Message => "Failed to start simulator.exe!");
            Pid := GNAT.Expect.Invalid_Pid;
      end Handle_Exception;

      SIM_Pid := Pid;
   end SIM_Start;

   function Switch_Bug (Self  : access Gtk.Switch.Gtk_Switch_Record'Class;
                        State : in     Boolean) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Shared_Parameters.Write.TDM_Bug_Enabled (Value => State);
      Log.Trace (Message =>
                   "TDM bug " & (if State then "en" else "dis") & "abled.");
      return False;
   end Switch_Bug;

end GUI.Callbacks;
