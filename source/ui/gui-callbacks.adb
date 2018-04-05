with Glib;
with GNAT.OS_Lib;
with Shared_Parameters.Write;

package body GUI.Callbacks is

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Main_Window (Button.all.Get_Toplevel).all.Quit_GUI;
   end Exit_Main;

   function Exit_Main (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                       Event  : in     Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Main_Window (Widget).all.Quit_GUI;
      return True; --  stop event processing chain
   end Exit_Main;

   procedure Reset_Value (Button : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      Write (Value => Default);
      Main_Window (Button.all.Get_Toplevel).all.Text_Entries (Text_Entry).all.
        Set_Value (Value => Glib.Gdouble (Default));
   end Reset_Value;

   function Set_Spin_Button_Value
     (Self  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : in     Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      SB : constant Gtk.Spin_Button.Gtk_Spin_Button :=
             Gtk.Spin_Button.Gtk_Spin_Button (Self);

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
         Write (Value => T (SB.all.Get_Value));
      exception
         when Constraint_Error =>
            Log.Trace (Message =>
                          "Bad input for " & Name & ". Must be in range "
                       & Range_Image & ".");
      end Handle_Invalid_Data;

      SB.all.Set_Value (Glib.Gdouble (Read));

      return False;
   end Set_Spin_Button_Value;

   procedure SIM_Abort (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Win     : constant Main_Window := Main_Window (Button.all.Get_Toplevel);
      Process : constant GNAT.Expect.Process_Descriptor_Access :=
                  Win.all.SIM_Process;
   begin
      Log.Trace
        (Message =>
           "Aborting simulator.exe... (PID ="
         & GNAT.Expect.Process_Id'Image (GNAT.Expect.Get_Pid (Process.all))
         & ")");
      GNAT.Expect.Close (Descriptor => Process.all);
      --  We do not handle the Invalid_Process exception which Close() may
      --  raise, because this is only expected if the process id is invalid.
      --  Yet, at this point we have a valid Pid even if the process might have
      --  been terminated already, thus we do not expect this exception to
      --  occur.

      --  Simulator should be aborted, hence disable "Abort" and enable "Start".
      Button.all.Set_Sensitive (Sensitive => False);
      Win.all.Buttons (Start_Button).all.Set_Sensitive (Sensitive => True);
   end SIM_Abort;

   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      Win     : constant Main_Window := Main_Window (Button.all.Get_Toplevel);
      Process : constant GNAT.Expect.Process_Descriptor_Access :=
                  Win.all.SIM_Process;
   begin
      Handle_Exception :
      begin
         GNAT.Expect.Non_Blocking_Spawn
           (Descriptor   => Process.all,
            Command      => "simulator",
            Args         => GNAT.OS_Lib.Argument_List'(1 .. 0 => null));

         Log.Trace
           (Message =>
              "simulator started (PID ="
            & GNAT.Expect.Process_Id'Image
                (GNAT.Expect.Get_Pid (Descriptor => Process.all))
            & ")");
         Reset_Timeline (Plot => Win.all.Plot);

         --  Simulator is running, hence disable "Start" and enable "Abort".
         Button.all.Set_Sensitive (Sensitive => False);
         Win.all.Buttons (Abort_Button).all.Set_Sensitive (Sensitive => True);
      exception
         when GNAT.Expect.Invalid_Process =>
            Log.Trace (Message => "Failed to start simulator!");
      end Handle_Exception;
   end SIM_Start;

   procedure Switch_Bug
     (Self : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class)
   is
      State : constant Boolean := Self.all.Get_Active;
   begin
      Shared_Parameters.Write.TDM_Bug_Enabled (Value => State);
      Log.Trace (Message =>
                   "TDM bug " & (if State then "en" else "dis") & "abled.");
   end Switch_Bug;

end GUI.Callbacks;
