with GNAT.OS_Lib;

with Gtk.Dialog;
with Gtk.GEntry;
with Gtk.Message_Dialog;
with Shared_Parameters.Write;

package body GUI.Callbacks is

   use type Gtk.Dialog.Gtk_Response_Type;

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
      return True; --  stop event processing chain
   end Exit_Main;

   procedure Quit_GUI is
      Do_Abort : Boolean := not Aborted;
   begin
      if Simulator_Running and then Do_Abort then
         User_Confirm :
         declare
            Confirm : constant Gtk.Message_Dialog.Gtk_Message_Dialog :=
                        Gtk.Message_Dialog.Gtk_Message_Dialog_New
                          (Parent   => The_Main_Window,
                           Flags    => Gtk.Dialog.Modal,
                           The_Type => Gtk.Message_Dialog.Message_Question,
                           Buttons  => Gtk.Message_Dialog.Buttons_Yes_No,
                           Message  =>
                             "Simulator seems still running, quit anyway?");
         begin
            Do_Abort := Confirm.all.Run = Gtk.Dialog.Gtk_Response_Yes;
            Confirm.all.Destroy;
         end User_Confirm;
      end if;

      if Do_Abort then
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
      if Simulator_Running then
         Log.Trace
           (Message =>
              "Aborting simulator.exe... (PID ="
            & GNAT.Expect.Process_Id'Image
                (GNAT.Expect.Get_Pid (SIM_Process.all))
            & ")");
         GNAT.Expect.Close (Descriptor => SIM_Process.all);
         --  We do not handle the Invalid_Process exception which Close() may
         --  raise, because this is only expected if the process id is invalid.
         --  Yet, at this point we have a valid Pid even if the process might
         --  have been terminated already, thus we do not expect this exception
         --  to occur.
      end if;
   end SIM_Abort;

   procedure SIM_Start (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Handle_Exception :
      begin
         GNAT.Expect.Non_Blocking_Spawn
           (Descriptor   => SIM_Process.all,
            Command      => "simulator",
            Args         => GNAT.OS_Lib.Argument_List'(1 .. 0 => null));

         Log.Trace
           (Message =>
              "simulator started (PID ="
            & GNAT.Expect.Process_Id'Image
                (GNAT.Expect.Get_Pid (Descriptor => SIM_Process.all))
            & ")");
      exception
         when GNAT.Expect.Invalid_Process =>
            Log.Trace (Message => "Failed to start simulator!");
      end Handle_Exception;
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
