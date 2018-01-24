with GNATCOLL.Traces;

with Global;

package body GUI_Callbacks is

   Logger : constant GNATCOLL.Traces.Trace_Handle
     := GNATCOLL.Traces.Create (Unit_Name => "GUI",
                                Default   => GNATCOLL.Traces.On,
                                Stream    => Global.Standard_Error);

   procedure Quit_GUI;

   procedure Exit_Main (Button : access Gtk.Button.Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);
   begin
      Quit_GUI;
   end Exit_Main;

   procedure Exit_Main (Win : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (Win);
   begin
      Quit_GUI;
   end Exit_Main;

   procedure Quit_GUI is
   begin
      if not Aborted then
         Logger.all.Trace
           (Message => "[" & Global.Clock_Image & "] Quitting GUI...");
         Aborted := True;
      end if;
   end Quit_GUI;

end GUI_Callbacks;
