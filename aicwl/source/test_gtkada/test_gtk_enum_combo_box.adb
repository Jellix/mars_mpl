with Gtk.Box;      use Gtk.Box;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Missed;   use Gtk.Missed;
with Gtk.Window;   use Gtk.Window;
with Gtk.Widget;
with Gtk.Combo_Box;
with Test_Gtk_P_Enum;

procedure Test_Gtk_Enum_Combo_Box is
   Win : Gtk_Window;
   Ecp : Test_Gtk_P_Enum.Enum_Combo.Gtk_Enum_Combo_Box;
   Box : Gtk_VBox;

begin
   Gtk.Main.Init;

   Gtk_New (Win, Gtk.Enums.Window_Toplevel);
   Test_Gtk_P_Enum.Enum_Combo.Gtk_New (Ecp, Capitalize_First, True);
   Gtk_New_VBox (Box);
   Box.Pack_Start (Ecp, False, False);
   Win.Add (Box);

   Test_Gtk_P_Enum.Enum_Combo.Set_Active_Value (Ecp, Test_Gtk_P_Enum.Light_Blue);

   Win.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Win.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Win.Show_All;

   Gtk.Main.Main;
end Test_Gtk_Enum_Combo_Box;
