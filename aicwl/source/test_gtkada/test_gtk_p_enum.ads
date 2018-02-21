with Gtk.Generic_Enum_Combo_Box;

package Test_Gtk_P_Enum is

   type Enum_Type is (Dark_Green, Light_Blue, Crimson_Red);

   package Enum_Combo is new Gtk.Generic_Enum_Combo_Box (Enum_Type);

end Test_Gtk_P_Enum;

