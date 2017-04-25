--                                                                    --
--  procedure Test_Gtk_Color        Copyright (c)  Dmitry A. Kazakov  --
--  Test for Gdk.Color.IHLS                        Luebeck            --
--                                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  09:44 08 Oct 2016  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Gdk.Color;            use Gdk.Color; 
with Gdk.Color.IHLS;       use Gdk.Color.IHLS; 
with GLib;                 use GLib;
with GtkAda.Handlers;      use GtkAda.Handlers;
with Gtk.Color_Selection;  use Gtk.Color_Selection;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Window;           use Gtk.Window;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Table;            use Gtk.Table;
with Gtk.Label;            use Gtk.Label;

with Ada.Unchecked_Conversion;
with Gtk.Main;
with Gtk.Missed;

procedure Test_Gtk_Color is
   Window     : Gtk_Window;
   Grid       : Gtk_Table;
   Label      : Gtk_Label;
   Hue        : Gtk_Label;
   Saturation : Gtk_Label;
   Luminance  : Gtk_Label;
   R          : Gtk_Label;
   G          : Gtk_Label;
   B          : Gtk_Label;
   Selection  : Gtk_Color_Selection;

   type Local_Callback is access procedure
        (  Selection : access Gtk_Color_Selection_Record'Class
        );
   function "+" is
      new Ada.Unchecked_Conversion
          (  Local_Callback,
             Cb_Gtk_Color_Selection_Void
          );

   procedure On_Color_Change
             (  Selection : access Gtk_Color_Selection_Record'Class
             )  is
      type D is delta 0.001 range 0.0..100.0;
      type F is delta 0.001 range 0.0..256.0;
      type H is delta 0.001 range 0.0..360.0;
      Color : Gdk_Color;
   begin
      Get_Current_Color (Selection, Color);
      declare
         Pixel : Gdk_IHLS_Color;
      begin
         Pixel := To_IHLS (Color);
         Set_Text
         (  Hue,
            H'Image
            (  H
               (  360.0
               *  Float (Pixel.Hue)
               /  Float (Gdk_Hue'Modulus)
         )  )  );
         Set_Text
         (  Saturation,
            D'Image (D (Float (Pixel.Saturation) / 655.36))
         );
         Set_Text
         (  Luminance,
            D'Image (D (Float (Pixel.Luminance) / 655.36))
         );
         Color := To_RGB (Pixel);
         Set_Text (R, F'Image (F (Float (Red   (Color)) / 256.0)));
         Set_Text (G, F'Image (F (Float (Green (Color)) / 256.0)));
         Set_Text (B, F'Image (F (Float (Blue  (Color)) / 256.0)));
      end;   
   end On_Color_Change;

begin
   Gtk.Main.Init;
   Gtk.Window.Gtk_New (Window);
   Window.Set_Title ("Test Color");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
   Gtk_New (Grid, 3, 6, False);
   Window.Add (Grid);
   Gtk_New (Selection);
   Selection.On_Color_Changed (+On_Color_Change'Access);
   Attach (Grid, Selection, 0, 1, 0, 6);
   
   Gtk_New (Label, "IHLS Hue:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 1, 2, 0, 1);
   Gtk_New (Label, "Saturation:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 1, 2, 1, 2);
   Gtk_New (Label, "Luminance:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 1, 2, 2, 3);
   Gtk_New (Label, "Red:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 1, 2, 3, 4);
   Gtk_New (Label, "Green:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 1, 2, 4, 5);
   Gtk_New (Label, "Blue:");
   Label.Set_Halign (Align_End);
   Label.Set_Valign (Align_Center);
   Attach (Grid, Label, 1, 2, 5, 6);

   Gtk_New (Hue);
   Hue.Set_Halign (Align_Start);
   Hue.Set_Valign (Align_Center);
   Attach (Grid, Hue, 2, 3, 0, 1);
   Gtk_New (Saturation);
   Saturation.Set_Halign (Align_Start);
   Saturation.Set_Valign (Align_Center);
   Attach (Grid, Saturation, 2, 3, 1, 2);
   Gtk_New (Luminance);
   Luminance.Set_Halign (Align_Start);
   Luminance.Set_Valign (Align_Center);
   Attach (Grid, Luminance,  2, 3, 2, 3);
   Gtk_New (R);
   R.Set_Halign (Align_Start);
   R.Set_Valign (Align_Center);
   Attach (Grid, R, 2, 3, 3, 4);
   Gtk_New (G);
   G.Set_Halign (Align_Start);
   G.Set_Valign (Align_Center);
   Attach (Grid, G, 2, 3, 4, 5);
   Gtk_New (B);
   B.Set_Halign (Align_Start);
   B.Set_Valign (Align_Center);
   Attach (Grid, B, 2, 3, 5, 6);

   Set_Col_Spacings (Grid, 5);

   Show_All (Grid);
   Show (Window);
   -- Enter the events processing loop
   Gtk.Main.Main;
end Test_Gtk_Color;
