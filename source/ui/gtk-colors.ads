with Gdk.Color;
with Gtk.Missed;

package Gtk.Colors is

   Black        : constant Gdk.Color.Gdk_Color;
   Blue         : constant Gdk.Color.Gdk_Color;
   Dark_Yellow  : constant Gdk.Color.Gdk_Color;
   Green        : constant Gdk.Color.Gdk_Color;
   Grey         : constant Gdk.Color.Gdk_Color;
   Light_Grey   : constant Gdk.Color.Gdk_Color;
   Light_Yellow : constant Gdk.Color.Gdk_Color;
   Purple       : constant Gdk.Color.Gdk_Color;
   Red          : constant Gdk.Color.Gdk_Color;
   White        : constant Gdk.Color.Gdk_Color;
   Yellow       : constant Gdk.Color.Gdk_Color;

private

   subtype Color is Gdk.Color.Gdk_Color;

   function RGB (R : in Glib.Gdouble;
                 G : in Glib.Gdouble;
                 B : in Glib.Gdouble) return Color renames Gtk.Missed.RGB;

   Black        : constant Color := RGB (R => 0.000, G => 0.000, B => 0.000);
   Blue         : constant Color := RGB (R => 0.000, G => 0.000, B => 1.000);
   Dark_Yellow  : constant Color := RGB (R => 1.000, G => 1.000, B => 0.200);
   Green        : constant Color := RGB (R => 0.000, G => 1.000, B => 0.000);
   Grey         : constant Color := RGB (R => 0.600, G => 0.600, B => 0.600);
   Light_Grey   : constant Color := RGB (R => 0.800, G => 0.800, B => 0.800);
   Light_Yellow : constant Color := RGB (R => 1.000, G => 1.000, B => 0.800);
   Purple       : constant Color := RGB (R => 1.000, G => 0.400, B => 0.400);
   Red          : constant Color := RGB (R => 1.000, G => 0.000, B => 0.000);
   White        : constant Color := RGB (R => 1.000, G => 1.000, B => 1.000);
   Yellow       : constant Color := RGB (R => 1.000, G => 1.000, B => 0.000);

end Gtk.Colors;
