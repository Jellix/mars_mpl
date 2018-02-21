--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--        On_Button_Release                        Summer, 2011       --
--  Separate body                                                     --
--                                Last revision :  22:22 11 Apr 2016  --
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
-- __________________________________________________________________ --

pragma Warnings (Off, "declaration hides ""Box""");
pragma Warnings (Off, "declaration hides ""Menu""");
pragma Warnings (Off, "declaration hides ""Oscilloscope""");

separate (Gtk.Oscilloscope)
function On_Button_Release
  (Object       : access GObject_Record'Class;
   Event        : Gdk.Event.Gdk_Event;
   Oscilloscope : Gtk_Oscilloscope) return Boolean
is
   pragma Unreferenced (Object);
   Menu      : Gtk.Menu.Gtk_Menu;
   Item      : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
   Separator : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
   Icon      : Gtk.Image.Gtk_Image;
   Box       : Cairo.Ellipses.Cairo_Box;
begin
   case Gdk.Event.Get_Button (Event) is
      when 1 =>
         if Oscilloscope.all.Selection.all.Area /= null then
            Oscilloscope.all.Change_Selection
              (Oscilloscope.all.Mouse_Event (Event, False));
            Box := Oscilloscope.all.Selection.all.Area.all.Get_Box;
            if Box.X2 - Box.X1 < 2.0 or else Box.Y2 - Box.Y1 < 2.0 then
               Oscilloscope.all.Restore_State;
               Free (Oscilloscope.all.Selection.all.Area);
            else
               case Oscilloscope.all.Selection_Mode is
                  when Interactive =>
                     Gtk.Menu.Gtk_New (Menu);
                     -- Zoom in
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-zoom-in"));
                     Gtk.Image.Gtk_New
                       (Icon,
                        Gtk.Stock.Stock_Zoom_In,
                        Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Zoom_In'Access,
                        Oscilloscope);
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-zoom-in-t"));
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Zoom_In_T'Access,
                        Oscilloscope);
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-zoom-in-v"));
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Zoom_In_V'Access,
                        Oscilloscope);
                     -- Zoom out
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-zoom-out"));
                     Gtk.Image.Gtk_New
                       (Icon,
                        Gtk.Stock.Stock_Zoom_Out,
                        Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Zoom_Out'Access,
                        Oscilloscope);
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-zoom-out-t"));
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Zoom_Out_T'Access,
                        Oscilloscope);
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-zoom-out-v"));
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Zoom_Out_V'Access,
                        Oscilloscope);
                     -- Copy values
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-copy-values"));
                     Gtk.Image.Gtk_New
                       (Icon, Gtk.Stock.Stock_Copy, Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Copy_Selection'Access,
                        Oscilloscope);
                     -- Copy differences
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-copy-differences"));
                     Gtk.Image.Gtk_New
                       (Icon, Gtk.Stock.Stock_Remove, Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Difference_Selection'Access,
                        Oscilloscope);
                     -- Copy range
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-copy-range"));
                     Gtk.Image.Gtk_New
                       (Icon, Gtk.Stock.Stock_Paste, Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Range_Selection'Access,
                        Oscilloscope);
                     -- Separator
                     Gtk.Separator_Menu_Item.Gtk_New (Separator);
                     Gtk.Menu.Append (Menu, Separator);
                     -- Delete selection
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-cancel"));
                     Gtk.Image.Gtk_New
                       (Icon, Gtk.Stock.Stock_Cancel, Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Menu_Handlers.Connect
                       (Item,
                        "activate",
                        On_Cancel_Selection'Access,
                        Oscilloscope);
                     Menu_Handlers.Connect
                       (Menu,
                        "destroy",
                        On_Cancel_Selection'Access,
                        Oscilloscope);
                     Gtk.Menu.Show_All (Menu);
                     Gtk.Menu.Popup
                       (Menu,
                        Button        => Gdk.Event.Get_Button (Event),
                        Activate_Time => Gdk.Event.Get_Time (Event));
                  when Zoom_In =>
                     On_Zoom_In (Oscilloscope, Oscilloscope);
                  when Zoom_In_Time =>
                     On_Zoom_In_T (Oscilloscope, Oscilloscope);
                  when Zoom_In_Values =>
                     On_Zoom_In_V (Oscilloscope, Oscilloscope);
                  when Zoom_Out =>
                     On_Zoom_Out (Oscilloscope, Oscilloscope);
                  when Zoom_Out_Time =>
                     On_Zoom_Out_T (Oscilloscope, Oscilloscope);
                  when Zoom_Out_Values =>
                     On_Zoom_Out_V (Oscilloscope, Oscilloscope);
                  when Copy_Range =>
                     On_Range_Selection (Oscilloscope, Oscilloscope);
                  when Copy_Values =>
                     On_Copy_Selection (Oscilloscope, Oscilloscope);
                  when Copy_Differences =>
                     On_Difference_Selection
                       (Oscilloscope,
                        Oscilloscope);
                  when User_Action =>
                     declare
                        Box : constant Cairo.Ellipses.Cairo_Box :=
                                Oscilloscope.all.Selection.all.Area.all.Get_Box;
                     begin
                        Free (Oscilloscope.all.Selection.all.Area);
                        Oscilloscope.all.Restore_State;
                        Oscilloscope.all.On_Selection (Box);
                     end;
                  when None =>
                     Free (Oscilloscope.all.Selection.all.Area);
                     Oscilloscope.all.Restore_State;
               end case;
            end if;
         end if;
      when others =>
         null;
   end case;
   return True;
exception
   when Error : others =>
      Glib.Messages.Log
        (Gtk.Missed.GtkAda_Contributions_Domain,
         Glib.Messages.Log_Level_Critical,
         "Fault: " & Ada.Exceptions.Exception_Information (Error) &
           Where ("On_Button_Release"));
      return True;
end On_Button_Release;

pragma Warnings (On, "declaration hides ""Box""");
pragma Warnings (On, "declaration hides ""Menu""");
pragma Warnings (On, "declaration hides ""Oscilloscope""");
