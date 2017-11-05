--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--        On_Button_Press                          Summer, 2011       --
--  Separate body                                                     --
--                                Last revision :  19:51 11 Apr 2016  --
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

separate (Gtk.Oscilloscope)
function On_Button_Press (Object       : access GObject_Record'Class;
                          Event        : Gdk.Event.Gdk_Event;
                          Oscilloscope : Gtk_Oscilloscope) return Boolean
is
   pragma Unreferenced (Object);

   procedure Free is new
     Ada.Unchecked_Deallocation (Gtk.Menu.Gtk_Menu_Record'Class,
                                 Gtk.Menu.Gtk_Menu);

   use Menu_Handlers;
   Menu : Gtk.Menu.Gtk_Menu;
   Item : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
   Icon : Gtk.Image.Gtk_Image;

   procedure Save;
   procedure Save is
      First : Boolean := True;
   begin
      for Group in 1 .. Oscilloscope.all.Groups_Number loop
         Oscilloscope.all.Save_Amplifier (Group, First);
         Oscilloscope.all.Set_Auto_Scaling (Group, False);
      end loop;
      for Index in Sweeper_Type'Range loop
         Oscilloscope.all.Save_Sweeper (Index, First);
         Oscilloscope.all.Set_Frozen (Index, True);
      end loop;
      Oscilloscope.all.Selection.all.Saved := True;
   end Save;
begin
   case Gdk.Event.Get_Button (Event) is
      when 1 =>
         if Oscilloscope.all.Selection_Mode /= None then
            if Oscilloscope.all.Selection.all.Area = null then
               Oscilloscope.all.Selection.all.Engaged := True;
               declare
                  Box   : constant Cairo.Ellipses.Cairo_Box := Oscilloscope.all.Get_Box;
                  Point : constant Cairo.Ellipses.Cairo_Tuple :=
                            Oscilloscope.all.Mouse_Event (Event, False);
               begin
                  if Point.X in Box.X1 .. Box.X2 and then
                    Point.Y in Box.Y1 .. Box.Y2
                  then
                     Oscilloscope.all.Selection.all.Area :=
                       Gtk.Layered.Rectangle.Add_Rectangle
                         (Under      => Oscilloscope.all.Layers,
                          Box        => (X1 => Point.X,
                                         X2 => Point.X,
                                         Y1 => Point.Y,
                                         Y2 => Point.Y),
                          Line_Width => 1.0,
                          Opacity    => 0.0,
                          Color      =>
                            Gtk.Widget.Styles.Style_Get
                              (Oscilloscope,
                               "selection-color",
                               Selection_Color)).all'Unchecked_Access;
                     Oscilloscope.all.Selection.all.Right := True;
                     Oscilloscope.all.Selection.all.Below := True;
                     Save;
                  end if;
               end;
               Oscilloscope.all.Selection.all.Engaged := False;
            else
               Oscilloscope.all.Change_Selection
                 (Oscilloscope.all.Mouse_Event (Event, False));
            end if;
         end if;
      when 3 =>
         declare
            Have_Menu : Boolean := False;
         begin
            Gtk.Menu.Gtk_New (Menu);
            if
              Oscilloscope.all.Manual_Sweep and then
              0 /= (Oscilloscope.all.Menu_Enabled and Hold_Release_Item)
            then
               for Sweeper in Oscilloscope.all.Time_Axis'Range loop
                  if Oscilloscope.all.Get_Frozen (Sweeper) then
                     -- Add release button
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-release"));
                     Gtk.Image.Gtk_New
                       (Icon,
                        Gtk.Stock.Stock_Media_Play,
                        Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Connect
                       (Item,
                        "activate",
                        On_Release'Access,
                        Oscilloscope);
                     Have_Menu := True;
                     exit;
                  end if;
               end loop;
               for Sweeper in Oscilloscope.all.Time_Axis'Range loop
                  if not Oscilloscope.all.Get_Frozen (Sweeper) then
                     -- Add hold button
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-pause"));
                     Gtk.Image.Gtk_New
                       (Icon,
                        Gtk.Stock.Stock_Media_Pause,
                        Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Connect
                       (Item,
                        "activate",
                        On_Pause'Access,
                        Oscilloscope);
                     Have_Menu := True;
                     exit;
                  end if;
               end loop;
            end if;
            -- Add latest
            if 0 /= (Oscilloscope.all.Menu_Enabled and Latest_Data_Item)
            then
               declare
                  Have_Time : Boolean := False;
               begin
                  for Sweeper in Sweeper_Type'Range loop
                     declare
                        Data : Time_Axis_Data renames
                                 Oscilloscope.all.Time_Axis (Sweeper);
                     begin
                        if Data.On and then Data.Time_Mode then
                           Have_Time := True;
                           exit;
                        end if;
                     end;
                  end loop;
                  if Have_Time then
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item,
                        Gtk.Widget.Styles.Style_Get
                          (Oscilloscope, "menu-latest"));
                     Gtk.Image.Gtk_New
                       (Icon,
                        Gtk.Stock.Stock_Media_Forward,
                        Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Connect
                       (Item,
                        "activate",
                        On_Latest'Access,
                        Oscilloscope);
                     Have_Menu := True;
                  end if;
               end;
            end if;
            -- Add undo
            if
              Oscilloscope.all.Undo_Stack.Actions /= null and then
              0 /= (Oscilloscope.all.Menu_Enabled and Undo_Redo_Item)
            then
               Gtk.Image_Menu_Item.Gtk_New
                 (Item,
                  Gtk.Widget.Styles.Style_Get (Oscilloscope, "menu-undo"));
               Gtk.Image.Gtk_New (Icon,
                                  Gtk.Stock.Stock_Undo,
                                  Gtk.Enums.Icon_Size_Menu);
               Gtk.Image_Menu_Item.Set_Image (Item, Icon);
               Gtk.Menu.Append (Menu, Item);
               Connect
                 (Item,
                  "activate",
                  On_Undo'Access,
                  Oscilloscope);
               Have_Menu := True;
            end if;
            -- Add redo
            if
              Oscilloscope.all.Redo_Stack.Actions /= null and then
              0 /= (Oscilloscope.all.Menu_Enabled and Undo_Redo_Item)
            then
               Gtk.Image_Menu_Item.Gtk_New
                 (Item,
                  Gtk.Widget.Styles.Style_Get (Oscilloscope, "menu-redo"));
               Gtk.Image.Gtk_New (Icon,
                                  Gtk.Stock.Stock_Redo,
                                  Gtk.Enums.Icon_Size_Menu);
               Gtk.Image_Menu_Item.Set_Image (Item, Icon);
               Gtk.Menu.Append (Menu, Item);
               Connect
                 (Item,
                  "activate",
                  On_Redo'Access,
                  Oscilloscope);
               Have_Menu := True;
            end if;
            -- Add toggle grid
            if 0 /= (Oscilloscope.all.Menu_Enabled and Grid_Item) then
               Gtk.Image_Menu_Item.Gtk_New
                 (Item,
                  Gtk.Widget.Styles.Style_Get
                    (Oscilloscope, "menu-toggle-grid"));
               Gtk.Image.Gtk_New (Icon,
                                  Gtk.Stock.Stock_Index,
                                  Gtk.Enums.Icon_Size_Menu);
               Gtk.Image_Menu_Item.Set_Image (Item, Icon);
               Gtk.Menu.Append (Menu, Item);
               Connect
                 (Item,
                  "activate",
                  On_Toggle_Grid'Access,
                  Oscilloscope);
               Have_Menu := True;
            end if;
            -- Add toggle interpolation
            if 0 /= (Oscilloscope.all.Menu_Enabled and Interpolation_Item)
            then
               Gtk.Image_Menu_Item.Gtk_New
                 (Item,
                  Gtk.Widget.Styles.Style_Get
                    (Oscilloscope, "menu-toggle-interpolation"));
               Gtk.Image.Gtk_New (Icon,
                                  Gtk.Stock.Stock_Italic,
                                  Gtk.Enums.Icon_Size_Menu);
               Gtk.Image_Menu_Item.Set_Image (Item, Icon);
               Gtk.Menu.Append (Menu, Item);
               Connect
                 (Item,
                  "activate",
                  On_Toggle_Interpolation'Access,
                  Oscilloscope);
               Have_Menu := True;
            end if;

            if
              Oscilloscope.all.Format /= No_Snapshot and then
              0 /= (Oscilloscope.all.Menu_Enabled and Snapshot_Item) and then
              Oscilloscope.all.File /= null and then
              Oscilloscope.all.File'Length > 0
            then -- Add snapshot button
               Gtk.Image_Menu_Item.Gtk_New
                 (Item,
                  (Gtk.Widget.Styles.Style_Get (Oscilloscope, "menu-snapshot") &
                     " to " & Oscilloscope.all.File.all));
               Gtk.Image.Gtk_New (Icon,
                                  Gtk.Stock.Stock_Save,
                                  Gtk.Enums.Icon_Size_Menu);
               Gtk.Image_Menu_Item.Set_Image (Item, Icon);
               Gtk.Menu.Append (Menu, Item);
               Connect
                 (Item,
                  "activate",
                  On_Snapshot'Access,
                  Oscilloscope);
               Have_Menu := True;
            end if;
            if Have_Menu then
               Gtk.Menu.Show_All (Menu);
               Gtk.Menu.Popup
                 (Menu,
                  Button        => Gdk.Event.Get_Button (Event),
                  Activate_Time => Gdk.Event.Get_Time (Event));
            else
               Menu.all.Destroy;
               Free (Menu);
            end if;
         end;
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
           Where ("On_Button_Press"));
      return True;

end On_Button_Press;
