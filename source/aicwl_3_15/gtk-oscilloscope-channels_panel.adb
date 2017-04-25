--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope.Channels_Panel             Luebeck            --
--  Implementation                                 Summer, 2011       --
--                                                                    --
--                                Last revision :  22:46 07 Apr 2016  --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Gdk.RGBA;                    use Gdk.RGBA;
with GLib.Messages;               use GLib.Messages;
with GLib.Properties;             use GLib.Properties;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with GLib.Types;                  use GLib.Types;
with Gtk.Dialog;                  use Gtk.Dialog;
with Gtk.Cell_Layout;             use Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;    use Gtk.Cell_Renderer_Toggle;
with Gtk.Color_Selection_Dialog;  use Gtk.Color_Selection_Dialog;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Image;                   use Gtk.Image;
with Gtk.Separator_Menu_Item;     use Gtk.Separator_Menu_Item;
with Gtk.Stock;                   use Gtk.Stock;
with Gtk.Tree_Selection;          use Gtk.Tree_Selection;
with Gtk.Widget.Styles;           use Gtk.Widget.Styles;
with GtkAda.Types;                use GtkAda.Types;
with Strings_Edit.Integers;       use Strings_Edit.Integers;

with GLib.Object.Checked_Destroy;

package body Gtk.Oscilloscope.Channels_Panel is
   use Gtk.Layered.Waveform.Edit;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Oscilloscope.Channels_Panel." & Name;
   end Where;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Tree_View.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "channel-name-title",
               Default => "Channel",
               Nick    => "Channel name",
               Blurb   => "The title of the channel name column"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "group-name-title",
               Default => "Group",
               Nick    => "Group name",
               Blurb   => "The title of the group name column"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "menu-delete",
               Default => "Delete channel",
               Nick    => "Delete",
               Blurb   => "The menu item deleting selected channel"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "menu-down",
               Default => "Move channel down",
               Nick    => "Down",
               Blurb   => "The menu item moving selected channel down"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "menu-select-color",
               Default => "Select color",
               Nick    => "Color",
               Blurb   => "The menu item changing the color of " &
                          "the selected channel"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "menu-up",
               Default => "Move channel up",
               Nick    => "Down",
               Blurb   => "The menu item moving selected channel up"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "values-title",
               Default => "Values",
               Nick    => "Values",
               Blurb   => "The title of the values column"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget       : out Gtk_Oscilloscope_Channels_Panel;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class
             )  is
   begin
      Widget := new Gtk_Oscilloscope_Channels_Panel_Record;
      Initialize (Widget, Oscilloscope);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Oscilloscope_Channels_Panel_Record'Class;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Tree_View.Initialize (Widget);
      Widget.Set_Model (To_Interface (Oscilloscope.Get_Channel_List));
      Widget.Oscilloscope := Oscilloscope.all'Unchecked_Access;
      declare
         Icon   : Gtk_Image;
         Text   : Gtk_Cell_Renderer_Text;
         Toggle : Gtk_Cell_Renderer_Toggle;
         Column : Gtk_Tree_View_Column;
      begin
            -- Column 1
         Gtk_New (Column);
         Widget.Color_Column := Widget.Append_Column (Column) - 1;
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Tree_Functions.Set_Cell_Data_Func
         (  Column,
            Text,
            On_Render_Color'Access,
            Widget.all'Access
         );
            -- Column 2
         Gtk_New (Column);
         Widget.Name_Column := Widget.Append_Column (Column) - 1;
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 0);
            -- Column 3
         Gtk_New (Column);
         Widget.Visible_Column := Widget.Append_Column (Column) - 1;
         Gtk_New (Toggle);
         Column.Pack_Start (Toggle, False);
         Column.Add_Attribute (Toggle, "active", 3);
         Gtk_New (Icon, Stock_Disconnect, Icon_Size_Menu);
         Icon.Show_All;
         Column.Set_Widget (Icon);
         Connect
         (  Toggle,
            "toggled",
            On_Visible_Toggled'Access,
            Widget.all'Unchecked_Access
         );
            -- Column 4
         Gtk_New (Column);
         Widget.Mode_Column := Widget.Append_Column (Column) - 1;
         Gtk_New (Toggle);
         Column.Pack_Start (Toggle, False);
         Column.Add_Attribute (Toggle, "active", 4);
         Gtk_New (Icon, Stock_Italic, Icon_Size_Menu);
         Icon.Show_All;
         Column.Set_Widget (Icon);
         Connect
         (  Toggle,
            "toggled",
            On_Mode_Toggled'Access,
            Widget.all'Unchecked_Access
         );
            -- Column 5
         Gtk_New (Column);
         Widget.Values_Column := Widget.Append_Column (Column) - 1;
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 5);
            -- Column 6
         Gtk_New (Column);
         Widget.Group_Column := Widget.Append_Column (Column) - 1;
         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Tree_Functions.Set_Cell_Data_Func
         (  Column,
            Text,
            On_Render_Group'Access,
            Widget.all'Access
         );
      end;
      Widget.Set_Events (Button_Press_Mask);
      Connect
      (  Widget,
         "style-updated",
         On_Style_Updated'Access,
         Widget.all'Unchecked_Access
      );
      Connect
      (  Widget,
         "button_press_event",
         To_Marshaller (On_Button_Press'Access),
         Widget.all'Unchecked_Access
      );
      On_Style_Updated (Widget, Widget.all'Unchecked_Access);
   end Initialize;

   function On_Button_Press
            (  Object : access GObject_Record'Class;
               Event  : Gdk_Event;
               Panel  : Gtk_Oscilloscope_Channels_Panel
            )  return Boolean is
      use Menu_Handlers;
      Menu      : Gtk_Menu;
      Model     : Gtk_Tree_Model;
      Item      : Gtk_Image_Menu_Item;
      Selection : Gtk_Tree_Selection;
      Selected  : Gtk_Tree_Iter;
      Separator : Gtk_Separator_Menu_Item;
      Icon      : Gtk_Image;
   begin
      case Get_Button (Event) is
         when 3 =>
            Selection := Panel.Get_Selection;
            if Selection.Count_Selected_Rows = 1 then
               Selection.Get_Selected (Model, Selected);
               if Selected /= Null_Iter then
                  Panel.Channel :=
                     Channel_Count (Get_Int (Model, Selected, 1));
                  Gtk_New (Menu);
                     -- Move up
                  if Panel.Channel > 1 then
                     Gtk_New (Item, Style_Get (Panel, "menu-up"));
                     Gtk_New (Icon, Stock_Go_Up, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Menu_Up'Access,
                        Panel
                     );
                  end if;
                     -- Move down
                  if (  Panel.Channel
                     <  Panel.Oscilloscope.Get_Channels_Number
                     )
                  then
                     Gtk_New (Item, Style_Get (Panel, "menu-down"));
                     Gtk_New (Icon, Stock_Go_Down, Icon_Size_Menu);
                     Set_Image (Item, Icon);
                     Append (Menu, Item);
                     Connect
                     (  Item,
                        "activate",
                        On_Menu_Down'Access,
                        Panel
                     );
                  end if;
                  Gtk_New
                  (  Item,
                     Style_Get (Panel, "menu-select-color")
                  );
                  Gtk_New (Icon, Stock_Select_Color, Icon_Size_Menu);
                  Set_Image (Item, Icon);
                  Append (Menu, Item);
                  Connect
                  (  Item,
                     "activate",
                     On_Menu_Select_Color'Access,
                     Panel
                  );
                     -- Separator
                  Gtk_New (Separator);
                  Append (Menu, Separator);
                     -- Delete selection
                  Gtk_New (Item, Style_Get (Panel, "menu-delete"));
                  Gtk_New (Icon, Stock_Delete, Icon_Size_Menu);
                  Set_Image (Item, Icon);
                  Append (Menu, Item);
                  Connect
                  (  Item,
                     "activate",
                     On_Menu_Delete'Access,
                     Panel
                  );
                  Menu.Show_All;
                  Popup
                  (  Menu,
                     Button => Gdk.Event.Get_Button (Event),
                     Activate_Time => Gdk.Event.Get_Time (Event)
                  );
               end if;
            end if;
         when others =>
            null;
      end case;
      return False;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Button_Press")
         )  );
         return True;
   end On_Button_Press;

   procedure On_Menu_Delete
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
   begin
      if Panel.Channel > 0 then
         Panel.Oscilloscope.Delete_Channel (Panel.Channel);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Menu_Delete")
         )  );
   end On_Menu_Delete;

   procedure On_Menu_Down
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
   begin
      if (  Panel.Channel > 0
         and then
            Panel.Channel < Panel.Oscilloscope.Get_Channels_Number
         )
      then
         Panel.Oscilloscope.Move_Channel
         (  Panel.Channel,
            Panel.Channel + 1
         );
         Panel.Get_Selection.Select_Iter
         (  Panel.Oscilloscope.Channel_Names.Nth_Child
            (  Null_Iter,
               GInt (Panel.Channel)
         )  );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Menu_Down")
         )  );
   end On_Menu_Down;

   procedure On_Menu_Select_Color
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
      Dialog : Gtk_Color_Selection_Dialog;
      Color  : Gdk_Color;
   begin
      if Panel.Channel > 0 then
         Gtk_New (Dialog, "Change channel color");
         case Dialog.Run is
            when Gtk_Response_OK =>
               Dialog.Get_Color_Selection.Get_Current_Color (Color);
               Panel.Oscilloscope.Get_Waveform
               (  Panel.Channel
               ) .Set_Color (Color);
            when others =>
               null;
         end case;
         Dialog.Destroy;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Menu_Select_Color")
         )  );
   end On_Menu_Select_Color;

   procedure On_Menu_Up
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
   begin
      if Panel.Channel > 1 then
         Panel.Oscilloscope.Move_Channel
         (  Panel.Channel,
            Panel.Channel - 1
         );
         Panel.Get_Selection.Select_Iter
         (  Panel.Oscilloscope.Channel_Names.Nth_Child
            (  Null_Iter,
               GInt (Panel.Channel) - 2
         )  );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Menu_Up")
         )  );
   end On_Menu_Up;

   procedure On_Mode_Toggled
             (  Widget : access GObject_Record'Class;
                Values : GValues;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
      List : constant Gtk_List_Store :=
                Panel.Oscilloscope.Get_Channel_List;
      Row  : constant Gtk_Tree_Iter :=
                Get_Iter_From_String
                (  List,
                   Get_String (Nth (Values, 1))
                );
      Channel : Channel_Number;
      Mode    : Interpolation_Mode;
   begin
      if Row /= Null_Iter then
         Channel := Channel_Number (List.Get_Int (Row, 1));
         case Panel.Oscilloscope.Get_Interpolation_Mode (Channel) is
            when Left =>
               Mode := Linear;
            when Linear =>
               Mode := Left;
         end case;
         Panel.Oscilloscope.Set_Interpolation_Mode (Channel, Mode);
      end if;
   end On_Mode_Toggled;

   procedure On_Render_Color
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
      Channel : Channel_Number;
      Color   : Gdk_RGBA;
   begin
      Channel := Channel_Number (Get_Int (Model, Iter, 1));
      Color   := To_RGBA (Panel.Oscilloscope.Get_Color (Channel));
      Set_Property
      (  Cell,
         Cell_Background_RGBA_Property,
         Color
      );
      Set_Property
      (  Cell,
         Foreground_RGBA_Property,
         Color
      );
      Set_Property
      (  Cell,
         Cell_Renderer_Text.Text_Property,
         "  "
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Render_Color")
         )  );
   end On_Render_Color;

   procedure On_Render_Group
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
      Group : Group_Number;
   begin
      Group := Group_Number (Get_Int (Model, Iter, 2));
      Set_Property
      (  Cell,
         Cell_Renderer_Text.Text_Property,
         Panel.Oscilloscope.Get_Name (Group)
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Render_Group")
         )  );
   end On_Render_Group;

   procedure On_Visible_Toggled
             (  Widget : access GObject_Record'Class;
                Values : GValues;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
      List : constant Gtk_List_Store :=
                Panel.Oscilloscope.Get_Channel_List;
      Row  : constant Gtk_Tree_Iter :=
                Get_Iter_From_String
                (  List,
                   Get_String (Nth (Values, 1))
                );
      Channel : Channel_Number;
      Visible : Boolean;
   begin
      if Row /= Null_Iter then
         Channel := Channel_Number (List.Get_Int (Row, 1));
         Visible := not Panel.Oscilloscope.Is_Visible (Channel);
         Panel.Oscilloscope.Set_Visible (Channel, Visible);
      end if;
   end On_Visible_Toggled;

   procedure On_Style_Updated
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             )  is
   begin
      Panel.Get_Column (Panel.Name_Column).Set_Title
      (  Style_Get (Panel, "channel-name-title")
      );
      Panel.Get_Column (Panel.Group_Column).Set_Title
      (  Style_Get (Panel, "group-name-title")
      );
      Panel.Get_Column (Panel.Values_Column).Set_Title
      (  Style_Get (Panel, "values-title")
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Style_Updated")
         )  );
   end On_Style_Updated;

end Gtk.Oscilloscope.Channels_Panel;
