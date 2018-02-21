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
-- __________________________________________________________________ --

with Ada.Exceptions;

with Gdk.RGBA;

with Glib.Messages;
with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;
with Glib.Types;

with Gtk.Box;
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;
with Gtk.Color_Selection_Dialog;
with Gtk.Dialog;
with Gtk.Enums;
with Gtk.Image;
with Gtk.Image_Menu_Item;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Separator_Menu_Item;
with Gtk.Stock;
with Gtk.Tree_Selection;
with Gtk.Widget.Styles;

package body Gtk.Oscilloscope.Channels_Panel is

   pragma Warnings (Off, "declaration hides ""Dialog""");
   pragma Warnings (Off, "declaration hides ""Group""");
   pragma Warnings (Off, "declaration hides ""Menu""");
   pragma Warnings (Off, "declaration hides ""Oscilloscope""");
   pragma Warnings (Off, "declaration hides ""Values""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String;

   function Get_Type return GType is
   begin
      if
        Initialize_Class_Record
          (Ancestor     => Gtk.Tree_View.Get_Type,
           Class_Record => Class_Record'Access,
           Type_Name    => Class_Name)
      then
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "channel-name-title",
               Default => "Channel",
               Nick    => "Channel name",
               Blurb   => "The title of the channel name column"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "group-name-title",
               Default => "Group",
               Nick    => "Group name",
               Blurb   => "The title of the group name column"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "menu-delete",
               Default => "Delete channel",
               Nick    => "Delete",
               Blurb   => "The menu item deleting selected channel"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "menu-down",
               Default => "Move channel down",
               Nick    => "Down",
               Blurb   => "The menu item moving selected channel down"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "menu-select-color",
               Default => "Select color",
               Nick    => "Color",
               Blurb   => "The menu item changing the color of " &
                          "the selected channel"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "menu-up",
               Default => "Move channel up",
               Nick    => "Down",
               Blurb   => "The menu item moving selected channel up"));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "values-title",
               Default => "Values",
               Nick    => "Values",
               Blurb   => "The title of the values column"));
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New
     (Widget       : out Gtk_Oscilloscope_Channels_Panel;
      Oscilloscope : not null access Gtk_Oscilloscope_Record'Class) is
   begin
      Widget := new Gtk_Oscilloscope_Channels_Panel_Record;
      Initialize (Widget, Oscilloscope);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget  : not null access Gtk_Oscilloscope_Channels_Panel_Record'Class;
      Oscilloscope : not null access Gtk_Oscilloscope_Record'Class) is
   begin
      G_New (Widget, Get_Type);
      Gtk.Tree_View.Initialize (Widget);
      Widget.all.Set_Model
        (Gtk.Tree_Model.To_Interface (Oscilloscope.all.Get_Channel_List));
      Widget.all.Oscilloscope := Oscilloscope.all'Unchecked_Access;
      declare
         Icon   : Gtk.Image.Gtk_Image;
         Text   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
         Toggle : Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
         Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      begin
            -- Column 1
         Gtk.Tree_View_Column.Gtk_New (Column);
         Widget.all.Color_Column := Widget.all.Append_Column (Column) - 1;
         Gtk.Cell_Renderer_Text.Gtk_New (Text);
         Column.all.Pack_Start (Text, True);
         Tree_Functions.Set_Cell_Data_Func
           (Column,
            Text,
            On_Render_Color'Access,
            Widget.all'Access);
            -- Column 2
         Gtk.Tree_View_Column.Gtk_New (Column);
         Widget.all.Name_Column := Widget.all.Append_Column (Column) - 1;
         Gtk.Cell_Renderer_Text.Gtk_New (Text);
         Column.all.Pack_Start (Text, True);
         Column.all.Add_Attribute (Text, "text", 0);
            -- Column 3
         Gtk.Tree_View_Column.Gtk_New (Column);
         Widget.all.Visible_Column := Widget.all.Append_Column (Column) - 1;
         Gtk.Cell_Renderer_Toggle.Gtk_New (Toggle);
         Column.all.Pack_Start (Toggle, False);
         Column.all.Add_Attribute (Toggle, "active", 3);
         Gtk.Image.Gtk_New
           (Icon, Gtk.Stock.Stock_Disconnect, Gtk.Enums.Icon_Size_Menu);
         Icon.all.Show_All;
         Column.all.Set_Widget (Icon);
         Panel_Handlers.Connect
           (Toggle,
            "toggled",
            On_Visible_Toggled'Access,
            Widget.all'Unchecked_Access);
            -- Column 4
         Gtk.Tree_View_Column.Gtk_New (Column);
         Widget.all.Mode_Column := Widget.all.Append_Column (Column) - 1;
         Gtk.Cell_Renderer_Toggle.Gtk_New (Toggle);
         Column.all.Pack_Start (Toggle, False);
         Column.all.Add_Attribute (Toggle, "active", 4);
         Gtk.Image.Gtk_New
           (Icon, Gtk.Stock.Stock_Italic, Gtk.Enums.Icon_Size_Menu);
         Icon.all.Show_All;
         Column.all.Set_Widget (Icon);
         Panel_Handlers.Connect
           (Toggle,
            "toggled",
            On_Mode_Toggled'Access,
            Widget.all'Unchecked_Access);
            -- Column 5
         Gtk.Tree_View_Column.Gtk_New (Column);
         Widget.all.Values_Column := Widget.all.Append_Column (Column) - 1;
         Gtk.Cell_Renderer_Text.Gtk_New (Text);
         Column.all.Pack_Start (Text, True);
         Column.all.Add_Attribute (Text, "text", 5);
            -- Column 6
         Gtk.Tree_View_Column.Gtk_New (Column);
         Widget.all.Group_Column := Widget.all.Append_Column (Column) - 1;
         Gtk.Cell_Renderer_Text.Gtk_New (Text);
         Column.all.Pack_Start (Text, True);
         Tree_Functions.Set_Cell_Data_Func
           (Column,
            Text,
            On_Render_Group'Access,
            Widget.all'Access);
      end;
      Widget.all.Set_Events (Gdk.Event.Button_Press_Mask);
      Panel_Handlers.Connect
        (Widget,
         "style-updated",
         On_Style_Updated'Access,
         Widget.all'Unchecked_Access);
      Button_Handlers.Connect
        (Widget,
         "button_press_event",
         Button_Handlers.To_Marshaller (On_Button_Press'Access),
         Widget.all'Unchecked_Access);
      On_Style_Updated (Widget, Widget.all'Unchecked_Access);
   end Initialize;

   function On_Button_Press
     (Object : access GObject_Record'Class;
      Event  : Gdk.Event.Gdk_Event;
      Panel  : Gtk_Oscilloscope_Channels_Panel) return Boolean
   is
      pragma Unreferenced (Object);
      Menu      : Gtk.Menu.Gtk_Menu;
      Model     : Gtk.Tree_Model.Gtk_Tree_Model;
      Item      : Gtk.Image_Menu_Item.Gtk_Image_Menu_Item;
      Selection : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Selected  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Separator : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
      Icon      : Gtk.Image.Gtk_Image;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      case Gdk.Event.Get_Button (Event) is
         when 3 =>
            Selection := Panel.all.Get_Selection;
            if Selection.all.Count_Selected_Rows = 1 then
               Selection.all.Get_Selected (Model, Selected);
               if Selected /= Gtk.Tree_Model.Null_Iter then
                  Panel.all.Channel :=
                     Channel_Count (Gtk.Tree_Model.Get_Int (Model, Selected, 1));
                  Gtk.Menu.Gtk_New (Menu);
                     -- Move up
                  if Panel.all.Channel > 1 then
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item, Gtk.Widget.Styles.Style_Get (Panel, "menu-up"));
                     Gtk.Image.Gtk_New
                       (Icon, Gtk.Stock.Stock_Go_Up, Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Panel_Handlers.Connect
                       (Item,
                        "activate",
                        On_Menu_Up'Access,
                        Panel);
                  end if;
                     -- Move down
                  if
                    Panel.all.Channel <
                      Panel.all.Oscilloscope.all.Get_Channels_Number
                  then
                     Gtk.Image_Menu_Item.Gtk_New
                       (Item, Gtk.Widget.Styles.Style_Get (Panel, "menu-down"));
                     Gtk.Image.Gtk_New
                       (Icon,
                        Gtk.Stock.Stock_Go_Down,
                        Gtk.Enums.Icon_Size_Menu);
                     Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                     Gtk.Menu.Append (Menu, Item);
                     Panel_Handlers.Connect
                       (Item,
                        "activate",
                        On_Menu_Down'Access,
                        Panel);
                  end if;
                  Gtk.Image_Menu_Item.Gtk_New
                    (Item,
                     Gtk.Widget.Styles.Style_Get (Panel, "menu-select-color"));
                  Gtk.Image.Gtk_New
                    (Icon,
                     Gtk.Stock.Stock_Select_Color,
                     Gtk.Enums.Icon_Size_Menu);
                  Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                  Gtk.Menu.Append (Menu, Item);
                  Panel_Handlers.Connect
                    (Item,
                     "activate",
                     On_Menu_Select_Color'Access,
                     Panel);
                     -- Separator
                  Gtk.Separator_Menu_Item.Gtk_New (Separator);
                  Gtk.Menu.Append (Menu, Separator);
                     -- Delete selection
                  Gtk.Image_Menu_Item.Gtk_New
                    (Item, Gtk.Widget.Styles.Style_Get (Panel, "menu-delete"));
                  Gtk.Image.Gtk_New
                    (Icon, Gtk.Stock.Stock_Delete, Gtk.Enums.Icon_Size_Menu);
                  Gtk.Image_Menu_Item.Set_Image (Item, Icon);
                  Gtk.Menu.Append (Menu, Item);
                  Panel_Handlers.Connect
                    (Item,
                     "activate",
                     On_Menu_Delete'Access,
                     Panel);
                  Menu.all.Show_All;
                  Gtk.Menu.Popup
                    (Menu,
                     Button => Gdk.Event.Get_Button (Event),
                     Activate_Time => Gdk.Event.Get_Time (Event));
               end if;
            end if;
         when others =>
            null;
      end case;
      return False;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Button_Press"));
         return True;
   end On_Button_Press;

   procedure On_Menu_Delete
     (Widget : access GObject_Record'Class;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Widget);
   begin
      if Panel.all.Channel > 0 then
         Panel.all.Oscilloscope.all.Delete_Channel (Panel.all.Channel);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Menu_Delete"));
   end On_Menu_Delete;

   procedure On_Menu_Down
     (Widget : access GObject_Record'Class;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Widget);
   begin
      if
        Panel.all.Channel > 0 and then
        Panel.all.Channel < Panel.all.Oscilloscope.all.Get_Channels_Number
      then
         Panel.all.Oscilloscope.all.Move_Channel
           (Panel.all.Channel,
            Panel.all.Channel + 1);
         Panel.all.Get_Selection.all.Select_Iter
           (Panel.all.Oscilloscope.all.Channel_Names.all.Nth_Child
              (Gtk.Tree_Model.Null_Iter,
               Gint (Panel.all.Channel)));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Menu_Down"));
   end On_Menu_Down;

   procedure On_Menu_Select_Color
     (Widget : access GObject_Record'Class;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Widget);

      Dialog : Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog;
      Color  : Gdk.Color.Gdk_Color;
   begin
      if Panel.all.Channel > 0 then
         Gtk.Color_Selection_Dialog.Gtk_New (Dialog, "Change channel color");
         case Dialog.all.Run is
            when Gtk.Dialog.Gtk_Response_OK =>
               Dialog.all.Get_Color_Selection.all.Get_Current_Color (Color);
               Panel.all.Oscilloscope.all.Get_Waveform
                 (Panel.all.Channel).all.Set_Color (Color);
            when others =>
               null;
         end case;
         Dialog.all.Destroy;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Menu_Select_Color"));
   end On_Menu_Select_Color;

   procedure On_Menu_Up
     (Widget : access GObject_Record'Class;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Widget);
   begin
      if Panel.all.Channel > 1 then
         Panel.all.Oscilloscope.all.Move_Channel
           (Panel.all.Channel,
            Panel.all.Channel - 1);
         Panel.all.Get_Selection.all.Select_Iter
           (Panel.all.Oscilloscope.all.Channel_Names.all.Nth_Child
              (Gtk.Tree_Model.Null_Iter,
               Gint (Panel.all.Channel) - 2));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Menu_Up"));
   end On_Menu_Up;

   procedure On_Mode_Toggled
     (Widget : access GObject_Record'Class;
      Values : Glib.Values.GValues;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Widget);
      List : constant Gtk.List_Store.Gtk_List_Store :=
                Panel.all.Oscilloscope.all.Get_Channel_List;
      Row  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                Gtk.List_Store.Get_Iter_From_String
                 (List,
                  Glib.Values.Get_String (Glib.Values.Nth (Values, 1)));
      Channel : Channel_Number;
      Mode    : Gtk.Layered.Interpolation_Mode;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row /= Gtk.Tree_Model.Null_Iter then
         Channel := Channel_Number (List.all.Get_Int (Row, 1));
         case Panel.all.Oscilloscope.all.Get_Interpolation_Mode (Channel) is
            when Gtk.Layered.Left =>
               Mode := Gtk.Layered.Linear;
            when Gtk.Layered.Linear =>
               Mode := Gtk.Layered.Left;
         end case;
         Panel.all.Oscilloscope.all.Set_Interpolation_Mode (Channel, Mode);
      end if;
   end On_Mode_Toggled;

   procedure On_Render_Color
     (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Column);
      Channel : Channel_Number;
      Color   : Gdk.RGBA.Gdk_RGBA;
   begin
      Channel := Channel_Number (Gtk.Tree_Model.Get_Int (Model, Iter, 1));
      Color   :=
        Gtk.Missed.To_RGBA (Panel.all.Oscilloscope.all.Get_Color (Channel));
      Gdk.RGBA.Set_Property
        (Cell,
         Gtk.Cell_Renderer.Cell_Background_Rgba_Property,
         Color);
      Gdk.RGBA.Set_Property
        (Cell,
         Gtk.Cell_Renderer_Text.Foreground_Rgba_Property,
         Color);
      Glib.Properties.Set_Property
        (Cell,
         Cell_Renderer_Text.Text_Property,
         "  ");
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Render_Color"));
   end On_Render_Color;

   procedure On_Render_Group
     (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Column);
      Group : Group_Number;
   begin
      Group := Group_Number (Gtk.Tree_Model.Get_Int (Model, Iter, 2));
      Glib.Properties.Set_Property
        (Cell,
         Cell_Renderer_Text.Text_Property,
         Panel.all.Oscilloscope.all.Get_Name (Group));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Render_Group"));
   end On_Render_Group;

   procedure On_Style_Updated
     (Widget : access GObject_Record'Class;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Widget);
   begin
      Panel.all.Get_Column (Panel.all.Name_Column).all.Set_Title
        (Gtk.Widget.Styles.Style_Get (Panel, "channel-name-title"));
      Panel.all.Get_Column (Panel.all.Group_Column).all.Set_Title
        (Gtk.Widget.Styles.Style_Get (Panel, "group-name-title"));
      Panel.all.Get_Column (Panel.all.Values_Column).all.Set_Title
        (Gtk.Widget.Styles.Style_Get (Panel, "values-title"));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Style_Updated"));
   end On_Style_Updated;

   procedure On_Visible_Toggled
     (Widget : access GObject_Record'Class;
      Values : Glib.Values.GValues;
      Panel  : Gtk_Oscilloscope_Channels_Panel)
   is
      pragma Unreferenced (Widget);

      List : constant Gtk.List_Store.Gtk_List_Store :=
                Panel.all.Oscilloscope.all.Get_Channel_List;
      Row  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
               Gtk.List_Store.Get_Iter_From_String
                 (List,
                  Glib.Values.Get_String (Glib.Values.Nth (Values, 1)));
      Channel : Channel_Number;
      Visible : Boolean;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row /= Gtk.Tree_Model.Null_Iter then
         Channel := Channel_Number (List.all.Get_Int (Row, 1));
         Visible := not Panel.all.Oscilloscope.all.Is_Visible (Channel);
         Panel.all.Oscilloscope.all.Set_Visible (Channel, Visible);
      end if;
   end On_Visible_Toggled;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Oscilloscope.Channels_Panel." & Name;
   end Where;

   pragma Warnings (On, "declaration hides ""Dialog""");
   pragma Warnings (On, "declaration hides ""Group""");
   pragma Warnings (On, "declaration hides ""Menu""");
   pragma Warnings (On, "declaration hides ""Oscilloscope""");
   pragma Warnings (On, "declaration hides ""Values""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Oscilloscope.Channels_Panel;
