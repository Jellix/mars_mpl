--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope.Amplifier_Panel            Luebeck            --
--  Implementation                                 Summer, 2011       --
--                                                                    --
--                                Last revision :  21:30 08 Aug 2015  --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties;           use GLib.Properties;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Cell_Layout;           use Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Enums;                 use Gtk.Enums;
with GtkAda.Types;              use GtkAda.Types;
with Strings_Edit.Integers;     use Strings_Edit.Integers;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with Ada.Calendar;
with GLib.Object.Checked_Destroy;

package body Gtk.Oscilloscope.Amplifier_Panel is
   use type Page_Amplify_Buttons.Gtk_Style_Button;
   use type Page_Down_Buttons.Gtk_Style_Button;
   use type Page_Reduce_Buttons.Gtk_Style_Button;
   use type Page_Up_Buttons.Gtk_Style_Button;
   use Gtk.Layered.Waveform.Edit;

   Page_Step  : constant := 1.5;
   Value_Step : constant := 0.5;
   Edit_Field : constant := 12;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Oscilloscope.Amplifier_Panel." & Name;
   end Where;

   function Get_Page_Label
            (  Widget : not null access
                        Gtk_Oscilloscope_Amplifier_Panel_Record
            )  return not null access Gtk_Label_Record'Class is
   begin
      return Widget.Page_Unit;
   end Get_Page_Label;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name  => "column-spacings",
               Nick  => "Column spacings",
               Blurb => "Spacing between columns",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name  => "row-spacings",
               Nick  => "Row spacings",
               Blurb => "Spacing between rows",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "group-label",
               Default => "Group",
               Nick    => "Group label",
               Blurb   => "The label of the group name combo box"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "page-tip",
               Nick    => "Page tip",
               Blurb   => "Tooltip of the page size entry field",
               Default => "The values range corresponding to the page"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "show-axis",
               Default => "Show axis",
               Nick    => "Show axis",
               Blurb   => "Show axis check box text"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "show-graph-paper",
               Default => "Show graph paper",
               Nick    => "Show graph paper",
               Blurb   => "Show graph paper check box text"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget       : out Gtk_Oscilloscope_Amplifier_Panel;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Amplifier    : Amplifier_Type := Left
             )  is
   begin
      Widget := new Gtk_Oscilloscope_Amplifier_Panel_Record;
      Initialize (Widget, Oscilloscope, Amplifier);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Oscilloscope_Amplifier_Panel_Record'Class;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Amplifier    : Amplifier_Type
             )  is
      Box  : Gtk_HBox;
      Text : Gtk_Cell_Renderer_Text;
   begin
      Widget.Amplifier := Amplifier;
      G_New (Widget, Get_Type);
      Gtk_Table_Record (Widget.all).Initialize (4, 8, False);
      Widget.Set_Col_Spacings (3);
      Widget.Set_Row_Spacings (3);
      Widget.Oscilloscope := Oscilloscope.all'Unchecked_Access;
      Ref (Oscilloscope);
      -- Row 1 ---------------------------------------------------------
      Gtk_New_HBox (Box);
      Box.Set_Spacing (3);
      Gtk_New (Widget.Group_Label, "Group");
      Pack_Start (Box, Widget.Group_Label, False, False);
      Gtk_New_With_Model
      (  Widget.Group_List,
         To_Interface (Oscilloscope.Get_Group_List)
      );
      Pack_Start (Box, Widget.Group_List, True, True);
      Attach
      (  Widget,
         Box,
         0, 8,
         0, 1,
         YOptions => Shrink
      );
      Gtk_New (Text);
      Pack_Start (+Widget.Group_List, Text, True);
      Add_Attribute (+Widget.Group_List, Text, "text", 0);
      Connect
      (  Widget.Group_List,
         "changed",
         On_Group_Set'Access,
         Widget.all'Unchecked_Access
      );
      -- Row 2 ---------------------------------------------------------
      Gtk_New_HBox (Widget.Button_Box);
      Attach
      (  Widget,
         Widget.Button_Box,
         0, 2,
         1, 2,
         YOptions => Shrink
      );
      -- Row 3 ---------------------------------------------------------
      -- Row 4 ---------------------------------------------------------
      -- Row 5 ---------------------------------------------------------
      Set_Group (Widget);

      Connect
      (  Widget,
         "destroy",
         On_Destroy'Access,
         Widget.all'Unchecked_Access
      );
      Connect
      (  Widget,
         "style-updated",
         On_Style_Updated'Access,
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "autoscaling-changed",
         To_Marshaller (On_Autoscaling_Changed'Access),
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "group-changed",
         To_Marshaller (On_Group_Changed'Access),
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "y-axis-toggled",
         To_Marshaller (On_Axis_Toggled'Access),
         Widget.all'Unchecked_Access
      );
      Connect
      (  Oscilloscope,
         "y-grid-toggled",
         To_Marshaller (On_Grid_Toggled'Access),
         Widget.all'Unchecked_Access
      );
      On_Style_Updated (Widget, Widget.all'Unchecked_Access);
   end Initialize;

   procedure On_Autoscaling_Changed
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Set_Scaling;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Autoscaling_Changed")
         )  );
   end On_Autoscaling_Changed;

   procedure On_Axis_Toggled
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      if Amplifier_Type'Pos (Panel.Amplifier) = Axis then
         if Panel.Axis_Button /= null then
            Panel.Axis_Button.Set_Active
            (  Panel.Oscilloscope.Get_Values_Axis (Panel.Amplifier)
            );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Axis_Toggled")
         )  );
   end On_Axis_Toggled;

   procedure On_Changed_Amplifier
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      if Panel.Page_Edit /= null then
         Set (Panel.On_Edit);
         Panel.Page_Edit.Set_Text
         (  Image
            (  Panel.Oscilloscope.Get_Amplifier
               (  Channel_Number (Panel.Group_List.Get_Active + 1)
               ) .Get_Page_Size,
               RelSmall => 6
         )  );
         Set
         (  Panel.On_Edit,
            Connect
            (  Panel.Page_Edit,
               "changed",
               To_Marshaller (On_Changed_Page'Access),
               Panel
         )  );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Changed_Amplifier")
         )  );
   end On_Changed_Amplifier;

   procedure On_Changed_Page
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
      Group : Group_Number;
   begin
      Group := Group_Number (Panel.Group_List.Get_Active + 1);
      Panel.Oscilloscope.Set_Auto_Scaling (Group, False);
      Panel.Oscilloscope.Get_Amplifier
      (  Group
      ) .Set_Page_Size (Value (Panel.Page_Edit.Get_Text));
   exception
      when others =>
         null;
   end On_Changed_Page;

   procedure On_Click_Amplify
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Page_Edit.Set_Text
      (  Image
         (  (  Panel.Oscilloscope.Get_Amplifier
               (  Group_Number (Panel.Group_List.Get_Active + 1)
               ) .Get_Page_Size
            *  Page_Step
            ),
            RelSmall => 6
      )  );
   exception
      when others =>
         null;
   end On_Click_Amplify;

   procedure On_Click_Autoscale
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Auto_Scaling (Panel.Amplifier, True);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Autoscale")
         )  );
   end On_Click_Autoscale;

   procedure On_Click_Down
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
      Amplifier : access Gtk_Waveform_Amplifier_Record'Class;
      Page      : GDouble;
   begin
      Amplifier := Panel.Oscilloscope.Get_Amplifier (Panel.Amplifier);
      Page := Amplifier.Get_Page_Size;
      Amplifier.Set_Value
      (  GDouble'Min
         (  Amplifier.Get_Upper - Page,
            Amplifier.Get_Value + Page * Value_Step
      )  );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Down")
         )  );
   end On_Click_Down;

   procedure On_Click_Manual
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Auto_Scaling (Panel.Amplifier, False);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Manual")
         )  );
   end On_Click_Manual;

   procedure On_Click_Reduce
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Page_Edit.Set_Text
      (  Image
         (  (  Panel.Oscilloscope.Get_Amplifier
               (  Group_Number (Panel.Group_List.Get_Active + 1)
               ) .Get_Page_Size
            /  Page_Step
            ),
            RelSmall => 6
      )  );
   exception
      when others =>
         null;
   end On_Click_Reduce;

   procedure On_Click_Up
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
      Amplifier : access Gtk_Waveform_Amplifier_Record'Class;
   begin
      Amplifier := Panel.Oscilloscope.Get_Amplifier (Panel.Amplifier);
      Amplifier.Set_Value
      (  GDouble'Max
         (  Amplifier.Get_Lower,
            Amplifier.Get_Value - Amplifier.Get_Page_Size * Value_Step
      )  );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Click_Up")
         )  );
   end On_Click_Up;

   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      if Panel.Oscilloscope /= null then
         Panel.Oscilloscope.Unref;
         Panel.Oscilloscope := null;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Destroy")
         )  );
   end On_Destroy;

   procedure On_Grid_Toggled
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      if Amplifier_Type'Pos (Panel.Amplifier) = Axis then
         if Panel.Grid_Button /= null then
            Panel.Grid_Button.Set_Active
            (  Panel.Oscilloscope.Get_Values_Grid (Panel.Amplifier)
            );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Grid_Toggled")
         )  );
   end On_Grid_Toggled;

   procedure On_Group_Changed
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      if Amplifier_Type'Pos (Panel.Amplifier) = Axis then
         Panel.Set_Group;
      end if;
   end On_Group_Changed;

   procedure On_Group_Set
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Group
      (  Panel.Amplifier,
         Group_Number (Panel.Group_List.Get_Active + 1)
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Group_Set")
         )  );
   end On_Group_Set;

   procedure On_Style_Updated
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Group_Label.Set_Text
      (  Style_Get (Panel, "group-label")
      );
      if Panel.Axis_Button /= null then
         Panel.Axis_Button.Set_Label
         (  Style_Get (Panel, "show-axis")
         );
      end if;
      if Panel.Grid_Button /= null then
         Panel.Grid_Button.Set_Label
         (  Style_Get (Panel, "show-graph-paper")
         );
      end if;
      if Panel.Page_Edit /= null then
         Panel.Page_Edit.Set_Tooltip_Text
         (  Style_Get (Panel, "page-tip")
         );
      end if;
      Set_Col_Spacings (Panel, Style_Get (Panel, "column-spacings"));
      Set_Row_Spacings (Panel, Style_Get (Panel, "row-spacings"));
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

   procedure On_Toggled_Axis
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Values_Axis
      (  Panel.Amplifier,
         Panel.Axis_Button.Get_Active
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled_Axis")
         )  );
   end On_Toggled_Axis;

   procedure On_Toggled_Grid
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             )  is
   begin
      Panel.Oscilloscope.Set_Values_Grid
      (  Panel.Amplifier,
         Panel.Grid_Button.Get_Active
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Toggled_Grid")
         )  );
   end On_Toggled_Grid;

   procedure Set_Group
             (  Widget : not null access
                         Gtk_Oscilloscope_Amplifier_Panel_Record
             )  is
      Group : Group_Count;
   begin
      begin
         Group := Widget.Oscilloscope.Get_Group (Widget.Amplifier);
      exception
         when Constraint_Error =>
            Group := 0;
      end;
      if Group > 0 then
         if Widget.Page_Amplify = null then
            Page_Amplify_Buttons.Gtk_New (Widget.Page_Amplify);
            Widget.Page_Amplify.Show_All;
            Attach
            (  Widget,
               Widget.Page_Amplify,
               2, 3,
               1, 2,
               XOptions => Shrink,
               YOptions => Shrink
            );
            Connect
            (  Widget.Page_Amplify,
               "clicked",
               On_Click_Amplify'Access,
               Widget.all'Unchecked_Access
            );
         end if;
         if Widget.Page_Edit = null then
            Gtk_New (Widget.Page_Edit);
            Widget.Page_Edit.Show_All;
            Widget.Page_Edit.Set_Width_Chars (Edit_Field);
            if (  Find_Property (Widget.Page_Edit, "max-width-chars")
               /= null
               )
            then
               Set_Property
               (  Widget.Page_Edit,
                  Build ("max-width-chars"),
                  GInt'(Edit_Field)
               );
            end if;
            Attach
            (  Widget,
               Widget.Page_Edit,
               3, 4,
               1, 2,
               YOptions => Shrink
            );
            Widget.Page_Edit.Set_Tooltip_Text
            (  Style_Get (Widget, "page-tip")
            );
         end if;
         Set (Widget.On_Edit);
         Widget.Page_Edit.Set_Text
         (  Image
            (  Widget.Oscilloscope.Get_Amplifier
               (  Group
               ) .Get_Page_Size,
               RelSmall => 6
         )  );
         Set
         (  Widget.On_Edit,
            Connect
            (  Widget.Page_Edit,
               "changed",
               To_Marshaller (On_Changed_Page'Access),
               Widget.all'Unchecked_Access
         )  );
         if Widget.Page_Unit = null then
            Gtk_New (Widget.Page_Unit, "");
            Widget.Page_Unit.Show_All;
            Attach
            (  Widget,
               Widget.Page_Unit,
               4, 5,
               1, 2,
               XOptions => Shrink,
               YOptions => Shrink
            );
         end if;
         if Widget.Page_Reduce = null then
            Page_Reduce_Buttons.Gtk_New (Widget.Page_Reduce);
            Widget.Page_Reduce.Show_All;
            Attach
            (  Widget,
               Widget.Page_Reduce,
               5, 6,
               1, 2,
               XOptions => Shrink,
               YOptions => Shrink
            );
            Connect
            (  Widget.Page_Reduce,
               "clicked",
               On_Click_Reduce'Access,
               Widget.all'Unchecked_Access
            );
         end if;
         if Widget.Page_Down = null then
            Page_Down_Buttons.Gtk_New (Widget.Page_Down);
            Widget.Page_Down.Show_All;
            Attach
            (  Widget,
               Widget.Page_Down,
               6, 7,
               1, 2,
               XOptions => Shrink,
               YOptions => Shrink
            );
            Connect
            (  Widget.Page_Down,
               "clicked",
               On_Click_Down'Access,
               Widget.all'Unchecked_Access
            );
         end if;
         if Widget.Page_Up = null then
            Page_Up_Buttons.Gtk_New (Widget.Page_Up);
            Widget.Page_Up.Show_All;
            Attach
            (  Widget,
               Widget.Page_Up,
               7, 8,
               1, 2,
               XOptions => Shrink,
               YOptions => Shrink
            );
            Connect
            (  Widget.Page_Up,
               "clicked",
               On_Click_Up'Access,
               Widget.all'Unchecked_Access
            );
         end if;
         if Widget.Axis_Button = null then
               -- Axis
            Gtk_New (Widget.Axis_Button, "show axis");
            Attach
            (  Widget,
               Widget.Axis_Button,
               0, 8,
               2, 3,
               YOptions => Shrink
            );
            Connect
            (  Widget.Axis_Button,
               "toggled",
               On_Toggled_Axis'Access,
               Widget.all'Unchecked_Access
            );
               -- Grid
            Gtk_New (Widget.Grid_Button, "show graph paper");
            Attach
            (  Widget,
               Widget.Grid_Button,
               0, 8,
               3, 4,
               YOptions => Shrink
            );
            Connect
            (  Widget.Grid_Button,
               "toggled",
               On_Toggled_Grid'Access,
               Widget.all'Unchecked_Access
            );
            Widget.Axis_Button.Show_All;
            Widget.Grid_Button.Show_All;
            On_Style_Updated (Widget, Widget.all'Unchecked_Access);
         end if;
         Widget.Axis_Button.Set_Active
         (  Widget.Oscilloscope.Get_Values_Axis (Widget.Amplifier)
         );
         Widget.Grid_Button.Set_Active
         (  Widget.Oscilloscope.Get_Values_Grid (Widget.Amplifier)
         );
         Set
         (  Widget.On_Change,
            Connect
            (  Widget.Oscilloscope.Groups (Group).Amplifier,
               "changed",
               To_Marshaller (On_Changed_Amplifier'Access),
               Widget.all'Unchecked_Access
         )  );
      else
         if Widget.Axis_Button /= null then
            Widget.Remove (Widget.Axis_Button);
            Widget.Remove (Widget.Grid_Button);
            Widget.Axis_Button := null;
            Widget.Grid_Button := null;
         end if;
         Set (Widget.On_Change);
      end if;
      Widget.Set_Scaling;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Group")
         )  );
   end Set_Group;

   procedure Set_Scaling
             (  Widget : not null access
                         Gtk_Oscilloscope_Amplifier_Panel_Record
             )  is
      use Autoscale_Buttons;
      use Manual_Scaling_Buttons;
      Auto : Boolean;
      None : Boolean;
   begin
      begin
         None := False;
         Auto :=
            Widget.Oscilloscope.Get_Auto_Scaling (Widget.Amplifier);
      exception
         when Constraint_Error =>
            None := True;
      end;
      if Widget.Manual_Button /= null then
         Widget.Button_Box.Remove (Widget.Manual_Button);
         Widget.Manual_Button := null;
      end if;
      if Widget.Auto_Button /= null then
         Widget.Button_Box.Remove (Widget.Auto_Button);
         Widget.Auto_Button := null;
      end if;
      if None then
         if Widget.Page_Amplify /= null then
            Widget.Page_Amplify.Set_Sensitive (False);
         end if;
         if Widget.Page_Edit /= null then
            Widget.Page_Edit.Set_Sensitive (False);
         end if;
         if Widget.Page_Reduce /= null then
            Widget.Page_Reduce.Set_Sensitive (False);
         end if;
         if Widget.Page_Down /= null then
            Widget.Page_Down.Set_Sensitive (False);
         end if;
         if Widget.Page_Up /= null then
            Widget.Page_Up.Set_Sensitive (False);
         end if;
      else
         if Auto then
            Gtk_New (Widget.Manual_Button);
            Widget.Button_Box.Pack_Start
            (  Widget.Manual_Button,
               False,
               False
            );
            Widget.Manual_Button.Show_All;
            Connect
            (  Widget.Manual_Button,
               "clicked",
               On_Click_Manual'Access,
               Widget.all'Unchecked_Access
            );
         else
            Gtk_New (Widget.Auto_Button);
            Widget.Button_Box.Pack_Start (Widget.Auto_Button,
               False,
               False
            );
            Widget.Auto_Button.Show_All;
            Connect
            (  Widget.Auto_Button,
               "clicked",
               On_Click_Autoscale'Access,
               Widget.all'Unchecked_Access
            );
         end if;
         Widget.Page_Amplify.Set_Sensitive (not Auto);
         Widget.Page_Edit.Set_Sensitive    (not Auto);
         Widget.Page_Reduce.Set_Sensitive  (not Auto);
         Widget.Page_Down.Set_Sensitive    (not Auto);
         Widget.Page_Up.Set_Sensitive      (not Auto);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Scaling")
         )  );
   end Set_Scaling;

end Gtk.Oscilloscope.Amplifier_Panel;
