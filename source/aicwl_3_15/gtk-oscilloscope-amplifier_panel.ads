--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope.Amplifier_Panel            Luebeck            --
--  Interface                                      Summer, 2011       --
--                                                                    --
--                                Last revision :  22:07 23 Jul 2014  --
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

with Gtk.Box;           use Gtk.Box;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Combo_Box;     use Gtk.Combo_Box;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Label;         use Gtk.Label;
with Gtk.Table;         use Gtk.Table;

with Gtk.Generic_Style_Button;

package Gtk.Oscilloscope.Amplifier_Panel is
--
-- Class_Name - Of the widget
--
   Class_Name : constant String :=
                   Gtk.Oscilloscope.Class_Name & "AmplifierPanel";
--
-- Gtk_Oscilloscope_Amplifier_Panel -- Amplifier control panel
--
   type Gtk_Oscilloscope_Amplifier_Panel_Record is
      new Gtk_Table_Record with private;
   type Gtk_Oscilloscope_Amplifier_Panel is
      access all Gtk_Oscilloscope_Amplifier_Panel_Record'Class;
--
-- Get_Page_Label -- Get the page label field of the widget
--
--    Widget - The amplifier panel
--
-- Returns :
--
--    The label of range values, which can be used to place units
--
   function Get_Page_Label
            (  Widget : not null access
                        Gtk_Oscilloscope_Amplifier_Panel_Record
            )  return not null access Gtk_Label_Record'Class;
--
-- Get_Type -- The type of the widget
--
-- Returns :
--
--    The GTK type of the widget
--
   function Get_Type return GType;
--
-- Gtk_New -- Widget construction
--
--    Widget       - The result
--    Oscilloscope - The oscilloscope
--    Amplifier    - The amplifier to use the panel with
--
   procedure Gtk_New
             (  Widget       : out Gtk_Oscilloscope_Amplifier_Panel;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Amplifier    : Amplifier_Type := Left
             );
--
-- Initialize -- The widget initialization
--
--    Widget       - The widget to initialize
--    Oscilloscope - The oscilloscope
--    Amplifier    - The amplifier to use the panel with
--
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Oscilloscope_Amplifier_Panel_Record'Class;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Amplifier    : Amplifier_Type
             );
private
   package Autoscale_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Autoscale",
             Icon       => "gtk-fullscreen",
             Tip        => "Set automatic scaling"
          );
   package Manual_Scaling_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ManualScale",
             Icon       => "gtk-leave-fullscreen",
             Tip        => "Set manual scaling"
          );
   package Page_Amplify_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ZoomIn",
             Icon       => "gtk-remove",
             Tip        => "Amplify"
          );
   package Page_Down_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Down",
             Icon       => "gtk-go-down",
             Tip        => "Down"
          );
   package Page_Reduce_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ZoomOut",
             Icon       => "gtk-add",
             Tip        => "Reduce"
          );
   package Page_Up_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Up",
             Icon       => "gtk-go-up",
             Tip        => "Up"
          );
   type Gtk_Oscilloscope_Amplifier_Panel_Record is
      new Gtk_Table_Record with
   record
      Amplifier     : Amplifier_Type;
      Oscilloscope  : Gtk_Oscilloscope;
      Group_Label   : Gtk_Label;
      Group_List    : Gtk_Combo_Box;
      Axis_Button   : Gtk_Check_Button;
      Grid_Button   : Gtk_Check_Button;
      Manual_Button : Manual_Scaling_Buttons.Gtk_Style_Button;
      Auto_Button   : Autoscale_Buttons.Gtk_Style_Button;
      Page_Reduce   : Page_Reduce_Buttons.Gtk_Style_Button;
      Page_Edit     : Gtk_GEntry;
      Page_Amplify  : Page_Amplify_Buttons.Gtk_Style_Button;
      Page_Unit     : Gtk_Label;
      Page_Up       : Page_Up_Buttons.Gtk_Style_Button;
      Page_Down     : Page_Down_Buttons.Gtk_Style_Button;
      Button_Box    : Gtk_HBox;
      On_Edit       : Handler_Reference;
      On_Value      : Handler_Reference;
      On_Change     : Handler_Reference;
   end record;

   procedure On_Autoscaling_Changed
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Axis_Toggled
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Grid_Toggled
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Group_Changed
             (  Object : access GObject_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );

   procedure On_Changed_Amplifier
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Changed_Page
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Click_Amplify
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Click_Autoscale
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Click_Down
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Click_Manual
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Click_Reduce
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Click_Up
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Destroy
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Group_Set
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Style_Updated
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Toggled_Axis
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure On_Toggled_Grid
             (  Object : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Amplifier_Panel
             );
   procedure Set_Group
             (  Widget : not null access
                         Gtk_Oscilloscope_Amplifier_Panel_Record
             );
   procedure Set_Scaling
             (  Widget : not null access
                         Gtk_Oscilloscope_Amplifier_Panel_Record
             );

   package Panel_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Oscilloscope_Amplifier_Panel
          );
   use Panel_Handlers;

end Gtk.Oscilloscope.Amplifier_Panel;
