--                                                                    --
--  package Gtk.Layered_Editor      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2011       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with Cairo;                    use Cairo;
with Cairo.Ellipses;           use Cairo.Ellipses;
with GLib.Values;              use GLib.Values;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Color_Button;         use Gtk.Color_Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers.References;  use Gtk.Handlers.References;
with Gtk.Layered;              use Gtk.Layered;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Table;                use Gtk.Table;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Viewport;             use Gtk.Viewport;
with Gtk.Widget;               use Gtk.Widget;

with Ada.Strings;
with GLib.Generic_Properties;
with Gtk.Generic_Enum_Combo_Box;
with Gtk.Generic_Style_Button;
with Gtk.Handlers;
with GLib.Object.Strong_References;
with Strings_Edit.Float_Edit;
with Pango.Cairo.Fonts;
with Pango.Enums;

package Gtk.Layered_Editor is
--
-- Gtk_Layered_Editor_Record -- An editor of layered widgets
--
   type Gtk_Layered_Editor_Record is new Gtk_Widget_Record with private;
   type Gtk_Layered_Editor is
      access all Gtk_Layered_Editor_Record'Class;
--
-- Get -- The widget being edited
--
--    Widget - The editor
--
-- Returns :
--
--    The layered widget being edit or else null
--
   function Get
            (  Widget : not null access Gtk_Layered_Editor_Record
            )  return Gtk_Layered;
--
-- Get_Buttons_Box -- The widget's button box
--
--    Widget - The editor
--
-- Returns :
--
--    The widget's box containing buttons
--
   function Get_Buttons_Box
            (  Widget : not null access Gtk_Layered_Editor_Record
            )  return Gtk_Box;
--
-- Gtk_New -- Editor creation
--
--    Widget  - The editor, created
--    Layered - The widget to edit
--
   procedure Gtk_New
             (  Widget   : out Gtk_Layered_Editor;
                Layered  : access Gtk_Layered_Record'Class := null
             );
--
-- Initialize -- Construction
--
--    Widget  - The editor, created
--    Layered - The widget to edit
--
-- When overridden this procedure must be called from the override.
--
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Layered_Editor_Record'Class;
                Layered : access Gtk_Layered_Record'Class
             );
--
-- Put -- Set the widget to edit
--
--    Widget  - The editor
--    Layered - The layered widget to edit
--
-- When Layered is null the editor edits no widget.
--
   procedure Put
             (  Widget  : not null access Gtk_Layered_Editor_Record;
                Layered : access Gtk_Layered_Record'Class := null
             );
private
   function Get_Name (Layer : Abstract_Layer'Class) return UTF8_String;

   type Layer_Ptr is access all Abstract_Layer'Class;
   type Layer_List is array (Positive range <>) of Layer_Ptr;
   type Layer_List_Ptr is access Layer_List;

   type Property_Count is new Natural;
   subtype Property_Index is
      Property_Count range 1..Property_Count'Last;

   type Properties_List is
      array (Positive range <>, Property_Index range <>) of Natural;
   type Properties_List_Ptr is access Properties_List;

   package Layered_References is
      new GLib.Object.Strong_References (Gtk_Layered_Record);
   use Layered_References;

   package Add_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorAddButton",
             Icon       => "gtk-add",
             Tip        => "Add new"
          );
   package Delete_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorDeleteButton",
             Icon       => "gtk-delete",
             Tip        => "Delete"
          );

   type Delete_Button_Record is
      new Delete_Buttons.Gtk_Style_Button_Record with
   record
      Index : Positive;
   end record;
   type Delete_Button is access all Delete_Button_Record'Class;

   type Markup_Button_Record is new Gtk_Check_Button_Record with record
      Position : Positive;
      Widget   : Gtk_Layered_Editor;
   end record;
   type Markup_Button is access all Markup_Button_Record'Class;

   package Down_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorDownButton",
             Icon       => "gtk-go-down",
             Tip        => "Move down"
          );
   package Insert_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorInsertButton",
             Icon       => "gtk-ok",
             Tip        => "Insert layer"
          );
   package Left_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorLeftButton",
             Icon       => "gtk-go-back",
             Tip        => "Move left"
          );
   package Lift_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorLiftButton",
             Icon       => "gtk-go-up",
             Tip        => "Lift the selected layers up"
          );
   package Lower_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorLowerButton",
             Icon       => "gtk-go-down",
             Tip        => "Lower the selected layers down"
          );
   package Move_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorMoveButton",
             Label      => "Move"
          );
   package Remove_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorRemoveButton",
             Icon       => "gtk-delete",
             Tip        => "Delete layer"
          );
   package Right_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorRightButton",
             Icon       => "gtk-go-forward",
             Tip        => "Move right"
          );
   package Up_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorUpButton",
             Icon       => "gtk-go-up",
             Tip        => "Move up"
          );
   package Zoom_In_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorZoomInButton",
             Icon       => "gtk-zoom-in",
             Tip        => "Increase"
          );
   package Zoom_Out_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => "GtkLayerEditorZoomOutButton",
             Icon       => "gtk-zoom-out",
             Tip        => "Decrease"
          );

   package Editor_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Layered_Editor
          );

   package Double_Edit is new Strings_Edit.Float_Edit (GDouble);
   use Double_Edit;

   generic
      with package Enum_Property is
         new GLib.Generic_Properties.Generic_Enumeration_Property (<>);
      with package Boxes is
         new Gtk.Generic_Enum_Combo_Box (Enum_Property.Enumeration);
   package Generic_Enum is

      procedure Add
                (  Widget     : Gtk_Layered_Editor;
                   Row        : GUint;
                   Layers     : Layer_List;
                   Properties : Properties_List;
                   No         : Property_Index;
                   Tip        : String
                );
   private
      use Boxes;
      type Gtk_Enum_Record is new Gtk_Enum_Combo_Box_Record with record
         No     : Property_Index;
         Widget : Gtk_Layered_Editor;
      end record;
      type Gtk_Enum is access all Gtk_Enum_Record'Class;

      procedure Changed (Combo : access Gtk_Enum_Record'Class);
      package Enum_Handlers is
         new Gtk.Handlers.Callback (Gtk_Enum_Record);

   end Generic_Enum;

   package Alignment_Box is
      new Gtk.Generic_Enum_Combo_Box (Ada.Strings.Alignment);
   package Font_Slant_Box is
      new Gtk.Generic_Enum_Combo_Box (Cairo.Cairo_Font_Slant);
   package Font_Type_Box is
      new Gtk.Generic_Enum_Combo_Box (Pango.Cairo.Fonts.Font_Type);
   package Font_Weight_Box is
      new Gtk.Generic_Enum_Combo_Box (Pango.Enums.Weight);
   package Interpolation_Mode_Box is
      new Gtk.Generic_Enum_Combo_Box (Interpolation_Mode);
   package Line_Cap_Box is
      new Gtk.Generic_Enum_Combo_Box (Cairo.Cairo_Line_Cap);
   package Shadow_Type_Box is
      new Generic_Enum_Combo_Box (Gtk_Shadow_Type);
   package Text_Transformation_Box is
      new Gtk.Generic_Enum_Combo_Box (Text_Transformation);
   package Waveform_Drawing_Method_Box is
      new Gtk.Generic_Enum_Combo_Box (Waveform_Drawing_Method);

   type Gtk_Edit_Record is new Gtk_Entry_Record with record
      No     : Property_Index;
      Widget : Gtk_Layered_Editor;
   end record;
   type Gtk_Edit is access all Gtk_Edit_Record'Class;

   procedure Changed_String (Edit : access Gtk_Edit_Record'Class);

   type Gtk_Text_Record is new Gtk_Entry_Record with record
      Position : Positive;
      Widget   : Gtk_Layered_Editor;
   end record;
   type Gtk_Text is access all Gtk_Text_Record'Class;

   procedure Changed_Markup
             (  Markup : access Gtk_Toggle_Button_Record'Class
             );
   procedure Changed_Text (Edit : access Gtk_Text_Record'Class);

   type Annotation is record
      Label  : Gtk_Label;
      Edit   : Gtk_Text;
      Markup : Markup_Button;
      Delete : Delete_Button;
   end record;
   type Annotation_Array is array (GUInt range <>) of Annotation;
   type Annotation_Array_Ptr is access Annotation_Array;
--
-- Annotation_Texts_Record -- Annotation texts
--
   type Gtk_Annotation_Texts_Record is new Gtk_Table_Record with record
      Editor : Gtk_Layered_Editor;
      Length : GUInt := 0;
      Add    : Add_Buttons.Gtk_Style_Button;
      Rows   : Annotation_Array_Ptr;
   end record;
   type Gtk_Annotation_Texts is
      access all Gtk_Annotation_Texts_Record'Class;
   procedure Add
             (  Button : access Add_Buttons.
                                Gtk_Style_Button_Record'Class;
                Widget : Gtk_Annotation_Texts
             );
   procedure Add_Row
             (  Widget : not null access Gtk_Annotation_Texts_Record;
                Row    : GUInt
             );
   procedure Initialize
             (  Widget : not null access
                         Gtk_Annotation_Texts_Record'Class;
                Editor : Gtk_Layered_Editor;
                Length : GUInt
             );
   procedure Insert
             (  Widget : not null access Gtk_Annotation_Texts_Record;
                From   : GUInt
             );
   procedure Remove
             (  Widget : not null access Gtk_Annotation_Texts_Record;
                From   : GUInt
             );
   procedure Delete
             (  Button : access Delete_Button_Record'Class;
                Widget : Gtk_Annotation_Texts
             );
   procedure Destroyed
             (  Widget : access Gtk_Annotation_Texts_Record'Class
             );

   type Gtk_Check_Edit_Record is new Gtk_Check_Button_Record with record
      No     : Property_Index;
      Widget : Gtk_Layered_Editor;
   end record;
   type Gtk_Check_Edit is access all Gtk_Check_Edit_Record'Class;

   procedure Toggled
             (  Button : access Gtk_Check_Edit_Record'Class
             );

   type Gtk_Color_Edit_Record is new Gtk_Color_Button_Record with record
      No     : Property_Index;
      Widget : Gtk_Layered_Editor;
   end record;
   type Gtk_Color_Edit is access all Gtk_Color_Edit_Record'Class;

   procedure Color_Set
             (  Button : access Gtk_Color_Edit_Record'Class
             );

   type Gtk_Double_Edit_Record is new Gtk_Entry_Record with record
      No     : Property_Index;
      Widget : Gtk_Layered_Editor;
   end record;
   type Gtk_Double_Edit is access all Gtk_Double_Edit_Record'Class;

   procedure Value_Set
             (  Edit : access Gtk_Double_Edit_Record'Class
             );

   type Gtk_UInt_Edit_Record is new Gtk_Entry_Record with record
      No     : Property_Index;
      Widget : Gtk_Layered_Editor;
   end record;
   type Gtk_UInt_Edit is access all Gtk_UInt_Edit_Record'Class;

   procedure Value_Set
             (  Edit : access Gtk_UInt_Edit_Record'Class
             );

   package Annotation_Text_Handlers is
      new Gtk.Handlers.Callback (Gtk_Annotation_Texts_Record);

   package Add_Text_Handlers is
      new Gtk.Handlers.User_Callback
          (  Add_Buttons.Gtk_Style_Button_Record,
             Gtk_Annotation_Texts
          );
   package Check_Handlers is
      new Gtk.Handlers.Callback (Gtk_Check_Edit_Record);

   package Color_Handlers is
      new Gtk.Handlers.Callback (Gtk_Color_Edit_Record);

   package Delete_Text_Handlers is
      new Gtk.Handlers.User_Callback
          (  Delete_Button_Record,
             Gtk_Annotation_Texts
          );
   package Double_Handlers is
      new Gtk.Handlers.Callback (Gtk_Double_Edit_Record);

   package Edit_Handlers is
      new Gtk.Handlers.Callback (Gtk_Edit_Record);

   package Text_Handlers is
      new Gtk.Handlers.Callback (Gtk_Text_Record);

   package UInt_Handlers is
      new Gtk.Handlers.Callback (Gtk_UInt_Edit_Record);

   type Double_Edit_List is
      array (Property_Index range <>) of Gtk_Double_Edit;
   type Double_Edit_List_Ptr is access Double_Edit_List;

   type Layer_Type is
        (  Arc_Layer,
           Bar_Layer,
           Cache_Layer,
           Cap_Layer,
           Clock_Hand_Layer,
           Elliptic_Annotation_Layer,
           Elliptic_Background_Layer,
           Elliptic_Bar_Layer,
           Elliptic_Scale_Layer,
           Flat_Annotation_Layer,
           Flat_Needle_Layer,
           Flat_Scale_Layer,
           Graph_Paper_Layer,
           Graph_Paper_Annotation_Layer,
           Graph_Paper_Time_Annotation_Layer,
           Label_Layer,
           Line_Layer,
           Needle_Layer,
           Rectangle_Layer,
           Rectangular_Background_Layer,
           Rectangular_Clip_Region_On_Layer,
           Sector_Needle_Layer,
           Waveform_Layer
        );
   package Layer_Combo is new Gtk.Generic_Enum_Combo_Box (Layer_Type);

   type Gtk_Layered_Editor_Record is new Gtk_VBox_Record with record
      Layered       : Layered_References.Strong_Reference;
      Layer_Added   : Handler_Reference;
      Layer_Removed : Handler_Reference;
      View          : Gtk_Tree_View;
      Store         : Gtk_List_Store;
      Properties    : Gtk_Table;
      Viewport      : Gtk_Viewport;
      Panel_Box     : Gtk_VBox;
      Button_Box    : Gtk_HBox;
      Add_Layer_Combo     : Layer_Combo.Gtk_Enum_Combo_Box;
      Move_Step           : GDouble := 0.01;
      Gain_Step           : GDouble := 1.01;
      Aspect_Edit         : Gtk_Entry;
      Border_Color        : Gtk_Color_Edit;
      Move_Step_Button    : Gtk_Spin_Button;
      Gain_Step_Button    : Gtk_Spin_Button;
      Selected_Layers     : Layer_List_Ptr;
      Remove              : Remove_Buttons.Gtk_Style_Button;
      Lift                : Lift_Buttons.Gtk_Style_Button;
      Lower               : Lower_Buttons.Gtk_Style_Button;
      Insert              : Insert_Buttons.Gtk_Style_Button;
      Selected_Properties : Properties_List_Ptr;
      Double_Edits        : Double_Edit_List_Ptr;
   end record;

   procedure Add_Boolean
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String;
                Button     : out Gtk_Check_Edit
             );
   procedure Add_Color
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String;
                Button     : out Gtk_Color_Edit
             );
   procedure Add_Double
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String;
                Edit       : out Gtk_Double_Edit
             );
   procedure Add_String
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Hint       : String;
                Tip        : String
             );
   procedure Add_UInt
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String
             );
   procedure Border_Color_Type_Toggled
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Changed_Aspect
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Destroyed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Delete_Layers
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Down
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Fill_Layer_List
             (  Widget  : not null access Gtk_Layered_Editor_Record;
                Layered : not null access Gtk_Layered_Record'Class
             );
   procedure Gain_Step_Changed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Inset_Layer
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Layer_Added
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Widget : Gtk_Layered_Editor
             );
   procedure Layer_Removed
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Widget : Gtk_Layered_Editor
             );
   procedure Left
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Lift
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Lower
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Move
             (  Widget : not null access Gtk_Layered_Editor_Record;
                Offset : Cairo_Tuple
             );
   procedure Move_Step_Changed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Reselect
             (  Widget : not null access Gtk_Layered_Editor_Record;
                List   : Layer_List
             );
   procedure Right
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Scale
             (  Widget : not null access Gtk_Layered_Editor_Record;
                Gain   : GDouble
             );
   procedure Selected_Layer_Combo
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Set_Properties
             (  Widget : not null access Gtk_Layered_Editor_Record;
                Layers       : Layer_List;
                Properties   : Properties_List;
                Double_Edits : in out Double_Edit_List;
                Row          : in out GUInt
             );
   procedure Up
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Update_Double_Edits
             (  Widget : not null access Gtk_Layered_Editor_Record
             );
   procedure Zoom_In
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
   procedure Zoom_Out
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             );
end Gtk.Layered_Editor;
