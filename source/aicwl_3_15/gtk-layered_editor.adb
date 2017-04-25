--                                                                    --
--  package Gtk.Layered_Editor      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2011       --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with Ada.Numerics;              use Ada.Numerics;
with Ada.Tags;                  use Ada.Tags;
with Gdk.Color;                 use Gdk.Color;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties;           use GLib.Properties;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Gtk.Adjustment;            use Gtk.Adjustment;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Separator;             use Gtk.Separator;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Strings_Edit.Integers;     use Strings_Edit.Integers;

with Ada.Unchecked_Deallocation;
with Cairo.Font_Slant_Property;
with Cairo.Line_Cap_Property;
with Gtk.Enums.Shadow_Property;
with Gtk.Layered.Alignment_Property;
with Gtk.Layered.Arc;
with Gtk.Layered.Bar;
with Gtk.Layered.Cache;
with Gtk.Layered.Cap;
with Gtk.Layered.Clock_Hand;
with Gtk.Layered.Elliptic_Annotation;
with Gtk.Layered.Elliptic_Background;
with Gtk.Layered.Elliptic_Bar;
with Gtk.Layered.Elliptic_Scale;
with Gtk.Layered.Flat_Annotation;
with Gtk.Layered.Flat_Needle;
with Gtk.Layered.Flat_Scale;
with Gtk.Layered.Graph_Paper;
with Gtk.Layered.Graph_Paper_Annotation;
with Gtk.Layered.Interpolation_Mode_Property;
with Gtk.Layered.Label;
with Gtk.Layered.Line;
with Gtk.Layered.Needle;
with Gtk.Layered.Rectangle;
with Gtk.Layered.Rectangular_Background;
with Gtk.Layered.Rectangular_Clip_Region;
with Gtk.Layered.Sector_Needle;
with Gtk.Layered.Text_Transformation_Property;
with Gtk.Layered.Waveform_Drawing_Method_Property;
with Gtk.Layered.Waveform;
with GLib.Object.Checked_Destroy;
with Pango.Cairo.Fonts.Font_Type_Property;
with Pango.Enums.Weight_Property;

package body Gtk.Layered_Editor is
   use Cairo.Ellipses;
   use GLib.Values;
   use Gtk.Enums;
   use Insert_Buttons;
   use Layer_Combo;
   use Lift_Buttons;
   use Lower_Buttons;
   use Remove_Buttons;

   Default_Width : constant := 12;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered_Editor." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Annotation_Array,
             Annotation_Array_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Double_Edit_List,
             Double_Edit_List_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation (Layer_List, Layer_List_Ptr);
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Properties_List,
             Properties_List_Ptr
          );

   type Selection_List is record
      Index  : Natural := 0;
      Widget : Gtk_Layered_Editor;
      List   : Layer_List_Ptr;
   end record;
   type Selection_List_Ptr is access all Selection_List;

   package Selection_Browsing is
      new Selected_Foreach_User_Data (Selection_List_Ptr);

   package body Generic_Enum is
      procedure Add
                (  Widget     : Gtk_Layered_Editor;
                   Row        : GUint;
                   Layers     : Layer_List;
                   Properties : Properties_List;
                   No         : Property_Index;
                   Tip        : String
                )  is
         use Enum_Property;
         use Boxes;
         Box   : constant Gtk_Enum := new Gtk_Enum_Record;
         State : Enum_Property.Enumeration;
         Error : Boolean := False;
      begin
         Box.No     := No;
         Box.Widget := Widget;
         Initialize (Box, Capitalize_First, True);
         Box.Set_Tooltip_Text (Tip);
         Attach
         (  Widget.Properties,
            Box,
            1, 2,
            Row, Row + 1,
            YOptions => Shrink
         );
         for Index in Properties'Range (1) loop
            declare
               Value : GValue;
            begin
               Value :=
                  Layers (Index).Get_Property_Value
                  (  Properties (Index, No)
                  );
               if Index = Properties'First (1) then
                  State := Get_Enum (Value);
               elsif State /= Get_Enum (Value) then
                  Error := True;
               end if;
               Unset (Value);
            end;
         end loop;
         if not Error then
            Set_Active_Value (Box, State);
         end if;
         Enum_Handlers.Connect
         (  Box.all'Unchecked_Access,
            "changed",
            Changed'Access
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Generic_Enum.Add")
            )  );
      end Add;

      procedure Changed (Combo : access Gtk_Enum_Record'Class) is
         use Enum_Property;
         Value : GValue;
      begin
         Set_Enum (Value, Combo.Get_Active_Value);
         for Layer_No in Combo.Widget.Selected_Layers'Range loop
            Combo.Widget.Selected_Layers (Layer_No).Set_Property_Value
            (  Combo.Widget.Selected_Properties (Layer_No, Combo.No),
               Value
            );
         end loop;
         Unset (Value);
         Queue_Draw (Combo.Widget.Layered.Get);
      exception
         when No_Selection =>
            null;
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Generic_Enum.Changed")
            )  );
      end Changed;

   end Generic_Enum;

   package Alignment_Enum is
      new Generic_Enum (Gtk.Layered.Alignment_Property, Alignment_Box);
   package Shadow_Type_Enum is
      new Generic_Enum (Gtk.Enums.Shadow_Property, Shadow_Type_Box);
   package Font_Slant_Enum is
      new Generic_Enum (Cairo.Font_Slant_Property, Font_Slant_Box);
   package Font_Type_Enum is
      new Generic_Enum
          (  Pango.Cairo.Fonts.Font_Type_Property,
             Font_Type_Box
          );
   package Font_Weight_Enum is
      new Generic_Enum (Pango.Enums.Weight_Property, Font_Weight_Box);
   package Interpolation_Mode_Enum is
      new Generic_Enum
          (  Gtk.Layered.Interpolation_Mode_Property,
             Interpolation_Mode_Box
          );
   package Line_Cap_Enum is
      new Generic_Enum (Cairo.Line_Cap_Property, Line_Cap_Box);
   package Text_Transformation_Enum is
      new Generic_Enum
          (  Gtk.Layered.Text_Transformation_Property,
             Text_Transformation_Box
          );
   package Waveform_Drawing_Method_Enum is
      new Generic_Enum
          (  Gtk.Layered.Waveform_Drawing_Method_Property,
             Waveform_Drawing_Method_Box
          );

   procedure Add
             (  Button : access Add_Buttons.
                                Gtk_Style_Button_Record'Class;
                Widget : Gtk_Annotation_Texts
             ) is
   begin
      Remove (Widget, Widget.Add);
      if Widget.Rows'Length >= Widget.Length then
         declare
            New_Rows : constant Annotation_Array_Ptr :=
                          new Annotation_Array (1..Widget.Length + 10);
         begin
            New_Rows (Widget.Rows'Range) := Widget.Rows.all;
            Free (Widget.Rows);
            Widget.Rows := New_Rows;
         end;
      end if;
      Widget.Length := Widget.Length + 1;
      Resize (Widget, Widget.Length + 1, 3);
      Add_Row (Widget, Widget.Length);
      Attach
      (  Widget,
         Widget.Add,
         2, 3,
         Widget.Length, Widget.Length + 1,
         XOptions => Shrink,
         YOptions => Shrink
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add")
         )  );
   end Add;

   procedure Add_Boolean
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String;
                Button     : out Gtk_Check_Edit
             )  is
   begin
      Button := new Gtk_Check_Edit_Record;
      Gtk.Check_Button.Initialize (Button);
      Button.No := No;
      Button.Widget := Widget.all'Unchecked_Access;
      Button.Set_Tooltip_Text (Tip);
      Attach
      (  Widget.Properties,
         Button,
         1, 2,
         Row, Row + 1,
         YOptions => Shrink
      );
      for Index in Properties'Range (1) loop
         declare
            Value : GValue;
         begin
            Value :=
               Layers (Index).Get_Property_Value
               (  Properties (Index, No)
               );
            if Index = Properties'First (1) then
               Set_Active (Button, Get_Boolean (Value));
            elsif Get_Active (Button) /= Get_Boolean (Value) then
               null; -- Inconsistent value
            end if;
            Unset (Value);
         end;
      end loop;
      Check_Handlers.Connect (Button, "toggled", Toggled'Access);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Boolean")
         )  );
   end Add_Boolean;

   procedure Add_Color
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String;
                Button     : out Gtk_Color_Edit
             )  is
   begin
      Button := new Gtk_Color_Edit_Record;
      Gtk.Color_Button.Initialize (Button);
      Button.No := No;
      Button.Widget := Widget.all'Unchecked_Access;
      Button.Set_Tooltip_Text (Tip);
      Widget.Properties.Attach
      (  Button,
         1, 2,
         Row, Row + 1,
         YOptions => Shrink
      );
      for Index in Properties'Range (1) loop
         declare
            Value : GValue;
         begin
            Value :=
               Layers (Index).Get_Property_Value
               (  Properties (Index, No)
               );
            if Index = Properties'First (1) then
               Set_Color (Button, Get_Value (Value));
               Unset (Value);
            else
               declare
                  Color : Gdk_Color;
               begin
                  Button.Get_Color (Color);
                  if Color /= Get_Value (Value) then
                     Unset (Value);
                     return;
                  else
                     Unset (Value);
                  end if;
               end;
            end if;
         end;
      end loop;
      Color_Handlers.Connect (Button, "color_set", Color_Set'Access);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Color")
         )  );
   end Add_Color;

   procedure Add_Double
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String;
                Edit       : out Gtk_Double_Edit
             )  is
      Min, Max : GDouble;
   begin
      Edit := new Gtk_Double_Edit_Record;
      Gtk.GEntry.Initialize (Edit);
      Edit.Set_Width_Chars (Default_Width);
      if Find_Property (Edit, "max-width-chars") /= null then
         Set_Property
         (  Edit,
            Build ("max-width-chars"),
            GInt'(Default_Width)
         );
      end if;
      Edit.No := No;
      Edit.Widget := Widget.all'Unchecked_Access;
      Edit.Set_Tooltip_Text (Tip);
      Attach
      (  Widget.Properties,
         Edit,
         1, 2,
         Row, Row + 1,
         YOptions => Shrink
      );
      for Index in Properties'Range (1) loop
         declare
            Value : GValue;
         begin
            Value :=
               Layers (Index).Get_Property_Value
               (  Properties (Index, No)
               );
            if Index = Properties'First (1) then
               Min := Get_Double (Value);
               Max := Min;
            elsif Min > Get_Double (Value) then
               Min := Get_Double (Value);
            elsif Max < Get_Double (Value) then
               Max := Get_Double (Value);
            end if;
            Unset (Value);
         end;
      end loop;
      if Min = Max then
         Set_Text (Edit, Image (Min, RelSmall => 5));
      else
         Set_Text
         (  Edit,
            (  Image (Min, RelSmall => 5)
            &  " .. "
            &  Image (Max, RelSmall => 5)
         )  );
      end if;
      Double_Handlers.Connect (Edit, "changed", Value_Set'Access);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Double")
         )  );
   end Add_Double;

   procedure Add_Row
             (  Widget : not null access Gtk_Annotation_Texts_Record;
                Row    : GUInt
             )  is
      This : Annotation renames Widget.Rows (Row);
   begin
      Gtk_New (This.Label);
      This.Label.Set_Text (Image (Integer (Row)));
      This.Label.Set_Halign (Align_End);
      This.Label.Set_Valign (Align_Center);
      Widget.Attach
      (  This.Label,
         0, 1,
         Row - 1, Row,
         XOptions => Gtk.Enums.Fill,
         YOptions => Shrink
      );
      This.Label.Ref;

      This.Edit := new Gtk_Text_Record;
      Gtk.GEntry.Initialize (This.Edit);
      This.Edit.Set_Width_Chars (Default_Width);
      if Find_Property (This.Edit, "max-width-chars") /= null then
         Set_Property
         (  This.Edit,
            Build ("max-width-chars"),
            GInt'(Default_Width)
         );
      end if;
      This.Edit.Position := Positive (Row);
      This.Edit.Widget   := Widget.Editor;
      This.Edit.Set_Tooltip_Text ("Annotation text");
      Widget.Attach
      (  This.Edit,
         1, 2,
         Row - 1, Row,
         YOptions => Shrink
      );
      This.Edit.Ref;

      This.Markup := new Markup_Button_Record;
      Gtk.Check_Button.Initialize (This.Markup);
      This.Markup.Position := Positive (Row);
      This.Markup.Widget   := Widget.Editor;
      This.Markup.Set_Tooltip_Text
      (  "If the annotation text uses pango markup"
      );
      Widget.Attach
      (  This.Markup,
         2, 3,
         Row - 1, Row,
         XOptions => Shrink,
         YOptions => Shrink
      );
      This.Markup.Ref;

      This.Delete := new Delete_Button_Record;
      Delete_Buttons.Initialize (This.Delete);
      This.Delete.Index := Positive (Row);
      Widget.Attach
      (  This.Delete,
         3, 4,
         Row - 1, Row,
         XOptions => Shrink,
         YOptions => Shrink
      );
      This.Delete.Ref;

      Delete_Text_Handlers.Connect
      (  This.Delete,
         "clicked",
         Delete'Access,
         Widget.all'Unchecked_Access
      );
      This.Markup.On_Toggled (Changed_Markup'Access);
      Text_Handlers.Connect
      (  This.Edit,
         "changed",
         Changed_Text'Access
      );
      This.Label.Show_All;
      This.Markup.Show_All;
      This.Edit.Show_All;
      This.Delete.Show_All;
    end Add_Row;

   procedure Add_String
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Hint       : String;
                Tip        : String
             )  is
      procedure Check
                (  Edit : not null access Gtk_Entry_Record'Class;
                   Text : String
                )  is
      begin
         if Get_Text (Edit) /= Text then
            Set_Text (Edit, "");
         end if;
      end Check;
      Count : GUInt := 1;
      Frame : Gtk_Frame;
      Edit  : Gtk_Edit;
      List  : Gtk_Annotation_Texts;
      Text  : String_Ptr;
   begin
      for Index in Properties'Range (1) loop
         declare
            Value : GValue;
         begin
            Value :=
               Layers (Index).Get_Property_Value
               (  Properties (Index, No)
               );
            if Index = Properties'First (1) then
               Text := new String'(Get_String (Value));
               for Index in Text'Range loop
                  if Text (Index) = Character'Val (10) then
                     Count := Count + 1;
                  end if;
               end loop;
               if Hint = "Annotation texts" then
                  List := new Gtk_Annotation_Texts_Record;
                  Initialize (List, Widget.all'Unchecked_Access, Count);
                  Gtk_New (Frame);
                  Frame.Set_Shadow_Type (Shadow_Etched_In);
                  Set_Border_Width (Frame, 3);
                  Attach
                  (  Widget.Properties,
                     Frame,
                     1, 2,
                     Row, Row + 1,
                     YOptions => Shrink
                  );
                  Frame.Add (List);
                  declare
                     Row   : GUInt   := 1;
                     First : Integer := Text'First;
                  begin
                     for Index in Text'Range loop
                        if Text (Index) = Character'Val (10) then
                           Set_Text
                           (  List.Rows (Row).Edit,
                              Text (First..Index - 1)
                           );
                           First := Index + 1;
                           Row   := Row + 1;
                        end if;
                     end loop;
                     Set_Text
                     (  List.Rows (Row).Edit,
                        Text (First..Text'Last)
                     );
                  end;
               else
                  Edit := new Gtk_Edit_Record;
                  Edit.No := No;
                  Edit.Widget := Widget.all'Unchecked_Access;
                  Gtk.GEntry.Initialize (Edit);
                  Edit.Set_Width_Chars (Default_Width);
                  if Find_Property (Edit, "max-width-chars") /= null
                  then
                     Set_Property
                     (  Edit,
                        Build ("max-width-chars"),
                        GInt'(Default_Width)
                     );
                  end if;
                  Edit.Set_Tooltip_Text (Tip);
                  Attach
                  (  Widget.Properties,
                     Edit,
                     1, 2,
                     Row, Row + 1,
                     YOptions => Shrink
                  );
                  Set_Text (Edit, Text.all);
                  Edit_Handlers.Connect
                  (  Edit,
                     "changed",
                     Changed_String'Access
                  );
               end if;
            elsif Text.all /= Get_String (Value) then
               if Count > 1 then
                  declare
                     First : Integer := Text'First;
                     Row   : GUInt   := 1;
                  begin
                     for Index in Text'Range loop
                        if Text (Index) = Character'Val (10) then
                           Check
                           (  List.Rows (Row).Edit,
                              Text (First..Index - 1)
                           );
                           First := Index + 1;
                           Row   := Row + 1;
                        end if;
                     end loop;
                     Check
                     (  List.Rows (Row).Edit,
                        Text (First..Text'Last)
                     );
                  end;
               else
                  Set_Text (Edit, "");
               end if;
            end if;
            Unset (Value);
         end;
      end loop;
      Free (Text);
   exception
      when Error : others =>
         Free (Text);
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_String")
         )  );
   end Add_String;

   procedure Add_UInt
             (  Widget     : not null access Gtk_Layered_Editor_Record;
                Row        : GUint;
                Layers     : Layer_List;
                Properties : Properties_List;
                No         : Property_Index;
                Tip        : String
             )  is
      Edit     : constant Gtk_UInt_Edit := new Gtk_UInt_Edit_Record;
      Min, Max : GUInt;
   begin
      Gtk.GEntry.Initialize (Edit);
      Set_Width_Chars (Edit, Default_Width);
      if Find_Property (Edit, "max-width-chars") /= null then
         Set_Property
         (  Edit,
            Build ("max-width-chars"),
            GInt'(Default_Width)
         );
      end if;
      Edit.No := No;
      Edit.Widget := Widget.all'Unchecked_Access;
      Edit.Set_Tooltip_Text (Tip);
      Attach
      (  Widget.Properties,
         Edit,
         1, 2,
         Row, Row + 1,
         YOptions => Shrink
      );
      for Index in Properties'Range (1) loop
         declare
            Value : GValue;
         begin
            Value :=
               Layers (Index).Get_Property_Value
               (  Properties (Index, No)
               );
            if Index = Properties'First (1) then
               Max := Get_UInt (Value);
               Min := Max;
            elsif Min > Get_UInt (Value) then
               Min := Get_UInt (Value);
            elsif Max < Get_UInt (Value) then
               Max := Get_UInt (Value);
            end if;
            Unset (Value);
         end;
      end loop;
      if Min = Max then
         Set_Text (Edit, Image (Integer (Min)));
      else
         Set_Text
         (  Edit,
            Image (Integer (Min)) & " .. " & Image (Integer (Max))
         );
      end if;
      UInt_Handlers.Connect (Edit, "changed", Value_Set'Access);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_UInt")
         )  );
   end Add_UInt;

   procedure Border_Color_Type_Toggled
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
      Value : GValue;
   begin
      if Widget.Border_Color /= null then
         Value :=
            Widget.Selected_Layers (1).Get_Property_Value
            (  Widget.Selected_Properties (1, Widget.Border_Color.No)
            );
         Set_Color (Widget.Border_Color, Get_Value (Value));
         Unset (Value);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Border_Color_Type_Toggled")
         )  );
   end Border_Color_Type_Toggled;

   procedure Browse_Selected
             (  Model : Gtk_Tree_Model;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter;
                Data  : Selection_List_Ptr
             )  is
      Selection : Selection_List renames Data.all;
      Item      : constant GInt_Array  := Get_Indices (Path);
      Layered   : constant access Gtk_Layered_Record'Class :=
                                  Get (Data.all.Widget.Layered);
   begin
      Selection.Index := Selection.Index + 1;
      Selection.List (Selection.Index) :=
         Get_Layer
         (  Get (Data.all.Widget.Layered),
            Layered.Get_Depth - Natural (Item (Item'First))
         ) .all'Unchecked_Access;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Browse_Selected")
         )  );
   end Browse_Selected;

   procedure Changed_Aspect
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Layered.Get.Set_Aspect_Ratio
      (  Double_Edit.Value (Get_Text (Widget.Aspect_Edit))
      );
      Queue_Draw (Widget.Layered.Get);
   exception
      when End_Error | Data_Error | Constraint_Error =>
         null;
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_Aspect")
         )  );
   end Changed_Aspect;

   procedure Changed_Markup
             (  Markup : access Gtk_Toggle_Button_Record'Class
             )  is
      This : constant Markup_Button :=
                Markup_Button_Record'Class (Markup.all)'Unchecked_Access;
   begin
      for Layer_No in This.Widget.Selected_Layers'Range loop
         if This.Widget.Selected_Layers (Layer_No).all in
            Annotation_Layer'Class
         then
            declare
               Layer : Annotation_Layer'Class renames
                       Annotation_Layer'Class
                       (  This.Widget.Selected_Layers (Layer_No).all
                       );
            begin
               Layer.Set_Text
               (  This.Position,
                  Layer.Get_Text (This.Position),
                  This.Get_Active
               );
            end;
         end if;
      end loop;
      Queue_Draw (This.Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_Markup")
         )  );
   end Changed_Markup;

   procedure Changed_String (Edit : access Gtk_Edit_Record'Class) is
      Text  : constant String := Get_Text (Edit);
      Value : GValue;
   begin
      Init (Value, GType_String);
      Set_String (Value, Text);
      for Layer_No in Edit.Widget.Selected_Layers'Range loop
         Edit.Widget.Selected_Layers (Layer_No).Set_Property_Value
         (  Edit.Widget.Selected_Properties (Layer_No, Edit.No),
            Value
         );
      end loop;
      Unset (Value);
      Queue_Draw (Edit.Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_String")
         )  );
   end Changed_String;

   procedure Changed_Text (Edit : access Gtk_Text_Record'Class) is
      Text : constant String := Get_Text (Edit);
   begin
      for Layer_No in Edit.Widget.Selected_Layers'Range loop
         if Edit.Widget.Selected_Layers (Layer_No).all in
            Annotation_Layer'Class
         then
            declare
               Layer : Annotation_Layer'Class renames
                       Annotation_Layer'Class
                       (  Edit.Widget.Selected_Layers (Layer_No).all
                       );
            begin
               Layer.Set_Text
               (  Edit.Position,
                  Text,
                  Layer.Get_Markup (Edit.Position)
               );
            end;
         end if;
      end loop;
      Queue_Draw (Edit.Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Changed_Text")
         )  );
   end Changed_Text;

   procedure Color_Set
             (  Button : access Gtk_Color_Edit_Record'Class
             )  is
      Value : GValue;
      Color : Gdk_Color;
   begin
      Button.Get_Color (Color);
      Set_Value (Value, Color);
      for Layer_No in Button.Widget.Selected_Layers'Range loop
         Button.Widget.Selected_Layers (Layer_No).Set_Property_Value
         (  Button.Widget.Selected_Properties (Layer_No, Button.No),
            Value
         );
      end loop;
      Unset (Value);
      Queue_Draw (Button.Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Color_Set")
         )  );
   end Color_Set;

   procedure Delete
             (  Button : access Delete_Button_Record'Class;
                Widget : Gtk_Annotation_Texts
             )  is
       function Get_Text (Row : GUInt) return String is
       begin
          if Row = Widget.Length then
             return "";
          else
             return Widget.Rows (Row + 1).Edit.Get_Text;
          end if;
      end Get_Text;
      Deleted : constant GUInt := GUInt (Button.Index);
      Editor  : Gtk_Layered_Editor_Record'Class renames
                Widget.Editor.all;
   begin
      for Index in Deleted..Widget.Length loop
         declare
            Text : constant String := Get_Text (Index);
         begin
            for Layer_No in Editor.Selected_Layers'Range loop
               if Editor.Selected_Layers (Layer_No).all in
                  Annotation_Layer'Class
               then
                  Annotation_Layer'Class
                  (  Editor.Selected_Layers (Layer_No).all
                  ) .Set_Text (Integer (Index), Text);
               end if;
            end loop;
         end;
      end loop;
      Widget.Remove (Deleted);
      Widget.Rows (Deleted).Label.Unref;
      Widget.Rows (Deleted).Edit.Unref;
      Widget.Rows (Deleted).Markup.Unref;
      Widget.Rows (Deleted).Delete.Unref;
      Widget.Rows (Deleted..Widget.Length - 1) :=
         Widget.Rows (Deleted + 1..Widget.Length);
      Resize (Widget, Widget.Length, 3);
      Widget.Length := Widget.Length - 1;
      Widget.Insert (Deleted);
      Queue_Draw (Editor.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete")
         )  );
   end Delete;

   procedure Delete_Layers
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Abstract_Layer'Class,
                Layer_Ptr
             );
   begin
      if Widget.Selected_Layers /= null then
         -- The selection is not empty
         for Index in Widget.Selected_Layers'Range loop
            -- Searching for a layer selected
            declare
               This : Layer_Ptr := Widget.Selected_Layers (Index);
            begin
               if This /= null then
                  Free (This);
                  exit;
               end if;
            end;
         end loop;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete_Layers")
         )  );
   end Delete_Layers;

   procedure Destroyed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Free (Widget.Double_Edits);
      Free (Widget.Selected_Layers);
      Free (Widget.Selected_Properties);
      Unref (Widget.Store);
   end Destroyed;

   procedure Destroyed
             (  Widget : access Gtk_Annotation_Texts_Record'Class
             )  is
   begin
      for Index in 1..Widget.Length loop
         Widget.Rows (Index).Label.Unref;
         Widget.Rows (Index).Markup.Unref;
         Widget.Rows (Index).Edit.Unref;
         Widget.Rows (Index).Delete.Unref;
      end loop;
      Widget.Add.Unref;
      Free (Widget.Rows);
   end Destroyed;

   procedure Down
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Move ((0.0, Widget.Move_Step));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Down")
         )  );
   end Down;

   procedure Fill_Layer_List
             (  Widget  : not null access Gtk_Layered_Editor_Record;
                Layered : not null access Gtk_Layered_Record'Class
             )  is
      Layer : access Abstract_Layer'Class := Layered.Get_Upper;
      Row   : Gtk_Tree_Iter;
      Depth : GInt := 1;
   begin
      Clear (Widget.Store);
      while Layer /= null loop
         Append (Widget.Store, Row);
         Gtk.Missed.Set (Widget.Store, Row, 0, Get_Name (Layer.all));
         Layer := Layer.Below;
         Depth := Depth + 1;
      end loop;
   end Fill_Layer_List;

   procedure Gain_Step_Changed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Gain_Step := Get_Value (Widget.Gain_Step_Button);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Gain_Step_Changed")
         )  );
   end Gain_Step_Changed;

   function Get
            (  Widget : not null access Gtk_Layered_Editor_Record
            )  return Gtk_Layered is
      Layered : constant access Gtk_Layered_Record'Class :=
                                Widget.Layered.Get;
   begin
      if Layered = null then
         return null;
      else
         return Layered.all'Unchecked_Access;
      end if;
   end Get;

   function Get_Buttons_Box
            (  Widget : not null access Gtk_Layered_Editor_Record
            )  return Gtk_Box is
   begin
      return Widget.Button_Box;
   end Get_Buttons_Box;

   function Get_Common_Properties (List : Layer_List)
      return Properties_List is
      Count  : Property_Count :=
                  Property_Count (List (1).Get_Properties_Number);
      Common : Properties_List (List'Range, 1..Count);
   begin
      for Index in Common'Range (2) loop
         Common (1, Index) := Positive (Index);
         declare
            Pattern : constant Param_Spec :=
               List (1).Get_Property_Specification (Common (1, Index));
         begin
            for Layer in 2..Common'Last (1) loop
               Common (Layer, Index) :=
                  Find_Property (List (Layer).all, Pattern);
               if 0 = Common (Layer, Index) then
                  Common (1, Index) := 0;
                  Count := Count - 1;
                  exit;
               end if;
            end loop;
            Unref (Pattern);
         end;
      end loop;
      if Count = 0 then
         return (List'Range => (1..0 => 0));
      else
         return Result : Properties_List (List'Range, 1..Count) do
            for Index in reverse Common'Range (2) loop
               if Common (1, Index) > 0 then
                  for Layer in Common'Range (1) loop
                     Result (Layer, Count) := Common (Layer, Index);
                  end loop;
                  exit when Count = 1;
                  Count := Count - 1;
               end if;
            end loop;
         end return;
      end if;
   end Get_Common_Properties;

   function Get_Name (Layer : Abstract_Layer'Class)
      return UTF8_String is
      Name : String := Expanded_Name (Layer'Tag);
   begin
      for Index in reverse Name'Range loop
         if Name (Index) = '.' then
            if Index < Name'Last then
               Name (Index + 1) := To_Upper (Name (Index + 1));
            end if;
            return Name (Index + 1..Name'Last);
         elsif Name (Index) = '_' then
            Name (Index) := ' ';
--            if Index < Name'Last then
--               Name (Index + 1) := To_Upper (Name (Index + 1));
--            end if;
         else
            Name (Index) := To_Lower (Name (Index));
         end if;
      end loop;
      return Name;
   end Get_Name;

   procedure Gtk_New
             (  Widget  : out Gtk_Layered_Editor;
                Layered : access Gtk_Layered_Record'Class := null
             )  is
   begin
      Widget := new Gtk_Layered_Editor_Record;
      Initialize (Widget, Layered);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Annotation_Texts_Record'Class;
                Editor : Gtk_Layered_Editor;
                Length : GUInt
             )  is
      use Delete_Buttons;
   begin
      Initialize (Widget, Length + 1, 4, False);
      Widget.Editor := Editor;
      Widget.Set_Border_Width (3);
      Widget.Set_Col_Spacings (3);
      Widget.Set_Row_Spacings (3);
      Widget.Length := Length;
      Widget.Rows   := new Annotation_Array (1..Length);
      for Row in Widget.Rows'Range loop
         Add_Row (Widget, Row);
      end loop;
      Add_Buttons.Gtk_New (Widget.Add);
      Widget.Attach
      (  Widget.Add,
         3, 4,
         Length, Length + 1,
         XOptions => Shrink,
         YOptions => Shrink
      );
      Widget.Add.Ref;
      Annotation_Text_Handlers.Connect
      (  Widget,
         "destroy",
         Destroyed'Access
      );
      Add_Text_Handlers.Connect
      (  Widget.Add,
         "clicked",
         Add'Access,
         Widget.all'Unchecked_Access
      );
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Layered_Editor_Record'Class;
                Layered : access Gtk_Layered_Record'Class
             )  is
      Frame    : Gtk_Frame;
      Scrolled : Gtk_Scrolled_Window;
      Paned    : Gtk_Paned;
   begin
      Initialize_VBox (Widget);
      Gtk_New_HBox (Widget.Button_Box);
      Widget.Button_Box.Set_Spacing (3);
      Widget.Pack_Start (Widget.Button_Box, False, False);

      Gtk_New_HPaned (Paned);
      Widget.Pack_Start (Paned);

      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_In);
      Paned.Add1 (Frame);
      Gtk_New (Scrolled);
      Frame.Add (Scrolled);
      Gtk_New (Widget.View);
      Widget.View.Set_Rules_Hint (True);
      Get_Selection (Widget.View).Set_Mode (Selection_Multiple);
      Scrolled.Add (Widget.View);
   --
   -- Adding columns
   --
      Set_Rules_Hint (Widget.View, True);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Text);
         Pack_Start (Column, Text, True);
         Add_Attribute (Column, Text, "text", 0);
         Column_No := Append_Column (Widget.View, Column);
         Set_Title (Column, "Layer type");
         Set_Resizable (Column, True);
         Set_Sort_Column_Id (Column, 0);
      end;
      Editor_Handlers.Connect
      (  Get_Selection (Widget.View),
         "changed",
         Selection_Changed'Access,
         Widget.all'Access
      );
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Paned.Pack2 (Scrolled, False, True);
      Gtk_New (Widget.Viewport);
      Widget.Viewport.Set_Shadow_Type (Shadow_None);
      Scrolled.Add (Widget.Viewport);
      Editor_Handlers.Connect
      (  Widget,
         "destroy",
         Destroyed'Access,
         Widget.all'Access
      );
   --
   -- Adding buttons
   --
      declare
         Label : Gtk_Label;
      begin
            -- Aspect edit
         Gtk_New (Widget.Aspect_Edit);
         Widget.Aspect_Edit.Set_Width_Chars (6);
         if (  Find_Property (Widget.Aspect_Edit, "max-width-chars")
            /= null
            )
         then
            Set_Property
            (  Widget.Aspect_Edit,
               Build ("max-width-chars"),
               GInt'(6)
            );
         end if;
         Widget.Button_Box.Pack_End (Widget.Aspect_Edit, False, False);
         Widget.Aspect_Edit.Set_Sensitive (False);
         Editor_Handlers.Connect
         (  Widget.Aspect_Edit,
            "changed",
            Changed_Aspect'Access,
            Widget.all'Access
         );
         Gtk_New (Label, "Aspect ratio");
         Widget.Button_Box.Pack_End (Label, False, False);
            -- Lift button
         Gtk_New (Widget.Lift);
         Widget.Lift.Set_Sensitive (False);
         Widget.Button_Box.Pack_Start (Widget.Lift, False, False);
         Editor_Handlers.Connect
         (  Widget.Lift,
            "clicked",
            Lift'Access,
            Widget.all'Access
         );
            -- Lower button
         Gtk_New (Widget.Lower);
         Widget.Lower.Set_Sensitive (False);
         Widget.Button_Box.Pack_Start (Widget.Lower, False, False);
         Editor_Handlers.Connect
         (  Widget.Lower,
            "clicked",
            Lower'Access,
            Widget.all'Access
         );
            -- Remove button
         Gtk_New (Widget.Remove);
         Widget.Remove.Set_Sensitive (False);
         Widget.Button_Box.Pack_Start (Widget.Remove, False, False);
         Editor_Handlers.Connect
         (  Widget.Remove,
            "clicked",
            Delete_Layers'Access,
            Widget.all'Access
         );
            -- Layer combo
         Gtk_New (Widget.Add_Layer_Combo, Capitalize_First, True);
         Widget.Button_Box.Pack_Start
         (  Widget.Add_Layer_Combo,
            False,
            False
         );
         Editor_Handlers.Connect
         (  Widget.Add_Layer_Combo,
            "changed",
            Selected_Layer_Combo'Access,
            Widget.all'Access
         );
            -- Insert button
         Gtk_New (Widget.Insert);
         Widget.Insert.Set_Sensitive (False);
         Widget.Button_Box.Pack_Start (Widget.Insert, False, False);
         Editor_Handlers.Connect
         (  Widget.Insert,
            "clicked",
            Inset_Layer'Access,
            Widget.all'Access
         );
      end;
      Gtk_New (Widget.Store, (0 => GType_String));
      Put (Widget, Layered);
   end Initialize;

   procedure Insert
             (  Widget : not null access Gtk_Annotation_Texts_Record;
                From   : GUInt
             )  is
   begin
      for Index in From..Widget.Length loop
         Widget.Attach
         (  Widget.Rows (Index).Label,
            0, 1,
            Index - 1, Index,
            XOptions => Gtk.Enums.Fill,
            YOptions => Shrink
         );
         Set_Text (Widget.Rows (Index).Label, Image (Integer (Index)));
         Widget.Attach
         (  Widget.Rows (Index).Edit,
            1, 2,
            Index - 1, Index,
            YOptions => Shrink
         );
         Widget.Rows (Index).Edit.Position := Integer (Index);
         Widget.Attach
         (  Widget.Rows (Index).Markup,
            2, 3,
            Index - 1, Index,
            XOptions => Shrink,
            YOptions => Shrink
         );
         Widget.Rows (Index).Markup.Position := Positive (Index);
         Widget.Attach
         (  Widget.Rows (Index).Delete,
            3, 4,
            Index - 1, Index,
            XOptions => Shrink,
            YOptions => Shrink
         );
         Widget.Rows (Index).Delete.Index := Positive (Index);
      end loop;
      Widget.Attach
      (  Widget.Add,
         3, 4,
         Widget.Length, Widget.Length + 1,
         XOptions => Shrink,
         YOptions => Shrink
      );
   end Insert;

   Default_Angle   : constant := Pi / 6.0;
   Default_Radius  : constant := 50.0;
   Default_Texts   : constant String := "1 2 3";
   Default_Color   : constant Gdk_Color := RGB (1.0, 0.0, 1.0);
   Default_Center  : constant Cairo_Tuple :=
                              (  X => Default_Radius,
                                 Y => Default_Radius
                              );
   Default_Box     : constant Cairo_Box :=
                              (  X1 => 0.0,
                                 X2 => Default_Radius * 2.0,
                                 Y1 => 0.0,
                                 Y2 => Default_Radius * 2.0
                              );
   Default_Ellipse : constant Ellipse_Parameters :=
                              (  Center          => Default_Center,
                                 Major_Curvature => 1.0/ Default_Radius,
                                 Minor_Radius    => Default_Radius,
                                 Angle           => 0.0
                              );

   procedure Inset_Layer
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
      Under      : access Layer_Location'Class := Widget.Layered.Get;
      Adjustment : Gtk_Adjustment;
      Position   : Positive;
   begin
      if Widget.Selected_Layers /= null then
         for Index in Widget.Selected_Layers'Range loop
            if Widget.Selected_Layers (Index) /= null then
               Under := Widget.Selected_Layers (Index).Above;
               exit when Under /= null;
               Under := Widget.Layered.Get;
               exit;
            end if;
         end loop;
      end if;
      case Get_Active_Value (Widget.Add_Layer_Combo) is
         when Arc_Layer =>
            Position :=
               Gtk.Layered.Arc.Add_Arc
               (  Under   => Under,
                  Ellipse => Default_Ellipse,
                  Color   => Default_Color
               ) .Get_Position;
         when Bar_Layer =>
            Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
               Gtk.Layered.Bar.Add_Bar
               (  Under      => Under,
                  From       => (Default_Radius, 2.0 * Default_Radius),
                  To         => (Default_Radius, 0.0),
                  Color      => Default_Color,
                  Adjustment => Adjustment
               ) .Get_Position;
         when Cache_Layer =>
            Position :=
               Gtk.Layered.Cache.Add_Cache (Under).Get_Position;
         when Cap_Layer =>
            Position :=
               Gtk.Layered.Cap.Add_Cap (Under).Get_Position;
         when Clock_Hand_Layer =>
            Position :=
               Gtk.Layered.Clock_Hand.Add_Clock_Hand
               (  Under  => Under,
                  Color  => Default_Color,
                  Center => Default_Center
               ) .Get_Position;
         when Elliptic_Annotation_Layer =>
            Position :=
               Gtk.Layered.Elliptic_Annotation.
                  Add_Elliptic_Annotation
                  (  Under   => Under,
                     Texts   => Default_Texts,
                     Step    => Default_Angle,
                     Color   => Default_Color,
                     Ellipse => Default_Ellipse
                  ) .Get_Position;
         when Elliptic_Background_Layer =>
            Position :=
               Gtk.Layered.Elliptic_Background.
                  Add_Elliptic_Background
                  (  Under        => Under,
                     Outer        => Default_Ellipse,
                     Color        => Default_Color,
                     Border_Width => 6.0,
                     Border_Depth => 3.0
                  ) .Get_Position;
         when Elliptic_Bar_Layer =>
            Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
               Gtk.Layered.Elliptic_Bar.Add_Elliptic_Bar
               (  Under      => Under,
                  Ellipse    => Default_Ellipse,
                  Color      => Default_Color,
                  Adjustment => Adjustment
               ) .Get_Position;
         when Elliptic_Scale_Layer =>
            Position :=
               Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
               (  Under => Under,
                  Outer => Default_Ellipse,
                  Inner => Default_Ellipse * 0.8,
                  Color => Default_Color,
                  Step  => Default_Angle
               ) .Get_Position;
         when Flat_Annotation_Layer =>
            Position :=
               Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
               (  Under  => Under,
                  From   => (Default_Radius, 2.0 * Default_Radius),
                  Length => 2.0 * Default_Radius,
                  Step   => Default_Radius / 5.0,
                  Color  => Default_Color,
                  Texts  => Default_Texts
               ) .Get_Position;
         when Flat_Needle_Layer =>
            Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
               Gtk.Layered.Flat_Needle.Add_Flat_Needle
               (  Under      => Under,
                  From       => (Default_Radius, 2.0 * Default_Radius),
                  To         => (Default_Radius, 0.0),
                  Color      => Default_Color,
                  Adjustment => Adjustment
               ) .Get_Position;
         when Flat_Scale_Layer =>
            Position :=
               Gtk.Layered.Flat_Scale.Add_Flat_Scale
               (  Under   => Under,
                  From    => (Default_Radius, 2.0 * Default_Radius),
                  Length  => 2.0 * Default_Radius,
                  Breadth => 10.0,
                  Color   => Default_Color,
                  Step    => Default_Radius / 5.0
               ) .Get_Position;
         when Graph_Paper_Layer =>
            Position :=
               Gtk.Layered.Graph_Paper.Add_Graph_Paper
               (  Under => Under,
                  Box   => Default_Box
               ) .Get_Position;
         when Graph_Paper_Annotation_Layer =>
            declare
               Layer : constant access
                          Gtk.Layered.Graph_Paper_Annotation.
                              Graph_Paper_Annotation_Layer :=
                       Gtk.Layered.Graph_Paper_Annotation.
                          Add_Graph_Paper_Annotation
                          (  Under   => Under,
                             Paper   => Gtk.Layered.Graph_Paper.
                                           Add_Graph_Paper
                                           (  Under => Under,
                                              Box   => Default_Box
                          )                );
            begin
               Position := Layer.Get_Position;
            end;
         when Graph_Paper_Time_Annotation_Layer =>
            Position :=
               Gtk.Layered.Graph_Paper_Annotation.
                  Add_Graph_Paper_Time_Annotation
                  (  Under   => Under,
                     Paper   => Gtk.Layered.Graph_Paper.
                                   Add_Graph_Paper
                                   (  Under => Under,
                                      Box   => Default_Box
                                   )
                  ) .Get_Position;
         when Label_Layer =>
            Position :=
               Gtk.Layered.Label.Add_Label
               (  Under    => Under,
                  Location => Default_Center,
                  Color    => Default_Color,
                  Text     => "Label"
               ) .Get_Position;
         when Line_Layer =>
            Position :=
               Gtk.Layered.Line.
                  Add_Line
                  (  Under   => Under,
                     Length  => Default_Radius,
                     Color   => Default_Color
                  ) .Get_Position;
         when Needle_Layer =>
            Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
               Gtk.Layered.Needle.Add_Needle
               (  Under      => Under,
                  Center     => Default_Center,
                  Color      => Default_Color,
                  Adjustment => Adjustment
               ) .Get_Position;
         when Rectangular_Background_Layer =>
            Position :=
               Gtk.Layered.Rectangular_Background.
                  Add_Rectangular_Background
                  (  Under        => Under,
                     Center       => Default_Center,
                     Height       => Default_Radius,
                     Width        => Default_Radius,
                     Color        => Default_Color,
                     Border_Width => 6.0,
                     Border_Depth => 3.0
                  ) .Get_Position;
         when Rectangle_Layer =>
            Position :=
               Gtk.Layered.Rectangle.
                  Add_Rectangle
                  (  Under => Under,
                     Box   => Default_Box,
                     Color => Default_Color
                  ) .Get_Position;
         when Waveform_Layer =>
            Position :=
               Gtk.Layered.Waveform.
                  Add_Waveform
                  (  Under => Under,
                     Box   => Default_Box,
                     Color => Default_Color
                  ) .Get_Position;
         when Rectangular_Clip_Region_On_Layer =>
            Position :=
               Gtk.Layered.Rectangular_Clip_Region.
                  Add_Rectangular_Clip_Region (Under).Get_Position;
         when Sector_Needle_Layer =>
            Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
               Gtk.Layered.Sector_Needle.Add_Sector_Needle
               (  Under      => Under,
                  Outer      => Default_Ellipse,
                  Center     => Default_Center,
                  Color      => Default_Color,
                  Adjustment => Adjustment
               ) .Get_Position;
      end case;
   exception
      when No_Selection =>
         null;
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Inset_Layer")
         )  );
   end Inset_Layer;

   procedure Layer_Added
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Widget : Gtk_Layered_Editor
             )  is
      Position : constant Natural :=
                 Natural (Get_UInt (Nth (Params, 1)));
      Row      : Gtk_Tree_Iter;
   begin
      Insert
      (  Widget.Store,
         Row,
         GInt (Widget.Layered.Get.Get_Depth - Position)
      );
      Gtk.Missed.Set
      (  Widget.Store,
         Row,
         0,
         Get_Name (Widget.Layered.Get.Get_Layer (Position).all)
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Layer_Added")
         )  );
   end Layer_Added;

   procedure Layer_Removed
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      declare
         Position : constant GUInt := Get_UInt (Nth (Params, 1));
         Row      : Gtk_Tree_Iter :=
                       Nth_Child
                       (  Widget.Store.all'Unchecked_Access,
                          Null_Iter,
                          (  N_Children
                             (  Widget.Store.all'Unchecked_Access,
                                Null_Iter
                             )
                          -  GInt (Position)
                       )  );
      begin
         if Row /= Null_Iter then
            Remove (Widget.Store, Row);
         end if;
      end;
      if Widget.Selected_Layers /= null then
         declare -- Cleaning up the list of selected layers
            List : Layer_List renames Widget.Selected_Layers.all;
         begin
            for Index in List'Range loop
               if List (Index) /= null then
                  declare
                     Found : Boolean := False;
                     Layer : access Abstract_Layer'Class :=
                                    Widget.Layered.Get.Get_Lower;
                  begin
                     while Layer /= null loop
                        Found := List (Index) = Layer;
                        exit when Found;
                        Layer := Layer.Above;
                     end loop;
                     if not Found then
                        List (Index) := null;
                     end if;
                  end;
               end if;
            end loop;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Layer_Removed")
         )  );
   end Layer_Removed;

   procedure Left
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Move ((-1.0 * Widget.Move_Step, 0.0));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Left")
         )  );
   end Left;

   procedure Lift
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      if Widget.Selected_Layers = null then
         return;
      end if;
      declare -- Moving the selected layers up
         List : constant Layer_List := Widget.Selected_Layers.all;
      begin
         for Index in List'Range loop
            if List (Index) /= null then
               Insert
               (  Widget.Layered.Get,
                  List (Index).all,
                  List (Index).Get_Position + 1
               );
            end if;
         end loop;
         Reselect (Widget, List);
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Lift")
         )  );
   end Lift;

   procedure Lower
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      if Widget.Selected_Layers = null then
         return;
      end if;
      declare -- Moving the selected layers down
         List : constant Layer_List := Widget.Selected_Layers.all;
      begin
         for Index in reverse List'Range loop
            if List (Index) /= null then
               Insert
               (  Widget.Layered.Get,
                  List (Index).all,
                  Integer'Max (List (Index).Get_Position - 1, 1)
               );
            end if;
         end loop;
         Reselect (Widget, List);
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Lower")
         )  );
   end Lower;

   procedure Move
             (  Widget : not null access Gtk_Layered_Editor_Record;
                Offset : Cairo_Tuple
             )  is
   begin
      for Layer_No in Widget.Selected_Layers'Range loop
         Widget.Selected_Layers (Layer_No).Move (Offset);
      end loop;
      Update_Double_Edits (Widget);
      Queue_Draw (Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Move")
         )  );
   end Move;

   procedure Move_Step_Changed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Move_Step := Get_Value (Widget.Move_Step_Button);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Move_Step_Changed")
         )  );
   end Move_Step_Changed;

   procedure Put
             (  Widget  : not null access Gtk_Layered_Editor_Record;
                Layered : access Gtk_Layered_Record'Class := null
             )  is
   begin
      Widget.Layered.Set (Layered);
      Set_Model (Widget.View, Null_Gtk_Tree_Model);
      if Layered /= null then
         Fill_Layer_List (Widget, Layered);
         Set_Model (Widget.View, To_Interface (Widget.Store));
         declare
            Size : Gtk_Requisition;
         begin
            Columns_Autosize (Widget.View);   -- Size columns
            Size_Request (Widget.View, Size); -- Query the integral size
            Set_Size_Request                  -- Set new size
            (  Widget.View,
               GInt'Min (Size.Width,  300),
               GInt'Min (Size.Height, 100)
            );
         end;
         Set_Sensitive (Widget.Aspect_Edit, True);
         Set_Text
         (  Widget.Aspect_Edit,
            Image (Layered.Get_Aspect_Ratio, AbsSmall => -3)
         );
         Set
         (  Widget.Layer_Added,
            Editor_Handlers.Connect
            (  Layered,
               "layer-added",
               Layer_Added'Access,
               Widget.all'Access
         )  );
         Set
         (  Widget.Layer_Removed,
            Editor_Handlers.Connect
            (  Layered,
               "layer-removed",
               Layer_Removed'Access,
               Widget.all'Access
         )  );
         Set_Sensitive
         (  Widget.Insert,
            (  Widget.Layered.Get /= null
            and then
               Get_Active (Widget.Add_Layer_Combo) >= 0
         )  );
      end if;
   end Put;

   procedure Remove
             (  Widget : not null access Gtk_Annotation_Texts_Record;
                From   : GUInt
             )  is
   begin
      for Index in From..Widget.Length loop
         Remove (Widget, Widget.Rows (Index).Label);
         Remove (Widget, Widget.Rows (Index).Edit);
         Remove (Widget, Widget.Rows (Index).Markup);
         Remove (Widget, Widget.Rows (Index).Delete);
      end loop;
      Remove (Widget, Widget.Add);
   end Remove;

   procedure Reselect
             (  Widget : not null access Gtk_Layered_Editor_Record;
                List   : Layer_List
             )  is
      Selection : constant Gtk_Tree_Selection :=
                  Widget.View.Get_Selection;
      Depth     : constant GInt :=
                  GInt (Widget.Layered.Get.Get_Depth);
      New_List  : array (List'Range) of Natural;
   begin
      for Index in List'Range loop
         if List (Index) = null then
            New_List (Index) := 0;
         else
            New_List (Index) := List (Index).Get_Position;
         end if;
      end loop;
      Unselect_All (Selection);
      for Index in New_List'Range loop
         if New_List (Index) /= 0 then
            Select_Iter
            (  Selection,
               Nth_Child
               (  Widget.Store,
                  Null_Iter,
                  Depth - GInt (New_List (Index))
            )  );
         end if;
      end loop;
   end Reselect;

   procedure Right
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Move ((Widget.Move_Step, 0.0));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Right")
         )  );
   end Right;

   procedure Scale
             (  Widget : not null access Gtk_Layered_Editor_Record;
                Gain   : GDouble
             )  is
   begin
      for Layer_No in Widget.Selected_Layers'Range loop
         begin
            Widget.Selected_Layers (Layer_No).Scale (Gain);
         exception
            when Constraint_Error =>
               null;
         end;
      end loop;
      Update_Double_Edits (Widget);
      Queue_Draw (Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Scale")
         )  );
   end Scale;

   procedure Selected_Layer_Combo
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Set_Sensitive
      (  Widget.Insert,
         (  Widget.Layered.Get /= null
         and then
            Get_Active (Widget.Add_Layer_Combo) >= 0
      )  );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selected_Layer_Combo")
         )  );
   end Selected_Layer_Combo;

   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
      Count  : aliased Natural := 0;
      Row    : GUInt           := 0;
      Layers : aliased Selection_List;
   begin
      if Widget.Properties /= null then
         Remove (Widget.Viewport, Widget.Properties);
         Widget.Properties := null;
      end if;
      Count :=
         Natural (Count_Selected_Rows (Get_Selection (Widget.View)));
      Free (Widget.Selected_Layers);
      Free (Widget.Selected_Properties);
      if Count = 0 then
         Set_Sensitive (Widget.Lift, False);
         Set_Sensitive (Widget.Lower, False);
         Set_Sensitive (Widget.Remove, False);
         return;
      end if;
      Set_Sensitive (Widget.Lift, True);
      Set_Sensitive (Widget.Lower, True);
      Set_Sensitive (Widget.Remove, True);
      Layers.List   := new Layer_List (1..Count);
      Layers.Widget := Widget;
      Selection_Browsing.Selected_Foreach
      (  Get_Selection (Widget.View),
         Browse_Selected'Access,
         Layers'Unchecked_Access
      );
      Widget.Selected_Layers := Layers.List;
      Widget.Selected_Properties :=
         new Properties_List'(Get_Common_Properties (Layers.List.all));
      Widget.Double_Edits :=
         new Double_Edit_List (Widget.Selected_Properties'Range (2));
      Gtk_New
      (  Widget.Properties,
         Widget.Selected_Properties'Length (2) + 1,
         2,
         False
      );
      Add (Widget.Viewport, Widget.Properties);
      Set_Col_Spacings (Widget.Properties, 3);
      Set_Row_Spacings (Widget.Properties, 3);
      Set_Properties
      (  Widget,
         Layers.List.all,
         Widget.Selected_Properties.all,
         Widget.Double_Edits.all,
         Row
      );
      declare
         Bar             : Gtk_Separator;
         Left_Button     : Left_Buttons.Gtk_Style_Button;
         Right_Button    : Right_Buttons.Gtk_Style_Button;
         Up_Button       : Up_Buttons.Gtk_Style_Button;
         Down_Button     : Down_Buttons.Gtk_Style_Button;
         Box             : Gtk_HBox;
         Zoom_In_Button  : Zoom_In_Buttons.Gtk_Style_Button;
         Zoom_Out_Button : Zoom_Out_Buttons.Gtk_Style_Button;
      begin
         Gtk_New_HBox (Box);
         Set_Spacing (Box, 3);
         Attach
         (  Widget.Properties,
            Box,
            0, 3, Row, Row + 1,
            YOptions => Shrink
         );
         Left_Buttons.Gtk_New (Left_Button);
         Pack_Start (Box, Left_Button, False, False);
         Right_Buttons.Gtk_New (Right_Button);
         Pack_Start (Box, Right_Button, False, False);
         Gtk_New (Widget.Move_Step_Button, 0.0, 100.0, 0.01);
         Set_Value (Widget.Move_Step_Button, Widget.Move_Step);
         Pack_Start (Box, Widget.Move_Step_Button, False, False);
         Up_Buttons.Gtk_New (Up_Button);
         Pack_Start (Box, Up_Button, False, False);
         Down_Buttons.Gtk_New (Down_Button);
         Pack_Start (Box, Down_Button, False, False);
         Gtk_New_Vseparator (Bar);
         Pack_Start (Box, Bar, False, False);

         Zoom_In_Buttons.Gtk_New (Zoom_In_Button);
         Pack_End (Box, Zoom_In_Button, False, False);
         Zoom_Out_Buttons.Gtk_New (Zoom_Out_Button);
         Pack_End (Box, Zoom_Out_Button, False, False);
         Gtk_New (Widget.Gain_Step_Button, 1.0, 4.0, 0.01);
         Set_Value (Widget.Gain_Step_Button, Widget.Gain_Step);
         Pack_End (Box, Widget.Gain_Step_Button, False, False);
         Gtk_New_Vseparator (Bar);
         Pack_End (Box, Bar, False, False);
         Editor_Handlers.Connect
         (  Down_Button,
            "clicked",
            Down'Access,
            Widget.all'Access
         );
         Editor_Handlers.Connect
         (  Widget.Gain_Step_Button,
            "value_changed",
            Gain_Step_Changed'Access,
            Widget.all'Access
         );
         Editor_Handlers.Connect
         (  Left_Button,
            "clicked",
            Left'Access,
            Widget.all'Access
         );
         Editor_Handlers.Connect
         (  Widget.Move_Step_Button,
            "value_changed",
            Move_Step_Changed'Access,
            Widget.all'Access
         );
         Editor_Handlers.Connect
         (  Right_Button,
            "clicked",
            Right'Access,
            Widget.all'Access
         );
         Editor_Handlers.Connect
         (  Up_Button,
            "clicked",
            Up'Access,
            Widget.all'Access
         );
         Editor_Handlers.Connect
         (  Zoom_In_Button,
            "clicked",
            Zoom_In'Access,
            Widget.all'Access
         );
         Editor_Handlers.Connect
         (  Zoom_Out_Button,
            "clicked",
            Zoom_Out'Access,
            Widget.all'Access
         );
      end;
      Show_All (Widget.Properties);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selection_Changed")
         )  );
   end Selection_Changed;

   procedure Set_Properties
             (  Widget : not null access Gtk_Layered_Editor_Record;
                Layers       : Layer_List;
                Properties   : Properties_List;
                Double_Edits : in out Double_Edit_List;
                Row          : in out GUInt
             )  is
      Label             : Gtk_Label;
      Edit              : Gtk_Entry;
      Border_Check_Edit : Gtk_Check_Edit;
      Border_Color_Edit : Gtk_Color_Edit;
   begin
      for Property in Properties'Range (2) loop
         declare
            Specification : constant Param_Spec :=
               Layers (1).Get_Property_Specification
               (  Properties (1, Property)
               );
            Name : String := Nick_Name (Specification);
         begin
            if Name'Length > 1 then
               Name (1) := To_Upper (Name (1));
            end if;
            if Name /= "Annotation text markups" then
               Gtk_New (Label, Name);
               Label.Set_Halign (Align_End);
               Label.Set_Valign (Align_Center);
               Label.Set_Tooltip_Text (Description (Specification));
               Attach
               (  Widget.Properties,
                  Label,
                  0, 1,
                  Row, Row + 1,
                  XOptions => Gtk.Enums.Fill,
                  YOptions => Shrink
               );
               case Value_Type (Specification) is
                  when GType_Boolean =>
                     declare
                        Check_Edit : Gtk_Check_Edit;
                     begin
                        Add_Boolean
                        (  Widget,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification),
                           Check_Edit
                        );
                        if Name = "Border color type" then
                           Border_Check_Edit := Check_Edit;
                        end if;
                     end;
                  when GType_Double =>
                     Add_Double
                     (  Widget,
                        Row,
                        Layers,
                        Properties,
                        Property,
                        Description (Specification),
                        Double_Edits (Property)
                     );
                  when GType_String =>
                     Add_String
                     (  Widget,
                        Row,
                        Layers,
                        Properties,
                        Property,
                        Name,
                        Description (Specification)
                     );
                  when GType_UInt =>
                     Add_UInt
                     (  Widget,
                        Row,
                        Layers,
                        Properties,
                        Property,
                        Description (Specification)
                     );
                  when others =>
                     if Value_Type (Specification) = Gdk_Color_Type then
                        declare
                           Color_Edit : Gtk_Color_Edit;
                        begin
                           Add_Color
                           (  Widget,
                              Row,
                              Layers,
                              Properties,
                              Property,
                              Description (Specification),
                              Color_Edit
                           );
                           if Name = "Border color" then
                              Border_Color_Edit := Color_Edit;
                           end if;
                        end;
                     elsif Value_Type (Specification) =
                           Gtk.Layered.Alignment_Property.Get_Type then
                        Alignment_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) =
                           Gtk.Layered.Interpolation_Mode_Property.
                           Get_Type then
                        Interpolation_Mode_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) =
                           Gtk.Layered.Waveform_Drawing_Method_Property.
                           Get_Type then
                        Waveform_Drawing_Method_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) =
                           Gtk.Layered.Text_Transformation_Property.
                           Get_Type then
                        Text_Transformation_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) =
                           Cairo.Font_Slant_Property.Get_Type then
                        Font_Slant_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) =
                           Pango.Enums.Weight_Property.Get_Type then
                        Font_Weight_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) = Pango.Cairo.
                           Fonts.Font_Type_Property.Get_Type then
                        Font_Type_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) =
                           Cairo.Line_Cap_Property.Get_Type then
                        Line_Cap_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     elsif Value_Type (Specification) =
                           Gtk.Enums.Shadow_Property.Get_Type then
                        Shadow_Type_Enum.Add
                        (  Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Description (Specification)
                        );
                     else
                        Gtk_New (Edit);
                        Edit.Set_Width_Chars (Default_Width);
                        if (  Find_Property (Edit, "max-width-chars")
                           /= null
                           )
                        then
                           Set_Property
                           (  Edit,
                              Build ("max-width-chars"),
                              GInt'(Default_Width)
                           );
                        end if;
                        Edit.Set_Tooltip_Text
                        (  Description (Specification)
                        );
                        Attach
                        (  Widget.Properties,
                           Edit,
                           1, 2,
                           Row, Row + 1,
                           YOptions => Shrink
                        );
                     end if;
               end case;
               Row := Row + 1;
               Unref (Specification);
            end if;
         end;
      end loop;
      if Border_Check_Edit /= null and then Border_Color_Edit /= null
      then
         Widget.Border_Color := Border_Color_Edit;
         Editor_Handlers.Connect
         (  Border_Check_Edit,
            "toggled",
            Border_Color_Type_Toggled'Access,
            Widget.all'Access
         );
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Properties")
         )  );
   end Set_Properties;

   procedure Toggled
             (  Button : access Gtk_Check_Edit_Record'Class
             )  is
      Value : GValue;
   begin
      Init (Value, GType_Boolean);
      Set_Boolean (Value, Get_Active (Button));
      for Layer_No in Button.Widget.Selected_Layers'Range loop
         Button.Widget.Selected_Layers (Layer_No).Set_Property_Value
         (  Button.Widget.Selected_Properties (Layer_No, Button.No),
            Value
         );
      end loop;
      Unset (Value);
      Queue_Draw (Button.Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Toggled")
         )  );
   end Toggled;

   procedure Up
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Move ((0.0, -1.0 * Widget.Move_Step));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Up")
         )  );
   end Up;

   procedure Update_Double_Edits
             (  Widget : not null access Gtk_Layered_Editor_Record
             )  is
      Value : GValue;
   begin
      for Property in Widget.Double_Edits'Range loop
         if Widget.Double_Edits (Property) /= null then
            Value :=
               Widget.Selected_Layers (1).Get_Property_Value
               (  Widget.Selected_Properties (1, Property)
               );
            Set_Text
            (  Widget.Double_Edits (Property),
               Image (Get_Double (Value), RelSmall => 5)
            );
            Unset (Value);
         end if;
      end loop;
   end Update_Double_Edits;

   procedure Value_Set
             (  Edit : access Gtk_Double_Edit_Record'Class
             )  is
      Input : GDouble;
   begin
      begin
         Input := Value (Get_Text (Edit));
      exception
         when Constraint_Error | Ada.IO_Exceptions.End_Error |
              Ada.IO_Exceptions.Data_Error =>
            return;
      end;
      declare
         Value : GValue;
      begin
         Init (Value, GType_Double);
         Set_Double (Value, Input);
         for Layer_No in Edit.Widget.Selected_Layers'Range loop
            Edit.Widget.Selected_Layers (Layer_No).Set_Property_Value
            (  Edit.Widget.Selected_Properties (Layer_No, Edit.No),
               Value
            );
         end loop;
         Unset (Value);
      end;
      Queue_Draw (Edit.Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Value_Set")
         )  );
   end Value_Set;

   procedure Value_Set
             (  Edit : access Gtk_UInt_Edit_Record'Class
             )  is
      Input : GUInt;
   begin
      begin
         Input := GUInt (Strings_Edit.Integers.Value (Get_Text (Edit)));
      exception
         when Constraint_Error | Ada.IO_Exceptions.End_Error |
              Ada.IO_Exceptions.Data_Error =>
            return;
      end;
      declare
         Value : GValue;
      begin
         Init (Value, GType_UInt);
         Set_UInt (Value, Input);
         for Layer_No in Edit.Widget.Selected_Layers'Range loop
            Edit.Widget.Selected_Layers (Layer_No).Set_Property_Value
            (  Edit.Widget.Selected_Properties (Layer_No, Edit.No),
               Value
            );
         end loop;
         Unset (Value);
      end;
      Queue_Draw (Edit.Widget.Layered.Get);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Value_Set")
         )  );
   end Value_Set;

   procedure Zoom_In
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Scale (Widget.Gain_Step);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoom_In")
         )  );
   end Zoom_In;

   procedure Zoom_Out
             (  Object : access GObject_Record'Class;
                Widget : Gtk_Layered_Editor
             )  is
   begin
      Widget.Scale (1.0 / Widget.Gain_Step);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoom_Out")
         )  );
   end Zoom_Out;

end Gtk.Layered_Editor;
