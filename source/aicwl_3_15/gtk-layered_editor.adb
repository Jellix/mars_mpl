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
-- __________________________________________________________________ --

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Numerics;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with Cairo.Font_Slant_Property;
with Cairo.Line_Cap_Property;

with Gdk.Color;

with Glib.Messages;
with Glib.Object.Checked_Destroy;
with Glib.Properties.Creation;

with Gtk.Adjustment;
with Gtk.Cell_Renderer_Text;
with Gtk.Enums.Shadow_Property;
with Gtk.Frame;
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
with Gtk.Layered.Waveform;
with Gtk.Layered.Waveform_Drawing_Method_Property;
with Gtk.Missed;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Tree_Model;
with Gtk.Tree_Selection;
with Gtk.Tree_View_Column;

with Pango.Cairo.Fonts.Font_Type_Property;
with Pango.Enums.Weight_Property;

with Strings_Edit.Integers;

package body Gtk.Layered_Editor is

   Default_Width : constant := 12;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered_Editor." & Name;
   end Where;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Annotation_Array,
        Annotation_Array_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Double_Edit_List,
        Double_Edit_List_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation (Layer_List, Layer_List_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Properties_List,
        Properties_List_Ptr);

   type Selection_List is record
      Index  : Natural := 0;
      Widget : Gtk_Layered_Editor;
      List   : Layer_List_Ptr;
   end record;
   type Selection_List_Ptr is access all Selection_List;

   package Selection_Browsing is
     new Gtk.Tree_Selection.Selected_Foreach_User_Data (Selection_List_Ptr);

   package body Generic_Enum is
      procedure Add
        (Widget     : Gtk_Layered_Editor;
         Row        : Guint;
         Layers     : Layer_List;
         Properties : Properties_List;
         No         : Property_Index;
         Tip        : String)
      is
         Box   : constant Gtk_Enum := new Gtk_Enum_Record;
         State : Enum_Property.Enumeration;
         Error : Boolean := False;
      begin
         Box.all.No     := No;
         Box.all.Widget := Widget;
         Boxes.Initialize (Box, Gtk.Missed.Capitalize_First, True);
         Box.all.Set_Tooltip_Text (Tip);
         Gtk.Table.Attach
           (Widget.all.Properties,
            Box,
            1, 2,
            Row, Row + 1,
            Yoptions => Gtk.Enums.Shrink);
         for Index in Properties'Range (1) loop
            declare
               Value : Glib.Values.GValue;

               use type Enum_Property.Enumeration;
            begin
               Value :=
                 Layers (Index).all.Get_Property_Value
                 (Properties (Index, No));
               if Index = Properties'First (1) then
                  State := Enum_Property.Get_Enum (Value);
               elsif State /= Enum_Property.Get_Enum (Value) then
                  Error := True;
               end if;
               Glib.Values.Unset (Value);
            end;
         end loop;
         if not Error then
            Set_Active_Value (Box, State);
         end if;
         Enum_Handlers.Connect
           (Box.all'Unchecked_Access,
            "changed",
            Changed'Access);
      exception
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               "Fault: "
               & Ada.Exceptions.Exception_Information (Error)
               & Where ("Generic_Enum.Add"));
      end Add;

      procedure Changed (Combo : access Gtk_Enum_Record'Class) is
         Value : Glib.Values.GValue;
      begin
         Enum_Property.Set_Enum (Value, Combo.all.Get_Active_Value);
         for Layer_No in Combo.all.Widget.all.Selected_Layers'Range loop
            Combo.all.Widget.all.Selected_Layers (Layer_No).all.Set_Property_Value
              (Combo.all.Widget.all.Selected_Properties (Layer_No, Combo.all.No),
               Value);
         end loop;
         Glib.Values.Unset (Value);
         Gtk.Layered.Queue_Draw (Combo.all.Widget.all.Layered.Get);
      exception
         when Gtk.Missed.No_Selection =>
            null;
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               "Fault: "
               & Ada.Exceptions.Exception_Information (Error)
               & Where ("Generic_Enum.Changed"));
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
       (Pango.Cairo.Fonts.Font_Type_Property,
        Font_Type_Box);

   package Font_Weight_Enum is
     new Generic_Enum (Pango.Enums.Weight_Property, Font_Weight_Box);

   package Interpolation_Mode_Enum is
     new Generic_Enum
       (Gtk.Layered.Interpolation_Mode_Property,
        Interpolation_Mode_Box);

   package Line_Cap_Enum is
     new Generic_Enum (Cairo.Line_Cap_Property, Line_Cap_Box);

   package Text_Transformation_Enum is
     new Generic_Enum
       (Gtk.Layered.Text_Transformation_Property,
        Text_Transformation_Box);

   package Waveform_Drawing_Method_Enum is
     new Generic_Enum
       (Gtk.Layered.Waveform_Drawing_Method_Property,
        Waveform_Drawing_Method_Box);

   procedure Add
     (Button : access Add_Buttons.Gtk_Style_Button_Record'Class;
      Widget : Gtk_Annotation_Texts) is
   begin
      Remove (Widget, Widget.all.Add);
      if Widget.all.Rows'Length >= Widget.all.Length then
         declare
            New_Rows : constant Annotation_Array_Ptr :=
                         new Annotation_Array (1 .. Widget.all.Length + 10);
         begin
            New_Rows (Widget.all.Rows'Range) := Widget.all.Rows.all;
            Free (Widget.all.Rows);
            Widget.all.Rows := New_Rows;
         end;
      end if;
      Widget.all.Length := Widget.all.Length + 1;
      Resize (Widget, Widget.all.Length + 1, 3);
      Add_Row (Widget, Widget.all.Length);
      Attach
        (Widget,
         Widget.all.Add,
         2, 3,
         Widget.all.Length, Widget.all.Length + 1,
         Xoptions => Gtk.Enums.Shrink,
         Yoptions => Gtk.Enums.Shrink);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Add"));
   end Add;

   procedure Add_Boolean
     (Widget     : not null access Gtk_Layered_Editor_Record;
      Row        : Guint;
      Layers     : Layer_List;
      Properties : Properties_List;
      No         : Property_Index;
      Tip        : String;
      Button     : out Gtk_Check_Edit) is
   begin
      Button := new Gtk_Check_Edit_Record;
      Gtk.Check_Button.Initialize (Button);
      Button.all.No := No;
      Button.all.Widget := Widget.all'Unchecked_Access;
      Button.all.Set_Tooltip_Text (Tip);
      Gtk.Table.Attach
        (Widget.all.Properties,
         Button,
         1, 2,
         Row, Row + 1,
         Yoptions => Gtk.Enums.Shrink);
      for Index in Properties'Range (1) loop
         declare
            Value : Glib.Values.GValue;
         begin
            Value :=
              Layers (Index).all.Get_Property_Value (Properties (Index, No));
            if Index = Properties'First (1) then
               Set_Active (Button, Glib.Values.Get_Boolean (Value));
            elsif Get_Active (Button) /= Glib.Values.Get_Boolean (Value) then
               null; -- Inconsistent value
            end if;
            Glib.Values.Unset (Value);
         end;
      end loop;
      Check_Handlers.Connect (Button, "toggled", Toggled'Access);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Add_Boolean"));
   end Add_Boolean;

   procedure Add_Color
     (Widget     : not null access Gtk_Layered_Editor_Record;
      Row        : Guint;
      Layers     : Layer_List;
      Properties : Properties_List;
      No         : Property_Index;
      Tip        : String;
      Button     : out Gtk_Color_Edit) is
   begin
      Button := new Gtk_Color_Edit_Record;
      Gtk.Color_Button.Initialize (Button);
      Button.all.No := No;
      Button.all.Widget := Widget.all'Unchecked_Access;
      Button.all.Set_Tooltip_Text (Tip);
      Widget.all.Properties.all.Attach
        (Button,
         1, 2,
         Row, Row + 1,
         Yoptions => Gtk.Enums.Shrink);
      for Index in Properties'Range (1) loop
         declare
            Value : Glib.Values.GValue;
         begin
            Value :=
              Layers (Index).all.Get_Property_Value
              (Properties (Index, No));
            if Index = Properties'First (1) then
               Set_Color (Button, Gdk.Color.Get_Value (Value));
               Glib.Values.Unset (Value);
            else
               declare
                  Color : Gdk.Color.Gdk_Color;

                  use type Gdk.Color.Gdk_Color;
               begin
                  Button.all.Get_Color (Color);
                  if Color /= Gdk.Color.Get_Value (Value) then
                     Glib.Values.Unset (Value);
                     return;
                  else
                     Glib.Values.Unset (Value);
                  end if;
               end;
            end if;
         end;
      end loop;
      Color_Handlers.Connect (Button, "color_set", Color_Set'Access);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Add_Color"));
   end Add_Color;

   procedure Add_Double
     (Widget     : not null access Gtk_Layered_Editor_Record;
      Row        : Guint;
      Layers     : Layer_List;
      Properties : Properties_List;
      No         : Property_Index;
      Tip        : String;
      Edit       : out Gtk_Double_Edit)
   is
      Min, Max : Gdouble;
   begin
      Edit := new Gtk_Double_Edit_Record;
      Gtk.GEntry.Initialize (Edit);
      Edit.all.Set_Width_Chars (Default_Width);
      if Gtk.Missed.Find_Property (Edit, "max-width-chars") /= null then
         Glib.Properties.Set_Property
           (Edit,
            Glib.Properties.Build ("max-width-chars"),
            Gint'(Default_Width));
      end if;
      Edit.all.No := No;
      Edit.all.Widget := Widget.all'Unchecked_Access;
      Edit.all.Set_Tooltip_Text (Tip);
      Gtk.Table.Attach
        (Widget.all.Properties,
         Edit,
         1, 2,
         Row, Row + 1,
         Yoptions => Gtk.Enums.Shrink);
      for Index in Properties'Range (1) loop
         declare
            Value : Glib.Values.GValue;
         begin
            Value :=
              Layers (Index).all.Get_Property_Value (Properties (Index, No));
            if Index = Properties'First (1) then
               Min := Glib.Values.Get_Double (Value);
               Max := Min;
            elsif Min > Glib.Values.Get_Double (Value) then
               Min := Glib.Values.Get_Double (Value);
            elsif Max < Glib.Values.Get_Double (Value) then
               Max := Glib.Values.Get_Double (Value);
            end if;
            Glib.Values.Unset (Value);
         end;
      end loop;
      if Min = Max then
         Set_Text (Edit, Double_Edit.Image (Min, RelSmall => 5));
      else
         Set_Text
           (Edit,
            Double_Edit.Image (Min, RelSmall => 5)
            & " .. "
            & Double_Edit.Image (Max, RelSmall => 5));
      end if;
      Double_Handlers.Connect (Edit, "changed", Value_Set'Access);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Add_Double"));
   end Add_Double;

   procedure Add_Row
     (Widget : not null access Gtk_Annotation_Texts_Record;
      Row    : Guint)
   is
      This : Annotation renames Widget.all.Rows (Row);
   begin
      Gtk.Label.Gtk_New (This.Label);
      This.Label.all.Set_Text (Strings_Edit.Integers.Image (Integer (Row)));
      This.Label.all.Set_Halign (Gtk.Widget.Align_End);
      This.Label.all.Set_Valign (Gtk.Widget.Align_Center);
      Widget.all.Attach
        (This.Label,
         0, 1,
         Row - 1, Row,
         Xoptions => Gtk.Enums.Fill,
         Yoptions => Gtk.Enums.Shrink);
      This.Label.all.Ref;

      This.Edit := new Gtk_Text_Record;
      Gtk.GEntry.Initialize (This.Edit);
      This.Edit.all.Set_Width_Chars (Default_Width);
      if Gtk.Missed.Find_Property (This.Edit, "max-width-chars") /= null then
         Glib.Properties.Set_Property
           (This.Edit,
            Glib.Properties.Build ("max-width-chars"),
            Gint'(Default_Width));
      end if;
      This.Edit.all.Position := Positive (Row);
      This.Edit.all.Widget   := Widget.all.Editor;
      This.Edit.all.Set_Tooltip_Text ("Annotation text");
      Widget.all.Attach
        (This.Edit,
         1, 2,
         Row - 1, Row,
         Yoptions => Gtk.Enums.Shrink);
      This.Edit.all.Ref;

      This.Markup := new Markup_Button_Record;
      Gtk.Check_Button.Initialize (This.Markup);
      This.Markup.all.Position := Positive (Row);
      This.Markup.all.Widget   := Widget.all.Editor;
      This.Markup.all.Set_Tooltip_Text
        ("If the annotation text uses pango markup");
      Widget.all.Attach
        (This.Markup,
         2, 3,
         Row - 1, Row,
         Xoptions => Gtk.Enums.Shrink,
         Yoptions => Gtk.Enums.Shrink);
      This.Markup.all.Ref;

      This.Delete := new Delete_Button_Record;
      Delete_Buttons.Initialize (This.Delete);
      This.Delete.all.Index := Positive (Row);
      Widget.all.Attach
        (This.Delete,
         3, 4,
         Row - 1, Row,
         Xoptions => Gtk.Enums.Shrink,
         Yoptions => Gtk.Enums.Shrink);
      This.Delete.all.Ref;

      Delete_Text_Handlers.Connect
        (This.Delete,
         "clicked",
         Delete'Access,
         Widget.all'Unchecked_Access);
      This.Markup.all.On_Toggled (Changed_Markup'Access);
      Text_Handlers.Connect
        (This.Edit,
         "changed",
         Changed_Text'Access);
      This.Label.all.Show_All;
      This.Markup.all.Show_All;
      This.Edit.all.Show_All;
      This.Delete.all.Show_All;
   end Add_Row;

   procedure Add_String
     (Widget     : not null access Gtk_Layered_Editor_Record;
      Row        : Guint;
      Layers     : Layer_List;
      Properties : Properties_List;
      No         : Property_Index;
      Hint       : String;
      Tip        : String)
   is
      procedure Check
        (Edit : not null access Gtk.GEntry.Gtk_Entry_Record'Class;
         Text : String) is
      begin
         if Gtk.GEntry.Get_Text (Edit) /= Text then
            Gtk.GEntry.Set_Text (Edit, "");
         end if;
      end Check;
      Count : Guint := 1;
      Frame : Gtk.Frame.Gtk_Frame;
      Edit  : Gtk_Edit;
      List  : Gtk_Annotation_Texts;
      Text  : String_Ptr;
   begin
      for Index in Properties'Range (1) loop
         declare
            Value : Glib.Values.GValue;
         begin
            Value :=
              Layers (Index).all.Get_Property_Value (Properties (Index, No));
            if Index = Properties'First (1) then
               Text := new String'(Glib.Values.Get_String (Value));
               for Index in Text'Range loop
                  if Text (Index) = Character'Val (10) then
                     Count := Count + 1;
                  end if;
               end loop;
               if Hint = "Annotation texts" then
                  List := new Gtk_Annotation_Texts_Record;
                  Initialize (List, Widget.all'Unchecked_Access, Count);
                  Gtk.Frame.Gtk_New (Frame);
                  Frame.all.Set_Shadow_Type (Gtk.Enums.Shadow_Etched_In);
                  Gtk.Frame.Set_Border_Width (Frame, 3);
                  Gtk.Table.Attach
                    (Widget.all.Properties,
                     Frame,
                     1, 2,
                     Row, Row + 1,
                     Yoptions => Gtk.Enums.Shrink);
                  Frame.all.Add (List);
                  declare
                     Row   : Guint   := 1;
                     First : Integer := Text'First;
                  begin
                     for Index in Text'Range loop
                        if Text (Index) = Character'Val (10) then
                           Set_Text
                             (List.all.Rows (Row).Edit,
                              Text (First .. Index - 1));
                           First := Index + 1;
                           Row   := Row + 1;
                        end if;
                     end loop;
                     Set_Text
                       (List.all.Rows (Row).Edit,
                        Text (First .. Text'Last));
                  end;
               else
                  Edit := new Gtk_Edit_Record;
                  Edit.all.No := No;
                  Edit.all.Widget := Widget.all'Unchecked_Access;
                  Gtk.GEntry.Initialize (Edit);
                  Edit.all.Set_Width_Chars (Default_Width);
                  if
                    Gtk.Missed.Find_Property (Edit, "max-width-chars") /= null
                  then
                     Glib.Properties.Set_Property
                       (Edit,
                        Glib.Properties.Build ("max-width-chars"),
                        Gint'(Default_Width));
                  end if;
                  Edit.all.Set_Tooltip_Text (Tip);
                  Gtk.Table.Attach
                    (Widget.all.Properties,
                     Edit,
                     1, 2,
                     Row, Row + 1,
                     Yoptions => Gtk.Enums.Shrink);
                  Set_Text (Edit, Text.all);
                  Edit_Handlers.Connect
                    (Edit,
                     "changed",
                     Changed_String'Access);
               end if;
            elsif Text.all /= Glib.Values.Get_String (Value) then
               if Count > 1 then
                  declare
                     First : Integer := Text'First;
                     Row   : Guint   := 1;
                  begin
                     for Index in Text'Range loop
                        if Text (Index) = Character'Val (10) then
                           Check
                             (List.all.Rows (Row).Edit,
                              Text (First .. Index - 1));
                           First := Index + 1;
                           Row   := Row + 1;
                        end if;
                     end loop;
                     Check
                       (List.all.Rows (Row).Edit,
                        Text (First .. Text'Last));
                  end;
               else
                  Set_Text (Edit, "");
               end if;
            end if;
            Glib.Values.Unset (Value);
         end;
      end loop;
      Free (Text);
   exception
      when Error : others =>
         Free (Text);
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Add_String"));
   end Add_String;

   procedure Add_UInt
     (Widget     : not null access Gtk_Layered_Editor_Record;
      Row        : Guint;
      Layers     : Layer_List;
      Properties : Properties_List;
      No         : Property_Index;
      Tip        : String)
   is
      Edit     : constant Gtk_UInt_Edit := new Gtk_UInt_Edit_Record;
      Min, Max : Guint;
   begin
      Gtk.GEntry.Initialize (Edit);
      Set_Width_Chars (Edit, Default_Width);
      if Gtk.Missed.Find_Property (Edit, "max-width-chars") /= null then
         Glib.Properties.Set_Property
           (Edit,
            Glib.Properties.Build ("max-width-chars"),
            Gint'(Default_Width));
      end if;
      Edit.all.No := No;
      Edit.all.Widget := Widget.all'Unchecked_Access;
      Edit.all.Set_Tooltip_Text (Tip);
      Gtk.Table.Attach
        (Widget.all.Properties,
         Edit,
         1, 2,
         Row, Row + 1,
         Yoptions => Gtk.Enums.Shrink);
      for Index in Properties'Range (1) loop
         declare
            Value : Glib.Values.GValue;
         begin
            Value :=
              Layers (Index).all.Get_Property_Value (Properties (Index, No));
            if Index = Properties'First (1) then
               Max := Glib.Values.Get_Uint (Value);
               Min := Max;
            elsif Min > Glib.Values.Get_Uint (Value) then
               Min := Glib.Values.Get_Uint (Value);
            elsif Max < Glib.Values.Get_Uint (Value) then
               Max := Glib.Values.Get_Uint (Value);
            end if;
            Glib.Values.Unset (Value);
         end;
      end loop;
      if Min = Max then
         Set_Text (Edit, Strings_Edit.Integers.Image (Integer (Min)));
      else
         Set_Text
           (Edit,
            Strings_Edit.Integers.Image (Integer (Min)) & " .. " &
              Strings_Edit.Integers.Image (Integer (Max)));
      end if;
      UInt_Handlers.Connect (Edit, "changed", Value_Set'Access);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Add_UInt"));
   end Add_UInt;

   procedure Border_Color_Type_Toggled
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor)
   is
      Value : Glib.Values.GValue;
   begin
      if Widget.all.Border_Color /= null then
         Value :=
           Widget.all.Selected_Layers (1).all.Get_Property_Value
           (Widget.all.Selected_Properties (1, Widget.all.Border_Color.all.No));
         Set_Color (Widget.all.Border_Color, Gdk.Color.Get_Value (Value));
         Glib.Values.Unset (Value);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Border_Color_Type_Toggled"));
   end Border_Color_Type_Toggled;

   procedure Browse_Selected
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data  : Selection_List_Ptr)
   is
      Selection : Selection_List renames Data.all;
      Item      : constant Gint_Array  := Gtk.Tree_Model.Get_Indices (Path);
      Layered   : constant access Gtk.Layered.Gtk_Layered_Record'Class :=
                    Layered_References.Get (Data.all.Widget.all.Layered);
   begin
      Selection.Index := Selection.Index + 1;
      Selection.List (Selection.Index) :=
        Gtk.Layered.Get_Layer
          (Layered_References.Get (Data.all.Widget.all.Layered),
           Layered.all.Get_Depth - Natural (Item (Item'First))).all'Unchecked_Access;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Browse_Selected"));
   end Browse_Selected;

   procedure Changed_Aspect
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Layered.Get.all.Set_Aspect_Ratio
        (Double_Edit.Value (Gtk.GEntry.Get_Text (Widget.all.Aspect_Edit)));
      Gtk.Layered.Queue_Draw (Widget.all.Layered.Get);
   exception
      when Ada.IO_Exceptions.End_Error |
           Ada.IO_Exceptions.Data_Error |
           Constraint_Error =>
         null;
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Changed_Aspect"));
   end Changed_Aspect;

   procedure Changed_Markup
     (Markup : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class)
   is
      This : constant Markup_Button :=
               Markup_Button_Record'Class (Markup.all)'Unchecked_Access;
   begin
      for Layer_No in This.all.Widget.all.Selected_Layers'Range loop
         if This.all.Widget.all.Selected_Layers (Layer_No).all in
           Gtk.Layered.Annotation_Layer'Class
         then
            declare
               Layer : Gtk.Layered.Annotation_Layer'Class renames
                         Gtk.Layered.Annotation_Layer'Class
                           (This.all.Widget.all.Selected_Layers (Layer_No).all);
            begin
               Layer.Set_Text
                 (This.all.Position,
                  Layer.Get_Text (This.all.Position),
                  This.all.Get_Active);
            end;
         end if;
      end loop;
      Gtk.Layered.Queue_Draw (This.all.Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Changed_Markup"));
   end Changed_Markup;

   procedure Changed_String (Edit : access Gtk_Edit_Record'Class) is
      Text  : constant String := Get_Text (Edit);
      Value : Glib.Values.GValue;
   begin
      Glib.Values.Init (Value, GType_String);
      Glib.Values.Set_String (Value, Text);
      for Layer_No in Edit.all.Widget.all.Selected_Layers'Range loop
         Edit.all.Widget.all.Selected_Layers (Layer_No).all.Set_Property_Value
           (Edit.all.Widget.all.Selected_Properties (Layer_No, Edit.all.No),
            Value);
      end loop;
      Glib.Values.Unset (Value);
      Gtk.Layered.Queue_Draw (Edit.all.Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Changed_String"));
   end Changed_String;

   procedure Changed_Text (Edit : access Gtk_Text_Record'Class) is
      Text : constant String := Get_Text (Edit);
   begin
      for Layer_No in Edit.all.Widget.all.Selected_Layers'Range loop
         if Edit.all.Widget.all.Selected_Layers (Layer_No).all in
           Gtk.Layered.Annotation_Layer'Class
         then
            declare
               Layer : Gtk.Layered.Annotation_Layer'Class renames
                         Gtk.Layered.Annotation_Layer'Class
                           (Edit.all.Widget.all.Selected_Layers (Layer_No).all);
            begin
               Layer.Set_Text
                 (Edit.all.Position,
                  Text,
                  Layer.Get_Markup (Edit.all.Position));
            end;
         end if;
      end loop;
      Gtk.Layered.Queue_Draw (Edit.all.Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Changed_Text"));
   end Changed_Text;

   procedure Color_Set
     (Button : access Gtk_Color_Edit_Record'Class)
   is
      Value : Glib.Values.GValue;
      Color : Gdk.Color.Gdk_Color;
   begin
      Button.all.Get_Color (Color);
      Gdk.Color.Set_Value (Value, Color);
      for Layer_No in Button.all.Widget.all.Selected_Layers'Range loop
         Button.all.Widget.all.Selected_Layers (Layer_No).all.Set_Property_Value
           (Button.all.Widget.all.Selected_Properties (Layer_No, Button.all.No),
            Value);
      end loop;
      Glib.Values.Unset (Value);
      Gtk.Layered.Queue_Draw (Button.all.Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Color_Set"));
   end Color_Set;

   procedure Delete
     (Button : access Delete_Button_Record'Class;
      Widget : Gtk_Annotation_Texts)
   is
      function Get_Text (Row : Guint) return String is
      begin
         if Row = Widget.all.Length then
            return "";
         else
            return Widget.all.Rows (Row + 1).Edit.all.Get_Text;
         end if;
      end Get_Text;
      Deleted : constant Guint := Guint (Button.all.Index);
      Editor  : Gtk_Layered_Editor_Record'Class renames
                  Widget.all.Editor.all;
   begin
      for Index in Deleted .. Widget.all.Length loop
         declare
            Text : constant String := Get_Text (Index);
         begin
            for Layer_No in Editor.Selected_Layers'Range loop
               if Editor.Selected_Layers (Layer_No).all in
                 Gtk.Layered.Annotation_Layer'Class
               then
                  Gtk.Layered.Annotation_Layer'Class
                    (Editor.Selected_Layers (Layer_No).all).
                         Set_Text (Integer (Index), Text);
               end if;
            end loop;
         end;
      end loop;
      Widget.all.Remove (Deleted);
      Widget.all.Rows (Deleted).Label.all.Unref;
      Widget.all.Rows (Deleted).Edit.all.Unref;
      Widget.all.Rows (Deleted).Markup.all.Unref;
      Widget.all.Rows (Deleted).Delete.all.Unref;
      Widget.all.Rows (Deleted .. Widget.all.Length - 1) :=
        Widget.all.Rows (Deleted + 1 .. Widget.all.Length);
      Resize (Widget, Widget.all.Length, 3);
      Widget.all.Length := Widget.all.Length - 1;
      Widget.all.Insert (Deleted);
      Gtk.Layered.Queue_Draw (Editor.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Delete"));
   end Delete;

   procedure Delete_Layers
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Gtk.Layered.Abstract_Layer'Class,
           Layer_Ptr);
   begin
      if Widget.all.Selected_Layers /= null then
         -- The selection is not empty
         for Index in Widget.all.Selected_Layers'Range loop
            -- Searching for a layer selected
            declare
               This : Layer_Ptr := Widget.all.Selected_Layers (Index);
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
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Delete_Layers"));
   end Delete_Layers;

   procedure Destroyed
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Free (Widget.all.Double_Edits);
      Free (Widget.all.Selected_Layers);
      Free (Widget.all.Selected_Properties);
      Gtk.List_Store.Unref (Widget.all.Store);
   end Destroyed;

   procedure Destroyed
     (Widget : access Gtk_Annotation_Texts_Record'Class) is
   begin
      for Index in 1 .. Widget.all.Length loop
         Widget.all.Rows (Index).Label.all.Unref;
         Widget.all.Rows (Index).Markup.all.Unref;
         Widget.all.Rows (Index).Edit.all.Unref;
         Widget.all.Rows (Index).Delete.all.Unref;
      end loop;
      Widget.all.Add.all.Unref;
      Free (Widget.all.Rows);
   end Destroyed;

   procedure Down
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Move ((0.0, Widget.all.Move_Step));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Down"));
   end Down;

   procedure Fill_Layer_List
     (Widget  : not null access Gtk_Layered_Editor_Record;
      Layered : not null access Gtk.Layered.Gtk_Layered_Record'Class)
   is
      Layer : access Gtk.Layered.Abstract_Layer'Class := Layered.all.Get_Upper;
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Depth : Gint := 1;
   begin
      Gtk.List_Store.Clear (Widget.all.Store);
      while Layer /= null loop
         Gtk.List_Store.Append (Widget.all.Store, Row);
         Gtk.Missed.Set (Widget.all.Store, Row, 0, Get_Name (Layer.all));
         Layer := Layer.all.Below;
         Depth := Depth + 1;
      end loop;
   end Fill_Layer_List;

   procedure Gain_Step_Changed
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Gain_Step :=
        Gtk.Spin_Button.Get_Value (Widget.all.Gain_Step_Button);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Gain_Step_Changed"));
   end Gain_Step_Changed;

   function Get
     (Widget : not null access Gtk_Layered_Editor_Record)
      return Gtk.Layered.Gtk_Layered
   is
      Layered : constant access Gtk.Layered.Gtk_Layered_Record'Class :=
                  Widget.all.Layered.Get;
   begin
      if Layered = null then
         return null;
      else
         return Layered.all'Unchecked_Access;
      end if;
   end Get;

   function Get_Buttons_Box
     (Widget : not null access Gtk_Layered_Editor_Record)
      return Gtk.Box.Gtk_Box is
   begin
      return Widget.all.Button_Box;
   end Get_Buttons_Box;

   function Get_Common_Properties (List : Layer_List)
                                   return Properties_List is
      Count  : Property_Count :=
                 Property_Count (List (1).all.Get_Properties_Number);
      Common : Properties_List (List'Range, 1 .. Count);
   begin
      for Index in Common'Range (2) loop
         Common (1, Index) := Positive (Index);
         declare
            Pattern : constant Param_Spec :=
                        List (1).all.Get_Property_Specification (Common (1, Index));
         begin
            for Layer in 2 .. Common'Last (1) loop
               Common (Layer, Index) :=
                 Gtk.Layered.Find_Property (List (Layer).all, Pattern);
               if 0 = Common (Layer, Index) then
                  Common (1, Index) := 0;
                  Count := Count - 1;
                  exit;
               end if;
            end loop;
            Glib.Properties.Creation.Unref (Pattern);
         end;
      end loop;
      if Count = 0 then
         return (List'Range => (1 .. 0 => 0));
      else
         return Result : Properties_List (List'Range, 1 .. Count) do
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

   function Get_Name (Layer : Gtk.Layered.Abstract_Layer'Class)
                      return UTF8_String
   is
      Name : String := Ada.Tags.Expanded_Name (Layer'Tag);
   begin
      for Index in reverse Name'Range loop
         if Name (Index) = '.' then
            if Index < Name'Last then
               Name (Index + 1) := Ada.Characters.Handling.To_Upper (Name (Index + 1));
            end if;
            return Name (Index + 1 .. Name'Last);
         elsif Name (Index) = '_' then
            Name (Index) := ' ';
            --            if Index < Name'Last then
            --               Name (Index + 1) := Ada.Characters.Handling.To_Upper (Name (Index + 1));
            --            end if;
         else
            Name (Index) := Ada.Characters.Handling.To_Lower (Name (Index));
         end if;
      end loop;
      return Name;
   end Get_Name;

   procedure Gtk_New
     (Widget  : out Gtk_Layered_Editor;
      Layered : access Gtk.Layered.Gtk_Layered_Record'Class := null) is
   begin
      Widget := new Gtk_Layered_Editor_Record;
      Initialize (Widget, Layered);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget : not null access Gtk_Annotation_Texts_Record'Class;
      Editor : Gtk_Layered_Editor;
      Length : Guint) is
   begin
      Gtk.Table.Initialize (Widget, Length + 1, 4, False);
      Widget.all.Editor := Editor;
      Widget.all.Set_Border_Width (3);
      Widget.all.Set_Col_Spacings (3);
      Widget.all.Set_Row_Spacings (3);
      Widget.all.Length := Length;
      Widget.all.Rows   := new Annotation_Array (1 .. Length);
      for Row in Widget.all.Rows'Range loop
         Add_Row (Widget, Row);
      end loop;
      Add_Buttons.Gtk_New (Widget.all.Add);
      Widget.all.Attach
        (Widget.all.Add,
         3, 4,
         Length, Length + 1,
         Xoptions => Gtk.Enums.Shrink,
         Yoptions => Gtk.Enums.Shrink);
      Widget.all.Add.all.Ref;
      Annotation_Text_Handlers.Connect
        (Widget,
         "destroy",
         Destroyed'Access);
      Add_Text_Handlers.Connect
        (Widget.all.Add,
         "clicked",
         Add'Access,
         Widget.all'Unchecked_Access);
   end Initialize;

   procedure Initialize
     (Widget  : not null access Gtk_Layered_Editor_Record'Class;
      Layered : access Gtk.Layered.Gtk_Layered_Record'Class)
   is
      Frame    : Gtk.Frame.Gtk_Frame;
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Paned    : Gtk.Paned.Gtk_Paned;
   begin
      Gtk.Box.Initialize_Vbox (Widget);
      Gtk.Box.Gtk_New_Hbox (Widget.all.Button_Box);
      Widget.all.Button_Box.all.Set_Spacing (3);
      Widget.all.Pack_Start (Widget.all.Button_Box, False, False);

      Gtk.Paned.Gtk_New_Hpaned (Paned);
      Widget.all.Pack_Start (Paned);

      Gtk.Frame.Gtk_New (Frame);
      Frame.all.Set_Shadow_Type (Gtk.Enums.Shadow_In);
      Paned.all.Add1 (Frame);
      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Frame.all.Add (Scrolled);
      Gtk.Tree_View.Gtk_New (Widget.all.View);
      Widget.all.View.all.Set_Rules_Hint (True);
      Gtk.Tree_View.Get_Selection (Widget.all.View).all.Set_Mode
        (Gtk.Enums.Selection_Multiple);
      Scrolled.all.Add (Widget.all.View);
      --
      -- Adding columns
      --
      Gtk.Tree_View.Set_Rules_Hint (Widget.all.View, True);
      declare
         Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
         Text      : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk.Tree_View_Column.Gtk_New (Column);
         Gtk.Cell_Renderer_Text.Gtk_New (Text);
         Gtk.Tree_View_Column.Pack_Start (Column, Text, True);
         Gtk.Tree_View_Column.Add_Attribute (Column, Text, "text", 0);
         Column_No := Gtk.Tree_View.Append_Column (Widget.all.View, Column);
         Gtk.Tree_View_Column.Set_Title (Column, "Layer type");
         Gtk.Tree_View_Column.Set_Resizable (Column, True);
         Gtk.Tree_View_Column.Set_Sort_Column_Id (Column, 0);
      end;
      Editor_Handlers.Connect
        (Gtk.Tree_View.Get_Selection (Widget.all.View),
         "changed",
         Selection_Changed'Access,
         Widget.all'Access);
      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.all.Set_Policy
        (Gtk.Enums.Policy_Never, Gtk.Enums.Policy_Automatic);
      Paned.all.Pack2 (Scrolled, False, True);
      Gtk.Viewport.Gtk_New (Widget.all.Viewport);
      Widget.all.Viewport.all.Set_Shadow_Type (Gtk.Enums.Shadow_None);
      Scrolled.all.Add (Widget.all.Viewport);
      Editor_Handlers.Connect
        (Widget,
         "destroy",
         Destroyed'Access,
         Widget.all'Access);
      --
      -- Adding buttons
      --
      declare
         Label : Gtk.Label.Gtk_Label;
      begin
         -- Aspect edit
         Gtk.GEntry.Gtk_New (Widget.all.Aspect_Edit);
         Widget.all.Aspect_Edit.all.Set_Width_Chars (6);
         if
           Gtk.Missed.Find_Property
             (Widget.all.Aspect_Edit, "max-width-chars") /= null
         then
            Glib.Properties.Set_Property
              (Widget.all.Aspect_Edit,
               Glib.Properties.Build ("max-width-chars"),
               Gint'(6));
         end if;
         Widget.all.Button_Box.all.Pack_End (Widget.all.Aspect_Edit, False, False);
         Widget.all.Aspect_Edit.all.Set_Sensitive (False);
         Editor_Handlers.Connect
           (Widget.all.Aspect_Edit,
            "changed",
            Changed_Aspect'Access,
            Widget.all'Access);
         Gtk.Label.Gtk_New (Label, "Aspect ratio");
         Widget.all.Button_Box.all.Pack_End (Label, False, False);
         -- Lift button
         Lift_Buttons.Gtk_New (Widget.all.Lift);
         Widget.all.Lift.all.Set_Sensitive (False);
         Widget.all.Button_Box.all.Pack_Start (Widget.all.Lift, False, False);
         Editor_Handlers.Connect
           (Widget.all.Lift,
            "clicked",
            Lift'Access,
            Widget.all'Access);
         -- Lower button
         Lower_Buttons.Gtk_New (Widget.all.Lower);
         Widget.all.Lower.all.Set_Sensitive (False);
         Widget.all.Button_Box.all.Pack_Start (Widget.all.Lower, False, False);
         Editor_Handlers.Connect
           (Widget.all.Lower,
            "clicked",
            Lower'Access,
            Widget.all'Access);
         -- Remove button
         Remove_Buttons.Gtk_New (Widget.all.Remove);
         Widget.all.Remove.all.Set_Sensitive (False);
         Widget.all.Button_Box.all.Pack_Start (Widget.all.Remove, False, False);
         Editor_Handlers.Connect
           (Widget.all.Remove,
            "clicked",
            Delete_Layers'Access,
            Widget.all'Access);
         -- Layer combo
         Layer_Combo.Gtk_New
           (Widget.all.Add_Layer_Combo, Gtk.Missed.Capitalize_First, True);
         Widget.all.Button_Box.all.Pack_Start
           (Widget.all.Add_Layer_Combo,
            False,
            False);
         Editor_Handlers.Connect
           (Widget.all.Add_Layer_Combo,
            "changed",
            Selected_Layer_Combo'Access,
            Widget.all'Access);
         -- Insert button
         Insert_Buttons.Gtk_New (Widget.all.Insert);
         Widget.all.Insert.all.Set_Sensitive (False);
         Widget.all.Button_Box.all.Pack_Start (Widget.all.Insert, False, False);
         Editor_Handlers.Connect
           (Widget.all.Insert,
            "clicked",
            Inset_Layer'Access,
            Widget.all'Access);
      end;
      Gtk.List_Store.Gtk_New (Widget.all.Store, (0 => GType_String));
      Put (Widget, Layered);
   end Initialize;

   procedure Insert
     (Widget : not null access Gtk_Annotation_Texts_Record;
      From   : Guint) is
   begin
      for Index in From .. Widget.all.Length loop
         Widget.all.Attach
           (Widget.all.Rows (Index).Label,
            0, 1,
            Index - 1, Index,
            Xoptions => Gtk.Enums.Fill,
            Yoptions => Gtk.Enums.Shrink);
         Gtk.Label.Set_Text (Widget.all.Rows (Index).Label,
                             Strings_Edit.Integers.Image (Integer (Index)));
         Widget.all.Attach
           (Widget.all.Rows (Index).Edit,
            1, 2,
            Index - 1, Index,
            Yoptions => Gtk.Enums.Shrink);
         Widget.all.Rows (Index).Edit.all.Position := Integer (Index);
         Widget.all.Attach
           (Widget.all.Rows (Index).Markup,
            2, 3,
            Index - 1, Index,
            Xoptions => Gtk.Enums.Shrink,
            Yoptions => Gtk.Enums.Shrink);
         Widget.all.Rows (Index).Markup.all.Position := Positive (Index);
         Widget.all.Attach
           (Widget.all.Rows (Index).Delete,
            3, 4,
            Index - 1, Index,
            Xoptions => Gtk.Enums.Shrink,
            Yoptions => Gtk.Enums.Shrink);
         Widget.all.Rows (Index).Delete.all.Index := Positive (Index);
      end loop;
      Widget.all.Attach
        (Widget.all.Add,
         3, 4,
         Widget.all.Length, Widget.all.Length + 1,
         Xoptions => Gtk.Enums.Shrink,
         Yoptions => Gtk.Enums.Shrink);
   end Insert;

   Default_Angle   : constant := Ada.Numerics.Pi / 6.0;
   Default_Radius  : constant := 50.0;
   Default_Texts   : constant String := "1 2 3";
   Default_Color   : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (1.0, 0.0, 1.0);
   Default_Center  : constant Cairo.Ellipses.Cairo_Tuple :=
                       (X => Default_Radius,
                        Y => Default_Radius);
   Default_Box     : constant Cairo.Ellipses.Cairo_Box :=
                       (X1 => 0.0,
                        X2 => Default_Radius * 2.0,
                        Y1 => 0.0,
                        Y2 => Default_Radius * 2.0);
   Default_Ellipse : constant Cairo.Ellipses.Ellipse_Parameters :=
                       (Center          => Default_Center,
                        Major_Curvature => 1.0 / Default_Radius,
                        Minor_Radius    => Default_Radius,
                        Angle           => 0.0);

   procedure Inset_Layer
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor)
   is
      Under      : access Gtk.Layered.Layer_Location'Class := Widget.all.Layered.Get;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment;
      Position   : Positive;

      use type Cairo.Ellipses.Ellipse_Parameters;
   begin
      if Widget.all.Selected_Layers /= null then
         for Index in Widget.all.Selected_Layers'Range loop
            if Widget.all.Selected_Layers (Index) /= null then
               Under := Widget.all.Selected_Layers (Index).all.Above;
               exit when Under /= null;
               Under := Widget.all.Layered.Get;
               exit;
            end if;
         end loop;
      end if;
      case Layer_Combo.Get_Active_Value (Widget.all.Add_Layer_Combo) is
         when Arc_Layer =>
            Position :=
              Gtk.Layered.Arc.Add_Arc
                (Under   => Under,
                 Ellipse => Default_Ellipse,
                 Color   => Default_Color).all.Get_Position;
         when Bar_Layer =>
            Gtk.Adjustment.Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
              Gtk.Layered.Bar.Add_Bar
                (Under      => Under,
                 From       => (Default_Radius, 2.0 * Default_Radius),
                 To         => (Default_Radius, 0.0),
                 Color      => Default_Color,
                 Adjustment => Adjustment).all.Get_Position;
         when Cache_Layer =>
            Position :=
              Gtk.Layered.Cache.Add_Cache (Under).all.Get_Position;
         when Cap_Layer =>
            Position :=
              Gtk.Layered.Cap.Add_Cap (Under).all.Get_Position;
         when Clock_Hand_Layer =>
            Position :=
              Gtk.Layered.Clock_Hand.Add_Clock_Hand
                (Under  => Under,
                 Color  => Default_Color,
                 Center => Default_Center).all.Get_Position;
         when Elliptic_Annotation_Layer =>
            Position :=
              Gtk.Layered.Elliptic_Annotation.
                Add_Elliptic_Annotation
                  (Under   => Under,
                   Texts   => Default_Texts,
                   Step    => Default_Angle,
                   Color   => Default_Color,
                   Ellipse => Default_Ellipse).all.Get_Position;
         when Elliptic_Background_Layer =>
            Position :=
              Gtk.Layered.Elliptic_Background.
                Add_Elliptic_Background
                  (Under        => Under,
                   Outer        => Default_Ellipse,
                   Color        => Default_Color,
                   Border_Width => 6.0,
                   Border_Depth => 3.0).all.Get_Position;
         when Elliptic_Bar_Layer =>
            Gtk.Adjustment.Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
              Gtk.Layered.Elliptic_Bar.Add_Elliptic_Bar
                (Under      => Under,
                 Ellipse    => Default_Ellipse,
                 Color      => Default_Color,
                 Adjustment => Adjustment).all.Get_Position;
         when Elliptic_Scale_Layer =>
            Position :=
              Gtk.Layered.Elliptic_Scale.Add_Elliptic_Scale
                (Under => Under,
                 Outer => Default_Ellipse,
                 Inner => Default_Ellipse * 0.8,
                 Color => Default_Color,
                 Step  => Default_Angle).all.Get_Position;
         when Flat_Annotation_Layer =>
            Position :=
              Gtk.Layered.Flat_Annotation.Add_Flat_Annotation
                (Under  => Under,
                 From   => (Default_Radius, 2.0 * Default_Radius),
                 Length => 2.0 * Default_Radius,
                 Step   => Default_Radius / 5.0,
                 Color  => Default_Color,
                 Texts  => Default_Texts).all.Get_Position;
         when Flat_Needle_Layer =>
            Gtk.Adjustment.Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
              Gtk.Layered.Flat_Needle.Add_Flat_Needle
                (Under      => Under,
                 From       => (Default_Radius, 2.0 * Default_Radius),
                 To         => (Default_Radius, 0.0),
                 Color      => Default_Color,
                 Adjustment => Adjustment).all.Get_Position;
         when Flat_Scale_Layer =>
            Position :=
              Gtk.Layered.Flat_Scale.Add_Flat_Scale
                (Under   => Under,
                 From    => (Default_Radius, 2.0 * Default_Radius),
                 Length  => 2.0 * Default_Radius,
                 Breadth => 10.0,
                 Color   => Default_Color,
                 Step    => Default_Radius / 5.0).all.Get_Position;
         when Graph_Paper_Layer =>
            Position :=
              Gtk.Layered.Graph_Paper.Add_Graph_Paper
                (Under => Under,
                 Box   => Default_Box).all.Get_Position;
         when Graph_Paper_Annotation_Layer =>
            declare
               Layer                        : constant access
                 Gtk.Layered.Graph_Paper_Annotation.
                   Graph_Paper_Annotation_Layer :=
                     Gtk.Layered.Graph_Paper_Annotation.
                       Add_Graph_Paper_Annotation
                         (Under   => Under,
                          Paper   => Gtk.Layered.Graph_Paper.
                            Add_Graph_Paper
                              (Under => Under,
                               Box   => Default_Box));
            begin
               Position := Layer.all.Get_Position;
            end;
         when Graph_Paper_Time_Annotation_Layer =>
            Position :=
              Gtk.Layered.Graph_Paper_Annotation.
                Add_Graph_Paper_Time_Annotation
                  (Under   => Under,
                   Paper   => Gtk.Layered.Graph_Paper.
                     Add_Graph_Paper
                       (Under => Under,
                        Box   => Default_Box)).all.Get_Position;
         when Label_Layer =>
            Position :=
              Gtk.Layered.Label.Add_Label
                (Under    => Under,
                 Location => Default_Center,
                 Color    => Default_Color,
                 Text     => "Label").all.Get_Position;
         when Line_Layer =>
            Position :=
              Gtk.Layered.Line.
                Add_Line
                  (Under   => Under,
                   Length  => Default_Radius,
                   Color   => Default_Color).all.Get_Position;
         when Needle_Layer =>
            Gtk.Adjustment.Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
              Gtk.Layered.Needle.Add_Needle
                (Under      => Under,
                 Center     => Default_Center,
                 Color      => Default_Color,
                 Adjustment => Adjustment).all.Get_Position;
         when Rectangular_Background_Layer =>
            Position :=
              Gtk.Layered.Rectangular_Background.
                Add_Rectangular_Background
                  (Under        => Under,
                   Center       => Default_Center,
                   Height       => Default_Radius,
                   Width        => Default_Radius,
                   Color        => Default_Color,
                   Border_Width => 6.0,
                   Border_Depth => 3.0).all.Get_Position;
         when Rectangle_Layer =>
            Position :=
              Gtk.Layered.Rectangle.
                Add_Rectangle
                  (Under => Under,
                   Box   => Default_Box,
                   Color => Default_Color).all.Get_Position;
         when Waveform_Layer =>
            Position :=
              Gtk.Layered.Waveform.
                Add_Waveform
                  (Under => Under,
                   Box   => Default_Box,
                   Color => Default_Color).all.Get_Position;
         when Rectangular_Clip_Region_On_Layer =>
            Position :=
              Gtk.Layered.Rectangular_Clip_Region.
                Add_Rectangular_Clip_Region (Under).all.Get_Position;
         when Sector_Needle_Layer =>
            Gtk.Adjustment.Gtk_New (Adjustment, 0.0, 0.0, 0.1, 0.01, 0.1);
            Position :=
              Gtk.Layered.Sector_Needle.Add_Sector_Needle
                (Under      => Under,
                 Outer      => Default_Ellipse,
                 Center     => Default_Center,
                 Color      => Default_Color,
                 Adjustment => Adjustment).all.Get_Position;
      end case;
   exception
      when Gtk.Missed.No_Selection =>
         null;
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Inset_Layer"));
   end Inset_Layer;

   procedure Layer_Added
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Widget : Gtk_Layered_Editor)
   is
      Position : constant Natural :=
                   Natural (Glib.Values.Get_Uint (Glib.Values.Nth (Params, 1)));
      Row      : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.List_Store.Insert
        (Widget.all.Store,
         Row,
         Gint (Widget.all.Layered.Get.all.Get_Depth - Position));
      Gtk.Missed.Set
        (Widget.all.Store,
         Row,
         0,
         Get_Name (Widget.all.Layered.Get.all.Get_Layer (Position).all));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Layer_Added"));
   end Layer_Added;

   procedure Layer_Removed
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Widget : Gtk_Layered_Editor) is
   begin
      declare
         Position : constant Guint :=
                      Glib.Values.Get_Uint (Glib.Values.Nth (Params, 1));
         Row      : Gtk.Tree_Model.Gtk_Tree_Iter :=
                      Gtk.List_Store.Nth_Child
                        (Widget.all.Store.all'Unchecked_Access,
                         Gtk.Tree_Model.Null_Iter,
                         (Gtk.List_Store.N_Children
                            (Widget.all.Store.all'Unchecked_Access,
                             Gtk.Tree_Model.Null_Iter) - Gint (Position)));

         use type Gtk.Tree_Model.Gtk_Tree_Iter;
      begin
         if Row /= Gtk.Tree_Model.Null_Iter then
            Gtk.List_Store.Remove (Widget.all.Store, Row);
         end if;
      end;
      if Widget.all.Selected_Layers /= null then
         declare -- Cleaning up the list of selected layers
            List : Layer_List renames Widget.all.Selected_Layers.all;
         begin
            for Index in List'Range loop
               if List (Index) /= null then
                  declare
                     Found : Boolean := False;
                     Layer : access Gtk.Layered.Abstract_Layer'Class :=
                               Widget.all.Layered.Get.all.Get_Lower;
                  begin
                     while Layer /= null loop
                        Found := List (Index) = Layer;
                        exit when Found;
                        Layer := Layer.all.Above;
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
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Layer_Removed"));
   end Layer_Removed;

   procedure Left
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Move ((-1.0 * Widget.all.Move_Step, 0.0));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Left"));
   end Left;

   procedure Lift
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      if Widget.all.Selected_Layers = null then
         return;
      end if;
      declare -- Moving the selected layers up
         List : constant Layer_List := Widget.all.Selected_Layers.all;
      begin
         for Index in List'Range loop
            if List (Index) /= null then
               Gtk.Layered.Insert
                 (Widget.all.Layered.Get,
                  List (Index).all,
                  List (Index).all.Get_Position + 1);
            end if;
         end loop;
         Reselect (Widget, List);
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Lift"));
   end Lift;

   procedure Lower
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      if Widget.all.Selected_Layers = null then
         return;
      end if;
      declare -- Moving the selected layers down
         List : constant Layer_List := Widget.all.Selected_Layers.all;
      begin
         for Index in reverse List'Range loop
            if List (Index) /= null then
               Gtk.Layered.Insert
                 (Widget.all.Layered.Get,
                  List (Index).all,
                  Integer'Max (List (Index).all.Get_Position - 1, 1));
            end if;
         end loop;
         Reselect (Widget, List);
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Lower"));
   end Lower;

   procedure Move
     (Widget : not null access Gtk_Layered_Editor_Record;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      for Layer_No in Widget.all.Selected_Layers'Range loop
         Widget.all.Selected_Layers (Layer_No).all.Move (Offset);
      end loop;
      Update_Double_Edits (Widget);
      Gtk.Layered.Queue_Draw (Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Move"));
   end Move;

   procedure Move_Step_Changed
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Move_Step :=
        Gtk.Spin_Button.Get_Value (Widget.all.Move_Step_Button);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Move_Step_Changed"));
   end Move_Step_Changed;

   procedure Put
     (Widget  : not null access Gtk_Layered_Editor_Record;
      Layered : access Gtk.Layered.Gtk_Layered_Record'Class := null) is
   begin
      Widget.all.Layered.Set (Layered);
      Gtk.Tree_View.Set_Model (Widget.all.View, Gtk.Tree_Model.Null_Gtk_Tree_Model);
      if Layered /= null then
         Fill_Layer_List (Widget, Layered);
         Gtk.Tree_View.Set_Model
           (Widget.all.View, Gtk.Tree_Model.To_Interface (Widget.all.Store));
         declare
            Size : Gtk.Widget.Gtk_Requisition;
         begin
            Gtk.Tree_View.Columns_Autosize (Widget.all.View);   -- Size columns
            Gtk.Tree_View.Size_Request (Widget.all.View, Size); -- Query the integral size
            Gtk.Tree_View.Set_Size_Request                  -- Set new size
              (Widget.all.View,
               Gint'Min (Size.Width,  300),
               Gint'Min (Size.Height, 100));
         end;
         Gtk.GEntry.Set_Sensitive (Widget.all.Aspect_Edit, True);
         Gtk.GEntry.Set_Text
           (Widget.all.Aspect_Edit,
            Double_Edit.Image (Layered.all.Get_Aspect_Ratio, AbsSmall => -3));
         Gtk.Handlers.References.Set
           (Widget.all.Layer_Added,
            Editor_Handlers.Connect
              (Layered,
               "layer-added",
               Layer_Added'Access,
               Widget.all'Access));
         Gtk.Handlers.References.Set
           (Widget.all.Layer_Removed,
            Editor_Handlers.Connect
              (Layered,
               "layer-removed",
               Layer_Removed'Access,
               Widget.all'Access));
         Insert_Buttons.Set_Sensitive
           (Widget.all.Insert,
            Widget.all.Layered.Get /= null and then
            Layer_Combo.Get_Active (Widget.all.Add_Layer_Combo) >= 0);
      end if;
   end Put;

   procedure Remove
     (Widget : not null access Gtk_Annotation_Texts_Record;
      From   : Guint) is
   begin
      for Index in From .. Widget.all.Length loop
         Remove (Widget, Widget.all.Rows (Index).Label);
         Remove (Widget, Widget.all.Rows (Index).Edit);
         Remove (Widget, Widget.all.Rows (Index).Markup);
         Remove (Widget, Widget.all.Rows (Index).Delete);
      end loop;
      Remove (Widget, Widget.all.Add);
   end Remove;

   procedure Reselect
     (Widget : not null access Gtk_Layered_Editor_Record;
      List   : Layer_List)
   is
      Selection : constant Gtk.Tree_Selection.Gtk_Tree_Selection :=
                    Widget.all.View.all.Get_Selection;
      Depth     : constant Gint :=
                    Gint (Widget.all.Layered.Get.all.Get_Depth);
      New_List  : array (List'Range) of Natural;
   begin
      for Index in List'Range loop
         if List (Index) = null then
            New_List (Index) := 0;
         else
            New_List (Index) := List (Index).all.Get_Position;
         end if;
      end loop;
      Gtk.Tree_Selection.Unselect_All (Selection);
      for Index in New_List'Range loop
         if New_List (Index) /= 0 then
            Gtk.Tree_Selection.Select_Iter
              (Selection,
               Gtk.List_Store.Nth_Child
                 (Widget.all.Store,
                  Gtk.Tree_Model.Null_Iter,
                  Depth - Gint (New_List (Index))));
         end if;
      end loop;
   end Reselect;

   procedure Right
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Move ((Widget.all.Move_Step, 0.0));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Right"));
   end Right;

   procedure Scale
     (Widget : not null access Gtk_Layered_Editor_Record;
      Gain   : Gdouble) is
   begin
      for Layer_No in Widget.all.Selected_Layers'Range loop
         begin
            Widget.all.Selected_Layers (Layer_No).all.Scale (Gain);
         exception
            when Constraint_Error =>
               null;
         end;
      end loop;
      Update_Double_Edits (Widget);
      Gtk.Layered.Queue_Draw (Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Scale"));
   end Scale;

   procedure Selected_Layer_Combo
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Insert_Buttons.Set_Sensitive
        (Widget.all.Insert,
         Widget.all.Layered.Get /= null and then
         Layer_Combo.Get_Active (Widget.all.Add_Layer_Combo) >= 0);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Selected_Layer_Combo"));
   end Selected_Layer_Combo;

   procedure Selection_Changed
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor)
   is
      Count  : aliased Natural := 0;
      Row    : Guint           := 0;
      Layers : aliased Selection_List;

      use type Gtk.Table.Gtk_Table;
   begin
      if Widget.all.Properties /= null then
         Gtk.Viewport.Remove (Widget.all.Viewport, Widget.all.Properties);
         Widget.all.Properties := null;
      end if;
      Count :=
        Natural
          (Gtk.Tree_Selection.Count_Selected_Rows
             (Gtk.Tree_View.Get_Selection (Widget.all.View)));
      Free (Widget.all.Selected_Layers);
      Free (Widget.all.Selected_Properties);
      if Count = 0 then
         Lift_Buttons.Set_Sensitive (Widget.all.Lift, False);
         Lower_Buttons.Set_Sensitive (Widget.all.Lower, False);
         Remove_Buttons.Set_Sensitive (Widget.all.Remove, False);
         return;
      end if;
      Lift_Buttons.Set_Sensitive (Widget.all.Lift, True);
      Lower_Buttons.Set_Sensitive (Widget.all.Lower, True);
      Remove_Buttons.Set_Sensitive (Widget.all.Remove, True);
      Layers.List   := new Layer_List (1 .. Count);
      Layers.Widget := Widget;
      Selection_Browsing.Selected_Foreach
        (Gtk.Tree_View.Get_Selection (Widget.all.View),
         Browse_Selected'Access,
         Layers'Unchecked_Access);
      Widget.all.Selected_Layers := Layers.List;
      Widget.all.Selected_Properties :=
        new Properties_List'(Get_Common_Properties (Layers.List.all));
      Widget.all.Double_Edits :=
        new Double_Edit_List (Widget.all.Selected_Properties'Range (2));
      Gtk.Table.Gtk_New
        (Widget.all.Properties,
         Widget.all.Selected_Properties'Length (2) + 1,
         2,
         False);
      Gtk.Viewport.Add (Widget.all.Viewport, Widget.all.Properties);
      Gtk.Table.Set_Col_Spacings (Widget.all.Properties, 3);
      Gtk.Table.Set_Row_Spacings (Widget.all.Properties, 3);
      Set_Properties
        (Widget,
         Layers.List.all,
         Widget.all.Selected_Properties.all,
         Widget.all.Double_Edits.all,
         Row);
      declare
         Bar             : Gtk.Separator.Gtk_Separator;
         Left_Button     : Left_Buttons.Gtk_Style_Button;
         Right_Button    : Right_Buttons.Gtk_Style_Button;
         Up_Button       : Up_Buttons.Gtk_Style_Button;
         Down_Button     : Down_Buttons.Gtk_Style_Button;
         Box             : Gtk.Box.Gtk_Hbox;
         Zoom_In_Button  : Zoom_In_Buttons.Gtk_Style_Button;
         Zoom_Out_Button : Zoom_Out_Buttons.Gtk_Style_Button;
      begin
         Gtk.Box.Gtk_New_Hbox (Box);
         Gtk.Box.Set_Spacing (Box, 3);
         Gtk.Table.Attach
           (Widget.all.Properties,
            Box,
            0, 3, Row, Row + 1,
            Yoptions => Gtk.Enums.Shrink);
         Left_Buttons.Gtk_New (Left_Button);
         Gtk.Box.Pack_Start (Box, Left_Button, False, False);
         Right_Buttons.Gtk_New (Right_Button);
         Gtk.Box.Pack_Start (Box, Right_Button, False, False);
         Gtk.Spin_Button.Gtk_New
           (Widget.all.Move_Step_Button, 0.0, 100.0, 0.01);
         Gtk.Spin_Button.Set_Value
           (Widget.all.Move_Step_Button, Widget.all.Move_Step);
         Gtk.Box.Pack_Start (Box, Widget.all.Move_Step_Button, False, False);
         Up_Buttons.Gtk_New (Up_Button);
         Gtk.Box.Pack_Start (Box, Up_Button, False, False);
         Down_Buttons.Gtk_New (Down_Button);
         Gtk.Box.Pack_Start (Box, Down_Button, False, False);
         Gtk.Separator.Gtk_New_Vseparator (Bar);
         Gtk.Box.Pack_Start (Box, Bar, False, False);

         Zoom_In_Buttons.Gtk_New (Zoom_In_Button);
         Gtk.Box.Pack_End (Box, Zoom_In_Button, False, False);
         Zoom_Out_Buttons.Gtk_New (Zoom_Out_Button);
         Gtk.Box.Pack_End (Box, Zoom_Out_Button, False, False);
         Gtk.Spin_Button.Gtk_New (Widget.all.Gain_Step_Button, 1.0, 4.0, 0.01);
         Gtk.Spin_Button.Set_Value
           (Widget.all.Gain_Step_Button, Widget.all.Gain_Step);
         Gtk.Box.Pack_End (Box, Widget.all.Gain_Step_Button, False, False);
         Gtk.Separator.Gtk_New_Vseparator (Bar);
         Gtk.Box.Pack_End (Box, Bar, False, False);
         Editor_Handlers.Connect
           (Down_Button,
            "clicked",
            Down'Access,
            Widget.all'Access);
         Editor_Handlers.Connect
           (Widget.all.Gain_Step_Button,
            "value_changed",
            Gain_Step_Changed'Access,
            Widget.all'Access);
         Editor_Handlers.Connect
           (Left_Button,
            "clicked",
            Left'Access,
            Widget.all'Access);
         Editor_Handlers.Connect
           (Widget.all.Move_Step_Button,
            "value_changed",
            Move_Step_Changed'Access,
            Widget.all'Access);
         Editor_Handlers.Connect
           (Right_Button,
            "clicked",
            Right'Access,
            Widget.all'Access);
         Editor_Handlers.Connect
           (Up_Button,
            "clicked",
            Up'Access,
            Widget.all'Access);
         Editor_Handlers.Connect
           (Zoom_In_Button,
            "clicked",
            Zoom_In'Access,
            Widget.all'Access);
         Editor_Handlers.Connect
           (Zoom_Out_Button,
            "clicked",
            Zoom_Out'Access,
            Widget.all'Access);
      end;
      Gtk.Table.Show_All (Widget.all.Properties);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Selection_Changed"));
   end Selection_Changed;

   procedure Set_Properties
     (Widget       : not null access Gtk_Layered_Editor_Record;
      Layers       : Layer_List;
      Properties   : Properties_List;
      Double_Edits : in out Double_Edit_List;
      Row          : in out Guint)
   is
      Label             : Gtk.Label.Gtk_Label;
      Edit              : Gtk.GEntry.Gtk_Entry;
      Border_Check_Edit : Gtk_Check_Edit;
      Border_Color_Edit : Gtk_Color_Edit;
   begin
      for Property in Properties'Range (2) loop
         declare
            Specification : constant Param_Spec :=
                              Layers (1).all.Get_Property_Specification
                                (Properties (1, Property));
            Name          : String := Glib.Properties.Creation.Nick_Name (Specification);
         begin
            if Name'Length > 1 then
               Name (1) := Ada.Characters.Handling.To_Upper (Name (1));
            end if;
            if Name /= "Annotation text markups" then
               Gtk.Label.Gtk_New (Label, Name);
               Label.all.Set_Halign (Gtk.Widget.Align_End);
               Label.all.Set_Valign (Gtk.Widget.Align_Center);
               Label.all.Set_Tooltip_Text
                 (Glib.Properties.Creation.Description (Specification));
               Gtk.Table.Attach
                 (Widget.all.Properties,
                  Label,
                  0, 1,
                  Row, Row + 1,
                  Xoptions => Gtk.Enums.Fill,
                  Yoptions => Gtk.Enums.Shrink);
               case Glib.Properties.Creation.Value_Type (Specification) is
                  when GType_Boolean =>
                     declare
                        Check_Edit : Gtk_Check_Edit;
                     begin
                        Add_Boolean
                          (Widget,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification),
                           Check_Edit);
                        if Name = "Border color type" then
                           Border_Check_Edit := Check_Edit;
                        end if;
                     end;
                  when GType_Double =>
                     Add_Double
                       (Widget,
                        Row,
                        Layers,
                        Properties,
                        Property,
                        Glib.Properties.Creation.Description (Specification),
                        Double_Edits (Property));
                  when GType_String =>
                     Add_String
                       (Widget,
                        Row,
                        Layers,
                        Properties,
                        Property,
                        Name,
                        Glib.Properties.Creation.Description (Specification));
                  when GType_Uint =>
                     Add_UInt
                       (Widget,
                        Row,
                        Layers,
                        Properties,
                        Property,
                        Glib.Properties.Creation.Description (Specification));
                  when others =>
                     if
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Gdk.Color.Gdk_Color_Type
                     then
                        declare
                           Color_Edit : Gtk_Color_Edit;
                        begin
                           Add_Color
                             (Widget,
                              Row,
                              Layers,
                              Properties,
                              Property,
                              Glib.Properties.Creation.Description (Specification),
                              Color_Edit);
                           if Name = "Border color" then
                              Border_Color_Edit := Color_Edit;
                           end if;
                        end;
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Gtk.Layered.Alignment_Property.Get_Type
                     then
                        Alignment_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Gtk.Layered.Interpolation_Mode_Property.Get_Type
                     then
                        Interpolation_Mode_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Gtk.Layered.Waveform_Drawing_Method_Property.Get_Type
                     then
                        Waveform_Drawing_Method_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Gtk.Layered.Text_Transformation_Property.Get_Type
                     then
                        Text_Transformation_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Cairo.Font_Slant_Property.Get_Type
                     then
                        Font_Slant_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Pango.Enums.Weight_Property.Get_Type
                     then
                        Font_Weight_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Pango.Cairo.Fonts.Font_Type_Property.Get_Type
                     then
                        Font_Type_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Cairo.Line_Cap_Property.Get_Type
                     then
                        Line_Cap_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     elsif
                       Glib.Properties.Creation.Value_Type (Specification) =
                       Gtk.Enums.Shadow_Property.Get_Type
                     then
                        Shadow_Type_Enum.Add
                          (Widget.all'Unchecked_Access,
                           Row,
                           Layers,
                           Properties,
                           Property,
                           Glib.Properties.Creation.Description (Specification));
                     else
                        Gtk.GEntry.Gtk_New (Edit);
                        Edit.all.Set_Width_Chars (Default_Width);
                        if
                          Gtk.Missed.Find_Property
                            (Edit, "max-width-chars") /= null
                        then
                           Glib.Properties.Set_Property
                             (Edit,
                              Glib.Properties.Build ("max-width-chars"),
                              Gint'(Default_Width));
                        end if;
                        Edit.all.Set_Tooltip_Text
                          (Glib.Properties.Creation.Description (Specification));
                        Gtk.Table.Attach
                          (Widget.all.Properties,
                           Edit,
                           1, 2,
                           Row, Row + 1,
                           Yoptions => Gtk.Enums.Shrink);
                     end if;
               end case;
               Row := Row + 1;
               Glib.Properties.Creation.Unref (Specification);
            end if;
         end;
      end loop;
      if Border_Check_Edit /= null and then Border_Color_Edit /= null
      then
         Widget.all.Border_Color := Border_Color_Edit;
         Editor_Handlers.Connect
           (Border_Check_Edit,
            "toggled",
            Border_Color_Type_Toggled'Access,
            Widget.all'Access);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Properties"));
   end Set_Properties;

   procedure Toggled
     (Button : access Gtk_Check_Edit_Record'Class)
   is
      Value : Glib.Values.GValue;
   begin
      Glib.Values.Init (Value, GType_Boolean);
      Glib.Values.Set_Boolean (Value, Get_Active (Button));
      for Layer_No in Button.all.Widget.all.Selected_Layers'Range loop
         Button.all.Widget.all.Selected_Layers (Layer_No).all.Set_Property_Value
           (Button.all.Widget.all.Selected_Properties (Layer_No, Button.all.No),
            Value);
      end loop;
      Glib.Values.Unset (Value);
      Gtk.Layered.Queue_Draw (Button.all.Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Toggled"));
   end Toggled;

   procedure Up
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Move ((0.0, -1.0 * Widget.all.Move_Step));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Up"));
   end Up;

   procedure Update_Double_Edits
     (Widget : not null access Gtk_Layered_Editor_Record)
   is
      Value : Glib.Values.GValue;
   begin
      for Property in Widget.all.Double_Edits'Range loop
         if Widget.all.Double_Edits (Property) /= null then
            Value :=
              Widget.all.Selected_Layers (1).all.Get_Property_Value
              (Widget.all.Selected_Properties (1, Property));
            Set_Text
              (Widget.all.Double_Edits (Property),
               Double_Edit.Image
                 (Glib.Values.Get_Double (Value), RelSmall => 5));
            Glib.Values.Unset (Value);
         end if;
      end loop;
   end Update_Double_Edits;

   procedure Value_Set
     (Edit : access Gtk_Double_Edit_Record'Class)
   is
      Input : Gdouble;
   begin
      begin
         Input := Double_Edit.Value (Get_Text (Edit));
      exception
         when Constraint_Error | Ada.IO_Exceptions.End_Error |
              Ada.IO_Exceptions.Data_Error =>
            return;
      end;
      declare
         Value : Glib.Values.GValue;
      begin
         Glib.Values.Init (Value, GType_Double);
         Glib.Values.Set_Double (Value, Input);
         for Layer_No in Edit.all.Widget.all.Selected_Layers'Range loop
            Edit.all.Widget.all.Selected_Layers (Layer_No).all.Set_Property_Value
              (Edit.all.Widget.all.Selected_Properties (Layer_No, Edit.all.No),
               Value);
         end loop;
         Glib.Values.Unset (Value);
      end;
      Gtk.Layered.Queue_Draw (Edit.all.Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Value_Set"));
   end Value_Set;

   procedure Value_Set
     (Edit : access Gtk_UInt_Edit_Record'Class)
   is
      Input : Guint;
   begin
      begin
         Input := Guint (Strings_Edit.Integers.Value (Get_Text (Edit)));
      exception
         when Constraint_Error | Ada.IO_Exceptions.End_Error |
              Ada.IO_Exceptions.Data_Error =>
            return;
      end;
      declare
         Value : Glib.Values.GValue;
      begin
         Glib.Values.Init (Value, GType_Uint);
         Glib.Values.Set_Uint (Value, Input);
         for Layer_No in Edit.all.Widget.all.Selected_Layers'Range loop
            Edit.all.Widget.all.Selected_Layers (Layer_No).all.Set_Property_Value
              (Edit.all.Widget.all.Selected_Properties (Layer_No, Edit.all.No),
               Value);
         end loop;
         Glib.Values.Unset (Value);
      end;
      Gtk.Layered.Queue_Draw (Edit.all.Widget.all.Layered.Get);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Value_Set"));
   end Value_Set;

   procedure Zoom_In
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Scale (Widget.all.Gain_Step);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Zoom_In"));
   end Zoom_In;

   procedure Zoom_Out
     (Object : access GObject_Record'Class;
      Widget : Gtk_Layered_Editor) is
   begin
      Widget.all.Scale (1.0 / Widget.all.Gain_Step);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Zoom_Out"));
   end Zoom_Out;

end Gtk.Layered_Editor;
