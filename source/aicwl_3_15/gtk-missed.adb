--                                                                    --
--  package Gtk.Missed              Copyright (c)  Maxim Reznik       --
--  Implementation                                 Summer, 2006       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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
with Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;

with Gdk.Cursor;
with Gdk.Window;

with Glib.Messages;
with Glib.Object.Checked_Destroy;
with Glib.Types;
with Glib.Unicode;

with Gtk.Box;
with Gtk.Css_Provider;
with Gtk.Icon_Set;
with Gtk.Icon_Source;
with Gtk.Icon_Theme;
with Gtk.Image;
with Gtk.Label;
with Gtk.Main;
with Gtk.Style_Provider;
with Gtk.Window;

package body Gtk.Missed is

   ------------------------------------------------------------------------
   type GHashTable is new System.Address;
   No_Table : constant GHashTable := GHashTable (System.Null_Address);

   type GEqualFunc is access function (A, B : Interfaces.C.char_array)
                                       return Gboolean;
   pragma Convention (C, GEqualFunc);

   type GHashFunc is access
     function (Key : Interfaces.C.char_array) return Gint;
   pragma Convention (C, GHashFunc);

   type GDestroyNotify is access
     procedure (Data : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, GDestroyNotify);

   function Str_Hash (Key : Interfaces.C.char_array) return Gint;
   pragma Import (C, Str_Hash, "g_str_hash");

   function Str_Equal (A, B : Interfaces.C.char_array) return Gboolean;
   pragma Import (C, Str_Equal, "g_str_equal");

   procedure Destroy (Data : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Destroy);

   function Table_New
     (Hash          : GHashFunc      := Str_Hash'Access;
      Equal         : GEqualFunc     := Str_Equal'Access;
      Key_Destroy   : GDestroyNotify := Destroy'Access;
      Value_Destroy : System.Address := System.Null_Address) return GHashTable;
   pragma Import (C, Table_New, "g_hash_table_new_full");

   function Insert (Table : GHashTable;
                    Key   : Interfaces.C.Strings.chars_ptr;
                    Value : System.Address) return Gboolean;
   pragma Import (C, Insert, "g_hash_table_insert");

   function Look_Up (Table : GHashTable;
                     Key   : Interfaces.C.char_array) return System.Address;
   pragma Import (C, Look_Up, "g_hash_table_lookup");

   Stock_Icons : GHashTable := No_Table;
   -----------------------------------------------------------------------------

   function G_Dir_Open
     (Path  : Interfaces.C.char_array;
      Flags : Guint;
      Error : access Glib.Error.GError) return GDir;
   pragma Import (C, G_Dir_Open, "g_dir_open_utf8");

   function G_Dir_Read_Name (Dir : GDir) return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, G_Dir_Read_Name, "g_dir_read_name_utf8");

   function G_File_Test
     (File_Name : Interfaces.C.char_array;
      Test      : GFileTest) return Gboolean;
   pragma Import (C, G_File_Test, "g_file_test_utf8");

   function G_Find_Program_In_Path_UTF8 (Program : Interfaces.C.char_array)
                                         return Interfaces.C.Strings.chars_ptr;
   pragma Import
     (C, G_Find_Program_In_Path_UTF8, "g_find_program_in_path_utf8");

   procedure G_Free (Border : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, G_Free, "g_free");

   function G_Get_Current_Dir_UTF8
     return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, G_Get_Current_Dir_UTF8, "g_get_current_dir_utf8");
   -----------------------------------------------------------------------------
   procedure Cell_Pixbuf
     (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data   : Gint);

   function Where (Name : String) return String;
   -----------------------------------------------------------------------------

   package body Set_Column_Cell_Data is

      package Closure is new Glib.Object.User_Data_Closure (User_Data, Destroy);

      function From_Address is
        new Ada.Unchecked_Conversion (System.Address, Cell_Data_Function);

      function To_Address is
        new Ada.Unchecked_Conversion (Cell_Data_Function, System.Address);

      procedure Callback
        (Column : System.Address;
         Cell   : System.Address;
         Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
         Data   : System.Address);
      pragma Convention (C, Callback);

      procedure Callback
        (Column : System.Address;
         Cell   : System.Address;
         Model  : Gtk.Tree_Model.Gtk_Tree_Model;
         Iter   : access Gtk.Tree_Model.Gtk_Tree_Iter;
         Data   : System.Address)
      is
         User_Data   : constant Closure.Internal_Data_Access :=
                         Closure.Convert (Data);
         Stub_Column : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
         Stub_Cell   : Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record;
      begin
         From_Address (User_Data.all.Func)
           (Gtk.Tree_View_Column.Gtk_Tree_View_Column
              (Get_User_Data (Column, Stub_Column)),
            Gtk.Cell_Renderer.Gtk_Cell_Renderer
              (Get_User_Data (Cell, Stub_Cell)),
            Model,
            Iter.all,
            User_Data.all.Data.all);
      exception
         when Error : others =>
            Glib.Messages.Log
              (GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               "Fault: " & Ada.Exceptions.Exception_Information (Error) &
                 Where ("Set_Column_Cell_Data (callback)"));
      end Callback;

      procedure Set_Cell_Data_Func
        (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
         Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
         Func   : Cell_Data_Function;
         Data   : User_Data)
      is
         procedure Internal
           (Tree_Column   : System.Address;
            Cell_Renderer : System.Address;
            Func          : System.Address;
            Func_data     : System.Address;
            Destroy       : System.Address);
         pragma Import (C, Internal, "gtk_tree_view_column_set_cell_data_func");
      begin
         if Func = null then
            Internal
              (Get_Object (Column),
               Get_Object (Cell),
               System.Null_Address,
               System.Null_Address,
               System.Null_Address);
         else
            Internal
              (Get_Object (Column),
               Get_Object (Cell),
               Callback'Address,
               Closure.Build (To_Address (Func), Data),
               Closure.Free_Data'Address);
         end if;
      end Set_Cell_Data_Func;

   end Set_Column_Cell_Data;

   package Set_Pixbuf_Data is new Set_Column_Cell_Data (GInt);

   function "+" (Width : Gint) return Gtk.Enums.Gtk_Icon_Size;
   function "+" (Width : Gint) return Gtk.Enums.Gtk_Icon_Size is
   begin
      if Width <= 16 then
         return Gtk.Enums.Icon_Size_Menu;
      elsif Width <= 18 then
         return Gtk.Enums.Icon_Size_Small_Toolbar;
      elsif Width <= 24 then
         return Gtk.Enums.Icon_Size_Large_Toolbar;
      elsif Width <= 32 then
         return Gtk.Enums.Icon_Size_Dnd;
      else
         return Gtk.Enums.Icon_Size_Dialog;
      end if;
   end "+";

   procedure Add_Button_From_Stock
     (Dialog     : not null access Gtk.Dialog.Gtk_Dialog_Record'Class;
      Response   : Gtk.Dialog.Gtk_Response_Type;
      Label      : UTF8_String                := "";
      Icon       : UTF8_String                := "";
      Icon_Left  : Boolean                    := True;
      Size       : Gtk.Enums.Gtk_Icon_Size    := Gtk.Enums.Icon_Size_Button;
      Spacing    : Guint                      := 3;
      Tip        : UTF8_String                := "";
      Relief     : Gtk.Enums.Gtk_Relief_Style := Gtk.Enums.Relief_Normal)
   is
      Button : Gtk.Button.Gtk_Button;
   begin
      Button :=
        Add_Button_From_Stock
          (Dialog    => Dialog,
           Response  => Response,
           Label     => Label,
           Icon      => Icon,
           Icon_Left => Icon_Left,
           Size      => Size,
           Spacing   => Spacing,
           Tip       => Tip,
           Relief    => Relief);
      pragma Unreferenced (Button);
   end Add_Button_From_Stock;

   function Add_Button_From_Stock
     (Dialog     : not null access Gtk.Dialog.Gtk_Dialog_Record'Class;
      Response   : Gtk.Dialog.Gtk_Response_Type;
      Label      : UTF8_String                := "";
      Icon       : UTF8_String                := "";
      Icon_Left  : Boolean                    := True;
      Size       : Gtk.Enums.Gtk_Icon_Size    := Gtk.Enums.Icon_Size_Button;
      Spacing    : Guint                      := 3;
      Tip        : UTF8_String                := "";
      Relief     : Gtk.Enums.Gtk_Relief_Style := Gtk.Enums.Relief_Normal)
      return Gtk.Button.Gtk_Button
   is
      Button : Gtk.Button.Gtk_Button;
      Box    : Gtk.Box.Gtk_Hbox;
      Text   : Gtk.Label.Gtk_Label;
      Image  : Gtk.Image.Gtk_Image;
   begin
      Gtk.Button.Gtk_New (Button);
      Gtk.Box.Gtk_New_Hbox (Box, False, 0);
      Box.all.Set_Border_Width (0);
      Box.all.Set_Spacing (Gint (Spacing));
      Button.all.Add (Box);
      Button.all.Set_Relief (Relief);
      if Icon_Left then
         if Icon'Length > 0 then
            Gtk.Image.Gtk_New (Image, Icon, Size);
            Box.all.Pack_Start (Image, False, False);
         end if;
         if Label'Length > 0 then
            Gtk.Label.Gtk_New_With_Mnemonic (Text, Label);
            Box.all.Pack_Start (Text, False, False);
         end if;
      else
         if Label'Length > 0 then
            Gtk.Label.Gtk_New_With_Mnemonic (Text, Label);
            Box.all.Pack_Start (Text, False, False);
         end if;
         if Icon'Length > 0 then
            Gtk.Image.Gtk_New (Image, Icon, Size);
            Box.all.Pack_Start (Image, False, False);
         end if;
      end if;
      if Tip'Length > 0 then
         Button.all.Set_Tooltip_Text (Tip);
      end if;
      Dialog.all.Add_Action_Widget (Button, Response);
      return Button;
   end Add_Button_From_Stock;

   procedure Add_Named (Name : UTF8_String;
                        Icon : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      Key    : Interfaces.C.Strings.chars_ptr;
      Set    : Gtk.Icon_Set.Gtk_Icon_Set;
      Result : Gboolean;
   begin
      Key := Interfaces.C.Strings.New_String (Name);
      if Stock_Icons = No_Table then
         Stock_Icons := Table_New;
         Gtk.Icon_Set.Gtk_New (Set);
         Result := Insert (Stock_Icons, Key, Set.Get_Object);
      else
         declare
            Address : constant System.Address :=
                        Look_Up (Stock_Icons, Interfaces.C.Strings.Value (Key));
            use type System.Address;
         begin
            if Address = System.Null_Address then
               Gtk.Icon_Set.Gtk_New (Set);
               Result := Insert (Stock_Icons, Key, Set.Get_Object);
            else
               Set.Set_Object (Address);
               Interfaces.C.Strings.Free (Key);
            end if;
         end;
      end if;
      pragma Unreferenced (Result);
      declare
         Source : Gtk.Icon_Source.Gtk_Icon_Source;
         use Gdk.Pixbuf;
      begin
         Gtk.Icon_Source.Gtk_New (Source);
         Gtk.Icon_Source.Set_Pixbuf (Source, Icon);
         Source.Set_Size (+Get_Width (Icon));
         Source.Set_Size_Wildcarded (False);
         Set.Add_Source (Source);
         Gtk.Icon_Source.Free (Source);
      end;
   end Add_Named;

   procedure Add_Stock_Attribute
     (Cell_Layout : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell        : not null access Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf_Record'Class;
      Column      : Gint) is
   begin
      Cell_Layout.all.Add_Attribute (Cell, "icon-name", Column);
      Set_Pixbuf_Data.Set_Cell_Data_Func
        (Cell_Layout,
         Cell,
         Cell_Pixbuf'Access,
         Column);
   exception
      when Error : others =>
         Glib.Messages.Log
           (GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Add_Stock_Attribute"));
   end Add_Stock_Attribute;

   function Build_Filename (First_Element, Second_Element : UTF8_String)
                            return UTF8_String is
      function Internal (Args : Interfaces.C.Strings.chars_ptr_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (First_Element);
      Second : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Second_Element);
      Ptr    : constant Interfaces.C.Strings.chars_ptr :=
                 Internal
                   ((0 =>
                       Interfaces.C.Strings.To_Chars_Ptr (First'Unchecked_Access),
                     1 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Second'Unchecked_Access),
                     2 => Interfaces.C.Strings.Null_Ptr));
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Build_Filename;

   function Build_Filename
     (First_Element  : UTF8_String;
      Second_Element : UTF8_String;
      Third_Element  : UTF8_String) return UTF8_String
   is
      function Internal (Args : Interfaces.C.Strings.chars_ptr_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (First_Element);
      Second : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Second_Element);
      Third  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Third_Element);
      Ptr    : constant Interfaces.C.Strings.chars_ptr :=
                 Internal
                   ((0 =>
                       Interfaces.C.Strings.To_Chars_Ptr (First'Unchecked_Access),
                     1 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Second'Unchecked_Access),
                     2 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Third'Unchecked_Access),
                     3 => Interfaces.C.Strings.Null_Ptr));
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Build_Filename;

   function Build_Filename
     (First_Element  : UTF8_String;
      Second_Element : UTF8_String;
      Third_Element  : UTF8_String;
      Fourth_Element : UTF8_String) return UTF8_String
   is
      function Internal (Args : Interfaces.C.Strings.chars_ptr_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (First_Element);
      Second : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Second_Element);
      Third  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Third_Element);
      Fourth : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Fourth_Element);
      Ptr    : constant Interfaces.C.Strings.chars_ptr :=
                 Internal
                   ((0 =>
                       Interfaces.C.Strings.To_Chars_Ptr (First'Unchecked_Access),
                     1 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Second'Unchecked_Access),
                     2 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Third'Unchecked_Access),
                     3 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Fourth'Unchecked_Access),
                     4 => Interfaces.C.Strings.Null_Ptr));
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Build_Filename;

   function Build_Filename
     (First_Element  : UTF8_String;
      Second_Element : UTF8_String;
      Third_Element  : UTF8_String;
      Fourth_Element : UTF8_String;
      Fifth_Element  : UTF8_String) return UTF8_String
   is
      function Internal (Args : Interfaces.C.Strings.chars_ptr_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (First_Element);
      Second : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Second_Element);
      Third  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Third_Element);
      Fourth : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Fourth_Element);
      Fifth  : aliased Interfaces.C.char_array :=
                 Interfaces.C.To_C (Fifth_Element);
      Ptr    : constant Interfaces.C.Strings.chars_ptr :=
                 Internal
                   ((0 =>
                       Interfaces.C.Strings.To_Chars_Ptr (First'Unchecked_Access),
                     1 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Second'Unchecked_Access),
                     2 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Third'Unchecked_Access),
                     3 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Fourth'Unchecked_Access),
                     4 =>
                       Interfaces.C.Strings.To_Chars_Ptr (Fifth'Unchecked_Access),
                     5 => Interfaces.C.Strings.Null_Ptr));
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Build_Filename;

   procedure Cell_Pixbuf
     (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data   : Gint)
   is
      Value : Glib.Values.GValue;
      Width : Gint;
   begin
      Gtk.Tree_Model.Get_Value (Model, Iter, Data, Value);
      Width := Glib.Properties.Get_Property (Cell, Glib.Properties.Build ("stock-size"));
      declare
         Name : constant String := Glib.Values.Get_String (Value);
      begin
         Glib.Values.Unset (Value);
         declare
            Address : System.Address;
            Set     : Gtk.Icon_Set.Gtk_Icon_Set;
            use type System.Address;
         begin
            if Stock_Icons /= No_Table then
               Address := Look_Up (Stock_Icons, Interfaces.C.To_C (Name));
               if Address /= System.Null_Address then
                  Set.Set_Object (Address);
                  Glib.Properties.Set_Property
                    (Cell,
                     Glib.Properties.Property_Object'
                       (Glib.Properties.Build ("pixbuf")),
                     Set.Render_Icon_Pixbuf
                       (Gtk.Style_Context.Get_Style_Context
                            (Column.all.Get_Tree_View),
                        +Width));
                  return;
               end if;
            end if;
         end;
         declare
            Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
            Default : constant Gtk.Icon_Theme.Gtk_Icon_Theme :=
                        Gtk.Icon_Theme.Get_Default;
            use type Gtk.Icon_Theme.Gtk_Icon_Theme;
            use type Gdk.Pixbuf.Gdk_Pixbuf;
         begin
            if Default /= null then
               Icon :=
                 Default.all.Load_Icon (Name,
                                        Width,
                                        Gtk.Icon_Theme.Icon_Lookup_Use_Builtin,
                                        null);
               if Icon /= null then
                  Glib.Properties.Set_Property
                    (Cell,
                     Glib.Properties.Property_Object'
                       (Glib.Properties.Build ("pixbuf")),
                     Icon);
                  return;
               end if;
            end if;
         end;
      end;
      Glib.Properties.Set_Property
        (Cell,
         Glib.Properties.Property_String'(Glib.Properties.Build ("icon-name")),
         "");
   exception
      when Error : others =>
         Glib.Messages.Log
           (GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Cell_Pixbuf"));
   end Cell_Pixbuf;

   procedure Check (Context : Cairo.Cairo_Context) is
      Error : Cairo.Cairo_Status;
      use type Cairo.Cairo_Status;
   begin
      Error := Cairo.Status (Context);
      if Error /= Cairo.Cairo_Status_Success then
         raise Ada.IO_Exceptions.Status_Error with To_String (Error);
      end if;
   end Check;

   function Compare
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Row_Order
   is
      pragma Unreferenced (Model);
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         if B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            return Equal;
         else
            return Before;
         end if;
      else
         if B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            return After;
         elsif Gtk.Tree_Model.Get_Depth (A) = 0 then
            if Gtk.Tree_Model.Get_Depth (B) = 0 then
               return Equal;
            else
               return Before;
            end if;
         elsif Gtk.Tree_Model.Get_Depth (B) = 0 then
            return After;
         else
            case Gtk.Tree_Model.Compare (A, B) is
               when Gint'First .. -1 => return Before;
               when 0                => return Equal;
               when 1 .. Gint'Last   => return After;
            end case;
         end if;
      end if;
   end Compare;

   function Compare
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Row_Order
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if A = Gtk.Tree_Model.Null_Iter then
         if B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            return Equal;
         else
            return Before;
         end if;
      else
         declare
            First  : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                       Gtk.Tree_Model.Get_Path (Model, A);
            Result : constant Row_Order := Compare (Model, First, B);
         begin
            Gtk.Tree_Model.Path_Free (First);
            return Result;
         end;
      end if;
   end Compare;

   function Compare
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Row_Order
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if B = Gtk.Tree_Model.Null_Iter then
         if A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            return Equal;
         else
            return After;
         end if;
      else
         declare
            Second : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                       Gtk.Tree_Model.Get_Path (Model, B);
            Result : constant Row_Order := Compare (Model, A, Second);
         begin
            Gtk.Tree_Model.Path_Free (Second);
            return Result;
         end;
      end if;
   end Compare;

   function Compare
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Row_Order
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if A = Gtk.Tree_Model.Null_Iter then
         if B = Gtk.Tree_Model.Null_Iter then
            return Equal;
         else
            return Before;
         end if;
      else
         if B = Gtk.Tree_Model.Null_Iter then
            return After;
         else
            declare
               First  : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                          Gtk.Tree_Model.Get_Path (Model, A);
               Second : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                          Gtk.Tree_Model.Get_Path (Model, B);
               Result : constant Row_Order :=
                          Compare (Model, First, Second);
            begin
               Gtk.Tree_Model.Path_Free (First);
               Gtk.Tree_Model.Path_Free (Second);
               return Result;
            end;
         end if;
      end if;
   end Compare;

   function Delete_Event_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event, Widget);
   begin
      return False;
   end Delete_Event_Handler;

   procedure Destroy (Data : Interfaces.C.Strings.chars_ptr) is
      Ptr : Interfaces.C.Strings.chars_ptr := Data;
   begin
      Interfaces.C.Strings.Free (Ptr);
      pragma Unreferenced (Ptr);
   end Destroy;

   procedure Destroy_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   procedure Dir_Close (Dir : in out GDir) is
      procedure Internal (Dir : GDir);
      pragma Import (C, Internal, "g_dir_close");
   begin
      if Dir /= null then
         Internal (Dir);
         Dir := null;
      end if;
   end Dir_Close;

   procedure Dir_Open
     (Path  : UTF8_String;
      Dir   : out GDir;
      Error : out Glib.Error.GError)
   is
      Code : aliased Glib.Error.GError;
   begin
      Dir := G_Dir_Open (Interfaces.C.To_C (Path), 0, Code'Access);
      if Dir = null then
         Error := Code;
      else
         Error := null;
      end if;
   end Dir_Open;

   function Dir_Read_Name (Dir : GDir) return UTF8_String
   is
      Ptr : constant Interfaces.C.Strings.chars_ptr := G_Dir_Read_Name (Dir);
      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         raise Ada.IO_Exceptions.End_Error;
      else
         return Interfaces.C.Strings.Value (Ptr);
      end if;
   end Dir_Read_Name;

   procedure Erase
     (Container : not null access Gtk.Container.Gtk_Container_Record'Class)
   is
      List : Gtk.Widget.Widget_List.Glist :=
               Gtk.Container.Get_Children (Container);
      This : Gtk.Widget.Widget_List.Glist :=
               Gtk.Widget.Widget_List.First (List);
      use type Gtk.Widget.Widget_List.Glist;
   begin
      while This /= Gtk.Widget.Widget_List.Null_List loop
         Gtk.Container.Remove (Container,
                               Gtk.Widget.Widget_List.Get_Data (This));
         This := Gtk.Widget.Widget_List.Next (This);
      end loop;
      Gtk.Widget.Widget_List.Free (List);
      pragma Unreferenced (List);
   end Erase;

   function File_Test
     (File_Name : UTF8_String;
      Flags     : GFileTest) return Boolean is
   begin
      return 0 /= G_File_Test (Interfaces.C.To_C (File_Name), Flags);
   end File_Test;

   function File_Test (File_Name : UTF8_String) return GFileTest is
      Name : constant Interfaces.C.char_array := Interfaces.C.To_C (File_Name);
   begin
      if 0 = G_File_Test (Name, File_Test_Exists) then
         return 0;
      elsif 0 /= G_File_Test (Name, File_Test_Is_Dir) then
         return File_Test_Exists or File_Test_Is_Dir;
      elsif 0 /= G_File_Test (Name, File_Test_Is_Executable) then
         return File_Test_Exists or File_Test_Is_Executable;
      elsif 0 /= G_File_Test (Name, File_Test_Is_Symlink) then
         return File_Test_Exists or File_Test_Is_Symlink;
      else
         return File_Test_Exists or File_Test_Is_Regular;
      end if;
   end File_Test;

   protected Wait_Cursors_Count is
      procedure Increment;
      procedure Decrement (Current : out Natural);
      function Current return Natural;
   private
      Wait_Cursors : Natural := 0;
   end Wait_Cursors_Count;

   protected body Wait_Cursors_Count is
      function Current return Natural is (Wait_Cursors);

      procedure Decrement (Current : out Natural) is
      begin
         Wait_Cursors := Wait_Cursors - 1;
         Current := Wait_Cursors;
      end Decrement;

      procedure Increment is
      begin
         Wait_Cursors := Wait_Cursors + 1;
      end Increment;
   end Wait_Cursors_Count;

   overriding procedure Finalize (Cursor : in out Wait_Cursor) is
      procedure Unref (Self : Gdk.Gdk_Window);
      pragma Import (C, Unref, "g_object_unref");
      Count : Natural;
   begin
      if Cursor.Realized then
         Wait_Cursors_Count.Decrement (Current => Count);
         if Count = 0 then
            Gdk.Window.Set_Cursor (Cursor.Window, null);
            Unref (Cursor.Window);
         end if;
      end if;
   end Finalize;

   function Find_Program_In_Path (Program : UTF8_String) return UTF8_String
   is
      Ptr : Interfaces.C.Strings.chars_ptr;
      use type Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := G_Find_Program_In_Path_UTF8 (Interfaces.C.To_C (Program));
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
            Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
         begin
            G_Free (Ptr);
            return Result;
         end;
      end if;
   end Find_Program_In_Path;

   function Find_Property
     (Class : GObject_Class;
      Name  : UTF8_String) return Param_Spec
   is
      function Internal
        (Class : GObject_Class;
         Name  : Interfaces.C.char_array) return Param_Spec;
      pragma Import (C, Internal, "g_object_class_find_property");
   begin
      return Internal (Class, Interfaces.C.To_C (Name));
   end Find_Property;

   function Find_Property
     (Object : not null access GObject_Record'Class;
      Name   : UTF8_String) return Param_Spec
   is
      Class : GObject_Class;
   begin
      Class := Glib.Types.Class_Peek (Get_Type (Object));
      if Class = Null_GObject_Class then
         return null;
      else
         return Find_Property (Class, Name);
      end if;
   end Find_Property;

   procedure Freeze_Notify
     (Object : not null access GObject_Record'Class)
   is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "g_object_freeze_notify");
   begin
      Internal (Get_Object (Object));
   end Freeze_Notify;

   function From_RGBA (Color : Gdk.RGBA.Gdk_RGBA) return Gdk.Color.Gdk_Color is
   begin
      return RGB (Color.Red, Color.Green, Color.Blue);
   end From_RGBA;

   function Get
     (Store  : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint) return String
   is
      Data : Glib.Values.GValue;
   begin
      Store.all.Get_Value (Row, Column, Data);
      declare
         Result : constant String := Glib.Values.Get_String (Data);
      begin
         Glib.Values.Unset (Data);
         return Result;
      end;
   exception
      when others =>
         return "";
   end Get;

   function Get
     (Store  : Gtk.Tree_Model.Gtk_Tree_Model;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint) return String
   is
      Data : Glib.Values.GValue;
   begin
      Gtk.Tree_Model.Get_Value (Store, Row, Column, Data);
      declare
         Result : constant String := Glib.Values.Get_String (Data);
      begin
         Glib.Values.Unset (Data);
         return Result;
      end;
   exception
      when others =>
         return "";
   end Get;

   function Get
     (Store  : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint) return String
   is
      Data : Glib.Values.GValue;
   begin
      Store.all.Get_Value (Row, Column, Data);
      declare
         Result : constant String := Glib.Values.Get_String (Data);
      begin
         Glib.Values.Unset (Data);
         return Result;
      end;
   exception
      when others =>
         return "";
   end Get;

   function Get_Application_Name return UTF8_String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_get_application_name");
   begin
      return Interfaces.C.Strings.Value (Internal);
   end Get_Application_Name;

   function Get_Background_Area
     (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Path      : Gtk.Tree_Model.Gtk_Tree_Path;
      Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column := null)
      return Gdk.Rectangle.Gdk_Rectangle
   is
      procedure Internal
        (Tree_View : System.Address;
         Path      : System.Address;
         Column    : System.Address;
         Rect      : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_background_area");
      Result : Gdk.Rectangle.Gdk_Rectangle;
      use type Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   begin
      if Column = null then
         Internal
           (Get_Object (Tree_View),
            Get_Object (Path),
            System.Null_Address,
            Result);
      else
         Internal
           (Get_Object (Tree_View),
            Get_Object (Path),
            Get_Object (Column),
            Result);
      end if;
      return Result;
   end Get_Background_Area;

   function Get_Background_Color
     (Context : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
      State   : Gtk.Enums.Gtk_State_Flags) return Gdk.RGBA.Gdk_RGBA
   is
      type Gdk_RGBA_Ptr is access all Gdk.RGBA.Gdk_RGBA;
      pragma Convention (C, Gdk_RGBA_Ptr);

      procedure Free (Ptr : Gdk_RGBA_Ptr);
      pragma Import (C, Free, "gdk_rgba_free");

      procedure Internal
        (Style : System.Address;
         State : Gtk.Enums.Gtk_State_Flags;
         Name  : Interfaces.C.char_array;
         Color : out Gdk_RGBA_Ptr;
         Nil   : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gtk_style_context_get");
      Color : Gdk.RGBA.Gdk_RGBA := (others => 0.0);
      Ptr   : Gdk_RGBA_Ptr;
   begin
      Internal
        (Get_Object (Context),
         State,
         Interfaces.C.To_C ("background-color"),
         Ptr);
      if Ptr /= null then
         Color := Ptr.all;
         Free (Ptr);
      end if;
      return Color;
   end Get_Background_Color;

   function Get_Basename (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : Interfaces.C.char_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_path_get_basename");
      Ptr    : constant Interfaces.C.Strings.chars_ptr :=
                 Internal (Interfaces.C.To_C (File_Name));
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Get_Basename;

   function Get_Clip_Rectangle (Context : Cairo.Cairo_Context)
                                return Boolean is
      function Internal
        (Context   : Cairo.Cairo_Context;
         Rectangle : System.Address := System.Null_Address) return Gboolean;
      pragma Import (C, Internal, "gdk_cairo_get_clip_rectangle");
   begin
      return 0 /= Internal (Context);
   end Get_Clip_Rectangle;

   procedure Get_Clip_Rectangle
     (Context   : Cairo.Cairo_Context;
      Rectangle : out Gdk.Rectangle.Gdk_Rectangle;
      Empty     : out Boolean)
   is
      function Internal
        (Context   : Cairo.Cairo_Context;
         Rectangle : access Gdk.Rectangle.Gdk_Rectangle) return Gboolean;
      pragma Import (C, Internal, "gdk_cairo_get_clip_rectangle");
      Area : aliased Gdk.Rectangle.Gdk_Rectangle;
   begin
      Empty := 0 = Internal (Context, Area'Access);
      if not Empty then
         Rectangle := Area;
      end if;
   end Get_Clip_Rectangle;

   function Get_Column
     (Value : Glib.Values.GValue) return Gtk.Tree_View_Column.Gtk_Tree_View_Column
   is
      Stub : Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record;
      Data : constant System.Address := Glib.Values.Get_Address (Value);
      use type System.Address;
   begin
      if Data = System.Null_Address then
         return null;
      else
         return
           Gtk.Tree_View_Column.Gtk_Tree_View_Column
             (Get_User_Data_Fast (Data, Stub));
      end if;
   end Get_Column;

   function Get_Column_No
     (Tree_View : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Column    : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class) return Gint
   is
      This : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      That : constant System.Address := Get_Object (Column);
      use type System.Address;
      use type Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   begin
      for Index in 0 .. Gint'Last loop
         This := Gtk.Tree_View.Get_Column (Tree_View, Index);
         exit when This = null;
         if Get_Object (This) = That then
            return Index;
         end if;
      end loop;
      return -1;
   end Get_Column_No;

   function Get_Current_Dir return UTF8_String is
      Ptr : Interfaces.C.Strings.chars_ptr;
   begin
      Ptr := G_Get_Current_Dir_UTF8;
      declare
         Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
      begin
         G_Free (Ptr);
         return Result;
      end;
   end Get_Current_Dir;

   function Get_Dirname (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : Interfaces.C.char_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_path_get_dirname");
      Ptr    : constant Interfaces.C.Strings.chars_ptr   :=
                 Internal (Interfaces.C.To_C (File_Name));
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Get_Dirname;

   function Get_PRGName return UTF8_String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_get_prgname");
   begin
      return Interfaces.C.Strings.Value (Internal);
   end Get_PRGName;

   function Get_Root (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : Interfaces.C.char_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_path_skip_root");
      Name : Interfaces.C.char_array := Interfaces.C.To_C (File_Name);
      Ptr  : constant Interfaces.C.Strings.chars_ptr := Internal (Name);
      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         raise Ada.IO_Exceptions.Use_Error;
      else
         declare
            Length : constant Interfaces.C.size_t :=
                       Interfaces.C.Strings.Strlen (Ptr);
            use type Interfaces.C.size_t;
         begin
            if Length > 0 then
               Name (Name'Last - Length) := Interfaces.C.nul;
               return Interfaces.C.To_Ada (Name);
            else
               return File_Name;
            end if;
         end;
      end if;
   end Get_Root;

   function Get_Row_No
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Gint
   is
      pragma Unreferenced (Model);
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return -1;
      else
         declare
            Indices : Gint_Array renames Gtk.Tree_Model.Get_Indices (Path);
         begin
            if Indices'Length > 0 then
               return Indices (Indices'Last);
            else
               return -1;
            end if;
         end;
      end if;
   end Get_Row_No;

   function Get_Row_No
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
               Gtk.Tree_Model.Get_Path (Model, Iter);
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return -1;
      else
         declare
            Indices : Gint_Array renames Gtk.Tree_Model.Get_Indices (Path);
            Result  : Gint;
         begin
            if Indices'Length > 0 then
               Result := Indices (Indices'Last);
            else
               Result := -1;
            end if;
            Gtk.Tree_Model.Path_Free (Path);
            return Result;
         end;
      end if;
   end Get_Row_No;

   procedure Get_Screen_Position
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : out Gint;
      Y      : out Gint)
   is
      Area : Gtk.Widget.Gtk_Allocation;
   begin
      Widget.all.Get_Allocation (Area);
      Gdk.Window.Get_Origin (Widget.all.Get_Window, X, Y);
      X := X + Area.X;
      Y := Y + Area.Y;
   end Get_Screen_Position;

   function Get_User_Special_Dir (Directory : User_Directory)
                                  return UTF8_String is
      function Internal (Directory : Interfaces.C.int)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_get_user_special_dir");
   begin
      return
        Interfaces.C.Strings.Value (Internal (User_Directory'Pos (Directory)));
   end Get_User_Special_Dir;

   procedure Get_Visible_Range
     (Tree_View  : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Start_Path : out Gtk.Tree_Model.Gtk_Tree_Path;
      End_Path   : out Gtk.Tree_Model.Gtk_Tree_Path)
   is
      function Internal
        (Tree_View : System.Address;
         From_Path : System.Address;
         To_Path   : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_visible_range");
      From_Path : constant System.Address := System.Null_Address;
      To_Path   : constant System.Address := System.Null_Address;
      Result    : Gboolean;
   begin
      Result := Internal (Get_Object (Tree_View), From_Path, To_Path);
      pragma Unreferenced (Result);
      Set_Object (Start_Path, From_Path);
      Set_Object (End_Path,   To_Path);
   end Get_Visible_Range;

   overriding procedure Initialize (Cursor : in out Wait_Cursor) is
      Clock  : Gdk.Gdk_Cursor;
      procedure Ref (Self : Gdk.Gdk_Window);
      pragma Import (C, Ref, "g_object_ref");
   begin
      Cursor.Realized := Gtk.Widget.Get_Realized (Cursor.Widget);
      if Cursor.Realized then
         if Wait_Cursors_Count.Current = 0 then
            Cursor.Window := Gtk.Widget.Get_Window (Cursor.Widget);
            Ref (Cursor.Window);
            Gdk.Cursor.Gdk_New (Clock, Gdk.Cursor.Watch);
            Gdk.Window.Set_Cursor (Cursor.Window, Clock);
            Gdk.Cursor.Unref (Clock);
            declare
               Active : Boolean;
            begin
               while Gtk.Main.Events_Pending loop -- Pump the events queue
                  Active := Gtk.Main.Main_Iteration;
               end loop;
               pragma Unreferenced (Active);
            end;
         end if;
         Wait_Cursors_Count.Increment;
      end if;
   end Initialize;

   procedure Insert_Alt
     (Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Text   : UTF8_String)
   is
      procedure Internal
        (Buffer : System.Address;
         Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
         Text   : Interfaces.C.char_array;
         Len    : Gint);
      pragma Import (C, Internal, "gtk_text_buffer_insert");
   begin
      Internal
        (Get_Object (Buffer),
         Iter,
         Interfaces.C.To_C (Text),
         Text'Length);
   end Insert_Alt;

   function Is_A (Derived, Ancestor : GType) return Boolean is
      function Internal (Derived, Ancestor : GType) return Gboolean;
      pragma Import (C, Internal, "g_type_is_a");
   begin
      return Internal (Derived, Ancestor) /= 0;
   end Is_A;

   function Is_Absolute (File_Name : UTF8_String) return Boolean is
      function Internal (File_Name : Interfaces.C.char_array) return Gboolean;
      pragma Import (C, Internal, "g_path_is_absolute");
   begin
      return Internal (Interfaces.C.To_C (File_Name)) /= 0;
   end Is_Absolute;

   function Is_In
     (Container : not null access Gtk.Container.Gtk_Container_Record'Class;
      Element   : not null access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
   is
      Data : aliased Search_Data := (Element.all'Unchecked_Access,
                                     False);
   begin
      For_Test.Foreach
        (Gtk.Container.Gtk_Container_Record (Container.all)'Unchecked_Access,
         Test'Access,
         Data'Unchecked_Access);
      return Data.Found;
   end Is_In;

   function Is_In
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      pragma Unreferenced (Model);
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return False;
      elsif B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return True;
      elsif Gtk.Tree_Model.Get_Depth (A) = 0 then
         return False;
      elsif Gtk.Tree_Model.Get_Depth (B) = 0 then
         return True;
      else
         return Gtk.Tree_Model.Is_Descendant (A, B);
      end if;
   end Is_In;

   function Is_In
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if A = Gtk.Tree_Model.Null_Iter then
         return False;
      elsif B = Gtk.Tree_Model.Null_Iter then
         return True;
      else
         declare
            First  : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                       Gtk.Tree_Model.Get_Path (Model, A);
            Second : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                       Gtk.Tree_Model.Get_Path (Model, B);
            Result : constant Boolean :=
                       Gtk.Tree_Model.Is_Descendant (First, Second);
         begin
            Gtk.Tree_Model.Path_Free (First);
            Gtk.Tree_Model.Path_Free (Second);
            return Result;
         end;
      end if;
   end Is_In;

   function Is_In
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if A = Gtk.Tree_Model.Null_Iter then
         return False;
      elsif B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return True;
      elsif Gtk.Tree_Model.Get_Depth (B) = 0 then
         return True;
      else
         declare
            First  : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                       Gtk.Tree_Model.Get_Path (Model, A);
            Result : constant Boolean :=
                       Gtk.Tree_Model.Is_Descendant (First, B);
         begin
            Gtk.Tree_Model.Path_Free (First);
            return Result;
         end;
      end if;
   end Is_In;

   function Is_In
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return False;
      elsif B = Gtk.Tree_Model.Null_Iter then
         return True;
      elsif Gtk.Tree_Model.Get_Depth (A) = 0 then
         return False;
      else
         declare
            Second : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                       Gtk.Tree_Model.Get_Path (Model, B);
            Result : constant Boolean :=
                       Gtk.Tree_Model.Is_Descendant (A, Second);
         begin
            Gtk.Tree_Model.Path_Free (Second);
            return Result;
         end;
      end if;
   end Is_In;

   function Is_Parent
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      pragma Unreferenced (Model);
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return False;
      elsif A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return Gtk.Tree_Model.Get_Indices (B)'Length = 1;
      end if;
      declare
         A_Indices : Gint_Array renames Gtk.Tree_Model.Get_Indices (A);
         B_Indices : Gint_Array renames Gtk.Tree_Model.Get_Indices (B);
      begin
         return
           (A_Indices'Length + 1 = B_Indices'Length and then
              (A_Indices'Length = 0 or else
                   (A_Indices =
                        B_Indices (B_Indices'First .. B_Indices'Last - 1))));
      end;
   end Is_Parent;

   function Is_Parent
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return False;
      elsif A = Gtk.Tree_Model.Null_Iter then
         return Gtk.Tree_Model.Get_Indices (B)'Length = 1;
      end if;
      declare
         A_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                    Gtk.Tree_Model.Get_Path (Model, A);
         Result : constant Boolean := Is_Parent (Model, A_Path, B);
      begin
         Gtk.Tree_Model.Path_Free (A_Path);
         return Result;
      end;
   end Is_Parent;

   function Is_Parent
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if B = Gtk.Tree_Model.Null_Iter then
         return False;
      elsif A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return Gtk.Tree_Model.Parent (Model, B) = Gtk.Tree_Model.Null_Iter;
      end if;
      declare
         B_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                    Gtk.Tree_Model.Get_Path (Model, B);
         Result : constant Boolean := Is_Parent (Model, A, B_Path);
      begin
         Gtk.Tree_Model.Path_Free (B_Path);
         return Result;
      end;
   end Is_Parent;

   function Is_Parent
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if B = Gtk.Tree_Model.Null_Iter then
         return False;
      elsif A = Gtk.Tree_Model.Null_Iter then
         return True;
      else
         return Gtk.Tree_Model.Parent (Model, B) = A;
      end if;
   end Is_Parent;

   function Is_Sibling
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      pragma Unreferenced (Model);
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return A = Gtk.Tree_Model.Null_Gtk_Tree_Path;
      elsif A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return False;
      end if;
      declare
         A_Indices : Gint_Array renames Gtk.Tree_Model.Get_Indices (A);
         B_Indices : Gint_Array renames Gtk.Tree_Model.Get_Indices (B);
      begin
         return
           (A_Indices'Length = B_Indices'Length and then
              (A_Indices'Length < 2 or else
                   (A_Indices (A_Indices'First .. A_Indices'Last - 1)
                    =  B_Indices (B_Indices'First .. B_Indices'Last - 1))));
      end;
   end Is_Sibling;

   function Is_Sibling
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Path) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if B = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return A = Gtk.Tree_Model.Null_Iter;
      elsif A = Gtk.Tree_Model.Null_Iter then
         return False;
      end if;
      declare
         A_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                    Gtk.Tree_Model.Get_Path (Model, A);
         Result : constant Boolean := Is_Sibling (Model, A_Path, B);
      begin
         Gtk.Tree_Model.Path_Free (A_Path);
         return Result;
      end;
   end Is_Sibling;

   function Is_Sibling
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Path;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if B = Gtk.Tree_Model.Null_Iter then
         return A = Gtk.Tree_Model.Null_Gtk_Tree_Path;
      elsif A = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return False;
      end if;
      declare
         B_Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                    Gtk.Tree_Model.Get_Path (Model, B);
         Result : constant Boolean := Is_Sibling (Model, A, B_Path);
      begin
         Gtk.Tree_Model.Path_Free (B_Path);
         return Result;
      end;
   end Is_Sibling;

   function Is_Sibling
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if B = Gtk.Tree_Model.Null_Iter then
         return A = Gtk.Tree_Model.Null_Iter;
      elsif A = Gtk.Tree_Model.Null_Iter then
         return False;
      else
         return
           Gtk.Tree_Model.Parent (Model, B) = Gtk.Tree_Model.Parent (Model, A);
      end if;
   end Is_Sibling;

   function Keyval_To_UTF8 (Key_Val : Gdk.Types.Gdk_Key_Type)
                            return UTF8_String is
      Result : UTF8_String (1 .. 8);
      Last   : Natural;
   begin
      Glib.Unicode.Unichar_To_UTF8 (Keyval_To_Unicode (Key_Val), Result, Last);
      return Result (1 .. Last);
   end Keyval_To_UTF8;

   procedure Message_Dialog
     (Message       : UTF8_String;
      Parent        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Title         : UTF8_String                         := "";
      Mode          : UTF8_String                         := Gtk.Stock.Stock_Dialog_Error;
      Justification : Gtk.Enums.Gtk_Justification         := Gtk.Enums.Justify_Center;
      Response      : access Gtk.Dialog.Gtk_Response_Type := null)
   is
      function Get_Title return String;
      function Get_Title return String is
      begin
         if Title'Length > 0 then
            return Title;
         elsif Mode = Gtk.Stock.Stock_Dialog_Error then
            return "Error";
         elsif Mode = Gtk.Stock.Stock_Dialog_Info then
            return "Information";
         elsif Mode = Gtk.Stock.Stock_Dialog_Question then
            return "Question";
         elsif Mode = Gtk.Stock.Stock_Dialog_Warning then
            return "Warning";
         else
            return "";
         end if;
      end Get_Title;
      Dialog : Gtk.Dialog.Gtk_Dialog;
      Label  : Gtk.Label.Gtk_Label;
      Box    : Gtk.Box.Gtk_Box;
      Image  : Gtk.Image.Gtk_Image;
      Result : Gtk.Dialog.Gtk_Response_Type;
      Top    : constant Gtk.Widget.Gtk_Widget := Parent.all.Get_Toplevel;
      use type Gtk.Dialog.Gtk_Dialog_Flags;
      use type Gtk.Widget.Gtk_Widget;
   begin
      if Top = null or else Top.all not in Gtk.Window.Gtk_Window_Record'Class then
         if Response /= null then
            Response.all := Gtk.Dialog.Gtk_Response_Cancel;
         end if;
         return;
      end if;
      Gtk.Dialog.Gtk_New
        (Dialog => Dialog,
         Title  => Title,
         Flags  => Gtk.Dialog.Modal or Gtk.Dialog.Destroy_With_Parent,
         Parent =>
           Gtk.Window.Gtk_Window_Record'Class (Top.all)'Unchecked_Access);
      Dialog.all.Realize;
      Gtk.Box.Gtk_New_Hbox (Box);
      Dialog.all.Get_Content_Area.all.Pack_Start (Box, Padding => 10);
      Gtk.Image.Gtk_New (Image, Mode, Gtk.Enums.Icon_Size_Dialog);
      Box.all.Pack_Start (Image, Padding => 10);

      if Mode = Gtk.Stock.Stock_Dialog_Question then
         Add_Button_From_Stock
           (Dialog   => Dialog,
            Response => Gtk.Dialog.Gtk_Response_Yes,
            Icon     => Gtk.Stock.Stock_Yes,
            Label    => "_Yes");
         Add_Button_From_Stock
           (Dialog   => Dialog,
            Response => Gtk.Dialog.Gtk_Response_No,
            Icon     => Gtk.Stock.Stock_Cancel,
            Label    => "_No");
      else
         Add_Button_From_Stock
           (Dialog   => Dialog,
            Response => Gtk.Dialog.Gtk_Response_OK,
            Icon     => Gtk.Stock.Stock_Ok,
            Label    => "_OK");
      end if;
      Gtk.Label.Gtk_New (Label, Message);
      Label.all.Set_Selectable (True);
      Label.all.Set_Justify (Justification);
      Box.all.Pack_Start (Label, Padding => 10);
      Dialog.all.Show_All;
      Result := Dialog.all.Run;
      if Response /= null then
         Response.all := Result;
      end if;
      Glib.Object.Checked_Destroy (Dialog);
   end Message_Dialog;

   procedure Remove (File_Name : UTF8_String) is
      function Internal (File_Name : Interfaces.C.char_array) return Interfaces.C.int;
      pragma Import (C, Internal, "g_remove");
      use type Interfaces.C.int;
   begin
      if 0 /= Internal (Interfaces.C.To_C (File_Name)) then
         if File_Test (File_Name, File_Test_Exists) then
            raise Ada.IO_Exceptions.Use_Error with "File cannot be deleted";
         end if;
      end if;
   end Remove;

   procedure Rename (Old_File_Name, New_File_Name : UTF8_String) is
      function Internal (Old_File_Name, New_File_Name : Interfaces.C.char_array)
                         return Interfaces.C.int;
      pragma Import (C, Internal, "g_rename");
      use type Interfaces.C.int;
   begin
      if 0 /= Internal (Interfaces.C.To_C (Old_File_Name),
                        Interfaces.C.To_C (New_File_Name))
      then
         if File_Test (Old_File_Name, File_Test_Exists) then
            raise Ada.IO_Exceptions.Use_Error with "File cannot be renamed";
         else
            raise Ada.IO_Exceptions.Name_Error with "File does not exist";
         end if;
      end if;
   end Rename;

   function RGB (Red, Green, Blue : Gdouble) return Gdk.Color.Gdk_Color is
      Result  : Gdk.Color.Gdk_Color;
      function "+" (Value : Gdouble) return Guint16;
      function "+" (Value : Gdouble) return Guint16 is
      begin
         if Value <= 0.0 then
            return 0;
         elsif Value >= 1.0 then
            return Guint16'Last;
         else
            return Guint16 (Value * Gdouble (Guint16'Last));
         end if;
      end "+";
   begin
      Gdk.Color.Set_Rgb (Result, +Red, +Green, +Blue);
      return Result;
   end RGB;

   procedure Set
     (Store  : not null access Gtk.List_Store.Gtk_List_Store_Record'Class;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint;
      Value  : String)
   is
      Data : Glib.Values.GValue;
   begin
      Glib.Values.Init (Data, GType_String);
      Glib.Values.Set_String (Data, Value);
      Store.all.Set_Value (Row, Column, Data);
      Glib.Values.Unset (Data);
   exception
      when others =>
         Glib.Values.Unset (Data);
         raise;
   end Set;

   procedure Set
     (Store  : not null access Gtk.Tree_Store.Gtk_Tree_Store_Record'Class;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint;
      Value  : String)
   is
      Data : Glib.Values.GValue;
   begin
      Glib.Values.Init (Data, GType_String);
      Glib.Values.Set_String (Data, Value);
      Store.all.Set_Value (Row, Column, Data);
      Glib.Values.Unset (Data);
   exception
      when others =>
         Glib.Values.Unset (Data);
         raise;
   end Set;

   procedure Set (Value : in out Glib.Values.GValue; Object : GObject)
   is
      procedure Internal (Value  : in out Glib.Values.GValue;
                          Object : System.Address);
      pragma Import (C, Internal, "g_value_set_object");
   begin
      Internal (Value, Convert (Object));
   end Set;

   procedure Set_Background_Color
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Color  : Gdk.RGBA.Gdk_RGBA)
   is
      Provider : Gtk.Css_Provider.Gtk_Css_Provider;
      CSS      : constant String := "* { background-color: " &
                   Gdk.RGBA.To_String (Color) &
                 "; }";
      use Gtk.Css_Provider;
   begin
      Gtk.Css_Provider.Gtk_New (Provider);
      if Provider.all.Load_From_Data (CSS, null) then
         Gtk.Style_Context.Get_Style_Context (Widget).all.Add_Provider
           (+Provider,
            Gtk.Style_Provider.Priority_Application);
      end if;
      Provider.all.Unref;
   exception
      when Error : others =>
         Glib.Messages.Log
           (GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Set_Background_Color"));
   end Set_Background_Color;

   procedure Set_Property
     (Object : not null access Glib.Object.GObject_Record'Class;
      Name   : Glib.Properties.Property_Float;
      Value  : Glib.Gfloat);
   procedure Set_Property
     (Object : not null access Glib.Object.GObject_Record'Class;
      Name   : Glib.Properties.Property_Float;
      Value  : Glib.Gfloat)
   is
      procedure Internal
        (Object : in     System.Address;
         Name   : in     Glib.Property;
         Value  : in out Glib.Values.GValue);
      pragma Import (C, Internal, "g_object_set_property");

      Val : Glib.Values.GValue;
   begin
      Glib.Values.Init (Val, Glib.GType_Float);
      Glib.Values.Set_Float (Val, Value);
      Internal
        (Glib.Object.Get_Object (Object),
         Glib.Property (Name),
         Val);
      pragma Unreferenced (Val);
   end Set_Property;

   procedure Set_Tip
     (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Widget : System.Address;
         Text   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_widget_set_tooltip_text");
   begin
      Internal (Get_Object (Widget), Interfaces.C.Strings.Null_Ptr);
   end Set_Tip;

   function Skip_Root (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : Interfaces.C.char_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_path_skip_root");
      Name : constant Interfaces.C.char_array := Interfaces.C.To_C (File_Name);
      Ptr  : constant Interfaces.C.Strings.chars_ptr := Internal (Name);
      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         raise Ada.IO_Exceptions.Use_Error;
      else
         return Interfaces.C.Strings.Value (Ptr);
      end if;
   end Skip_Root;

   procedure Test
     (Item : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Data : Search_Data_Ptr)
   is
      use type Gtk.Widget.Gtk_Widget;
   begin
      if Data.all.Item = Item.all'Access then
         Data.all.Found := True;
      end if;
   end Test;

   procedure Thaw_Notify
     (Object : not null access GObject_Record'Class)
   is
      procedure Internal (Object : System.Address);
      pragma Import (C, Internal, "g_object_thaw_notify");
   begin
      Internal (Get_Object (Object));
   end Thaw_Notify;

   function Themed_Icon_New (Icon_Name : UTF8_String) return GObject is
      function Internal (Name : Interfaces.C.char_array) return System.Address;
      pragma Import (C, Internal, "g_themed_icon_new");
   begin
      return Convert (Internal (Interfaces.C.To_C (Icon_Name)));
   end Themed_Icon_New;

   function Themed_Icon_New_With_Default_Fallbacks
     (Icon_Name : UTF8_String) return GObject
   is
      function Internal (Name : Interfaces.C.char_array) return System.Address;
      pragma Import
        (C, Internal, "g_themed_icon_new_with_default_fallbacks");
   begin
      return Convert (Internal (Interfaces.C.To_C (Icon_Name)));
   end Themed_Icon_New_With_Default_Fallbacks;

   function To_RGBA (Color : Gdk.Color.Gdk_Color) return Gdk.RGBA.Gdk_RGBA is
   begin
      return
        (Red   => Gdouble (Gdk.Color.Red   (Color)) / Gdouble (Guint16'Last),
         Green => Gdouble (Gdk.Color.Green (Color)) / Gdouble (Guint16'Last),
         Blue  => Gdouble (Gdk.Color.Blue  (Color)) / Gdouble (Guint16'Last),
         Alpha => 1.0);
   end To_RGBA;

   function To_String (Status : Cairo.Cairo_Status) return String is
      function Internal (Status : Cairo.Cairo_Status)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "cairo_status_to_string");
      Result : Interfaces.C.Strings.chars_ptr;
      use type Interfaces.C.Strings.chars_ptr;
   begin
      Result := Internal (Status);
      if Result /= Interfaces.C.Strings.Null_Ptr then
         return Interfaces.C.Strings.Value (Result);
      else
         return "";
      end if;
   end To_String;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Missed." & Name;
   end Where;

begin
   --
   -- Blocking   annoying Windows' popup  error  boxes.   Most   notably
   -- "please insert disk". Under Linux SetErrorMode is routed to a null
   -- implementation   using a   weak  reference   implementation   from
   -- Glib.Wrappers.
   --
   declare
      function SetErrorMode (Mode : Interfaces.Unsigned_32)
                             return Interfaces.Unsigned_32;
      pragma Import (Stdcall, SetErrorMode, "SetErrorMode");
      --
      -- The  system does not display the critical-error-handler message
      -- box. Instead,  the  system  sends  the  error  to  the  calling
      -- process.
      SEM_FAILCRITICALERRORS : constant := 16#0001#;
      --
      -- The  system  automatically  fixes  memory  alignment faults and
      -- makes  them  invisible to the application. It does this for the
      -- calling  process  and any descendant processes. This feature is
      -- only supported by certain  processor  architectures.  For  more
      -- information, see the Remarks section. After this value  is  set
      -- for a process, subsequent  attempts  to  clear  the  value  are
      -- ignored.
      --
      SEM_NOALIGNMENTFAULTEXCEPT : constant := 16#0004#;
      --
      -- The system does not display the Windows Error Reporting dialog.
      --
      SEM_NOGPFAULTERRORBOX : constant := 16#0002#;
      --
      -- The system does not display a message box when it fails to find
      -- a file. Instead, the error is returned to the calling process.
      --
      SEM_NOOPENFILEERRORBOX : constant := 16#8000#;

      Result : Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_32;
   begin
      Result := SetErrorMode (SEM_NOOPENFILEERRORBOX or
                              SEM_NOGPFAULTERRORBOX  or
                                  SEM_FAILCRITICALERRORS);
      pragma Unreferenced (Result);
   end;
end Gtk.Missed;
