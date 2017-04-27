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
with Gdk.Cursor;            use Gdk.Cursor;
with Gdk.Window;            use Gdk.Window;
with Glib.Messages;         use Glib.Messages;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Glib.Unicode;          use Glib.Unicode;
with Gtk.Main;              use Gtk.Main;
with Gtk.Box;               use Gtk.Box;
with Gtk.Css_Provider;      use Gtk.Css_Provider;
with Gtk.Icon_Source;       use Gtk.Icon_Source;
with Gtk.Icon_Set;          use Gtk.Icon_Set;
with Gtk.Icon_Theme;        use Gtk.Icon_Theme;
with Gtk.Image;             use Gtk.Image;
with Gtk.Label;             use Gtk.Label;
with Gtk.Style_Provider;    use Gtk.Style_Provider;
with Gtk.Window;
with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;                use System;

with Ada.Unchecked_Conversion;
with Glib.Object.Checked_Destroy;

package body Gtk.Missed is
   use Gtk.Cell_Renderer;

   procedure Destroy (Data : chars_ptr);
   pragma Convention (C, Destroy);

   procedure Destroy (Data : chars_ptr) is
      Ptr : chars_ptr := Data;
   begin
      Free (Ptr);
   end Destroy;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Missed." & Name;
   end Where;

   procedure G_Free (Border : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, G_Free, "g_free");
------------------------------------------------------------------------
   type GHashTable is new System.Address;
   No_Table : constant GHashTable := GHashTable (Null_Address);

   type GEqualFunc is access function (A, B : char_array)
      return Gboolean;
   pragma Convention (C, GEqualFunc);

   type GHashFunc is access function (Key : char_array) return Gint;
   pragma Convention (C, GHashFunc);

   type GDestroyNotify is access procedure (Data : chars_ptr);
   pragma Convention (C, GDestroyNotify);

   function Str_Hash (Key : char_array) return Gint;
   pragma Import (C, Str_Hash, "g_str_hash");

   function Str_Equal (A, B : char_array) return Gboolean;
   pragma Import (C, Str_Equal, "g_str_equal");

   function Table_New
     (Hash          : GHashFunc      := Str_Hash'Access;
      Equal         : GEqualFunc     := Str_Equal'Access;
      Key_Destroy   : GDestroyNotify := Destroy'Access;
      Value_Destroy : System.Address := System.Null_Address) return GHashTable;
   pragma Import (C, Table_New, "g_hash_table_new_full");

   function Insert (Table : GHashTable;
                    Key   : chars_ptr;
                    Value : System.Address) return Gboolean;
   pragma Import (C, Insert, "g_hash_table_insert");

   function Look_Up (Table : GHashTable;
                     Key   : char_array) return System.Address;
   pragma Import (C, Look_Up, "g_hash_table_lookup");
------------------------------------------------------------------------
   Stock_Icons : GHashTable := No_Table;

   function "+" (Width : Gint) return Gtk_Icon_Size is
   begin
      if Width <= 16 then
         return Icon_Size_Menu;
      elsif Width <= 18 then
         return Icon_Size_Small_Toolbar;
      elsif Width <= 24 then
         return Icon_Size_Large_Toolbar;
      elsif Width <= 32 then
         return Icon_Size_Dnd;
      else
         return Icon_Size_Dialog;
      end if;
   end "+";

   procedure Add_Button_From_Stock
     (Dialog     : not null access Gtk_Dialog_Record'Class;
      Response   : Gtk_Response_Type;
      Label      : UTF8_String   := "";
      Icon       : UTF8_String   := "";
      Icon_Left  : Boolean       := True;
      Size       : Gtk_Icon_Size := Icon_Size_Button;
      Spacing    : Guint         := 3;
      Tip        : UTF8_String   := "";
      Relief     : Gtk_Relief_Style := Relief_Normal)
   is
      Button : Gtk_Button;
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
     (Dialog     : not null access Gtk_Dialog_Record'Class;
      Response   : Gtk_Response_Type;
      Label      : UTF8_String   := "";
      Icon       : UTF8_String   := "";
      Icon_Left  : Boolean       := True;
      Size       : Gtk_Icon_Size := Icon_Size_Button;
      Spacing    : Guint         := 3;
      Tip        : UTF8_String   := "";
      Relief     : Gtk_Relief_Style := Relief_Normal) return Gtk_Button
   is
      Button : Gtk_Button;
      Box    : Gtk_Hbox;
      Text   : Gtk_Label;
      Image  : Gtk_Image;
   begin
      Gtk_New (Button);
      Gtk_New_Hbox (Box, False, 0);
      Box.all.Set_Border_Width (0);
      Box.all.Set_Spacing (Gint (Spacing));
      Button.all.Add (Box);
      Button.all.Set_Relief (Relief);
      if Icon_Left then
         if Icon'Length > 0 then
            Gtk_New (Image, Icon, Size);
            Box.all.Pack_Start (Image, False, False);
         end if;
         if Label'Length > 0 then
            Gtk_New_With_Mnemonic (Text, Label);
            Box.all.Pack_Start (Text, False, False);
         end if;
      else
         if Label'Length > 0 then
            Gtk_New_With_Mnemonic (Text, Label);
            Box.all.Pack_Start (Text, False, False);
         end if;
         if Icon'Length > 0 then
            Gtk_New (Image, Icon, Size);
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
                        Icon : Gdk_Pixbuf)
   is
      Key    : chars_ptr;
      Set    : Gtk_Icon_Set;
      Result : Gboolean;
   begin
      Key := New_String (Name);
      if Stock_Icons = No_Table then
         Stock_Icons := Table_New;
         Gtk_New (Set);
         Result := Insert (Stock_Icons, Key, Set.Get_Object);
      else
         declare
            Address : constant System.Address :=
                      Look_Up (Stock_Icons, Value (Key));
         begin
            if Address = Null_Address then
               Gtk_New (Set);
               Result := Insert (Stock_Icons, Key, Set.Get_Object);
            else
               Set.Set_Object (Address);
               Free (Key);
            end if;
         end;
      end if;
      pragma Unreferenced (Result);
      declare
         Source : Gtk_Icon_Source;
      begin
         Gtk_New (Source);
         Set_Pixbuf (Source, Icon);
         Source.Set_Size (+Get_Width (Icon));
         Source.Set_Size_Wildcarded (False);
         Set.Add_Source (Source);
         Free (Source);
      end;
   end Add_Named;

   function Build_Filename (First_Element, Second_Element : UTF8_String)
      return UTF8_String is
      function Internal (Args : chars_ptr_array) return chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased char_array := To_C (First_Element);
      Second : aliased char_array := To_C (Second_Element);
      Ptr    : constant chars_ptr :=
                 Internal
                   ((0 => To_Chars_Ptr (First'Unchecked_Access),
                     1 => To_Chars_Ptr (Second'Unchecked_Access),
                     2 => Null_Ptr));
      Result : constant UTF8_String := Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Build_Filename;

   function Build_Filename
     (First_Element  : UTF8_String;
      Second_Element : UTF8_String;
      Third_Element  : UTF8_String) return UTF8_String
   is
      function Internal (Args : chars_ptr_array) return chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased char_array := To_C (First_Element);
      Second : aliased char_array := To_C (Second_Element);
      Third  : aliased char_array := To_C (Third_Element);
      Ptr    : constant chars_ptr :=
                  Internal
                   ((0 => To_Chars_Ptr (First'Unchecked_Access),
                     1 => To_Chars_Ptr (Second'Unchecked_Access),
                     2 => To_Chars_Ptr (Third'Unchecked_Access),
                     3 => Null_Ptr));
      Result : constant UTF8_String := Value (Ptr);
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
      function Internal (Args : chars_ptr_array) return chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased char_array := To_C (First_Element);
      Second : aliased char_array := To_C (Second_Element);
      Third  : aliased char_array := To_C (Third_Element);
      Fourth : aliased char_array := To_C (Fourth_Element);
      Ptr    : constant chars_ptr :=
                  Internal
                   ((0 => To_Chars_Ptr (First'Unchecked_Access),
                     1 => To_Chars_Ptr (Second'Unchecked_Access),
                     2 => To_Chars_Ptr (Third'Unchecked_Access),
                     3 => To_Chars_Ptr (Fourth'Unchecked_Access),
                     4 => Null_Ptr));
      Result : constant UTF8_String := Value (Ptr);
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
      function Internal (Args : chars_ptr_array) return chars_ptr;
      pragma Import (C, Internal, "g_build_filenamev");
      First  : aliased char_array := To_C (First_Element);
      Second : aliased char_array := To_C (Second_Element);
      Third  : aliased char_array := To_C (Third_Element);
      Fourth : aliased char_array := To_C (Fourth_Element);
      Fifth  : aliased char_array := To_C (Fifth_Element);
      Ptr    : constant chars_ptr :=
                  Internal
                   ((0 => To_Chars_Ptr (First'Unchecked_Access),
                     1 => To_Chars_Ptr (Second'Unchecked_Access),
                     2 => To_Chars_Ptr (Third'Unchecked_Access),
                     3 => To_Chars_Ptr (Fourth'Unchecked_Access),
                     4 => To_Chars_Ptr (Fifth'Unchecked_Access),
                     5 => Null_Ptr));
      Result : constant UTF8_String := Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Build_Filename;

   procedure Check (Context : Cairo_Context) is
      Error : Cairo_Status;
   begin
      Error := Status (Context);
      if Error /= Cairo_Status_Success then
         raise Ada.IO_Exceptions.Status_Error with To_String (Error);
      end if;
   end Check;

   function Compare
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Path;
      B     : Gtk_Tree_Path) return Row_Order
   is
      pragma Unreferenced (Model);
   begin
      if A = Null_Gtk_Tree_Path then
         if B = Null_Gtk_Tree_Path then
            return Equal;
         else
            return Before;
         end if;
      else
         if B = Null_Gtk_Tree_Path then
            return After;
         elsif Get_Depth (A) = 0 then
            if Get_Depth (B) = 0 then
               return Equal;
            else
               return Before;
            end if;
         elsif Get_Depth (B) = 0 then
            return After;
         else
            case Compare (A, B) is
               when Gint'First .. -1 => return Before;
               when 0                => return Equal;
               when 1 .. Gint'Last   => return After;
            end case;
         end if;
      end if;
   end Compare;

   function Compare
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Path) return Row_Order is
   begin
      if A = Null_Iter then
         if B = Null_Gtk_Tree_Path then
            return Equal;
         else
            return Before;
         end if;
      else
         declare
            First  : constant Gtk_Tree_Path := Get_Path (Model, A);
            Result : constant Row_Order := Compare (Model, First, B);
         begin
            Path_Free (First);
            return Result;
         end;
      end if;
   end Compare;

   function Compare
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Path;
      B     : Gtk_Tree_Iter) return Row_Order is
   begin
      if B = Null_Iter then
         if A = Null_Gtk_Tree_Path then
            return Equal;
         else
            return After;
         end if;
      else
         declare
            Second : constant Gtk_Tree_Path := Get_Path (Model, B);
            Result : constant Row_Order := Compare (Model, A, Second);
         begin
            Path_Free (Second);
            return Result;
         end;
      end if;
   end Compare;

   function Compare
     (Model : Gtk_Tree_Model;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Row_Order is
   begin
      if A = Null_Iter then
         if B = Null_Iter then
            return Equal;
         else
            return Before;
         end if;
      else
         if B = Null_Iter then
            return After;
         else
            declare
               First  : constant Gtk_Tree_Path := Get_Path (Model, A);
               Second : constant Gtk_Tree_Path := Get_Path (Model, B);
               Result : constant Row_Order :=
                        Compare (Model, First, Second);
            begin
               Path_Free (First);
               Path_Free (Second);
               return Result;
            end;
         end if;
      end if;
   end Compare;

   function Delete_Event_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event, Widget);
   begin
      return False;
   end Delete_Event_Handler;

   procedure Destroy_Handler
     (Widget : access Gtk_Widget_Record'Class)
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

   function From_RGBA (Color : Gdk_RGBA) return Gdk_Color is
   begin
      return RGB (Color.Red, Color.Green, Color.Blue);
   end From_RGBA;

   function G_Dir_Open
     (Path  : char_array;
      Flags : Guint;
      Error : access GError) return GDir;
   pragma Import (C, G_Dir_Open, "g_dir_open_utf8");

   procedure Dir_Open
     (Path  : UTF8_String;
      Dir   : out GDir;
      Error : out GError)
   is
      Code : aliased GError;
   begin
      Dir := G_Dir_Open (To_C (Path), 0, Code'Access);
      if Dir = null then
         Error := Code;
      else
         Error := null;
      end if;
   end Dir_Open;

   function G_Dir_Read_Name (Dir : GDir) return chars_ptr;
   pragma Import (C, G_Dir_Read_Name, "g_dir_read_name_utf8");

   function Dir_Read_Name (Dir : GDir) return UTF8_String is
      Ptr : constant chars_ptr := G_Dir_Read_Name (Dir);
   begin
      if Ptr = Null_Ptr then
         raise Ada.IO_Exceptions.End_Error;
      else
         return Value (Ptr);
      end if;
   end Dir_Read_Name;

   procedure Erase
     (Container : not null access Gtk_Container_Record'Class)
   is
      use Gtk.Widget.Widget_List;
      List : Glist := Get_Children (Container);
      This : Glist := First (List);
   begin
      while This /= Null_List loop
         Remove (Container, Get_Data (This));
         This := Next (This);
      end loop;
      Free (List);
   end Erase;

   function Get
     (Store  : not null access Gtk_List_Store_Record'Class;
      Row    : Gtk_Tree_Iter;
      Column : Gint) return String
   is
      Data : GValue;
   begin
      Store.all.Get_Value (Row, Column, Data);
      declare
         Result : constant String := Get_String (Data);
      begin
         Unset (Data);
         return Result;
      end;
   exception
      when others =>
         return "";
   end Get;

   function Get
     (Store  : Gtk_Tree_Model;
      Row    : Gtk_Tree_Iter;
      Column : Gint) return String
   is
      Data : GValue;
   begin
      Get_Value (Store, Row, Column, Data);
      declare
         Result : constant String := Get_String (Data);
      begin
         Unset (Data);
         return Result;
      end;
   exception
      when others =>
         return "";
   end Get;

   function Get
     (Store  : not null access Gtk_Tree_Store_Record'Class;
      Row    : Gtk_Tree_Iter;
      Column : Gint) return String
   is
      Data : GValue;
   begin
      Store.all.Get_Value (Row, Column, Data);
      declare
         Result : constant String := Get_String (Data);
      begin
         Unset (Data);
         return Result;
      end;
   exception
      when others =>
         return "";
   end Get;

   function G_Find_Program_In_Path_UTF8 (Program : char_array)
      return chars_ptr;
   pragma Import
     (C,
      G_Find_Program_In_Path_UTF8,
      "g_find_program_in_path_utf8");

   function Find_Program_In_Path (Program : UTF8_String)
      return UTF8_String is
      Ptr : chars_ptr;
   begin
      Ptr := G_Find_Program_In_Path_UTF8 (To_C (Program));
      if Ptr = Null_Ptr then
         return "";
      else
         declare
            Result : constant UTF8_String := Value (Ptr);
         begin
            G_Free (Ptr);
            return Result;
         end;
      end if;
   end Find_Program_In_Path;

   function G_File_Test
     (File_Name : char_array;
      Test      : GFileTest) return Gboolean;
   pragma Import (C, G_File_Test, "g_file_test_utf8");

   function File_Test
     (File_Name : UTF8_String;
      Flags     : GFileTest) return Boolean is
   begin
      return 0 /= G_File_Test (To_C (File_Name), Flags);
   end File_Test;

   function File_Test (File_Name : UTF8_String) return GFileTest is
      Name : constant char_array := To_C (File_Name);
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

   Wait_Cursors_Count : Natural := 0;
   pragma Atomic (Wait_Cursors_Count);

   overriding procedure Finalize (Cursor : in out Wait_Cursor) is
      procedure Unref (Self : Gdk.Gdk_Window);
      pragma Import (C, Unref, "g_object_unref");
   begin
      if Cursor.Realized then
         Wait_Cursors_Count := Wait_Cursors_Count - 1;
         if Wait_Cursors_Count = 0 then
            Set_Cursor (Cursor.Window, null);
            Unref (Cursor.Window);
         end if;
      end if;
   end Finalize;

   function Find_Property
     (Class : GObject_Class;
      Name  : UTF8_String) return Param_Spec
   is
      function Internal
        (Class : GObject_Class;
         Name  : char_array) return Param_Spec;
      pragma Import (C, Internal, "g_object_class_find_property");
   begin
      return Internal (Class, To_C (Name));
   end Find_Property;

   function Find_Property
     (Object : not null access GObject_Record'Class;
      Name   : UTF8_String) return Param_Spec
   is
      Class : GObject_Class;
   begin
      Class := Class_Peek (Get_Type (Object));
      if Class = Null_GObject_Class then
         return null;
      else
         return Find_Property (Class, Name);
      end if;
   end Find_Property;

   procedure Freeze_Notify
     (Object : not null access GObject_Record'Class)
   is
      procedure Internal (Object : Address);
      pragma Import (C, Internal, "g_object_freeze_notify");
   begin
      Internal (Get_Object (Object));
   end Freeze_Notify;

   function Get_Application_Name return UTF8_String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_get_application_name");
   begin
      return Interfaces.C.Strings.Value (Internal);
   end Get_Application_Name;

   function G_Get_Current_Dir_UTF8
      return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, G_Get_Current_Dir_UTF8, "g_get_current_dir_utf8");

   function Get_Current_Dir return UTF8_String is
      Ptr : chars_ptr;
   begin
      Ptr := G_Get_Current_Dir_UTF8;
      declare
         Result : constant UTF8_String := Value (Ptr);
      begin
         G_Free (Ptr);
         return Result;
      end;
   end Get_Current_Dir;

   function Get_PRGName return UTF8_String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_get_prgname");
   begin
      return Interfaces.C.Strings.Value (Internal);
   end Get_PRGName;

   function Get_Background_Area
     (Tree_View : not null access Gtk_Tree_View_Record'Class;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null) return Gdk_Rectangle
   is
      procedure Internal
        (Tree_View : Address;
         Path      : Address;
         Column    : Address;
         Rect      : out Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_tree_view_get_background_area");
      Result : Gdk_Rectangle;
   begin
      if Column = null then
         Internal
           (Get_Object (Tree_View),
            Get_Object (Path),
            Null_Address,
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
     (Context : not null access Gtk_Style_Context_Record'Class;
      State   : Gtk_State_Flags) return Gdk_RGBA
   is
      type Gdk_RGBA_Ptr is access all Gdk_RGBA;
      pragma Convention (C, Gdk_RGBA_Ptr);

      procedure Free (Ptr : Gdk_RGBA_Ptr);
      pragma Import (C, Free, "gdk_rgba_free");

      procedure Internal
        (Style : System.Address;
         State : Gtk_State_Flags;
         Name  : char_array;
         Color : out Gdk_RGBA_Ptr;
         Nil   : System.Address := Null_Address);
      pragma Import (C, Internal, "gtk_style_context_get");
      Color : Gdk_RGBA := (others => 0.0);
      Ptr   : Gdk_RGBA_Ptr;
   begin
      Internal
        (Get_Object (Context),
         State,
         To_C ("background-color"),
         Ptr);
      if Ptr /= null then
         Color := Ptr.all;
         Free (Ptr);
      end if;
      return Color;
   end Get_Background_Color;

   function Get_Basename (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : char_array) return chars_ptr;
      pragma Import (C, Internal, "g_path_get_basename");
      Ptr    : constant chars_ptr   := Internal (To_C (File_Name));
      Result : constant UTF8_String := Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Get_Basename;

   function Get_Clip_Rectangle (Context : Cairo_Context)
      return Boolean is
      function Internal
        (Context   : Cairo_Context;
         Rectangle : System.Address := Null_Address) return Gboolean;
      pragma Import (C, Internal, "gdk_cairo_get_clip_rectangle");
   begin
      return 0 /= Internal (Context);
   end Get_Clip_Rectangle;

   procedure Get_Clip_Rectangle
     (Context   : Cairo_Context;
      Rectangle : out Gdk_Rectangle;
      Empty     : out Boolean)
   is
      function Internal
        (Context   : Cairo_Context;
         Rectangle : access Gdk_Rectangle) return Gboolean;
      pragma Import (C, Internal, "gdk_cairo_get_clip_rectangle");
      Area : aliased Gdk_Rectangle;
   begin
      Empty := 0 = Internal (Context, Area'Access);
      if not Empty then
         Rectangle := Area;
      end if;
   end Get_Clip_Rectangle;

   function Get_Column (Value : GValue) return Gtk_Tree_View_Column is
      use System;
      Stub : Gtk_Tree_View_Column_Record;
      Data : constant Address := Get_Address (Value);
   begin
      if Data = Null_Address then
         return null;
      else
         return Gtk_Tree_View_Column (Get_User_Data_Fast (Data, Stub));
      end if;
   end Get_Column;

   function Get_Column_No
     (Tree_View : not null access Gtk_Tree_View_Record'Class;
      Column    : not null access Gtk_Tree_View_Column_Record'Class) return Gint
   is
      This : Gtk_Tree_View_Column;
      That : constant Address := Get_Object (Column);
   begin
      for Index in 0 .. Gint'Last loop
         This := Get_Column (Tree_View, Index);
         exit when This = null;
         if Get_Object (This) = That then
            return Index;
         end if;
      end loop;
      return -1;
   end Get_Column_No;

   function Get_Dirname (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : char_array) return chars_ptr;
      pragma Import (C, Internal, "g_path_get_dirname");
      Ptr    : constant chars_ptr   := Internal (To_C (File_Name));
      Result : constant UTF8_String := Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Get_Dirname;

   procedure Get_Screen_Position
     (Widget : not null access Gtk_Widget_Record'Class;
      X      : out Gint;
      Y      : out Gint)
   is
      Area : Gtk_Allocation;
   begin
      Widget.all.Get_Allocation (Area);
      Get_Origin (Widget.all.Get_Window, X, Y);
      X := X + Area.X;
      Y := Y + Area.Y;
   end Get_Screen_Position;

   function Get_Root (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : char_array) return chars_ptr;
      pragma Import (C, Internal, "g_path_skip_root");
      Name : char_array := To_C (File_Name);
      Ptr  : constant chars_ptr  := Internal (Name);
   begin
      if Ptr = Null_Ptr then
         raise Ada.IO_Exceptions.Use_Error;
      else
         declare
            Length : constant size_t := Strlen (Ptr);
         begin
            if Length > 0 then
               Name (Name'Last - Length) := nul;
               return To_Ada (Name);
            else
               return File_Name;
            end if;
         end;
      end if;
   end Get_Root;

   function Get_Row_No
     (Model : Gtk_Tree_Model;
      Path  : Gtk_Tree_Path) return Gint is
   begin
      if Path = Null_Gtk_Tree_Path then
         return -1;
      else
         declare
            Indices : Gint_Array renames Get_Indices (Path);
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
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Gint
   is
      Path : constant Gtk_Tree_Path := Get_Path (Model, Iter);
   begin
      if Path = Null_Gtk_Tree_Path then
         return -1;
      else
         declare
            Indices : Gint_Array renames Get_Indices (Path);
            Result  : Gint;
         begin
            if Indices'Length > 0 then
               Result := Indices (Indices'Last);
            else
               Result := -1;
            end if;
            Path_Free (Path);
            return Result;
         end;
      end if;
   end Get_Row_No;

   function Get_User_Special_Dir (Directory : User_Directory)
      return UTF8_String is
      function Internal (Directory : int) return chars_ptr;
      pragma Import (C, Internal, "g_get_user_special_dir");
   begin
      return Value (Internal (User_Directory'Pos (Directory)));
   end Get_User_Special_Dir;

   procedure Get_Visible_Range
             (  Tree_View  : not null access Gtk_Tree_View_Record'Class;
                Start_Path : out Gtk_Tree_Path;
                End_Path   : out Gtk_Tree_Path
             )  is
      function Internal
               (  Tree_View : Address;
                  From_Path : Address;
                  To_Path   : Address
               )  return Gboolean;
      pragma Import (C, Internal, "gtk_tree_view_get_visible_range");
      From_Path : constant Address := Null_Address;
      To_Path   : constant Address := Null_Address;
      Result    : Gboolean;
   begin
      Result := Internal (Get_Object (Tree_View), From_Path, To_Path);
      Set_Object (Start_Path, From_Path);
      Set_Object (End_Path,   To_Path);
   end Get_Visible_Range;

   overriding procedure Initialize (Cursor : in out Wait_Cursor) is
      Clock  : Gdk.Gdk_Cursor;
      Active : Boolean;
      procedure Ref (Self : Gdk.Gdk_Window);
      pragma Import (C, Ref, "g_object_ref");
   begin
      Cursor.Realized := Get_Realized (Cursor.Widget);
      if Cursor.Realized then
         if Wait_Cursors_Count = 0 then
            Cursor.Window := Get_Window (Cursor.Widget);
            Ref (Cursor.Window);
            Gdk_New (Clock, Watch);
            Set_Cursor (Cursor.Window, Clock);
            Unref (Clock);
            while Events_Pending loop -- Pump the events queue
               Active := Main_Iteration;
            end loop;
         end if;
         Wait_Cursors_Count := Wait_Cursors_Count + 1;
      end if;
   end Initialize;

   procedure Insert_Alt
             (  Buffer : not null access Gtk_Text_Buffer_Record'Class;
                Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
                Text   : UTF8_String
             )  is
      procedure Internal
                (  Buffer : System.Address;
                   Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
                   Text   : char_array;
                   Len    : Gint
                );
      pragma Import (C, Internal, "gtk_text_buffer_insert");
   begin
      Internal
      (  Get_Object (Buffer),
         Iter,
         Interfaces.C.To_C (Text),
         Text'Length
      );
   end Insert_Alt;

   function Is_A (Derived, Ancestor : GType) return Boolean is
      function Internal (Derived, Ancestor : GType) return Gboolean;
      pragma Import (C, Internal, "g_type_is_a");
   begin
      return Internal (Derived, Ancestor) /= 0;
   end Is_A;

   function Is_In
            (  Container : not null access Gtk_Container_Record'Class;
               Element   : not null access Gtk_Widget_Record'Class
            )  return Boolean is
      Data : aliased Search_Data :=
                     (  Element.all'Unchecked_Access,
                        False
                     );
   begin
      For_Test.Foreach
      (  Gtk_Container_Record (Container.all)'Unchecked_Access,
         Test'Access,
         Data'Unchecked_Access
      );
      return Data.Found;
   end Is_In;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Path
            )  return Boolean is
   begin
      if A = Null_Gtk_Tree_Path then
         return False;
      elsif B = Null_Gtk_Tree_Path then
         return True;
      elsif Get_Depth (A) = 0 then
         return False;
      elsif Get_Depth (B) = 0 then
         return True;
      else
         return Is_Descendant (A, B);
      end if;
   end Is_In;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if A = Null_Iter then
         return False;
      elsif B = Null_Iter then
         return True;
      else
         declare
            First  : constant Gtk_Tree_Path := Get_Path (Model, A);
            Second : constant Gtk_Tree_Path := Get_Path (Model, B);
            Result : constant Boolean := Is_Descendant (First, Second);
         begin
            Path_Free (First);
            Path_Free (Second);
            return Result;
         end;
      end if;
   end Is_In;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Path
            )  return Boolean is
   begin
      if A = Null_Iter then
         return False;
      elsif B = Null_Gtk_Tree_Path then
         return True;
      elsif Get_Depth (B) = 0 then
         return True;
      else
         declare
            First  : constant Gtk_Tree_Path := Get_Path (Model, A);
            Result : constant Boolean := Is_Descendant (First, B);
         begin
            Path_Free (First);
            return Result;
         end;
      end if;
   end Is_In;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if A = Null_Gtk_Tree_Path then
         return False;
      elsif B = Null_Iter then
         return True;
      elsif Get_Depth (A) = 0 then
         return False;
      else
         declare
            Second : constant Gtk_Tree_Path := Get_Path (Model, B);
            Result : constant Boolean := Is_Descendant (A, Second);
         begin
            Path_Free (Second);
            return Result;
         end;
      end if;
   end Is_In;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Path
            )  return Boolean is
   begin
      if B = Null_Gtk_Tree_Path then
         return False;
      elsif A = Null_Gtk_Tree_Path then
         return Get_Indices (B)'Length = 1;
      end if;
      declare
         A_Indices : Gint_Array renames Get_Indices (A);
         B_Indices : Gint_Array renames Get_Indices (B);
      begin
         return
         (  A_Indices'Length + 1 = B_Indices'Length
         and then
            (  A_Indices'Length = 0
            or else
               (  A_Indices
               =  B_Indices (B_Indices'First..B_Indices'Last - 1)
         )  )  );
      end;
   end Is_Parent;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Path
            )  return Boolean is
   begin
      if B = Null_Gtk_Tree_Path then
         return False;
      elsif A = Null_Iter then
         return Get_Indices (B)'Length = 1;
      end if;
      declare
         A_Path : constant Gtk_Tree_Path := Get_Path (Model, A);
         Result : constant Boolean := Is_Parent (Model, A_Path, B);
      begin
         Path_Free (A_Path);
         return Result;
      end;
   end Is_Parent;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if B = Null_Iter then
         return False;
      elsif A = Null_Gtk_Tree_Path then
         return Parent (Model, B) = Null_Iter;
      end if;
      declare
         B_Path : constant Gtk_Tree_Path := Get_Path (Model, B);
         Result : constant Boolean := Is_Parent (Model, A, B_Path);
      begin
         Path_Free (B_Path);
         return Result;
      end;
   end Is_Parent;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if B = Null_Iter then
         return False;
      elsif A = Null_Iter then
         return True;
      else
         return Parent (Model, B) = A;
      end if;
   end Is_Parent;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Path
            )  return Boolean is
   begin
      if B = Null_Gtk_Tree_Path then
         return A = Null_Gtk_Tree_Path;
      elsif A = Null_Gtk_Tree_Path then
         return False;
      end if;
      declare
         A_Indices : Gint_Array renames Get_Indices (A);
         B_Indices : Gint_Array renames Get_Indices (B);
      begin
         return
         (  A_Indices'Length = B_Indices'Length
         and then
            (  A_Indices'Length < 2
            or else
               (  A_Indices (A_Indices'First..A_Indices'Last - 1)
               =  B_Indices (B_Indices'First..B_Indices'Last - 1)
         )  )  );
      end;
   end Is_Sibling;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Path
            )  return Boolean is
   begin
      if B = Null_Gtk_Tree_Path then
         return A = Null_Iter;
      elsif A = Null_Iter then
         return False;
      end if;
      declare
         A_Path : constant Gtk_Tree_Path := Get_Path (Model, A);
         Result : constant Boolean := Is_Sibling (Model, A_Path, B);
      begin
         Path_Free (A_Path);
         return Result;
      end;
   end Is_Sibling;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if B = Null_Iter then
         return A = Null_Gtk_Tree_Path;
      elsif A = Null_Gtk_Tree_Path then
         return False;
      end if;
      declare
         B_Path : constant Gtk_Tree_Path := Get_Path (Model, B);
         Result : constant Boolean := Is_Sibling (Model, A, B_Path);
      begin
         Path_Free (B_Path);
         return Result;
      end;
   end Is_Sibling;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if B = Null_Iter then
         return A = Null_Iter;
      elsif A = Null_Iter then
         return False;
      else
         return Parent (Model, B) = Parent (Model, A);
      end if;
   end Is_Sibling;

   function Keyval_To_UTF8 (Key_Val : Gdk.Types.Gdk_Key_Type)
      return UTF8_String is
      Result : UTF8_String (1..8);
      Last   : Natural;
   begin
      Unichar_To_UTF8 (Keyval_To_Unicode (Key_Val), Result, Last);
      return Result (1..Last);
   end Keyval_To_UTF8;

   function Is_Absolute (File_Name : UTF8_String) return Boolean is
      function Internal (File_Name : char_array) return Gboolean;
      pragma Import (C, Internal, "g_path_is_absolute");
   begin
      return Internal (To_C (File_Name)) /= 0;
   end Is_Absolute;

   procedure Message_Dialog
             (  Message       : UTF8_String;
                Parent        : not null access Gtk_Widget_Record'Class;
                Title         : UTF8_String       := "";
                Mode          : UTF8_String       := Stock_Dialog_Error;
                Justification : Gtk_Justification := Justify_Center;
                Response      : access Gtk_Response_Type := null
             )  is
      function Get_Title return String is
      begin
         if Title'Length > 0 then
            return Title;
         elsif Mode = Stock_Dialog_Error then
            return "Error";
         elsif Mode = Stock_Dialog_Info then
            return "Information";
         elsif Mode = Stock_Dialog_Question then
            return "Question";
         elsif Mode = Stock_Dialog_Warning then
            return "Warning";
         else
            return "";
         end if;
      end Get_Title;
      Dialog : Gtk_Dialog;
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Image  : Gtk_Image;
      Result : Gtk_Response_Type;
      Top    : constant Gtk_Widget := Parent.Get_Toplevel;
   begin
      if Top = null or else Top.all not in Gtk.Window.Gtk_Window_Record'Class then
         if Response /= null then
            Response.all := Gtk_Response_Cancel;
         end if;
         return;
      end if;
      Gtk_New
      (  Dialog => Dialog,
         Title  => Title,
         Flags  => Modal or Destroy_With_Parent,
         Parent => Gtk.Window.Gtk_Window_Record'Class (Top.all)'Unchecked_Access
      );
      Dialog.all.Realize;
      Gtk_New_Hbox (Box);
      Dialog.all.Get_Content_Area.all.Pack_Start (Box, Padding => 10);
      Gtk_New (Image, Mode, Icon_Size_Dialog);
      Box.all.Pack_Start (Image, Padding => 10);

      if Mode = Stock_Dialog_Question then
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_Yes,
            Icon     => Stock_Yes,
            Label    => "_Yes"
         );
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_No,
            Icon     => Stock_Cancel,
            Label    => "_No"
         );
      else
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_OK,
            Icon     => Stock_Ok,
            Label    => "_OK"
         );
      end if;
      Gtk_New (Label, Message);
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
      function Internal (File_Name : char_array) return int;
      pragma Import (C, Internal, "g_remove");
   begin
      if 0 /= Internal (To_C (File_Name)) then
         if File_Test (File_Name, File_Test_Exists) then
            raise Ada.IO_Exceptions.Use_Error with "File cannot be deleted";
         end if;
      end if;
   end Remove;

   procedure Rename (Old_File_Name, New_File_Name : UTF8_String) is
      function Internal (Old_File_Name, New_File_Name : char_array)
         return int;
      pragma Import (C, Internal, "g_rename");
   begin
      if 0 /= Internal (To_C (Old_File_Name), To_C (New_File_Name)) then
         if File_Test (Old_File_Name, File_Test_Exists) then
            raise Ada.IO_Exceptions.Use_Error with "File cannot be renamed";
         else
            raise Ada.IO_Exceptions.Name_Error with "File does not exist";
         end if;
      end if;
   end Rename;

   function RGB (Red, Green, Blue : Gdouble) return Gdk_Color is
      Result  : Gdk_Color;
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
      Set_Rgb (Result, +Red, +Green, +Blue);
      return Result;
   end RGB;

   procedure Set
             (  Store  : not null access Gtk_List_Store_Record'Class;
                Row    : Gtk_Tree_Iter;
                Column : Gint;
                Value  : String
             )  is
      Data : GValue;
   begin
      Init (Data, GType_String);
      Set_String (Data, Value);
      Store.all.Set_Value (Row, Column, Data);
      Unset (Data);
   exception
      when others =>
         Unset (Data);
         raise;
   end Set;

   procedure Set
             (  Store  : not null access Gtk_Tree_Store_Record'Class;
                Row    : Gtk_Tree_Iter;
                Column : Gint;
                Value  : String
             )  is
      Data : GValue;
   begin
      Init (Data, GType_String);
      Set_String (Data, Value);
      Store.all.Set_Value (Row, Column, Data);
      Unset (Data);
   exception
      when others =>
         Unset (Data);
         raise;
   end Set;

   procedure Set (Value : in out GValue; Object : GObject) is
      procedure Internal (Value : in out GValue; Object : Address);
      pragma Import (C, Internal, "g_value_set_object");
   begin
      Internal (Value, Convert (Object));
   end Set;

   procedure Set_Background_Color
             (  Widget : not null access Gtk_Widget_Record'Class;
                Color  : Gdk_RGBA
             )  is
      Provider : Gtk_Css_Provider;
      CSS      : constant String := "* { background-color: " &
                                    To_String (Color) &
                                    "; }";
   begin
      Gtk_New (Provider);
      if Provider.all.Load_From_Data (CSS, null) then
         Get_Style_Context (Widget).all.Add_Provider
         (  +Provider,
            Priority_Application
         );
      end if;
      Provider.all.Unref;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Ada.Exceptions.Exception_Information (Error)
            &  Where ("Set_Background_Color")
         )  );
   end Set_Background_Color;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
             (  Object : not null access
                         Glib.Object.GObject_Record'Class;
                Name   : Glib.Properties.Property_Float;
                Value  : Glib.Gfloat
             )  is
      use Glib.Values;

      procedure Internal
        (Object : in     Address;
         Name   : in     Glib.Property;
         Value  : in out GValue);
      pragma Import (C, Internal, "g_object_set_property");

      Val : GValue;
   begin
      Init (Val, Glib.GType_Float);
      Set_Float (Val, Value);
      Internal
      (  Glib.Object.Get_Object (Object),
         Glib.Property (Name),
         Val
      );
   end Set_Property;

   procedure Set_Tip
             (  Widget : not null access
                         Gtk.Widget.Gtk_Widget_Record'Class
             )  is
      procedure Internal
                (  Widget : System.Address;
                   Text   : Interfaces.C.Strings.chars_ptr
                );
      pragma Import (C, Internal, "gtk_widget_set_tooltip_text");
   begin
      Internal (Get_Object (Widget), Interfaces.C.Strings.Null_Ptr);
   end Set_Tip;

   function Skip_Root (File_Name : UTF8_String) return UTF8_String is
      function Internal (File_Name : char_array) return chars_ptr;
      pragma Import (C, Internal, "g_path_skip_root");
      Name : constant char_array := To_C (File_Name);
      Ptr  : constant chars_ptr  := Internal (Name);
   begin
      if Ptr = Null_Ptr then
         raise Ada.IO_Exceptions.Use_Error;
      else
         return Value (Ptr);
      end if;
   end Skip_Root;

   procedure Thaw_Notify
             (  Object : not null access GObject_Record'Class
             )  is
      procedure Internal (Object : Address);
      pragma Import (C, Internal, "g_object_thaw_notify");
   begin
      Internal (Get_Object (Object));
   end Thaw_Notify;

   function Themed_Icon_New (Icon_Name : UTF8_String) return GObject is
      function Internal (Name : char_array) return Address;
      pragma Import (C, Internal, "g_themed_icon_new");
   begin
      return Convert (Internal (To_C (Icon_Name)));
   end Themed_Icon_New;

   function Themed_Icon_New_With_Default_Fallbacks
            (  Icon_Name : UTF8_String
            )  return GObject is
      function Internal (Name : char_array) return Address;
      pragma Import
             (  C,
                Internal,
                "g_themed_icon_new_with_default_fallbacks"
             );
   begin
      return Convert (Internal (To_C (Icon_Name)));
   end Themed_Icon_New_With_Default_Fallbacks;

   function To_RGBA (Color : Gdk_Color) return Gdk_RGBA is
   begin
      return
      (  Red   => Gdouble (Red   (Color)) / Gdouble (Guint16'Last),
         Green => Gdouble (Green (Color)) / Gdouble (Guint16'Last),
         Blue  => Gdouble (Blue  (Color)) / Gdouble (Guint16'Last),
         Alpha => 1.0
      );
   end To_RGBA;

   function To_String (Status : Cairo_Status) return String is
      function Internal (Status : Cairo_Status)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "cairo_status_to_string");
      Result : Interfaces.C.Strings.chars_ptr;
   begin
      Result := Internal (Status);
      if Result /= Interfaces.C.Strings.Null_Ptr then
         return Value (Result);
      else
         return "";
      end if;
   end To_String;

   procedure Test
             (  Item : not null access Gtk_Widget_Record'Class;
                Data : Search_Data_Ptr
             )  is
   begin
      if Data.Item = Item.all'Access then
         Data.Found := True;
      end if;
   end Test;

   package body Set_Column_Cell_Data is
      use Gtk.Cell_Renderer;

      package Closure is
         new Glib.Object.User_Data_Closure (User_Data, Destroy);
      use Closure;

      function From_Address is
          new Ada.Unchecked_Conversion (Address, Cell_Data_Function);

      function To_Address is
          new Ada.Unchecked_Conversion (Cell_Data_Function, Address);

      procedure Callback
                (  Column : Address;
                   Cell   : Address;
                   Model  : Gtk_Tree_Model;
                   Iter   : access Gtk_Tree_Iter;
                   Data   : Address
                );
      pragma Convention (C, Callback);

      procedure Callback
                (  Column : Address;
                   Cell   : Address;
                   Model  : Gtk_Tree_Model;
                   Iter   : access Gtk_Tree_Iter;
                   Data   : Address
                )  is
         User_Data   : constant Internal_Data_Access := Convert (Data);
         Stub_Column : Gtk_Tree_View_Column_Record;
         Stub_Cell   : Gtk_Cell_Renderer_Record;
      begin
         From_Address (User_Data.Func)
         (  Gtk_Tree_View_Column (Get_User_Data (Column, Stub_Column)),
            Gtk_Cell_Renderer (Get_User_Data (Cell, Stub_Cell)),
            Model,
            Iter.all,
            User_Data.Data.all
         );
      exception
         when Error : others =>
            Log
              (GtkAda_Contributions_Domain,
               Log_Level_Critical,
               "Fault: " & Ada.Exceptions.Exception_Information (Error) &
                 Where ("Set_Column_Cell_Data (callback)"));
      end Callback;

      procedure Set_Cell_Data_Func
                (  Column : not null access
                            Gtk_Tree_View_Column_Record'Class;
                   Cell   : not null access
                            Gtk_Cell_Renderer_Record'Class;
                   Func   : Cell_Data_Function;
                   Data   : User_Data
                )  is
          procedure Internal
                    (  Tree_Column   : Address;
                       Cell_Renderer : Address;
                       Func          : Address;
                       Func_data     : Address;
                       Destroy       : Address
                    );
          pragma Import
                 (  C,
                    Internal,
                    "gtk_tree_view_column_set_cell_data_func"
                 );
      begin
         if Func = null then
            Internal
            (  Get_Object (Column),
               Get_Object (Cell),
               Null_Address,
               Null_Address,
               Null_Address
            );
         else
            Internal
            (  Get_Object (Column),
               Get_Object (Cell),
               Callback'Address,
               Build (To_Address (Func), Data),
               Free_Data'Address
            );
         end if;
      end Set_Cell_Data_Func;

   end Set_Column_Cell_Data;

   package Set_Pixbuf_Data is new Set_Column_Cell_Data (GInt);

   procedure Cell_Pixbuf
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Gint
             )  is
      Value : GValue;
      Width : Gint;
   begin
      Get_Value (Model, Iter, Data, Value);
      Width := Get_Property (Cell, Build ("stock-size"));
      declare
         Name : constant String := Get_String (Value);
      begin
         Unset (Value);
         declare
            Address : System.Address;
            Set     : Gtk_Icon_Set;
         begin
            if Stock_Icons /= No_Table then
               Address := Look_Up (Stock_Icons, To_C (Name));
               if Address /= Null_Address then
                  Set.Set_Object (Address);
                  Set_Property
                  (  Cell,
                     Property_Object'(Build ("pixbuf")),
                     Set.Render_Icon_Pixbuf
                     (  Get_Style_Context (Column.Get_Tree_View),
                        +Width
                  )  );
                  return;
               end if;
            end if;
         end;
         declare
            Icon    : Gdk_Pixbuf;
            Default : constant Gtk_Icon_Theme := Get_Default;
         begin
            if Default /= null then
               Icon := Default.Load_Icon
                       (  Name,
                          Width,
                          Icon_Lookup_Use_Builtin,
                          null
                       );
               if Icon /= null then
                  Set_Property
                  (  Cell,
                     Property_Object'(Build ("pixbuf")),
                     Icon
                  );
                  return;
               end if;
            end if;
         end;
      end;
      Set_Property (Cell, Property_String'(Build ("icon-name")), "");
   exception
      when Error : others =>
         Log
           (GtkAda_Contributions_Domain,
            Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Cell_Pixbuf"));
   end Cell_Pixbuf;

   procedure Add_Stock_Attribute
             (  Cell_Layout : not null access
                              Gtk_Tree_View_Column_Record'Class;
                Cell        : not null access
                              Gtk_Cell_Renderer_Pixbuf_Record'Class;
                Column      : Gint
             )  is
   begin
      Cell_Layout.all.Add_Attribute (Cell, "icon-name", Column);
      Set_Pixbuf_Data.Set_Cell_Data_Func
      (  Cell_Layout,
         Cell,
         Cell_Pixbuf'Access,
         Column
      );
   exception
      when Error : others =>
         Log
           (GtkAda_Contributions_Domain,
            Log_Level_Critical,
            "Fault: " & Ada.Exceptions.Exception_Information (Error) &
              Where ("Add_Stock_Attribute"));
   end Add_Stock_Attribute;

begin
   --
   -- Blocking   annoying Windows' popup  error  boxes.   Most   notably
   -- "please insert disk". Under Linux SetErrorMode is routed to a null
   -- implementation   using a   weak  reference   implementation   from
   -- Glib.Wrappers.
   --
   declare
      use Interfaces;
      function SetErrorMode (Mode : Unsigned_32) return Unsigned_32;
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

      Result : Unsigned_32;
   begin
      Result := SetErrorMode
                (  SEM_NOOPENFILEERRORBOX
                or SEM_NOGPFAULTERRORBOX
                or SEM_FAILCRITICALERRORS
                );
   end;
end Gtk.Missed;
