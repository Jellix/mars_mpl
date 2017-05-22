--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Abstract_Browser                        Luebeck            --
--  Implementation                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Gdk.Pixbuf.Conversions;
with Gdk.Rectangle;
with Gdk.RGBA;
with Gdk.Types.Keysyms;
with Gdk.Window;

with Glib.Messages;
with Glib.Object.Checked_Destroy;
with Glib.Properties;
with Glib.Unicode;

with Gtk.Main.Router;
with Gtk.Stock;
with Gtk.Style_Context;

with Gtkada.Types;

with Interfaces.C.Strings;

package body Gtk.Abstract_Browser is

   pragma Warnings (Off, "declaration hides ""Params""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   Cache_Model_Type : GType := GType_Invalid;
   Items_Model_Type : GType := GType_Invalid;
   Has_File         : Boolean := False;
   File             : Ada.Text_IO.File_Type;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Abstract_Browser." & Name;
   end Where;

   package Traverse is
     new Gtk.Tree_Model.Foreach_User_Data (Traced_Actions);

   type Path_Node;
   type Path_Node_Ptr is access Path_Node;
   type Path_Node (Length : Natural) is record
      Next      : Path_Node_Ptr;
      Directory : Item_Path (1 .. Length);
   end record;

   overriding function "=" (Left, Right : Item_Path_Reference) return Boolean is
   begin
      if Left.Ptr = null then
         if Right.Ptr = null then
            return True;
         else
            return Right.Ptr.all.Length = 0;
         end if;
      else
         if Right.Ptr = null then
            return False;
         else
            return Left.Ptr.all = Right.Ptr.all;
         end if;
      end if;
   end "=";

   function "=" (Left : Item_Path_Reference; Right : Item_Path)
                 return Boolean is
   begin
      if Left.Ptr = null then
         return Right'Length = 0;
      else
         return Left.Ptr.all.Path = Right;
      end if;
   end "=";

   function "=" (Left : Item_Path; Right : Item_Path_Reference)
                 return Boolean is
   begin
      if Right.Ptr = null then
         return Left'Length = 0;
      else
         return Left = Right.Ptr.all.Path;
      end if;
   end "=";

   procedure Free is
     new Ada.Unchecked_Deallocation (Path_Node, Path_Node_Ptr);

   procedure Release (Ptr : in out Path_Node_Ptr);

   procedure Split
     (Store  : access Gtk_Abstract_Directory_Record'Class;
      Item   : Item_Path;
      Path   : out Path_Node_Ptr;
      Length : out Positive);

   function "+" (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
                 return Gtk.Widget.Gtk_Widget
   is
      Parent : constant Gtk.Widget.Gtk_Widget := Gtk.Widget.Get_Parent (Widget);
      use type Gtk.Widget.Gtk_Widget;
   begin
      if Parent = null then
         return Widget.all'Unchecked_Access;
      else
         return Parent;
      end if;
   end "+";

   Directory_Items_Class                : Ada_GObject_Class := Uninitialized_Class;
   --
   -- Signals of abstract directory
   --
   Abstract_Directory_Signal_Names      : constant
     Gtkada.Types.Chars_Ptr_Array :=
       (0 => Interfaces.C.Strings.New_String ("read-error"),
        1 => Interfaces.C.Strings.New_String ("rewind-error"),
        2 => Interfaces.C.Strings.New_String ("refreshed"),
        3 => Interfaces.C.Strings.New_String ("progress"),
        4 => Interfaces.C.Strings.New_String ("item-inserted"),
        5 => Interfaces.C.Strings.New_String ("item-renamed"),
        6 => Interfaces.C.Strings.New_String ("item-deleting"),
        7 => Interfaces.C.Strings.New_String ("item-deleted"));

   Abstract_Directory_Signal_Parameters : constant
     Signal_Parameter_Types :=
       (0 => (0 => GType_String,  1 => GType_String),
        1 => (0 => GType_String,  1 => GType_String),
        2 => (0 => GType_String,  1 => GType_None),
        3 => (0 => GType_String,  1 => GType_Double),
        4 => (0 => GType_Pointer, 1 => Gtk.Tree_Model.Path_Get_Type),
        5 => (0 => GType_Pointer, 1 => GType_String),
        6 => (0 => GType_Pointer, 1 => Gtk.Tree_Model.Path_Get_Type),
        7 => (0 => GType_String,  1 => GType_None));

   Read_Error_ID                        : Signal_Id := Invalid_Signal_Id;
   Rewind_Error_ID                      : Signal_Id;
   Refreshed_ID                         : Signal_Id;
   Progress_ID                          : Signal_Id;
   Inserted_ID                          : Signal_Id;
   Renamed_ID                           : Signal_Id;
   Deleted_ID                           : Signal_Id;
   Deleting_ID                          : Signal_Id;
   --
   -- Signals of directory items
   --
   Directory_Items_Signal_Names         : constant
     Gtkada.Types.Chars_Ptr_Array :=
       (0 => Interfaces.C.Strings.New_String ("directory-changed"),
        1 => Interfaces.C.Strings.New_String ("selection-changed"));

   function Get_Name
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint := 1) return Item_Name
   is
      Data : Glib.Values.GValue;
   begin
      Gtk.Tree_Model.Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Name :=
        Item_Name (Glib.Values.Get_String (Data)) do
         Glib.Values.Unset (Data);
      end return;
   end Get_Name;

   function Get_Name
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint := 1) return Item_Name
   is
      Data : Glib.Values.GValue;
   begin
      Gtk.Tree_Store.Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Name :=
        Item_Name (Glib.Values.Get_String (Data)) do
         Glib.Values.Unset (Data);
      end return;
   end Get_Name;

   function Get_Type
     (Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint := 0) return Item_Type
   is
      Data : Glib.Values.GValue;
   begin
      Gtk.Tree_Model.Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Type :=
        Item_Type (Glib.Values.Get_String (Data)) do
         Glib.Values.Unset (Data);
      end return;
   end Get_Type;

   function Get_Type
     (Model  : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint := 0) return Item_Type
   is
      Data : Glib.Values.GValue;
   begin
      Gtk.Tree_Store.Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Type :=
        Item_Type (Glib.Values.Get_String (Data)) do
         Glib.Values.Unset (Data);
      end return;
   end Get_Type;

   procedure Enter
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Path  : Item_Path;
      High  : out Gdouble)
   is
      pragma Inline (Enter);
   begin
      if Store.all.Depth = 0 then
         Store.all.Last_Time := Ada.Calendar.Clock;
         Store.all.Low  := 0.0;
         Store.all.High := 1.0;
         High       := 1.0;
         Store.all.Progress (Path, 0.0);
      else
         High := Store.all.High;
         declare
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            use type Ada.Calendar.Time;
         begin
            if Now - Store.all.Last_Time > 0.2 then
               Store.all.Progress (Path, Store.all.Low);
            end if;
         end;
      end if;
      Store.all.Depth := Store.all.Depth + 1;
   end Enter;

   procedure Leave
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Path  : Item_Path;
      High  : Gdouble)
   is
      pragma Inline (Leave);
   begin
      Store.all.Depth := Store.all.Depth - 1;
      Store.all.Low   := High;
      Store.all.High  := High;
      if Store.all.Depth = 0 then
         Store.all.Progress (Path, 1.0);
      else
         declare
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            use type Ada.Calendar.Time;
         begin
            if Now - Store.all.Last_Time > 0.2 then
               Store.all.Progress (Path, High);
            end if;
         end;
      end if;
   end Leave;

   function Expanded
     (Tree : not null access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Path   : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                 Gtk.Tree_Model.Get_Path (Gtk.Tree_View.Get_Model (Tree), Iter);
      Result : constant Boolean := Tree.all.Row_Expanded (Path);
   begin
      Gtk.Tree_Model.Path_Free (Path);
      return Result;
   end Expanded;

   procedure Activated
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) is
   begin
      if Widget.all.Is_Directory (Index) then
         Widget.all.Directories.all.Set_Current_Directory
           (Widget.all.Get_Path (Index));
      elsif Widget.all.Is_Editable then
         --
         -- When  the  widget has editable names, then activation starts
         -- editing.
         --
         declare
            Row    : Gtk.Tree_Model.Gtk_Tree_Iter :=
                       Widget.all.Get_Iter (Index);
            Column : Positive                     :=
                       Widget.all.Get_Column (Index);
            Path   : Gtk.Tree_Model.Gtk_Tree_Path;

            use type Gtk.Tree_Model.Gtk_Tree_Iter;
         begin
            if Row = Gtk.Tree_Model.Null_Iter then
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Invisible item activated"
                  & Where ("Activated"));
            else
               Widget.all.Columns.all.To_Columned (Row, Column);
               Path :=
                 Gtk.Tree_Model.Columned_Store.Get_Path (Widget.all.Columns,
                                                         Row);
               Set_Cursor_On_Cell
                 (Tree_View     => Widget,
                  Path          => Path,
                  Start_Editing => True,
                  Focus_Column  =>
                    Widget.all.Get_Column (Gint (Column) - 1),
                  Focus_Cell    =>
                    Widget.all.Name_Renderers (Column).all'Access);
               Gtk.Tree_Model.Path_Free (Path);
            end if;
         end;
      end if;
   end Activated;

   procedure Add_Folder
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Row       : in out Gtk.Tree_Model.Gtk_Tree_Iter;
      Directory : Item_Path;
      Expanded  : Boolean;
      Updated   : out Boolean)
   is
      Item    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path    : Gtk.Tree_Model.Gtk_Tree_Path;
      High    : Gdouble;
      Step    : Gdouble;
      Recurse : Natural := 0;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      Enter (Store, Directory, High);
      if Row = Gtk.Tree_Model.Null_Iter then
         if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
            Store.all.Trace
              (Store.all.Depth - 1,
               "start adding root");
         end if;
         Item := Gtk.Tree_Store.Get_Iter_First (Store.all.Tree);
      else
         if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
            Store.all.Trace
              (Store.all.Depth - 1,
               "start adding " & String (Directory));
         end if;
         if
           Gtk.Tree_Store.Get_Int (Store.all.Tree, Row, 2) not in Cached_Directory
         then
            Updated := False;
            Leave (Store, Directory, High);
            if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
               Store.all.Trace
                 (Store.all.Depth,
                  "stop adding " & String (Directory) & " (item)");
            end if;
            return;
         end if;
         Item := Store.all.Tree.all.Children (Row);
      end if;
      if Item /= Gtk.Tree_Model.Null_Iter then
         -- This directory is already cached
         if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
            Store.all.Trace
              (Store.all.Depth - 1,
               "stop adding "
               & String (Directory)
               & " (already cached, first: "
               & String (Get_Name (Store.all.Tree, Item))
               & ')');
         end if;
         if Expanded then
            Expand_Folder (Store, Row, Directory);
         end if;
         Updated := False;
         Leave (Store, Directory, High);
         return;
      end if;
      -- Prepare to read the directory items out
      begin
         declare
            Data : Directory_Item := Rewind (Store, Directory);
         begin
            case Data.Policy is
               when Cache_Never =>
                  if Row /= Gtk.Tree_Model.Null_Iter then
                     Set_Item (Store, Row, Data);
                  end if;
                  Updated := True;
                  Leave (Store, Directory, High);
                  return;
               when Cache_Expanded =>
                  Data.Directory := True;
                  if Row /= Gtk.Tree_Model.Null_Iter then
                     Set_Item (Store, Row, Data);
                  end if;
               when Cache_Ahead =>
                  null;
            end case;
         end;
      exception
         when Error : Ada.IO_Exceptions.Data_Error =>
            Store.all.Tree.all.Set (Row, 2, Cached_Never_Directory);
            --          Store.Tree.Remove (Row);  Removing the directory from cache
            --          Row := Gtk.Tree_Model.Null_Iter;
            Rewind_Error
              (Store,
               Ada.Exceptions.Exception_Message (Error),
               Directory);
            Updated := True;
            Leave (Store, Directory, High);
            if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
               Trace
                 (Store,
                  Store.all.Depth,
                  "stop adding "
                  & String (Directory)
                  & " (error: "
                  & Ada.Exceptions.Exception_Message (Error)
                  &  ')');
            end if;
            return;
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               "Rewind error:"
               & Ada.Exceptions.Exception_Information (Error)
               & Where ("Add_Folder"));
            Store.all.Tree.all.Set (Row, 2, Cached_Never_Directory);
            --          Store.Tree.Remove (Row);
            --          Row := Gtk.Tree_Model.Null_Iter;
            Rewind_Error
              (Store,
               Ada.Exceptions.Exception_Message (Error),
               Directory);
            Updated := True;
            Leave (Store, Directory, High);
            if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
               Store.all.Trace
                 (Store.all.Depth,
                  "stop adding "
                  &  String (Directory)
                  &  " (fault: "
                  &  Ada.Exceptions.Exception_Information (Error)
                  &  ')');
            end if;
            return;
      end;
      --
      -- Here  we  will modify the store and we don't know if that would
      -- have an effect on the iterators of. For this reason we save the
      -- iterator as a path, which won't change and restore the iterator
      -- after each suspicious operation.
      --
      if Row /= Gtk.Tree_Model.Null_Iter then
         Path := Gtk.Tree_Store.Get_Path (Store.all.Tree, Row);
      end if;
      loop
         declare
            Data : Directory_Item renames Read (Store);
            Size : Gint;

            use type Gtk.Tree_Model.Gtk_Tree_Path;
         begin
            if 0 /= (Store.all.Tracing and Trace_Read_Items) then
               if Data.Directory then
                  Store.all.Trace
                    (Store.all.Depth,
                     String (Data.Name)
                     & "   directory, "
                     & String (Data.Kind)
                     & ", "
                     & Caching_Policy'Image (Data.Policy));
               else
                  Store.all.Trace
                    (Store.all.Depth,
                     String (Data.Name)
                     & "   file, "
                     & String (Data.Kind)
                     & ", "
                     & Caching_Policy'Image (Data.Policy));
               end if;
            end if;
            Add_Item (Store, Row, Directory, Data, False, Item, Size);
            if
              Item /= Gtk.Tree_Model.Null_Iter and then
              Data.Directory and then
              Data.Policy in Cache_Expanded .. Cache_Ahead
            then
               Recurse := Recurse + 1;
            end if;
            if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
               Row := Gtk.Tree_Model.Null_Iter;
            else
               Row := Gtk.Tree_Store.Get_Iter (Store.all.Tree, Path);
            end if;
         end;
      end loop;
   exception
      when Ada.IO_Exceptions.End_Error =>
         --
         -- No  more items. We go though the directory items in order to
         -- check if some of them are themselves directories  needed  to
         -- be cached.
         --
         if Recurse > 0 then
            Step := (Store.all.High - Store.all.Low) / Gdouble (Recurse);
         else
            Step := 0.0;
         end if;
         for
           Index in reverse 0 .. Gtk.Tree_Store.N_Children (Store.all.Tree, Row) - 1
         loop
            exit when Recurse = 0;
            Row := Store.all.Tree.all.Nth_Child (Row, Index);
            exit when Row = Gtk.Tree_Model.Null_Iter;
            case Gtk.Tree_Store.Get_Int (Store.all.Tree, Row, 2) is
               when Cached_Ahead_Directory =>
                  Store.all.High := Store.all.Low + Step;
                  Add_Folder
                    (Store,
                     Row,
                     Get_Path
                       (Store,
                        Directory,
                        Get_Name (Store.all.Tree, Row)),
                       True,
                       Updated);
                  Recurse := Recurse - 1;
               when Cached_Expanded_Directory =>
                  Store.all.High := Store.all.Low + Step;
                  if Expanded then
                     Add_Folder
                       (Store,
                        Row,
                        Get_Path
                          (Store,
                           Directory,
                           Get_Name (Store.all.Tree, Row)),
                          False,
                          Updated);
                  end if;
                  Recurse := Recurse - 1;
               when others =>
                  null;
            end case;
            if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
               Row := Gtk.Tree_Model.Null_Iter;
            else
               Row := Gtk.Tree_Store.Get_Iter (Store.all.Tree, Path);
            end if;
            declare
               Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
               use type Ada.Calendar.Time;
            begin
               if Now - Store.all.Last_Time > 0.2 then
                  Store.all.Last_Time := Now;
                  Store.all.Progress (Directory, Store.all.Low);
               end if;
            end;
         end loop;
         Gtk.Tree_Model.Path_Free (Path);
         Updated := True;
         Leave (Store, Directory, High);
         if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
            Store.all.Trace
              (Store.all.Depth,
               "stop adding " & String (Directory));
         end if;
      when Error : Ada.IO_Exceptions.Data_Error =>
         if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            Row := Gtk.Tree_Model.Null_Iter;
         else
            Row := Gtk.Tree_Store.Get_Iter (Store.all.Tree, Path);
            Gtk.Tree_Model.Path_Free (Path);
            Store.all.Tree.all.Set (Row, 2, Cached_Never_Directory);
         end if;
         Read_Error
           (Store,
            Ada.Exceptions.Exception_Message (Error),
            Directory);
         Updated := True;
         Leave (Store, Directory, High);
         if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
            Store.all.Trace
              (Store.all.Depth,
               "stop adding "
               & String (Directory)
               & " (read error: "
               & Ada.Exceptions.Exception_Message (Error)
               & ')');
         end if;
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Read error: "
            &  Ada.Exceptions.Exception_Information (Error)
            &  Where ("Add_Folder"));
         if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            Row := Gtk.Tree_Model.Null_Iter;
         else
            Row := Gtk.Tree_Store.Get_Iter (Store.all.Tree, Path);
            Gtk.Tree_Model.Path_Free (Path);
            Store.all.Tree.all.Set (Row, 2, Cached_Never_Directory);
         end if;
         Read_Error
           (Store,
            Ada.Exceptions.Exception_Message (Error),
            Directory);
         Updated := True;
         Leave (Store, Directory, High);
         if 0 /= (Store.all.Tracing and Trace_Read_Directory) then
            Trace
              (Store,
               Store.all.Depth,
               "stop adding "
               & String (Directory)
               & " (read fault: "
               & Ada.Exceptions.Exception_Message (Error)
               & ')');
         end if;
   end Add_Folder;

   procedure Add_Item
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Directory : Item_Path;
      Item      : Directory_Item;
      Update    : Boolean;
      Result    : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Size      : out Gint)
   is
      Position : Gint;
   begin
      Find (Store, Row, Directory, Item, Position, Size);
      if Position < 0 then
         Gtk.Tree_Store.Insert (Store.all.Tree, Result, Row, -Position - 1);
         Set_Item (Store, Result, Item);
         declare
            Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                     Gtk.Tree_Store.Get_Path (Store.all.Tree, Result);
         begin
            Emit (Store, Inserted_ID, Result, Path);
            if Item.Directory then
               Gtk.Tree_Model.Row_Inserted
                 (Gtk.Tree_Model.To_Interface (Store), Path, Result);
            end if;
            Gtk.Tree_Model.Path_Free (Path);
         end;
      else
         Result := Store.all.Tree.all.Nth_Child (Row, Position - 1);
         if Update then
            Set_Item (Store, Result, Item);
            if Item.Directory then
               declare
                  Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                           Store.Get_Path (Result);
               begin
                  Gtk.Tree_Model.Row_Changed
                    (Gtk.Tree_Model.To_Interface (Store), Path, Result);
                  Gtk.Tree_Model.Path_Free (Path);
               end;
            end if;
         end if;
      end if;
   end Add_Item;

   overriding procedure Adjust (Object : in out Item_Path_Reference) is
   begin
      if Object.Ptr /= null then
         Object.Ptr.all.Count := Object.Ptr.all.Count + 1;
      end if;
   end Adjust;

   procedure Cache
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Item  : Item_Path)
   is
      Best_Match, Exact_Match : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Cache (Store, Item, Best_Match, Exact_Match);
   end Cache;

   procedure Cache
     (Store       : not null access Gtk_Abstract_Directory_Record'Class;
      Item        : Item_Path;
      Best_Match  : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Exact_Match : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Updated : Boolean;
      Path    : Path_Node_Ptr;
      Current : Path_Node_Ptr;
      Length  : Positive;
      High    : Gdouble;
      Step    : Gdouble;
      Recurse : Natural := 0;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Enter (Store, Item, High);
      Best_Match  := Gtk.Tree_Model.Null_Iter;
      Exact_Match := Gtk.Tree_Model.Null_Iter;
      if Item'Length = 0 then
         return;
      end if;
      Split (Store, Item, Path, Length);
      Step := (Store.all.High - Store.all.Low) / Gdouble (Length);
      Current := Path;

      Store.all.High := Store.all.Low + Step;
      Add_Folder (Store, Exact_Match, "", False, Updated);
      Exact_Match :=
        Find
          (Store,
           Best_Match,
           "",
           Get_Name (Store, Current.all.Directory));
      while
        Exact_Match /= Gtk.Tree_Model.Null_Iter and then
        Gtk.Tree_Store.Get_Int
          (Store.all.Tree, Exact_Match, 2) in Cached_Directory
      -- A directory was found, it is cached
      loop
         Store.all.High := Store.all.Low + Step;
         Add_Folder
           (Store,
            Exact_Match,
            Current.all.Directory,
            False,
            Updated);
         exit when Exact_Match = Gtk.Tree_Model.Null_Iter;
         Best_Match := Exact_Match;
         exit when Current.all.Next = null;
         Exact_Match :=
           Find
             (Store,
              Best_Match,
              Current.all.Directory,
              Get_Name (Store, Current.all.Next.all.Directory));
         Current := Current.all.Next;
      end loop;
      Release (Path);
      Leave (Store, Item, High);
   exception
      when others =>
         Release (Path);
         Leave (Store, Item, High);
   end Cache;

   procedure Changed
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Directory : Item_Path)
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter := Find (Store, Directory);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row /= Gtk.Tree_Model.Null_Iter then
         Refresh (Store, Row);
      end if;
   end Changed;

   procedure Changed
     (Store : not null access Gtk_Abstract_Directory_Record'Class)
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      Refresh (Store, Row);
   end Changed;

   procedure Change_Selection
     (Widget : access Gtk_Directory_Items_View_Record;
      Index  : Positive;
      State  : Boolean)
   is
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter := Widget.all.Get_Iter (Index);
      Column : Positive                     := Widget.all.Get_Column (Index);
      Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Area   : Gdk.Rectangle.Gdk_Rectangle;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Non-existing item selected"
            & Where ("Set_Selection"));
      else
         Widget.all.Markup.all.Set_Extension (Row, 1, State);
         if State then
            Widget.all.Markup.all.Selected := Widget.all.Markup.all.Selected + 1;
         else
            Widget.all.Markup.all.Selected := Widget.all.Markup.all.Selected - 1;
         end if;
         --
         -- Forcing rendering the selected column
         --
         Widget.all.Columns.all.To_Columned (Row, Column);
         Path :=
           Gtk.Tree_Model.Columned_Store.Get_Path (Widget.all.Columns, Row);
         Widget.all.Get_Cell_Area
           (Path,
            Widget.all.Get_Column (Gint (Column)),
            Area);
         Gtk.Tree_Model.Path_Free (Path);
         Gdk.Window.Invalidate_Rect
           (Widget.all.Get_Bin_Window,
            Area,
            True);
      end if;
   end Change_Selection;

   function Check_Iter
     (Model : Gtk_Directory_Items_Store_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if
        Iter = Gtk.Tree_Model.Null_Iter or else
        Model.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path
      then
         return False;
      end if;
      declare
         Parent : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                    Model.Cache.all.Tree.all.Get_Path (Iter);
      begin
         if Parent = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            return False;
         end if;
         if not Gtk.Tree_Model.Up (Parent) or else
           0 /= Gtk.Tree_Model.Compare (Parent, Model.Root)
         then
            Gtk.Tree_Model.Path_Free (Parent);
            return False;
         else
            Gtk.Tree_Model.Path_Free (Parent);
            return True;
         end if;
      end;
   end Check_Iter;

   overriding function Children
     (Model  : not null access Gtk_Abstract_Directory_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Parent = Gtk.Tree_Model.Null_Iter then
         Row := Model.all.Tree.all.Get_Iter_First;
      else
         Row := Model.all.Tree.all.Children (Parent);
      end if;
      while Row /= Gtk.Tree_Model.Null_Iter loop
         exit when
           Gtk.Tree_Store.Get_Int (Model.all.Tree, Row, 2) in Cached_Directory;
         Model.all.Tree.all.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Children (Gtk_Abstract_Directory)"));
         return Gtk.Tree_Model.Null_Iter;
   end Children;

   overriding function Children
     (Model  : not null access Gtk_Directory_Items_Store_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Parent /= Gtk.Tree_Model.Null_Iter then
         return Gtk.Tree_Model.Null_Iter;
      elsif Model.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return Gtk.Tree_Model.Null_Iter;
      end if;
      Row := Model.all.Cache.all.Tree.all.Get_Iter (Model.all.Root);
      if Row = Gtk.Tree_Model.Null_Iter then
         return Gtk.Tree_Model.Null_Iter;
      end if;
      Row := Gtk.Tree_Store.Children (Model.all.Cache.all.Tree, Row);
      while Row /= Gtk.Tree_Model.Null_Iter loop
         case Gtk.Tree_Store.Get_Int (Model.all.Cache.all.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               return Row;
            when Cached_Item =>      -- Items are to be filtered
               if Model.all.View.all.Filter
                 (False,
                  Get_Name (Model.all.Cache.all.Tree, Row),
                  Get_Type (Model.all.Cache.all.Tree, Row))
               then
                  return Row;
               end if;
            when others =>
               null;
         end case;
         Model.all.Cache.all.Tree.all.Next (Row);
      end loop;
      return Gtk.Tree_Model.Null_Iter;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Children (Gtk_Directory_Items_Store)"));
         return Gtk.Tree_Model.Null_Iter;
   end Children;

   procedure Created
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Directory : Item_Path;
      Item      : Directory_Item)
   is
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter := Find (Store, Directory);
      Size : Gint;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         Add_Item (Store, Gtk.Tree_Model.Null_Iter, "", Item, False, Row, Size);
      else
         Add_Item (Store, Row, Directory, Item, False, Row, Size);
      end if;
   end Created;

   procedure Deleted
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Item  : Item_Path)
   is
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path := Gtk.Tree_Model.Null_Gtk_Tree_Path;

      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      Row  := Find (Store, Item);
      Path := Gtk.Tree_Store.Get_Path (Store.all.Tree, Row);
      Emit (Store, Deleting_ID, Row, Path);
      Store.all.Tree.all.Remove (Row);
      if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
         Gtk.Tree_Model.Row_Deleted (Gtk.Tree_Model.To_Interface (Store), Path);
         Gtk.Tree_Model.Path_Free (Path);
      end if;
      Emit (Store, Deleted_ID, String (Item));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Deleted (Gtk_Abstract_Directory)"));
   end Deleted;

   overriding procedure Deleted
     (Model : not null access Gtk_Selection_Store_Record;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      use type Gtk.Enums.Gtk_Selection_Mode;
   begin
      Gtk.Tree_Model.Extension_Store.Deleted
        (Gtk.Tree_Model.Extension_Store.Gtk_Extension_Store_Record (Model.all)'Access,
         Path);
      if Model.all.Changed then
         Model.all.Changed := False;
         if
           Model.all.Mode = Gtk.Enums.Selection_Browse and then
           Model.all.Selected = 0 and then
           Model.all.View.all.Get_Directory_Size > 0
         then
            -- Select another item
            declare
               Current : constant Natural := Model.all.View.all.Get_Current;
            begin
               if Current > 0 then
                  Model.all.View.all.Change_Selection (Current, True);
               else
                  Model.all.View.all.Change_Selection (1, True);
               end if;
            end;
         end if;
         Model.all.View.all.Selection_Changed;
      end if;
   end Deleted;

   overriding procedure Deleting
     (Model : not null access Gtk_Selection_Store_Record;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      if Get_Boolean (Model, Iter, 3) then
         Model.all.Selected := Model.all.Selected - 1;
         Model.all.Changed  := True;
      end if;
   end Deleting;

   procedure Destroy
     (Object  : access GObject_Record'Class;
      Browser : Gtk_Directory_Items_View) is
   begin
      Finalize (Browser);
   end Destroy;

   procedure Directory_Activated
     (Object  : access GObject_Record'Class;
      Params  : Glib.Values.GValues;
      Browser : Gtk_Directory_Tree_View)
   is
      use Gtk.Tree_Model;
      Row : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
              Get_Iter
                (Browser.all.Cache,
                 Convert
                   (Glib.Values.Get_Address (Glib.Values.Nth (Params, 1))));

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row /= Gtk.Tree_Model.Null_Iter then
         Gtk.Tree_Selection.Select_Iter (Get_Selection (Browser), Row);
      end if;
   end Directory_Activated;

   procedure Directory_Changed
     (Widget : not null access Gtk_Directory_Items_View_Record) is
   begin
      Directory_Tree_Handlers.Emit_By_Name
        (Widget,
         "directory-changed");
   end Directory_Changed;

   procedure Directory_Changed
     (Object  : access GObject_Record'Class;
      Browser : Gtk_Directory_Tree_View) is
   begin
      Browser.all.Last := Browser.all.This;
      Set (Browser.all.This, Browser);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Directory_Changed"));
   end Directory_Changed;

   procedure Directory_Collapsed
     (Object  : access GObject_Record'Class;
      Params  : Glib.Values.GValues;
      Browser : Gtk_Directory_Items_View)
   is
      Row  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item : Gtk.Tree_Model.Gtk_Tree_Iter;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.Tree_Model.Get_Tree_Iter (Glib.Values.Nth (Params, 1), Row);
      if Row = Gtk.Tree_Model.Null_Iter then
         return;
      end if;
      Item := To_Item (Browser.all.Content.all.Cache, Row);
      if Row = Gtk.Tree_Model.Null_Iter then
         return;
      end if;
      Item := To_Marked (Browser, Item);
      if
        Item = Gtk.Tree_Model.Null_Iter or else
        not Gtk.Missed.Is_In
          (Gtk.Tree_Model.To_Interface (Browser.all.Markup),
           Gtk.Tree_Model.Gtk_Tree_Iter'
             (Gtk.Tree_Model.Columned_Store.Get_Root (Browser.all.Columns)),
           Item)
      then
         return;
      end if;
      --
      -- A   directory   containing  the  currently  selected  one  gets
      -- collapsed. We set the  selection  to  the  collapsed  directory
      -- which in turn chages the current directory.
      --
      Gtk.Tree_Selection.Select_Iter
        (Get_Selection (Browser.all.Directories), Row);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Directory_Collapsed"));
   end Directory_Collapsed;

   procedure Directory_Expanded
     (Object  : access GObject_Record'Class;
      Params  : Glib.Values.GValues;
      Browser : Gtk_Directory_Tree_View)
   is
      Wait    : Gtk.Missed.Wait_Cursor (+Browser);
      Updated : Boolean;
      Row     : Gtk.Tree_Model.Gtk_Tree_Iter;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Row := Gtk.Tree_Model.Get_Tree_Iter (Glib.Values.Nth (Params, 1));
      if Row /= Gtk.Tree_Model.Null_Iter then
         Row := To_Item (Browser.all.Cache, Row);
         if 0 /= (Trace_Expand_Directory and Browser.all.Cache.all.Tracing) then
            Trace
              (Browser.all.Cache,
               Browser.all.Cache.all.Depth,
               "Expanding "
               & String (Get_Name (Browser.all.Cache.all.Tree, Row))
               & " caching...");
         end if;
         Add_Folder
           (Browser.all.Cache,
            Row,
            Get_Path (Browser.all.Cache, Row),
            True,
            Updated);
         if 0 /= (Trace_Expand_Directory and Browser.all.Cache.all.Tracing) then
            Trace
              (Browser.all.Cache,
               Browser.all.Cache.all.Depth,
               "Expanding "
               & String (Get_Name (Browser.all.Cache.all.Tree, Row))
               & " cached");
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Directory_Expanded"));
   end Directory_Expanded;

   procedure Directory_Refreshed
     (Object  : access GObject_Record'Class;
      Params  : Glib.Values.GValues;
      Browser : Gtk_Directory_Tree_View)
   is
      Wait : Gtk.Missed.Wait_Cursor (+Browser);
      Path : constant String :=
               Glib.Values.Get_String (Glib.Values.Nth (Params, 1));
   begin
      Set_Current_Directory (Browser, Item_Path (Path));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Directory_Refreshed"));
   end Directory_Refreshed;

   procedure Edited_Directory
     (Object  : access GObject_Record'Class;
      Params  : Glib.Values.GValues;
      Browser : Gtk_Directory_Tree_View)
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter :=
              Gtk.Tree_Model.Get_Iter_From_String
                (Gtk.Tree_Model.To_Interface (Browser.all.Cache),
                 Glib.Values.Get_String (Glib.Values.Nth (Params, 1)));

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         return;
      end if;
      Row := To_Item (Browser.all.Cache, Row);
      if Row = Gtk.Tree_Model.Null_Iter then
         return;
      end if;
      Name_Commit
        (Browser,
         Get_Path (Browser.all.Cache, Row),
         Item_Name (Glib.Values.Get_String (Glib.Values.Nth (Params, 2))));
   end Edited_Directory;

   procedure Edited_Item
     (Object  : access GObject_Record'Class;
      Params  : Glib.Values.GValues;
      Column  : Column_Data)
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter :=
              Gtk.Tree_Model.Get_Iter_From_String
                (Gtk.Tree_Model.To_Interface (Column.Browser.all.Columns),
                 Glib.Values.Get_String (Glib.Values.Nth (Params, 1)));
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         return;
      end if;
      Row :=
        Gtk.Tree_Model.Columned_Store.From_Columned
          (Column.Browser.all.Columns,
           Row,
           Column.Column);
      if Row = Gtk.Tree_Model.Null_Iter then
         return;
      end if;
      Name_Commit
        (Column.Browser,
         Positive
           (Gtk.Missed.Get_Row_No
                (Gtk.Tree_Model.To_Interface (Column.Browser.all.Markup), Row) + 1),
         Item_Name (Glib.Values.Get_String (Glib.Values.Nth (Params, 2))));
   end Edited_Item;

   procedure EmitV
     (Params : System.Address;
      Signal : Signal_Id;
      Quark  : GQuark;
      Result : System.Address);
   pragma Import (C, EmitV, "g_signal_emitv");

   procedure Emit
     (Store  : not null access Gtk_Abstract_Directory_Record'Class;
      Signal : Signal_Id;
      Text   : UTF8_String;
      Path   : Item_Path)
   is
      Params : Glib.Values.GValue_Array (0 .. 2);
      Result : Glib.Values.GValue;
   begin
      Glib.Values.Init (Params (0), Get_Cache_Model_Type);
      Glib.Values.Set_Object (Params (0), Store);
      Glib.Values.Init (Params (1), GType_String);
      Glib.Values.Set_String (Params (1), Text);
      Glib.Values.Init (Params (2), GType_String);
      Glib.Values.Set_String (Params (2), String (Path));
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Glib.Values.Unset (Params (0));
      Glib.Values.Unset (Params (1));
      Glib.Values.Unset (Params (2));
   end Emit;

   procedure Emit
     (Store  : not null access Gtk_Abstract_Directory_Record'Class;
      Signal : Signal_Id;
      Path   : Item_Path;
      Value  : Gdouble)
   is
      Params : Glib.Values.GValue_Array (0 .. 2);
      Result : Glib.Values.GValue;
   begin
      Glib.Values.Init (Params (0), Get_Cache_Model_Type);
      Glib.Values.Set_Object (Params (0), Store);
      Glib.Values.Init (Params (1), GType_String);
      Glib.Values.Set_String (Params (1), String (Path));
      Glib.Values.Init (Params (2), GType_Double);
      Glib.Values.Set_Double (Params (2), Value);
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Glib.Values.Unset (Params (0));
      Glib.Values.Unset (Params (1));
      Glib.Values.Unset (Params (2));
   end Emit;

   procedure Emit
     (Store  : not null access Gtk_Abstract_Directory_Record'Class;
      Signal : Signal_Id;
      Path   : Item_Path)
   is
      Params : Glib.Values.GValue_Array (0 .. 1);
      Result : Glib.Values.GValue;
   begin
      Glib.Values.Init (Params (0), Get_Cache_Model_Type);
      Glib.Values.Set_Object (Params (0), Store);
      Glib.Values.Init (Params (1), GType_String);
      Glib.Values.Set_String (Params (1), String (Path));
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Glib.Values.Unset (Params (0));
      Glib.Values.Unset (Params (1));
   end Emit;

   procedure Emit
     (Store  : not null access Gtk_Abstract_Directory_Record'Class;
      Signal : Signal_Id;
      Text   : UTF8_String)
   is
      Params : Glib.Values.GValue_Array (0 .. 1);
      Result : Glib.Values.GValue;
   begin
      Glib.Values.Init (Params (0), Get_Cache_Model_Type);
      Glib.Values.Set_Object (Params (0), Store);
      Glib.Values.Init (Params (1), GType_String);
      Glib.Values.Set_String (Params (1), Text);
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Glib.Values.Unset (Params (0));
      Glib.Values.Unset (Params (1));
   end Emit;

   procedure Emit
     (Store  : not null access Gtk_Abstract_Directory_Record'Class;
      Signal : Signal_Id;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path   : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      Params : Glib.Values.GValue_Array (0 .. 2);
      Iter   : aliased Gtk.Tree_Model.Gtk_Tree_Iter := Row;
      Result : Glib.Values.GValue;
   begin
      Glib.Values.Init (Params (0), Get_Cache_Model_Type);
      Glib.Values.Set_Object (Params (0), Store);
      Glib.Values.Init (Params (1), GType_Pointer);
      Glib.Values.Set_Address (Params (1), Iter'Address);
      Glib.Values.Init (Params (2), Gtk.Tree_Model.Path_Get_Type);
      Glib.Values.Set_Address (Params (2), Get_Object (Path));
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Glib.Values.Unset (Params (0));
      Glib.Values.Unset (Params (1));
      --    Glib.Values.Unset (Params (2));
   end Emit;

   procedure Emit
     (Store  : not null access Gtk_Abstract_Directory_Record'Class;
      Signal : Signal_Id;
      Row    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Text   : String)
   is
      Params : Glib.Values.GValue_Array (0 .. 2);
      Iter   : aliased Gtk.Tree_Model.Gtk_Tree_Iter := Row;
      Result : Glib.Values.GValue;
   begin
      Glib.Values.Init (Params (0), Get_Cache_Model_Type);
      Glib.Values.Set_Object (Params (0), Store);
      Glib.Values.Init (Params (1), GType_Pointer);
      Glib.Values.Set_Address (Params (1), Iter'Address);
      Glib.Values.Init (Params (2), GType_String);
      Glib.Values.Set_String (Params (2), Text);
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Glib.Values.Unset (Params (0));
      Glib.Values.Unset (Params (1));
      Glib.Values.Unset (Params (2));
   end Emit;

   procedure Expand_Folder
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Directory : Item_Path)
   is
      Updated : Boolean;
      Count   : Gint;
      Item    : Gtk.Tree_Model.Gtk_Tree_Iter :=
                  Store.all.Tree.all.Nth_Child (Row, 0);
      High    : Gdouble;
      Step    : Gdouble;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Enter (Store, Directory, High);
      Count := Gtk.Tree_Store.N_Children (Store.all.Tree, Row);
      if Count > 0 then
         Step := (Store.all.High - Store.all.Low) / Gdouble (Count);
      else
         Step := 0.0;
      end if;
      Count := 0;
      while Item /= Gtk.Tree_Model.Null_Iter loop
         if
           Gtk.Tree_Store.Get_Int (Store.all.Tree, Item, 2) in Cached_Children
         then
            Store.all.High := Store.all.Low + Step;
            Add_Folder
              (Store,
               Item,
               Store.all.Get_Path (Directory, Get_Name (Store.all.Tree, Item)),
               False,
               Updated);
            if Item /= Gtk.Tree_Model.Null_Iter then
               Count := Count + 1;
            end if;
         else
            Count := Count + 1;
         end if;
         Item := Store.all.Tree.all.Nth_Child (Row, Count);
         declare
            Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
            use type Ada.Calendar.Time;
         begin
            if Now - Store.all.Last_Time > 0.2 then
               Store.all.Last_Time := Now;
               Store.all.Progress (Directory, Store.all.Low);
            end if;
         end;
      end loop;
      Leave (Store, Directory, High);
   exception
      when Error : others =>
         Leave (Store, Directory, High);
         raise;
   end Expand_Folder;

   function Filter
     (Widget    : not null access Gtk_Directory_Items_View_Record;
      Directory : Boolean;
      Name      : Item_Name;
      Kind      : Item_Type) return Boolean is
   begin
      return True;
   end Filter;

   overriding procedure Finalize
     (Model : not null access Gtk_Abstract_Directory_Record)
   is
      use type Gtk.Tree_Store.Gtk_Tree_Store;
   begin
      Gtk.Tree_Model.Abstract_Store.Gtk_Abstract_Model_Record (Model.all).Finalize;
      if Model.all.Tree /= null then
         Gtk.Tree_Store.Unref (Model.all.Tree);
         Model.all.Tree := null;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Finalize (Gtk_Abstract_Directory_Record)"));
   end Finalize;

   overriding procedure Finalize
     (Model : not null access Gtk_Directory_Items_Store_Record) is
   begin
      Gtk.Tree_Model.Abstract_Store.Gtk_Abstract_Model_Record (Model.all).Finalize;
      if Model.all.Cache /= null then
         Unref (Model.all.Cache);
         Model.all.Cache := null;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Finalize (Gtk_Directory_Items_Store)"));
   end Finalize;

   procedure Finalize
     (Widget : not null access Gtk_Directory_Items_View_Record)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Gtk_Cell_Renderer_Text_Array,
           Gtk_Cell_Renderer_Text_Array_Ptr);

      use type Gtk.Tree_Model.Columned_Store.Gtk_Columned_Store;
   begin
      if Widget.all.Columns /= null then
         Gtk.Tree_Model.Columned_Store.Unref (Widget.all.Columns);
         Widget.all.Columns := null;
      end if;
      if Widget.all.Markup /= null then
         Unref (Widget.all.Markup);
         Widget.all.Markup := null;
      end if;
      if Widget.all.Directories /= null then
         Unref (Widget.all.Directories);
      end if;
      Free (Widget.all.Name_Renderers);
      if Widget.all.Content /= null then
         Unref (Widget.all.Content);
         Widget.all.Content := null;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Finalize (Gtk_Directory_Items_View)"));
   end Finalize;

   overriding procedure Finalize (Object : in out Item_Path_Reference) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Item_Path_Object,
           Item_Path_Object_Ptr);
   begin
      if Object.Ptr /= null then
         Object.Ptr.all.Count := Object.Ptr.all.Count - 1;
         if Object.Ptr.all.Count = 0 then
            Free (Object.Ptr);
         else
            Object.Ptr := null;
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Finalize (Item_Path_Reference)"));
   end Finalize;

   function Find
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Path  : Item_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Path'Length = 0 then
         return Gtk.Tree_Model.Null_Iter;
      end if;
      declare
         Directory : Item_Path renames Store.all.Get_Directory (Path);
         Row       : Gtk.Tree_Model.Gtk_Tree_Iter := Find (Store, Directory);
      begin
         if Row /= Gtk.Tree_Model.Null_Iter then
            Row := Find (Store, Row, Directory, Get_Name (Store, Path));
         end if;
         return Row;
      end;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return Find (Store,
                      Gtk.Tree_Model.Null_Iter,
                      "",
                      Get_Name (Store, Path));
   end Find;

   function Find
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Directory : Item_Path;
      Name      : Item_Name) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Aim  : constant Directory_Item :=
               (Directory   => False,
                Policy      => Cache_Ahead,
                Name_Length => Name'Length,
                Name        => Name,
                Kind_Length => 0,
                Kind        => "");

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         Iter := Gtk.Tree_Store.Get_Iter_First (Store.all.Tree);
      else
         Iter := Store.all.Tree.all.Children (Row);
      end if;
      while Iter /= Gtk.Tree_Model.Null_Iter loop
         declare
            Name : constant Item_Name := Get_Name (Store.all.Tree, Iter);

            use type Gtk.Missed.Row_Order;
         begin
            exit when
              Gtk.Missed.Equal =
                Compare
                  (Store,
                   Directory,
                   Aim,
                   (Directory   => False,
                    Policy      => Cache_Ahead,
                    Name_Length => Name'Length,
                    Name        => Name,
                    Kind_Length => 0,
                    Kind        => ""),
                   True);
         end;
         Gtk.Tree_Store.Next (Store.all.Tree, Iter);
      end loop;
      return Iter;
   end Find;

   procedure Find
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Directory : Item_Path;
      Item      : Directory_Item;
      Position  : out Gint;
      Size      : out Gint)
   is
      Upper : Gint := Store.all.Tree.all.N_Children (Row);
      Lower : Gint := -1;
      This  : Gint;
      That  : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Size := Upper;
      if Upper = 0 then
         Position := -1;
         return;
      end if;
      loop
         This := (Upper + Lower) / 2;
         That := Store.all.Tree.all.Nth_Child (Row, This);
         declare
            Cached : constant Directory_Item := Store.all.Get_Item (That);
         begin
            case Compare (Store, Directory, Item, Cached, False) is
               when Gtk.Missed.Before =>
                  Upper := This;
                  if Upper - Lower <= 1 then
                     Position := -(This + 1);
                     return;
                  end if;
               when Gtk.Missed.Equal =>
                  Position := This + 1;
                  return;
               when Gtk.Missed.After =>
                  Lower := This;
                  if Upper - Lower <= 1 then
                     Position := -(This + 2);
                     return;
                  end if;
            end case;
         end;
      end loop;
   end Find;

   function Find
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Row       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Directory : Item_Path;
      Item      : Directory_Item) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Position : Gint;
      Size     : Gint;
   begin
      Find (Store, Row, Directory, Item, Position, Size);
      if Position < 0 then
         return Gtk.Tree_Model.Null_Iter;
      else
         return Store.all.Tree.all.Nth_Child (Row, Position - 1);
      end if;
   end Find;

   procedure From_Filtered
     (Model      : not null access Gtk_Directory_Items_Store_Record;
      Unfiltered : out Gtk.Tree_Model.Gtk_Tree_Iter;
      Filtered   : Gtk.Tree_Model.Gtk_Tree_Iter) is
   begin
      Unfiltered := Filtered;
   end From_Filtered;

   function From_Item
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Item  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      return Item;
   end From_Item;

   function From_Marked
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Item   : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Widget.all.Content.all.From_Filtered
        (Row,
         Widget.all.Markup.all.From_Extension (Item));
      return Row;
   end From_Marked;

   function Get (Object : Item_Path_Reference) return Item_Path is
   begin
      if Object.Ptr = null then
         return "";
      else
         return Object.Ptr.all.Path;
      end if;
   end Get;

   function Get_Cache
     (Widget : not null access Gtk_Directory_Tree_View_Record)
      return Gtk_Abstract_Directory is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Cache
     (Widget : not null access Gtk_Directory_Items_View_Record)
      return Gtk_Abstract_Directory is
   begin
      return Widget.all.Content.all.Cache;
   end Get_Cache;

   function Get_Cached
     (Store : not null access Gtk_Abstract_Directory_Record;
      Path  : Item_Path) return Directory_Item
   is
      Row : constant Gtk.Tree_Model.Gtk_Tree_Iter := Find (Store, Path);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         raise Constraint_Error;
      else
         return Get_Item (Store, Row);
      end if;
   end Get_Cached;

   function Get_Cache_Model_Type return Gtk_Type is
      use Interfaces.C.Strings;

      function Lookup (Index : Interfaces.C.size_t) return Signal_Id is
      begin
         return Lookup
           (Cache_Model_Type,
            Glib.Signal_Name
              (String'
                   (Value
                        (Abstract_Directory_Signal_Names (Index)))));
      end Lookup;
   begin
      if Cache_Model_Type = GType_Invalid then
         Cache_Model_Type :=
           Gtk.Tree_Model.Abstract_Store.Register
             (Abstract_Directory_Class_Name,
              Abstract_Directory_Signal_Names,
              Abstract_Directory_Signal_Parameters);
         Read_Error_ID   := Lookup (0);
         Rewind_Error_ID := Lookup (1);
         Refreshed_ID    := Lookup (2);
         Progress_ID     := Lookup (3);
         Inserted_ID     := Lookup (4);
         Renamed_ID      := Lookup (5);
         Deleting_ID     := Lookup (6);
         Deleted_ID      := Lookup (7);
      end if;
      return Cache_Model_Type; -- Registering the GTK+ type
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Cache_Model_Type"));
         raise;
   end Get_Cache_Model_Type;

   function Get_Column
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Positive
   is
      Item   : Gtk.Tree_Model.Gtk_Tree_Iter := Widget.all.Get_Iter (Index);
      Column : Positive;
   begin
      Widget.all.Columns.all.To_Columned (Item, Column);
      return Column;
   end Get_Column;

   function Get_Columns
     (Widget : not null access Gtk_Directory_Items_View_Record) return Positive
   is
   begin
      return Widget.all.Columns.all.Get_Major_Columns;
   end Get_Columns;

   overriding function Get_Column_Type
     (Model : not null access Gtk_Abstract_Directory_Record;
      Index : Gint) return GType is
   begin
      return Gtk.Tree_Store.Get_Column_Type (Model.all.Tree, Index);
   end Get_Column_Type;

   overriding function Get_Column_Type
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Index : Gint) return GType is
   begin
      return Model.all.Cache.all.Tree.all.Get_Column_Type (Index);
   end Get_Column_Type;

   function Get_Current
     (Widget : not null access Gtk_Directory_Items_View_Record) return Natural
   is
      Columned_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Markup_Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Column        : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Column_No     : Gint;

      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      Widget.all.Get_Cursor (Columned_Path, Column);
      if Columned_Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return 0;
      end if;
      Column_No := Gtk.Missed.Get_Column_No (Widget, Column);
      if Column_No < 0 then
         Gtk.Tree_Model.Path_Free (Columned_Path);
         return 0;
      end if;
      Markup_Path :=
        Widget.all.Columns.all.From_Columned
          (Columned_Path,
           Positive (Column_No + 1));
      if Markup_Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         Gtk.Tree_Model.Path_Free (Columned_Path);
         return 0;
      end if;
      declare
         Indices : Gint_Array renames Gtk.Tree_Model.Get_Indices (Markup_Path);
         Result  : Natural := 0;
      begin
         if Indices'Length > 0 then
            Result := Natural (Indices (Indices'Last) + 1);
         end if;
         Gtk.Tree_Model.Path_Free (Markup_Path);
         Gtk.Tree_Model.Path_Free (Columned_Path);
         return Result;
      end;
   end Get_Current;

   function Get_Current_Directory
     (Widget : not null access Gtk_Directory_Tree_View_Record) return Item_Path
   is
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Model : Gtk.Tree_Model.Gtk_Tree_Model;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Widget.all.Get_Selection.all.Get_Selected (Model, Row);
      if Row = Gtk.Tree_Model.Null_Iter then
         raise Ada.IO_Exceptions.Name_Error;
      end if;
      return Widget.all.Cache.Get_Path (To_Item (Widget.all.Cache, Row));
   end Get_Current_Directory;

   function Get_Depth
     (Store : not null access Gtk_Abstract_Directory_Record'Class)
      return Natural is
   begin
      return Store.all.Depth;
   end Get_Depth;

   function Get_Directory
     (Widget : not null access Gtk_Directory_Items_View_Record) return Item_Path
   is
      Current : constant Gtk.Tree_Model.Gtk_Tree_Iter := Widget.all.Get_Directory;
   begin
      return Get_Path (Widget.all.Content.all.Cache, Current);
   end Get_Directory;

   function Get_Directory
     (Widget : not null access Gtk_Directory_Items_View_Record)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Widget.all.Content.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return Gtk.Tree_Model.Null_Iter;
      else
         return
           Widget.all.Content.all.Cache.all.Tree.all.Get_Iter (Widget.all.Content.all.Root);
      end if;
   end Get_Directory;

   function Get_Directory_Items_View_Type return Gtk_Type is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Tree_View.Get_Type,
         Class_Record => Directory_Items_Class,
         Type_Name    => Directory_Items_Class_Name,
         Signals      => Directory_Items_Signal_Names);
      return Directory_Items_Class.all.The_Type;
   end Get_Directory_Items_View_Type;

   function Get_Directory_Size
     (Widget : not null access Gtk_Directory_Items_View_Record) return Natural
   is
   begin
      return
        Natural (Widget.all.Markup.all.N_Children (Widget.all.Columns.all.Get_Root));
   end Get_Directory_Size;

   function Get_Directory_Tree_View
     (Widget : not null access Gtk_Directory_Items_View_Record)
      return Gtk_Directory_Tree_View is
   begin
      return Widget.all.Directories;
   end Get_Directory_Tree_View;

   function Get_Directory_Tree_View_Type return Gtk_Type is
   begin
      return Gtk.Tree_View.Get_Type;
   end Get_Directory_Tree_View_Type;

   overriding function Get_Flags
     (Model : not null access Gtk_Abstract_Directory_Record)
      return Gtk.Tree_Model.Tree_Model_Flags
   is
      use type Gtk.Tree_Model.Tree_Model_Flags;
   begin
      return Model.all.Tree.all.Get_Flags and not Gtk.Tree_Model.Tree_Model_Iters_Persist;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Flags"));
         return Gtk.Tree_Model.Tree_Model_List_Only;
   end Get_Flags;

   overriding function Get_Flags
     (Model : not null access Gtk_Directory_Items_Store_Record)
      return Gtk.Tree_Model.Tree_Model_Flags is
   begin
      return Gtk.Tree_Model.Tree_Model_List_Only;
   end Get_Flags;

   function Get_Icon
     (Widget       : not null access Gtk_Directory_Tree_View_Record;
      Kind         : Item_Type;
      Expanded     : Boolean;
      Has_Children : Boolean;
      Topmost      : Boolean) return Icon_Data
   is
      This : constant String := String (Kind);
   begin
      if not Has_Children then
         return (Stock_ID, Gtk.Stock.Stock_Stop'Length, Gtk.Stock.Stock_Stop);
      elsif Expanded and then This = Gtk.Stock.Stock_Directory then
         return (Stock_ID, Gtk.Stock.Stock_Open'Length, Gtk.Stock.Stock_Open);
      else
         return (Stock_ID, This'Length, This);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Icon (Gtk_Directory_Tree_View)"));
         return
           (Stock_ID,
            Gtk.Stock.Stock_Dialog_Error'Length,
            Gtk.Stock.Stock_Dialog_Error);
   end Get_Icon;

   function Get_Icon
     (Widget       : not null access Gtk_Directory_Items_View_Record;
      Name         : Item_Name;
      Kind         : Item_Type;
      Directory    : Boolean;
      Has_Children : Boolean) return Icon_Data is
   begin
      if not Directory or else Has_Children then
         return (Stock_ID, Kind'Length, String (Kind));
      else
         return (Stock_ID,
                 Gtk.Stock.Stock_Cancel'Length,
                 Gtk.Stock.Stock_Cancel);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Icon (Gtk_Directory_Items_View)"));
         return (Stock_ID,
                 Gtk.Stock.Stock_Cancel'Length,
                 Gtk.Stock.Stock_Cancel);
   end Get_Icon;

   function Get_Index
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Row    : Positive;
      Column : Positive) return Natural is
   begin
      return
        Natural
          (Gtk.Missed.Get_Row_No
             (Gtk.Tree_Model.To_Interface (Widget.all.Markup),
              Widget.all.Columns.all.Get_Reference_Iter (Row, Column)) + 1);
   end Get_Index;

   function Get_Index
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Name   : Item_Name) return Natural
   is
      Row       : Gtk.Tree_Model.Gtk_Tree_Iter := Widget.all.Get_Directory;
      Directory : Item_Path renames Widget.all.Get_Directory;
      Index     : Natural := 0;
      Aim       : constant Directory_Item :=
                    (Directory   => False,
                     Policy      => Cache_Ahead,
                     Name_Length => Name'Length,
                     Name        => Name,
                     Kind_Length => 0,
                     Kind        => "");
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Row := Widget.all.To_Marked (Row);
      if Row = Gtk.Tree_Model.Null_Iter then
         return 0;
      end if;
      Row := Widget.all.Markup.all.Children (Row);
      while Row /= Gtk.Tree_Model.Null_Iter loop
         Index := Index + 1;
         declare
            Name : constant Item_Name :=
                     Get_Name (Gtk.Tree_Model.To_Interface (Widget.all.Markup), Row);

            use type Gtk.Missed.Row_Order;
         begin
            if
              Gtk.Missed.Equal =
                Compare
                  (Widget.all.Content.all.Cache,
                   Directory,
                   Aim,
                   (Directory   => False,
                    Policy      => Cache_Ahead,
                    Name_Length => Name'Length,
                    Name        => Name,
                    Kind_Length => 0,
                    Kind        => ""),
                   True)
            then
               return Index;
            end if;
         end;
         Next (Widget.all.Markup, Row);
      end loop;
      return 0;
   end Get_Index;

   function Get_Item
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Item  : Gtk.Tree_Model.Gtk_Tree_Iter) return Directory_Item
   is
      Kind : constant Item_Type := Get_Type (Store.all.Tree, Item);
      Name : constant Item_Name := Get_Name (Store.all.Tree, Item);
   begin
      return
        (Policy      => Cache_Expanded,
         Name_Length => Name'Length,
         Name        => Name,
         Kind_Length => Kind'Length,
         Kind        => Kind,
         Directory   =>
             Gtk.Tree_Store.Get_Int (Store.all.Tree, Item, 2) in Cached_Directory);
   end Get_Item;

   function Get_Items_Model_Type return Gtk_Type is
      use Interfaces.C.Strings;
   begin
      if Items_Model_Type = GType_Invalid then
         Items_Model_Type :=
           Gtk.Tree_Model.Abstract_Store.Register ("GtkDirectoryItemsStore");
      end if;
      return Items_Model_Type; -- Registering the GTK+ type
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Items_Model_Type"));
         raise;
   end Get_Items_Model_Type;

   overriding function Get_Iter
     (Model : not null access Gtk_Abstract_Directory_Record;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return Gtk.Tree_Model.Null_Iter;
      end if;
      declare
         Indices : constant Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
         Row     : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
      begin
         for Level in Indices'Range loop
            Row := Children (Model, Row);
            exit when Row = Gtk.Tree_Model.Null_Iter;
            for Child in 1 .. Indices (Level) loop
               Next (Model, Row);
               exit when Row = Gtk.Tree_Model.Null_Iter;
            end loop;
         end loop;
         if 0 /= (Model.all.Tracing and Trace_Cache) then
            declare
               Other : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                         Get_Path (Model, Row);
            begin
               if Other /= Path then
                  Glib.Messages.Log
                    (Gtk.Missed.GtkAda_Contributions_Domain,
                     Glib.Messages.Log_Level_Critical,
                     "Inconsistent iterator for path "
                     & Gtk.Tree_Model.To_String (Path)
                     & " at "
                     & String (Get_Name (Model.all.Tree, Row)));
               end if;
               Gtk.Tree_Model.Path_Free (Other);
            end;
         end if;
         return Row;
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Iter (Gtk_Abstract_Directory)"));
         return Gtk.Tree_Model.Null_Iter;
   end Get_Iter;

   overriding function Get_Iter
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
   begin
      if Indices'Length = 1 then -- The index is the child number
         return
           Model.all.Nth_Child
             (Gtk.Tree_Model.Null_Iter, Indices (Indices'First));
      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Iter (Gtk_Directory_Items_Store)"));
         return Gtk.Tree_Model.Null_Iter;
   end Get_Iter;

   function Get_Iter
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Row : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
              Widget.all.Markup.all.Nth_Child
                (Widget.all.Columns.all.Get_Root,
                 Gint (Index) - 1);
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         raise Constraint_Error;
      else
         return Row;
      end if;
   end Get_Iter;

   overriding function Get_N_Columns
     (Model : not null access Gtk_Abstract_Directory_Record) return Gint is
   begin
      return Gtk.Tree_Store.Get_N_Columns (Model.all.Tree);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_N_Columns (Gtk_Abstract_Directory)"));
         return 0;
   end Get_N_Columns;

   overriding function Get_N_Columns
     (Model : not null access Gtk_Directory_Items_Store_Record) return Gint is
   begin
      return Gtk.Tree_Store.Get_N_Columns (Model.all.Cache.all.Tree);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_N_Columns (Gtk_Directory_Items_Store)"));
         return 0;
   end Get_N_Columns;

   function Get_Name
     (Widget       : not null access Gtk_Directory_Tree_View_Record;
      Name         : Item_Name;
      Kind         : Item_Type;
      Expanded     : Boolean;
      Has_Children : Boolean;
      Topmost      : Boolean) return Item_Name is
   begin
      return Name;
   end Get_Name;

   function Get_Name
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Item_Name is
   begin
      return Get_Name
        (Gtk.Tree_Model.To_Interface (Widget.all.Markup),
         Widget.all.Get_Iter (Index));
   end Get_Name;

   overriding function Get_Path
     (Model : not null access Gtk_Abstract_Directory_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
      end if;
      declare
         Path    : Gtk.Tree_Model.Gtk_Tree_Path :=
                     Gtk.Tree_Store.Get_Path (Model.all.Tree, Iter);
         Indices : constant Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
         Row     : Gtk.Tree_Model.Gtk_Tree_Iter;
         Count   : Gint;
      begin
         Gtk.Tree_Model.Path_Free (Path);
         Gtk.Tree_Model.Gtk_New (Path);
         for Level in Indices'Range loop
            if Level = Indices'First then
               Row := Model.all.Tree.all.Get_Iter_First;
            else
               Row := Model.all.Tree.all.Children (Row);
            end if;
            Count := 0;
            if Row = Gtk.Tree_Model.Null_Iter then
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Invalid iterator: no children at the level"
                  & Natural'Image (Level)
                  & Where ("Get_Path (Gtk_Abstract_Directory)"));
               Gtk.Tree_Model.Path_Free (Path);
               return Gtk.Tree_Model.Null_Gtk_Tree_Path;
            end if;
            for Child in 1 .. Indices (Level) loop
               if
                 Gtk.Tree_Store.Get_Int (Model.all.Tree, Row, 2) in Cached_Directory
               then
                  Count := Count + 1;
               end if;
               Model.all.Tree.all.Next (Row);
               if Row = Gtk.Tree_Model.Null_Iter then
                  Glib.Messages.Log
                    (Gtk.Missed.GtkAda_Contributions_Domain,
                     Glib.Messages.Log_Level_Critical,
                     "Invalid iterator: less than"
                     & Gint'Image (Indices (Level))
                     & " children at the level"
                     & Natural'Image (Level)
                     & Where ("Get_Path (Gtk_Abstract_Directory)"));
                  Gtk.Tree_Model.Path_Free (Path);
                  return Gtk.Tree_Model.Null_Gtk_Tree_Path;
               end if;
            end loop;
            if
              Gtk.Tree_Store.Get_Int (Model.all.Tree, Row, 2) not in Cached_Directory
            then
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Invalid iterator: the child"
                  & Gint'Image (Indices (Level))
                  & " at the level"
                  & Natural'Image (Level)
                  & " is not a directory"
                  & Where ("Get_Path (Gtk_Abstract_Directory)"));
               Gtk.Tree_Model.Path_Free (Path);
               return Gtk.Tree_Model.Null_Gtk_Tree_Path;
            end if;
            Gtk.Tree_Model.Append_Index (Path, Count);
         end loop;
         if 0 /= (Model.all.Tracing and Trace_Cache) then
            if Get_Iter (Model, Path) /= Row then
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Inconsistent path "
                  & Gtk.Tree_Model.To_String (Path)
                  & " of "
                  & String (Get_Name (Model.all.Tree, Row)));
            end if;
         end if;
         return Path;
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Path (Gtk_Abstract_Directory)"));
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
   end Get_Path;

   overriding function Get_Path
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if
        Iter = Gtk.Tree_Model.Null_Iter or else
        Model.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path
      then
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
      end if;
      declare
         Unfiltered : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                        Model.all.Cache.all.Tree.all.Get_Path (Iter);
         Filtered   : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                        Model.To_Filtered (Unfiltered);
      begin
         Gtk.Tree_Model.Path_Free (Unfiltered);
         return Filtered;
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Path (Gtk_Directory_Items_Store_Record)"));
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
   end Get_Path;

   function Get_Path
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Name   : Item_Name) return Item_Path is
   begin
      return Widget.all.Content.all.Cache.all.Get_Path (Widget.all.Get_Directory, Name);
   end Get_Path;

   function Get_Path
     (Store  : not null access Gtk_Abstract_Directory_Record'Class;
      Item   : Gtk.Tree_Model.Gtk_Tree_Iter) return Item_Path
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Item = Gtk.Tree_Model.Null_Iter then
         return "";
      end if;
      declare
         Folder : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                    Store.all.Tree.all.Parent (Item);
         Name   : constant Item_Name := Get_Name (Store.all.Tree, Item);
      begin
         if Folder = Gtk.Tree_Model.Null_Iter then
            return Store.all.Get_Path ("", Name);
         else
            return Store.all.Get_Path (Store.Get_Path (Folder), Name);
         end if;
      end;
   end Get_Path;

   function Get_Path
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Item_Path is
   begin
      return
        Widget.all.Content.all.Cache.Get_Path
          (Widget.all.From_Marked (Widget.all.Get_Iter (Index)));
   end Get_Path;

   function Get_Path_Before
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Tree  : constant Gtk.Tree_Store.Gtk_Tree_Store :=
                Model.all.Cache.all.Tree;
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Count : Gint := 0;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Model.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
      end if;
      Row := Gtk.Tree_Store.Get_Iter (Tree, Model.all.Root);
      if Row = Gtk.Tree_Model.Null_Iter then
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
      end if;
      Row := Gtk.Tree_Store.Children (Tree, Row);
      while Row /= Gtk.Tree_Model.Null_Iter loop
         if Row = Iter then
            declare
               Path : Gtk.Tree_Model.Gtk_Tree_Path;
            begin
               Gtk.Tree_Model.Gtk_New (Path);
               Gtk.Tree_Model.Append_Index (Path, Count);
               return Path;
            end;
         end if;
         case Gtk.Tree_Store.Get_Int (Model.all.Cache.all.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               Count := Count + 1;
            when Cached_Item =>      -- Items are to be filtered
               if
                 Model.all.View.all.Filter
                   (False,
                    Get_Name (Model.all.Cache.all.Tree, Row),
                    Get_Type (Model.all.Cache.all.Tree, Row))
               then
                  Count := Count + 1;
               end if;
            when others =>
               null;
         end case;
         Gtk.Tree_Store.Next (Tree, Row);
      end loop;
      return Gtk.Tree_Model.Null_Gtk_Tree_Path;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Path_Before"));
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
   end Get_Path_Before;

   procedure Get_Position
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive;
      Row    : out Positive;
      Column : out Positive)
   is
      No   : Gint;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter :=
               Widget.all.Markup.all.Nth_Child
                 (Widget.all.Columns.all.Get_Root,
                  Gint (Index) - 1);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Iter /= Gtk.Tree_Model.Null_Iter then
         Widget.all.Columns.all.To_Columned (Iter, Column);
         No :=
           Gtk.Missed.Get_Row_No
             (Gtk.Tree_Model.To_Interface (Widget.all.Columns), Iter);

         if No >= 0 then
            Row := Positive (No + 1);
            return;
         end if;
      end if;
      raise Constraint_Error;
   end Get_Position;

   function Get_Row
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Positive
   is
      Item   : Gtk.Tree_Model.Gtk_Tree_Iter := Widget.all.Get_Iter (Index);
      Column : Positive;
   begin
      Widget.all.Columns.all.To_Columned (Item, Column);
      return
        Positive
          (Gtk.Missed.Get_Row_No
             (Gtk.Tree_Model.To_Interface (Widget.all.Columns), Item) + 1);
   end Get_Row;

   function Get_Selection
     (Widget : not null access Gtk_Directory_Items_View_Record) return Selection
   is
      Result : Selection (1 .. Widget.all.Markup.all.Selected);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Result'Length > 0 then
         declare
            Root  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                      Widget.all.Columns.all.Get_Root;
            Index : Gint := 0;
            Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
         begin
            for Selected in Result'Range loop
               loop
                  Row := Widget.all.Markup.all.Nth_Child (Root, Index);
                  if Row = Gtk.Tree_Model.Null_Iter then
                     Glib.Messages.Log
                       (Gtk.Missed.GtkAda_Contributions_Domain,
                        Glib.Messages.Log_Level_Critical,
                        "Inconsitent selection in "
                        & Where ("Get_Selection"));
                     Widget.all.Markup.all.Selected := Selected - 1;
                     return Result (1 .. Widget.all.Markup.all.Selected);
                  end if;
                  Index := Index + 1;
                  exit when Get_Boolean (Widget.all.Markup, Row, 3);
               end loop;
               Result (Selected) := Positive (Index);
            end loop;
         end;
      end if;
      return Result;
   end Get_Selection;

   function Get_Selection_Mode
     (Widget : not null access Gtk_Directory_Items_View_Record)
      return Gtk.Enums.Gtk_Selection_Mode is
   begin
      return Widget.all.Markup.all.Mode;
   end Get_Selection_Mode;

   function Get_Selection_Size
     (Widget : not null access Gtk_Directory_Items_View_Record) return Natural
   is
   begin
      return Widget.all.Markup.all.Selected;
   end Get_Selection_Size;

   function Get_Tracing
     (Store : not null access Gtk_Abstract_Directory_Record'Class)
      return Traced_Actions is
   begin
      return Store.all.Tracing;
   end Get_Tracing;

   function Get_Tree_Store
     (Store : not null access Gtk_Abstract_Directory_Record)
      return Gtk.Tree_Store.Gtk_Tree_Store is
   begin
      return Store.all.Tree;
   end Get_Tree_Store;

   function Get_Type
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Item_Type is
   begin
      return Get_Type
        (Widget.all.Content.all.Cache.all.Tree,
         Widget.all.From_Marked (Widget.all.Get_Iter (Index)));
   end Get_Type;

   overriding procedure Get_Value
     (Model  : not null access Gtk_Abstract_Directory_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint;
      Value  : out Glib.Values.GValue)
   is
   begin
      Gtk.Tree_Store.Get_Value (Model.all.Tree, Iter, Column, Value);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Value (Gtk_Abstract_Directory)"));
   end Get_Value;

   overriding procedure Get_Value
     (Model  : not null access Gtk_Directory_Items_Store_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Gint;
      Value  : out Glib.Values.GValue) is
   begin
      Gtk.Tree_Store.Get_Value (Model.all.Cache.all.Tree, Iter, Column, Value);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Get_Value (Gtk_Directory_Items_Store)"));
   end Get_Value;

   function Get_Visible_Height
     (Widget : not null access Gtk_Directory_Items_View_Record) return Natural
   is
      Window : constant Gdk.Gdk_Window := Widget.all.Get_Bin_Window;
      use type Gdk.Gdk_Window;
   begin
      if Window = null then
         return 0;
      end if;
      declare
         Bottom : Natural;
         Top    : Natural;
      begin
         Top := Locate (Widget, 0.5, 0.5);
         if Top = 0 then
            return 0;
         end if;
         Bottom :=
           Locate
             (Widget,
              0.5,
              Gdouble (Gdk.Window.Get_Height (Window)) - 0.5);
         if Bottom = 0 then
            Bottom :=
              Widget.all.Columns.all.Get_Column_Height
                (Widget.all.Get_Column (Top));
         end if;
         return Bottom - Top + 1;
      end;
   end Get_Visible_Height;

   function Get_Visible_Width
     (Widget : not null access Gtk_Directory_Items_View_Record) return Natural
   is
      Window : constant Gdk.Gdk_Window := Widget.all.Get_Bin_Window;
      use type Gdk.Gdk_Window;
   begin
      if Window = null then
         return 0;
      end if;
      declare
         Left  : Natural;
         Right : Natural;
      begin
         Left := Widget.all.Locate (0.5, 0.5);
         if Left = 0 then
            return 0;
         end if;
         Right :=
           Locate (Widget, Gdouble (Gdk.Window.Get_Width (Window)) - 0.5, 0.5);
         if Right = 0 then
            Right :=
              Gtk.Tree_Model.Columned_Store.Get_Row_Width
                (Widget.all.Columns,
                 Get_Row (Widget, Left));
         end if;
         return Right - Left + 1;
      end;
   end Get_Visible_Width;

   procedure Gtk_New
     (Model : out Gtk_Directory_Items_Store;
      Cache : not null access Gtk_Abstract_Directory_Record'Class;
      View  : not null access Gtk_Directory_Items_View_Record'Class) is
   begin
      Model := new Gtk_Directory_Items_Store_Record;
      Gtk.Tree_Model.Abstract_Store.Initialize (Model, Get_Items_Model_Type);
      Model.all.View := View.all'Unchecked_Access;
      Model.all.Cache := Cache.all'Unchecked_Access;
      Ref (Model.all.Cache);
      Directory_Items_Store_Handlers.Connect
        (Cache,
         "item-deleted",
         Item_Deleted'Access,
         Model.all'Access);
      Directory_Items_Store_Handlers.Connect
        (Cache,
         "item-deleting",
         Item_Deleting'Access,
         Model.all'Access);
      Directory_Items_Store_Handlers.Connect
        (Cache,
         "item-inserted",
         Item_Inserted'Access,
         Model.all'Access);
      Directory_Items_Store_Handlers.Connect
        (Cache,
         "item-renamed",
         Item_Renamed'Access,
         Model.all'Access);
   end Gtk_New;

   procedure Gtk_New
     (Widget  : out Gtk_Directory_Items_View;
      Store   : not null access Gtk_Abstract_Directory_Record'Class;
      Columns : Positive;
      Current : Item_Path := "") is
   begin
      Widget := new Gtk_Directory_Items_View_Record;
      Initialize (Widget, Store, Columns, Current);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget  : out Gtk_Directory_Items_View;
      Tree    : not null access Gtk_Directory_Tree_View_Record'Class;
      Columns : Positive) is
   begin
      Widget := new Gtk_Directory_Items_View_Record;
      Initialize (Widget, Tree, Columns);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Widget   : out Gtk_Directory_Tree_View;
      Store    : not null access Gtk_Abstract_Directory_Record'Class;
      Selected : Item_Path := "") is
   begin
      Widget := new Gtk_Directory_Tree_View_Record;
      Initialize (Widget, Store, Selected);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   overriding function Has_Child
     (Model : not null access Gtk_Abstract_Directory_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      return Children (Model, Iter) /= Gtk.Tree_Model.Null_Iter;
   end Has_Child;

   overriding function Has_Child
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      return Children (Model, Iter) /= Gtk.Tree_Model.Null_Iter;
   end Has_Child;

   procedure Initialize
     (Store : not null access Gtk_Abstract_Directory_Record'Class) is
   begin
      Gtk.Tree_Store.Gtk_New
        (Store.all.Tree,
         (GType_String, -- Icon
          GType_String, -- Name
          GType_Int));     -- Directory caching flag
      -- Directories view
      Gtk.Tree_Model.Abstract_Store.Initialize (Store, Get_Cache_Model_Type);
      -- Filling the root directory
      declare
         Updated : Boolean;
         Root    : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
      begin
         Add_Folder (Store, Root, "", True, Updated);
      end;
   end Initialize;

   procedure Initialize
     (Widget   : not null access Gtk_Directory_Tree_View_Record'Class;
      Store    : not null access Gtk_Abstract_Directory_Record'Class;
      Selected : Item_Path) is
   begin
      Widget.all.Cache := Store.all'Access;
      Gtk.Tree_View.Initialize (Widget);
      Widget.all.Set_Enable_Search (False);
      Set_Rules_Hint (Widget, True);
      Widget.all.Set_Headers_Visible (False);
      declare
         Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
         Column_No : Gint;
      begin
         Gtk.Tree_View_Column.Gtk_New (Column);

         Gtk.Cell_Renderer_Pixbuf.Gtk_New (Widget.all.Icon_Renderer);
         Column.all.Pack_Start (Widget.all.Icon_Renderer, False);
         Gtk.Missed.Add_Stock_Attribute (Column, Widget.all.Icon_Renderer, 0);

         Gtk.Cell_Renderer_Text.Gtk_New (Widget.all.Name_Renderer);
         Column.all.Pack_Start (Widget.all.Name_Renderer, True);
         Column.all.Add_Attribute (Widget.all.Name_Renderer, "text", 1);
         Column_No := Append_Column (Widget, Column);
         Column.all.Set_Resizable (True);
         Tree_Functions.Set_Cell_Data_Func
           (Column,
            Widget.all.Name_Renderer,
            Set_Tree_Name'Access,
            Widget.all'Access);
         Tree_Functions.Set_Cell_Data_Func
           (Column,
            Widget.all.Icon_Renderer,
            Set_Tree_Icon'Access,
            Widget.all'Access);
      end;
      Widget.all.Get_Selection.all.Set_Mode (Gtk.Enums.Selection_Single);
      Directory_Tree_Handlers.Connect
        (Widget.all.Name_Renderer,
         "edited",
         Edited_Directory'Access,
         Widget.all'Access);
      Directory_Tree_Handlers.Connect
        (Widget,
         "row-activated",
         Directory_Activated'Access,
         Widget.all'Access);
      Directory_Tree_Handlers.Connect
        (Widget,
         "row-expanded",
         Directory_Expanded'Access,
         Widget.all'Access);
      Directory_Tree_Handlers.Connect
        (Widget.all.Get_Selection,
         "changed",
         Directory_Changed'Access,
         Widget.all'Access);
      Directory_Tree_Handlers.Connect
        (Widget.all.Cache,
         "refreshed",
         Directory_Refreshed'Access,
         Widget.all'Access);
      Widget.all.Set_Model (Gtk.Tree_Model.To_Interface (Store));
      if Selected'Length > 0 then
         Widget.all.Set_Current_Directory (Selected);
      end if;
   end Initialize;

   procedure Initialize
     (Widget  : not null access Gtk_Directory_Items_View_Record'Class;
      Store   : not null access Gtk_Abstract_Directory_Record'Class;
      Columns : Positive;
      Current : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Changed : Boolean := False;

      use type Gtk.Tree_Model.Columned_Store.Gtk_Columned_Store;
   begin
      G_New (Widget, Get_Directory_Items_View_Type);
      Gtk.Tree_View.Initialize (Widget);
      Widget.all.Set_Headers_Visible (False);
      Widget.all.Set_Enable_Search (False);
      Gtk_New (Widget.all.Content, Store, Widget);
      Widget.all.Markup := new Gtk_Selection_Store_Record;
      Widget.all.Markup.all.View := Widget.all'Access;
      Gtk.Tree_Model.Extension_Store.Initialize
        (Widget.all.Markup, Widget.all.Content, (1 => GType_Boolean));
      Gtk.Tree_Model.Columned_Store.Gtk_New
        (Widget.all.Columns,
         Widget.all.Markup,
         Columns,
         Gtk.Tree_Model.Null_Iter); -- Rooted in the markup's root

      -- List view
      Widget.all.Name_Renderers :=
        new Gtk_Cell_Renderer_Text_Array (1 .. Columns);
      declare
         Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
         Column_No : Gint;
         Data      : Column_Data;
      begin
         Data.Browser := Widget.all'Access;
         Gtk.Cell_Renderer_Pixbuf.Gtk_New (Data.Icon_Renderer);
         for No in 0 .. Gint (Columns) - 1 loop
            Gtk.Cell_Renderer_Text.Gtk_New (Data.Text_Renderer);
            Widget.all.Name_Renderers (Integer (No + 1)) :=
              Data.Text_Renderer;
            Gtk.Tree_View_Column.Gtk_New (Column);

            Column.all.Pack_Start (Data.Icon_Renderer, False);
            Column.all.Pack_Start (Data.Text_Renderer, True);
            Column_No := Append_Column (Widget, Column);
            --          Set_Sizing (Column, Tree_View_Column_Fixed);
            Gtk.Tree_View_Column.Set_Expand (Column, True);

            Data.Column := Positive (No + 1);
            Column_Functions.Set_Cell_Data_Func
              (Column,
               Data.Icon_Renderer,
               Set_Column_Data'Access,
               Data);
            Directory_Items_Commit_Handlers.Connect
              (Data.Text_Renderer,
               "edited",
               Edited_Item'Access,
               Data);
         end loop;
      end;
      Widget.all.Get_Selection.all.Set_Mode (Gtk.Enums.Selection_None);
      if Widget.all.Directories /= null then
         Directory_Items_Result_Handlers.Connect
           (Widget,
            "button_press_event",
            Directory_Items_Result_Handlers.
              Event_Marshaller.To_Marshaller (Key_Press'Access),
            Widget.all'Access);
         Directory_Items_Handlers.Connect
           (Widget,
            "destroy",
            Destroy'Access,
            Widget.all'Access);
         Directory_Selection_Handlers.Connect
           (Widget.all.Directories.all.Get_Selection,
            "changed",
            Selection_Changed'Access,
            Widget.all'Access);
         Directory_Items_Result_Handlers.Connect
           (Widget,
            "key_press_event",
            Directory_Items_Result_Handlers.
              To_Marshaller (Key_Press'Access),
            Widget.all'Access);
         Directory_Items_Handlers.Connect
           (Widget,
            "row-activated",
            Item_Activated'Access,
            Widget.all'Access);
         Directory_Items_Handlers.Connect
           (Widget.all.Directories,
            "row-collapsed",
            Directory_Collapsed'Access,
            Widget.all'Access);
      end if;
      Widget.all.Set_Model (Gtk.Tree_Model.To_Interface (Widget.all.Columns));
      Set_Current (Widget, Current, Changed);
      if Changed  then
         Directory_Changed (Widget);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            &  Where ("Initialize (Gtk_Directory_Items_View)"));
         if Widget.all.Columns /= null then
            Gtk.Tree_Model.Columned_Store.Unref (Widget.all.Columns);
            Widget.all.Columns := null;
         end if;
         if Widget.all.Markup /= null then
            Unref (Widget.all.Markup);
            Widget.all.Markup := null;
         end if;
         if Widget.all.Content /= null then
            Unref (Widget.all.Content);
            Widget.all.Content := null;
         end if;
         if Widget.all.Directories /= null then
            Unref (Widget.all.Directories);
         end if;
         raise;
   end Initialize;

   procedure Initialize
     (Widget  : not null access Gtk_Directory_Items_View_Record'Class;
      Store   : not null access Gtk_Abstract_Directory_Record'Class;
      Columns : Positive;
      Current : Item_Path)
   is
      Best_Match, Exact_Match : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Cache (Store, Current, Best_Match, Exact_Match);
      Initialize (Widget, Store, Columns, Best_Match);
   end Initialize;

   procedure Initialize
     (Widget  : not null access Gtk_Directory_Items_View_Record'Class;
      Tree    : not null access Gtk_Directory_Tree_View_Record'Class;
      Columns : Positive)
   is
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Model : Gtk.Tree_Model.Gtk_Tree_Model;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Tree.all.Get_Selection.all.Get_Selected (Model, Row);
      if Row /= Gtk.Tree_Model.Null_Iter then
         Row := To_Item (Tree.all.Cache, Row);
      end if;
      Widget.all.Directories := Tree.all'Access;
      Ref (Widget.all.Directories);
      Initialize (Widget, Tree.all.Cache, Columns, Row);
   end Initialize;

   function Input_Event
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive;
      Event  : Gdk.Event.Gdk_Event) return Boolean is
   begin
      return False;
   end Input_Event;

   overriding procedure Inserted
     (Model : not null access Gtk_Selection_Store_Record;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use type Gtk.Enums.Gtk_Selection_Mode;
   begin
      Gtk.Tree_Model.Extension_Store.Inserted
        (Gtk.Tree_Model.Extension_Store.Gtk_Extension_Store_Record (Model.all)'Access,
         Path,
         Iter);

      if
        Model.all.Mode = Gtk.Enums.Selection_Browse and then
        Model.all.Selected = 0
      then
         -- Select one item
         declare
            Current : constant Natural := Model.all.View.all.Get_Current;
         begin
            if Current > 0 then
               Model.all.View.all.Change_Selection (Current, True);
            else
               Model.all.View.all.Change_Selection (1, True);
            end if;
         end;
         Model.all.View.all.Selection_Changed;
      end if;
   end Inserted;

   function Is_Directory
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Boolean is
   begin
      return
        (Gtk.Tree_Store.Get_Int
           (Widget.all.Content.all.Cache.all.Tree,
            Widget.all.From_Marked (Widget.all.Get_Iter (Index)),
            2) in Cached_Directory);
   end Is_Directory;

   function Is_Editable
     (Widget : not null access Gtk_Directory_Items_View_Record) return Boolean
   is
   begin
      return
        Glib.Properties.Get_Property
          (Widget.all.Name_Renderers (1),
           Gtk.Cell_Renderer_Text.Editable_Property);
   end Is_Editable;

   function Is_Editable
     (Widget : not null access Gtk_Directory_Tree_View_Record) return Boolean is
   begin
      return
        Glib.Properties.Get_Property
          (Widget.all.Name_Renderer,
           Gtk.Cell_Renderer_Text.Editable_Property);
   end Is_Editable;

   function Is_Selected
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive) return Boolean is
   begin
      return
        Get_Boolean (Widget.all.Markup, Widget.all.Get_Iter (Index), 3);
   end Is_Selected;

   procedure Item_Activated
     (Object  : access GObject_Record'Class;
      Params  : Glib.Values.GValues;
      Browser : Gtk_Directory_Items_View)
   is
      Path   : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                 Gtk.Tree_Model.Convert
                   (Glib.Values.Get_Address (Glib.Values.Nth (Params, 1)));
      Column : constant Gtk.Tree_View_Column.Gtk_Tree_View_Column :=
                 Gtk.Missed.Get_Column (Glib.Values.Nth (Params, 2));

      use type Gtk.Tree_Model.Gtk_Tree_Path;
      use type Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   begin
      if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path or else Column = null then
         return;
      else
         Browser.all.Activated
           (Browser.all.Get_Index
              (Positive
                   (Gtk.Missed.Get_Row_No
                        (Gtk.Tree_Model.To_Interface (Browser.all.Columns),
                         Path) + 1),
               Positive (Gtk.Missed.Get_Column_No (Browser, Column) + 1)));
      end if;
   end Item_Activated;

   procedure Item_Deleted
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Store  : Gtk_Directory_Items_Store)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Store.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return;
      end if;
      if Store.all.Deleted >= 0 then
         declare
            Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                     Gtk.Tree_Model.Gtk_Tree_Path_New;
         begin
            Gtk.Tree_Model.Append_Index (Path, Store.all.Deleted);
            Store.all.Count   := Store.all.Count - 1;
            Store.all.Deleted := -1;
            Gtk.Tree_Model.Row_Deleted
              (Gtk.Tree_Model.To_Interface (Store), Path);
            Gtk.Tree_Model.Path_Free (Path);
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Item_Deleting (Gtk_Directory_Items_Store)"));
   end Item_Deleted;

   procedure Item_Deleting
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Store  : Gtk_Directory_Items_Store)
   is
      Row : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
              Gtk.Tree_Model.Get_Tree_Iter (Glib.Values.Nth (Params, 1));

      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Store.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         Store.all.Deleted := -1;
      else
         Store.all.Deleted := Store.To_Filtered (Row);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Item_Deleting (Gtk_Directory_Items_Store)"));
   end Item_Deleting;

   procedure Item_Inserted
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Store  : Gtk_Directory_Items_Store)
   is
      Row  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
               Gtk.Tree_Model.Get_Tree_Iter (Glib.Values.Nth (Params, 1));
      Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
               Gtk.Tree_Model.Get_Tree_Path (Glib.Values.Nth (Params, 2));

      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Store.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return;
      end if;
      if Store.all.Cache.all.Get_Int (Row, 2) in Cached_Node then
         declare
            Filtered : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                         Store.To_Filtered (Path);
         begin
            if Filtered /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
               Gtk.Tree_Model.Row_Inserted
                 (Gtk.Tree_Model.To_Interface (Store), Filtered, Row);
               Gtk.Tree_Model.Path_Free (Filtered);
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Item_Inserted (Gtk_Directory_Items_Store)"));
   end Item_Inserted;

   procedure Item_Renamed
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Store  : Gtk_Directory_Items_Store)
   is
      Row     : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                  Gtk.Tree_Model.Get_Tree_Iter (Glib.Values.Nth (Params, 1));
      Exists  : Boolean := False;
      Existed : Boolean := False;
      Path    : Gtk.Tree_Model.Gtk_Tree_Path;

      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Store.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return;
      end if;
      case Gtk.Tree_Store.Get_Int (Store.all.Cache.all.Tree, Row, 2) is
         when Cached_Directory => -- Directory is always visible
            Existed := True;
            Exists  := True;
         when Cached_Item =>      -- Items are to be filtered
            Exists :=
              Store.all.View.all.Filter
                (False,
                 Get_Name (Store.all.Cache.all.Tree, Row),
                 Get_Type (Store.all.Cache.all.Tree, Row));
            Existed :=
              Store.all.View.all.Filter
                (False,
                 Item_Name
                   (Glib.Values.Get_String (Glib.Values.Nth (Params, 2))),
                 Get_Type (Store.all.Cache.all.Tree, Row));
         when others =>
            return;
      end case;
      if Existed then
         if Exists then
            Path := Store.all.View.all.Content.all.Get_Path (Row);
            if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
               Gtk.Tree_Model.Row_Changed
                 (Gtk.Tree_Model.To_Interface (Store.all.View.all.Content),
                  Path,
                  Row);
               Gtk.Tree_Model.Path_Free (Path);
            end if;
         else
            Path := Store.all.View.all.Content.all.Get_Path_Before (Row);
            if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
               Gtk.Tree_Model.Row_Deleted
                 (Gtk.Tree_Model.To_Interface (Store.all.View.all.Content),
                  Path);
               Gtk.Tree_Model.Path_Free (Path);
            end if;
         end if;
      else
         if Exists then
            Path := Store.all.View.all.Content.all.Get_Path (Row);
            if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
               Gtk.Tree_Model.Row_Inserted
                 (Gtk.Tree_Model.To_Interface (Store.all.View.all.Content),
                  Path,
                  Row);
               Gtk.Tree_Model.Path_Free (Path);
            end if;
         else
            null;
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Item_Renamed (Gtk_Directory_Items_Store)"));
   end Item_Renamed;

   function Key_Press
     (Object  : access GObject_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Browser : Gtk_Directory_Items_View) return Boolean
   is
      Changed : Boolean;
   begin
      case Gdk.Event.Get_Event_Type (Event) is
         when Gdk.Event.Button_Press =>
            -- Button click
            if
              0 /= (Trace_Key_Presses and Browser.all.Content.all.Cache.all.Tracing)
            then
               Browser.all.Content.all.Cache.all.Trace
                 (Browser.all.Content.all.Cache.all.Depth,
                  "button click");
            end if;
            declare
               Index : constant Natural :=
                         Browser.all.Locate (Event.all.Button.X, Event.all.Button.Y);
            begin
               if Gdk.Event.Get_Button (Event) = 1 then
                  Browser.all.Move (Changed, Gdk.Event.Get_State (Event), Index);
                  return not Changed;
               else
                  if Index > 0 then
                     return Input_Event (Browser, Index, Event);
                  else
                     return False;
                  end if;
               end if;
            end;
         when Gdk.Event.Gdk_2button_Press | Gdk.Event.Gdk_3button_Press =>
            -- GDouble click
            if
              0 /= (Trace_Key_Presses and Browser.all.Content.all.Cache.all.Tracing)
            then
               Trace
                 (Browser.all.Content.all.Cache,
                  Browser.all.Content.all.Cache.all.Depth,
                  "GDouble click");
            end if;
            declare
               Index : constant Natural :=
                         Browser.all.Locate (Event.all.Button.X, Event.all.Button.Y);
            begin
               if Index > 0 then
                  Browser.all.Activated (Index);
               end if;
               return True;
            end;
         when Gdk.Event.Key_Press =>
            -- Key press
            case Gdk.Event.Get_Key_Val (Event) is
               when Gdk.Types.Keysyms.GDK_Up | Gdk.Types.Keysyms.GDK_KP_Up =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key up");
                  end if;
                  Browser.all.Move
                    (Changed,
                     Gdk.Event.Get_State (Event),
                     Browser.all.Get_Current,
                     -1);
                  return True;
               when Gdk.Types.Keysyms.GDK_Down | Gdk.Types.Keysyms.GDK_KP_Down =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key down");
                  end if;
                  Browser.all.Move
                    (Changed,
                     Gdk.Event.Get_State (Event),
                     Browser.all.Get_Current,
                     1);
                  return True;
               when Gdk.Types.Keysyms.GDK_Left |
                    Gdk.Types.Keysyms.GDK_KP_Left =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key left");
                  end if;
                  Browser.all.Move
                    (Changed,
                     Gdk.Event.Get_State (Event),
                     Browser.all.Get_Current,
                     -Browser.all.Columns.all.Get_Rows (False),
                     True);
                  return True;
               when Gdk.Types.Keysyms.GDK_Page_Down |
                    Gdk.Types.Keysyms.GDK_KP_Page_Down =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key down");
                  end if;
                  Browser.all.Move
                    (Changed,
                     Gdk.Event.Get_State (Event),
                     Browser.all.Get_Current,
                     Get_Visible_Height (Browser));
                  return True;
               when Gdk.Types.Keysyms.GDK_Page_Up |
                    Gdk.Types.Keysyms.GDK_KP_Page_Up =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key page up");
                  end if;
                  Browser.all.Move
                    (Changed,
                     Gdk.Event.Get_State (Event),
                     Browser.all.Get_Current,
                     -Get_Visible_Height (Browser));
                  return True;
               when Gdk.Types.Keysyms.GDK_Right |
                    Gdk.Types.Keysyms.GDK_KP_Right =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key right");
                  end if;
                  Browser.all.Move
                    (Changed,
                     Gdk.Event.Get_State (Event),
                     Browser.all.Get_Current,
                     Browser.all.Columns.all.Get_Rows (False),
                     True);
                  return True;
               when Gdk.Types.Keysyms.GDK_Home |
                    Gdk.Types.Keysyms.GDK_KP_Home =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key home");
                  end if;
                  Browser.all.Move (Changed, Gdk.Event.Get_State (Event), 1);
                  return True;
               when Gdk.Types.Keysyms.GDK_End | Gdk.Types.Keysyms.GDK_KP_End =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Trace
                       (Browser.all.Content.all.Cache,
                        Browser.all.Content.all.Cache.all.Depth,
                        "key end");
                  end if;
                  Browser.all.Move
                    (Changed,
                     Gdk.Event.Get_State (Event),
                     Browser.all.Get_Directory_Size);
                  return True;
               when Gdk.Types.Keysyms.GDK_F5 =>
                  if
                    0 /= (Trace_Key_Presses and
                            Browser.all.Content.all.Cache.all.Tracing)
                  then
                     Browser.all.Content.all.Cache.all.Trace
                       (Browser.all.Content.all.Cache.all.Depth,
                        "key F5");
                  end if;
                  declare
                     Wait : Gtk.Missed.Wait_Cursor (+Browser);
                  begin
                     Gtk.Abstract_Browser.Changed
                       (Browser.all.Content.all.Cache,
                        Browser.all.Directories.all.Get_Current_Directory);
                  end;
                  return True;
               when others =>
                  -- Not a special key
                  declare
                     Key : constant Gunichar :=
                             Gtk.Missed.Keyval_To_Unicode
                               (Gdk.Event.Get_Key_Val (Event));
                  begin
                     if Key = 0 then
                        return False;
                     else
                        if
                          0 /= (Trace_Key_Presses and
                                  Browser.all.Content.all.Cache.all.Tracing)
                        then
                           Browser.all.Content.all.Cache.all.Trace
                             (Browser.all.Content.all.Cache.all.Depth,
                              "key" & Gunichar'Image (Key));
                        end if;
                        if Browser.all.Last_Key /= Key then
                           Browser.all.Last_Key       := Key;
                           Browser.all.Last_Position  := 0;
                        end if;
                        declare
                           Prefix : UTF8_String (1 .. 8);
                           Last   : Natural;
                        begin
                           Glib.Unicode.Unichar_To_UTF8 (Key, Prefix, Last);
                           Browser.all.Last_Position :=
                             Scan
                               (Browser,
                                Item_Name (Prefix (1 .. Last)),
                                Browser.all.Last_Position);
                        end;
                        if Browser.all.Last_Position /= 0 then
                           Browser.all.Move
                             (Changed,
                              Gdk.Event.Get_State (Event),
                              Browser.all.Last_Position);
                           return True;
                        end if;
                     end if;
                  end;
            end case;
            return False;
         when others =>
            return False;
      end case;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Key_Press"));
         return False;
   end Key_Press;

   function Locate
     (Widget : not null access Gtk_Directory_Items_View_Record;
      X, Y   : Gdouble) return Natural
   is
      Cell_X        : Gint;
      Cell_Y        : Gint;
      Column        : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Columned_Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Column_No     : Gint;
      Markup_Path   : Gtk.Tree_Model.Gtk_Tree_Path;
      Found         : Boolean;
      Result        : Natural := 0;

      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      begin
         Widget.all.Get_Path_At_Pos
           (Gint (X),
            Gint (Y),
            Columned_Path,
            Column,
            Cell_X,
            Cell_Y,
            Found);
      exception
         when Constraint_Error =>
            return 0;
      end;
      if Found then
         Column_No := Gtk.Missed.Get_Column_No (Widget, Column);
         if Column_No >= 0 then
            Markup_Path :=
              Widget.all.Columns.all.From_Columned
                (Columned_Path,
                 Positive (Column_No + 1));
            if Markup_Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
               declare
                  Indices : Gint_Array renames
                              Gtk.Tree_Model.Get_Indices (Markup_Path);
               begin
                  if Indices'Length > 0 then
                     Result := Natural (Indices (Indices'Last) + 1);
                  end if;
               end;
               Gtk.Tree_Model.Path_Free (Markup_Path);
            end if;
         end if;
         Gtk.Tree_Model.Path_Free (Columned_Path);
      end if;
      return Result;
   end Locate;

   procedure Move
     (Widget       : not null access Gtk_Directory_Items_View_Record;
      Changed      : out Boolean;
      Modifier     : Gdk.Types.Gdk_Modifier_Type;
      To           : Natural;
      By           : Integer := 0;
      Fixed_Row    : Boolean := False;
      Fixed_Column : Boolean := False)
   is
      Size     : constant Natural := Widget.all.Get_Directory_Size;
      Position : Integer;

      use type Gdk.Types.Gdk_Modifier_Type;
   begin
      Changed := False;
      -- Determining the target position
      if To not in 1 .. Size then
         return;
      end if;
      if Fixed_Row or else Fixed_Column then
         declare
            New_Row    : Positive;
            New_Column : Positive;
            Old_Row    : Positive;
            Old_Column : Positive;
         begin
            Widget.all.Get_Position (To, Old_Row, Old_Column);
            Position := To + By;
            if Position > Widget.all.Get_Directory_Size then
               Position := Widget.all.Get_Directory_Size;
            elsif Position < 1 then
               Position := 1;
            end if;
            Widget.all.Get_Position (Position, New_Row, New_Column);
            if Fixed_Row and then New_Row /= Old_Row then
               -- The row differs
               if By > 0 then
                  -- Moving  forwards using the last available column to
                  -- in the row.
                  New_Column := Widget.all.Columns.all.Get_Row_Width (New_Row);
               else
                  -- Moving backwards using the first column
                  New_Column := 1;
               end if;
               if Fixed_Column and then New_Column /= Old_Column then
                  return;
               end if;
               Position := Widget.all.Get_Index (Old_Row, New_Column);
            elsif Fixed_Column and then New_Column /= Old_Column then
               if By > 0 then
                  -- Moving forwards using the last available row
                  New_Row :=
                    Widget.all.Columns.all.Get_Column_Height (New_Column);
               else
                  -- Moving backwards using the first row
                  New_Row := 1;
               end if;
               if Fixed_Row and then New_Row /= Old_Row then
                  return;
               end if;
               Position := Widget.all.Get_Index (New_Row, Old_Column);
            end if;
         end;
      else
         Position := To + By;
         if Position > Widget.all.Get_Directory_Size then
            Position := Widget.all.Get_Directory_Size;
         elsif Position < 1 then
            Position := 1;
         end if;
      end if;
      -- Dealing with selection
      case Widget.all.Markup.all.Mode is
         when Gtk.Enums.Selection_None =>
            null;
         when Gtk.Enums.Selection_Single =>
            if 0 = (Modifier and Gdk.Types.Control_Mask) then
               -- Simple selection
               if not Widget.all.Is_Selected (Position) then
                  if Widget.all.Get_Selection_Size > 0 then
                     -- Deselect any selected
                     for Index in 1 .. Size loop
                        if Widget.all.Is_Selected (Index) then
                           Widget.all.Change_Selection (Index, False);
                           exit;
                        end if;
                     end loop;
                  end if;
                  Widget.all.Change_Selection (Position, True);
                  Changed := True;
               end if;
            else
               -- Selection toggling
               if Widget.all.Is_Selected (Position) then
                  Widget.all.Change_Selection (Position, False);
               else
                  if Widget.all.Get_Selection_Size > 0 then
                     -- Deselect any selected
                     for Index in 1 .. Size loop
                        if Widget.all.Is_Selected (Index) then
                           Widget.all.Change_Selection (Index, False);
                           exit;
                        end if;
                     end loop;
                  end if;
                  Widget.all.Change_Selection (Position, True);
               end if;
               Changed := True;
            end if;
         when Gtk.Enums.Selection_Browse =>
            if not Widget.all.Is_Selected (Position) then
               -- Deselect any selected
               for Index in 1 .. Size loop
                  if Widget.all.Is_Selected (Index) then
                     Widget.all.Change_Selection (Index, False);
                     exit;
                  end if;
               end loop;
               Widget.all.Change_Selection (Position, True);
               Changed := True;
            end if;
         when Gtk.Enums.Selection_Multiple =>
            if 0 = (Modifier and Gdk.Types.Shift_Mask) then
               if 0 = (Modifier and Gdk.Types.Control_Mask) then
                  -- Single selection
                  for Index in 1 .. Size loop
                     if Widget.all.Is_Selected (Index) then
                        if Index /= Position then
                           Widget.all.Change_Selection (Index, False);
                           Changed := True;
                        end if;
                     else
                        if Index = Position then
                           Widget.all.Change_Selection (Index, True);
                           Changed := True;
                        end if;
                     end if;
                  end loop;
               else
                  -- Toggling selection
                  if Widget.all.Is_Selected (Position) then
                     Widget.all.Change_Selection (Position, False);
                  else
                     Widget.all.Change_Selection (Position, True);
                  end if;
                  Changed := True;
               end if;
            else
               -- Range selection in nearest to Position
               declare
                  Above : Integer := 0;
                  Below : Integer := 0;
                  From  : Integer := Position;
                  To    : Integer := Position;
               begin
                  if not Widget.all.Is_Selected (Position) then
                     Above := Integer'Last;
                     while From > 1 loop
                        From := From - 1;
                        if Widget.all.Is_Selected (From) then
                           Above := Position - From;
                           exit;
                        end if;
                     end loop;
                     Below := Integer'Last;
                     while To < Size loop
                        To := To + 1;
                        if Widget.all.Is_Selected (To) then
                           Below := Below - Position;
                           exit;
                        end if;
                     end loop;
                  end if;
                  while From > 1 loop
                     exit when not Widget.all.Is_Selected (From - 1);
                     From := From - 1;
                  end loop;
                  while To < Size loop
                     exit when not Widget.all.Is_Selected (To + 1);
                     To := To + 1;
                  end loop;
                  if Above < Below then
                     To := Position;
                  elsif Above > Below then
                     From := Position;
                  elsif Above = 0 and then By /= 0 then
                     if By > 0 then
                        From := Position;
                     else
                        To := Position;
                     end if;
                  else
                     if Position - From > To - Position then
                        To := Position;
                     else
                        From := Position;
                     end if;
                  end if;
                  if 0 = (Modifier and Gdk.Types.Control_Mask) then
                     -- Deselect anything before From
                     for Index in 1 .. From - 1 loop
                        if Widget.all.Is_Selected (Index) then
                           Widget.all.Change_Selection (Index, False);
                           Changed := True;
                        end if;
                     end loop;
                     -- Deselect anything after To
                     for Index in To + 1 .. Size loop
                        if Widget.all.Is_Selected (Index) then
                           Widget.all.Change_Selection (Index, False);
                           Changed := True;
                        end if;
                     end loop;
                  end if;
                  -- Select everything in From..To
                  for Index in From .. To loop
                     if not Widget.all.Is_Selected (Index) then
                        Widget.all.Change_Selection (Index, True);
                        Changed := True;
                     end if;
                  end loop;
               end;
            end if;
      end case;
      -- Moving to the target position
      if Widget.all.Get_Current /= Position then
         -- Set position to the target
         declare
            Column_No : Positive;
            Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
            Iter      : Gtk.Tree_Model.Gtk_Tree_Iter :=
                          Widget.all.Get_Iter (Position);
            Path      : Gtk.Tree_Model.Gtk_Tree_Path;
         begin
            Widget.all.Columns.all.To_Columned (Iter, Column_No);
            Path :=
              Gtk.Tree_Model.Columned_Store.Get_Path (Widget.all.Columns, Iter);
            Column := Widget.all.Get_Column (Gint (Column_No) - 1);
            Widget.all.Set_Cursor_On_Cell
              (Path          => Path,
               Focus_Column  => Column,
               Start_Editing => False,
               Focus_Cell    =>
                 Widget.all.Name_Renderers (Column_No).all'Access);
            Widget.all.Scroll_To_Cell (Path, Column, False, 0.5, 0.0);
            Widget.all.Grab_Focus;
            Gtk.Tree_Model.Path_Free (Path);
         end;
      end if;
      if Changed then
         Widget.all.Selection_Changed;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Move"));
   end Move;

   overriding procedure Next
     (Model : not null access Gtk_Abstract_Directory_Record;
      Iter  : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      loop
         Model.all.Tree.all.Next (Iter);
         exit when Iter = Gtk.Tree_Model.Null_Iter
           or else Gtk.Tree_Store.Get_Int (Model.all.Tree, Iter, 2) in Cached_Directory;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Next (Gtk_Abstract_Directory)"));
   end Next;

   overriding procedure Next
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Iter  : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if not Model.all.Check_Iter (Iter) then
         Iter := Gtk.Tree_Model.Null_Iter;
         return;
      end if;
      while Iter /= Gtk.Tree_Model.Null_Iter loop
         Model.all.Cache.all.Tree.all.Next (Iter);
         exit when Iter = Gtk.Tree_Model.Null_Iter;
         case Gtk.Tree_Store.Get_Int (Model.all.Cache.all.Tree, Iter, 2) is
            when Cached_Directory => -- Directory is always visible
               return;
            when Cached_Item =>      -- Items are to be filtered
               if
                 Model.all.View.all.Filter
                   (False,
                    Get_Name (Model.all.Cache.all.Tree, Iter),
                    Get_Type (Model.all.Cache.all.Tree, Iter))
               then
                  return;
               end if;
            when others =>
               null;
         end case;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Next (Gtk_Directory_Items_Store)"));
   end Next;

   overriding function Nth_Child
     (Model  : not null access Gtk_Abstract_Directory_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Count : Gint := N;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Parent = Gtk.Tree_Model.Null_Iter then
         Row := Model.all.Tree.all.Get_Iter_First;
      else
         Row := Model.all.Tree.all.Children (Parent);
      end if;
      while Row /= Gtk.Tree_Model.Null_Iter loop
         if Gtk.Tree_Store.Get_Int (Model.all.Tree, Row, 2) in Cached_Directory then
            exit when Count = 0;
            Count := Count - 1;
         end if;
         Model.all.Tree.all.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Nth_Child (Gtk_Directory_Items_Store)"));
         return Gtk.Tree_Model.Null_Iter;
   end Nth_Child;

   overriding function Nth_Child
     (Model  : not null access Gtk_Directory_Items_Store_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter :=
                Gtk.Tree_Store.Children (Model.all.Cache.all.Tree, Parent);
      Count : Gint := 0;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Parent /= Gtk.Tree_Model.Null_Iter then -- Only one level of depth
         return Gtk.Tree_Model.Null_Iter;
      elsif Model.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         return Gtk.Tree_Model.Null_Iter;
      end if;
      Row := Gtk.Tree_Store.Children
        (Model.all.Cache.all.Tree,
         Model.all.Cache.all.Tree.all.Get_Iter (Model.all.Root));
      while Row /= Gtk.Tree_Model.Null_Iter loop
         case Gtk.Tree_Store.Get_Int (Model.all.Cache.all.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               Count := Count + 1;
               if Count > Model.all.Count then
                  Model.all.Count := Count;
               end if;
               exit when Count > N;
            when Cached_Item =>      -- Items are to be filtered
               if Model.all.View.all.Filter
                 (False,
                  Get_Name (Model.all.Cache.all.Tree, Row),
                  Get_Type (Model.all.Cache.all.Tree, Row))
               then
                  Count := Count + 1;
                  if Count > Model.all.Count then
                     Model.all.Count := Count;
                  end if;
                  exit when Count > N;
               end if;
            when others =>
               null;
         end case;
         Model.all.Cache.all.Tree.all.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Nth_Child (Gtk_Directory_Items_Store)"));
         return Gtk.Tree_Model.Null_Iter;
   end Nth_Child;

   overriding function N_Children
     (Model : not null access Gtk_Abstract_Directory_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Gint
   is
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Count : Gint := 0;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         Row := Model.all.Tree.all.Get_Iter_First;
      else
         Row := Model.all.Tree.all.Children (Iter);
      end if;
      while Row /= Gtk.Tree_Model.Null_Iter loop
         if Gtk.Tree_Store.Get_Int (Model.all.Tree, Row, 2) in Cached_Directory then
            Count := Count + 1;
         end if;
         Model.all.Tree.all.Next (Row);
      end loop;
      return Count;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("N_Children (Gtk_Abstract_Directory)"));
         return 0;
   end N_Children;

   overriding function N_Children
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Gint
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if
        Iter /= Gtk.Tree_Model.Null_Iter or else
        Model.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path
      then
         return 0;
      end if;
      Row := Model.all.Cache.all.Tree.all.Get_Iter (Model.all.Root);
      Model.all.Count := 0;
      while Row /= Gtk.Tree_Model.Null_Iter loop
         case Gtk.Tree_Store.Get_Int (Model.all.Cache.all.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               Model.all.Count := Model.all.Count + 1;
            when Cached_Item =>      -- Items are to be filtered
               if
                 Model.all.View.all.Filter
                   (False,
                    Get_Name (Model.all.Cache.all.Tree, Row),
                    Get_Type (Model.all.Cache.all.Tree, Row))
               then
                  Model.all.Count := Model.all.Count + 1;
               end if;
            when others =>
               null;
         end case;
         Model.all.Cache.all.Tree.all.Next (Row);
      end loop;
      return Model.all.Count;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("N_Children (Gtk_Directory_Items_Store)"));
         return Model.all.Count;
   end N_Children;

   overriding function Parent
     (Model : not null access Gtk_Abstract_Directory_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      return Gtk.Tree_Store.Parent (Model.all.Tree, Child);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Parent"));
         return Gtk.Tree_Model.Null_Iter;
   end Parent;

   overriding function Parent
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
   begin
      return Gtk.Tree_Model.Null_Iter;
   end Parent;

   overriding procedure Previous
     (Model : not null access Gtk_Abstract_Directory_Record;
      Iter  : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      loop
         Model.all.Tree.all.Previous (Iter);
         exit when Iter = Gtk.Tree_Model.Null_Iter
           or else Gtk.Tree_Store.Get_Int (Model.all.Tree, Iter, 2) in Cached_Directory;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Previous (Gtk_Abstract_Directory)"));
   end Previous;

   overriding procedure Previous
     (Model : not null access Gtk_Directory_Items_Store_Record;
      Iter  : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if not Model.all.Check_Iter (Iter) then
         Iter := Gtk.Tree_Model.Null_Iter;
         return;
      end if;
      while Iter /= Gtk.Tree_Model.Null_Iter loop
         Model.all.Cache.all.Tree.all.Previous (Iter);
         exit when Iter = Gtk.Tree_Model.Null_Iter;
         case Gtk.Tree_Store.Get_Int (Model.all.Cache.all.Tree, Iter, 2) is
            when Cached_Directory => -- Directory is always visible
               exit;
            when Cached_Item =>      -- Items are to be filtered
               exit when
                 Model.all.View.all.Filter
                   (False,
                    Get_Name (Model.all.Cache.all.Tree, Iter),
                    Get_Type (Model.all.Cache.all.Tree, Iter));
            when others =>
               null;
         end case;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Previous (Gtk_Directory_Items_Store)"));
   end Previous;

   procedure Progress
     (Store     : not null access Gtk_Abstract_Directory_Record;
      Directory : Item_Path;
      State     : Gdouble) is
   begin
      Emit (Store, Progress_ID, Directory, State);
   end Progress;

   procedure Read_Error
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Text  : UTF8_String;
      Path  : Item_Path) is
   begin
      Emit (Store, Read_Error_ID, Text, Path);
   end Read_Error;

   procedure Refilter
     (Widget : not null access Gtk_Directory_Items_View_Record)
   is
      Row     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path    : Gtk.Tree_Model.Gtk_Tree_Path;
      Content : constant Gtk.Tree_Model.Gtk_Tree_Model :=
                  Gtk.Tree_Model.To_Interface (Widget.all.Content);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Gtk.Tree_Model.Gtk_New (Path);
      for Item in reverse 0 .. Widget.all.Content.all.Count - 1 loop
         Gtk.Tree_Model.Append_Index (Path, Item);
         Gtk.Tree_Model.Row_Deleted (Content, Path);
         if Gtk.Tree_Model.Up (Path) then
            null;
         end if;
      end loop;
      Row := Widget.all.Content.all.Children (Gtk.Tree_Model.Null_Iter);
      Widget.all.Content.all.Count := 0;
      while Row /= Gtk.Tree_Model.Null_Iter loop
         Gtk.Tree_Model.Append_Index (Path, Widget.all.Content.all.Count);
         Widget.all.Content.all.Count := Widget.all.Content.all.Count + 1;
         Gtk.Tree_Model.Row_Inserted (Content, Path, Row);
         if Gtk.Tree_Model.Up (Path) then
            null;
         end if;
         Widget.all.Content.all.Next (Row);
      end loop;
      Gtk.Tree_Model.Path_Free (Path);
   end Refilter;

   procedure Refresh
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Item  : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Directory : constant Item_Path := Store.Get_Path (Item);
      Updated   : Boolean;
      Child     : Gtk.Tree_Model.Gtk_Tree_Iter;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      loop
         if Item = Gtk.Tree_Model.Null_Iter then
            Child := Gtk.Tree_Store.Get_Iter_First (Store.all.Tree);
         else
            Child := Store.all.Tree.all.Children (Item);
         end if;
         exit when Child = Gtk.Tree_Model.Null_Iter;
         declare
            Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                     Store.Get_Path (Child);
            Name : constant Item_Name := Get_Name (Store.all.Tree, Child);
         begin
            --           if Get_Int (Store.Tree, Child, 2) in Cached_Directory then
            --              Path := Store.Get_Path (Child);
            --           end if;
            Emit (Store, Deleting_ID, Child, Path);
            Store.all.Tree.all.Remove (Child);
            --              if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
            --                 Row_Deleted (Gtk.Tree_Model.To_Interface (Store), Path);
            --                 Gtk.Tree_Model.Path_Free (Path);
            --                 Path := Gtk.Tree_Model.Null_Gtk_Tree_Path;
            --              end if;
            Gtk.Tree_Model.Row_Deleted
              (Gtk.Tree_Model.To_Interface (Store), Path);
            Gtk.Tree_Model.Path_Free (Path);
            Emit
              (Store,
               Deleted_ID,
               String (Store.all.Get_Path (Directory, Name)));
         end;
      end loop;
      if Item = Gtk.Tree_Model.Null_Iter then
         Add_Folder (Store, Item, "", True, Updated);
         Store.all.Refreshing := Store.all.Refreshing + 1;
         Emit (Store, Refreshed_ID, UTF8_String'(""));
         Store.all.Refreshing := Store.all.Refreshing - 1;
      else
         declare
            Path : constant Item_Path := Store.Get_Path (Item);
         begin
            Add_Folder (Store, Item, Path, True, Updated);
            Store.all.Refreshing := Store.all.Refreshing + 1;
            Emit (Store, Refreshed_ID, String (Path));
            Store.all.Refreshing := Store.all.Refreshing - 1;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Refresh"));
   end Refresh;

   procedure Refresh
     (Widget : not null access Gtk_Directory_Items_View_Record)
   is
      Changed : Boolean := True;
   begin
      Set_Current (Widget, Widget.all.Get_Directory, Changed);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Refresh"));
   end Refresh;

   procedure Release (Ptr : in out Path_Node_Ptr) is
   begin
      if Ptr.all.Next /= null then
         Release (Ptr.all.Next);
      end if;
      Free (Ptr);
   end Release;

   procedure Renamed
     (Store    : not null access Gtk_Abstract_Directory_Record'Class;
      Old_Path : Item_Path;
      New_Name : Item_Name)
   is
      Row : constant Gtk.Tree_Model.Gtk_Tree_Iter := Find (Store, Old_Path);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Row = Gtk.Tree_Model.Null_Iter then
         raise Constraint_Error;
      end if;
      declare
         Old_Name : constant Item_Name := Get_Name (Store.all.Tree, Row);
      begin
         Store.all.Tree.all.Set (Row, 1, String (New_Name));
         if
           Gtk.Tree_Store.Get_Int (Store.all.Tree, Row, 2) in Cached_Directory
         then
            declare
               Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                        Store.Get_Path (Row);
            begin
               if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
                  Gtk.Tree_Model.Row_Changed
                    (Gtk.Tree_Model.To_Interface (Store), Path, Row);
                  Gtk.Tree_Model.Path_Free (Path);
               end if;
            end;
         end if;
         Emit (Store, Renamed_ID, Row, String (Old_Name));
      end;
   end Renamed;

   procedure Renamed
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : in out Natural;
      Name   : Item_Name)
   is
      Row : Gtk.Tree_Model.Gtk_Tree_Iter := Widget.all.Get_Iter (Index);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      Row := Widget.all.From_Marked (Row);
      declare
         Store    : constant Gtk_Abstract_Directory :=
                      Widget.all.Content.all.Cache;
         Old_Name : constant Item_Name := Get_Name (Store.all.Tree, Row);
      begin
         Store.all.Tree.all.Set (Row, 1, String (Name));
         declare
            Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                     Widget.all.To_Marked (Row);
         begin
            if Iter = Gtk.Tree_Model.Null_Iter then
               Index := 0;
            else
               Index :=
                 Positive
                   (Gtk.Missed.Get_Row_No
                      (Gtk.Tree_Model.To_Interface
                         (Widget.all.Markup), Iter) + 1);
            end if;
         end;
         if
           Gtk.Tree_Store.Get_Int (Store.all.Tree, Row, 2) in Cached_Directory
         then
            declare
               Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                        Store.Get_Path (Row);
            begin
               if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
                  Gtk.Tree_Model.Row_Changed
                    (Gtk.Tree_Model.To_Interface (Store), Path, Row);
                  Gtk.Tree_Model.Path_Free (Path);
               end if;
            end;
         end if;
         Emit (Store, Renamed_ID, Row, String (Old_Name));
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Renamed (item)"));
   end Renamed;

   procedure Reset (Object : in out Item_Path_Reference) is
   begin
      Finalize (Object);
   end Reset;

   procedure Reset_Selection
     (Widget : not null access Gtk_Directory_Items_View_Record)
   is
      Changed : Boolean := False;
   begin
      case Widget.all.Markup.all.Mode is
         when Gtk.Enums.Selection_None | Gtk.Enums.Selection_Browse =>
            null;
         when Gtk.Enums.Selection_Single =>
            for Index in 1 .. Widget.all.Get_Directory_Size loop
               if Widget.all.Is_Selected (Index) then
                  Widget.all.Change_Selection (Index, False);
                  Changed := True;
                  exit;
               end if;
            end loop;
         when Gtk.Enums.Selection_Multiple =>
            for Index in 1 .. Widget.all.Get_Directory_Size loop
               if Widget.all.Is_Selected (Index) then
                  Widget.all.Change_Selection (Index, False);
                  Changed := True;
               end if;
            end loop;
      end case;
      if Changed then
         Widget.all.Selection_Changed;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Reset_Selection"));
   end Reset_Selection;

   procedure Rewind_Error
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Text  : UTF8_String;
      Path  : Item_Path) is
   begin
      Emit (Store, Rewind_Error_ID, Text, Path);
   end Rewind_Error;

   function Scan
     (Widget : not null access Gtk_Directory_Items_View_Record'Class;
      Prefix : Item_Name;
      Index  : Natural) return Natural
   is
      Row       : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                    To_Marked  (Widget, Widget.all.Get_Directory);
      Directory : Item_Path renames Widget.all.Get_Directory;
      Aim       : constant Directory_Item :=
                    (Directory   => False,
                     Policy      => Cache_Ahead,
                     Kind_Length => 0,
                     Kind        => "",
                     Name_Length => Prefix'Length,
                     Name        => Prefix);
      function Is_Prefix (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
      is
         Name : constant Item_Name :=
                  Get_Name
                    (Gtk.Tree_Model.To_Interface (Widget.all.Markup), Iter);

         use type Gtk.Missed.Row_Order;
      begin
         if Name'Length < Aim.Name_Length then
            return False;
         else
            return
              Gtk.Missed.Equal =
                Compare
                  (Widget.all.Content.all.Cache,
                   Directory,
                   Aim,
                   (Directory   => False,
                    Policy      => Cache_Ahead,
                    Kind_Length => 0,
                    Kind        => "",
                    Name_Length => Aim.Name_Length,
                    Name        => Name (1 .. Aim.Name_Length)),
                   True);
         end if;
      end Is_Prefix;

      Iter     : Gtk.Tree_Model.Gtk_Tree_Iter :=
                   Widget.all.Markup.all.Nth_Child (Row, Gint (Index));
      Position : Natural := Index;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      while Iter /= Gtk.Tree_Model.Null_Iter loop
         Position := Position + 1;
         if Is_Prefix (Iter) then
            return Position;
         end if;
         Next (Widget.all.Markup, Iter);
      end loop;
      return 0;
   end Scan;

   procedure Selection_Changed
     (Selection : access Gtk.Tree_Selection.Gtk_Tree_Selection_Record'Class;
      Browser   : Gtk_Directory_Items_View)
   is
      Wait : Gtk.Missed.Wait_Cursor (+Browser);
   begin
      declare
         Model     : Gtk.Tree_Model.Gtk_Tree_Model;
         Directory : Gtk.Tree_Model.Gtk_Tree_Iter;
         Changed   : Boolean := False;

         use type Gtk.Tree_Model.Gtk_Tree_Iter;
      begin
         Selection.all.Get_Selected (Model, Directory);
         if Directory = Gtk.Tree_Model.Null_Iter then
            -- Try to cache the root directory we are switching to
            Add_Folder
              (Browser.all.Content.all.Cache,
               Directory,
               "",
               False,
               Changed);
            if Directory = Gtk.Tree_Model.Null_Iter then
               Set_Current (Browser, Gtk.Tree_Model.Null_Iter, Changed);
            else
               Set_Current (Browser, Directory, Changed);
            end if;
         else
            Directory := To_Item (Browser.all.Content.all.Cache, Directory);
            case Gtk.Tree_Store.Get_Int (Browser.all.Content.all.Cache.all.Tree, Directory, 2) is
               when Cached_Children =>
                  null;
               when Cached_Never_Directory | Cached_Item =>
                  -- Try to cache the directory we are switching to
                  Add_Folder
                    (Browser.all.Content.all.Cache,
                     Directory,
                     Get_Path (Browser.all.Content.all.Cache, Directory),
                     False,
                     Changed);
               when others =>
                  return;
            end case;
            Set_Current (Browser, Directory, Changed);
            Gtk.Tree_Selection.Select_Iter
              (Selection,
               From_Item
                 (Browser.all.Content.all.Cache,
                  Browser.all.Get_Directory));
         end if;
         if Changed then
            Browser.all.Directory_Changed;
         end if;
      exception
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               "Fault: "
               & Ada.Exceptions.Exception_Information (Error)
               & Where ("Selection_Changed"));
      end;
   end Selection_Changed;

   procedure Selection_Changed
     (Widget : not null access Gtk_Directory_Items_View_Record) is
   begin
      Directory_Tree_Handlers.Emit_By_Name
        (Widget,
         "selection-changed");
   end Selection_Changed;

   procedure Set
     (Object : in out Item_Path_Reference;
      Path   : Item_Path) is
   begin
      if Object.Ptr = null then
         if Path'Length = 0 then
            return;
         end if;
      else
         if Object.Ptr.all.Path = Path then
            return;
         end if;
         Finalize (Object);
      end if;
      Object.Ptr := new Item_Path_Object (Path'Length);
      Object.Ptr.all.Path := Path;
   end Set;

   procedure Set
     (Object : in out Item_Path_Reference;
      Widget : not null access Gtk_Directory_Tree_View_Record'Class)
   is
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Model : Gtk.Tree_Model.Gtk_Tree_Model;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Widget.all.Get_Selection.all.Get_Selected (Model, Row);
      if Row = Gtk.Tree_Model.Null_Iter then
         Finalize (Object);
      else
         Set
           (Object,
            Widget.all.Cache.Get_Path (To_Item (Widget.all.Cache, Row)));
      end if;
   end Set;

   procedure Set_Column_Data
     (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data   : Column_Data)
   is
      FG, BG : Gdk.RGBA.Gdk_RGBA;
      Style  : Gtk.Style_Context.Gtk_Style_Context;
      Offset : constant Gint := Gint (Data.Column - 1) * 4;
   begin
      Glib.Properties.Set_Property
        (Data.Text_Renderer,
         Gtk.Cell_Renderer_Text.Background_Set_Property,
         True);
      Glib.Properties.Set_Property
        (Data.Text_Renderer,
         Gtk.Cell_Renderer_Text.Foreground_Set_Property,
         True);
      Glib.Properties.Set_Property
        (Data.Text_Renderer,
         Gtk.Cell_Renderer.Cell_Background_Set_Property,
         True);
      Style := Gtk.Style_Context.Get_Style_Context (Data.Browser);
      if Gtk.Tree_Model.Get_Boolean (Model, Iter, Offset + 3) then
         --           if Data.Browser.Has_Visible_Focus then
         Style.all.Get_Color (Gtk.Enums.Gtk_State_Flag_Selected, FG);
         Style.all.Get_Background_Color (Gtk.Enums.Gtk_State_Flag_Selected, BG);
         --           else
         --              Style.Get_Color (Gtk_State_Flag_Active, FG);
         --              Style.Get_Background_Color (Gtk_State_Flag_Active, BG);
         --           end if;
      else
         Style.all.Get_Color (Gtk.Enums.Gtk_State_Flag_Normal, FG);
         Style.all.Get_Background_Color (Gtk.Enums.Gtk_State_Flag_Normal, BG);
      end if;

      Gdk.RGBA.Set_Property (Data.Text_Renderer,
                             Gtk.Cell_Renderer_Text.Foreground_Rgba_Property,
                             FG);
      Gdk.RGBA.Set_Property (Data.Text_Renderer,
                             Gtk.Cell_Renderer_Text.Background_Rgba_Property,
                             BG);
      Gdk.RGBA.Set_Property
        (Data.Text_Renderer,
         Gtk.Cell_Renderer.Cell_Background_Rgba_Property,
         BG);

      declare
         Flags : constant Gint :=
                   Gtk.Tree_Model.Get_Int  (Model, Iter, Offset + 2);
         Name  : constant Item_Name :=
                   Get_Name (Model, Iter, Offset + 1);
         Path  : Gtk.Tree_Model.Gtk_Tree_Path :=
                   Gtk.Tree_Model.Get_Path (Model, Iter);
         Icon  : Icon_Data :=
                   Get_Icon
                     (Data.Browser,
                      Name,
                      Get_Type (Model, Iter, Offset),
                      Flags in Cached_Directory,
                      Flags /= Cached_Never_Directory);

         use type Gdk.Pixbuf.Gdk_Pixbuf;
      begin
         case Icon.Kind is
            when Stock_ID =>
               if Icon.Name'Length > 0 then
                  Glib.Properties.Set_Property
                    (Data.Icon_Renderer,
                     Gtk.Cell_Renderer_Pixbuf.Stock_Id_Property,
                     Icon.Name);
               else
                  Set_Null (Data.Icon_Renderer);
               end if;
            when GIcon =>
               if Icon.Icon = null then
                  Set_Null (Data.Icon_Renderer);
               else
                  Glib.Properties.Set_Property
                    (Data.Icon_Renderer,
                     Glib.Properties.Property_Object (Glib.Build ("gicon")),
                     Icon.Icon);
                  Unref (Icon.Icon);
               end if;
            when Pixbuf =>
               if Icon.Image = null then
                  Set_Null (Data.Icon_Renderer);
               else
                  Gdk.Pixbuf.Conversions.Set_Pixbuf_Property
                    (Object => Data.Icon_Renderer,
                     Value  => Gdk.Pixbuf.Conversions.To_Value (Icon.Image));
                  Gdk.Pixbuf.Unref (Icon.Image);
               end if;
            when Themed =>
               Glib.Properties.Set_Property
                 (Data.Icon_Renderer,
                  Gtk.Cell_Renderer_Pixbuf.Icon_Name_Property,
                  Icon.Name);
         end case;
         Glib.Properties.Set_Property
           (Data.Text_Renderer,
            Gtk.Cell_Renderer_Text.Text_Property,
            String (Name));
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Column_Data"));
   end Set_Column_Data;

   procedure Set_Current
     (Widget    : not null access Gtk_Directory_Items_View_Record'Class;
      Directory : Gtk.Tree_Model.Gtk_Tree_Iter;
      Changed   : in out Boolean)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      Changed := Changed or Directory /= Widget.all.Get_Directory;
      if Changed or else Widget.all.Content.all.Cache.all.Refreshing > 0 then
         if
           0 /= (Get_Tracing (Widget.all.Content.all.Cache) and
                     Trace_Set_Directory)
         then
            Trace
              (Widget.all.Content.all.Cache,
               Get_Depth (Widget.all.Content.all.Cache),
               "Items root seting to "
               & String
                 (Item_Path'
                      (Get_Path (Widget.all.Content.all.Cache, Directory))));
         end if;
         declare
            Columns : constant Positive :=
                        Widget.all.Columns.all.Get_Major_Columns;
         begin
            Ref (Widget.all.Markup);
            Ref (Widget.all.Content);
            Widget.all.Markup.all.Set_Null_Reference;
            Widget.all.Columns.all.Set_Null_Reference;
            if Widget.all.Markup.all.Selected /= 0 then
               --               Glib.Messages.Log
               --               (  Gtk.Missed.GtkAda_Contributions_Domain,
               --                  Glib.Messages.Log_Level_Critical,
               --                  (  "Persisting selection removed"
               --                  &  Where ("Set_Current")
               --               )  );
               Widget.all.Markup.all.Selected := 0;
            end if;
            if
              Widget.all.Content.all.Root /= Gtk.Tree_Model.Null_Gtk_Tree_Path
            then
               Gtk.Tree_Model.Path_Free (Widget.all.Content.all.Root);
               Widget.all.Content.all.Root := Gtk.Tree_Model.Null_Gtk_Tree_Path;
            end if;
            if Directory /= Gtk.Tree_Model.Null_Iter then
               Widget.all.Content.all.Root :=
                 Widget.all.Content.all.Cache.all.Tree.all.Get_Path (Directory);
            end if;
            Refilter (Widget);
            Widget.all.Markup.all.Set_Reference (Widget.all.Content);
            Widget.all.Columns.all.Set_Reference
              (Widget.all.Markup,
               Columns,
               Gtk.Tree_Model.Null_Iter); -- Root of columns is always root of items list
            Unref (Widget.all.Content);
            Unref (Widget.all.Markup);
            Columns_Autosize (Widget);
         end;
         if 0 /= (Get_Tracing (Widget.all.Content.all.Cache) and
                    Trace_Set_Directory)
         then
            Widget.all.Content.all.Cache.all.Trace
              (Get_Depth (Widget.all.Content.all.Cache),
               "Items root has been set to "
               & String (Item_Path'(Widget.all.Get_Directory)));
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Current"));
   end Set_Current;

   procedure Set_Current_Directory
     (Widget    : not null access Gtk_Directory_Tree_View_Record;
      Directory : Item_Path)
   is
      Wait        : Gtk.Missed.Wait_Cursor (+Widget);
      Best_Match  : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
      Exact_Match : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if 0 /= (Get_Tracing (Widget.all.Cache) and Trace_Set_Directory) then
         Trace
           (Widget.all.Cache,
            Get_Depth (Widget.all.Cache),
            "Set to " & String (Directory));
      end if;
      Cache (Widget.all.Cache, Directory, Best_Match, Exact_Match);
      if Best_Match /= Gtk.Tree_Model.Null_Iter then
         if 0 /= (Get_Tracing (Widget.all.Cache) and Trace_Set_Directory)
         then
            declare
               Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                        Gtk.Tree_Store.Get_Path (Widget.all.Cache.all.Tree, Best_Match);
            begin
               Trace
                 (Widget.all.Cache,
                  Get_Depth (Widget.all.Cache),
                  "Set to "
                  & String (Directory)
                  & ", best match "
                  & String (Get_Name (Widget.all.Cache.all.Tree, Best_Match))
                  & " = "
                  & Gtk.Tree_Model.To_String (Path));
               Gtk.Tree_Model.Path_Free (Path);
            end;
         end if;
         while Exact_Match /= Gtk.Tree_Model.Null_Iter loop
            exit when
              (Gtk.Tree_Store.Get_Int (Widget.all.Cache.all.Tree, Exact_Match, 2)
               in Cached_Children);
            Exact_Match :=
              Gtk.Tree_Store.Parent (Widget.all.Cache.all.Tree, Exact_Match);
         end loop;
         if Exact_Match /= Gtk.Tree_Model.Null_Iter then
            Best_Match := From_Item (Widget.all.Cache, Exact_Match);
            if Best_Match /= Gtk.Tree_Model.Null_Iter then
               declare
                  Path : Gtk.Tree_Model.Gtk_Tree_Path;
               begin
                  Path := Widget.all.Cache.Get_Path (Best_Match);
                  if
                    0 /= (Get_Tracing (Widget.all.Cache) and
                              Trace_Set_Directory)
                  then
                     Trace
                       (Widget.all.Cache,
                        Get_Depth (Widget.all.Cache),
                        "Set to "
                        & String (Directory)
                        & ", selected path ="
                        & Gtk.Tree_Model.To_String (Path));
                  end if;
                  Widget.all.Expand_To_Path (Path);
                  Widget.all.Scroll_To_Cell
                    (Path,
                     Widget.all.Get_Column (0),
                     False,
                     0.0,
                     0.0);
                  Widget.all.Get_Selection.all.Select_Path (Path);
                  Gtk.Tree_Model.Path_Free (Path);
                  return;
               exception
                  when others =>
                     Gtk.Tree_Model.Path_Free (Path);
                     raise;
               end;
            end if;
         end if;
      else
         if 0 /= (Get_Tracing (Widget.all.Cache) and Trace_Set_Directory)
         then
            Trace
              (Widget.all.Cache,
               Get_Depth (Widget.all.Cache),
               "Set to " & String (Directory) & ", no best match");
         end if;
      end if;
      Widget.all.Get_Selection.all.Unselect_All;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Current_Directory"));
   end Set_Current_Directory;

   procedure Set_Editable
     (Widget   : not null access Gtk_Directory_Tree_View_Record;
      Editable : Boolean) is
   begin
      Glib.Properties.Set_Property
        (Widget.all.Name_Renderer,
         Gtk.Cell_Renderer_Text.Editable_Property,
         Editable);
   end Set_Editable;

   procedure Set_Editable
     (Widget   : not null access Gtk_Directory_Items_View_Record;
      Editable : Boolean) is
   begin
      for Index in Widget.all.Name_Renderers'Range loop
         Glib.Properties.Set_Property
           (Widget.all.Name_Renderers (Index),
            Gtk.Cell_Renderer_Text.Editable_Property,
            Editable);
      end loop;
   end Set_Editable;

   procedure Set_Item
     (Store : not null access Gtk_Abstract_Directory_Record'Class;
      Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Item  : Directory_Item)
   is
      Flag : Gint;
   begin
      if 0 /= (Store.all.Tracing and Trace_Cache) then
         if Item.Directory then
            Store.all.Trace
              (Store.all.Depth,
               "insert dir "
               & UTF8_String (Item.Name)
               & " ["
               & UTF8_String (Item.Kind)
               & ']');
         else
            Store.all.Trace
              (Store.all.Depth,
               "insert item "
               & UTF8_String (Item.Name)
               & " ["
               & UTF8_String (Item.Kind)
               & ']');
         end if;
      end if;
      Store.all.Tree.all.Set (Row, 1, UTF8_String (Item.Name));
      Store.all.Tree.all.Set (Row, 0, UTF8_String (Item.Kind));
      if Item.Directory then
         case Item.Policy is
            when Cache_Never =>
               Flag := Cached_Never_Directory;
            when Cache_Expanded =>
               Flag := Cached_Expanded_Directory;
            when Cache_Ahead =>
               Flag := Cached_Ahead_Directory;
         end case;
      else
         Flag := Cached_Item;
      end if;
      Store.all.Tree.all.Set (Row, 2, Flag);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Item"));
   end Set_Item;

   procedure Set_Null
     (Cell : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class)
   is
   begin
      Glib.Properties.Set_Property
        (Cell,
         Glib.Properties.Property_Address
           (Gtk.Cell_Renderer_Pixbuf.Stock_Id_Property),
         System.Null_Address);
   end Set_Null;

   procedure Set_Selection
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Index  : Positive;
      State  : Boolean)
   is
      Size    : constant Natural := Widget.all.Get_Directory_Size;
      Changed : Boolean := False;

      use type Gtk.Enums.Gtk_Selection_Mode;
   begin
      if Index > Size then
         return;
      end if;
      case Widget.all.Markup.all.Mode is
         when Gtk.Enums.Selection_None =>
            null;
         when Gtk.Enums.Selection_Single | Gtk.Enums.Selection_Browse =>
            if State xor Widget.all.Is_Selected (Index) then
               if State then
                  -- Deselect any selected
                  for Item in 1 .. Size loop
                     if Widget.all.Is_Selected (Item) then
                        Widget.all.Change_Selection (Item, False);
                        exit;
                     end if;
                  end loop;
                  Widget.all.Change_Selection (Index, True);
                  Changed := True;
               elsif
                 Widget.all.Markup.all.Mode = Gtk.Enums.Selection_Single
               then
                  Widget.all.Change_Selection (Index, False);
                  Changed := True;
               end if;
            end if;
         when Gtk.Enums.Selection_Multiple =>
            if Widget.all.Is_Selected (Index) xor State then
               Widget.all.Change_Selection (Index, State);
               Changed := True;
            end if;
      end case;
      if Changed then
         Widget.all.Selection_Changed;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Selection (items)"));
   end Set_Selection;

   procedure Set_Selection
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Name   : Item_Name;
      State  : Boolean)
   is
      Position : constant Natural := Widget.all.Get_Index (Name);
   begin
      if Position > 0 then
         Widget.all.Set_Selection (Position, State);
      end if;
   end Set_Selection;

   procedure Set_Selection_Mode
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Mode   : Gtk.Enums.Gtk_Selection_Mode)
   is
      use type Gtk.Enums.Gtk_Selection_Mode;
   begin
      if Widget.all.Markup.all.Mode = Mode then
         return;
      end if;
      declare
         Changed : Boolean := False;
         Size    : constant Natural := Widget.all.Get_Directory_Size;
         Current : Natural;

         use type Gtk.Enums.Gtk_Selection_Mode;
      begin
         Widget.all.Markup.all.Mode := Mode;
         case Mode is
            when Gtk.Enums.Selection_None =>
               -- We just deselect anything selected
               for Index in 1 .. Size loop
                  if Widget.all.Is_Selected (Index) then
                     Widget.all.Change_Selection (Index, False);
                     Changed := True;
                  end if;
               end loop;
            when Gtk.Enums.Selection_Single | Gtk.Enums.Selection_Browse =>
               case Widget.all.Get_Selection_Size is
                  when 0 =>
                     if Mode = Gtk.Enums.Selection_Browse then
                        -- Nothing  selected.  When  there  is a current
                        -- item we select it, else we do the first item,
                        -- otherwise nothing.
                        Current := Widget.all.Get_Current;
                        if Current > 0 then
                           Widget.all.Change_Selection (Current, True);
                           Changed := True;
                        elsif Size > 0 then
                           Widget.all.Change_Selection (1, True);
                           Changed := True;
                        end if;
                     end if;
                  when 1 =>
                     null;  -- One item selection, nothing to do
                  when others =>
                     -- Multiple selection, we shall deselect everything
                     -- but  one. Let's never deselect the current item,
                     -- otherwise it will be the first one that stays.
                     Current := Widget.all.Get_Current;
                     for Index in reverse 1 .. Size loop
                        if
                          Current /= Index and then
                          Widget.all.Is_Selected (Index)
                        then
                           Widget.all.Change_Selection (Index, False);
                           Changed := True;
                           exit when Widget.all.Get_Selection_Size = 1;
                        end if;
                     end loop;
               end case;
            when Gtk.Enums.Selection_Multiple =>
               -- There is nothing to do
               null;
         end case;
         if Changed then
            Widget.all.Selection_Changed;
         end if;
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Selection_Mode"));
   end Set_Selection_Mode;

   procedure Set_Trace_File (File_Name : String) is
      use Ada.Text_IO;
   begin
      if Has_File then
         Close (File);
         Has_File := False;
      end if;
      Set_Trace_File;
      Open (File, Out_File, File_Name);
      Has_File := True;
   end Set_Trace_File;

   procedure Set_Trace_File is
      use Ada.Text_IO;
   begin
      if Has_File then
         Close (File);
         Has_File := False;
      end if;
   end Set_Trace_File;

   procedure Set_Tracing
     (Store   : not null access Gtk_Abstract_Directory_Record'Class;
      Tracing : Traced_Actions) is
   begin
      Store.all.Tracing := Tracing;
   end Set_Tracing;

   procedure Set_Tree_Icon
     (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data   : Gtk_Directory_Tree_View)
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;

      Item  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                To_Item (Data.all.Cache, Iter);
      Flags : constant Gint :=
                Gtk.Tree_Store.Get_Int (Data.all.Cache.all.Tree, Item, 2);
      Icon  : Icon_Data :=
                Get_Icon
                  (Data,
                   Get_Type (Data.all.Cache.all.Tree, Item),
                   Expanded (Data, Iter),
                   Flags /= Cached_Never_Directory,
                   Gtk.Tree_Store.Parent (Data.all.Cache.all.Tree, Item) =
                     Gtk.Tree_Model.Null_Iter);

      use type Gdk.Pixbuf.Gdk_Pixbuf;
   begin
      case Icon.Kind is
         when Stock_ID =>
            if Icon.Name'Length > 0 then
               Glib.Properties.Set_Property
                 (Cell,
                  Gtk.Cell_Renderer_Pixbuf.Stock_Id_Property,
                  Icon.Name);
            else
               Set_Null (Cell);
            end if;
         when GIcon =>
            if Icon.Icon = null then
               Set_Null (Cell);
            else
               Glib.Properties.Set_Property
                 (Cell,
                  Glib.Properties.Property_Object (Glib.Build ("gicon")),
                  Icon.Icon);
               Unref (Icon.Icon);
            end if;
         when Pixbuf =>
            if Icon.Image = null then
               Set_Null (Cell);
            else
               Gdk.Pixbuf.Conversions.Set_Pixbuf_Property
                 (Object => Cell,
                  Value  => Gdk.Pixbuf.Conversions.To_Value (Icon.Image));
               Gdk.Pixbuf.Unref (Icon.Image);
            end if;
         when Themed =>
            Glib.Properties.Set_Property
              (Cell,
               Gtk.Cell_Renderer_Pixbuf.Icon_Name_Property,
               Icon.Name);
      end case;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Tree_Icon"));
   end Set_Tree_Icon;

   procedure Set_Tree_Name
     (Column : not null access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Cell   : not null access Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record'Class;
      Model  : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Data   : Gtk_Directory_Tree_View)
   is
      Item  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                To_Item (Data.all.Cache, Iter);
      Flags : constant Gint :=
                Gtk.Tree_Store.Get_Int (Data.all.Cache.all.Tree, Item, 2);

      use type Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Glib.Properties.Set_Property
        (Cell,
         Gtk.Cell_Renderer_Text.Text_Property,
         UTF8_String
           (Get_Name
                (Data,
                 Get_Name (Data.all.Cache.all.Tree, Item),
                 Get_Type (Data.all.Cache.all.Tree, Item),
                 Expanded (Data, Iter),
                 Flags /= Cached_Never_Directory,
                 Gtk.Tree_Store.Parent (Data.all.Cache.all.Tree, Item) =
                       Gtk.Tree_Model.Null_Iter)));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Set_Tree_Name"));
   end Set_Tree_Name;

   procedure Split
     (Store  : access Gtk_Abstract_Directory_Record'Class;
      Item   : Item_Path;
      Path   : out Path_Node_Ptr;
      Length : out Positive) is
   begin
      Path := new Path_Node'(Item'Length, null, Item);
      Length := 1;
      loop
         declare
            Directory : constant Item_Path :=
                          Store.all.Get_Directory (Path.all.Directory);
         begin
            Path := new Path_Node'(Directory'Length, Path, Directory);
            Length := Length + 1;
         end;
      end loop;
   exception
      when Ada.IO_Exceptions.Name_Error =>
         null;
   end Split;

   function To_Filtered
     (Model      : not null access Gtk_Directory_Items_Store_Record;
      Unfiltered : Gtk.Tree_Model.Gtk_Tree_Path) return Gint
   is
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Unfiltered = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null argument path" & Where ("To_Filtered"));
         return -1;
      elsif Model.all.Root = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "No model root path set" & Where ("To_Filtered"));
         return -1;
      end if;
      declare
         Indices : Gint_Array := Gtk.Tree_Model.Get_Indices (Unfiltered);
         Root    : constant Gint_Array := Gtk.Tree_Model.Get_Indices (Model.all.Root);
         Count   : Gint := -1;

         use type Gtk.Tree_Model.Gtk_Tree_Iter;
      begin
         if
           Indices'Length /= Root'Length + 1 or else
           Indices (Indices'First .. Indices'Last - 1) /= Root
         then -- Not a prefx
            return -1;
         end if;
         declare
            Row   : Gtk.Tree_Model.Gtk_Tree_Iter;
            Found : Boolean := False;
            Index : Gint renames Indices (Indices'Last);
         begin
            Row := Model.all.Cache.all.Tree.all.Children
              (Model.all.Cache.all.Tree.all.Get_Iter (Model.all.Root));
            --
            -- Counting filtered rows before ours
            --
            loop
               if Row = Gtk.Tree_Model.Null_Iter then
                  Glib.Messages.Log
                    (Gtk.Missed.GtkAda_Contributions_Domain,
                     Glib.Messages.Log_Level_Critical,
                     "Wrong unfiltered path" &  Where ("To_Filtered"));
                  return -1;
               end if;
               case Gtk.Tree_Store.Get_Int (Model.all.Cache.all.Tree, Row, 2) is
                  when Cached_Directory => -- Directory is always
                     Count := Count + 1;   -- visible
                     Found := True;
                  when Cached_Item =>      -- Items are to be filtered
                     Found := Model.all.View.all.Filter
                       (False,
                        Get_Name (Model.all.Cache.all.Tree, Row),
                        Get_Type (Model.all.Cache.all.Tree, Row));
                     if Found then
                        Count := Count + 1;
                     end if;
                  when others =>
                     Found := False;
               end case;
               exit when Index = 0;
               Index := Index - 1;
               Model.all.Cache.all.Tree.all.Next (Row);
            end loop;
            if Found then
               return Count;
            else
               return -1;
            end if;
         end;
      end;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("To_Filtered"));
         return -1;
   end To_Filtered;

   function To_Filtered
     (Model      : not null access Gtk_Directory_Items_Store_Record;
      Unfiltered : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Unfiltered = Gtk.Tree_Model.Null_Iter then
         return -1;
      else
         declare
            Path : Gtk.Tree_Model.Gtk_Tree_Path;
         begin
            Path := Model.all.Cache.all.Tree.all.Get_Path (Unfiltered);
            if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
               return -1;
            else
               declare
                  Index : constant Gint := Model.To_Filtered (Path);
               begin
                  Gtk.Tree_Model.Path_Free (Path);
                  return Index;
               end;
            end if;
         end;
      end if;
   end To_Filtered;

   function To_Filtered
     (Model      : not null access Gtk_Directory_Items_Store_Record;
      Unfiltered : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      if Model.To_Filtered (Unfiltered) >= 0 then
         return Unfiltered;
      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end To_Filtered;

   function To_Filtered
     (Model      : not null access Gtk_Directory_Items_Store_Record;
      Unfiltered : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Index : constant Gint := Model.To_Filtered (Unfiltered);
   begin
      if Index >= 0 then
         declare
            Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                     Gtk.Tree_Model.Gtk_Tree_Path_New;
         begin
            Gtk.Tree_Model.Append_Index (Path, Index);
            return Path;
         end;
      else
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
      end if;
   end To_Filtered;

   function To_Filtered
     (Model      : not null access Gtk_Directory_Items_Store_Record;
      Unfiltered : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Index : constant Gint := Model.To_Filtered (Unfiltered);
   begin
      if Index < 0 then
         return Gtk.Tree_Model.Null_Gtk_Tree_Path;
      else
         declare
            Path : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                     Gtk.Tree_Model.Gtk_Tree_Path_New;
         begin
            Gtk.Tree_Model.Append_Index (Path, Index);
            return Path;
         end;
      end if;
   end To_Filtered;

   function To_Item
     (Store     : not null access Gtk_Abstract_Directory_Record'Class;
      Directory : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      return Directory;
   end To_Item;

   function To_Marked
     (Widget : not null access Gtk_Directory_Items_View_Record;
      Item   : Gtk.Tree_Model.Gtk_Tree_Iter) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      if Item = Gtk.Tree_Model.Null_Iter then
         return Gtk.Tree_Model.Null_Iter;
      end if;
      declare
         Unfiltered : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                        Widget.all.Content.all.Cache.all.Tree.all.Get_Path (Item);
         Filtered   : constant Gtk.Tree_Model.Gtk_Tree_Path :=
                        To_Filtered (Widget.all.Content, Unfiltered);
         Row        : Gtk.Tree_Model.Gtk_Tree_Iter;
      begin
         if Filtered = Gtk.Tree_Model.Null_Gtk_Tree_Path then
            return Gtk.Tree_Model.Null_Iter;
         else
            Row := Widget.all.Content.all.Get_Iter (Filtered);
            if Row = Gtk.Tree_Model.Null_Iter then
               return Gtk.Tree_Model.Null_Iter;
            else
               Gtk.Tree_Model.Path_Free (Unfiltered);
               Gtk.Tree_Model.Path_Free (Filtered);
               return Widget.all.Markup.all.To_Extension (Row);
            end if;
         end if;
      end;
   end To_Marked;

   procedure Trace
     (Store : not null access Gtk_Abstract_Directory_Record;
      Depth : Natural;
      Text  : String)
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
   begin
      if
        Trace_To_Output_Only /=
          (Store.all.Tracing and (Trace_To_Both or Trace_To_Output_Only))
      then
         Gtk.Main.Router.Trace (Depth * "  " & Text);
      end if;
      if
        0 /= (Store.all.Tracing and (Trace_To_Both or Trace_To_Output_Only))
      then
         if Has_File then
            Put_Line (File, Depth * "  " & Text);
         else
            Put_Line (Depth * "  " & Text);
         end if;
      end if;
   end Trace;

   function Trace
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Mode  : Traced_Actions) return Boolean
   is
      use Ada.Text_IO;

      function Image (Kind : Gint) return String is
      begin
         case Kind is
            when Cached_New =>
               return "new";
            when Cached_Never_Directory =>
               return "dir (never)";
            when Cached_Ahead_Directory =>
               return "dir (ahead)";
            when Cached_Expanded_Directory =>
               return "dir (expand)";
            when Cached_Item =>
               return "item";
            when others =>
               return "?";
         end case;
      end Image;

      Depth : constant Natural := Gtk.Tree_Model.Get_Indices (Path)'Length - 1;
      Text  : constant String  := String (Get_Name (Model, Iter)) &
                " - " &
                Image (Gtk.Tree_Model.Get_Int (Model, Iter, 2));

      use Ada.Strings.Fixed;
   begin
      if
        Trace_To_Output_Only /=
          (Mode and (Trace_To_Both or Trace_To_Output_Only))
      then
         Gtk.Main.Router.Trace (Depth * "  " & Text);
      end if;
      if
        0 /= (Mode and (Trace_To_Both or Trace_To_Output_Only))
      then
         if Has_File then
            Put_Line (File, Depth * "  " & Text);
         else
            Put_Line (Depth * "  " & Text);
         end if;
      end if;
      return False;
   end Trace;

   procedure Trace
     (Model   : not null access Gtk.Tree_Model.Gtk_Root_Tree_Model_Record'Class;
      Tracing : Traced_Actions := Trace_To_Both)
   is
      use Traverse;
   begin
      Foreach (Gtk.Tree_Model.To_Interface (Model), Trace'Access, Tracing);
   end Trace;

   pragma Warnings (On, "declaration hides ""Params""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Abstract_Browser;
