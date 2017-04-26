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
--____________________________________________________________________--

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Gdk;                       use Gdk;
with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gdk.Window;                use Gdk.Window;
with GIO.Content_Type;          use GIO.Content_Type;
with GLib.Properties;           use GLib.Properties;
with GLib.Messages;             use GLib.Messages;
with GLib.Values.Handling;      use GLib.Values.Handling;
with GLib.Unicode;              use GLib.Unicode;
with Gtk.Main.Router;           use Gtk.Main.Router;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Style;                 use Gtk.Style;
with Gtk.Style_Context;         use Gtk.Style_Context;

with Ada.Text_IO;
with Gdk.Pixbuf.Conversions;
with GtkAda.Types;
with GLib.Object.Checked_Destroy;
with Interfaces.C.Strings;

package body Gtk.Abstract_Browser is
   use GLib.Values;
   use Gtk.Cell_Renderer_Pixbuf;
   use Gtk.Enums;
   use Gtk.Widget;

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
      Directory : Item_Path (1..Length);
   end record;

   function "=" (Left, Right : Item_Path_Reference) return Boolean is
   begin
      if Left.Ptr = null then
         if Right.Ptr = null then
            return True;
         else
            return Right.Ptr.Length = 0;
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
         return Left.Ptr.Path = Right;
      end if;
   end "=";

   function "=" (Left : Item_Path; Right : Item_Path_Reference)
      return Boolean is
   begin
      if Right.Ptr = null then
         return Left'Length = 0;
      else
         return Left = Right.Ptr.Path;
      end if;
   end "=";

   procedure Free is
      new Ada.Unchecked_Deallocation (Path_Node, Path_Node_Ptr);

   procedure Release (Ptr : in out Path_Node_Ptr);

   procedure Split
             (  Store  : access Gtk_Abstract_Directory_Record'Class;
                Item   : Item_Path;
                Path   : out Path_Node_Ptr;
                Length : out Positive
             );

   function "+" (Widget : access Gtk_Widget_Record'Class)
      return Gtk_Widget is
      Parent : constant Gtk_Widget := Get_Parent (Widget);
   begin
      if Parent = null then
         return Widget.all'Unchecked_Access;
      else
         return Parent;
      end if;
   end "+";

   Directory_Items_Class : Ada_GObject_Class := Uninitialized_Class;
   --
   -- Signals of abstract directory
   --
   Abstract_Directory_Signal_Names : constant
                                     GtkAda.Types.Chars_Ptr_Array :=
      (  0 => Interfaces.C.Strings.New_String ("read-error"),
         1 => Interfaces.C.Strings.New_String ("rewind-error"),
         2 => Interfaces.C.Strings.New_String ("refreshed"),
         3 => Interfaces.C.Strings.New_String ("progress"),
         4 => Interfaces.C.Strings.New_String ("item-inserted"),
         5 => Interfaces.C.Strings.New_String ("item-renamed"),
         6 => Interfaces.C.Strings.New_String ("item-deleting"),
         7 => Interfaces.C.Strings.New_String ("item-deleted")
      );
   Abstract_Directory_Signal_Parameters : constant
                                          Signal_Parameter_Types :=
      (  0 => (0 => GType_String,  1 => GType_String),
         1 => (0 => GType_String,  1 => GType_String),
         2 => (0 => GType_String,  1 => GType_None),
         3 => (0 => GType_String,  1 => GType_Double),
         4 => (0 => GType_Pointer, 1 => Path_Get_Type),
         5 => (0 => GType_Pointer, 1 => GType_String),
         6 => (0 => GType_Pointer, 1 => Path_Get_Type),
         7 => (0 => GType_String,  1 => GType_None)
      );
   Read_Error_ID   : Signal_ID := Invalid_Signal_Id;
   Rewind_Error_ID : Signal_ID;
   Refreshed_ID    : Signal_ID;
   Progress_ID     : Signal_ID;
   Inserted_ID     : Signal_ID;
   Renamed_ID      : Signal_ID;
   Deleted_ID      : Signal_ID;
   Deleting_ID     : Signal_ID;
   --
   -- Signals of directory items
   --
   Directory_Items_Signal_Names : constant
                                  GtkAda.Types.Chars_Ptr_Array :=
      (  0 => Interfaces.C.Strings.New_String ("directory-changed"),
         1 => Interfaces.C.Strings.New_String ("selection-changed")
      );

   function Get_Name
            (  Model  : Gtk_Tree_Model;
               Iter   : Gtk_Tree_Iter;
               Column : GInt := 1
            )  return Item_Name is
      Data : GValue;
   begin
      Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Name :=
                               Item_Name (Get_String (Data)) do
         Unset (Data);
      end return;
   end Get_Name;

   function Get_Name
            (  Model  : Gtk_Tree_Store;
               Iter   : Gtk_Tree_Iter;
               Column : GInt := 1
            )  return Item_Name is
      Data : GValue;
   begin
      Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Name :=
                               Item_Name (Get_String (Data)) do
         Unset (Data);
      end return;
   end Get_Name;

   function Get_Type
            (  Model  : Gtk_Tree_Model;
               Iter   : Gtk_Tree_Iter;
               Column : GInt := 0
            )  return Item_Type is
      Data : GValue;
   begin
      Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Type :=
                               Item_Type (Get_String (Data)) do
         Unset (Data);
      end return;
   end Get_Type;

   function Get_Type
            (  Model  : Gtk_Tree_Store;
               Iter   : Gtk_Tree_Iter;
               Column : GInt := 0
            )  return Item_Type is
      Data : GValue;
   begin
      Get_Value (Model, Iter, Column, Data);
      return Result : constant Item_Type :=
                               Item_Type (Get_String (Data)) do
         Unset (Data);
      end return;
   end Get_Type;

   procedure Enter
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Path  : Item_Path;
                High  : out GDouble
             )  is
      pragma Inline (Enter);
   begin
      if Store.Depth = 0 then
         Store.Last_Time := Clock;
         Store.Low  := 0.0;
         Store.High := 1.0;
         High       := 1.0;
         Store.Progress (Path, 0.0);
      else
         High := Store.High;
         declare
            Now : constant Time := Clock;
         begin
            if Now - Store.Last_Time > 0.2 then
               Store.Progress (Path, Store.Low);
            end if;
         end;
      end if;
      Store.Depth := Store.Depth + 1;
   end Enter;

   procedure Leave
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Path  : Item_Path;
                High  : GDouble
             )  is
      pragma Inline (Leave);
   begin
      Store.Depth := Store.Depth - 1;
      Store.Low   := High;
      Store.High  := High;
      if Store.Depth = 0 then
         Store.Progress (Path, 1.0);
      else
         declare
            Now : constant Time := Clock;
         begin
            if Now - Store.Last_Time > 0.2 then
               Store.Progress (Path, High);
            end if;
         end;
      end if;
   end Leave;

   function Expanded
            (  Tree : not null access Gtk_Tree_View_Record'Class;
               Iter : Gtk_Tree_Iter
            )  return Boolean is
      Path   : constant Gtk_Tree_Path :=
               Get_Path (Get_Model (Tree), Iter);
      Result : constant Boolean := Tree.Row_Expanded (Path);
   begin
      Path_Free (Path);
      return Result;
   end Expanded;

   procedure Activated
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : Positive
             )  is
   begin
      if Widget.Is_Directory (Index) then
         Widget.Directories.Set_Current_Directory
         (  Widget.Get_Path (Index)
         );
      elsif Widget.Is_Editable then
         --
         -- When  the  widget has editable names, then activation starts
         -- editing.
         --
         declare
            Row    : Gtk_Tree_Iter := Widget.Get_Iter (Index);
            Column : Positive      := Widget.Get_Column (Index);
            Path   : Gtk_Tree_Path;
         begin
            if Row = Null_Iter then
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Critical,
                  (  "Invisible item activated"
                  &  Where ("Activated")
               )  );
            else
               Widget.Columns.To_Columned (Row, Column);
               Path := Get_Path (Widget.Columns, Row);
               Set_Cursor_On_Cell
               (  Tree_View     => Widget,
                  Path          => Path,
                  Start_Editing => True,
                  Focus_Column  =>
                     Widget.Get_Column (GInt (Column) - 1),
                  Focus_Cell    =>
                     Widget.Name_Renderers (Column).all'Access
               );
               Path_Free (Path);
            end if;
         end;
      end if;
   end Activated;

   procedure Add_Folder
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : in out Gtk_Tree_Iter;
                Directory : Item_Path;
                Expanded  : Boolean;
                Updated   : out Boolean
             )  is
      Item    : Gtk_Tree_Iter;
      Path    : Gtk_Tree_Path;
      High    : GDouble;
      Step    : GDouble;
      Recurse : Natural := 0;
   begin
      Enter (Store, Directory, High);
      if Row = Null_Iter then
         if 0 /= (Store.Tracing and Trace_Read_Directory) then
            Store.Trace
            (  Store.Depth - 1,
               "start adding root"
            );
         end if;
         Item := Get_Iter_First (Store.Tree);
      else
         if 0 /= (Store.Tracing and Trace_Read_Directory) then
            Store.Trace
            (  Store.Depth - 1,
               "start adding " & String (Directory)
            );
         end if;
         if Get_Int (Store.Tree, Row, 2) not in Cached_Directory then
            Updated := False;
            Leave (Store, Directory, High);
            if 0 /= (Store.Tracing and Trace_Read_Directory) then
               Store.Trace
               (  Store.Depth,
                  "stop adding " & String (Directory) & " (item)"
               );
            end if;
            return;
         end if;
         Item := Store.Tree.Children (Row);
      end if;
      if Item /= Null_Iter then
         -- This directory is already cached
         if 0 /= (Store.Tracing and Trace_Read_Directory) then
            Store.Trace
            (  Store.Depth - 1,
               (  "stop adding "
               &  String (Directory)
               &  " (already cached, first: "
               &  String (Get_Name (Store.Tree, Item))
               &  ')'
            )  );
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
                  if Row /= Null_Iter then
                     Set_Item (Store, Row, Data);
                  end if;
                  Updated := True;
                  Leave (Store, Directory, High);
                  return;
               when Cache_Expanded =>
                  Data.Directory := True;
                  if Row /= Null_Iter then
                     Set_Item (Store, Row, Data);
                  end if;
               when Cache_Ahead =>
                  null;
            end case;
         end;
      exception
         when Error : Data_Error =>
            Store.Tree.Set (Row, 2, Cached_Never_Directory);
--          Store.Tree.Remove (Row);  Removing the directory from cache
--          Row := Null_Iter;
            Rewind_Error
            (  Store,
               Exception_Message (Error),
               Directory
            );
            Updated := True;
            Leave (Store, Directory, High);
            if 0 /= (Store.Tracing and Trace_Read_Directory) then
               Trace
               (  Store,
                  Store.Depth,
                  (  "stop adding "
                  &  String (Directory)
                  &  " (error: "
                  &  Exception_Message (Error)
                  &  ')'
               )  );
            end if;
            return;
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Rewind error:"
               &  Exception_Information (Error)
               &  Where ("Add_Folder")
            )  );
            Store.Tree.Set (Row, 2, Cached_Never_Directory);
--          Store.Tree.Remove (Row);
--          Row := Null_Iter;
            Rewind_Error
            (  Store,
               Exception_Message (Error),
               Directory
            );
            Updated := True;
            Leave (Store, Directory, High);
            if 0 /= (Store.Tracing and Trace_Read_Directory) then
               Store.Trace
               (  Store.Depth,
                  (  "stop adding "
                  &  String (Directory)
                  &  " (fault: "
                  &  Exception_Information (Error)
                  &  ')'
               )  );
            end if;
            return;
      end;
      --
      -- Here  we  will modify the store and we don't know if that would
      -- have an effect on the iterators of. For this reason we save the
      -- iterator as a path, which won't change and restore the iterator
      -- after each suspicious operation.
      --
      if Row /= Null_Iter then
         Path := Get_Path (Store.Tree, Row);
      end if;
      loop
         declare
            Data : Directory_Item renames Read (Store);
            Size : GInt;
         begin
            if 0 /= (Store.Tracing and Trace_Read_Items) then
               if Data.Directory then
                  Store.Trace
                  (  Store.Depth,
                     (  String (Data.Name)
                     &  "   directory, "
                     &  String (Data.Kind)
                     &  ", "
                     &  Caching_Policy'Image (Data.Policy)
                  )  );
               else
                  Store.Trace
                  (  Store.Depth,
                     (  String (Data.Name)
                     &  "   file, "
                     &  String (Data.Kind)
                     &  ", "
                     &  Caching_Policy'Image (Data.Policy)
                  )  );
               end if;
            end if;
            Add_Item (Store, Row, Directory, Data, False, Item, Size);
            if (  Item /= Null_Iter
               and then
                  Data.Directory
               and then
                  Data.Policy in Cache_Expanded..Cache_Ahead
               )
            then
               Recurse := Recurse + 1;
            end if;
            if Path = Null_Gtk_Tree_Path then
               Row := Null_Iter;
            else
               Row := Get_Iter (Store.Tree, Path);
            end if;
         end;
      end loop;
   exception
      when End_Error =>
         --
         -- No  more items. We go though the directory items in order to
         -- check if some of them are themselves directories  needed  to
         -- be cached.
         --
         if Recurse > 0 then
            Step := (Store.High - Store.Low) / GDouble (Recurse);
         else
            Step := 0.0;
         end if;
         for Index in reverse 0..N_Children (Store.Tree, Row) - 1 loop
            exit when Recurse = 0;
            Row := Store.Tree.Nth_Child (Row, Index);
            exit when Row = Null_Iter;
            case Get_Int (Store.Tree, Row, 2) is
               when Cached_Ahead_Directory =>
                  Store.High := Store.Low + Step;
                  Add_Folder
                  (  Store,
                     Row,
                     Get_Path
                     (  Store,
                        Directory,
                        Get_Name (Store.Tree, Row)
                     ),
                     True,
                     Updated
                  );
                  Recurse := Recurse - 1;
               when Cached_Expanded_Directory =>
                  Store.High := Store.Low + Step;
                  if Expanded then
                     Add_Folder
                     (  Store,
                        Row,
                        Get_Path
                        (  Store,
                           Directory,
                           Get_Name (Store.Tree, Row)
                        ),
                        False,
                        Updated
                     );
                  end if;
                  Recurse := Recurse - 1;
               when others =>
                  null;
            end case;
            if Path = Null_Gtk_Tree_Path then
               Row := Null_Iter;
            else
               Row := Get_Iter (Store.Tree, Path);
            end if;
            declare
               Now : constant Time := Clock;
            begin
               if Now - Store.Last_Time > 0.2 then
                  Store.Last_Time := Now;
                  Store.Progress (Directory, Store.Low);
               end if;
            end;
         end loop;
         Path_Free (Path);
         Updated := True;
         Leave (Store, Directory, High);
         if 0 /= (Store.Tracing and Trace_Read_Directory) then
            Store.Trace
            (  Store.Depth,
               "stop adding " & String (Directory)
            );
         end if;
      when Error : Data_Error =>
         if Path = Null_Gtk_Tree_Path then
            Row := Null_Iter;
         else
            Row := Get_Iter (Store.Tree, Path);
            Path_Free (Path);
            Store.Tree.Set (Row, 2, Cached_Never_Directory);
         end if;
         Read_Error
         (  Store,
            Exception_Message (Error),
            Directory
         );
         Updated := True;
         Leave (Store, Directory, High);
         if 0 /= (Store.Tracing and Trace_Read_Directory) then
            Store.Trace
            (  Store.Depth,
               (  "stop adding "
               &  String (Directory)
               &  " (read error: "
               &  Exception_Message (Error)
               &  ')'
            )  );
         end if;
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Read error: "
            &  Exception_Information (Error)
            &  Where ("Add_Folder")
         )  );
         if Path = Null_Gtk_Tree_Path then
            Row := Null_Iter;
         else
            Row := Get_Iter (Store.Tree, Path);
            Path_Free (Path);
            Store.Tree.Set (Row, 2, Cached_Never_Directory);
         end if;
         Read_Error
         (  Store,
            Exception_Message (Error),
            Directory
         );
         Updated := True;
         Leave (Store, Directory, High);
         if 0 /= (Store.Tracing and Trace_Read_Directory) then
            Trace
            (  Store,
               Store.Depth,
               (  "stop adding "
               &  String (Directory)
               &  " (read fault: "
               &  Exception_Message (Error)
               &  ')'
            )  );
         end if;
   end Add_Folder;

   procedure Add_Item
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : Gtk_Tree_Iter;
                Directory : Item_Path;
                Item      : Directory_Item;
                Update    : Boolean;
                Result    : out Gtk_Tree_Iter;
                Size      : out GInt
             )  is
      Position : GInt;
   begin
      Find (Store, Row, Directory, Item, Position, Size);
      if Position < 0 then
         Insert (Store.Tree, Result, Row, -Position - 1);
         Set_Item (Store, Result, Item);
         declare
            Path : constant Gtk_Tree_Path :=
                   Get_Path (Store.Tree, Result);
         begin
            Emit (Store, Inserted_ID, Result, Path);
            if Item.Directory then
               Row_Inserted (To_Interface (Store), Path, Result);
            end if;
            Path_Free (Path);
         end;
      else
         Result := Store.Tree.Nth_Child (Row, Position - 1);
         if Update then
            Set_Item (Store, Result, Item);
            if Item.Directory then
               declare
                  Path : constant Gtk_Tree_Path :=
                         Store.Get_Path (Result);
               begin
                  Row_Changed (To_Interface (Store), Path, Result);
                  Path_Free (Path);
               end;
            end if;
         end if;
      end if;
   end Add_Item;

   procedure Adjust (Object : in out Item_Path_Reference) is
   begin
      if Object.Ptr /= null then
         Object.Ptr.Count := Object.Ptr.Count + 1;
      end if;
   end Adjust;

   procedure Cache
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Item  : Item_Path
             )  is
      Best_Match, Exact_Match : Gtk_Tree_Iter;
   begin
      Cache (Store, Item, Best_Match, Exact_Match);
   end Cache;

   procedure Cache
             (  Store       : not null access
                              Gtk_Abstract_Directory_Record'Class;
                Item        : Item_Path;
                Best_Match  : out Gtk_Tree_Iter;
                Exact_Match : out Gtk_Tree_Iter
             )  is
      Updated : Boolean;
      Path    : Path_Node_Ptr;
      Current : Path_Node_Ptr;
      Length  : Positive;
      High    : GDouble;
      Step    : GDouble;
      Recurse : Natural := 0;
   begin
      Enter (Store, Item, High);
      Best_Match  := Null_Iter;
      Exact_Match := Null_Iter;
      if Item'Length = 0 then
         return;
      end if;
      Split (Store, Item, Path, Length);
      Step := (Store.High - Store.Low) / GDouble (Length);
      Current := Path;

      Store.High := Store.Low + Step;
      Add_Folder (Store, Exact_Match, "", False, Updated);
      Exact_Match :=
         Find
         (  Store,
            Best_Match,
            "",
            Get_Name (Store, Current.Directory)
         );
      while (  Exact_Match /= Null_Iter
            and then
               Get_Int (Store.Tree, Exact_Match, 2) in Cached_Directory
            )  -- A directory was found, it is cached
      loop
         Store.High := Store.Low + Step;
         Add_Folder
         (  Store,
            Exact_Match,
            Current.Directory,
            False,
            Updated
         );
         exit when Exact_Match = Null_Iter;
         Best_Match := Exact_Match;
         exit when Current.Next = null;
         Exact_Match :=
            Find
            (  Store,
               Best_Match,
               Current.Directory,
               Get_Name (Store, Current.Next.Directory)
            );
         Current := Current.Next;
      end loop;
      Release (Path);
      Leave (Store, Item, High);
   exception
      when others =>
         Release (Path);
         Leave (Store, Item, High);
   end Cache;

   procedure Changed
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Directory : Item_Path
             )  is
      Row : Gtk_Tree_Iter := Find (Store, Directory);
   begin
      if Row /= Null_Iter then
         Refresh (Store, Row);
      end if;
   end Changed;

   procedure Changed
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class
             )  is
      Row : Gtk_Tree_Iter := Null_Iter;
   begin
      Refresh (Store, Row);
   end Changed;

   procedure Change_Selection
             (  Widget : access Gtk_Directory_Items_View_Record;
                Index  : Positive;
                State  : Boolean
             )  is
      Row    : Gtk_Tree_Iter := Widget.Get_Iter (Index);
      Column : Positive      := Widget.Get_Column (Index);
      Path   : Gtk_Tree_Path;
      Area   : Gdk_Rectangle;
   begin
      if Row = Null_Iter then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Non-existing item selected"
            &  Where ("Set_Selection")
         )  );
      else
         Widget.Markup.Set_Extension (Row, 1, State);
         if State then
            Widget.Markup.Selected := Widget.Markup.Selected + 1;
         else
            Widget.Markup.Selected := Widget.Markup.Selected - 1;
         end if;
         --
         -- Forcing rendering the selected column
         --
         Widget.Columns.To_Columned (Row, Column);
         Path := Get_Path (Widget.Columns, Row);
         Widget.Get_Cell_Area
         (  Path,
            Widget.Get_Column (GInt (Column)),
            Area
         );
         Path_Free (Path);
         Gdk.Window.Invalidate_Rect
         (  Widget.Get_Bin_Window,
            Area,
            True
         );
      end if;
   end Change_Selection;

   function Check_Iter
            (  Model : Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      if Iter = Null_Iter or else Model.Root = Null_Gtk_Tree_Path then
         return False;
      end if;
      declare
         Parent : constant Gtk_Tree_Path :=
                  Model.Cache.Tree.Get_Path (Iter);
      begin
         if Parent = Null_Gtk_Tree_Path then
            return False;
         end if;
         if (  not Up (Parent)
            or else
               0 /= Compare (Parent, Model.Root)
            )
         then
            Path_Free (Parent);
            return False;
         else
            Path_Free (Parent);
            return True;
         end if;
      end;
   end Check_Iter;

   function Children
            (  Model  : not null access Gtk_Abstract_Directory_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
      Row : Gtk_Tree_Iter;
   begin
      if Parent = Null_Iter then
         Row := Model.Tree.Get_Iter_First;
      else
         Row := Model.Tree.Children (Parent);
      end if;
      while Row /= Null_Iter loop
         exit when Get_Int (Model.Tree, Row, 2) in Cached_Directory;
         Model.Tree.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Children (Gtk_Abstract_Directory)")
         )  );
         return Null_Iter;
   end Children;

   function Children
            (  Model  : not null access
                        Gtk_Directory_Items_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
      Row : Gtk_Tree_Iter;
   begin
      if Parent /= Null_Iter then
         return Null_Iter;
      elsif Model.Root = Null_Gtk_Tree_Path then
         return Null_Iter;
      end if;
      Row := Model.Cache.Tree.Get_Iter (Model.Root);
      if Row = Null_Iter then
         return Null_Iter;
      end if;
      Row := Children (Model.Cache.Tree, Row);
      while Row /= Null_Iter loop
         case Get_Int (Model.Cache.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               return Row;
            when Cached_Item =>      -- Items are to be filtered
               if Model.View.Filter
                  (  False,
                     Get_Name (Model.Cache.Tree, Row),
                     Get_Type (Model.Cache.Tree, Row)
                  )
               then
                  return Row;
               end if;
            when others =>
               null;
         end case;
         Model.Cache.Tree.Next (Row);
      end loop;
      return Null_Iter;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Children (Gtk_Directory_Items_Store)")
         )  );
         return Null_Iter;
   end Children;

   procedure Created
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Directory : Item_Path;
                Item      : Directory_Item
             )  is
      Row  : Gtk_Tree_Iter := Find (Store, Directory);
      Size : GInt;
   begin
      if Row = Null_Iter then
         Add_Item (Store, Null_Iter, "", Item, False, Row, Size);
      else
         Add_Item (Store, Row, Directory, Item, False, Row, Size);
      end if;
   end Created;

   procedure Deleted
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Item  : Item_Path
             )  is
      Row  : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;
   begin
      Row  := Find (Store, Item);
      Path := Get_Path (Store.Tree, Row);
      Emit (Store, Deleting_ID, Row, Path);
      Store.Tree.Remove (Row);
      if Path /= Null_Gtk_Tree_Path then
         Row_Deleted (To_Interface (Store), Path);
         Path_Free (Path);
      end if;
      Emit (Store, Deleted_ID, String (Item));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Deleted (Gtk_Abstract_Directory)")
         )  );
   end Deleted;

   procedure Deleted
             (  Model : not null access Gtk_Selection_Store_Record;
                Path  : Gtk_Tree_Path
             )  is
   begin
      Deleted (Gtk_Extension_Store_Record (Model.all)'Access, Path);
      if Model.Changed then
         Model.Changed := False;
         if (  Model.Mode = Selection_Browse
            and then
               Model.Selected = 0
            and then
               Model.View.Get_Directory_Size > 0
            )
         then
            -- Select another item
            declare
               Current : constant Natural := Model.View.Get_Current;
            begin
               if Current > 0 then
                  Model.View.Change_Selection (Current, True);
               else
                  Model.View.Change_Selection (1, True);
               end if;
            end;
         end if;
         Model.View.Selection_Changed;
      end if;
   end Deleted;

   procedure Deleting
             (  Model : not null access Gtk_Selection_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      if Get_Boolean (Model, Iter, 3) then
         Model.Selected := Model.Selected - 1;
         Model.Changed  := True;
      end if;
   end Deleting;

   procedure Destroy
             (  Object  : access GObject_Record'Class;
                Browser : Gtk_Directory_Items_View
             )  is
   begin
      Finalize (Browser);
   end Destroy;

   procedure Directory_Activated
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             )  is
      Row : constant Gtk_Tree_Iter :=
               Get_Iter
               (  Browser.Cache,
                  Convert (Get_Address (Nth (Params, 1)))
               );
   begin
      if Row /= Null_Iter then
         Select_Iter (Get_Selection (Browser), Row);
      end if;
   end Directory_Activated;

   procedure Directory_Changed
             (  Widget : not null access Gtk_Directory_Items_View_Record
             )  is
   begin
      Directory_Tree_Handlers.Emit_By_Name
      (  Widget,
         "directory-changed"
      );
   end Directory_Changed;

   procedure Directory_Changed
             (  Object  : access GObject_Record'Class;
                Browser : Gtk_Directory_Tree_View
             )  is
   begin
      Browser.Last := Browser.This;
      Set (Browser.This, Browser);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Directory_Changed")
         )  );
   end Directory_Changed;

   procedure Directory_Collapsed
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Items_View
             )  is
      Row  : Gtk_Tree_Iter;
      Item : Gtk_Tree_Iter;
   begin
      Get_Tree_Iter (Nth (Params, 1), Row);
      if Row = Null_Iter then
         return;
      end if;
      Item := To_Item (Browser.Content.Cache, Row);
      if Row = Null_Iter then
         return;
      end if;
      Item := To_Marked (Browser, Item);
      if (  Item = Null_Iter
         or else
            not Is_In
                (  To_Interface (Browser.Markup),
                   Gtk_Tree_Iter'(Get_Root (Browser.Columns)),
                   Item
         )      )
      then
         return;
      end if;
      --
      -- A   directory   containing  the  currently  selected  one  gets
      -- collapsed. We set the  selection  to  the  collapsed  directory
      -- which in turn chages the current directory.
      --
      Select_Iter (Get_Selection (Browser.Directories), Row);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Directory_Collapsed")
         )  );
   end Directory_Collapsed;

   procedure Directory_Expanded
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             )  is
      Wait    : Wait_Cursor (+Browser);
      Updated : Boolean;
      Row     : Gtk_Tree_Iter;
   begin
      Row := Get_Tree_Iter (Nth (Params, 1));
      if Row /= Null_Iter then
         Row := To_Item (Browser.Cache, Row);
         if 0 /= (Trace_Expand_Directory and Browser.Cache.Tracing) then
            Trace
            (  Browser.Cache,
               Browser.Cache.Depth,
               (  "Expanding "
               &  String (Get_Name (Browser.Cache.Tree, Row))
               &  " caching..."
            )  );
         end if;
         Add_Folder
         (  Browser.Cache,
            Row,
            Get_Path (Browser.Cache, Row),
            True,
            Updated
         );
         if 0 /= (Trace_Expand_Directory and Browser.Cache.Tracing) then
            Trace
            (  Browser.Cache,
               Browser.Cache.Depth,
               (  "Expanding "
               &  String (Get_Name (Browser.Cache.Tree, Row))
               &  " cached"
            )  );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Directory_Expanded")
         )  );
   end Directory_Expanded;

   procedure Directory_Refreshed
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             )  is
      Wait : Wait_Cursor (+Browser);
      Path : constant String := Get_String (Nth (Params, 1));
   begin
      Set_Current_Directory (Browser, Item_Path (Path));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Directory_Refreshed")
         )  );
   end Directory_Refreshed;

   procedure Edited_Directory
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             )  is
      Row : Gtk_Tree_Iter :=
               Get_Iter_From_String
               (  To_Interface (Browser.Cache),
                  Get_String (Nth (Params, 1))
               );
   begin
      if Row = Null_Iter then
         return;
      end if;
      Row := To_Item (Browser.Cache, Row);
      if Row = Null_Iter then
         return;
      end if;
      Name_Commit
      (  Browser,
         Get_Path (Browser.Cache, Row),
         Item_Name (Get_String (Nth (Params, 2)))
      );
   end Edited_Directory;

   procedure Edited_Item
             (  Object  : access GObject_Record'Class;
                Params : GValues;
                Column : Column_Data
             )  is
      Row : Gtk_Tree_Iter :=
               Get_Iter_From_String
               (  To_Interface (Column.Browser.Columns),
                  Get_String (Nth (Params, 1))
               );
   begin
      if Row = Null_Iter then
         return;
      end if;
      Row :=
         From_Columned
         (  Column.Browser.Columns,
            Row,
            Column.Column
         );
      if Row = Null_Iter then
         return;
      end if;
      Name_Commit
      (  Column.Browser,
         Positive
         (  Get_Row_No (To_Interface (Column.Browser.Markup), Row)
         +  1
         ),
         Item_Name (Get_String (Nth (Params, 2)))
      );
   end Edited_Item;

   procedure EmitV
             (  Params : System.Address;
                Signal : Signal_ID;
                Quark  : GQuark;
                Result : System.Address
             );
   pragma Import (C, EmitV, "g_signal_emitv");

   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Text   : UTF8_String;
                Path   : Item_Path
             )  is
      Params : GValue_Array (0..2);
      Result : GValue;
   begin
      Init (Params (0), Get_Cache_Model_Type);
      Set_Object (Params (0), Store);
      Init (Params (1), GType_String);
      Set_String (Params (1), Text);
      Init (Params (2), GType_String);
      Set_String (Params (2), String (Path));
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Unset (Params (0));
      Unset (Params (1));
      Unset (Params (2));
   end Emit;

   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Path   : Item_Path;
                Value  : GDouble
             )  is
      Params : GValue_Array (0..2);
      Result : GValue;
   begin
      Init (Params (0), Get_Cache_Model_Type);
      Set_Object (Params (0), Store);
      Init (Params (1), GType_String);
      Set_String (Params (1), String (Path));
      Init (Params (2), GType_Double);
      Set_Double (Params (2), Value);
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Unset (Params (0));
      Unset (Params (1));
      Unset (Params (2));
   end Emit;

   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Path   : Item_Path
             )  is
      Params : GValue_Array (0..1);
      Result : GValue;
   begin
      Init (Params (0), Get_Cache_Model_Type);
      Set_Object (Params (0), Store);
      Init (Params (1), GType_String);
      Set_String (Params (1), String (Path));
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Unset (Params (0));
      Unset (Params (1));
   end Emit;

   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Text   : UTF8_String
             )  is
      Params : GValue_Array (0..1);
      Result : GValue;
   begin
      Init (Params (0), Get_Cache_Model_Type);
      Set_Object (Params (0), Store);
      Init (Params (1), GType_String);
      Set_String (Params (1), Text);
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Unset (Params (0));
      Unset (Params (1));
   end Emit;

   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Row    : Gtk_Tree_Iter;
                Path   : Gtk_Tree_Path
             )  is
      Params : GValue_Array (0..2);
      Iter   : aliased Gtk_Tree_Iter := Row;
      Result : GValue;
   begin
      Init (Params (0), Get_Cache_Model_Type);
      Set_Object (Params (0), Store);
      Init (Params (1), GType_Pointer);
      Set_Address (Params (1), Iter'Address);
      Init (Params (2), Path_Get_Type);
      Set_Address (Params (2), Get_Object (Path));
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Unset (Params (0));
      Unset (Params (1));
--    Unset (Params (2));
   end Emit;

   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Row    : Gtk_Tree_Iter;
                Text   : String
             )  is
      Params : GValue_Array (0..2);
      Iter   : aliased Gtk_Tree_Iter := Row;
      Result : GValue;
   begin
      Init (Params (0), Get_Cache_Model_Type);
      Set_Object (Params (0), Store);
      Init (Params (1), GType_Pointer);
      Set_Address (Params (1), Iter'Address);
      Init (Params (2), GType_String);
      Set_String (Params (2), Text);
      EmitV (Params (0)'Address, Signal, 0, Result'Address);
      Unset (Params (0));
      Unset (Params (1));
      Unset (Params (2));
   end Emit;

   procedure Expand_Folder
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : Gtk_Tree_Iter;
                Directory : Item_Path
             )  is
      Updated : Boolean;
      Count   : GInt;
      Item    : Gtk_Tree_Iter := Store.Tree.Nth_Child (Row, 0);
      High    : GDouble;
      Step    : GDouble;
   begin
      Enter (Store, Directory, High);
      Count := N_Children (Store.Tree, Row);
      if Count > 0 then
         Step := (Store.High - Store.Low) / GDouble (Count);
      else
         Step := 0.0;
      end if;
      Count := 0;
      while Item /= Null_Iter loop
         if Get_Int (Store.Tree, Item, 2) in Cached_Children then
            Store.High := Store.Low + Step;
            Add_Folder
            (  Store,
               Item,
               Store.Get_Path (Directory, Get_Name (Store.Tree, Item)),
               False,
               Updated
            );
            if Item /= Null_Iter then
               Count := Count + 1;
            end if;
         else
            Count := Count + 1;
         end if;
         Item := Store.Tree.Nth_Child (Row, Count);
         declare
            Now : constant Time := Clock;
         begin
            if Now - Store.Last_Time > 0.2 then
               Store.Last_Time := Now;
               Store.Progress (Directory, Store.Low);
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
            (  Widget    : not null access
                           Gtk_Directory_Items_View_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean is
   begin
      return True;
   end Filter;

   procedure Finalize
             (  Model : not null access Gtk_Abstract_Directory_Record
             )  is
   begin
      Gtk_Abstract_Model_Record (Model.all).Finalize;
      if Model.Tree /= null then
         Unref (Model.Tree);
         Model.Tree := null;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Gtk_Abstract_Directory_Record)")
         )  );
   end Finalize;

   procedure Finalize
             (  Model : not null access Gtk_Directory_Items_Store_Record
             )  is
   begin
      Gtk_Abstract_Model_Record (Model.all).Finalize;
      if Model.Cache /= null then
         Unref (Model.Cache);
         Model.Cache := null;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Gtk_Directory_Items_Store)")
         )  );
   end Finalize;

   procedure Finalize
             (  Widget : not null access Gtk_Directory_Items_View_Record
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Gtk_Cell_Renderer_Text_Array,
                Gtk_Cell_Renderer_Text_Array_Ptr
             );
   begin
      if Widget.Columns /= null then
         Unref (Widget.Columns);
         Widget.Columns := null;
      end if;
      if Widget.Markup /= null then
         Unref (Widget.Markup);
         Widget.Markup := null;
      end if;
      if Widget.Directories /= null then
         Unref (Widget.Directories);
      end if;
      Free (Widget.Name_Renderers);
      if Widget.Content /= null then
         Unref (Widget.Content);
         Widget.Content := null;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Gtk_Directory_Items_View)")
         )  );
   end Finalize;

   procedure Finalize (Object : in out Item_Path_Reference) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Item_Path_Object,
                Item_Path_Object_Ptr
             );
   begin
      if Object.Ptr /= null then
         Object.Ptr.Count := Object.Ptr.Count - 1;
         if Object.Ptr.Count = 0 then
            Free (Object.Ptr);
         else
            Object.Ptr := null;
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Item_Path_Reference)")
         )  );
   end Finalize;

   function Find
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class;
               Path  : Item_Path
            )  return Gtk_Tree_Iter is
   begin
      if Path'Length = 0 then
         return Null_Iter;
      end if;
      declare
         Directory : Item_Path renames Store.Get_Directory (Path);
         Row       : Gtk_Tree_Iter := Find (Store, Directory);
      begin
         if Row /= Null_Iter then
            Row := Find (Store, Row, Directory, Get_Name (Store, Path));
         end if;
         return Row;
      end;
   exception
      when Name_Error =>
         return Find (Store, Null_Iter, "", Get_Name (Store, Path));
   end Find;

   function Find
            (  Store     : not null access
                           Gtk_Abstract_Directory_Record'Class;
               Row       : Gtk_Tree_Iter;
               Directory : Item_Path;
               Name      : Item_Name
            )  return Gtk_Tree_Iter is
      Iter : Gtk_Tree_Iter;
      Aim  : constant Directory_Item :=
                      (  Directory   => False,
                         Policy      => Cache_Ahead,
                         Name_Length => Name'Length,
                         Name        => Name,
                         Kind_Length => 0,
                         Kind        => ""
                      );
   begin
      if Row = Null_Iter then
         Iter := Get_Iter_First (Store.Tree);
      else
         Iter := Store.Tree.Children (Row);
      end if;
      while Iter /= Null_Iter loop
         declare
            Name : constant Item_Name := Get_Name (Store.Tree, Iter);
         begin
            exit when
                 (  Equal
                 =  Compare
                    (  Store,
                       Directory,
                       Aim,
                       (  Directory   => False,
                          Policy      => Cache_Ahead,
                          Name_Length => Name'Length,
                          Name        => Name,
                          Kind_Length => 0,
                          Kind        => ""
                       ),
                       True
                 )  );
         end;
         Next (Store.Tree, Iter);
      end loop;
      return Iter;
   end Find;

   procedure Find
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : Gtk_Tree_Iter;
                Directory : Item_Path;
                Item      : Directory_Item;
                Position  : out GInt;
                Size      : out GInt
             )  is
      Upper : GInt := Store.Tree.N_Children (Row);
      Lower : GInt := -1;
      This  : GInt;
      That  : Gtk_Tree_Iter;
   begin
      Size := Upper;
      if Upper = 0 then
         Position := -1;
         return;
      end if;
      loop
         This := (Upper + Lower) / 2;
         That := Store.Tree.Nth_Child (Row, This);
         declare
            Cached : constant Directory_Item := Store.Get_Item (That);
         begin
            case Compare (Store, Directory, Item, Cached, False) is
               when Before =>
                  Upper := This;
                  if Upper - Lower <= 1 then
                     Position := -(This + 1);
                     return;
                  end if;
               when Equal =>
                  Position := This + 1;
                  return;
               when After =>
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
            (  Store     : not null access
                           Gtk_Abstract_Directory_Record'Class;
               Row       : Gtk_Tree_Iter;
               Directory : Item_Path;
               Item      : Directory_Item
            )  return Gtk_Tree_Iter is
      Position : GInt;
      Size     : GInt;
   begin
      Find (Store, Row, Directory, Item, Position, Size);
      if Position < 0 then
         return Null_Iter;
      else
         return Store.Tree.Nth_Child (Row, Position - 1);
      end if;
   end Find;

   procedure From_Filtered
             (  Model      : not null access
                             Gtk_Directory_Items_Store_Record;
                Unfiltered : out Gtk_Tree_Iter;
                Filtered   : Gtk_Tree_Iter
             )  is
   begin
      Unfiltered := Filtered;
   end From_Filtered;

   function From_Item
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class;
               Item  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Item;
   end From_Item;

   function From_Marked
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Item   : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
      Row : Gtk_Tree_Iter;
   begin
      Widget.Content.From_Filtered
      (  Row,
         Widget.Markup.From_Extension (Item)
      );
      return Row;
   end From_Marked;

   function Get (Object : Item_Path_Reference) return Item_Path is
   begin
      if Object.Ptr = null then
         return "";
      else
         return Object.Ptr.Path;
      end if;
   end Get;

   function Get_Cache
            (  Widget : not null access Gtk_Directory_Tree_View_Record
            )  return Gtk_Abstract_Directory is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Cache
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Abstract_Directory is
   begin
      return Widget.Content.Cache;
   end Get_Cache;

   function Get_Cached
            (  Store : not null access Gtk_Abstract_Directory_Record;
               Path  : Item_Path
            )  return Directory_Item is
      Row : constant Gtk_Tree_Iter := Find (Store, Path);
   begin
      if Row = Null_Iter then
         raise Constraint_Error;
      else
         return Get_Item (Store, Row);
      end if;
   end Get_Cached;

   function Get_Cache_Model_Type return Gtk_Type is
      use Interfaces.C.Strings;

      function Lookup (Index : Interfaces.C.size_t) return Signal_ID is
      begin
         return Lookup
                (  Cache_Model_Type,
                   GLib.Signal_Name
                   (  String'
                      (  Value
                         (  Abstract_Directory_Signal_Names (Index)
                )  )  )  );
      end Lookup;
   begin
      if Cache_Model_Type = GType_Invalid then
         Cache_Model_Type :=
            Register
            (  Abstract_Directory_Class_Name,
               Abstract_Directory_Signal_Names,
               Abstract_Directory_Signal_Parameters
            );
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
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Cache_Model_Type")
         )  );
         raise;
   end Get_Cache_Model_Type;

   function Get_Column
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Positive is
      Item   : Gtk_Tree_Iter := Widget.Get_Iter (Index);
      Column : Positive;
   begin
      Widget.Columns.To_Columned (Item, Column);
      return Column;
   end Get_Column;

   function Get_Columns
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Positive is
   begin
      return Widget.Columns.Get_Major_Columns;
   end Get_Columns;

   function Get_Column_Type
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Index : GInt
            )  return GType is
   begin
      return Get_Column_Type (Model.Tree, Index);
   end Get_Column_Type;

   function Get_Column_Type
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Index : GInt
            )  return GType is
   begin
      return Model.Cache.Tree.Get_Column_Type (Index);
   end Get_Column_Type;

   function Get_Current
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural is
      Columned_Path : Gtk_Tree_Path;
      Markup_Path   : Gtk_Tree_Path;
      Column        : Gtk_Tree_View_Column;
      Column_No     : GInt;
   begin
      Widget.Get_Cursor (Columned_Path, Column);
      if Columned_Path = Null_Gtk_Tree_Path then
         return 0;
      end if;
      Column_No := Get_Column_No (Widget, Column);
      if Column_No < 0 then
         Path_Free (Columned_Path);
         return 0;
      end if;
      Markup_Path :=
         Widget.Columns.From_Columned
         (  Columned_Path,
            Positive (Column_No + 1)
         );
      if Markup_Path = Null_Gtk_Tree_Path then
         Path_Free (Columned_Path);
         return 0;
      end if;
      declare
         Indices : GInt_Array renames Get_Indices (Markup_Path);
         Result  : Natural := 0;
      begin
         if Indices'Length > 0 then
            Result := Natural (Indices (Indices'Last) + 1);
         end if;
         Path_Free (Markup_Path);
         Path_Free (Columned_Path);
         return Result;
      end;
   end Get_Current;

   function Get_Current_Directory
            (  Widget : not null access Gtk_Directory_Tree_View_Record
            )  return Item_Path is
      Row   : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Widget.Get_Selection.Get_Selected (Model, Row);
      if Row = Null_Iter then
         raise Name_Error;
      end if;
      return Widget.Cache.Get_Path (To_Item (Widget.Cache, Row));
   end Get_Current_Directory;

   function Get_Depth
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class
            )  return Natural is
   begin
      return Store.Depth;
   end Get_Depth;

   function Get_Directory
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Item_Path is
      Current : constant Gtk_Tree_Iter := Widget.Get_Directory;
   begin
      return Get_Path (Widget.Content.Cache, Current);
   end Get_Directory;

   function Get_Directory
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Tree_Iter is
   begin
      if Widget.Content.Root = Null_Gtk_Tree_Path then
         return Null_Iter;
      else
         return
            Widget.Content.Cache.Tree.Get_Iter (Widget.Content.Root);
      end if;
   end Get_Directory;

   function Get_Directory_Items_View_Type return Gtk_Type is
   begin
      Initialize_Class_Record
      (  Ancestor     => Gtk.Tree_View.Get_Type,
         Class_Record => Directory_Items_Class,
         Type_Name    => Directory_Items_Class_Name,
         Signals      => Directory_Items_Signal_Names
      );
      return Directory_Items_Class.The_Type;
   end Get_Directory_Items_View_Type;

   function Get_Directory_Size
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural is
   begin
      return
         Natural (Widget.Markup.N_Children (Widget.Columns.Get_Root));
   end Get_Directory_Size;

   function Get_Directory_Tree_View
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Directory_Tree_View is
   begin
      return Widget.Directories;
   end Get_Directory_Tree_View;

   function Get_Directory_Tree_View_Type return Gtk_Type is
   begin
      return Gtk.Tree_View.Get_Type;
   end Get_Directory_Tree_View_Type;

   function Get_Flags
            (  Model : not null access Gtk_Abstract_Directory_Record
            )  return Tree_Model_Flags is
   begin
      return Model.Tree.Get_Flags and not Tree_Model_Iters_Persist;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Flags")
         )  );
         return Tree_Model_List_Only;
   end Get_Flags;

   function Get_Flags
            (  Model : not null access Gtk_Directory_Items_Store_Record
            )  return Tree_Model_Flags is
   begin
      return Tree_Model_List_Only;
   end Get_Flags;

   function Get_Icon
            (  Widget       : not null access
                              Gtk_Directory_Tree_View_Record;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Icon_Data is
      This : constant String := String (Kind);
   begin
      if not Has_Children then
         return (Stock_ID, Stock_Stop'Length, Stock_Stop);
      elsif Expanded and then This = Stock_Directory then
         return (Stock_ID, Stock_Open'Length, Stock_Open);
      else
         return (Stock_ID, This'Length, This);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Icon (Gtk_Directory_Tree_View)")
         )  );
         return
         (  Stock_ID,
            Stock_Dialog_Error'Length,
            Stock_Dialog_Error
         );
   end Get_Icon;

   function Get_Icon
            (  Widget       : not null access
                              Gtk_Directory_Items_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Directory    : Boolean;
               Has_Children : Boolean
            )  return Icon_Data is
   begin
      if not Directory or else Has_Children then
         return (Stock_ID, Kind'Length, String (Kind));
      else
         return (Stock_ID, Stock_Cancel'Length, Stock_Cancel);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Icon (Gtk_Directory_Items_View)")
         )  );
         return (Stock_ID, Stock_Cancel'Length, Stock_Cancel);
   end Get_Icon;

   function Get_Index
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Row    : Positive;
               Column : Positive
            )  return Natural is
   begin
      return
         Natural
         (  Get_Row_No
            (  To_Interface (Widget.Markup),
               Widget.Columns.Get_Reference_Iter (Row, Column)
            )
         +  1
         );
   end Get_Index;

   function Get_Index
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Name   : Item_Name
            )  return Natural is
      Row       : Gtk_Tree_Iter := Widget.Get_Directory;
      Directory : Item_Path renames Widget.Get_Directory;
      Index     : Natural := 0;
      Aim       : constant Directory_Item :=
                  (  Directory   => False,
                     Policy      => Cache_Ahead,
                     Name_Length => Name'Length,
                     Name        => Name,
                     Kind_Length => 0,
                     Kind        => ""
                  );
   begin
      Row := Widget.To_Marked (Row);
      if Row = Null_Iter then
         return 0;
      end if;
      Row := Widget.Markup.Children (Row);
      while Row /= Null_Iter loop
         Index := Index + 1;
         declare
            Name : constant Item_Name :=
                   Get_Name (To_Interface (Widget.Markup), Row);
         begin
            if (  Equal
               =  Compare
                  (  Widget.Content.Cache,
                     Directory,
                     Aim,
                     (  Directory   => False,
                        Policy      => Cache_Ahead,
                        Name_Length => Name'Length,
                        Name        => Name,
                        Kind_Length => 0,
                        Kind        => ""
                     ),
                     True
               )  )
            then
               return Index;
            end if;
         end;
         Next (Widget.Markup, Row);
      end loop;
      return 0;
   end Get_Index;

   function Get_Item
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class;
               Item  : Gtk_Tree_Iter
            )  return Directory_Item is
      Kind : constant Item_Type := Get_Type (Store.Tree, Item);
      Name : constant Item_Name := Get_Name (Store.Tree, Item);
   begin
      return
      (  Policy      => Cache_Expanded,
         Name_Length => Name'Length,
         Name        => Name,
         Kind_Length => Kind'Length,
         Kind        => Kind,
         Directory   =>
            Get_Int (Store.Tree, Item, 2) in Cached_Directory
      );
   end Get_Item;

   function Get_Items_Model_Type return Gtk_Type is
      use Interfaces.C.Strings;
   begin
      if Items_Model_Type = GType_Invalid then
         Items_Model_Type := Register ("GtkDirectoryItemsStore");
      end if;
      return Items_Model_Type; -- Registering the GTK+ type
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Items_Model_Type")
         )  );
         raise;
   end Get_Items_Model_Type;

   function Get_Iter
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
   begin
      if Path = Null_Gtk_Tree_Path then
         return Null_Iter;
      end if;
      declare
         Indices : constant GInt_Array := Get_Indices (Path);
         Row     : Gtk_Tree_Iter := Null_Iter;
      begin
         for Level in Indices'Range loop
            Row := Children (Model, Row);
            exit when Row = Null_Iter;
            for Child in 1..Indices (Level) loop
               Next (Model, Row);
               exit when Row = Null_Iter;
            end loop;
         end loop;
         if 0 /= (Model.Tracing and Trace_Cache) then
            declare
               Other : constant Gtk_Tree_Path := Get_Path (Model, Row);
            begin
               if Other /= Path then
                  Log
                  (  GtkAda_Contributions_Domain,
                     Log_Level_Critical,
                     (  "Inconsistent iterator for path "
                     &  To_String (Path)
                     &  " at "
                     &  String (Get_Name (Model.Tree, Row))
                  )  );
               end if;
               Path_Free (Other);
            end;
         end if;
         return Row;
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Iter (Gtk_Abstract_Directory)")
         )  );
         return Null_Iter;
   end Get_Iter;

   function Get_Iter
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
      Indices : constant GInt_Array := Get_Indices (Path);
   begin
      if Indices'Length = 1 then -- The index is the child number
         return Model.Nth_Child (Null_Iter, Indices (Indices'First));
      else
         return Null_Iter;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Iter (Gtk_Directory_Items_Store)")
         )  );
         return Null_Iter;
   end Get_Iter;

   function Get_Iter
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Gtk_Tree_Iter is
      Row : constant Gtk_Tree_Iter :=
               Widget.Markup.Nth_Child
               (  Widget.Columns.Get_Root,
                  GInt (Index) - 1
               );
   begin
      if Row = Null_Iter then
         raise Constraint_Error;
      else
         return Row;
      end if;
   end Get_Iter;

   function Get_N_Columns
            (  Model : not null access Gtk_Abstract_Directory_Record
            )  return GInt is
   begin
      return Get_N_Columns (Model.Tree);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_N_Columns (Gtk_Abstract_Directory)")
         )  );
         return 0;
   end Get_N_Columns;

   function Get_N_Columns
            (  Model : not null access Gtk_Directory_Items_Store_Record
            )  return GInt is
   begin
      return Get_N_Columns (Model.Cache.Tree);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_N_Columns (Gtk_Directory_Items_Store)")
         )  );
         return 0;
   end Get_N_Columns;

   function Get_Name
            (  Widget       : not null access
                              Gtk_Directory_Tree_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Item_Name is
   begin
      return Name;
   end Get_Name;

   function Get_Name
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Item_Name is
   begin
      return Get_Name
             (  To_Interface (Widget.Markup),
                Widget.Get_Iter (Index)
             );
   end Get_Name;

   function Get_Path
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
   begin
      if Iter = Null_Iter then
         return Null_Gtk_Tree_Path;
      end if;
      declare
         Path    : Gtk_Tree_Path := Get_Path (Model.Tree, Iter);
         Indices : constant GInt_Array := Get_Indices (Path);
         Row     : Gtk_Tree_Iter;
         Count   : GInt;
      begin
         Path_Free (Path);
         Gtk_New (Path);
         for Level in Indices'Range loop
            if Level = Indices'First then
               Row := Model.Tree.Get_Iter_First;
            else
               Row := Model.Tree.Children (Row);
            end if;
            Count := 0;
            if Row = Null_Iter then
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Critical,
                  (  "Invalid iterator: no children at the level"
                  &  Natural'Image (Level)
                  &  Where ("Get_Path (Gtk_Abstract_Directory)")
               )  );
               Path_Free (Path);
               return Null_Gtk_Tree_Path;
            end if;
            for Child in 1..Indices (Level) loop
               if Get_Int (Model.Tree, Row, 2) in Cached_Directory then
                  Count := Count + 1;
               end if;
               Model.Tree.Next (Row);
               if Row = Null_Iter then
                  Log
                  (  GtkAda_Contributions_Domain,
                     Log_Level_Critical,
                     (  "Invalid iterator: less than"
                     &  GInt'Image (Indices (Level))
                     &  " children at the level"
                     &  Natural'Image (Level)
                     &  Where ("Get_Path (Gtk_Abstract_Directory)")
                  )  );
                  Path_Free (Path);
                  return Null_Gtk_Tree_Path;
               end if;
            end loop;
            if Get_Int (Model.Tree, Row, 2) not in Cached_Directory then
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Critical,
                  (  "Invalid iterator: the child"
                  &  GInt'Image (Indices (Level))
                  &  " at the level"
                  &  Natural'Image (Level)
                  &  " is not a directory"
                  &  Where ("Get_Path (Gtk_Abstract_Directory)")
               )  );
               Path_Free (Path);
               return Null_Gtk_Tree_Path;
            end if;
            Append_Index (Path, Count);
         end loop;
         if 0 /= (Model.Tracing and Trace_Cache) then
            if Get_Iter (Model, Path) /= Row then
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Critical,
                  (  "Inconsistent path "
                  &  To_String (Path)
                  &  " of "
                  &  String (Get_Name (Model.Tree, Row))
               )  );
            end if;
         end if;
         return Path;
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Path (Gtk_Abstract_Directory)")
         )  );
         return Null_Gtk_Tree_Path;
   end Get_Path;

   function Get_Path
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
   begin
      if Iter = Null_Iter or else Model.Root = Null_Gtk_Tree_Path then
         return Null_Gtk_Tree_Path;
      end if;
      declare
         Unfiltered : constant Gtk_Tree_Path :=
                      Model.Cache.Tree.Get_Path (Iter);
         Filtered   : constant Gtk_Tree_Path :=
                      Model.To_Filtered (Unfiltered);
      begin
         Path_Free (Unfiltered);
         return Filtered;
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Path (Gtk_Directory_Items_Store_Record)")
         )  );
         return Null_Gtk_Tree_Path;
   end Get_Path;

   function Get_Path
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Name   : Item_Name
            )  return Item_Path is
   begin
      return Widget.Content.Cache.Get_Path (Widget.Get_Directory, Name);
   end Get_Path;

   function Get_Path
            (  Store  : not null access
                        Gtk_Abstract_Directory_Record'Class;
               Item   : Gtk_Tree_Iter
            )  return Item_Path is
   begin
      if Item = Null_Iter then
         return "";
      end if;
      declare
         Folder : constant Gtk_Tree_Iter :=
                  Store.Tree.Parent (Item);
         Name   : constant Item_Name := Get_Name (Store.Tree, Item);
      begin
         if Folder = Null_Iter then
            return Store.Get_Path ("", Name);
         else
            return Store.Get_Path (Store.Get_Path (Folder), Name);
         end if;
      end;
   end Get_Path;

   function Get_Path
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Item_Path is
   begin
      return
         Widget.Content.Cache.Get_Path
         (  Widget.From_Marked (Widget.Get_Iter (Index))
         );
   end Get_Path;

   function Get_Path_Before
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
      Tree  : constant Gtk_Tree_Store := Model.Cache.Tree;
      Row   : Gtk_Tree_Iter;
      Count : GInt := 0;
   begin
      if Model.Root = Null_Gtk_Tree_Path then
         return Null_Gtk_Tree_Path;
      end if;
      Row := Get_Iter (Tree, Model.Root);
      if Row = Null_Iter then
         return Null_Gtk_Tree_Path;
      end if;
      Row := Children (Tree, Row);
      while Row /= Null_Iter loop
         if Row = Iter then
            declare
               Path : Gtk_Tree_Path;
            begin
               Gtk_New (Path);
               Append_Index (Path, Count);
               return Path;
            end;
         end if;
         case Get_Int (Model.Cache.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               Count := Count + 1;
            when Cached_Item =>      -- Items are to be filtered
               if Model.View.Filter
                  (  False,
                     Get_Name (Model.Cache.Tree, Row),
                     Get_Type (Model.Cache.Tree, Row)
                  )
               then
                  Count := Count + 1;
               end if;
            when others =>
               null;
         end case;
         Next (Tree, Row);
      end loop;
      return Null_Gtk_Tree_Path;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Path_Before")
         )  );
         return Null_Gtk_Tree_Path;
   end Get_Path_Before;

   procedure Get_Position
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : Positive;
                Row    : out Positive;
                Column : out Positive
             )  is
      No   : GInt;
      Iter : Gtk_Tree_Iter :=
                Widget.Markup.Nth_Child
                (  Widget.Columns.Get_Root,
                   GInt (Index) - 1
                );
   begin
      if Iter /= Null_Iter then
         Widget.Columns.To_Columned (Iter, Column);
         No := Get_Row_No (To_Interface (Widget.Columns), Iter);
         if No >= 0 then
            Row := Positive (No + 1);
            return;
         end if;
      end if;
      raise Constraint_Error;
   end Get_Position;

   function Get_Row
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Positive is
      Item   : Gtk_Tree_Iter := Widget.Get_Iter (Index);
      Column : Positive;
   begin
      Widget.Columns.To_Columned (Item, Column);
      return
         Positive
         (  Get_Row_No (To_Interface (Widget.Columns), Item)
         +  1
         );
   end Get_Row;

   function Get_Selection
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Selection is
      Result : Selection (1..Widget.Markup.Selected);
   begin
      if Result'Length > 0 then
         declare
            Root  : constant Gtk_Tree_Iter := Widget.Columns.Get_Root;
            Index : GInt := 0;
            Row   : Gtk_Tree_Iter;
         begin
            for Selected in Result'Range loop
               loop
                  Row := Widget.Markup.Nth_Child (Root, Index);
                  if Row = Null_Iter then
                     Log
                     (  GtkAda_Contributions_Domain,
                        Log_Level_Critical,
                        (  "Inconsitent selection in "
                        &  Where ("Get_Selection")
                     )  );
                     Widget.Markup.Selected := Selected - 1;
                     return Result (1..Widget.Markup.Selected);
                  end if;
                  Index := Index + 1;
                  exit when Get_Boolean (Widget.Markup, Row, 3);
               end loop;
               Result (Selected) := Positive (Index);
            end loop;
         end;
      end if;
      return Result;
   end Get_Selection;

   function Get_Selection_Mode
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Selection_Mode is
   begin
      return Widget.Markup.Mode;
   end Get_Selection_Mode;

   function Get_Selection_Size
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural is
   begin
      return Widget.Markup.Selected;
   end Get_Selection_Size;

   function Get_Tracing
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class
            )  return Traced_Actions is
   begin
      return Store.Tracing;
   end Get_Tracing;

   function Get_Tree_Store
            (  Store : not null access Gtk_Abstract_Directory_Record
            )  return Gtk_Tree_Store is
   begin
      return Store.Tree;
   end Get_Tree_Store;

   function Get_Type
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Item_Type is
   begin
      return Get_Type
             (  Widget.Content.Cache.Tree,
                Widget.From_Marked (Widget.Get_Iter (Index))
             );
   end Get_Type;

   procedure Get_Value
             (  Model  : not null access Gtk_Abstract_Directory_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             )  is
   begin
      Get_Value (Model.Tree, Iter, Column, Value);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Value (Gtk_Abstract_Directory)")
         )  );
   end Get_Value;

   procedure Get_Value
             (  Model  : not null access
                         Gtk_Directory_Items_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : GInt;
                Value  : out GValue
             )  is
   begin
      Get_Value (Model.Cache.Tree, Iter, Column, Value);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Value (Gtk_Directory_Items_Store)")
         )  );
   end Get_Value;

   function Get_Visible_Height
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural is
      Window : constant Gdk_Window := Widget.Get_Bin_Window;
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
            (  Widget,
               0.5,
               GDouble (Get_Height (Window)) - 0.5
            );
         if Bottom = 0 then
            Bottom :=
               Widget.Columns.Get_Column_Height
               (  Widget.Get_Column (Top)
               );
         end if;
         return Bottom - Top + 1;
      end;
   end Get_Visible_Height;

   function Get_Visible_Width
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural is
      Window : constant Gdk_Window := Widget.Get_Bin_Window;
   begin
      if Window = null then
         return 0;
      end if;
      declare
         Left  : Natural;
         Right : Natural;
      begin
         Left := Widget.Locate (0.5, 0.5);
         if Left = 0 then
            return 0;
         end if;
         Right :=
            Locate (Widget, GDouble (Get_Width (Window)) - 0.5, 0.5);
         if Right = 0 then
            Right :=
               Get_Row_Width
               (  Widget.Columns,
                  Get_Row (Widget, Left)
               );
         end if;
         return Right - Left + 1;
      end;
   end Get_Visible_Width;

   procedure Gtk_New
             (  Model : out Gtk_Directory_Items_Store;
                Cache : not null access
                        Gtk_Abstract_Directory_Record'Class;
                View  : not null access
                        Gtk_Directory_Items_View_Record'Class
             )  is
   begin
      Model := new Gtk_Directory_Items_Store_Record;
      Initialize (Model, Get_Items_Model_Type);
      Model.View := View.all'Unchecked_Access;
      Model.Cache := Cache.all'Unchecked_Access;
      Ref (Model.Cache);
      Directory_Items_Store_Handlers.Connect
      (  Cache,
         "item-deleted",
         Item_Deleted'Access,
         Model.all'Access
      );
      Directory_Items_Store_Handlers.Connect
      (  Cache,
         "item-deleting",
         Item_Deleting'Access,
         Model.all'Access
      );
      Directory_Items_Store_Handlers.Connect
      (  Cache,
         "item-inserted",
         Item_Inserted'Access,
         Model.all'Access
      );
      Directory_Items_Store_Handlers.Connect
      (  Cache,
         "item-renamed",
         Item_Renamed'Access,
         Model.all'Access
      );
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Directory_Items_View;
                Store   : not null access
                          Gtk_Abstract_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path := ""
             )  is
   begin
      Widget := new Gtk_Directory_Items_View_Record;
      Initialize (Widget, Store, Columns, Current);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Directory_Items_View;
                Tree    : not null access
                          Gtk_Directory_Tree_View_Record'Class;
                Columns : Positive
             )  is
   begin
      Widget := new Gtk_Directory_Items_View_Record;
      Initialize (Widget, Tree, Columns);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget   : out Gtk_Directory_Tree_View;
                Store    : not null access
                           Gtk_Abstract_Directory_Record'Class;
                Selected : Item_Path := ""
             )  is
   begin
      Widget := new Gtk_Directory_Tree_View_Record;
      Initialize (Widget, Store, Selected);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   function Has_Child
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      return Children (Model, Iter) /= Null_Iter;
   end Has_Child;

   function Has_Child
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      return Children (Model, Iter) /= Null_Iter;
   end Has_Child;

   procedure Initialize
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class
             )  is
   begin
      Gtk_New
      (  Store.Tree,
         (  GType_String, -- Icon
            GType_String, -- Name
            GType_Int     -- Directory caching flag
      )  );
      -- Directories view
      Initialize (Store, Get_Cache_Model_Type);
      -- Filling the root directory
      declare
         Updated : Boolean;
         Root    : Gtk_Tree_Iter := Null_Iter;
      begin
         Add_Folder (Store, Root, "", True, Updated);
      end;
   end Initialize;

   procedure Initialize
             (  Widget   : not null access
                           Gtk_Directory_Tree_View_Record'Class;
                Store    : not null access
                           Gtk_Abstract_Directory_Record'Class;
                Selected : Item_Path
             )  is
   begin
      Widget.Cache := Store.all'Access;
      Gtk.Tree_View.Initialize (Widget);
      Widget.Set_Enable_Search (False);
      Set_Rules_Hint (Widget, True);
      Widget.Set_Headers_Visible (False);
      declare
         Column    : Gtk_Tree_View_Column;
         Column_No : GInt;
      begin
         Gtk_New (Column);

         Gtk_New (Widget.Icon_Renderer);
         Column.Pack_Start (Widget.Icon_Renderer, False);
         Add_Stock_Attribute (Column, Widget.Icon_Renderer, 0);

         Gtk_New (Widget.Name_Renderer);
         Column.Pack_Start (Widget.Name_Renderer, True);
         Column.Add_Attribute (Widget.Name_Renderer, "text", 1);
         Column_No := Append_Column (Widget, Column);
         Column.Set_Resizable (True);
         Tree_Functions.Set_Cell_Data_Func
         (  Column,
            Widget.Name_Renderer,
            Set_Tree_Name'Access,
            Widget.all'Access
         );
         Tree_Functions.Set_Cell_Data_Func
         (  Column,
            Widget.Icon_Renderer,
            Set_Tree_Icon'Access,
            Widget.all'Access
         );
      end;
      Widget.Get_Selection.Set_Mode (Selection_Single);
      Directory_Tree_Handlers.Connect
      (  Widget.Name_Renderer,
         "edited",
         Edited_Directory'Access,
         Widget.all'Access
      );
      Directory_Tree_Handlers.Connect
      (  Widget,
         "row-activated",
         Directory_Activated'Access,
         Widget.all'Access
      );
      Directory_Tree_Handlers.Connect
      (  Widget,
         "row-expanded",
         Directory_Expanded'Access,
         Widget.all'Access
      );
      Directory_Tree_Handlers.Connect
      (  Widget.Get_Selection,
         "changed",
         Directory_Changed'Access,
         Widget.all'Access
      );
      Directory_Tree_Handlers.Connect
      (  Widget.Cache,
         "refreshed",
         Directory_Refreshed'Access,
         Widget.all'Access
      );
      Widget.Set_Model (To_Interface (Store));
      if Selected'Length > 0 then
         Widget.Set_Current_Directory (Selected);
      end if;
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Directory_Items_View_Record'Class;
                Store   : not null access
                          Gtk_Abstract_Directory_Record'Class;
                Columns : Positive;
                Current : Gtk_Tree_Iter
             )  is
      Changed : Boolean := False;
   begin
      G_New (Widget, Get_Directory_Items_View_Type);
      Gtk.Tree_View.Initialize (Widget);
      Widget.Set_Headers_Visible (False);
      Widget.Set_Enable_Search (False);
      Gtk_New (Widget.Content, Store, Widget);
      Widget.Markup := new Gtk_Selection_Store_Record;
      Widget.Markup.View := Widget.all'Access;
      Initialize (Widget.Markup, Widget.Content, (1 => GType_Boolean));
      Gtk_New
      (  Widget.Columns,
         Widget.Markup,
         Columns,
         Null_Iter -- Rooted in the markup's root
      );
      -- List view
      Widget.Name_Renderers :=
         new Gtk_Cell_Renderer_Text_Array (1..Columns);
      declare
         Column    : Gtk_Tree_View_Column;
         Column_No : GInt;
         Data      : Column_Data;
      begin
         Data.Browser := Widget.all'Access;
         Gtk_New (Data.Icon_Renderer);
         for No in 0..GInt (Columns) - 1 loop
            Gtk_New (Data.Text_Renderer);
            Widget.Name_Renderers (Integer (No + 1)) :=
               Data.Text_Renderer;
            Gtk_New (Column);

            Column.Pack_Start (Data.Icon_Renderer, False);
            Column.Pack_Start (Data.Text_Renderer, True);
            Column_No := Append_Column (Widget, Column);
--          Set_Sizing (Column, Tree_View_Column_Fixed);
            Set_Expand (Column, True);

            Data.Column := Positive (No + 1);
            Column_Functions.Set_Cell_Data_Func
            (  Column,
               Data.Icon_Renderer,
               Set_Column_Data'Access,
               Data
            );
            Directory_Items_Commit_Handlers.Connect
            (  Data.Text_Renderer,
               "edited",
               Edited_Item'Access,
               Data
            );
         end loop;
      end;
      Widget.Get_Selection.Set_Mode (Selection_None);
      if Widget.Directories /= null then
         Directory_Items_Result_Handlers.Connect
         (  Widget,
            "button_press_event",
            Directory_Items_Result_Handlers.
            Event_Marshaller.To_Marshaller (Key_Press'Access),
            Widget.all'Access
         );
         Directory_Items_Handlers.Connect
         (  Widget,
            "destroy",
            Destroy'Access,
            Widget.all'Access
         );
         Directory_Selection_Handlers.Connect
         (  Widget.Directories.Get_Selection,
            "changed",
            Selection_Changed'Access,
            Widget.all'Access
         );
         Directory_Items_Result_Handlers.Connect
         (  Widget,
            "key_press_event",
            Directory_Items_Result_Handlers.
            To_Marshaller (Key_Press'Access),
            Widget.all'Access
         );
         Directory_Items_Handlers.Connect
         (  Widget,
            "row-activated",
            Item_Activated'Access,
            Widget.all'Access
         );
         Directory_Items_Handlers.Connect
         (  Widget.Directories,
            "row-collapsed",
            Directory_Collapsed'Access,
            Widget.all'Access
         );
      end if;
      Widget.Set_Model (To_Interface (Widget.Columns));
      Set_Current (Widget, Current, Changed);
      if Changed  then
         Directory_Changed (Widget);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Initialize (Gtk_Directory_Items_View)")
         )  );
         if Widget.Columns /= null then
            Unref (Widget.Columns);
            Widget.Columns := null;
         end if;
         if Widget.Markup /= null then
            Unref (Widget.Markup);
            Widget.Markup := null;
         end if;
         if Widget.Content /= null then
            Unref (Widget.Content);
            Widget.Content := null;
         end if;
         if Widget.Directories /= null then
            Unref (Widget.Directories);
         end if;
         raise;
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Directory_Items_View_Record'Class;
                Store   : not null access
                          Gtk_Abstract_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path
             )  is
      Best_Match, Exact_Match : Gtk_Tree_Iter;
   begin
      Cache (Store, Current, Best_Match, Exact_Match);
      Initialize (Widget, Store, Columns, Best_Match);
   end Initialize;

   procedure Initialize
             (  Widget  : not null access
                          Gtk_Directory_Items_View_Record'Class;
                Tree    : not null access
                          Gtk_Directory_Tree_View_Record'Class;
                Columns : Positive
             )  is
      Row   : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Tree.Get_Selection.Get_Selected (Model, Row);
      if Row /= Null_Iter then
         Row := To_Item (Tree.Cache, Row);
      end if;
      Widget.Directories := Tree.all'Access;
      Ref (Widget.Directories);
      Initialize (Widget, Tree.Cache, Columns, Row);
   end Initialize;

   function Input_Event
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive;
               Event  : Gdk_Event
            )  return Boolean is
   begin
      return False;
   end Input_Event;

   procedure Inserted
             (  Model : not null access Gtk_Selection_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      Inserted
      (  Gtk_Extension_Store_Record (Model.all)'Access,
         Path,
         Iter
      );
      if (  Model.Mode = Selection_Browse
         and then
            Model.Selected = 0
         )
      then
         -- Select one item
         declare
            Current : constant Natural := Model.View.Get_Current;
         begin
            if Current > 0 then
               Model.View.Change_Selection (Current, True);
            else
               Model.View.Change_Selection (1, True);
            end if;
         end;
         Model.View.Selection_Changed;
      end if;
   end Inserted;

   function Is_Directory
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Boolean is
   begin
      return
      (  Get_Int
         (  Widget.Content.Cache.Tree,
            Widget.From_Marked (Widget.Get_Iter (Index)),
            2
         )
      in Cached_Directory
      );
   end Is_Directory;

   function Is_Editable
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Boolean is
   begin
      return
         Get_Property
         (  Widget.Name_Renderers (1),
            Editable_Property
         );
   end Is_Editable;

   function Is_Editable
            (  Widget : not null access Gtk_Directory_Tree_View_Record
            )  return Boolean is
   begin
      return Get_Property (Widget.Name_Renderer, Editable_Property);
   end Is_Editable;

   function Is_Selected
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Boolean is
   begin
      return
         Get_Boolean (Widget.Markup, Widget.Get_Iter (Index), 3);
   end Is_Selected;

   procedure Item_Activated
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Items_View
             )  is
      Path   : constant Gtk_Tree_Path :=
               Convert (Get_Address (Nth (Params, 1)));
      Column : constant Gtk_Tree_View_Column :=
               Get_Column (Nth (Params, 2));
   begin
      if Path = Null_Gtk_Tree_Path or else Column = null then
         return;
      else
         Browser.Activated
         (  Browser.Get_Index
            (  Positive
               (  Get_Row_No (To_Interface (Browser.Columns), Path)
               +  1
               ),
               Positive (Get_Column_No (Browser, Column) + 1)
         )  );
      end if;
   end Item_Activated;

   procedure Item_Deleted
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             )  is
   begin
      if Store.Root = Null_Gtk_Tree_Path then
         return;
      end if;
      if Store.Deleted >= 0 then
         declare
            Path : constant Gtk_Tree_Path := Gtk_Tree_Path_New;
         begin
            Append_Index (Path, Store.Deleted);
            Store.Count   := Store.Count - 1;
            Store.Deleted := -1;
            Row_Deleted (To_Interface (Store), Path);
            Path_Free (Path);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Item_Deleting (Gtk_Directory_Items_Store)")
         )  );
   end Item_Deleted;

   procedure Item_Deleting
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             )  is
      Row : constant Gtk_Tree_Iter := Get_Tree_Iter (Nth (Params, 1));
   begin
      if Store.Root = Null_Gtk_Tree_Path then
         Store.Deleted := -1;
      else
         Store.Deleted := Store.To_Filtered (Row);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Item_Deleting (Gtk_Directory_Items_Store)")
         )  );
   end Item_Deleting;

   procedure Item_Inserted
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             )  is
      Row  : constant Gtk_Tree_Iter := Get_Tree_Iter (Nth (Params, 1));
      Path : constant Gtk_Tree_Path := Get_Tree_Path (Nth (Params, 2));
   begin
      if Store.Root = Null_Gtk_Tree_Path then
         return;
      end if;
      if Store.Cache.Get_Int (Row, 2) in Cached_Node then
         declare
            Filtered : constant Gtk_Tree_Path :=
                       Store.To_Filtered (Path);
         begin
            if Filtered /= Null_Gtk_Tree_Path then
               Row_Inserted (To_Interface (Store), Filtered, Row);
               Path_Free (Filtered);
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Item_Inserted (Gtk_Directory_Items_Store)")
         )  );
   end Item_Inserted;

   procedure Item_Renamed
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             )  is
      Row     : constant Gtk_Tree_Iter :=
                Get_Tree_Iter (Nth (Params, 1));
      Exists  : Boolean := False;
      Existed : Boolean := False;
      Path    : Gtk_Tree_Path;
   begin
      if Store.Root = Null_Gtk_Tree_Path then
         return;
      end if;
      case Get_Int (Store.Cache.Tree, Row, 2) is
         when Cached_Directory => -- Directory is always visible
            Existed := True;
            Exists  := True;
         when Cached_Item =>      -- Items are to be filtered
            Exists :=
               Store.View.Filter
               (  False,
                  Get_Name (Store.Cache.Tree, Row),
                  Get_Type (Store.Cache.Tree, Row)
               );
            Existed :=
               Store.View.Filter
               (  False,
                  Item_Name (Get_String (Nth (Params, 2))),
                  Get_Type (Store.Cache.Tree, Row)
               );
         when others =>
            return;
      end case;
      if Existed then
         if Exists then
            Path := Store.View.Content.Get_Path (Row);
            if Path /= Null_Gtk_Tree_Path then
               Row_Changed
               (  To_Interface (Store.View.Content),
                  Path,
                  Row
               );
               Path_Free (Path);
            end if;
         else
            Path := Store.View.Content.Get_Path_Before (Row);
            if Path /= Null_Gtk_Tree_Path then
               Row_Deleted (To_Interface (Store.View.Content), Path);
               Path_Free (Path);
            end if;
         end if;
      else
         if Exists then
            Path := Store.View.Content.Get_Path (Row);
            if Path /= Null_Gtk_Tree_Path then
               Row_Inserted
               (  To_Interface (Store.View.Content),
                  Path,
                  Row
               );
               Path_Free (Path);
            end if;
         else
            null;
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Item_Renamed (Gtk_Directory_Items_Store)")
         )  );
   end Item_Renamed;

   function Key_Press
            (  Object  : access GObject_Record'Class;
               Event   : Gdk_Event;
               Browser : Gtk_Directory_Items_View
            )  return Boolean is
      Changed : Boolean;
   begin
      case Get_Event_Type (Event) is
         when Button_Press =>
            -- Button click
            if (  0
               /= (Trace_Key_Presses and Browser.Content.Cache.Tracing)
               )
            then
               Browser.Content.Cache.Trace
               (  Browser.Content.Cache.Depth,
                  "button click"
               );
            end if;
            declare
               Index : constant Natural :=
                       Browser.Locate (Event.Button.X, Event.Button.Y);
            begin
               if Get_Button (Event) = 1 then
                  Browser.Move (Changed, Get_State (Event), Index);
                  return not Changed;
               else
                  if Index > 0 then
                     return Input_Event (Browser, Index, Event);
                  else
                     return False;
                  end if;
               end if;
            end;
         when Gdk_2Button_Press | Gdk_3Button_Press =>
            -- GDouble click
            if (  0
               /= (Trace_Key_Presses and Browser.Content.Cache.Tracing)
               )
            then
               Trace
               (  Browser.Content.Cache,
                  Browser.Content.Cache.Depth,
                  "GDouble click"
               );
            end if;
            declare
               Index : constant Natural :=
                       Browser.Locate (Event.Button.X, Event.Button.Y);
            begin
               if Index > 0 then
                  Browser.Activated (Index);
               end if;
               return True;
            end;
         when Key_Press =>
            -- Key press
            case Get_Key_Val (Event) is
               when GDK_Up | GDK_KP_Up =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key up"
                     );
                  end if;
                  Browser.Move
                  (  Changed,
                     Get_State (Event),
                     Browser.Get_Current,
                     -1
                  );
                  return True;
               when GDK_Down | GDK_KP_Down =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key down"
                     );
                  end if;
                  Browser.Move
                  (  Changed,
                     Get_State (Event),
                     Browser.Get_Current,
                     1
                  );
                  return True;
               when GDK_Left | GDK_KP_Left =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key left"
                     );
                  end if;
                  Browser.Move
                  (  Changed,
                     Get_State (Event),
                     Browser.Get_Current,
                     -Browser.Columns.Get_Rows (False),
                     True
                  );
                  return True;
               when GDK_Page_Down | GDK_KP_Page_Down =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key down"
                     );
                  end if;
                  Browser.Move
                  (  Changed,
                     Get_State (Event),
                     Browser.Get_Current,
                     Get_Visible_Height (Browser)
                  );
                  return True;
               when GDK_Page_Up | GDK_KP_Page_Up =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key page up"
                     );
                  end if;
                  Browser.Move
                  (  Changed,
                     Get_State (Event),
                     Browser.Get_Current,
                     -Get_Visible_Height (Browser)
                  );
                  return True;
               when GDK_Right | GDK_KP_Right =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key right"
                     );
                  end if;
                  Browser.Move
                  (  Changed,
                     Get_State (Event),
                     Browser.Get_Current,
                     Browser.Columns.Get_Rows (False),
                     True
                  );
                  return True;
               when GDK_Home | GDK_KP_Home =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key home"
                     );
                  end if;
                  Browser.Move (Changed, Get_State (Event), 1);
                  return True;
               when GDK_End | GDK_KP_End =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Trace
                     (  Browser.Content.Cache,
                        Browser.Content.Cache.Depth,
                        "key end"
                     );
                  end if;
                  Browser.Move
                  (  Changed,
                     Get_State (Event),
                     Browser.Get_Directory_Size
                  );
                  return True;
               when GDK_F5 =>
                  if (  0
                     /= (  Trace_Key_Presses
                        and
                           Browser.Content.Cache.Tracing
                     )  )
                  then
                     Browser.Content.Cache.Trace
                     (  Browser.Content.Cache.Depth,
                        "key F5"
                     );
                  end if;
                  declare
                     Wait : Wait_Cursor (+Browser);
                  begin
                     Gtk.Abstract_Browser.Changed
                     (  Browser.Content.Cache,
                        Browser.Directories.Get_Current_Directory
                     );
                  end;
                  return True;
               when others =>
                  -- Not a special key
                  declare
                     Key : constant GUnichar :=
                           Keyval_To_Unicode (Get_Key_Val (Event));
                  begin
                     if Key = 0 then
                        return False;
                     else
                        if (  0
                           /= (  Trace_Key_Presses
                              and
                                 Browser.Content.Cache.Tracing
                           )  )
                        then
                           Browser.Content.Cache.Trace
                           (  Browser.Content.Cache.Depth,
                              "key" & GUnichar'Image (Key)
                           );
                        end if;
                        if Browser.Last_Key /= Key then
                           Browser.Last_Key       := Key;
                           Browser.Last_Position  := 0;
                        end if;
                        declare
                           Prefix : UTF8_String (1..8);
                           Last   : Natural;
                        begin
                           Unichar_To_UTF8 (Key, Prefix, Last);
                           Browser.Last_Position :=
                              Scan
                              (  Browser,
                                 Item_Name (Prefix (1..Last)),
                                 Browser.Last_Position
                              );
                        end;
                        if Browser.Last_Position /= 0 then
                           Browser.Move
                           (  Changed,
                              Get_State (Event),
                              Browser.Last_Position
                           );
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
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Key_Press")
         )  );
         return False;
   end Key_Press;

   function Locate
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               X, Y   : GDouble
            )  return Natural is
      Cell_X        : GInt;
      Cell_Y        : GInt;
      Column        : Gtk_Tree_View_Column;
      Columned_Path : Gtk_Tree_Path;
      Column_No     : GInt;
      Markup_Path   : Gtk_Tree_Path;
      Found         : Boolean;
      Result        : Natural := 0;
   begin
      begin
         Widget.Get_Path_At_Pos
         (  GInt (X),
            GInt (Y),
            Columned_Path,
            Column,
            Cell_X,
            Cell_Y,
            Found
         );
      exception
         when Constraint_Error =>
            return 0;
      end;
      if Found then
         Column_No := Get_Column_No (Widget, Column);
         if Column_No >= 0 then
            Markup_Path :=
               Widget.Columns.From_Columned
               (  Columned_Path,
                  Positive (Column_No + 1)
               );
            if Markup_Path /= Null_Gtk_Tree_Path then
               declare
                  Indices : GInt_Array renames
                            Get_Indices (Markup_Path);
               begin
                  if Indices'Length > 0 then
                     Result := Natural (Indices (Indices'Last) + 1);
                  end if;
               end;
               Path_Free (Markup_Path);
            end if;
         end if;
         Path_Free (Columned_Path);
      end if;
      return Result;
   end Locate;

   procedure Move
             (  Widget       : not null access
                               Gtk_Directory_Items_View_Record;
                Changed      : out Boolean;
                Modifier     : Gdk_Modifier_Type;
                To           : Natural;
                By           : Integer := 0;
                Fixed_Row    : Boolean := False;
                Fixed_Column : Boolean := False
             )  is
      Size     : constant Natural := Widget.Get_Directory_Size;
      Position : Integer;
   begin
      Changed := False;
      -- Determining the target position
      if To not in 1..Size then
         return;
      end if;
      if Fixed_Row or else Fixed_Column then
         declare
            New_Row    : Positive;
            New_Column : Positive;
            Old_Row    : Positive;
            Old_Column : Positive;
         begin
            Widget.Get_Position (To, Old_Row, Old_Column);
            Position := To + By;
            if Position > Widget.Get_Directory_Size then
               Position := Widget.Get_Directory_Size;
            elsif Position < 1 then
               Position := 1;
            end if;
            Widget.Get_Position (Position, New_Row, New_Column);
            if Fixed_Row and then New_Row /= Old_Row then
               -- The row differs
               if By > 0 then
                  -- Moving  forwards using the last available column to
                  -- in the row.
                  New_Column := Widget.Columns.Get_Row_Width (New_Row);
               else
                  -- Moving backwards using the first column
                  New_Column := 1;
               end if;
               if Fixed_Column and then New_Column /= Old_Column then
                  return;
               end if;
               Position := Widget.Get_Index (Old_Row, New_Column);
            elsif Fixed_Column and then New_Column /= Old_Column then
               if By > 0 then
                  -- Moving forwards using the last available row
                  New_Row :=
                     Widget.Columns.Get_Column_Height (New_Column);
               else
                  -- Moving backwards using the first row
                  New_Row := 1;
               end if;
               if Fixed_Row and then New_Row /= Old_Row then
                  return;
               end if;
               Position := Widget.Get_Index (New_Row, Old_Column);
            end if;
         end;
      else
         Position := To + By;
         if Position > Widget.Get_Directory_Size then
            Position := Widget.Get_Directory_Size;
         elsif Position < 1 then
            Position := 1;
         end if;
      end if;
      -- Dealing with selection
      case Widget.Markup.Mode is
         when Selection_None =>
            null;
         when Selection_Single =>
            if 0 = (Modifier and Control_Mask) then
               -- Simple selection
               if not Widget.Is_Selected (Position) then
                  if Widget.Get_Selection_Size > 0 then
                     -- Deselect any selected
                     for Index in 1..Size loop
                        if Widget.Is_Selected (Index) then
                           Widget.Change_Selection (Index, False);
                           exit;
                        end if;
                     end loop;
                  end if;
                  Widget.Change_Selection (Position, True);
                  Changed := True;
               end if;
            else
               -- Selection toggling
               if Widget.Is_Selected (Position) then
                  Widget.Change_Selection (Position, False);
               else
                  if Widget.Get_Selection_Size > 0 then
                     -- Deselect any selected
                     for Index in 1..Size loop
                        if Widget.Is_Selected (Index) then
                           Widget.Change_Selection (Index, False);
                           exit;
                        end if;
                     end loop;
                  end if;
                  Widget.Change_Selection (Position, True);
               end if;
               Changed := True;
            end if;
         when Selection_Browse =>
            if not Widget.Is_Selected (Position) then
               -- Deselect any selected
               for Index in 1..Size loop
                  if Widget.Is_Selected (Index) then
                     Widget.Change_Selection (Index, False);
                     exit;
                  end if;
               end loop;
               Widget.Change_Selection (Position, True);
               Changed := True;
            end if;
         when Selection_Multiple =>
            if 0 = (Modifier and Shift_Mask) then
               if 0 = (Modifier and Control_Mask) then
                  -- Single selection
                  for Index in 1..Size loop
                     if Widget.Is_Selected (Index) then
                        if Index /= Position then
                           Widget.Change_Selection (Index, False);
                           Changed := True;
                        end if;
                     else
                        if Index = Position then
                           Widget.Change_Selection (Index, True);
                           Changed := True;
                        end if;
                     end if;
                  end loop;
               else
                  -- Toggling selection
                  if Widget.Is_Selected (Position) then
                     Widget.Change_Selection (Position, False);
                  else
                     Widget.Change_Selection (Position, True);
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
                  if not Widget.Is_Selected (Position) then
                     Above := Integer'Last;
                     while From > 1 loop
                        From := From - 1;
                        if Widget.Is_Selected (From) then
                           Above := Position - From;
                           exit;
                        end if;
                     end loop;
                     Below := Integer'Last;
                     while To < Size loop
                        To := To + 1;
                        if Widget.Is_Selected (To) then
                           Below := Below - Position;
                           exit;
                        end if;
                     end loop;
                  end if;
                  while From > 1 loop
                     exit when not Widget.Is_Selected (From - 1);
                     From := From - 1;
                  end loop;
                  while To < Size loop
                     exit when not Widget.Is_Selected (To + 1);
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
                  if 0 = (Modifier and Control_Mask) then
                     -- Deselect anything before From
                     for Index in 1..From - 1 loop
                        if Widget.Is_Selected (Index) then
                           Widget.Change_Selection (Index, False);
                           Changed := True;
                        end if;
                     end loop;
                     -- Deselect anything after To
                     for Index in To + 1..Size loop
                        if Widget.Is_Selected (Index) then
                           Widget.Change_Selection (Index, False);
                           Changed := True;
                        end if;
                     end loop;
                  end if;
                  -- Select everything in From..To
                  for Index in From..To loop
                     if not Widget.Is_Selected (Index) then
                        Widget.Change_Selection (Index, True);
                        Changed := True;
                     end if;
                  end loop;
               end;
            end if;
      end case;
      -- Moving to the target position
      if Widget.Get_Current /= Position then
         -- Set position to the target
         declare
            Column_No : Positive;
            Column    : Gtk_Tree_View_Column;
            Iter      : Gtk_Tree_Iter := Widget.Get_Iter (Position);
            Path      : Gtk_Tree_Path;
         begin
            Widget.Columns.To_Columned (Iter, Column_No);
            Path := Get_Path (Widget.Columns, Iter);
            Column := Widget.Get_Column (GInt (Column_No) - 1);
            Widget.Set_Cursor_On_Cell
            (  Path          => Path,
               Focus_Column  => Column,
               Start_Editing => False,
               Focus_Cell    =>
                  Widget.Name_Renderers (Column_No).all'Access
            );
            Widget.Scroll_To_Cell (Path, Column, False, 0.5, 0.0);
            Widget.Grab_Focus;
            Path_Free (Path);
         end;
      end if;
      if Changed then
         Widget.Selection_Changed;
      end if;
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

   procedure Next
             (  Model : not null access Gtk_Abstract_Directory_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      loop
         Model.Tree.Next (Iter);
         exit when Iter = Null_Iter
           or else Get_Int (Model.Tree, Iter, 2) in Cached_Directory;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Next (Gtk_Abstract_Directory)")
         )  );
   end Next;

   procedure Next
             (  Model : not null access
                        Gtk_Directory_Items_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      if not Model.Check_Iter (Iter) then
         Iter := Null_Iter;
         return;
      end if;
      while Iter /= Null_Iter loop
         Model.Cache.Tree.Next (Iter);
         exit when Iter = Null_Iter;
         case Get_Int (Model.Cache.Tree, Iter, 2) is
            when Cached_Directory => -- Directory is always visible
               return;
            when Cached_Item =>      -- Items are to be filtered
               if Model.View.Filter
                  (  False,
                     Get_Name (Model.Cache.Tree, Iter),
                     Get_Type (Model.Cache.Tree, Iter)
                  )
               then
                  return;
               end if;
            when others =>
               null;
         end case;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Next (Gtk_Directory_Items_Store)")
         )  );
   end Next;

   function Nth_Child
            (  Model  : not null access Gtk_Abstract_Directory_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is
      Row   : Gtk_Tree_Iter;
      Count : GInt := N;
   begin
      if Parent = Null_Iter then
         Row := Model.Tree.Get_Iter_First;
      else
         Row := Model.Tree.Children (Parent);
      end if;
      while Row /= Null_Iter loop
         if Get_Int (Model.Tree, Row, 2) in Cached_Directory then
            exit when Count = 0;
            Count := Count - 1;
         end if;
         Model.Tree.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Nth_Child (Gtk_Directory_Items_Store)")
         )  );
         return Null_Iter;
   end Nth_Child;

   function Nth_Child
            (  Model  : not null access
                        Gtk_Directory_Items_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is
      Row   : Gtk_Tree_Iter := Children (Model.Cache.Tree, Parent);
      Count : GInt := 0;
   begin
      if Parent /= Null_Iter then -- Only one level of depth
         return Null_Iter;
      elsif Model.Root = Null_Gtk_Tree_Path then
         return Null_Iter;
      end if;
      Row := Children
             (  Model.Cache.Tree,
                Model.Cache.Tree.Get_Iter (Model.Root)
             );
      while Row /= Null_Iter loop
         case Get_Int (Model.Cache.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               Count := Count + 1;
               if Count > Model.Count then
                  Model.Count := Count;
               end if;
               exit when Count > N;
            when Cached_Item =>      -- Items are to be filtered
               if Model.View.Filter
                  (  False,
                     Get_Name (Model.Cache.Tree, Row),
                     Get_Type (Model.Cache.Tree, Row)
                  )
               then
                  Count := Count + 1;
                  if Count > Model.Count then
                     Model.Count := Count;
                  end if;
                  exit when Count > N;
               end if;
            when others =>
               null;
         end case;
         Model.Cache.Tree.Next (Row);
      end loop;
      return Row;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Nth_Child (Gtk_Directory_Items_Store)")
         )  );
         return Null_Iter;
   end Nth_Child;

   function N_Children
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt is
      Row   : Gtk_Tree_Iter;
      Count : GInt := 0;
   begin
      if Iter = Null_Iter then
         Row := Model.Tree.Get_Iter_First;
      else
         Row := Model.Tree.Children (Iter);
      end if;
      while Row /= Null_Iter loop
         if Get_Int (Model.Tree, Row, 2) in Cached_Directory then
            Count := Count + 1;
         end if;
         Model.Tree.Next (Row);
      end loop;
      return Count;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("N_Children (Gtk_Abstract_Directory)")
         )  );
         return 0;
   end N_Children;

   function N_Children
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt is
      Row : Gtk_Tree_Iter;
   begin
      if Iter /= Null_Iter or else Model.Root = Null_Gtk_Tree_Path then
         return 0;
      end if;
      Row := Model.Cache.Tree.Get_Iter (Model.Root);
      Model.Count := 0;
      while Row /= Null_Iter loop
         case Get_Int (Model.Cache.Tree, Row, 2) is
            when Cached_Directory => -- Directory is always visible
               Model.Count := Model.Count + 1;
            when Cached_Item =>      -- Items are to be filtered
               if Model.View.Filter
                  (  False,
                     Get_Name (Model.Cache.Tree, Row),
                     Get_Type (Model.Cache.Tree, Row)
                  )
               then
                  Model.Count := Model.Count + 1;
               end if;
            when others =>
               null;
         end case;
         Model.Cache.Tree.Next (Row);
      end loop;
      return Model.Count;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("N_Children (Gtk_Directory_Items_Store)")
         )  );
         return Model.Count;
   end N_Children;

   function Parent
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Parent (Model.Tree, Child);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Parent")
         )  );
         return Null_Iter;
   end Parent;

   function Parent
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Null_Iter;
   end Parent;

   procedure Previous
             (  Model : not null access Gtk_Abstract_Directory_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      loop
         Model.Tree.Previous (Iter);
         exit when Iter = Null_Iter
           or else Get_Int (Model.Tree, Iter, 2) in Cached_Directory;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Previous (Gtk_Abstract_Directory)")
         )  );
   end Previous;

   procedure Previous
             (  Model : not null access
                        Gtk_Directory_Items_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      if not Model.Check_Iter (Iter) then
         Iter := Null_Iter;
         return;
      end if;
      while Iter /= Null_Iter loop
         Model.Cache.Tree.Previous (Iter);
         exit when Iter = Null_Iter;
         case Get_Int (Model.Cache.Tree, Iter, 2) is
            when Cached_Directory => -- Directory is always visible
               exit;
            when Cached_Item =>      -- Items are to be filtered
               exit when
                  Model.View.Filter
                  (  False,
                     Get_Name (Model.Cache.Tree, Iter),
                     Get_Type (Model.Cache.Tree, Iter)
                  );
            when others =>
               null;
         end case;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Previous (Gtk_Directory_Items_Store)")
         )  );
   end Previous;

   procedure Progress
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record;
                Directory : Item_Path;
                State     : GDouble
             )  is
   begin
      Emit (Store, Progress_ID, Directory, State);
   end Progress;

   procedure Read_Error
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Text  : UTF8_String;
                Path  : Item_Path
             )  is
   begin
      Emit (Store, Read_Error_ID, Text, Path);
   end Read_Error;

   procedure Refilter
             (  Widget : not null access Gtk_Directory_Items_View_Record
             )  is
      Row     : Gtk_Tree_Iter;
      Path    : Gtk_Tree_Path;
      Content : constant Gtk_Tree_Model :=
                To_Interface (Widget.Content);
   begin
      Gtk_New (Path);
      for Item in reverse 0..Widget.Content.Count - 1 loop
         Append_Index (Path, Item);
         Row_Deleted (Content, Path);
         if Up (Path) then null; end if;
      end loop;
      Row := Widget.Content.Children (Null_Iter);
      Widget.Content.Count := 0;
      while Row /= Null_Iter loop
         Append_Index (Path, Widget.Content.Count);
         Widget.Content.Count := Widget.Content.Count + 1;
         Row_Inserted (Content, Path, Row);
         if Up (Path) then null; end if;
         Widget.Content.Next (Row);
      end loop;
      Path_Free (Path);
   end Refilter;

   procedure Refresh
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Item  : in out Gtk_Tree_Iter
             )  is
      Directory : constant Item_Path := Store.Get_Path (Item);
      Updated   : Boolean;
      Child     : Gtk_Tree_Iter;
   begin
      loop
         if Item = Null_Iter then
            Child := Get_Iter_First (Store.Tree);
         else
            Child := Store.Tree.Children (Item);
         end if;
         exit when Child = Null_Iter;
         declare
            Path : constant Gtk_Tree_Path := Store.Get_Path (Child);
            Name : constant Item_Name := Get_Name (Store.Tree, Child);
         begin
--           if Get_Int (Store.Tree, Child, 2) in Cached_Directory then
--              Path := Store.Get_Path (Child);
--           end if;
            Emit (Store, Deleting_ID, Child, Path);
            Store.Tree.Remove (Child);
--              if Path /= Null_Gtk_Tree_Path then
--                 Row_Deleted (To_Interface (Store), Path);
--                 Path_Free (Path);
--                 Path := Null_Gtk_Tree_Path;
--              end if;
            Row_Deleted (To_Interface (Store), Path);
            Path_Free (Path);
            Emit
            (  Store,
               Deleted_ID,
               String (Store.Get_Path (Directory, Name))
            );
         end;
      end loop;
      if Item = Null_Iter then
         Add_Folder (Store, Item, "", True, Updated);
         Store.Refreshing := Store.Refreshing + 1;
         Emit (Store, Refreshed_ID, UTF8_String'(""));
         Store.Refreshing := Store.Refreshing - 1;
      else
         declare
            Path : constant Item_Path := Store.Get_Path (Item);
         begin
            Add_Folder (Store, Item, Path, True, Updated);
            Store.Refreshing := Store.Refreshing + 1;
            Emit (Store, Refreshed_ID, String (Path));
            Store.Refreshing := Store.Refreshing - 1;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Refresh")
         )  );
   end Refresh;

   procedure Refresh
             (  Widget : not null access Gtk_Directory_Items_View_Record
             )  is
      Changed : Boolean := True;
   begin
      Set_Current (Widget, Widget.Get_Directory, Changed);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Refresh")
         )  );
   end Refresh;

   procedure Release (Ptr : in out Path_Node_Ptr) is
   begin
      if Ptr.Next /= null then
         Release (Ptr.Next);
      end if;
      Free (Ptr);
   end Release;

   procedure Renamed
             (  Store    : not null access
                           Gtk_Abstract_Directory_Record'Class;
                Old_Path : Item_Path;
                New_Name : Item_Name
             )  is
      Row : constant Gtk_Tree_Iter := Find (Store, Old_Path);
   begin
      if Row = Null_Iter then
         raise Constraint_Error;
      end if;
      declare
         Old_Name : constant Item_Name := Get_Name (Store.Tree, Row);
      begin
         Store.Tree.Set (Row, 1, String (New_Name));
         if Get_Int (Store.Tree, Row, 2) in Cached_Directory then
           declare
               Path : constant Gtk_Tree_Path := Store.Get_Path (Row);
            begin
               if Path /= Null_Gtk_Tree_Path then
                  Row_Changed (To_Interface (Store), Path, Row);
                  Path_Free (Path);
               end if;
            end;
         end if;
         Emit (Store, Renamed_ID, Row, String (Old_Name));
      end;
   end Renamed;

   procedure Renamed
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : in out Natural;
                Name   : Item_Name
             )  is
      Row : Gtk_Tree_Iter := Widget.Get_Iter (Index);
   begin
      Row := Widget.From_Marked (Row);
      declare
         Store    : constant Gtk_Abstract_Directory :=
                    Widget.Content.Cache;
         Old_Name : constant Item_Name := Get_Name (Store.Tree, Row);
      begin
         Store.Tree.Set (Row, 1, String (Name));
         declare
            Iter : constant Gtk_Tree_Iter := Widget.To_Marked (Row);
         begin
            if Iter = Null_Iter then
               Index := 0;
            else
               Index :=
                  Positive
                  (  Get_Row_No (To_Interface (Widget.Markup), Iter)
                  +  1
                  );
            end if;
         end;
         if Get_Int (Store.Tree, Row, 2) in Cached_Directory then
            declare
               Path : constant Gtk_Tree_Path := Store.Get_Path (Row);
            begin
               if Path /= Null_Gtk_Tree_Path then
                  Row_Changed (To_Interface (Store), Path, Row);
                  Path_Free (Path);
               end if;
            end;
         end if;
         Emit (Store, Renamed_ID, Row, String (Old_Name));
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Renamed (item)")
         )  );
   end Renamed;

   procedure Reset (Object : in out Item_Path_Reference) is
   begin
      Finalize (Object);
   end Reset;

   procedure Reset_Selection
             (  Widget : not null access Gtk_Directory_Items_View_Record
             )  is
      Changed : Boolean := False;
   begin
      case Widget.Markup.Mode is
         when Selection_None | Selection_Browse =>
            null;
         when Selection_Single =>
            for Index in 1..Widget.Get_Directory_Size loop
               if Widget.Is_Selected (Index) then
                  Widget.Change_Selection (Index, False);
                  Changed := True;
                  exit;
               end if;
            end loop;
         when Selection_Multiple =>
            for Index in 1..Widget.Get_Directory_Size loop
               if Widget.Is_Selected (Index) then
                  Widget.Change_Selection (Index, False);
                  Changed := True;
               end if;
            end loop;
      end case;
      if Changed then
         Widget.Selection_Changed;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Reset_Selection")
         )  );
   end Reset_Selection;

   procedure Rewind_Error
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Text  : UTF8_String;
                Path  : Item_Path
             )  is
   begin
      Emit (Store, Rewind_Error_ID, Text, Path);
   end Rewind_Error;

   function Scan
            (  Widget : not null access
                        Gtk_Directory_Items_View_Record'Class;
               Prefix : Item_Name;
               Index  : Natural
            )  return Natural is
      Row       : constant Gtk_Tree_Iter :=
                           To_Marked  (Widget, Widget.Get_Directory);
      Directory : Item_Path renames Widget.Get_Directory;
      Aim       : constant Directory_Item :=
                  (  Directory   => False,
                     Policy      => Cache_Ahead,
                     Kind_Length => 0,
                     Kind        => "",
                     Name_Length => Prefix'Length,
                     Name        => Prefix
                  );
      function Is_Prefix (Iter : Gtk_Tree_Iter) return Boolean is
         Name : constant Item_Name :=
                         Get_Name (To_Interface (Widget.Markup), Iter);
      begin
         if Name'Length < Aim.Name_Length then
            return False;
         else
            return
            (  Equal
            =  Compare
               (  Widget.Content.Cache,
                  Directory,
                  Aim,
                  (  Directory   => False,
                     Policy      => Cache_Ahead,
                     Kind_Length => 0,
                     Kind        => "",
                     Name_Length => Aim.Name_Length,
                     Name        => Name (1..Aim.Name_Length)
                  ),
                  True
            )  );
         end if;
      end Is_Prefix;

      Iter     : Gtk_Tree_Iter :=
                    Widget.Markup.Nth_Child (Row, GInt (Index));
      Position : Natural := Index;
   begin
      while Iter /= Null_Iter loop
         Position := Position + 1;
         if Is_Prefix (Iter) then
            return Position;
         end if;
         Next (Widget.Markup, Iter);
      end loop;
      return 0;
   end Scan;

   procedure Selection_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Browser   : Gtk_Directory_Items_View
             )  is
      Wait : Wait_Cursor (+Browser);
   begin
      declare
         Model     : Gtk_Tree_Model;
         Directory : Gtk_Tree_Iter;
         Changed   : Boolean := False;
      begin
         Selection.Get_Selected (Model, Directory);
         if Directory = Null_Iter then
            -- Try to cache the root directory we are switching to
            Add_Folder
            (  Browser.Content.Cache,
               Directory,
               "",
               False,
               Changed
            );
            if Directory = Null_Iter then
               Set_Current (Browser, Null_Iter, Changed);
            else
               Set_Current (Browser, Directory, Changed);
            end if;
         else
            Directory := To_Item (Browser.Content.Cache, Directory);
            case Get_Int (Browser.Content.Cache.Tree, Directory, 2) is
               when Cached_Children =>
                  null;
               when Cached_Never_Directory | Cached_Item =>
                  -- Try to cache the directory we are switching to
                  Add_Folder
                  (  Browser.Content.Cache,
                     Directory,
                     Get_Path (Browser.Content.Cache, Directory),
                     False,
                     Changed
                  );
               when others =>
                  return;
            end case;
            Set_Current (Browser, Directory, Changed);
            Select_Iter
            (  Selection,
               From_Item
               (  Browser.Content.Cache,
                  Browser.Get_Directory
            )  );
         end if;
         if Changed then
            Browser.Directory_Changed;
         end if;
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Selection_Changed")
            )  );
      end;
   end Selection_Changed;

   procedure Selection_Changed
             (  Widget : not null access Gtk_Directory_Items_View_Record
             )  is
   begin
      Directory_Tree_Handlers.Emit_By_Name
      (  Widget,
         "selection-changed"
      );
   end Selection_Changed;

   procedure Set
             (  Object : in out Item_Path_Reference;
                Path   : Item_Path
             )  is
   begin
      if Object.Ptr = null then
         if Path'Length = 0 then
            return;
         end if;
      else
         if Object.Ptr.Path = Path then
            return;
         end if;
         Finalize (Object);
      end if;
      Object.Ptr := new Item_Path_Object (Path'Length);
      Object.Ptr.Path := Path;
   end Set;

   procedure Set
             (  Object : in out Item_Path_Reference;
                Widget : not null access
                         Gtk_Directory_Tree_View_Record'Class
             )  is
      Row   : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Widget.Get_Selection.Get_Selected (Model, Row);
      if Row = Null_Iter then
         Finalize (Object);
      else
         Set
         (  Object,
            Widget.Cache.Get_Path (To_Item (Widget.Cache, Row))
         );
      end if;
   end Set;

   procedure Set_Column_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access
                         Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Column_Data
             )  is
      FG, BG : Gdk_RGBA;
      Style  : Gtk_Style_Context;
      Offset : constant GInt := GInt (Data.Column - 1) * 4;
   begin
      Set_Property (Data.Text_Renderer, Background_Set_Property, True);
      Set_Property (Data.Text_Renderer, Foreground_Set_Property, True);
      Set_Property
      (  Data.Text_Renderer,
         Cell_Background_Set_Property,
         True
      );
      Style := Get_Style_Context (Data.Browser);
      if Get_Boolean (Model, Iter, Offset + 3) then
--           if Data.Browser.Has_Visible_Focus then
         Style.Get_Color (Gtk_State_Flag_Selected, FG);
         Style.Get_Background_Color (Gtk_State_Flag_Selected, BG);
--           else
--              Style.Get_Color (Gtk_State_Flag_Active, FG);
--              Style.Get_Background_Color (Gtk_State_Flag_Active, BG);
--           end if;
      else
         Style.Get_Color (Gtk_State_Flag_Normal, FG);
         Style.Get_Background_Color (Gtk_State_Flag_Normal, BG);
      end if;
      Set_Property (Data.Text_Renderer, Foreground_RGBA_Property, FG);
      Set_Property (Data.Text_Renderer, Background_RGBA_Property, BG);
      Set_Property
      (  Data.Text_Renderer,
         Cell_Background_RGBA_Property,
         BG
      );
      declare
         Flags : constant GInt := Get_Int  (Model, Iter, Offset + 2);
         Name  : constant Item_Name :=
                 Get_Name (Model, Iter, Offset + 1);
         Path  : Gtk_Tree_Path := Get_Path (Model, Iter);
         Icon  : Icon_Data :=
                    Get_Icon
                    (  Data.Browser,
                       Name,
                       Get_Type (Model, Iter, Offset),
                       Flags in Cached_Directory,
                       Flags /= Cached_Never_Directory
                    );
      begin
         case Icon.Kind is
            when Stock_ID =>
               if Icon.Name'Length > 0 then
                  Set_Property
                  (  Data.Icon_Renderer,
                     Stock_ID_Property,
                     Icon.Name
                  );
               else
                  Set_Null (Data.Icon_Renderer);
               end if;
            when GIcon =>
               if Icon.Icon = null then
                  Set_Null (Data.Icon_Renderer);
               else
                  Set_Property
                  (  Data.Icon_Renderer,
                     Property_Object (GLib.Build ("gicon")),
                     Icon.Icon
                  );
                  Unref (Icon.Icon);
               end if;
            when Pixbuf =>
               if Icon.Image = null then
                  Set_Null (Data.Icon_Renderer);
               else
                  Gdk.Pixbuf.Conversions.Set_Pixbuf_Property
                  (  Object => Data.Icon_Renderer,
                     Value  => Gdk.Pixbuf.Conversions.
                               To_Value (Icon.Image)
                  );
                  Unref (Icon.Image);
               end if;
            when Themed =>
               Set_Property
               (  Data.Icon_Renderer,
                  Icon_Name_Property,
                  Icon.Name
               );
         end case;
         Set_Property
         (  Data.Text_Renderer,
            Text_Property,
            String (Name)
         );
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Column_Data")
         )  );
   end Set_Column_Data;

   procedure Set_Current
             (  Widget    : not null access
                            Gtk_Directory_Items_View_Record'Class;
                Directory : Gtk_Tree_Iter;
                Changed   : in out Boolean
             )  is
   begin
      Changed := Changed or Directory /= Widget.Get_Directory;
      if Changed or else Widget.Content.Cache.Refreshing > 0 then
         if (  0
            /= (  Get_Tracing (Widget.Content.Cache)
               and
                  Trace_Set_Directory
            )  )
         then
            Trace
            (  Widget.Content.Cache,
               Get_Depth (Widget.Content.Cache),
               (  "Items root seting to "
               &  String
                  (  Item_Path'
                     (  Get_Path (Widget.Content.Cache, Directory)
            )  )  )  );
         end if;
         declare
            Columns : constant Positive :=
                               Widget.Columns.Get_Major_Columns;
         begin
            Ref (Widget.Markup);
            Ref (Widget.Content);
            Widget.Markup.Set_Null_Reference;
            Widget.Columns.Set_Null_Reference;
            if Widget.Markup.Selected /= 0 then
--               Log
--               (  GtkAda_Contributions_Domain,
--                  Log_Level_Critical,
--                  (  "Persisting selection removed"
--                  &  Where ("Set_Current")
--               )  );
               Widget.Markup.Selected := 0;
            end if;
            if Widget.Content.Root /= Null_Gtk_Tree_Path then
               Path_Free (Widget.Content.Root);
               Widget.Content.Root := Null_Gtk_Tree_Path;
            end if;
            if Directory /= Null_Iter then
               Widget.Content.Root :=
                  Widget.Content.Cache.Tree.Get_Path (Directory);
            end if;
            Refilter (Widget);
            Widget.Markup.Set_Reference (Widget.Content);
            Widget.Columns.Set_Reference
            (  Widget.Markup,
               Columns,
               Null_Iter -- Root of columns is always root of items list
            );
            Unref (Widget.Content);
            Unref (Widget.Markup);
            Columns_Autosize (Widget);
         end;
         if (  0
            /= (  Get_Tracing (Widget.Content.Cache)
               and
                  Trace_Set_Directory
            )  )
         then
            Widget.Content.Cache.Trace
            (  Get_Depth (Widget.Content.Cache),
               (  "Items root has been set to "
               &  String (Item_Path'(Widget.Get_Directory))
            )  );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Current")
         )  );
   end Set_Current;

   procedure Set_Current_Directory
             (  Widget    : not null access
                            Gtk_Directory_Tree_View_Record;
                Directory : Item_Path
             )  is
      Wait        : Wait_Cursor (+Widget);
      Best_Match  : Gtk_Tree_Iter := Null_Iter;
      Exact_Match : Gtk_Tree_Iter := Null_Iter;
   begin
      if 0 /= (Get_Tracing (Widget.Cache) and Trace_Set_Directory) then
         Trace
         (  Widget.Cache,
            Get_Depth (Widget.Cache),
            "Set to " & String (Directory)
         );
      end if;
      Cache (Widget.Cache, Directory, Best_Match, Exact_Match);
      if Best_Match /= Null_Iter then
         if 0 /= (Get_Tracing (Widget.Cache) and Trace_Set_Directory)
         then
            declare
               Path : constant Gtk_Tree_Path :=
                      Get_Path (Widget.Cache.Tree, Best_Match);
            begin
               Trace
               (  Widget.Cache,
                  Get_Depth (Widget.Cache),
                  (  "Set to "
                  &  String (Directory)
                  &  ", best match "
                  &  String (Get_Name (Widget.Cache.Tree, Best_Match))
                  &  " = "
                  &  To_String (Path)
               )  );
               Path_Free (Path);
            end;
         end if;
         while Exact_Match /= Null_Iter loop
            exit when
                 (  Get_Int (Widget.Cache.Tree, Exact_Match, 2)
                 in Cached_Children
                 );
            Exact_Match := Parent (Widget.Cache.Tree, Exact_Match);
         end loop;
         if Exact_Match /= Null_Iter then
            Best_Match := From_Item (Widget.Cache, Exact_Match);
            if Best_Match /= Null_Iter then
               declare
                  Path : Gtk_Tree_Path;
               begin
                  Path := Widget.Cache.Get_Path (Best_Match);
                  if (  0
                     /= (  Get_Tracing (Widget.Cache)
                        and
                           Trace_Set_Directory
                     )  )
                  then
                     Trace
                     (  Widget.Cache,
                        Get_Depth (Widget.Cache),
                        (  "Set to "
                        &  String (Directory)
                        &  ", selected path ="
                        &  To_String (Path)
                     )  );
                  end if;
                  Widget.Expand_To_Path (Path);
                  Widget.Scroll_To_Cell
                  (  Path,
                     Widget.Get_Column (0),
                     False,
                     0.0,
                     0.0
                  );
                  Widget.Get_Selection.Select_Path (Path);
                  Path_Free (Path);
                  return;
               exception
                  when others =>
                     Path_Free (Path);
                     raise;
               end;
            end if;
         end if;
      else
         if 0 /= (Get_Tracing (Widget.Cache) and Trace_Set_Directory)
         then
            Trace
            (  Widget.Cache,
               Get_Depth (Widget.Cache),
               "Set to " & String (Directory) & ", no best match"
            );
         end if;
      end if;
      Widget.Get_Selection.Unselect_All;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Current_Directory")
         )  );
   end Set_Current_Directory;

   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Directory_Tree_View_Record;
                Editable : Boolean
             )  is
   begin
      Set_Property
      (  Widget.Name_Renderer,
         Editable_Property,
         Editable
      );
   end Set_Editable;

   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Directory_Items_View_Record;
                Editable : Boolean
             )  is
   begin
      for Index in Widget.Name_Renderers'Range loop
         Set_Property
         (  Widget.Name_Renderers (Index),
            Editable_Property,
            Editable
         );
      end loop;
   end Set_Editable;

   procedure Set_Item
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Row   : Gtk_Tree_Iter;
                Item  : Directory_Item
             )  is
      Flag : GInt;
   begin
      if 0 /= (Store.Tracing and Trace_Cache) then
         if Item.Directory then
            Store.Trace
            (  Store.Depth,
               (  "insert dir "
               &  UTF8_String (Item.Name)
               &  " ["
               &  UTF8_String (Item.Kind)
               &  ']'
            )  );
         else
            Store.Trace
            (  Store.Depth,
               (  "insert item "
               &  UTF8_String (Item.Name)
               &  " ["
               &  UTF8_String (Item.Kind)
               &  ']'
            )  );
         end if;
      end if;
      Store.Tree.Set (Row, 1, UTF8_String (Item.Name));
      Store.Tree.Set (Row, 0, UTF8_String (Item.Kind));
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
      Store.Tree.Set (Row, 2, Flag);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Item")
         )  );
   end Set_Item;

   procedure Set_Null
             (  Cell : not null access Gtk_Cell_Renderer_Record'Class
             )  is
   begin
      Set_Property
      (  Cell,
         Property_Address (Stock_ID_Property),
         System.Null_Address
      );
   end Set_Null;

   procedure Set_Selection
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : Positive;
                State  : Boolean
             )  is
      Size    : constant Natural := Widget.Get_Directory_Size;
      Changed : Boolean := False;
   begin
      if Index > Size then
         return;
      end if;
      case Widget.Markup.Mode is
         when Selection_None =>
            null;
         when Selection_Single | Selection_Browse =>
            if State xor Widget.Is_Selected (Index) then
               if State then
                  -- Deselect any selected
                  for Item in 1..Size loop
                     if Widget.Is_Selected (Item) then
                        Widget.Change_Selection (Item, False);
                        exit;
                     end if;
                  end loop;
                  Widget.Change_Selection (Index, True);
                  Changed := True;
               elsif Widget.Markup.Mode = Selection_Single then
                  Widget.Change_Selection (Index, False);
                  Changed := True;
               end if;
            end if;
         when Selection_Multiple =>
            if Widget.Is_Selected (Index) xor State then
               Widget.Change_Selection (Index, State);
               Changed := True;
            end if;
      end case;
      if Changed then
         Widget.Selection_Changed;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Selection (items)")
         )  );
   end Set_Selection;

   procedure Set_Selection
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Name   : Item_Name;
                State  : Boolean
             )  is
      Position : constant Natural := Widget.Get_Index (Name);
   begin
      if Position > 0 then
         Widget.Set_Selection (Position, State);
      end if;
   end Set_Selection;

   procedure Set_Selection_Mode
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Mode   : Gtk_Selection_Mode
             )  is
   begin
      if Widget.Markup.Mode = Mode then
         return;
      end if;
      declare
         Changed : Boolean := False;
         Size    : constant Natural := Widget.Get_Directory_Size;
         Current : Natural;
      begin
         Widget.Markup.Mode := Mode;
         case Mode is
            when Selection_None =>
               -- We just deselect anything selected
               for Index in 1..Size loop
                  if Widget.Is_Selected (Index) then
                     Widget.Change_Selection (Index, False);
                     Changed := True;
                  end if;
               end loop;
            when Selection_Single | Selection_Browse =>
               case Widget.Get_Selection_Size is
                  when 0 =>
                     if Mode = Selection_Browse then
                        -- Nothing  selected.  When  there  is a current
                        -- item we select it, else we do the first item,
                        -- otherwise nothing.
                        Current := Widget.Get_Current;
                        if Current > 0 then
                           Widget.Change_Selection (Current, True);
                           Changed := True;
                        elsif Size > 0 then
                           Widget.Change_Selection (1, True);
                           Changed := True;
                        end if;
                     end if;
                  when 1 =>
                     null;  -- One item selection, nothing to do
                  when others =>
                     -- Multiple selection, we shall deselect everything
                     -- but  one. Let's never deselect the current item,
                     -- otherwise it will be the first one that stays.
                     Current := Widget.Get_Current;
                     for Index in reverse 1..Size loop
                        if (  Current /= Index
                           and then
                              Widget.Is_Selected (Index)
                           )
                        then
                           Widget.Change_Selection (Index, False);
                           Changed := True;
                           exit when Widget.Get_Selection_Size = 1;
                        end if;
                     end loop;
               end case;
            when Selection_Multiple =>
               -- There is nothing to do
               null;
         end case;
         if Changed then
            Widget.Selection_Changed;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Selection_Mode")
         )  );
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
             (  Store   : not null access
                          Gtk_Abstract_Directory_Record'Class;
                Tracing : Traced_Actions
             )  is
   begin
      Store.Tracing := Tracing;
   end Set_Tracing;

   procedure Set_Tree_Icon
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Gtk_Directory_Tree_View
             )  is
      Item  : constant Gtk_Tree_Iter := To_Item (Data.Cache, Iter);
      Flags : constant GInt := Get_Int (Data.Cache.Tree, Item, 2);
      Icon  : Icon_Data :=
                 Get_Icon
                 (  Data,
                    Get_Type (Data.Cache.Tree, Item),
                    Expanded (Data, Iter),
                    Flags /= Cached_Never_Directory,
                    Parent (Data.Cache.Tree, Item) = Null_Iter
                 );
   begin
      case Icon.Kind is
         when Stock_ID =>
            if Icon.Name'Length > 0 then
               Set_Property (Cell, Stock_ID_Property, Icon.Name);
            else
               Set_Null (Cell);
            end if;
         when GIcon =>
            if Icon.Icon = null then
               Set_Null (Cell);
            else
               Set_Property
               (  Cell,
                  Property_Object (GLib.Build ("gicon")),
                  Icon.Icon
               );
               Unref (Icon.Icon);
            end if;
         when Pixbuf =>
            if Icon.Image = null then
               Set_Null (Cell);
            else
               Gdk.Pixbuf.Conversions.Set_Pixbuf_Property
               (  Object => Cell,
                  Value  => Gdk.Pixbuf.Conversions.To_Value (Icon.Image)
               );
               Unref (Icon.Image);
            end if;
         when Themed =>
            Set_Property
            (  Cell,
               Icon_Name_Property,
               Icon.Name
            );
      end case;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Tree_Icon")
         )  );
   end Set_Tree_Icon;

   procedure Set_Tree_Name
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Gtk_Directory_Tree_View
             )  is
      Item  : constant Gtk_Tree_Iter := To_Item (Data.Cache, Iter);
      Flags : constant GInt := Get_Int (Data.Cache.Tree, Item, 2);
   begin
      Set_Property
      (  Cell,
         Text_Property,
         UTF8_String
         (  Get_Name
            (  Data,
               Get_Name (Data.Cache.Tree, Item),
               Get_Type (Data.Cache.Tree, Item),
               Expanded (Data, Iter),
               Flags /= Cached_Never_Directory,
               Parent (Data.Cache.Tree, Item) = Null_Iter
      )  )  );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Tree_Name")
         )  );
   end Set_Tree_Name;

   procedure Split
             (  Store  : access Gtk_Abstract_Directory_Record'Class;
                Item   : Item_Path;
                Path   : out Path_Node_Ptr;
                Length : out Positive
             )  is
   begin
      Path := new Path_Node'(Item'Length, null, Item);
      Length := 1;
      loop
         declare
            Directory : constant Item_Path :=
                           Store.Get_Directory (Path.Directory);
         begin
            Path := new Path_Node'(Directory'Length, Path, Directory);
            Length := Length + 1;
         end;
      end loop;
   exception
      when Name_Error =>
         null;
   end Split;

   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Path
            )  return GInt is
   begin
      if Unfiltered = Null_Gtk_Tree_Path then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            "Null argument path" & Where ("To_Filtered")
         );
         return -1;
      elsif Model.Root = Null_Gtk_Tree_Path then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            "No model root path set" & Where ("To_Filtered")
         );
         return -1;
      end if;
      declare
         Indices : GInt_Array := Get_Indices (Unfiltered);
         Root    : constant GInt_Array := Get_Indices (Model.Root);
         Count   : GInt := -1;
      begin
         if (  Indices'Length /= Root'Length + 1
            or else
               Indices (Indices'First..Indices'Last - 1) /= Root
            )
         then -- Not a prefx
            return -1;
         end if;
         declare
            Row   : Gtk_Tree_Iter;
            Found : Boolean := False;
            Index : GInt renames Indices (Indices'Last);
         begin
            Row := Model.Cache.Tree.Children
                   (  Model.Cache.Tree.Get_Iter (Model.Root)
                   );
            --
            -- Counting filtered rows before ours
            --
            loop
               if Row = Null_Iter then
                  Log
                  (  GtkAda_Contributions_Domain,
                     Log_Level_Critical,
                     "Wrong unfiltered path" &  Where ("To_Filtered")
                  );
                  return -1;
               end if;
               case Get_Int (Model.Cache.Tree, Row, 2) is
                  when Cached_Directory => -- Directory is always
                     Count := Count + 1;   -- visible
                     Found := True;
                  when Cached_Item =>      -- Items are to be filtered
                     Found := Model.View.Filter
                              (  False,
                                 Get_Name (Model.Cache.Tree, Row),
                                 Get_Type (Model.Cache.Tree, Row)
                              );
                     if Found then
                        Count := Count + 1;
                     end if;
                  when others =>
                     Found := False;
               end case;
               exit when Index = 0;
               Index := Index - 1;
               Model.Cache.Tree.Next (Row);
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
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("To_Filtered")
         )  );
         return -1;
   end To_Filtered;

   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Iter
            )  return GInt is
   begin
      if Unfiltered = Null_Iter then
         return -1;
      else
         declare
            Path : Gtk_Tree_Path;
         begin
            Path := Model.Cache.Tree.Get_Path (Unfiltered);
            if Path = Null_Gtk_Tree_Path then
               return -1;
            else
               declare
                  Index : constant GInt := Model.To_Filtered (Path);
               begin
                  Path_Free (Path);
                  return Index;
               end;
            end if;
         end;
      end if;
   end To_Filtered;

   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if Model.To_Filtered (Unfiltered) >= 0 then
         return Unfiltered;
      else
         return Null_Iter;
      end if;
   end To_Filtered;

   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
      Index : constant GInt := Model.To_Filtered (Unfiltered);
   begin
      if Index >= 0 then
         declare
            Path : constant Gtk_Tree_Path := Gtk_Tree_Path_New;
         begin
            Append_Index (Path, Index);
            return Path;
         end;
      else
         return Null_Gtk_Tree_Path;
      end if;
   end To_Filtered;

   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Path
            )  return Gtk_Tree_Path is
      Index : constant GInt := Model.To_Filtered (Unfiltered);
   begin
      if Index < 0 then
         return Null_Gtk_Tree_Path;
      else
         declare
            Path : constant Gtk_Tree_Path := Gtk_Tree_Path_New;
         begin
            Append_Index (Path, Index);
            return Path;
         end;
      end if;
   end To_Filtered;

   function To_Item
            (  Store     : not null access
                           Gtk_Abstract_Directory_Record'Class;
               Directory : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Directory;
   end To_Item;

   function To_Marked
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Item   : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if Item = Null_Iter then
         return Null_Iter;
      end if;
      declare
         Unfiltered : constant Gtk_Tree_Path :=
                      Widget.Content.Cache.Tree.Get_Path (Item);
         Filtered   : constant Gtk_Tree_Path :=
                      To_Filtered (Widget.Content, Unfiltered);
         Row : Gtk_Tree_Iter;
      begin
         if Filtered = Null_Gtk_Tree_Path then
            return Null_Iter;
         else
            Row := Widget.Content.Get_Iter (Filtered);
            if Row = Null_Iter then
               return Null_Iter;
            else
               Path_Free (Unfiltered);
               Path_Free (Filtered);
               return Widget.Markup.To_Extension (Row);
            end if;
         end if;
      end;
   end To_Marked;

   procedure Trace
             (  Store : not null access Gtk_Abstract_Directory_Record;
                Depth : Natural;
                Text  : String
             )  is
      use Ada.Text_IO;
   begin
      if (  Trace_To_Output_Only
         /= (Store.Tracing and (Trace_To_Both or Trace_To_Output_Only))
         )
      then
         Trace (Depth * "  " & Text);
      end if;
      if (  0
         /= (Store.Tracing and (Trace_To_Both or Trace_To_Output_Only))
         )
      then
         if Has_File then
            Put_Line (File, Depth * "  " & Text);
         else
            Put_Line (Depth * "  " & Text);
         end if;
      end if;
   end Trace;

   function Trace
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path;
               Iter  : Gtk_Tree_Iter;
               Mode  : Traced_Actions
            )  return Boolean is
      use Ada.Text_IO;

      function Image (Kind : GInt) return String is
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

      Depth : constant Natural := Get_Indices (Path)'Length - 1;
      Text  : constant String  := String (Get_Name (Model, Iter)) &
                                  " - " &
                                  Image (Get_Int (Model, Iter, 2));
   begin
      if (  Trace_To_Output_Only
         /= (Mode and (Trace_To_Both or Trace_To_Output_Only))
         )
      then
         Trace (Depth * "  " & Text);
      end if;
      if (  0
         /= (Mode and (Trace_To_Both or Trace_To_Output_Only))
         )
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
             (  Model   : not null access
                          Gtk_Root_Tree_Model_Record'Class;
                Tracing : Traced_Actions := Trace_To_Both
             )  is
      use Traverse;
   begin
      Foreach (To_Interface (Model), Trace'Access, Tracing);
   end Trace;

end Gtk.Abstract_Browser;
