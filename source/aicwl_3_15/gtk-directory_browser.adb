--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Directory_Browser                       Luebeck            --
--  Implementation                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with GIO.Content_Type;           use GIO.Content_Type;
with GIO.Volume_Monitor;         use GIO.Volume_Monitor;
with GLib.Error;                 use GLib.Error;
with Glib.Messages;              use Glib.Messages;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Strings_Edit.UTF8;          use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Mapping;  use Strings_Edit.UTF8.Mapping;

with Ada.Unchecked_Deallocation;
with GLib.Object.Checked_Destroy;

package body Gtk.Directory_Browser is
   use Gtk.Missed;
   use Gtk.Widget;

   Unreadable : constant String := "emblem-unreadable";

   type Case_Type is (Sensitive, Insensitive);
   Mode : Case_Type := Sensitive;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Directory_Browser." & Name;
   end Where;

   procedure Say
             (  Text   : String;
                Parent : not null access Gtk_Widget_Record'Class
             )  is
   begin
      Message_Dialog
      (  Title         => "File renaming error",
         Justification => Justify_Left,
         Message       => Text,
         Parent        => Parent
      );
   end Say;

   function Compare
            (  Store     : not null access Gtk_Directory_Record;
               Directory : Item_Path;
               A, B      : Directory_Item;
               By_Name   : Boolean
            )  return Row_Order is
   begin
      if not By_Name then
         if A.Directory xor B.Directory then
            if A.Directory then
               return Before;
            else
               return After;
            end if;
         else
            if A.Kind /= B.Kind then
               if A.Kind < B.Kind then
                  return Before;
               else
                  return After;
               end if;
            end if;
         end if;
      end if;
      case Mode is
         when Sensitive =>
            if A.Name = B.Name then
               return Equal;
            elsif A.Name < B.Name then
               return Before;
            else
               return After;
            end if;
         when Insensitive =>
            declare
               A_Name  : constant UTF8_String := UTF8_String (A.Name);
               B_Name  : constant UTF8_String := UTF8_String (B.Name);
               A_Code  : UTF8_Code_Point;
               B_Code  : UTF8_Code_Point;
               A_Index : Integer := A.Name'First;
               B_Index : Integer := B.Name'First;
            begin
               while A_Index <= A.Name'Last loop
                  begin
                     Get (A_Name, A_Index, A_Code);
                  exception
                     when Ada.IO_Exceptions.Data_Error =>
                        exit;
                  end;
                  if B_Index > B.Name'Last then
                     return After;
                  end if;
                  begin
                     Get (B_Name, B_Index, B_Code);
                  exception
                     when Ada.IO_Exceptions.Data_Error =>
                        return After;
                  end;
                  A_Code := To_Lowercase (A_Code);
                  B_Code := To_Lowercase (B_Code);
                  if A_Code /= B_Code then
                     if A_Code < B_Code then
                        return Before;
                     else
                        return After;
                     end if;
                  end if;
               end loop;
               if B_Index > B.Name'Last then
                  return Equal;
               else
                  return Before;
               end if;
            end;
      end case;
   end Compare;

   procedure Delete
             (  Store : not null access Gtk_Directory_Record;
                File  : UTF8_String
             )  is
   begin
      Remove (File);
      Deleted (Store, Item_Path (File));
   end Delete;

   procedure Finalize (Reference : in out Browsing_Data_Ref) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Browsing_Data,
                Browsing_Data_Ptr
             );
      Mount : GMount;
   begin
      if Reference.Ptr /= null then
         for Index in 1..GetSize (Reference.Ptr.Roots) loop
            Mount := GetTag (Reference.Ptr.Roots, Index);
            if Mount /= null then
               Unref (Mount);
            end if;
         end loop;
         Free (Reference.Ptr.Folder);
         Free (Reference.Ptr);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   function Filter
            (  Widget    : not null access Gtk_Directory_Browser_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean is
   begin
      return True;
   end Filter;

   function Filter
            (  Widget    : not null access Gtk_Files_View_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean is
   begin
      return Filter (Widget.Browser, Directory, Name, Kind);
   end Filter;

   function Get_Cache
            (  Widget : not null access Gtk_Directory_Browser_Record
            )  return Gtk_Directory is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Directory
            (  Store : not null access Gtk_Directory_Record;
               Item  : Item_Path
            )  return Item_Path is
      Name   : constant UTF8_String := UTF8_String (Item);
      Result : constant UTF8_String := Get_Dirname (Name);
   begin
      if Result = "." or else Result = Name then
         if Result = "." then
            raise Name_Error with
                  "Directory of " & Name & " is pseudo-directory '.'";
         else
            raise Name_Error with
                  "Directory of " & Name & " is itself";
         end if;
      else
         return Item_Path (Result);
      end if;
   end Get_Directory;

   function Get_Files_View
            (  Widget : not null access Gtk_Directory_Browser_Record
            )  return Gtk_Directory_Items_View is
   begin
      return Widget.Items.all'Access;
   end Get_Files_View;

   function Get_Icon
            (  Widget       : not null access Gtk_Folders_View_Record;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Icon_Data is
      This : constant String := String (Kind);
   begin
      if Topmost then
         declare
            Roots : Table renames
                       Gtk_Directory_Record'Class
                       (  Get_Cache (Widget).all
                       ) .Data.Ptr.Roots;
            Index : constant Natural := Locate (Roots, String (Kind));
         begin
            if Index = 0 or else GetTag (Roots, Index) = null then
               return
               (  Stock_ID,
                  Stock_Harddisk'Length,
                  Stock_Harddisk
               );
            else
               return (GIcon, 0, Get_Icon (GetTag (Roots, Index)));
            end if;
         end;
      elsif not Has_Children then
         return (Themed, Unreadable'Length, Unreadable);
      elsif This = Stock_Directory then
         if Expanded then
            return (Stock_ID, Stock_Open'Length, Stock_Open);
         else
            return (Stock_ID, Stock_Directory'Length, Stock_Directory);
         end if;
      end if;
      return (GIcon, 0, Get_Icon (This));
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Icon (tree)")
         )  );
         return (Stock_ID, Stock_Cancel'Length, Stock_Cancel);
   end Get_Icon;

   function Get_Icon
            (  Widget       : not null access Gtk_Files_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Directory    : Boolean;
               Has_Children : Boolean
            )  return Icon_Data is
   begin
      if Name'Length = 0 and then Kind'Length = 0 then
         return (Stock_ID, 0, "");
      elsif Directory then
         if Has_Children then
            return (Stock_ID, Stock_Directory'Length, Stock_Directory);
         else
            return (Themed, Unreadable'Length, Unreadable);
         end if;
      elsif Kind = Item_Type (Stock_File) then
         return (Stock_ID, Stock_File'Length, Stock_File);
      else
         return (GIcon, 0, Get_Icon (String (Kind)));
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Icon (files)")
         )  );
         return (Stock_ID, Stock_Cancel'Length, Stock_Cancel);
   end Get_Icon;

   function Get_Name
            (  Store : not null access Gtk_Directory_Record;
               Item  : Item_Path
            )  return Item_Name is
      Name : constant UTF8_String := UTF8_String (Item);
   begin
      if Get_Root (Name) = Name then
         return Item_Name (Name);
      else
         return Item_Name (Get_Basename (Name));
      end if;
   exception
      when Use_Error =>
         return Item_Name (Name);
   end Get_Name;

   function Get_Name
            (  Widget       : not null access Gtk_Folders_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Item_Name is
   begin
      if Topmost then
         declare
            Roots : Table renames
                       Gtk_Directory_Record'Class
                       (  Get_Cache (Widget).all
                       ) .Data.Ptr.Roots;
            Index : constant Natural := Locate (Roots, String (Kind));
         begin
            if Index = 0 or else GetTag (Roots, Index) = null then
               return Item_Name (Kind);
            else
               return Item_Name (Get_Name (GetTag (Roots, Index)));
            end if;
         end;
      else
         return Name;
      end if;
   end Get_Name;

   function Get_Path
            (  Store     : not null access Gtk_Directory_Record;
               Directory : Item_Path;
               Item      : Item_Name
            )  return Item_Path is
      Data : Browsing_Data renames Store.Data.Ptr.all;
   begin
      if Directory'Length = 0 then
         if Is_Root (Store, Item_Path (Item)) then
            return Item_Path (Item);
         else
            return
               Item_Path
               (  Build_Filename
                  (  GetName (Data.Roots, 1),
                     UTF8_String (Item)
               )  );
         end if;
      else
         return
            Item_Path
            (  Build_Filename
               (  UTF8_String (Directory),
                  UTF8_String (Item)
            )  );
      end if;
   end Get_Path;

   function Get_Tracing
            (  Widget : not null access Gtk_Directory_Browser_Record
            )  return Traced_Actions is
   begin
      return Get_Tracing (Widget.Cache);
   end Get_Tracing;

   function Get_Tree_View
            (  Widget : not null access Gtk_Directory_Browser_Record
            )  return Gtk_Directory_Tree_View is
   begin
      return Widget.Tree.all'Unchecked_Access;
   end Get_Tree_View;

   function Get_Root_Directory (File : String) return String is
   begin
      return Get_Root (File);
   exception
      when Use_Error =>
         return File;
   end Get_Root_Directory;

   procedure Gtk_New
             (  Store   : out Gtk_Directory;
                Policy  : Caching_Policy := Cache_Expanded;
                Tracing : Traced_Actions := Trace_Nothing
             )  is
   begin
      Store := new Gtk_Directory_Record;
      begin
         Initialize (Store, Policy, Tracing);
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Directory)")
            )  );
            Unref (Store);
            Store := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget    : out Gtk_Directory_Browser;
                File      : UTF8_String := Get_Current_Dir;
                Columns   : Positive    := 4;
                Vertical  : Boolean     := False;
                Tree_Size : Gtk_Requisition :=
                               (Width => 180, Height => 500);
                List_Size : Gtk_Requisition :=
                               (Width => 600, Height => 500);
                Store     : Gtk_Directory  := null;
                Tracing   : Traced_Actions := Trace_Nothing
             )  is
   begin
      Widget := new Gtk_Directory_Browser_Record;
      begin
         Initialize
         (  Widget,
            File,
            Columns,
            Vertical,
            Tree_Size,
            List_Size,
            Store,
            Tracing
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Directory_Browser)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Store   : not null access Gtk_Directory_Record'Class;
                Policy  : Caching_Policy;
                Tracing : Traced_Actions
             )  is
   begin
      Set_Tracing (Store, Tracing);
      Store.Data.Ptr := new Browsing_Data;
      --
      -- Determining the existing roots
      --
      declare
         use Mount_List;
         Monitor : constant GVolume_Monitor := Get;
         Mounts  : Glist := Get_Mounts (Monitor);
         Item    : Glist := First (Mounts);
         Mount   : GMount;
      begin
         while Item /= Null_List loop
            Mount := Get_Data (Item);
            if Mount /= null then
               declare
                  Path : constant UTF8_String := Get_Root (Mount);
               begin
                  if Get_Root (Path) = Path then
                     begin
                        Add (Store.Data.Ptr.Roots, Path, Mount);
                        if 0 /= (Get_Tracing (Store) and Trace_IO) then
                           Trace
                           (  Store,
                              Get_Depth (Store),
                              "mount " & Path
                           );
                        end if;
                     exception
                        when others =>
                           Unref (Mount);
                     end;
                  else
                     Unref (Mount);
                  end if;
               end;
            end if;
            Item := Next (Item);
         end loop;
         Free (Mounts);
         Unref (Monitor);
      end;
      if GetSize (Store.Data.Ptr.Roots) = 0 then
         if File_Test ("/", File_Test_Exists) then
            if 0 /= (Get_Tracing (Store) and Trace_IO) then
               Trace (Store, Get_Depth (Store), "added /");
            end if;
            Add (Store.Data.Ptr.Roots, "/", null);
         else
            if 0 /= (Get_Tracing (Store) and Trace_IO) then
               Trace (Store, Get_Depth (Store), "assuming Windows");
            end if;
            Mode := Insensitive;
         end if;
      else
         if 0 /= (Get_Tracing (Store) and Trace_IO) then
            Trace (Store, Get_Depth (Store), "assuming Windows");
         end if;
         Mode := Insensitive;
      end if;
      Store.Data.Ptr.Policy := Policy;
      Store.Data.Ptr.Current_Root := 1;
      Gtk.Abstract_Browser.Initialize (Store);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Initialize (store)")
         )  );
         raise;
   end Initialize;

   procedure Initialize
             (  Widget    : not null access
                            Gtk_Directory_Browser_Record'Class;
                File      : UTF8_String;
                Columns   : Positive;
                Vertical  : Boolean;
                Tree_Size : Gtk_Requisition;
                List_Size : Gtk_Requisition;
                Store     : Gtk_Directory;
                Tracing   : Traced_Actions
             )  is
      Scroll : Gtk_Scrolled_Window;
   begin
      if Vertical then
         Initialize_Vpaned (Widget);
      else
         Initialize_Hpaned (Widget);
      end if;
      if Store = null then
         Gtk_New (Widget.Cache, Tracing => Tracing);
      else
         Widget.Cache := Store;
         Set_Tracing (Widget.Cache, Tracing);
      end if;
      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add1 (Widget, Scroll);
      Widget.Tree := new Gtk_Folders_View_Record;
      Initialize (Widget.Tree, Widget.Cache, Item_Path (File));
      declare
         Dummy  : GInt;
         Height : GInt;
         Width  : GInt;
      begin
         Widget.Tree.Columns_Autosize;
         Widget.Tree.Get_Preferred_Width  (Dummy, Width);
         Widget.Tree.Get_Preferred_Height (Dummy, Height);
         Widget.Tree.Set_Size_Request
         (  GInt'Max (1, GInt'Min (Width,  Tree_Size.Width)),
            GInt'Max (1, GInt'Min (Height, Tree_Size.Height))
         );
      end;
      Add (Scroll, Widget.Tree);
      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add2 (Widget, Scroll);
      Widget.Items := new Gtk_Files_View_Record;
      Widget.Items.Browser := Widget.all'Access;
      Initialize (Widget.Items, Widget.Tree, Columns);
      Add (Scroll, Widget.Items);
      declare
         Size : Gtk_Requisition;
      begin
         Size_Request (Widget.Items, Size); -- Query the integral size
         Set_Size_Request                   -- Set new size
         (  Widget.Items,
            GInt'Max (1, GInt'Min (Size.Width,  List_Size.Width)),
            GInt'Max (1, GInt'Min (Size.Height, List_Size.Height))
         );
      end;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Initialize (widget)")
         )  );
         raise;
   end Initialize;

   function Is_Root
            (  Store : not null access Gtk_Directory_Record;
               Item  : Item_Path
            )  return Boolean is
      Name : constant UTF8_String := String (Item);
   begin
      return Is_Absolute (Name) and then Get_Root (Name) = Name;
   exception
      when Use_Error =>
         return False;
   end Is_Root;

   procedure Name_Commit
             (  Widget   : not null access Gtk_Folders_View_Record;
                Old_Path : Item_Path;
                New_Name : Item_Name
             )  is
      Cache : Gtk_Abstract_Directory renames Get_Cache (Widget);
   begin
      if Get_Name (Cache, Old_Path) /= New_Name then
         declare
            New_Path : constant Item_Path :=
                       Get_Path
                       (  Cache,
                          Get_Directory (Cache, Old_Path),
                          New_Name
                       );
         begin
            Rename (String (Old_Path), String (New_Path));
         exception
            when Name_Error =>
               Say
               (  "Directory " & String (Old_Path) & " does not exist",
                  Widget
               );
               return;
            when Use_Error =>
               Say
               (  (  "Directory "
                  &  String (Old_Path)
                  &  " cannot be renamed to "
                  &  String (New_Path)
                  ),
                  Widget
               );
               return;
            when Error : others =>
               Say
               (  (  "Unable to rename "
                  &  String (Old_Path)
                  &  " to "
                  &  String (New_Path)
                  &  " due to internal error. "
                  &  Exception_Message (Error)
                  ),
                  Widget
               );
               return;
         end;
         Renamed (Cache, Old_Path, New_Name);
      end if;
   exception
      when Name_Error => -- Illegal name
          null;
   end Name_Commit;

   procedure Name_Commit
             (  Widget : not null access Gtk_Files_View_Record;
                Index  : Positive;
                Name   : Item_Name
             )  is
      Item    : Natural := Index;
      Changed : Boolean;
   begin
      if Get_Name (Widget, Index) /= Name then
         begin
            Rename
            (  String (Get_Path (Widget, Index)),
               String (Get_Path (Widget, Name))
            );
         exception
            when Name_Error =>
               Say
               (  (  "File "
                  &  String (Get_Path (Widget, Index))
                  &  " does not exist"
                  ),
                  Widget
               );
               return;
            when Use_Error =>
               Say
               (  (  "File "
                  &  String (Get_Path (Widget, Index))
                  &  " cannot be renamed to "
                  &  String (Get_Path (Widget, Name))
                  ),
                  Widget
               );
               return;
            when Error : others =>
               Say
               (  (  "Unable to rename "
                  &  String (Get_Path (Widget, Index))
                  &  " to "
                  &  String (Get_Path (Widget, Name))
                  &  " due to internal error. "
                  &  Exception_Message (Error)
                  ),
                  Widget
               );
               return;
         end;
         Renamed (Widget, Item, Name);
         if Item > 0 then
            Move (Widget, Changed, 0, Item);
         end if;
      end if;
   end Name_Commit;

   function Read
            (  Store : not null access Gtk_Directory_Record
            )  return Directory_Item is
      This : Browsing_Data renames Store.Data.Ptr.all;
   begin
      case This.Mode is
         when None =>
            raise End_Error;
         when Root =>
            if This.Current_Root > GetSize (This.Roots) then
               This.Mode := None;
               raise End_Error;
            else
               declare
                  Name   : constant UTF8_String :=
                              GetName (This.Roots, This.Current_Root);
                  Result : constant Directory_Item :=
                           (  Directory   => True,
                              Policy      => This.Policy,
                              Name_Length => Name'Length,
                              Kind_Length => Name'Length,
                              Name        => Item_Name (Name),
                              Kind        => Item_Type (Name)
                           );
               begin
                  This.Current_Root := This.Current_Root + 1;
                  if 0 /= (Get_Tracing (Store) and Trace_IO) then
                     Trace
                     (  Store,
                        Get_Depth (Store),
                        "read root " & Name
                     );
                  end if;
                  return Result;
               end;
            end if;
         when Directory =>
            null;
      end case;
      begin
         loop
            declare
               Name   : constant UTF8_String :=
                        Dir_Read_Name (This.Search);
               Status : GFileTest;
            begin
               if Name = "." or else Name = ".." then
                  if 0 /= (Get_Tracing (Store) and Trace_IO) then
                     Trace
                     (  Store,
                        Get_Depth (Store),
                        "read ignored " & Name
                     );
                  end if;
               else
                  declare
                     Path : constant UTF8_String :=
                            Build_Filename
                            (  This.Folder (1..This.Length),
                               Name
                            );
                  begin
                     Status := File_Test (Path);
                     if 0 = (Status and File_Test_Exists) then
                        if 0 /= (Get_Tracing (Store) and Trace_IO) then
                           Trace
                           (  Store,
                              Get_Depth (Store),
                              "read non-existing " & Name
                           );
                        end if;
                     elsif 0 /= (Status and File_Test_Is_Dir) then
                        if 0 /= (Get_Tracing (Store) and Trace_IO) then
                           Trace
                           (  Store,
                              Get_Depth (Store),
                              "read directory " & Name
                           );
                        end if;
                        return
                        (  Directory   => True,
                           Policy      => This.Policy,
                           Name_Length => Name'Length,
                           Kind_Length => Stock_Directory'Length,
                           Name        => Item_Name (Name),
                           Kind        => Item_Type (Stock_Directory)
                        );
                     else
                        declare
                           Content : constant UTF8_String :=
                                              Guess (Path);
                        begin
                           if Content'Length = 0 then
                              if (  0
                                 /= (Get_Tracing (Store) and Trace_IO)
                                 )
                              then
                                 Trace
                                 (  Store,
                                    Get_Depth (Store),
                                    "read file " & Name & " (?)"
                                 );
                              end if;
                              return
                              (  Directory   => False,
                                 Policy      => This.Policy,
                                 Name_Length => Name'Length,
                                 Kind_Length => Stock_File'Length,
                                 Name        => Item_Name (Name),
                                 Kind        => Item_Type (Stock_File)
                              );
                           else
                              if (  0
                                 /= (Get_Tracing (Store) and Trace_IO)
                                 )
                              then
                                 Trace
                                 (  Store,
                                    Get_Depth (Store),
                                    (  "read file "
                                    &  Name
                                    &  " ("
                                    &  Content
                                    &  ')'
                                 )  );
                              end if;
                              return
                              (  Directory   => False,
                                 Policy      => This.Policy,
                                 Name_Length => Name'Length,
                                 Kind_Length => Content'Length,
                                 Name        => Item_Name (Name),
                                 Kind        => Item_Type (Content)
                              );
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;
      exception
         when End_Error =>
            Dir_Close (This.Search);
         when Error : others =>
            Dir_Close (This.Search);
            raise Ada.IO_Exceptions.Data_Error with
                  Exception_Message (Error);
      end;
      if 0 /= (Get_Tracing (Store) and Trace_IO) then
         Trace (Store, Get_Depth (Store), "read completed");
      end if;
      This.Mode := None;
      raise End_Error;
   exception
      when End_Error | Data_Error =>
         raise;
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Rewind")
         )  );
         raise;
   end Read;

   function Rewind
            (  Store  : not null access Gtk_Directory_Record;
               Folder : Item_Path
            )  return Directory_Item is
      This  : Browsing_Data renames Store.Data.Ptr.all;
      Error : GError;
   begin
      case This.Mode is
         when None =>
            null;
         when Root =>
            if 0 /= (Get_Tracing (Store) and Trace_IO) then
               Trace (Store, Get_Depth (Store), "end search <root>");
            end if;
         when Directory =>
            if 0 /= (Get_Tracing (Store) and Trace_IO) then
               Trace (Store, Get_Depth (Store), "end search");
            end if;
            Dir_Close (This.Search);
      end case;
      if Folder'Length = 0 then
         This.Mode := Root;
         This.Current_Root := 1;
         if 0 /= (Get_Tracing (Store) and Trace_IO) then
            Trace (Store, Get_Depth (Store), "search <root>");
         end if;
      else
         if 0 /= (Get_Tracing (Store) and Trace_IO) then
            Trace
            (  Store,
               Get_Depth (Store),
               "search " & String (Folder)
            );
         end if;
         This.Mode := None;
         if This.Search = null then
            Dir_Close (This.Search);
         end if;
         if (  This.Folder = null
            or else
               Folder'Length > This.Folder'Length
            )
         then
            Free (This.Folder);
            This.Folder := new String (1..Folder'Length);
         end if;
         This.Folder (1..Folder'Length) := String (Folder);
         This.Length := Folder'Length;
         Dir_Open (String (Folder), This.Search, Error);
         if Error = null then
            This.Mode := Directory;
         else
            declare
               Message : constant String := Get_Message (Error);
            begin
               Error_Free (Error);
               raise Data_Error with Message;
            end;
         end if;
      end if;
      return Directory_Entered;
   exception
      when End_Error | Data_Error =>
         raise;
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Rewind")
         )  );
         raise;
   end Rewind;

   procedure Set_Tracing
             (  Widget  : not null access Gtk_Directory_Browser_Record;
                Tracing : Traced_Actions
             )  is
   begin
      Set_Tracing (Widget.Cache, Tracing);
   end Set_Tracing;

end Gtk.Directory_Browser;
