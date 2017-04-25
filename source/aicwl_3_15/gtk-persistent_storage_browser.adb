--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Persistent_Storage_Browser              Luebeck            --
--  Implementation                                 Winter, 2008       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with GLib.Error;                use GLib.Error;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties;           use GLib.Properties;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Interfaces;                use Interfaces;
with Strings_Edit;              use Strings_Edit;

with Strings_Edit.Symmetric_Serialization;
use  Strings_Edit.Symmetric_Serialization;

with Deposit_Handles;
with GLib.Object.Checked_Destroy;
with Persistent.Directory;

package body Gtk.Persistent_Storage_Browser is
   use Deposit_Handles;
   use Persistent.Catalogue;

   Database_Closed : String renames Stock_Disconnect;
   Database_Open   : String renames Stock_Connect;
   Unknown         : String renames Stock_Dialog_Error;

   Persistent_Storage_Tree_View_Class_Record :
      aliased Ada_GObject_Class := Uninitialized_Class;
   Persistent_Storage_Items_View_Class_Record :
      aliased Ada_GObject_Class := Uninitialized_Class;

   DSN_URI : constant String := "Persistent storage DSN";

   procedure Store_Item
             (  Manager : Gtk_Recent_Manager;
                Value   : UTF8_String
             );

   function Where (Text : String) return String is
   begin
      return " in Gtk.Persistent_Storage_Browser." & Text;
   end Where;

   procedure Wipe (Text : in out Unbounded_String) is
   begin
      for Index in 1..Length (Text) loop
         Replace_Element (Text, Index, ' ');
      end loop;
   end Wipe;

   procedure Add_Storage
             (  Store   : not null access
  Gtk_Persistent_Directory_Record;
                Storage : out Storage_Handle
             )  is
      Scheme   : Scheme_Type;
      Stored   : Boolean := False;
      Name     : Unbounded_String;
      User     : Unbounded_String;
      Password : Unbounded_String;
      Data     : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
      if not Is_Valid (Data.Query) then
         return;
      end if;
      Create
      (  Ptr (Data.Query).all,
         Scheme,
         Name,
         User,
         Password,
         Stored,
         Storage
      );
      if not Is_Valid (Storage) then
         Wipe (Password);
         return;
      end if;
      declare
         DSN : UTF8_String renames To_String (Name);
      begin
         case Scheme is
            when DSN_Scheme =>
               declare
                  URI : constant String :=
                                 To_DSN_URI
                                 (  DSN,
                                    To_String (User),
                                    To_String (Password),
                                    Stored
                                 );
               begin
                  if IsIn (Data.Root_List, DSN) then
                     --
                     -- When a same named  root  exists  and  refers  to
                     -- another storage, the latter is replaced  by  the
                     -- new one.
                     --
                     declare
                        Item : Root_Item'Class renames
                               Ptr (Find (Data.Root_List, DSN)).all;
                     begin
                        if Ptr (Item.Storage) /= Ptr (Storage) then
                           if Is_Valid (Item.Storage) then
                              Storage := Item.Storage;
                           else
                              Item.Storage := Storage;
                              Changed
                              (  Store,
                                 Get_Path (Store, "", Item_Name (DSN))
                              );
                           end if;
                           Remove_Item (Data.Manager, Item.URI);
                           Item.URI := To_Unbounded_String (URI);
                           Store_Item (Data.Manager, URI);
                        end if;
                     end;
                  else
                     declare
                        Item : constant Root_Item_Handles.Handle :=
                               Ref (new Root_Item);
                     begin
                        Ptr (Item).Storage := Storage;
                        Append (Ptr (Item).URI, URI);
                        Add (Data.Root_List, DSN, Item);
                        Store_Item (Data.Manager, URI);
                        Created
                        (  Store,
                           "",
                           (  Policy      => Cache_Never,
                              Directory   => True,
                              Name_Length => DSN'Length,
                              Kind_Length => Database_Open'Length,
                              Name        => Item_Name (DSN),
                              Kind        => Item_Type (Database_Open)
                        )  );
                     end;
                  end if;
               end;
               Wipe (Password);
            when FDB_Scheme =>
               declare
                  File : UTF8_String renames To_String (User);
                  URI  : constant UTF8_String := To_FDB_URI (DSN, File);
               begin
                  if IsIn (Data.Root_List, DSN) then
                     declare
                        Item : Root_Item'Class renames
                               Ptr (Find (Data.Root_List, DSN)).all;
                     begin
                        if Ptr (Item.Storage) /= Ptr (Storage) then
                           if Is_Valid (Item.Storage) then
                              Storage := Item.Storage;
                           else
                              Item.Storage := Storage;
                              Changed
                              (  Store,
                                 Get_Path (Store, "", Item_Name (DSN))
                              );
                           end if;
                           Remove_Item (Data.Manager, Item.URI);
                           Item.URI := To_Unbounded_String (URI);
                           Store_Item (Data.Manager, URI);
                        end if;
                     end;
                  else
                     declare
                        Item : constant Root_Item_Handles.Handle :=
                               Ref (new Root_Item);
                     begin
                        Ptr (Item).Storage := Storage;
                        Append (Ptr (Item).URI, URI);
                        Add (Data.Root_List, DSN, Item);
                        Store_Item (Data.Manager, URI);
                        Created
                        (  Store,
                           "",
                           (  Policy      => Cache_Never,
                              Directory   => True,
                              Name_Length => DSN'Length,
                              Kind_Length => Database_Open'Length,
                              Name        => Item_Name (DSN),
                              Kind        => Item_Type (Database_Open)
                        )  );
                     end;
                  end if;
               end;
            when SQLite_Scheme =>
               declare
                  File : UTF8_String renames To_String (User);
                  URI  : constant UTF8_String :=
                                  To_SQLite_URI (DSN, File);
               begin
                  if IsIn (Data.Root_List, DSN) then
                     declare
                        Item : Root_Item'Class renames
                               Ptr (Find (Data.Root_List, DSN)).all;
                     begin
                        if Ptr (Item.Storage) /= Ptr (Storage) then
                           if Is_Valid (Item.Storage) then
                              Storage := Item.Storage;
                           else
                              Item.Storage := Storage;
                              Changed
                              (  Store,
                                 Get_Path (Store, "", Item_Name (DSN))
                              );
                           end if;
                           Remove_Item (Data.Manager, Item.URI);
                           Item.URI := To_Unbounded_String (URI);
                           Store_Item (Data.Manager, URI);
                        end if;
                     end;
                  else
                     declare
                        Item : constant Root_Item_Handles.Handle :=
                               Ref (new Root_Item);
                     begin
                        Ptr (Item).Storage := Storage;
                        Append (Ptr (Item).URI, URI);
                        Add (Data.Root_List, DSN, Item);
                        Store_Item (Data.Manager, URI);
                        Created
                        (  Store,
                           "",
                           (  Policy      => Cache_Never,
                              Directory   => True,
                              Name_Length => DSN'Length,
                              Kind_Length => Database_Open'Length,
                              Name        => Item_Name (DSN),
                              Kind        => Item_Type (Database_Open)
                        )  );
                     end;
                  end if;
               end;
         end case;
      end;
   exception
      when others =>
         Wipe (Password);
         raise;
   end Add_Storage;

   procedure Browse
             (  Store   : not null access
                          Gtk_Persistent_Directory_Record;
                Path    : Item_Path;
                Storage : out Storage_Handle;
                Object  : out Deposit_Handle;
                Partial : Boolean := False
             )  is
      Pointer : Integer := Path'First;
      Length  : Natural;
      Start   : Integer;
      Data    : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
      Scan (Path, Pointer, Length, Path_Delimiters);
      if Length = 0 then
         raise Name_Error with "Empty path";
      end if;
      Storage :=
         Ptr
         (  Find
            (  Data.Root_List,
               String
               (  From_Escaped (Path (Path'First..Pointer - 1), Length)
         )  )  ) .Storage;
      Invalidate (Object);
      if Is_Valid (Storage) then
         while Pointer <= Path'Last loop
            Pointer := Pointer + 1;
            Start   := Pointer;
            Scan (Path, Pointer, Length, Path_Delimiters);
            if Length = 0 then
               raise Name_Error with "A name in the path is empty";
            end if;
            begin
               Object :=
                  Get
                  (  Storage,
                     String
                     (  From_Escaped (Path (Start..Pointer - 1), Length)
                     ),
                     Object
                  );
            exception
               when End_Error =>
                  if Partial then
                     return;
                  else
                     raise;
                  end if;
            end;
         end loop;
      end if;
   exception
      when End_Error =>
         if not Partial then
            raise;
         end if;
         Invalidate (Storage);
         Invalidate (Object);
      when Error : Constraint_Error =>
         raise Ada.IO_Exceptions.End_Error with
               Exception_Message (Error);
   end Browse;

   function Compare
            (  Store     : not null access
                           Gtk_Persistent_Directory_Record;
               Directory : Item_Path;
               A, B      : Directory_Item;
               By_Name   : Boolean
            )  return Row_Order is
   begin
      if not By_Name and then (A.Directory xor B.Directory) then
         if A.Directory then
            return Before;
         else
            return After;
         end if;
      end if;
      if A.Name = B.Name then
         return Equal;
      elsif A.Name < B.Name then
         return Before;
      else
         return After;
      end if;
   end Compare;

   procedure Delete
             (  Store : not null access
                        Gtk_Persistent_Directory_Record;
                Path  : Item_Path
             )  is
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
      Data    : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
      if Is_Root (Path) then
         -- Deleting a persistent storage
         declare
            Name : constant UTF8_String := String (Get_Name (Path));
         begin
            Remove_Item
            (  Data.Manager,
               Ptr (Find (Data.Root_List, Name)).URI
            );
            Delete (Data.Root_List, Name);
         end;
      else
         -- Deleting an object
         declare
            Directory : constant Item_Path := Get_Directory (Store, Path);
         begin
            Browse (Store, Directory, Storage, Object);
            Unname
            (  Storage,
               String (Get_Name (Store, Path)),
               Object
            );
         end;
      end if;
      Deleted (Store, Path);
   exception
      when End_Error =>
         null;
   end Delete;

   function Filter
            (  Widget    : not null access
                           Gtk_Persistent_Storage_Browser_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean is
   begin
      return True;
   end Filter;

   function Filter
            (  Widget    : not null access
                           Gtk_Persistent_Storage_Objects_View_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean is
   begin
      return Filter (Widget.Browser, Directory, Name, Kind);
   end Filter;

   procedure Finalize (Data : in out Directory_Data) is
   begin
      if Data.Manager /= null then
         Unref (Data.Manager);
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

   function From_Escaped (Name : Item_Path; Length : Natural)
      return Item_Name is
      Result  : Item_Name (1..Length);
      Pointer : Integer := Name'First;
   begin
      for Index in Result'Range loop
         if Name (Pointer) = Escape then
            Pointer := Pointer + 1;
         end if;
         Result (Index) := Name (Pointer);
         Pointer := Pointer + 1;
      end loop;
      return Result;
   end From_Escaped;

   function Get_Cache
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Persistent_Directory is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Class
            (  Store : not null access
                       Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return String is
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
      Pointer : Integer := Path'First;
      Length  : Natural;
      Start   : Integer;
      Data    : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
      Scan (Path, Pointer, Length, Path_Delimiters);
      if Length = 0 then
         raise Name_Error with "Empty path";
      end if;
      Object := Root_Directory;
      Storage :=
         Ptr
         (  Find
            (  Data.Root_List,
               String
               (  From_Escaped (Path (Path'First..Pointer - 1), Length)
         )  )  ) .Storage;
      if not Is_Valid (Storage) then
         raise Name_Error with "Invalid persistent storage";
      end if;
      loop
         Pointer := Pointer + 1;
         Start   := Pointer;
         Scan (Path, Pointer, Length, Path_Delimiters);
         if Length = 0 then
            raise Name_Error with "A name in the path is empty";
         end if;
         if Pointer > Path'Last then
            return
               Get_Class
               (  Storage,
                  String
                  (  From_Escaped
                     (  Path (Start..Pointer - 1),
                        Length
                  )  ),
                  Object
               );
         end if;
         Object :=
            Get
            (  Storage,
               String
               (  From_Escaped (Path (Start..Pointer - 1), Length)
               ),
               Object
            );
      end loop;
   exception
      when Error : Constraint_Error =>
         raise Ada.IO_Exceptions.End_Error with
               Exception_Message (Error);
      when Layout_Error =>
         raise Name_Error with "Empty object name path";
   end Get_Class;

   function Get_Creation_Time
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return Time is
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
      Pointer : Integer := Path'First;
      Length  : Natural;
      Start   : Integer;
      Data    : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
      Scan (Path, Pointer, Length, Path_Delimiters);
      if Length = 0 then
         raise Name_Error with "Empty path";
      end if;
      Object := Root_Directory;
      Storage :=
         Ptr
         (  Find
            (  Data.Root_List,
               String
               (  From_Escaped (Path (Path'First..Pointer - 1), Length)
         )  )  ) .Storage;
      if not Is_Valid (Storage) then
         raise Name_Error with "Invalid persistent storage";
      end if;
      loop
         Pointer := Pointer + 1;
         Start   := Pointer;
         Scan (Path, Pointer, Length, Path_Delimiters);
         if Length = 0 then
            raise Name_Error with "A name in the path is empty";
         end if;
         if Pointer > Path'Last then
            return
               Get_Creation_Time
               (  Storage,
                  String
                  (  From_Escaped
                     (  Path (Start..Pointer - 1),
                        Length
                  )  ),
                  Object
               );
         end if;
         Object :=
            Get
            (  Storage,
               String
               (  From_Escaped (Path (Start..Pointer - 1), Length)
               ),
               Object
            );
      end loop;
   exception
      when Error : Constraint_Error =>
         raise Ada.IO_Exceptions.End_Error with
               Exception_Message (Error);
      when Layout_Error =>
         raise Name_Error with "Empty object name path";
   end Get_Creation_Time;

   procedure Get_Credentials
             (  URI          : UTF8_String;
                User         : out Unbounded_String;
                Password     : out Unbounded_String;
                Has_Password : out Boolean
             )  is
      Path    : constant Item_Path := Item_Path (URI);
      Pointer : Integer;
      Start   : Integer;
      Length  : Natural := 0;
   begin
      User         := Null_Unbounded_String;
      Password     := Null_Unbounded_String;
      Has_Password := False;
      if Is_Prefix (DSN_URI_Scheme, URI) then
         Pointer := Path'First + DSN_URI_Scheme'Length;
         Start   := Pointer;
         Scan (Path, Pointer, Length, Delimiters);
         if (  Pointer > Path'Length
            or else
               Path (Pointer) /= '@'
            or else
               Pointer - Start /= Encoded_String'Length
            )
         then
            return;
         end if;
         declare
            Data : constant String :=
                      Decode
                      (  UTF8_String (Path (Start..Pointer - 1)),
                         Get_Application_Name
                      );
            User_Length : constant Natural := Character'Pos (Data (1));
            Pass_Length : constant Natural := Character'Pos (Data (2));
         begin
            if Pass_Length = 255 then
               if User_Length > 254 then
                  return;
               end if;
               User := To_Unbounded_String (Data (3..2 + User_Length));
            else
               if User_Length + Pass_Length > 254 then
                  return;
               end if;
               Has_Password := True;
               User := To_Unbounded_String (Data (3..2 + User_Length));
               Password :=
                  To_Unbounded_String
                  (  Data
                     (  3 + User_Length
                     .. 2 + User_Length + Pass_Length
                  )  );
            end if;
         end;
      elsif Is_Prefix (SQLite_URI_Scheme & '@', URI) then
         Pointer := Path'First + SQLite_URI_Scheme'Length + 1;
         Scan (Path, Pointer, Length, Delimiters);
         if Pointer > Path'Length or else Path (Pointer) /= '/' then
            return;
         end if;
         Pointer := Pointer + 1;
         Start   := Pointer;
         Scan (Path, Pointer, Length, Null_Set);
         Append
         (  User,
            UTF8_String
            (  From_Escaped (Path (Start..Pointer - 1), Length)
         )  );
      elsif Is_Prefix (FDB_URI_Scheme & '@', URI) then
         Pointer := Path'First + FDB_URI_Scheme'Length + 1;
         Scan (Path, Pointer, Length, Delimiters);
         if Pointer > Path'Length or else Path (Pointer) /= '/' then
            return;
         end if;
         Pointer := Pointer + 1;
         Start   := Pointer;
         Scan (Path, Pointer, Length, Null_Set);
         Append
         (  User,
            UTF8_String
            (  From_Escaped (Path (Start..Pointer - 1), Length)
         )  );
      end if;
   end Get_Credentials;

   function Get_Current_Object
            (  Widget : not null access
                        Gtk_Persistent_Storage_Tree_View_Record
            )  return Deposit_Handle is
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
   begin
      Browse
      (  Get_Directory_Cache (Widget),
         Get_Current_Directory (Widget),
         Storage,
         Object
      );
      return Object;
   exception
      when Constraint_Error | Name_Error | End_Error =>
         return Object;
   end Get_Current_Object;

   function Get_Current_Storage
            (  Widget : not null access
                        Gtk_Persistent_Storage_Tree_View_Record
            )  return Storage_Handle is
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
   begin
      Browse
      (  Get_Directory_Cache (Widget),
         Get_Current_Directory (Widget),
         Storage,
         Object
      );
      return Storage;
   exception
      when Constraint_Error | Name_Error | End_Error =>
         return Storage;
   end Get_Current_Storage;

   function Get_Directory (Path : Item_Path) return Item_Path is
      Pointer : Integer := Path'Last + 1;
      Length  : Natural;
   begin
      Scan_Backwards (Path, Pointer, Length);
      if Length = 0 or else Pointer < Path'First + 2 then
         raise Name_Error;
      end if;
      return Path (Path'First..Pointer - 2);
   end Get_Directory;

   function Get_Directory
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Item  : Item_Path
            )  return Item_Path is
   begin
      return Get_Directory (Item);
   end Get_Directory;

   function Get_Directory_Cache
            (  Widget : not null access
                        Gtk_Persistent_Storage_Tree_View_Record
            )  return Gtk_Persistent_Directory is
      Cache : constant Gtk_Abstract_Directory := Get_Cache (Widget);
   begin
      return
         Gtk_Persistent_Directory_Record'Class
         (  Cache.all
         ) 'Unchecked_Access;
   end Get_Directory_Cache;

   function Get_Directory_Cache
            (  Widget : not null access
                        Gtk_Persistent_Storage_Items_View_Record
            )  return Gtk_Persistent_Directory is
      Cache : constant Gtk_Abstract_Directory := Get_Cache (Widget);
   begin
      return
         Gtk_Persistent_Directory_Record'Class
         (  Cache.all
         ) 'Unchecked_Access;
   end Get_Directory_Cache;

   function Get_Directory_Object
            (  Widget : not null access
                        Gtk_Persistent_Storage_Items_View_Record
            )  return Deposit_Handle is
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
   begin
      Browse
      (  Get_Directory_Cache (Widget),
         Get_Directory (Widget),
         Storage,
         Object
      );
      return Object;
   exception
      when Constraint_Error | Name_Error | End_Error =>
         return Object;
   end Get_Directory_Object;

   function Get_DSN (URI : UTF8_String) return Item_Name is
      function Get_Path return Item_Path is
      begin
         if Is_Prefix (DSN_URI_Scheme, URI) then
            return
               Item_Path
               (  URI
                  (  URI'First + DSN_URI_Scheme'Length
                  .. URI'Last
               )  );
         elsif Is_Prefix (SQLite_URI_Scheme, URI) then
            return
               Item_Path
               (  URI
                  (  URI'First + SQLite_URI_Scheme'Length
                  .. URI'Last
               )  );
         elsif Is_Prefix (FDB_URI_Scheme, URI) then
            return
               Item_Path
               (  URI
                  (  URI'First + FDB_URI_Scheme'Length
                  .. URI'Last
               )  );
         else
            return "";
         end if;
      end Get_Path;
      Path    : constant Item_Path := Get_Path;
      Pointer : Integer   := Path'First;
      Start   : Integer;
      Length  : Natural;
   begin
      Scan (Path, Pointer, Length, Delimiters);
      if Pointer > Path'Last or else Path (Pointer) /= '@' then
         return "";
      end if;
      Pointer := Pointer + 1;
      Start   := Pointer;
      Scan (Path, Pointer, Length, Path_Delimiters);
      if Length = 0 then
         return "";
      end if;
      return From_Escaped (Path (Start..Pointer - 1), Length);
   end Get_DSN;

   function Get_DSN
            (  Store   : not null access
                         Gtk_Persistent_Directory_Record;
               Storage : Storage_Handle
            )  return Item_Name is
      Root_List : Table renames Ptr (Store.Data).Root_List;
   begin
      for Index in 1..GetSize (Root_List) loop
         declare
            Item : Root_Item'Class renames
                       Ptr (GetTag (Root_List, Index)).all;
         begin
            if Item.Storage = Storage then
               return Get_DSN (To_String (Item.URI));
            end if;
         end;
      end loop;
      raise Constraint_Error with "Unmapped storage";
   end Get_DSN;

   function Get_Icon
            (  Widget : not null access
                        Gtk_Persistent_Storage_Tree_View_Record;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Icon_Data is
      This : constant String := String (Kind);
   begin
      if This = Persistent.Directory.Directory_Class then
         if Expanded then
            return (Stock_ID, Stock_Open'Length, Stock_Open);
         else
            return (Stock_ID, Stock_Directory'Length, Stock_Directory);
         end if;
      else
         return (Stock_ID, This'Length, This);
      end if;
   end Get_Icon;

   function Get_Icon
            (  Widget : not null access
                        Gtk_Persistent_Storage_Items_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Directory    : Boolean;
               Has_Children : Boolean
            )  return Icon_Data is
   begin
      if Kind'Length = 0 then
         return (Stock_ID, 0, "");
      elsif Directory then
         declare
            This : constant String := String (Kind);
         begin
            if This = Persistent.Directory.Directory_Class then
               return
               (  Stock_ID,
                  Stock_Directory'Length,
                  Stock_Directory
               );
            else
               return (Stock_ID, This'Length, This);
            end if;
         end;
      else
         declare
            This : constant String := String (Kind);
         begin
            return (Stock_ID, This'Length, This);
         end;
      end if;
   end Get_Icon;

   function Get_Items_View
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Persistent_Storage_Items_View is
   begin
      return Widget.Items.all'Access;
   end Get_Items_View;

   function Get_Manager
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Recent_Manager is
   begin
      return Ptr (Widget.Cache.Data).Manager;
   end Get_Manager;

   function Get_Manager
            (  Store : not null access Gtk_Persistent_Directory_Record
            )  return Gtk_Recent_Manager is
   begin
      return Ptr (Store.Data).Manager;
   end Get_Manager;

   function Get_Name (Path : Item_Path) return Item_Name is
      Pointer : Integer := Path'Last + 1;
      Length  : Natural;
   begin
      Scan_Backwards (Path, Pointer, Length);
      if Length = 0 then
         raise Name_Error;
      end if;
      return From_Escaped (Path (Pointer..Path'Last), Length);
   end Get_Name;

   function Get_Name
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Item  : Item_Path
            )  return Item_Name is
   begin
      return Get_Name (Item);
   end Get_Name;

   function Get_Path
            (  Store     : not null access
                           Gtk_Persistent_Directory_Record;
               Directory : Item_Path;
               Item      : Item_Name
            )  return Item_Path is
   begin
      return Directory & Item;
   end Get_Path;

   function Get_Path
            (  Store   : not null access
                         Gtk_Persistent_Directory_Record;
               Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Item_Path is
      use Deposit_Handles;
      function Up (Object : Deposit_Handle) return Item_Path is
      begin
         if Is_Valid (Object) then
            return
               Get_Path
               (  Store,
                  Up (Get_Parent (Storage, Object)),
                  Item_Name (Get_Name (Storage, Object))
               );
         else
            return Item_Path (Get_DSN (Store, Storage));
         end if;
      end Up;
   begin
      return Up (Object);
   end Get_Path;

   function Get_Persistent_Storage_Items_View_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor =>
               Get_Directory_Items_View_Type,
            Class_Record =>
               Persistent_Storage_Items_View_Class_Record'Access,
            Type_Name =>
               Persistent_Storage_Items_View_Class_Name
         )
      then
         Install (Persistent_Storage_Items_View_Class_Record);
      end if;
      return Persistent_Storage_Items_View_Class_Record.The_Type;
   end Get_Persistent_Storage_Items_View_Type;

   function Get_Persistent_Storage_Tree_View_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor =>
               Get_Directory_Tree_View_Type,
            Class_Record =>
               Persistent_Storage_Tree_View_Class_Record'Access,
            Type_Name =>
               Persistent_Storage_Tree_View_Class_Name
         )
      then
         Install (Persistent_Storage_Tree_View_Class_Record);
      end if;
      return Persistent_Storage_Tree_View_Class_Record.The_Type;
   end Get_Persistent_Storage_Tree_View_Type;

   function Get_Query
            (  Store : not null access Gtk_Persistent_Directory_Record
            )  return Query_Handles.Handle is
   begin
      return Ptr (Store.Data).Query;
   end Get_Query;

   function Get_Scheme (URI : UTF8_String) return Scheme_Type is
   begin
      if Is_Prefix (DSN_URI_Scheme, URI) then
         return DSN_Scheme;
      elsif Is_Prefix (SQLite_URI_Scheme, URI) then
         return SQLite_Scheme;
      elsif Is_Prefix (FDB_URI_Scheme, URI) then
         return FDB_Scheme;
      else
         raise Constraint_Error;
      end if;
   end Get_Scheme;

   function Get_Storage
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return Storage_Handle is
      Pointer : Integer := Path'First;
      Length  : Natural;
      Data    : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
      Scan (Path, Pointer, Length, Path_Delimiters);
      if Length = 0 then
         raise Name_Error;
      end if;
      return
         Ptr
         (  Find
            (  Data.Root_List,
               String
               (  From_Escaped
                  (  Path (Path'First..Pointer - 1),
                     Length
               )  )
         )  ) .Storage;
   end Get_Storage;

   function Get_Storage
            (  Widget : not null access
                        Gtk_Persistent_Storage_Items_View_Record
            )  return Storage_Handle is
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
   begin
      Browse
      (  Get_Directory_Cache (Widget),
         Get_Directory (Widget),
         Storage,
         Object
      );
      return Storage;
   exception
      when Constraint_Error | Name_Error | End_Error =>
         return Storage;
   end Get_Storage;

   function Get_Tracing
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Traced_Actions is
   begin
      return Get_Tracing (Widget.Cache);
   end Get_Tracing;

   function Get_Tree_View
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Persistent_Storage_Tree_View is
   begin
      return Widget.Tree;
   end Get_Tree_View;

   procedure Gtk_New
             (  Store   : out Gtk_Persistent_Directory;
                Query   : Query_Handles.Handle;
                Manager : not null access
                          Gtk_Recent_Manager_Record'Class :=
                             Get_Default;
                Tracing : Traced_Actions := Trace_Nothing
             )  is
   begin
      Store := new Gtk_Persistent_Directory_Record;
      begin
         Initialize (Store, Query, Manager, Tracing);
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Persistent_Directory)")
            )  );
            Unref (Store);
            Store := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget   : out Gtk_Persistent_Storage_Tree_View;
                Store    : not null access
                           Gtk_Persistent_Directory_Record'Class;
                Selected : Item_Path := ""
             )  is
   begin
      Widget := new Gtk_Persistent_Storage_Tree_View_Record;
      begin
         Gtk.Persistent_Storage_Browser.Initialize
         (  Widget,
            Store,
            Selected
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Persistent_Storage_Tree_View)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Persistent_Storage_Items_View;
                Store   : not null access
                          Gtk_Persistent_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path := ""
             )  is
   begin
      Widget := new Gtk_Persistent_Storage_Items_View_Record;
      begin
         Gtk.Persistent_Storage_Browser.Initialize
         (  Widget,
            Store,
            Columns,
            Current
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Persistent_Storage_Items_View)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Persistent_Storage_Items_View;
                Tree    : not null access
                          Gtk_Persistent_Storage_Tree_View_Record'Class;
                Columns : Positive
             )  is
   begin
      Widget := new Gtk_Persistent_Storage_Items_View_Record;
      begin
         Gtk.Persistent_Storage_Browser.Initialize
         (  Widget,
            Tree,
            Columns
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Persistent_Storage_Items_View)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget    : out Gtk_Persistent_Storage_Browser;
                Query     : Query_Handles.Handle;
                Path      : Item_Path := "";
                Columns   : Positive  := 4;
                Vertical  : Boolean   := False;
                Tree_Size : Gtk_Requisition :=
                               (Width => 180, Height => 500);
                List_Size : Gtk_Requisition :=
                               (Width => 600, Height => 500);
                Store     : Gtk_Persistent_Directory := null;
                Manager   : not null access
                            Gtk_Recent_Manager_Record'Class :=
                               Get_Default;
                Tracing   : Traced_Actions := Trace_Nothing
             )  is
   begin
      Widget := new Gtk_Persistent_Storage_Browser_Record;
      begin
         Initialize
         (  Widget,
            Query,
            Path,
            Columns,
            Vertical,
            Tree_Size,
            List_Size,
            Store,
            Manager,
            Tracing
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Persistent_Storage_Browser)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Store   : not null access
                          Gtk_Persistent_Directory_Record'Class;
                Query   : Query_Handles.Handle;
                Manager : not null access
                          Gtk_Recent_Manager_Record'Class;
                Tracing : Traced_Actions
             )  is
      Data : constant Directory_Data_Ptr := new Directory_Data;
   begin
      Directory_Data_Handles.Set (Store.Data, Data);
      Data.Query   := Query;
      Data.Manager := Manager.all'Access;
      Ref (Manager);
      Gtk.Abstract_Browser.Initialize (Store);
      Set_Tracing (Store, Tracing);
   end Initialize;

   procedure Install (Class : Ada_GObject_Class) is
   begin
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "message-title",
            Nick    => "Message dialog title",
            Blurb   => "The message dialog title",
            Default => "Error"
      )  );
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "name-conflict-error",
            Nick    => "Already exists",
            Blurb   => "The message when name is in use",
            Default => "There already exists an object with this name."
      )  );
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "null-renaming-error",
            Nick    => "Empty name",
            Blurb   => "The message when name is empty",
            Default => "No empty names are allowed."
      )  );
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "root-renaming-error",
            Nick    => "Root renaming",
            Blurb   => "The message when a data source renamed",
            Default =>
               (  "The name of a data source cannot be changed. "
               &  "Please use ODBC tools available on your "
               &  "platform in order to rename the data source."
      )  )     );
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "storage-error",
            Nick    => "Data error",
            Blurb   => "The message on a persistent storage fault",
            Default => "Persistent storage fault:"
      )  );
   end Install;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Persistent_Storage_Tree_View_Record'Class;
                Store  : not null access
                         Gtk_Persistent_Directory_Record'Class;
                Selected : Item_Path
             )  is
   begin
      G_New (Widget, Get_Persistent_Storage_Tree_View_Type);
      Gtk.Abstract_Browser.Initialize (Widget, Store, Selected);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record'Class;
                Store  : not null access
                         Gtk_Persistent_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path
             )  is
   begin
      G_New (Widget, Get_Persistent_Storage_Items_View_Type);
      Gtk.Abstract_Browser.Initialize (Widget, Store, Columns, Current);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record'Class;
                Tree   : not null access
                         Gtk_Persistent_Storage_Tree_View_Record'Class;
                Columns : Positive
             )  is
   begin
      G_New (Widget, Get_Persistent_Storage_Items_View_Type);
      Gtk.Abstract_Browser.Initialize (Widget, Tree, Columns);
   end Initialize;

   procedure Initialize
             (  Widget    : not null access
                            Gtk_Persistent_Storage_Browser_Record'Class;
                Query     : Query_Handles.Handle;
                Path      : Item_Path;
                Columns   : Positive;
                Vertical  : Boolean;
                Tree_Size : Gtk_Requisition;
                List_Size : Gtk_Requisition;
                Store     : Gtk_Persistent_Directory;
                Manager   : not null access
                            Gtk_Recent_Manager_Record'Class;
                Tracing   : Traced_Actions
             )  is
      Scroll : Gtk_Scrolled_Window;
   begin
      if Vertical then
         Initialize_VPaned (Widget);
      else
         Initialize_HPaned (Widget);
      end if;
      Widget.Cache := Store;
      if Widget.Cache = null then
         Gtk_New (Widget.Cache, Query, Manager, Tracing => Tracing);
      else
         Set_Tracing (Widget.Cache, Tracing);
      end if;
      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add1 (Widget, Scroll);
      Widget.Tree := new Gtk_Persistent_Storage_Tree_View_Record;
      Gtk.Persistent_Storage_Browser.Initialize
      (  Widget.Tree,
         Widget.Cache,
         Path
      );
      declare
         Size : Gtk_Requisition;
      begin
         Columns_Autosize (Widget.Tree);   -- Size columns
         Size_Request (Widget.Tree, Size); -- Query the integral size
         Set_Size_Request                  -- Set new size
         (  Widget.Tree,
            GInt'Max (1, GInt'Min (Size.Width,  Tree_Size.Width)),
            GInt'Max (1, GInt'Min (Size.Height, Tree_Size.Height))
         );
      end;
      Add (Scroll, Widget.Tree);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add2 (Widget, Scroll);
      Widget.Items := new Gtk_Persistent_Storage_Objects_View_Record;
      Widget.Items.Browser := Widget.all'Access;
      Gtk.Persistent_Storage_Browser.Initialize
      (  Widget.Items,
         Widget.Tree,
         Columns
      );
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
      Add (Scroll, Widget.Items);
      if Store = null then
         Unref (Widget.Cache);
      end if;
   exception
      when others =>
         if Store = null and then Widget.Cache /= null then
            Unref (Widget.Cache);
         end if;
         raise;
   end Initialize;

   function Is_Directory
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Name  : String;
               Class : String
            )  return Boolean is
   begin
      return Class = Persistent.Directory.Directory_Class;
   end Is_Directory;

   function Is_Root (Path : Item_Path) return Boolean is
      Pointer : Integer := Path'Last + 1;
      Length  : Natural;
   begin
      Scan_Backwards (Path, Pointer, Length);
      return Pointer = Path'First;
   end Is_Root;

   procedure Name_Commit
             (  Widget   : not null access
                           Gtk_Persistent_Storage_Tree_View_Record;
                Old_Path : Item_Path;
                New_Name : Item_Name
             )  is
      Cache   : Gtk_Persistent_Directory_Record'Class
                   renames Get_Directory_Cache (Widget).all;
      Storage : Storage_Handle;
      Object  : Deposit_Handle;
   begin
      if Get_Name (Old_Path) = New_Name then
         return;
      elsif Is_Root (Old_Path) then
         Say (Widget, "root-renaming-error");
         return;
      elsif New_Name'Length = 0 then
         Say (Widget, "null-renaming-error");
         return;
      end if;
      begin
         Browse (Cache'Access, Old_Path, Storage, Object);
      exception
         when Error : Name_Error | End_Error |
                      Ada.IO_Exceptions.Data_Error =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               "Wrong old path renamed" & Where ("Name_Commit")
            );
            return;
      end;
      begin
         Rename
         (  Storage,
            Object,
            String (New_Name),
            Get_Parent (Storage, Object)
         );
      exception
         when Error : Constraint_Error =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               "Renamed object doesn't persist" & Where ("Name_Commit")
            );
            return;
         when Name_Error =>
            Say (Widget, "name-conflict-error");
            return;
         when Error : Ada.IO_Exceptions.Data_Error =>
            Say (Widget, "storage-error", Exception_Message (Error));
            return;
      end;
      Renamed (Cache'Access, Old_Path, New_Name);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            Exception_Message (Error) & Where ("Name_Commit")
         );
   end Name_Commit;

   procedure Name_Commit
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record;
                Index  : Positive;
                Name   : Item_Name
             )  is
      Cache   : Gtk_Persistent_Directory_Record'Class renames
                   Get_Directory_Cache (Widget).all;
      Item    : Natural   := Index;
      Path    : constant Item_Path := Get_Directory (Widget);
      Storage : Storage_Handle;
      Parent  : Deposit_Handle;
      Changed : Boolean;
   begin
      if Get_Name (Widget, Index) = Name then
         return;
      elsif Name'Length = 0 then
         Say (Widget, "null-renaming-error");
         return;
      elsif Path'Length = 0 then
         Say (Widget, "root-renaming-error");
         return;
      end if;
      begin
         Browse
         (  Cache'Access,
            Path,
            Storage,
            Parent
         );
      exception
         when Error : Name_Error | End_Error |
                      Ada.IO_Exceptions.Data_Error =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               "Wrong old path renamed" & Where ("Name_Commit")
            );
            return;
      end;
      begin
         Rename
         (  Storage,
            String (Get_Name (Widget, Index)),
            Parent,
            String (Name),
            Parent
         );
      exception
         when Error : Constraint_Error =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               "Renamed object doesn't persist" & Where ("Name_Commit")
            );
            return;
         when Name_Error =>
            Say (Widget, "name-conflict-error");
            return;
         when Error : Ada.IO_Exceptions.Data_Error =>
            Say (Widget, "storage-error", Exception_Message (Error));
            return;
      end;
      Renamed (Widget, Item, Name);
      if Item > 0 then
         Move (Widget, Changed, 0, Item);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            Exception_Information (Error) & Where ("Name_Commit")
         );
   end Name_Commit;

   function Read
            (  Store : not null access Gtk_Persistent_Directory_Record
            )  return Directory_Item is
      Data : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
       case Data.Mode is
         when None =>
            -- No browsing
            if 0 /= (Get_Tracing (Store) and Trace_IO) then
               Trace (Store, Get_Depth (Store), "read completed");
            end if;
            raise End_Error;
         when Root =>
            -- Browsing the root-level (persistent storages names)
            if Data.Index >= GetSize (Data.Root_List) then
               if 0 /= (Get_Tracing (Store) and Trace_IO) then
                  Trace (Store, Get_Depth (Store), "read completed");
               end if;
               raise End_Error;
            end if;
            Data.Index := Data.Index + 1;
            declare
               Name : String renames
                         GetName (Data.Root_List, Data.Index);
            begin
               if Is_Valid
                  (  Ptr
                     (  GetTag (Data.Root_List, Data.Index)
                     ) .Storage
                  )
               then
                  if 0 /= (Get_Tracing (Store) and Trace_IO) then
                     Trace
                     (  Store,
                        Get_Depth (Store),
                        "read root " & Name & " " & Database_Open
                     );
                  end if;
                  return
                  (  Directory   => True,
                     Policy      => Cache_Expanded,
                     Name_Length => Name'Length,
                     Name        => Item_Name (Name),
                     Kind_Length => Database_Open'Length,
                     Kind        => Item_Type (Database_Open)
                  );
               else
                  if 0 /= (Get_Tracing (Store) and Trace_IO) then
                     Trace
                     (  Store,
                        Get_Depth (Store),
                        "read root " & Name & " " & Database_Closed
                     );
                  end if;
                  return
                  (  Directory   => True,
                     Policy      => Cache_Never,
                     Name_Length => Name'Length,
                     Name        => Item_Name (Name),
                     Kind_Length => Database_Closed'Length,
                     Kind        => Item_Type (Database_Closed)
                  );
               end if;
            end;
         when Catalogue =>
            -- Browsing a directory in a persistent storage
            if Data.Index >= Get_Size (Data.Folder) then
               if 0 /= (Get_Tracing (Store) and Trace_IO) then
                  Trace (Store, Get_Depth (Store), "read completed");
               end if;
               raise End_Error;
            end if;
            Data.Index := Data.Index + 1;
            declare
               Name  : constant String := Get (Data.Folder, Data.Index);
               Class : String renames
                          Get_Class (Data.Storage, Name, Data.Parent);
            begin
               if 0 /= (Get_Tracing (Store) and Trace_IO) then
                  Trace
                  (  Store,
                     Get_Depth (Store),
                     "read " & Name & " " & Class
                  );
               end if;
               return
               (  Directory =>
                     Is_Directory
                     (  Gtk_Persistent_Directory_Record'Class
                        (  Store.all
                        ) 'Unchecked_Access,
                        Name,
                        Class
                     ),
                  Policy      => Cache_Expanded,
                  Name_Length => Name'Length,
                  Kind_Length => Class'Length,
                  Name        => Item_Name (Name),
                  Kind        => Item_Type (Class)
               );
            end;
      end case;
   end Read;

   procedure Remove_Item
             (  Manager : Gtk_Recent_Manager;
                URI     : Unbounded_String
             )  is
      Error : GError;
   begin
      Remove_Item (Manager, To_String (URI), Error);
      if Error /= null then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Warning,
            Get_Message (Error) & Where ("Remove_Item")
         );
         Error_Free (Error);
      end if;
   end Remove_Item;

   function Rewind
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return Directory_Item is
      Data : Directory_Data'Class renames Ptr (Store.Data).all;
   begin
      Data.Mode := None;
      if Path'Length = 0 then
         --
         -- Browsing the root-level
         --
         Erase (Data.Root_List);
         declare
            Exec : UTF8_String renames Get_Application_Name;
            Info : Gtk_Recent_Info;
            List : constant Gtk_Recent_Info_Array :=
                   Get_Items (Data.Manager);
            Item : Root_Item_Handles.Handle;
         begin
            for Index in List'Range loop
               Info := List (Index);
               if (  Has_Application (Info, Exec)
                  and then
                     Get_Description (List (Index)) = DSN_URI
                  )
               then
                  -- This item is from us
                  declare
                     URI : UTF8_String renames Get_URI (Info);
                     DSN : constant UTF8_String :=
                           UTF8_String (Get_DSN (URI));
                  begin
                     if (  DSN'Length > 0
                        and then
                           not IsIn (Data.Root_List, DSN)
                        )
                     then
                        -- It has valid syntax and is new
                        Root_Item_Handles.Set (Item, new Root_Item);
                        Append (Ptr (Item).URI, URI);
                        Add (Data.Root_List, DSN, Item);
                     end if;
                  end;
               end if;
               Unref (Info);
            end loop;
         end;
         Data.Mode  := Root;
         Data.Index := 0;
         return Directory_Entered;
      elsif Is_Root (Path) then
         --
         -- Browsing a persistent storage
         --
         declare
            Name : Item_Name renames Get_Name (Store, Path);
            Item : Root_Item_Handles.Handle;
         begin
            Item := Find (Data.Root_List, String (Name));
            if not Is_Valid (Ptr (Item).Storage) then
               if not Is_Valid (Data.Query) then
                  raise Ada.IO_Exceptions.Data_Error;
               end if;
               --
               -- Query the operator for the password in order to access
               -- the storage.
               --
               declare
                  This     : Root_Item'Class renames Ptr (Item).all;
                  Old_URI  : constant UTF8_String :=
                                      To_String (This.URI);
                  New_URI  : Unbounded_String;
                  User     : Unbounded_String;
                  Password : Unbounded_String;
                  Stored   : Boolean := False;
               begin
                  case Get_Scheme (Old_URI) is
                     when DSN_Scheme =>
                        Get_Credentials
                        (  Old_URI,
                           User,
                           Password,
                           Stored
                        );
                        Get
                        (  Ptr (Data.Query).all,
                           DSN_Scheme,
                           String (Name),
                           User,
                           Password,
                           Stored,
                           This.Storage
                        );
                        New_URI :=
                           To_Unbounded_String
                           (  To_DSN_URI
                              (  UTF8_String (Get_Name (Store, Path)),
                                 To_String (User),
                                 To_String (Password),
                                 Stored
                           )  );
                        Wipe (Password);
                     when FDB_Scheme =>
                        Get_Credentials
                        (  Old_URI,
                           User,
                           Password,
                           Stored
                        );
                        Get
                        (  Ptr (Data.Query).all,
                           FDB_Scheme,
                           String (Name),
                           User,
                           Password,
                           Stored,
                           This.Storage
                        );
                        New_URI :=
                           To_Unbounded_String
                           (  To_FDB_URI
                              (  UTF8_String (Get_Name (Store, Path)),
                                 To_String (User)
                           )  );
                     when SQLite_Scheme =>
                        Get_Credentials
                        (  Old_URI,
                           User,
                           Password,
                           Stored
                        );
                        Get
                        (  Ptr (Data.Query).all,
                           SQLite_Scheme,
                           String (Name),
                           User,
                           Password,
                           Stored,
                           This.Storage
                        );
                        New_URI :=
                           To_Unbounded_String
                           (  To_SQLite_URI
                              (  UTF8_String (Get_Name (Store, Path)),
                                 To_String (User)
                           )  );
                  end case;
                  if not Is_Valid (This.Storage) then
                     -- Cancelled
                     Wipe (Password);
                     return
                     (  Policy      => Cache_Never,
                        Directory   => True,
                        Name        => Name,
                        Name_Length => Name'Length,
                        Kind_Length => Database_Closed'Length,
                        Kind  => Item_Type (Database_Closed)
                     );
                  end if;
                  if New_URI /= This.URI then
                     -- Replace old item with an update
                     Remove_Item (Data.Manager, This.URI);
                     Store_Item (Data.Manager, To_String (New_URI));
                     This.URI := New_URI;
                  end if;
               exception
                  when Ada.IO_Exceptions.Use_Error =>
                     -- Drop the storage, but first remove its item
                     Remove_Item (Data.Manager, This.URI);
                     Delete (Data.Root_List, String (Name));
                     Wipe (Password);
                     raise Ada.IO_Exceptions.Data_Error with
                           "Storage removed on user request";
                  when others =>
                     Wipe (Password);
                     raise;
               end;
            end if;
            Data.Storage := Ptr (Item).Storage;
            Deposit_Handles.Invalidate (Data.Parent);
            Data.Folder  := Get_List (Data.Storage);
            Data.Mode    := Catalogue;
            Data.Index   := 0;
            return
            (  Policy      => Cache_Expanded,
               Directory   => True,
               Name        => Name,
               Name_Length => Name'Length,
               Kind_Length => Database_Open'Length,
               Kind        => Item_Type (Database_Open)
            );
         end;
      else
         --
         -- Browsing children of a persistent object
         --
         Browse (Store, Path, Data.Storage, Data.Parent);
         Data.Folder :=
            Get_List (Data.Storage, Parent => Data.Parent);
         Data.Mode  := Catalogue;
         Data.Index := 0;
         return Directory_Entered;
      end if;
   exception
      when Error : Use_Error => -- Unregistered class
         declare
            Name : Item_Name renames Get_Name (Store, Path);
         begin
            Rewind_Error (Store, Exception_Message (Error), Path);
            return
            (  Policy      => Cache_Never,
               Directory   => True,
               Name        => Name,
               Name_Length => Name'Length,
               Kind_Length => Unknown'Length,
               Kind        => Item_Type (Unknown)
            );
         end;
      when Ada.IO_Exceptions.Data_Error =>
         raise;
      when Error : others => -- This should not happen
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            Exception_Information (Error) & Where ("Rewind")
         );
         raise Ada.IO_Exceptions.Data_Error with
               Exception_Message (Error);
   end Rewind;

   procedure Say
             (  Widget : not null access
                         Gtk_Persistent_Storage_Tree_View_Record;
                Name   : UTF8_String;
                Reason : UTF8_String := ""
             )  is
   begin
      if Reason'Length > 0 then
         Message_Dialog
         (  Title         => Style_Get (Widget, "message-title"),
            Justification => Justify_Left,
            Message       => Style_Get (Widget, Name) & ". " & Reason,
            Parent        => Widget
         );
      else
         Message_Dialog
         (  Title         => Style_Get (Widget, "message-title"),
            Justification => Justify_Left,
            Message       => Style_Get (Widget, Name),
            Parent        => Widget
         );
      end if;
   end Say;

   procedure Say
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record;
                Name   : UTF8_String;
                Reason : UTF8_String := ""
             )  is
   begin
      if Reason'Length > 0 then
         Message_Dialog
         (  Title         => Style_Get (Widget, "message-title"),
            Justification => Justify_Left,
            Message       => Style_Get (Widget, Name) & ". " & Reason,
            Parent        => Widget
         );
      else
         Message_Dialog
         (  Title         => Style_Get (Widget, "message-title"),
            Justification => Justify_Left,
            Message       => Style_Get (Widget, Name),
            Parent        => Widget
         );
      end if;
   end Say;

   procedure Scan
             (  Path        : Item_Path;
                Pointer     : in out Integer;
                Length      : out Natural;
                Terminators : Character_Set
             )  is
      Sequence : Boolean := False;
   begin
      if (  Pointer < Path'First
         or else
            (Pointer > Path'Last and then Pointer > Path'Last + 1)
         )
      then
         raise Layout_Error;
      end if;
      Length := 0;
      while Pointer <= Path'Last loop
         if Path (Pointer) = Escape then
            Pointer := Pointer + 1;
            exit when Pointer > Path'Last;
         elsif Is_In (Path (Pointer), Terminators) then
            return;
         end if;
         Length  := Length  + 1;
         Pointer := Pointer + 1;
      end loop;
   end Scan;

   procedure Scan_Backwards
             (  Path    : Item_Path;
                Pointer : in out Integer;
                Length  : out Natural
             )  is
      Sequence : Boolean := False;
      Break    : Boolean := False;
   begin
      if (  Pointer < Path'First
         or else
            (Pointer > Path'Last and then Pointer > Path'Last + 1)
         )
      then
         raise Layout_Error;
      end if;
      Length := 0;
      for Index in reverse Path'First..Pointer - 1 loop
         declare
            Symbol : Character renames Path (Index);
         begin
            if Symbol = Escape then
               -- The body of an escape sequence
               Break := False;
               if Sequence then
                  -- A pair of escapes
                  Length := Length + 1;
                  Sequence := False;
               else
                  -- A new escape
                  Sequence := True;
               end if;
            else
               -- Outside escape sequence
               if Break then
                  -- A non-escaped separator found
                  Pointer := Index + 2;
                  Length  := Length - 1;
                  return;
               end if;
               Length := Length + 1;
               Break  := Symbol = Separator;
            end if;
         end;
      end loop;
      if Break then
         Pointer := Path'First + 1;
         Length  := Length - 1;
      else
         Pointer := Path'First;
      end if;
   end Scan_Backwards;

   procedure Set_Query
             (  Store : not null access Gtk_Persistent_Directory_Record;
                Query : Query_Handles.Handle
             )  is
   begin
      Ptr (Store.Data).Query := Query;
   end Set_Query;

   procedure Set_Tracing
             (  Widget  : not null access
                          Gtk_Persistent_Storage_Browser_Record;
                Tracing : Traced_Actions
             )  is
   begin
      Set_Tracing (Widget.Cache, Tracing);
   end Set_Tracing;

   procedure Store_Item
             (  Manager : Gtk_Recent_Manager;
                Value   : UTF8_String
             )  is
   begin
      if Add_Full
         (  Manager      => Manager,
            URI          => Value,
            Display_Name => "",
            Description  => DSN_URI
         )
      then
         null;
      end if;
   end Store_Item;

   function To_Escaped (Name : Item_Name) return Item_Path is
      Length : Integer := Name'Length;
   begin
      for Index in Name'Range loop
         if Is_In (Name (Index), Delimiters) then
            Length := Length + 1;
         end if;
      end loop;
      declare
         Result : Item_Path (1..Length);
         Symbol : Character;
      begin
         Length := Result'First;
         for Index in Name'Range loop
            Symbol := Name (Index);
            if Is_In (Symbol, Delimiters) then
               Result (Length) := Escape;
               Length := Length + 1;
            end if;
            Result (Length) := Symbol;
            Length := Length + 1;
         end loop;
         return Result;
      end;
   end To_Escaped;

   function To_DSN_URI
            (  DSN      : UTF8_String;
               User     : UTF8_String;
               Password : UTF8_String    := "";
               Stored_Password : Boolean := False
            )  return UTF8_String is
   begin
      if Stored_Password then
         if User'Length + Password'Length > 254 then
            raise Constraint_Error;
         end if;
         return
         (  DSN_URI_Scheme
         &  Encode
            (  (  Character'Val (User'Length)
               &  Character'Val (Password'Length)
               &  User
               &  Password
               ),
               Get_Application_Name
            )
         &  '@'
         &  UTF8_String (To_Escaped (Item_Name (DSN)))
         );
      else
         if User'Length > 254 then
            raise Constraint_Error;
         end if;
         return
         (  DSN_URI_Scheme
         &  Encode
            (  (  Character'Val (User'Length)
               &  Character'Val (255)
               &  User
               ),
               Get_Application_Name
            )
         &  '@'
         &  UTF8_String (To_Escaped (Item_Name (DSN)))
         );
      end if;
   end To_DSN_URI;

   function To_FDB_URI
            (  DSN  : UTF8_String;
               File : UTF8_String
            )  return UTF8_String is
   begin
      return
      (  FDB_URI_Scheme
      &  '@'
      &  UTF8_String (To_Escaped (Item_Name (DSN)))
      &  '/'
      &  UTF8_String (To_Escaped (Item_Name (File)))
      );
   end To_FDB_URI;

   function To_SQLite_URI
            (  DSN  : UTF8_String;
               File : UTF8_String
            )  return UTF8_String is
   begin
      return
      (  SQLite_URI_Scheme
      &  '@'
      &  UTF8_String (To_Escaped (Item_Name (DSN)))
      &  '/'
      &  UTF8_String (To_Escaped (Item_Name (File)))
      );
   end To_SQLite_URI;

   function "&" (Path : Item_Path; Name : Item_Name) return Item_Path is
   begin
      if Path'Length > 0 then
         return Path & Separator & To_Escaped (Name);
      else
         return To_Escaped (Name);
      end if;
   end "&";

end Gtk.Persistent_Storage_Browser;
