--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Persistent_Storage_Browser             Luebeck            --
--  Interface                                      Winter, 2008       --
--                                                                    --
--                                Last revision :  19:57 08 Aug 2015  --
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
--
--  This  package  provides facilities for visual browsing of persistent
--  storages and their objects. It contains of the following parts:
--
--  (o)  Object paths and name management. Each named persistent  object
--       is  uniquely  identified  by its path. The path also identifies
--       the persistent storage itself.
--  (o)  Gtk_Persistent_Directory  is  a  directory  cache of persistent
--       objects. The  cache  is  a  tree  view  model  interfacing  the
--       persistent storage catalogue.
--  (o)  Gtk_Persistent_Storage_Tree_View is  a  specialized  tree  view
--       widget to render storage catalogue as a hierarchy of objects.
--  (o)  Gtk_Persistent_Storage_Items_View is a  specialized  tree  view
--       widget to render storage objects as a columned list.
--  (o)  Gtk_Persistent_Storage_Browser_Record  is a composite widget of
--       two panels containing the tree and items views.
--
with Ada.Calendar;            use Ada.Calendar;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Gtk.Abstract_Browser;    use Gtk.Abstract_Browser;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Paned;               use Gtk.Paned;
with Gtk.Recent_Manager_Alt;  use Gtk.Recent_Manager_Alt;
with Gtk.Widget;              use Gtk.Widget;
with Persistent;              use Persistent;
with Persistent.Handle;       use Persistent.Handle;

with Ada.Finalization;
with Object.Handle;
with Tables;

package Gtk.Persistent_Storage_Browser is
   pragma Elaborate_Body (Gtk.Persistent_Storage_Browser);
--
-- Class names
--
   Persistent_Storage_Tree_View_Class_Name : constant String :=
      "GtkPersistentStorageTreeView";
   Persistent_Storage_Items_View_Class_Name : constant String :=
      "GtkPersistentStorageItemsView";
------------------------------------------------------------------------
-- Path and URI manipulation. A path  consists  of  names  separated  by
-- slahes  (/).  Slashes  and  other characters can be escaped using the
-- back slash (\). So an object name can  be  any.  Item_Name  does  not
-- contain escape sequences. URI has the following format:
--
--    dsn://<credentials>@<dsn>
--    sqlite://@<dsn>/<file>
--
--    <credentials> - Encrypted user name and password
--    <dsn>         - The data source / server name
--    <engine>      - The engine type, e.g. sqlite
--    <file>        - The file name containing a data base
--
-- All fields are escaped using the backward slash. Credentials field is
-- encoded and encrypted.
--
   Escape            : constant Character := '\';
   Separator         : constant Character := '/';
   Delimiters        : constant Character_Set := To_Set ("+#:;=@/\?");
   DSN_URI_Scheme    : constant String := "dsn://";
   SQLite_URI_Scheme : constant String := "sqlite://";
   FDB_URI_Scheme    : constant String := "fdb://";
--
-- Scheme_Type -- URI scheme type
--
   type Scheme_Type is (DSN_Scheme, SQLite_Scheme, FDB_Scheme);
--
-- & -- Path composition
--
--    Path - A parent object's directory path
--    Name - The child object's name
--
-- Returns :
--
--    The object's path
--
-- Exceptions :
--
--    Name_Error - Empty name
--
   function "&" (Path : Item_Path; Name : Item_Name) return Item_Path;
--
-- Get_Credentials -- Get user and password from URI
--
--    URI          - To parse
--    User         - The user name or else file name
--    Password     - The password
--    Has_Password - Password presence flag
--
-- The  result  is  stored  in  User  and Password. The output parameter
-- Has_Password  set to false indicates that there is no password in the
-- credentials  string,  so it has to be ignored. When the URI scheme is
-- ODBC, User and Password  are  the  credentials  to  access  the  data
-- source.  When  the URI scheme is SQLite or FDB, User is the data base
-- file name, Password is empty string. Has_Pasword is False.
--
   procedure Get_Credentials
             (  URI          : UTF8_String;
                User         : out Unbounded_String;
                Password     : out Unbounded_String;
                Has_Password : out Boolean
             );
--
-- Get_DSN -- Get storage name from a URI
--
--    URI - To parse
--
-- The result is an empty string if the argument has wrong syntax.
--
-- Returns :
--
--    The persistent storage name (server name or DSN)
--
   function Get_DSN (URI : UTF8_String) return Item_Name;
--
-- Get_Name -- From a path
--
--    Path - To an object
--
-- Returns :
--
--    The object name
--
-- Exceptions :
--
--    Name_Error - Wrong path
--
   function Get_Name (Path : Item_Path) return Item_Name;
--
-- Get_Directory -- From a path
--
--   Path - To an object
--
-- Returns :
--
--    The object's directory path
--
-- Exceptions :
--
--    Name_Error - Wrong path or there is no parent path
--
   function Get_Directory (Path : Item_Path) return Item_Path;
--
-- Get_Scheme -- URI scheme
--
--    URI - To parse
--
-- Returns :
--
--    The scheme
--
-- Exceptions :
--
--    Constraint_Error - Unknown scheme
--
   function Get_Scheme (URI : UTF8_String) return Scheme_Type;
--
-- Is_Root -- Root-level check
--
--    Path - An object path
--
-- Returns :
--
--    True if Path is of a root-level object
--
   function Is_Root (Path : Item_Path) return Boolean;
--
-- To_DSN_URI -- Create an URI
--
--    DSN             - The data source name
--    User            - The user name
--    Password        - The password, ignored if the following is false
--    Stored_Password - Password storing flag
--
-- Returns :
--
--    The URI in DSN scheme
--
   function To_DSN_URI
            (  DSN      : UTF8_String;
               User     : UTF8_String;
               Password : UTF8_String    := "";
               Stored_Password : Boolean := False
            )  return UTF8_String;
--
-- To_FDB_URI -- Create an URI
--
--    DSN  - The data source name
--    File - The data base file name
--
-- Returns :
--
--    The URI in FDB scheme
--
   function To_FDB_URI
            (  DSN  : UTF8_String;
               File : UTF8_String
            )  return UTF8_String;
--
-- To_SQLite_URI -- Create an URI
--
--    DSN  - The data source name
--    File - The data base file name
--
-- Returns :
--
--    The URI in SQLite scheme
--
   function To_SQLite_URI
            (  DSN  : UTF8_String;
               File : UTF8_String
            )  return UTF8_String;
------------------------------------------------------------------------
-- Abstract_Credentials_Query -- Querying user credentials. This  is  an
--                               abstract  type serving as an interface.
-- An object of this class is passed to the persistent  directory  store
-- object  to provide credentials input. Usually it is a dialog box that
-- is created to accomplish the input.  This  package  provides  such  a
-- dialog and a derived from this one type based on this dialog.
--
   type Abstract_Credentials_Query is abstract
      new Object.Entity with null record;
   type Abstract_Credentials_Query_Ptr is
      access Abstract_Credentials_Query'Class;
--
-- Create -- Ask user for storage name and credentials
--
--    Query           - The query
--    Scheme          - The scheme of URI
--    Name            - Of the persistent storage server or DSN or file
--    User            - The user name
--    Password        - The password
--    Stored_Password - Password storing flag (when true)
--    Storage         - The result
--
-- This function asks user to input the persistent  storage  name,  user
-- name and password. After successful completion the output  parameters
-- are set and the the parameter  Storage  is  a  valid  handle  to  the
-- persistent  storage.  When  user cancels input Storage is invalid. An
-- implementation  may  propagate  Use_Error  to   indicate   that   the
-- persistent storage should not  be  used,  i.e.  dropped.  The  output
-- parameter  Scheme  indicates  the type of the credentials, which then
-- determines other output parameters:
--
--    DSN    - Name  is  the  persistent storage server name or else DSN
--             name.  User  is  the user name, Password is the password.
--             Stored_Password is the pasword storing flag.
--    SQLite - Name is the persistent storage name.  User  contains  the
--             file name. Password is an empty  string.  Stored_Password
--             is false.
--    FDB    - Name is the persistent storage name.  User  contains  the
--             file name. Password is an empty  string.  Stored_Password
--             is false.
--
-- Exceptions :
--
--    Use_Error - Drop storage
--
   procedure Create
             (  Query           : in out Abstract_Credentials_Query;
                Scheme          : out Scheme_Type;
                Name            : out Unbounded_String;
                User            : out Unbounded_String;
                Password        : out Unbounded_String;
                Stored_Password : out Boolean;
                Storage         : out Storage_Handle
             )  is abstract;
--
-- Get -- Ask user for credentials
--
--    Query           - The query
--    Scheme          - The scheme of URI
--    Name            - Of the persistent storage server, DSN or name
--    User            - The user name (ODBC) or else file name (SQLite)
--    Password        - The password
--    Stored_Password - Password storing flag
--    Storage         - The result
--
-- This procedure is like Create but has initial values for  Name,  User
-- and Stored_Password. Further, Scheme and Name cannot be changed.
--
-- Exceptions :
--
--    Use_Error - Drop storage
--
   procedure Get
             (  Query           : in out Abstract_Credentials_Query;
                Scheme          : Scheme_Type;
                Name            : UTF8_String;
                User            : in out Unbounded_String;
                Password        : in out Unbounded_String;
                Stored_Password : in out Boolean;
                Storage         : out Storage_Handle
             )  is abstract;
--
-- Query_Handles -- Handles to query objects
--
   package Query_Handles is
      new Object.Handle
          (  Abstract_Credentials_Query,
             Abstract_Credentials_Query_Ptr
          );
   use Query_Handles;
------------------------------------------------------------------------
-- Gtk_Persistent_Directory_Record -- A  directory  cache,  which can be
--                                    used   as   a   tree   model.  The
-- directory contains persistent storages  on  the  root  level.  Deeper
-- level constains  persistent  objects  with  children.  An  additional
-- constraint can  be  put  upon  the  objects  which  fall  under  this
-- cathegory. All other named objects are leafs and do not appear in the
-- store.
--
-- The first column of the store contains at the root level the stock-id
-- Stock_Disconnect  or   Stock_Connect   depending   on   whether   the
-- corresponding persistent store is  connected.  The  following  levels
-- have the object's class as the first column value. One might wish  to
-- override Get_Icon operations of the tree and items  view  widgets  in
-- order to translate the first column values into desired stock-ids.
--
   type Gtk_Persistent_Directory_Record is
      new Gtk_Abstract_Directory_Record with private;
   type Gtk_Persistent_Directory is
      access all Gtk_Persistent_Directory_Record'Class;
--
-- Add_Storage -- Queries the user for a persitent storage to add
--
--    Store   - The directory cache
--    Storage - The result, a handle to, invalid if user cancels intput
--
-- This procedure can be called in order to query the  user  for  a  new
-- persistent storage. For this it uses the credential dialog associated
-- with the store. When the storage was successfully added a  handle  to
-- it is returned. Otherwise it is set invalid. The storage is added  at
-- the  root  level. When the user chooses an already connected storage,
-- its handle is returned.
--
   procedure Add_Storage
             (  Store   : not null access
                          Gtk_Persistent_Directory_Record;
                Storage : out Storage_Handle
             );
--
-- Browse - Path in the cache
--
--    Store   - The directory cache
--    Path    - An object path
--    Storage - A handle to the persistent storage of
--    Object  - A handle to the object
--    Partial - When true, End_Error does not propagate
--
-- This procedure browses an object path.  It  returns  handles  to  the
-- object  indicated  by  the  path  and  the persistent storage of. The
-- persistent  storage  must  be  connected. When the storage is not yet
-- connected,  or  some  items  of  the  path  do not exist End_Error is
-- propagated. If the parameter  Partial  is  true,  then  the  path  is
-- browsed as far as possible and End_Error is not propagated. Use_Error
-- is propagated when an object on the path cannot be restored from  the
-- persistent storage because its class is unknown.
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--    End_Error  - Wrong path (some items do not exist)
--    Name_Error - Illegal path
--    Use_Error  - Unknown object's class cannot be restored
--
   procedure Browse
             (  Store   : not null access
                          Gtk_Persistent_Directory_Record;
                Path    : Item_Path;
                Storage : out Storage_Handle;
                Object  : out Deposit_Handle;
                Partial : Boolean := False
             );
--
-- Compare -- Overrides Gtk.Abstract_Browser...
--
   function Compare
            (  Store     : not null access
                           Gtk_Persistent_Directory_Record;
               Directory : Item_Path;
               A, B      : Directory_Item;
               By_Name   : Boolean
            )  return Row_Order;
--
-- Delete -- An object
--
--    Store - The directory cache
--    Path  - The object's path
--
-- The  object  is  unnamed  and  then  the  cache  is  synchronized  as
-- necessary. When path specifies  a  persistent  storage,  then  it  is
-- only disconnected and removed from the cache.
--
-- Exceptions :
--
--    Data_Error - Deletion error
--    Name_Error - Illegal path
--
   procedure Delete
             (  Store : not null access Gtk_Persistent_Directory_Record;
                Path  : Item_Path
             );
--
-- Get_Class - Class of an object by its path
--
--    Store - The directory cache
--    Path  - An object path
--
-- This function browses an object path. It returns the object's   class
-- The persistent storage must be connected. When the storage is not yet
-- connected,  or  some  items  of  the  path  do not exist End_Error is
-- propagated. Use_Error is propagated when an object on the path cannot
-- be restored from the persistent storage because its class is unknown.
-- The object idicated by Path is not restored, but all its parents have
-- to.
--
-- Returns :
--
--    The object's class
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--    End_Error  - Wrong path (some items do not exist)
--    Name_Error - Illegal path
--    Use_Error  - Unknown object's class cannot be restored
--
   function Get_Class
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return String;
--
-- Get_Creation_Time - Creation time of an object by its path
--
--    Store - The directory cache
--    Path  - An object path
--
-- This function  browses  an  object  path.  It  returns  the  object's
-- creation time/ The persistent storage must  be  connected.  When  the
-- storage  is not yet connected, or some items of the path do not exist
-- End_Error  is  propagated.  Use_Error is propagated when an object on
-- the  path  cannot be restored from the persistent storage because its
-- class  is  unknown.  The object idicated by Path is not restored, but
-- all its parents have to.
--
-- Returns :
--
--    The object's creation time
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--    End_Error  - Wrong path (some items do not exist)
--    Name_Error - Illegal path
--    Use_Error  - Unknown object's class cannot be restored
--
   function Get_Creation_Time
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return Time;
--
-- Get_Directory -- Overrides Gtk.Abstract_Browser...
--
   function Get_Directory
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Item  : Item_Path
            )  return Item_Path;
--
-- Get_DSN -- Get storage name from a URI
--
--    Store   - The directory cache
--    Storage - A handle to
--
-- The result is the DSN name of Storage as registered in Store.
--
-- Returns :
--
--    The persistent storage name (server name or DSN)
--
-- Exceptions :
--
--    Constraint_Error - Storage is invalid or unknown to Store
--
   function Get_DSN
            (  Store   : not null access
                         Gtk_Persistent_Directory_Record;
               Storage : Storage_Handle
            )  return Item_Name;
--
-- Get_Manager -- The recent resource manager
--
--    Store - The directory cache
--
-- Returns :
--
--    The resource manager used with the store
--
   function Get_Manager
            (  Store : not null access Gtk_Persistent_Directory_Record
            )  return Gtk_Recent_Manager;
--
-- Get_Name -- Overrides Gtk.Abstract_Browser...
--
   function Get_Name
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Item  : Item_Path
            )  return Item_Name;
--
-- Get_Path -- Overrides Gtk.Abstract_Browser...
--
   function Get_Path
            (  Store     : not null access
                           Gtk_Persistent_Directory_Record;
               Directory : Item_Path;
               Item      : Item_Name
            )  return Item_Path;
--
-- Get_Path -- Get path of a persistent object
--
--    Store   - The directory cache
--    Storage - A handle to persistent storage
--    Object  - A handle to the object
--
-- When Object is not an valid handle, the  result  the  root  directory
-- path.
--
-- Returns :
--
--    The object's path
--
-- Exceptions :
--
--    Constraint_Error - Not a persistent object
--    Data_Error       - Inconsistent storage
--    Name_Error       - Anonymous object
--
   function Get_Path
            (  Store   : not null access
                         Gtk_Persistent_Directory_Record;
               Storage : Storage_Handle;
               Object  : Deposit_Handle
            )  return Item_Path;
--
-- Get_Query -- The credentials dialog
--
--    Store - The directory cache
--
-- Returns :
--
--    A handle to the credentials dialogue used
--
   function Get_Query
            (  Store : not null access Gtk_Persistent_Directory_Record
            )  return Query_Handles.Handle;
--
-- Get_Storage -- Path check
--
--    Store - The directory cache
--    Path  - An object path
--
-- This function returns a  handle  to  the  storage  where  the  object
-- belongs. Root-level paths denote the storage itself. The result is an
-- invalid handle if the storage is not yet connected.
--
-- Returns :
--
--    A handle to the storage
--
-- Exceptions :
--
--    End_Error  - The storage specified in the path is not in the cache
--    Name_Error - Wrong path
--
   function Get_Storage
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return Storage_Handle;
--
-- Gtk_New -- Factory
--
--    Store   - The result
--    Query   - The credentials dialog to use with
--    Manager - The recently used resources manager
--    Tracing - Actions desired for tracing
--
-- This procedure creates a new cache. The parameter Manager is a recent
-- resource  manager to use. The manager keeps the configured persistent
-- storage data sources as items. Each such item  appears  at  the  root
-- level  of  the cache. When Query is an invalid handle no  querying is
-- used and so root-level items attempted for browsing will be removed.
--
   procedure Gtk_New
             (  Store   : out Gtk_Persistent_Directory;
                Query   : Query_Handles.Handle;
                Manager : not null access
                          Gtk_Recent_Manager_Record'Class :=
                             Get_Default;
                Tracing : Traced_Actions := Trace_Nothing
             );
--
-- Initialize -- To be called by any derived type
--
--    Store   - A newly created object to initialize
--    Query   - The credentials dialog to use with
--    Manager - The recently used resources manager
--    Tracing - Actions desired for tracing
--
   procedure Initialize
             (  Store   : not null access
                          Gtk_Persistent_Directory_Record'Class;
                Query   : Query_Handles.Handle;
                Manager : not null access
                          Gtk_Recent_Manager_Record'Class;
                Tracing : Traced_Actions
             );
--
-- Is_Directory -- Directory test
--
--    Store - The directory cache
--    Name  - An item name
--    Class - The class of
--
-- This function is used to determine if a persistent object  should  be
-- considered  a  directory.  Only  the  objects for which this function
-- returns true are scanned for having children. In fact any  persistent
-- object  may  serve  as  a  directory.  But  usually  there  is   some
-- restriction  put on. The default implementation returns true only for
-- the objects that have class "Directory".
--
-- Returns :
--
--    True if the item is a directory
--
   function Is_Directory
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Name  : String;
               Class : String
            )  return Boolean;
--
-- Read -- Overrides Gtk.Abstract_Browser...
--
   function Read
            (  Store : not null access Gtk_Persistent_Directory_Record
            )  return Directory_Item;
--
-- Rewind -- Overrides Gtk.Abstract_Browser...
--
   function Rewind
            (  Store : not null access Gtk_Persistent_Directory_Record;
               Path  : Item_Path
            )  return Directory_Item;
--
-- Set_Query -- Set the credentials dialog to use
--
--    Store - The directory cache
--    Query - A handle to
--
   procedure Set_Query
             (  Store : not null access Gtk_Persistent_Directory_Record;
                Query : Query_Handles.Handle
             );
------------------------------------------------------------------------
-- Gtk_Persistent_Storage_Tree_View -- Tree   view   of  the  persistent
--                                     storage.  The  widget  shows  the
-- directories of the persistent storage as a tree.
--
-- Style properties :
--
--    message-title       - The message dialog title. String.
--    name-conflict-error - The  message  shown when a name of a renamed
--                          object is already in use. String.
--    null-renaming-error - The message shown when a name  specified  is
--                          empty. String.
--    root-renaming-error - The message shown when a storage (root-level
--                          item) is renamed. Storage (data sources) can
--                          be renamed only by operating system specific
--                          means. String.
--    storage-error       - The message shown on  a  persistent  storage
--                          fault. String.
--
   type Gtk_Persistent_Storage_Tree_View_Record is
      new Gtk_Directory_Tree_View_Record with private;
   type Gtk_Persistent_Storage_Tree_View is
      access all Gtk_Persistent_Storage_Tree_View_Record'Class;
--
-- Get_Current_Object -- The current directory object
--
--    Store - The directory cache
--
-- Returns :
--
--    A handle to the directory, the handle is invalid if  no  directory
--    is selected
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--
   function Get_Current_Object
            (  Widget : not null access
                        Gtk_Persistent_Storage_Tree_View_Record
            )  return Deposit_Handle;
--
-- Get_Current_Storage -- The storage of the current directory
--
--    Widget - The widget
--
-- Returns :
--
--    A handle to the storage, the  handle  is  invalid  if  no  storage
--    selected
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--
   function Get_Current_Storage
            (  Widget : not null access
                        Gtk_Persistent_Storage_Tree_View_Record
            )  return Storage_Handle;
--
-- Get_Directory_Cache -- Get the items cache
--
--    Widget - The widget
--
-- Returns :
--
--    The cache of
--
   function Get_Directory_Cache
            (  Widget : not null access
                         Gtk_Persistent_Storage_Tree_View_Record
            )  return Gtk_Persistent_Directory;
--
-- Get_Icon -- Overrides Gtk.Abstract_Browser...
--
   function Get_Icon
            (  Widget       : not null access
                              Gtk_Persistent_Storage_Tree_View_Record;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Icon_Data;
--
-- Get_Persistent_Storage_Tree_View_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Persistent_Storage_Tree_View_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Widget   - The result
--    Store    - The directory store to use with
--    Selected - The path of the item to select
--
-- The parameter Selected is the path of an item to expand the directory
-- tree  to.  When the item is a directory it is selected. Otherwise its
-- parent  is selected. When Selected is an empty string then nothing is
-- done.
--
   procedure Gtk_New
             (  Widget   : out Gtk_Persistent_Storage_Tree_View;
                Store    : not null access
                           Gtk_Persistent_Directory_Record'Class;
                Selected : Item_Path := ""
             );
--
-- Initialize -- To be called by any derived type
--
--    Widget   - The widget initialized
--    Store    - The directory store to use with
--    Selected - The path of the item to select
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Persistent_Storage_Tree_View_Record'Class;
                Store  : not null access
                         Gtk_Persistent_Directory_Record'Class;
                Selected : Item_Path
             );
--
-- Name_Commit -- Overrides Gtk.Abstract_Browser...
--
   procedure Name_Commit
             (  Widget : not null access
                         Gtk_Persistent_Storage_Tree_View_Record;
                Old_Path : Item_Path;
                New_Name : Item_Name
             );
------------------------------------------------------------------------
-- Gtk_Persistent_Storage_Items_View_Record -- A  columned  list view of
--                                             persistent objects.
-- Style properties :
--
--    message-title       - The message dialog title. String.
--    name-conflict-error - The  message  shown when a name of a renamed
--                          object is already in use. String.
--    null-renaming-error - The message shown when a name  specified  is
--                          empty. String.
--    root-renaming-error - The message shown when a storage (root-level
--                          item) is renamed. Storage (data sources) can
--                          be renamed only by operating system specific
--                          means. String.
--    storage-error       - The message shown on  a  persistent  storage
--                          fault. String.
--
   type Gtk_Persistent_Storage_Items_View_Record is
      new Gtk_Directory_Items_View_Record with private;
   type Gtk_Persistent_Storage_Items_View is
      access all Gtk_Persistent_Storage_Items_View_Record'Class;
--
-- Get_Directory_Cache -- Get the items cache
--
--    Widget - The widget
--
-- Returns :
--
--    The cache of
--
   function Get_Directory_Cache
            (  Widget : not null access
                        Gtk_Persistent_Storage_Items_View_Record
            )  return Gtk_Persistent_Directory;
--
-- Get_Directory_Object -- The directory object
--
--    Store - The directory cache
--
-- Returns :
--
--    A handle to the directory, the handle is invalid if  no  directory
--    is selected
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--
   function Get_Directory_Object
            (  Widget : not null access
                        Gtk_Persistent_Storage_Items_View_Record
            )  return Deposit_Handle;
--
-- Get_Storage -- The storage of the current directory
--
--    Widget - The widget
--
-- Returns :
--
--    A handle to the storage, the  handle  is  invalid  if  no  storage
--    selected
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--
   function Get_Storage
            (  Widget : not null access
                        Gtk_Persistent_Storage_Items_View_Record
            )  return Storage_Handle;
--
-- Get_Icon -- Overrides Gtk.Abstract_Browser...
--
   function Get_Icon
            (  Widget       : not null access
                              Gtk_Persistent_Storage_Items_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Directory    : Boolean;
               Has_Children : Boolean
            )  return Icon_Data;
--
-- Get_Persistent_Storage_Items_View_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Persistent_Storage_Items_View_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Widget  - The result
--    Store   - The directory store or tree to use with
--    Columns - The number of columns
--    Current - The directory set
--
-- The parameter Current is the path  of  the  directory  of  items  the
-- result will contain. It can be empty string when the  root  directory
-- is the one.
--
   procedure Gtk_New
             (  Widget  : out Gtk_Persistent_Storage_Items_View;
                Store   : not null access
                          Gtk_Persistent_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path := ""
             );
   procedure Gtk_New
             (  Widget  : out Gtk_Persistent_Storage_Items_View;
                Tree    : not null access
                          Gtk_Persistent_Storage_Tree_View_Record'Class;
                Columns : Positive
             );
--
-- Initialize -- To be called by any derived type
--
--    Widget       - Being initialized
--    Store / Tree - The directory store or tree to use with
--    Columns      - The number of columns
--    Current      - The directory set
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record'Class;
                Store  : not null access
                         Gtk_Persistent_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path
             );
   procedure Initialize
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record'Class;
                Tree   : not null access
                         Gtk_Persistent_Storage_Tree_View_Record'Class;
                Columns : Positive
             );
--
-- Name_Commit -- Overrides Gtk.Abstract_Browser...
--
   procedure Name_Commit
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record;
                Index  : Positive;
                Name   : Item_Name
             );
------------------------------------------------------------------------
-- Gtk_Persistent_Storage_Browser_Record -- A persistent storage browser
--                                          widget.  The  widget  is   a
-- descendant of paned. One of its  children  is  a  tree  view  of  the
-- persistent  storage  directory  browsed another is a columned list of
-- persistent objects in the currently viewed directory.
--
   type Gtk_Persistent_Storage_Browser_Record is
      new Gtk_Paned_Record with private;
   type Gtk_Persistent_Storage_Browser is
      access all Gtk_Persistent_Storage_Browser_Record'Class;
--
-- Filter -- Items filtering
--
--    Widget    - The widget
--    Directory - True if Simple_Name is a directory name
--    Name      - The item name
--    Kind      - The item type
--
-- This function can be overridden to prevent some  objects  from  being
-- shown  in  the  directory items pane. The default implementation lets
-- all items in.
--
-- Returns :
--
--    True if the item has to be in the model
--
   function Filter
            (  Widget    : not null access
                           Gtk_Persistent_Storage_Browser_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean;
--
-- Get_Cache -- Get directory cache
--
--    Widget - The widget
--
-- Returns :
--
--    The directory cache
--
   function Get_Cache
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Persistent_Directory;
--
-- Get_Items_View -- Objects view widget
--
--    Widget - The widget
--
-- Returns :
--
--    The widget responsible for objects list representation
--
   function Get_Items_View
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Persistent_Storage_Items_View;
--
-- Get_Manager -- The recent resource manager
--
--    Widget - The widget
--
-- Returns :
--
--    The resource manager used with the widget
--
   function Get_Manager
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Recent_Manager;
--
-- Get_Tracing -- Current tracing status
--
--    Widget - The widget
--
-- Returns :
--
--    Current tracing status
--
   function Get_Tracing
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Traced_Actions;
--
-- Get_Tree_View -- Directory view widget
--
--    Widget - The widget
--
-- Returns :
--
--    The widget responsible for directory tree representation
--
   function Get_Tree_View
            (  Widget : not null access
                        Gtk_Persistent_Storage_Browser_Record
            )  return Gtk_Persistent_Storage_Tree_View;
--
-- Gtk_New -- Factory
--
--    Widget    - The result
--    Query     - The credentials dialog to use with
--    Path      - A path in the storage to browse
--    Columns   - The number of columns to use in the files list
--    Vertical  - The vertically vs. horizontally panned
--    Tree_Size - The maximal size of the tree pane upon start
--    List_Size - The maximal size of the list pane upon start
--    Store     - The cache to use
--    Manager   - The recently used resources manager
--    Tracing   - Desired tracing
--
-- When  the  parameter  Store  is  null  a new Gtk_Persistent_Directory
-- object  is  created.  Otherwise,  the  specified  one  is  used.  The
-- parameters Tree_Size and List_Size controls the  size  of  the  panes
-- upon  start. The panes will initially try to show all content without
-- scroll   bars   but  not  larger  than  the  size  specified  by  the
-- corresponding parameter.
--
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
             );
--
-- Initialize -- To be called by any derived type
--
--    Widget   - The result
--    Path     - A path in the storage to browse
--    Columns  - The number of columns to use in the files list
--    Vertical - The vertically vs. horizontally panned
--    Store    - The cache to use
--    Query    - The credentials dialog to use with
--    Manager  - The recently used resources manager
--    Tracing  - Desired tracing
--
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
             );
--
-- Set_Tracing -- Change tracing
--
--    Widget  - The widget
--    Tracing - To set
--
   procedure Set_Tracing
             (  Widget  : not null access
                          Gtk_Persistent_Storage_Browser_Record;
                Tracing : Traced_Actions
             );
private
   Path_Delimiters : constant Character_Set := To_Set ("/");

   type Root_Item is new Object.Entity with record
      Storage : Storage_Handle;
      URI     : Unbounded_String;
   end record;

   type Root_Item_Ptr is access Root_Item'Class;
   package Root_Item_Handles is
      new Object.Handle (Root_Item, Root_Item_Ptr);
   use Root_Item_Handles;
--
-- Root_Items_Tables -- The  table of the root items. It contains URI ->
--                      Storage  handle map. When the handle is invalid,
-- then browsing the storage pops a user credentials dialog up.
--
   package Root_Items_Tables is new Tables (Root_Item_Handles.Handle);
   use Root_Items_Tables;

   type Search_Mode is (None, Root, Catalogue);
--
-- Directory_Data -- The directory  browsing  data.  This  needs  to  be
--                   limited so it cannot be put into the object as  is,
--                   because the object itself is non-limited.
--
   type Directory_Data is new Object.Entity with record
      Mode      : Search_Mode := None;
      Manager   : Gtk_Recent_Manager;
      Query     : Query_Handles.Handle;
      Index     : Natural := 0;
      -- Mode = Root
      Root_List : Table;
      -- Mode = Catalogue
      Storage   : Storage_Handle;
      Parent    : Deposit_Handle;
      Folder    : Persistent.Catalogue.Set;
   end record;
   type Directory_Data_Ptr is access Directory_Data'Class;
--
-- Finalize -- Overrides Object...
--
   procedure Finalize (Data : in out Directory_Data);

   package Directory_Data_Handles is
      new Object.Handle (Directory_Data, Directory_Data_Ptr);
   use Directory_Data_Handles;

   type Gtk_Persistent_Directory_Record is
      new Gtk_Abstract_Directory_Record with
   record
      Data : Directory_Data_Handles.Handle;
   end record;
--
-- From_Escaped -- Path to name conversion
--
--    Path   - Name with escape sequences
--    Length - The length of the result (obtained from Scan)
--
-- Returns :
--
--    The name
--
   function From_Escaped (Name : Item_Path; Length : Natural)
      return Item_Name;
--
-- Remove_Item -- Remove a recent item
--
--    Manager - The recent items manager
--    URI     - Of the item
--
   procedure Remove_Item
             (  Manager : Gtk_Recent_Manager;
                URI     : Unbounded_String
             );
--
-- Scan -- Get prefix of an escaped path ended by a delimiter
--
--    Path        - The path
--    Pointer     - Points to the first character of prefix
--    Length      - The length of the prefix (unescaped)
--    Terminators - To stop at
--
-- Pointer is moved ater the last character of the prefix.
--
-- Exceptions :
--
--    Layout_Error - Pointer is not in Path'First..Path'Last + 1
--
   procedure Scan
             (  Path        : Item_Path;
                Pointer     : in out Integer;
                Length      : out Natural;
                Terminators : Character_Set
             );
--
-- Scan_Backwards -- Get suffix of an escaped path started by separator
--
--    Path    - The path
--    Pointer - Points to the first character following the suffix
--    Length  - The length of the suffix (unescaped)
--
-- Pointer is moved to the first character of the suffix.
--
-- Exceptions :
--
--    Layout_Error - Pointer is not in Path'First..Path'Last + 1
--
   procedure Scan_Backwards
             (  Path    : Item_Path;
                Pointer : in out Integer;
                Length  : out Natural
             );
--
-- To_Escaped -- Convert a name to path
--
--    Name - To convert
--
-- Returns :
--
--    The content of Name with delimiters escaped
--
   function To_Escaped (Name : Item_Name) return Item_Path;

   type Gtk_Persistent_Storage_Tree_View_Record is
      new Gtk_Directory_Tree_View_Record with null record;
--
-- Say -- Pop message dialog
--
--    Widget - The widget
--    Name   - The style property name of the message text
--    Reason - Error text (if not empty)
--
   procedure Say
             (  Widget : not null access
                         Gtk_Persistent_Storage_Tree_View_Record;
                Name   : UTF8_String;
                Reason : UTF8_String := ""
             );

   type Gtk_Persistent_Storage_Items_View_Record is
      new Gtk_Directory_Items_View_Record with null record;
--
-- Say -- Pop message dialog
--
--    Widget - The widget
--    Name   - The style property name of the message text
--    Reason - Error text (if not empty)
--
   procedure Say
             (  Widget : not null access
                         Gtk_Persistent_Storage_Items_View_Record;
                Name   : UTF8_String;
                Reason : UTF8_String := ""
             );

   type Gtk_Persistent_Storage_Objects_View_Record is
      new Gtk_Persistent_Storage_Items_View_Record with
   record
      Browser : Gtk_Persistent_Storage_Browser;
   end record;
   type Gtk_Persistent_Storage_Objects_View is
      access all Gtk_Persistent_Storage_Objects_View_Record'Class;
--
-- Filter -- Overrides Gtk.Abstract_Browser...
--
   function Filter
            (  Widget    : not null access
                           Gtk_Persistent_Storage_Objects_View_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean;

   type Gtk_Persistent_Storage_Browser_Record is
      new Gtk_Paned_Record with
   record
      Cache : Gtk_Persistent_Directory;
      Tree  : Gtk_Persistent_Storage_Tree_View;
      Items : Gtk_Persistent_Storage_Objects_View;
   end record;
--
-- Install -- Class properties
--
--    Class - The object to install properties into
--
   procedure Install (Class : Ada_GObject_Class);

end Gtk.Persistent_Storage_Browser;
