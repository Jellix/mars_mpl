--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Abstract_Browser                        Luebeck            --
--  Interface                                      Autumn, 2007       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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
--  This  package  provides  tree model and specialized tree widgets for
--  creating browsers, like files directory browser,  for  example.  The
--  external source browsed is cached as necessary into  a  tree  store.
--  Items  of  the structure are accessible through the iterators of the
--  store.
--
--  The following primitive operations  are  abstract  and  need  to  be
--  overridden:
--
--     Compare       - To sort directory items
--     Get_Directory - Get the directory part from a full name (path)
--     Get_Name      - Get the simple name part of a path
--     Get_Path      - Composes a path
--     Read          - Get item and advance to the next one
--     Rewind        - Set read cursor to the first directory item
--
--  The following primitive operations have a predefined implementation,
--  but likely should be overridden:
--
--     Get_Policy    - Get caching policy of an item by its full name
--
--  The   primitive   operations   dealing   with  the  item  names  are
--  Get_Full_Name,   Get_Simple_Name,    Get_Directory.    Get_Full_Name
--  composes  the fully qualified name of an item from its directory and
--  simple  name  parts.  The  full name has to be unique for each item.
--  Get_Directory extracts the full name of the directory (parent) of an
--  item from the full name of the latter. Get_Simple_Name extracts  the
--  simple  name  of. Simple names are unique within the same directory.
--  It is not required that the directory name be a prefix of  the  full
--  name,  or  that  the  simple  name  be  a  postfix  of.  They can be
--  completely unrelated.
--     The primitive operations Rewind and Read are used  for  searching
--  directories. It warrantied that only one search it active at a time.
--
with Ada.Calendar;                   use Ada.Calendar;
with Gdk.Event;                      use Gdk.Event;
with Gdk.Pixbuf;                     use Gdk.Pixbuf;
with Gdk.Types;                      use Gdk.Types;
with GLib.Values;                    use GLib.Values;
with Gtk.Paned;                      use Gtk.Paned;
with Gtk.Cell_Layout;                use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;              use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf;       use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;         use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;
with Gtk.Tree_Model.Columned_Store;  use Gtk.Tree_Model.Columned_Store;
with Gtk.Tree_Model.Extension_Store; use Gtk.Tree_Model.Extension_Store;
with Gtk.Tree_Selection;             use Gtk.Tree_Selection;
with Gtk.Tree_Sortable;              use Gtk.Tree_Sortable;
with Gtk.Tree_Store;                 use Gtk.Tree_Store;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Widget;                     use Gtk.Widget;

with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Gtk.Handlers;

package Gtk.Abstract_Browser is
   pragma Elaborate_Body (Gtk.Abstract_Browser);
--
-- Caching_Policy -- Of a directory item
--
--    Cache_Never    - The  directory  is  not cached until its children
--                     get visible. This policy  can  be  used  for  the
--                     directories  which  caching might be expensive or
--                     else  not  always  possible.  An attempt to do it
--                     will  be  postponed  until  the user would try to
--                     browse   the   tree  item  corresponding  to  the
--                     directory;
--    Cache_Expanded - The directory is cached first when its parent get
--                     expanded. This ensures that the view of any  item
--                     is always consistent. This  policy  is  used  for
--                     directories which are always browsable;
--    Cache_Ahead    - The  directory  is  cached  immediately.  If  all
--                     directories  of  a path have this policy the tree
--                     along  this  path will be cached in advance. This
--                     policy  is  used  when the directory tree becomes
--                     available only once, or when it  would  makes  no
--                     sense to postone its caching.
--
-- Usually Cache_Expanded is a reasonable policy. An implementation  may
-- wish to change the policy for some items. For instance the policy  of
-- a  removable  disk  could  be  Cache_Never.  The policy of a database
-- connection might be Cache_Ahead, etc.
--
   type Caching_Policy is (Cache_Never, Cache_Expanded, Cache_Ahead);

   type Selection is array (Positive range <>) of Positive;
   type Item_Name is new UTF8_String;
   type Item_Path is new UTF8_String;
   type Item_Type is new UTF8_String;
--
-- Directory_Item -- Directory item description
--
--    Directory - True if the item is a directory;
--    Policy    - The browsing policy of the item;
--    Name      - The name of the item;
--    Kind      - This  can  be  used to do things like indicating icons
--                next to the item name in a tree view.
--
   type Directory_Item
        (  Name_Length : Natural;
           Kind_Length : Natural
        )  is
   record
      Policy    : Caching_Policy;
      Directory : Boolean;
      Name      : Item_Name (1..Name_Length);
      Kind      : Item_Type (1..Kind_Length);
   end record;
--
-- Directory_Entered -- Constand  usualy  returned  by  Rewind (see), to
--                      indicate   that   the  directory  items  can  be
--                      enumerated.
--
   Directory_Entered : constant Directory_Item :=
                          (  Policy      => Cache_Ahead,
                             Directory   => False,
                             Name_Length => 0,
                             Kind_Length => 0,
                             Name        => "",
                             Kind        => ""
                          );
--
-- Icon_Type -- The type of icon
--
--    Stock_ID - The icon is identified the stock ID
--    GIcon    - The icon is an image object GIcon
--    Pixbuf   - The icon is an image object Pixbuf
--    Themed   - The name of the themed icon
--
   type Icon_Type is (Stock_ID, GIcon, Pixbuf, Themed);
--
-- Icon_Data -- Icon description
--
   type Icon_Data (Kind : Icon_Type; Length : Natural) is record
      case Kind is
         when Stock_ID | Themed =>
            Name : String (1..Length);
         when GIcon =>
            Icon : GObject;
         when Pixbuf =>
            Image : Gdk_Pixbuf;
      end case;
   end record;
------------------------------------------------------------------------
-- Gtk_Abstract_Directory_Record -- An  abstract  model  bound  to  some
--                                  external directory structure
--
-- Signals : (additional to Gtk_Tree_Model)
--
--    item-inserted - An  item  was  added  to  the  model.  The   first
--                    parameter   is   an  unfiltered  iterator  to  the
--                    inserted   row.   The  second   parameter  is  the
--                    unfiltered path of.
--    item-renamed  - An  item  was  renamed.  The first parameter is an
--                    unfiltered   iterator   to  the  row.  The  second
--                    parameters is the old item name.
--    item-deleted  - An  item  was  deleted.  The  first  parameter  is
--                    unfiltered path of deleted item.
--    item-deleting - An  item  is  about  to  be  deleted.  The   first
--                    parameter is an unfiltered  iterator  to  the  row
--                    being deleted.
--    progress      - State  of a caching operation. The first parameter
--                    is the path of the item being cached.  The  second
--                    parameter   is   GDouble  in  the  range  0.0..1.0
--                    reflecting the current caching operation state;
--    read-error    - An  error  occured  while  reading external store.
--                    Such errors propagating out of Read operation lead
--                    to removing the corresponding item from the cache.
--                    The  first  signal  parameter  is  the   exception
--                    message string. The second parameter is  the  path
--                    of  the  directory  containing the item. A handler
--                    may show it in a messages window for instance;
--    refreshed    -  The  store  was  refreshed. The first parameter is
--                    the refreshed path;
--    rewind-error  - An error occured while preparing an external store
--                    for reading a directory of items. It is propagated
--                    out  of  Rewind  operation. The first parameter is
--                    the exception message string. The second parameter
--                    is the directory path.
--
   type Gtk_Abstract_Directory_Record is
      abstract new Gtk_Abstract_Model_Record with private;
   type Gtk_Abstract_Directory is
      access all Gtk_Abstract_Directory_Record'Class;
--
-- Traced_Actions -- Actions chosen for tracing
--
   type Traced_Actions is mod 2**9;
   Trace_Nothing          : constant Traced_Actions := 0;
   Trace_Read_Items       : constant Traced_Actions := 2**0;
   Trace_Read_Directory   : constant Traced_Actions := 2**1;
   Trace_IO               : constant Traced_Actions := 2**2;
   Trace_Expand_Directory : constant Traced_Actions := 2**3;
   Trace_Set_Directory    : constant Traced_Actions := 2**4;
   Trace_Tree_Filter      : constant Traced_Actions := 2**5;
   Trace_Key_Presses      : constant Traced_Actions := 2**6;
   Trace_To_Output_Only   : constant Traced_Actions := 2**7;
   Trace_To_Both          : constant Traced_Actions := 2**8;
   Trace_Cache            : constant Traced_Actions := 2**9;
   Trace_All              : constant Traced_Actions :=
                                        Traced_Actions'Last;

   Abstract_Directory_Class_Name : constant String :=
      "GtkAbstractDirectory";
--
-- Cache -- A path
--
--    Store - The widget
--    Item  - The path of the item to naviate to
--
-- This  procedire  caches the path to the item. The directory structure
-- is queried as necessary and cached. The root directory full  name  is
-- empty string.
--
   procedure Cache
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Item  : Item_Path
             );
--
-- Changed -- Notification
--
--    Store     - The directory store
--    Directory - The path of a modified directory
--
-- This  procedure  synchronizes  the  cache  with  the  directory.  The
-- parameter Directory is the full name of the directory changed.
--
   procedure Changed
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Directory : Item_Path
             );
--
-- Compare -- Items ordering
--
--    Store     - The directory store
--    Directory - The path of the parent directory
--    A, B      - Items to compare
--    By_Name   - Ignore type and directory flag when comparing
--
-- It is warrantied that A and B are siblings.
--
-- Returns :
--
--    The comparison result
--
   function Compare
            (  Store : not null access Gtk_Abstract_Directory_Record;
               Directory : Item_Path;
               A, B      : Directory_Item;
               By_Name   : Boolean
            )  return Row_Order is abstract;
--
-- Created -- Item insertion notification
--
--    Store     - The directory store
--    Directory - The path of the parent directory
--    Item      - A description of the item created
--
-- This procedure is called to synchronize the store  when  a  new  item
-- gets inserted from outside. The parameter  Item  describes  the  item
-- created. The parameter Directory is the path of the directory of. For
-- the  root  directory  items  the  parameter  is  an  empty string. An
-- alternative to Created is  refreshing  the  cache  at  the  directory
-- containing   inserted   item  (see  Changed).  The  latter  might  be
-- preferable upon a massive items insertions.
--
   procedure Created
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Directory : Item_Path;
                Item      : Directory_Item
             );
--
-- Deleted -- Item deletion notification
--
--    Store - The directory store
--    Item  - The path of a deleted item
--
-- This  procedure  is called to synchronize the store when an item gets
-- deleted from outside. Item is the path of the deleted item.
--
   procedure Deleted
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Item  : Item_Path
             );
--
-- Finalize -- Custom finalization
--
--    Model - A pointer to
--
-- This  procedure  is  called upon object destruction. The override, if
-- any, shall call the parent's version.
--
   overriding
      procedure Finalize
                (  Model : not null access Gtk_Abstract_Directory_Record
                );
--
-- Get_Cached -- By its path
--
--    Store - The directory store
--    Path  - Of the item to find
--
-- The  function searches through the cache for an item specified by its
-- path. The efficiency of this operation is O(n).
--
-- Returns :
--
--    The iterator to Item or Null_Iter
--
-- Exceptions :
--
--    Constraint_Error - There is no such item cached
--    Name_Error       - Illegal path
--
   function Get_Cached
            (  Store : not null access Gtk_Abstract_Directory_Record;
               Path  : Item_Path
            )  return Directory_Item;
--
-- Get_Depth -- Of an item
--
--    Store - The directory store
--    Item  - The path of
--
-- Returns :
--
--    The path of the directory
--
   function Get_Depth
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class
            )  return Natural;
--
-- Get_Directory -- Of an item
--
--    Store - The directory store
--    Item  - The path of
--
-- Returns :
--
--    The path of the directory
--
-- Exception :
--
--    Name_Error - Item is a root directory item
--
   function Get_Directory
            (  Store : not null access Gtk_Abstract_Directory_Record;
               Item  : Item_Path
            )  return Item_Path is abstract;
--
-- Get_Name -- Get name from path
--
--    Store - The directory store
--    Item  - The path of
--
-- Returns :
--
--    The name of
--
   function Get_Name
            (  Store : not null access Gtk_Abstract_Directory_Record;
               Item  : Item_Path
            )  return Item_Name is abstract;
--
-- Get_Path -- Of an item from parent's path and item name
--
--    Store     - The directory store
--    Directory - The path of the directory
--    Name      - The name of an item in the directory
--
-- Returns :
--
--    The path of
--
   function Get_Path
            (  Store     : not null access
                           Gtk_Abstract_Directory_Record;
               Directory : Item_Path;
               Name      : Item_Name
            )  return Item_Path is abstract;
--
-- Get_Tracing -- Current tracing status
--
--    Store - The directory store
--
-- Returns :
--
--    Current tracing status
--
   function Get_Tracing
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class
            )  return Traced_Actions;
--
-- Get_Tree_Store -- Get unfiltered tree model
--
--    Store - The directory store
--
-- This  function returns a tree store of the cached directory items. It
-- contains all items of, both directories and leaf items. The model can
-- be  modified in order to add and remove items, though a better way to
-- do it is to use Created, Deleted and Refresh calls.
--
-- Returns :
--
--    The full model of the cache
--
   function Get_Tree_Store
            (  Store : not null access Gtk_Abstract_Directory_Record
            )  return Gtk_Tree_Store;
--
-- Initialize -- To be called by any derived type
--
--    Store - A newly created object to initialize
--
   procedure Initialize
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class
             );
--
-- Progress -- Progress indication
--
--    Store     - The directory store
--    Directory - Of the folder currently being cached
--    State     - The progress state 0.0..1.0
--
-- The procedure is called to indicate  progress  of  a  folder  caching
-- opearation. The default implementation emits the signal "progress".
--
   procedure Progress
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record;
                Directory : Item_Path;
                State     : GDouble
             );
--
-- Read -- The next item of a directory
--
--    Store - The directory store
--
-- This function returns the next item of the directory for which Rewind
-- was  called before. The result is the item found. The internal cursor
-- is  advanced to the item. When an implementation detects an error, it
-- has several options to handle this. It can
--
-- (o)  raise End_Error and ignore the rest of the directory;
-- (o)  return a special item and raise End_Error by the next call;
-- (o)  raise  Data_Error  with  the  effect  of  removing the directory
--      content from the cache.
--
-- Giving any error messages is the responsibility of Read.
--
-- Returns :
--
--    The item description
--
-- Exceptions :
--
--    End_Error  - No more items found
--    Data_Error - An error, the directory is removed from the cache
--
   function Read
            (  Store : not null access Gtk_Abstract_Directory_Record
            )  return Directory_Item is abstract;
--
-- Read_Error -- Emits read-error signal
--
--    Store - The directory store
--    Text  - The message (error reason)
--    Path  - The path (of the directory read)
--
   procedure Read_Error
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Text  : UTF8_String;
                Path  : Item_Path
             );
--
-- Renamed -- Item renaming notification
--
--    Store    - The directory store
--    Old_Path - The renamed item path
--    New_Name - The new name of
--
-- This procedure  synchronizes  the  store  when  a  cached  item  gets
-- renamed.  The parameter Old_Path is the path of the item. New_Name is
-- the new name of the item. The procedure assumes that renaming happens
-- in the same directory.
--
-- Exceptions :
--
--    Constraint_Error - Old_Path does not specify an item
--
   procedure Renamed
             (  Store    : not null access
                           Gtk_Abstract_Directory_Record'Class;
                Old_Path : Item_Path;
                New_Name : Item_Name
             );
--
-- Rewind -- Set the cursor to first item of a disrectory
--
--    Widget    - The widget
--    Directory - The path of the directory
--
-- This  function  is  called  to  iterate  items  of  a  directory.  An
-- implementation typically  prepares  iteration  of  Directory.  It  is
-- guaranteed that no nested or multiple  iterations  happen.  When  the
-- parameter  Directory  is  an  empty  string, the cursor is set to the
-- first  root  item (of the root directory). Otherwise it is set to the
-- first item in the directory. The cursor is implementation maintained.
-- The  function  returns the new state of the directory being iterated.
-- Usually   the   state   does   not  change.  But  in  some  cases  an
-- implementation might wish to change the directory type  in  order  to
-- show a different icon for a mounted volume, for example. The result's
-- policy is interpreted as follows:
--
--    Cache_Ahead    - The  directory  will be cached and its state does
--                     not  change.  This  is  the  most frequently used
--                     policy. All other fields  are  ignored.  One  can
--                     also use the constant value Directory_Entered for
--                     this purpose;
--    Cache_Expanded - This is like above, but the  directory  name  and
--                     type  replace  the  values  in  the  cache.   The
--                     field Directory is ignored;
--    Cache_Never    - This  prevents  of  the  directory iteration. The
--                     directory  name, type replace the current ones in
--                     the  cache.  When  the  discriminant Directory is
--                     changed to false, the directory is replaced by an
--                     item in the cache.
--
-- When  access  to  the  directory  requires  credentials  or some user
-- action, then Rewind is a place to ask the user for. When such request
-- fails,  due  to  cancellation,  for  example, then Cache_Never can be
-- returned.
--
-- Returns :
--
--    The directory description (updated)
--
-- Exceptions :
--
--    Data_Error - An error, the directory is not cached
--
   function Rewind
            (  Store : not null access Gtk_Abstract_Directory_Record;
               Directory : Item_Path
            )  return Directory_Item is abstract;
--
-- Rewind_Error -- Emits rewind-error signal
--
--    Store - The directory store
--    Text  - The message (error reason)
--    Path  - The path (of the directory)
--
   procedure Rewind_Error
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Text  : UTF8_String;
                Path  : Item_Path
             );
--
-- Set_Tracing -- Change tracing
--
--    Store   - The directory store
--    Tracing - To set
--
   procedure Set_Tracing
             (  Store   : not null access
                          Gtk_Abstract_Directory_Record'Class;
                Tracing : Traced_Actions
             );
--
-- Trace -- The procedure used for tracing
--
--    Store - The directory store
--    Depth - Of the caller
--    Text  - Tracing text
--
   procedure Trace
             (  Store : not null access Gtk_Abstract_Directory_Record;
                Depth : Natural;
                Text  : String
             );
--
-- Trace -- The contents of a model used for abstract directory
--
--    Model   - Containing data of a directory
--    Tracing - Tracing mode
--
   procedure Trace
             (  Model   : not null access
                          Gtk_Root_Tree_Model_Record'Class;
                Tracing : Traced_Actions := Trace_To_Both
             );
------------------------------------------------------------------------
-- Gtk_Directory_Tree_View -- Specialized tree view, the tree view shows
--                            the directories of an  abstract  directory
-- store. It does not show items of. The directories are indicated as  a
-- tree. The item type is used as the name of a stock item to  be  shown
-- left of the directory name. For the name the simple  name  is  shown.
-- The tree view supports single selection.
--
   type Gtk_Directory_Tree_View_Record is
      new Gtk_Tree_View_Record with private;
   type Gtk_Directory_Tree_View is
      access all Gtk_Directory_Tree_View_Record'Class;
--
-- Get_Cache -- Get the items cache
--
--    Widget - The widget
--
-- Returns :
--
--    The cache of
--
   function Get_Cache
            (  Widget : not null access Gtk_Directory_Tree_View_Record
            )  return Gtk_Abstract_Directory;
--
-- Get_Current_Directory -- The currently selected directory
--
--    Widget - The widget
--
-- Returns :
--
--    The path of the selected directory
--
-- Exceptions :
--
--    Name_Error - No directory selected
--
   function Get_Current_Directory
            (  Widget : not null access Gtk_Directory_Tree_View_Record
            )  return Item_Path;
--
-- Get_Directory_Tree_View_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Directory_Tree_View_Type return Gtk_Type;
--
-- Get_Icon -- Get the icon to show for a directory
--
--    Widget       - The widget
--    Kind         - Of the directory
--    Expaded      - Row expanded state
--    Has_Children - True when directory content is cached
--    Topmost      - True when the directory is a root
--
-- The default implementation returns Kind except for the case when Kind
-- is "gtk-directory" and Expanded is True.  In  that  case  it  returns
-- "gtk-open".
--
-- Returns :
--
--    The icon of the directory icon
--
   function Get_Icon
            (  Widget       : not null access
                              Gtk_Directory_Tree_View_Record;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Icon_Data;
--
-- Get_Name -- Get the name to show for a directory
--
--    Widget       - The widget
--    Name         - Of the directory
--    Kind         - Of the directory
--    Expaded      - Row expanded state
--    Has_Children - True when directory content is cached
--    Topmost      - True when the directory is a root
--
-- The default implementation returns Name.
--
-- Returns :
--
--    The name to show
--
   function Get_Name
            (  Widget       : not null access
                              Gtk_Directory_Tree_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Item_Name;
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
             (  Widget   : out Gtk_Directory_Tree_View;
                Store    : not null access
                           Gtk_Abstract_Directory_Record'Class;
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
             (  Widget   : not null access
                           Gtk_Directory_Tree_View_Record'Class;
                Store    : not null access
                           Gtk_Abstract_Directory_Record'Class;
                Selected : Item_Path
             );
--
-- Is_Editable -- Get the current name editing mode
--
--    Widget - The widget initialized
--
-- Returns :
--
--    True if directory names are editable
--
   function Is_Editable
            (  Widget : not null access Gtk_Directory_Tree_View_Record
            )  return Boolean;
--
-- Name_Commit -- Validation of a directory item name
--
--    Widget   - The widget
--    Old_Path - The old directory path
--    New_Name - The new name of the directory
--
-- This procedure is called when a directory name editing is  done.  For
-- this the widget must have directory names editing enabled by  a  call
-- to Set_Editable. The parameter Old_Name identifies the directory. The
-- parameter New_Name specifies a new name for it.  When  the  directory
-- name  is valid the implementation can change it outside the cache and
-- when that succeeds notify the cache as follows:
--
--    Renamed (Get_Cache (Widget), Old_Path, New_Name);
--
-- This  replaces  the  old  item with the updated one in the cache. The
-- default implementation does nothing, i.e. it rejects editing.
--
   procedure Name_Commit
             (  Widget   : not null access
                           Gtk_Directory_Tree_View_Record;
                Old_Path : Item_Path;
                New_Name : Item_Name
             )  is null;
--
-- Set_Current_Directory -- Select a directory
--
--    Widget    - The widget
--    Directory - The path of a directory to select
--
-- This procedure selects Directory in the widget. The directory tree is
-- expanded  as  necessary. Any selected before directory is deselected.
-- The selected one is scrolled into the view. The path to Directory  is
-- cached  when  required.  When Directory does not exist  the procedure
-- stops by its most nested existing parent.
--
   procedure Set_Current_Directory
             (  Widget    : not null access
                            Gtk_Directory_Tree_View_Record;
                Directory : Item_Path
             );
--
-- Set_Editable -- Set editable mode of the directory names
--
--    Widget   - The widget initialized
--    Editable - The mode to set
--
-- By default the directory names listed by the widget are not editable.
-- When  Set_Editable  is called with Editable set to true, the operator
-- can edit the names. The name input is confirmed through the primitive
-- operation Name_Commit.
--
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Directory_Tree_View_Record;
                Editable : Boolean
             );
------------------------------------------------------------------------
-- Gtk_Directory_Items_View_Record -- A specialized  tree  view  of  the
--                                    directory    items.    The    view
-- represents  items  of  one  directory shown in columns. The number of
-- columns is specified upon creation. The tree view  supports  multiple
-- items selection and filtering items.
--
-- Signals (additional to tree view) :
--
--    directory-changed
--    selection-changed
--
   type Gtk_Directory_Items_View_Record is
      new Gtk_Tree_View_Record with private;
   type Gtk_Directory_Items_View is
      access all Gtk_Directory_Items_View_Record'Class;

   Directory_Items_Class_Name : constant String :=
                                   "GtkDirectoryItemsView";
--
-- Activated -- Item activation
--
--    Widget - The directory view
--    Index  - Of the item activated (1..)
--
-- This  procedure  is  called  upon  activation of an item. That is for
-- example  when  the  user  clicks  doubly  on  an  item.  The  default
-- implementation  when  the  item is a directory selects it in the tree
-- view which in turn causes the widget to  show  the  directory  items.
-- When the item is not a directory and editing is enabled then  editing
-- starts on it. Otherwise it does nothing.
--
   procedure Activated
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : Positive
             );
--
-- Directory_Changed -- Notification
--
--    Widget - The directory items view
--
-- This procedure  is  called  upon  switching  the  widget  to  another
-- directory. The default implementation emits directory-changed signal.
--
   procedure Directory_Changed
             (  Widget : not null access Gtk_Directory_Items_View_Record
             );
--
-- Filter -- Items filtering
--
--    Widget    - The directory view
--    Directory - True if Simple_Name is a directory name
--    Name      - The item name
--    Kine      - The item type
--
-- This function can be overridden to  prevent  some  items  from  being
-- visible. The default implementation lets all items in.
--
-- Returns :
--
--    True if the item has to be in the model
--
   function Filter
            (  Widget    : not null access
                           Gtk_Directory_Items_View_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean;
--
-- Finalize -- Destruction
--
--    Widget - The widget
--
-- This  subprogram  is  called upon destruction. It has to be called it
-- from its overriding.
--
   procedure Finalize
             (  Widget : not null access Gtk_Directory_Items_View_Record
             );
--
-- Get_Cache -- Get the items cache
--
--    Widget - The directory items view
--
-- Returns :
--
--    The cache of
--
   function Get_Cache
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Abstract_Directory;
--
-- Get_Current -- Item by position
--
--    Widget - The directory items view
--
-- Returns :
--
--    [0]  No current item
--    [+]  The item index
--
   function Get_Current
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural;
--
-- Get_Columns -- Of an item by its index
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    The column number 1..
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Column
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Positive;
--
-- Get_Columns -- The number of columns
--
--    Widget - The directory items view
--
-- Returns :
--
--    The number of columns
--
   function Get_Columns
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Positive;
--
-- Get_Directory -- Get the current directory
--
--    Widget - The directory items view
--
-- Returns :
--
--    The path of
--
   function Get_Directory
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Item_Path;
--
-- Get_Directory_Items_View_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Directory_Items_View_Type return Gtk_Type;
--
-- Get_Directory_Size -- Get the total number of items
--
--    Widget - The directory items view
--
-- Returns :
--
--    The number of items
--
   function Get_Directory_Size
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural;
--
-- Get_Directory_Tree_View -- Get the directory tree view widget
--
--    Widget - The directory items view
--
-- The result is null if no directory tree  view  widget  is  associated
-- with the widget.
--
-- Returns :
--
--    The associated widget performing directory tree view
--
   function Get_Directory_Tree_View
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Directory_Tree_View;
--
-- Get_Icon -- Get the icon to show for a directory item
--
--    Widget       - The widget
--    Name         - Of the item
--    Kind         - Of the item
--    Directory    - True if the item is a directory
--    Has_Children - True when directory content is cached
--
-- The  default  implementation  returns  Kind converted to string. Note
-- that  an  implementation  should  take  into  account  that  Get_Icon
-- receives  empty Name and Kind when called for empty cells of columned
-- view.  In  such  cases  it  is  reasonable  to return empty string in
-- response.
--
-- Returns :
--
--    The directory icon
--
   function Get_Icon
            (  Widget       : not null access
                              Gtk_Directory_Items_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Directory    : Boolean;
               Has_Children : Boolean
            )  return Icon_Data;
--
-- Get_Index -- Item index by name
--
--    Widget - The directory items view
--    Name   - Of the item
--
-- The function searches through the cache only. The efficiency of  this
-- operation is O(n).
--
-- Returns :
--
--    The index of an item with Name or 0
--
   function Get_Index
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Name   : Item_Name
            )  return Natural;
--
-- Get_Index -- Item index by row and column
--
--    Widget - The directory items view
--    Row    - The item row 1..
--    Column - The item column 1..
--
-- Returns :
--
--    [0]  Row and Column do not identify an item
--    [+]  The item index
--
   function Get_Index
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Row    : Positive;
               Column : Positive
            )  return Natural;
--
-- Get_Name -- Of an item by its index
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    The name of the item, Index refers to
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Name
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Item_Name;
--
-- Get_Path -- Of an item by its name
--
--    Widget - The directory items view
--    Name   - The name of
--
-- Returns :
--
--    The path of the item
--
   function Get_Path
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Name   : Item_Name
            )  return Item_Path;
--
-- Get_Path -- Of an item by its index
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    The path the item, Index refers to
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Path
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Item_Path;
--
-- Get_Row -- Of an item by its index
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    The row number 1..
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Row
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Positive;
--
-- Get_Selection -- Get the total number of selected items
--
--    Widget - The directory items view
--
-- Returns :
--
--    The indices of selected items
--
   function Get_Selection
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Selection;
--
-- Get_Selection_Mode -- Get the current selection mode
--
--    Widget - The directory items view
--
-- Returns :
--
--    The selection mode
--
   function Get_Selection_Mode
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Selection_Mode;
--
-- Get_Selection_Size -- Get the total number of selected items
--
--    Widget - The directory items view
--
-- Returns :
--
--    The number of items selected
--
   function Get_Selection_Size
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural;
--
-- Get_Type -- Of an item by its index
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    The type of the item, Index refers to
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Type
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Item_Type;
--
-- Get_Visible_Height -- The number of visible items in a column
--
--    Widget - The directory items view
--
-- This function returns the number of items currently  visible  in  the
-- leftmost visible column of the widget. The widget has to be realized.
--
-- Returns :
--
--    The page size
--
   function Get_Visible_Height
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Natural;
--
-- Gtk_New -- Factory
--
--    Widget       - The result
--    Store / Tree - The directory store or tree to use with
--    Columns      - The number of columns
--    Current      - The directory set
--
-- The parameter Current is the path  of  the  directory  of  items  the
-- result will contain. It can be empty string when the  root  directory
-- is the one.
--
   procedure Gtk_New
             (  Widget  : out Gtk_Directory_Items_View;
                Store   : not null access
                          Gtk_Abstract_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path := ""
             );
   procedure Gtk_New
             (  Widget  : out Gtk_Directory_Items_View;
                Tree    : not null access
                          Gtk_Directory_Tree_View_Record'Class;
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
             (  Widget  : not null access
                          Gtk_Directory_Items_View_Record'Class;
                Store   : not null access
                          Gtk_Abstract_Directory_Record'Class;
                Columns : Positive;
                Current : Item_Path
             );
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Directory_Items_View_Record'Class;
                Tree    : not null access
                          Gtk_Directory_Tree_View_Record'Class;
                Columns : Positive
             );
--
-- Is_Directory -- Test if an item is a directory
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    True if Index refers to a directory item
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Is_Directory
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Boolean;
--
-- Is_Editable -- Get the current name editing mode
--
--    Widget - The directory items view
--
-- Returns :
--
--    True if item names are editable
--
   function Is_Editable
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Boolean;
--
-- Is_Selected -- Get selection state
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    True if the item is selected
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Is_Selected
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Boolean;
--
-- Locate -- Item by position
--
--    Widget - The directory items view
--    X, Y   - Coordinates (relative to the widget's bin_window)
--
-- This function determines the index of an item under the  cursor.  The
-- index can then be used to access its data.
--
-- Returns :
--
--    [0]  X, Y do not specify an item
--    [+]  The item index
--
   function Locate
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               X, Y   : GDouble
            )  return Natural;
--
-- Move -- To an item by position
--
--    Widget    - The directory items view
--    Changed   - Set to true if position was changed, false otherwise
--    Modifier  - The modifier used while moving
--    To        - The destination (absolute position)
--    By        - The destination relative increment
--    Fixed_Row - Retains the row of To if true
--
-- This procedure moves current position to the point To +  By.  Nothing
-- happens  if  To is less than 1. When incrementing using By would lead
-- to an non-existing position the result is saturated  to  an  existing
-- one. The parameter Modifier controls selection:
--
--    Shift_Mask  Control_Mask
--    Reset       Reset          Single selection of the target
--    Reset       Set            Toggling selection of
--    Set         Reset          Range selection of nearest to To+By
--    Set         Set            Range accumulation of nearest to To+By
--
-- The parameters Fixed_Row and Fixed_Column when True prevent  the  row
-- or column of To from being left.
--
   procedure Move
             (  Widget       : not null access
                               Gtk_Directory_Items_View_Record;
                Changed      : out Boolean;
                Modifier     : Gdk_Modifier_Type;
                To           : Natural;
                By           : Integer := 0;
                Fixed_Row    : Boolean := False;
                Fixed_Column : Boolean := False
             );
--
-- Name_Commit -- Validation of an item name
--
--    Widget - The directory items view
--    Index  - Of the item 1..
--    Name   - The new name
--
-- This procedure is called when item name editing is done. For this the
-- widget  must  have  item  names  editing  enabled  by   a   call   to
-- Set_Editable. The parameter Index identifies the item. The  parameter
-- Name specifies a new name for it. When the item  name  is  valid  the
-- implementation can change it outside the cache and when that succeeds
-- notify the cache as follows:
--
--    Renamed (Widget, Index, Name);
--
-- This  replaces  the  old  item with the updated one in the cache. The
-- default implementation does nothing, i.e. it rejects editing.
--
   procedure Name_Commit
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : Positive;
                Name   : Item_Name
             )  is null;
--
-- Input_Event -- Key and button press handler
--
--    Widget - The directory view
--    Index  - Of the item activated (1..)
--    Event  - The event
--
-- This function  is  called  to  handle  key  and  button  presses  not
-- processed by the widget. For instance to handle  right  button  click
-- event. The default implementation returns False.
--
-- Returns :
--
--    False if the event has to be passed to other handlers
--
   function Input_Event
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive;
               Event  : Gdk_Event
            )  return Boolean;
--
-- Refilter -- The directory items
--
--    Widget - The directory items view
--
-- This procedure is called when the item filtering  logic  changes.  It
-- forces scanning the directory items to decide whether they should  be
-- visible.
--
   procedure Refilter
             (  Widget : not null access Gtk_Directory_Items_View_Record
             );
--
-- Refresh -- Current directory
--
--    Widget - The directory items view
--
   procedure Refresh
             (  Widget : not null access Gtk_Directory_Items_View_Record
             );
--
-- Renamed -- Item renaming notification
--
--    Widget - The directory items view
--    Index  - The renamed item index
--    Name   - The new name
--
-- This  is  procedure  is  used  when an item is renamed. The parameter
-- index is set to the new index of the renamed  item  if  that  becomes
-- visible. Otherwise it is set to 0.
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   procedure Renamed
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : in out Natural;
                Name   : Item_Name
             );
--
-- Reset_Selection -- All selected items
--
--    Widget - The directory items view
--
   procedure Reset_Selection
             (  Widget : not null access Gtk_Directory_Items_View_Record
             );
--
-- Selection_Changed -- Notification
--
--    Widget - The directory items view
--
-- This   procedure   is  called  upon  selection  change.  The  default
-- implementation emits selection-changed signal.
--
   procedure Selection_Changed
             (  Widget : not null access Gtk_Directory_Items_View_Record
             );
--
-- Set_Editable -- Set editable mode of the item names
--
--    Widget   - The directory items view
--    Editable - The mode to set
--
-- By default the item names listed by the widget are not editable. When
-- Set_Editable is called with Editable set to true,  the  operator  can
-- edit the  item  names.  The  name  input  is  confirmed  through  the
-- primitive operation Name_Commit.
--
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Directory_Items_View_Record;
                Editable : Boolean
             );
--
-- Set_Selection -- Of an item by name or position
--
--    Widget       - The directory items view
--    Name / Index - Of the item to select or deselect
--    State        - New selection status
--
-- Nothing happens if Item_Name is not in the directory.
--
   procedure Set_Selection
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Name   : Item_Name;
                State  : Boolean
             );
   procedure Set_Selection
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : Positive;
                State  : Boolean
             );
--
-- Set_Selection_Mode -- Set the selection mode
--
--    Widget - The directory items view
--    Mode   - New selection mode to set
--
   procedure Set_Selection_Mode
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Mode   : Gtk_Selection_Mode
             );
--
-- Children        -- Overrides Gtk.Tree_Model.Abstract_Store...
-- Get_Column_Type
-- Get_Flags
-- Get_Iter
-- Get_N_Columns
-- Get_Path
-- Get_Value
-- Has_Child
-- Next
-- Nth_Child
-- N_Children
-- Parent
-- Previous
--
   overriding
   function Children
            (  Model  : not null access Gtk_Abstract_Directory_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   overriding
   function Get_Column_Type
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Index : GInt
            )  return GType;
   overriding
   function Get_Flags
            (  Model : not null access Gtk_Abstract_Directory_Record
            )  return Tree_Model_Flags;
   overriding
   function Get_Iter
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter;
   overriding
   function Get_N_Columns
            (  Model : not null access Gtk_Abstract_Directory_Record
            )  return GInt;
   overriding
   function Get_Path
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
   overriding
   procedure Get_Value
             (  Model  : not null access Gtk_Abstract_Directory_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
   overriding
   function Has_Child
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
   overriding
   procedure Next
             (  Model : not null access Gtk_Abstract_Directory_Record;
                Iter  : in out Gtk_Tree_Iter
             );
   overriding
   function Nth_Child
            (  Model  : not null access Gtk_Abstract_Directory_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter;
   overriding
   function N_Children
            (  Model  : not null access Gtk_Abstract_Directory_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt;
   overriding
   function Parent
            (  Model : not null access Gtk_Abstract_Directory_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   overriding
   procedure Previous
             (  Model : not null access Gtk_Abstract_Directory_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- Set_Trace_File -- Redirect text tracing to a file
---
--  [ File_Name ] - The file is opened and used for tracing
--
-- By default the standard output is used for tracing.  When a file  was
-- set it  is closed.  Without parameters the procedure redirects to the
-- standard output, which is the default.
--
   procedure Set_Trace_File (File_Name : String);
   procedure Set_Trace_File;

private
   pragma Inline (Get_Depth);
   pragma Inline (Get_Tracing);
--
-- Item_Path_Reference -- An object containing path.  The implementation
--                        uses reference counting
--
   type Item_Path_Object (Length : Natural) is record
      Count : Natural := 1;
      Path  : Item_Path (1..Length);
   end record;
   type Item_Path_Object_Ptr is access Item_Path_Object;
   type Item_Path_Reference is
      new Ada.Finalization.Controlled with
   record
      Ptr : Item_Path_Object_Ptr;
   end record;
--
-- = -- Comparison
--
--    Left, Right - Path or path reference object
--
-- Returns :
--
--    True if left and right are same
--
   function "=" (Left, Right : Item_Path_Reference) return Boolean;
   function "=" (Left : Item_Path_Reference; Right : Item_Path)
      return Boolean;
   function "=" (Left : Item_Path; Right : Item_Path_Reference)
      return Boolean;
   procedure Adjust (Object : in out Item_Path_Reference);
   procedure Finalize (Object : in out Item_Path_Reference);
--
-- Get -- Path from reference object
--
--    Object - The reference object
--
-- Returns :
--
--    The path
--
   function Get (Object : Item_Path_Reference) return Item_Path;
--
-- Reset -- Set null path
--
--    Object - The reference object
--
   procedure Reset (Object : in out Item_Path_Reference);
--
-- Set -- Set path
--
--    Object - The reference object
--    Path   - To set into
--
   procedure Set
             (  Object : in out Item_Path_Reference;
                Path   : Item_Path
             );
------------------------------------------------------------------------
   type Gtk_Abstract_Directory_Record is
      abstract new Gtk_Abstract_Model_Record with
   record
      Refreshing : Natural := 0;
      Depth      : Natural := 0;        -- Cache recursion depth
      Tracing    : Traced_Actions := 0; -- Cache tracing enabled
      Tree       : Gtk_Tree_Store;      -- Unfiltered store
      Low, High  : GDouble;             -- Caching progress range
      Last_Time  : Time;                -- Progress was signaled
   end record;
--
-- Add_Folder -- Add a folder
--
--    Store     - The directory store
--    Row       - An iterator to the directory node (unfiltered)
--    Directory - The directory path (ignored if Row is Null_Iter)
--    Expanded  - Directory expanded flag
--    Updated   - Set to true if contents was modified, else unchanged
--
-- This procedure adds the items of Directory as the children of Row. If
-- Expanded is true the children of children are added as  well.  On  an
-- error Row might get deleted and will be then replaced by Null_Iter.
--
   procedure Add_Folder
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : in out Gtk_Tree_Iter;
                Directory : Item_Path;
                Expanded  : Boolean;
                Updated   : out Boolean
             );
--
-- Add_Item -- Add an item
--
--    Store     - The directory store
--    Row       - An iterator to the directory node (unfiltered)
--    Directory - The directory path (ignored if Row is Null_Iter)
--    Item      - A description of
--    Update    - When true the item is updated even if already exists
--    Result    - An iterator to the item added
--    Size      - Number of sibling items, excluding the added one
--
   procedure Add_Item
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : Gtk_Tree_Iter;
                Directory : Item_Path;
                Item      : Directory_Item;
                Update    : Boolean;
                Result    : out Gtk_Tree_Iter;
                Size      : out GInt
             );
--
-- Cache -- A path
--
--    Store       - The widget
--    Item        - The path of the item to navigate to
--    Best_Match  - An iterator most close to item (unfiltered)
--    Exact_Match - An iterator to item or Null_Iter (unfiltered)
--
-- This subprogram caches the path to the item. The directory  structure
-- is queried as necessary and cached.  Exact_Match  is  Null_Iter  when
-- Item is not found. Best_Match is  an  iterator  to  the  most  nested
-- directory found along the path to Item. Exact_Match is an iterator to
-- the  item.  Note  that because this operation deals with names it has
-- O(n*m) efficiency, where m is the path depth.
--
   procedure Cache
             (  Store       : not null access
                              Gtk_Abstract_Directory_Record'Class;
                Item        : Item_Path;
                Best_Match  : out Gtk_Tree_Iter;
                Exact_Match : out Gtk_Tree_Iter
             );
--
-- Emit -- Signals
--
--    Store   - The directory store
--    Signal  - The signal ID
--  [ Text    - The message
--  [ Path ]] - The path
--
   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Text   : UTF8_String;
                Path   : Item_Path
             );
   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Text   : UTF8_String
             );
--
-- Emit -- Signals
--
--    Store   - The directory store
--    Signal  - The signal name
--    Path    - The path
--  [ Value ] - The value
--
   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Path   : Item_Path;
                Value  : GDouble
             );
   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Path   : Item_Path
             );
--
-- Emit -- Signals
--
--    Store       - The directory store
--    Signal      - The signal name
--    Row         - The iterator
--    Text / Path - The text or path
--
   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Row    : Gtk_Tree_Iter;
                Path   : Gtk_Tree_Path
             );
   procedure Emit
             (  Store  : not null access
                         Gtk_Abstract_Directory_Record'Class;
                Signal : Signal_ID;
                Row    : Gtk_Tree_Iter;
                Text   : String
             );
--
-- Expand_Folder -- Expand a folder
--
--    Store     - The directory store
--    Row       - An iterator to the directory node (unfiltered)
--    Directory - The directory path (ignored if Row is Null_Iter)
--
-- This procedure goes through the children of Row and calls  Add_Folder
-- on them if they are directories.
--
   procedure Expand_Folder
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : Gtk_Tree_Iter;
                Directory : Item_Path
             );
--
-- Find -- An item by its path
--
--    Store - The directory store
--    Path  - Of the item to find
--
-- The function searches through the cache only. The efficiency of  this
-- operation is O(n).
--
-- Returns :
--
--    The iterator to Item or Null_Iter (unfiltered)
--
   function Find
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class;
               Path  : Item_Path
            )  return Gtk_Tree_Iter;
--
-- Find -- An item by its simple name
--
--    Store     - The directory store
--    Row       - An iterator to the directory node (unfiltered)
--    Directory - The path (ignored if Row is Null_Iter)
--    Name      - Of the item to find
--
-- The function searches through the cache only. Row is an  iterator  to
-- the directory  containing  the  item.  Directory  is  its  path.  The
-- efficiency of this operation is O(n).
--
-- Returns :
--
--    The iterator to Item or Null_Iter (unfiltered)
--
   function Find
            (  Store     : not null access
                           Gtk_Abstract_Directory_Record'Class;
               Row       : Gtk_Tree_Iter;
               Directory : Item_Path;
               Name      : Item_Name
            )  return Gtk_Tree_Iter;
--
-- Find -- An item by its description
--
--    Store     - The directory store
--    Row       - An iterator to the directory node (unfiltered)
--    Directory - The path (ignored if Row is Null_Iter)
--    Item      - The item to find
--
-- The function searches through the cache only. Row is an  iterator  to
-- the directory  containing  the  item.  Directory  is  its  path.  The
-- efficiency of this operation is O(log2 n).
--
-- Returns :
--
--    The iterator to Item or Null_Iter (unfiltered)
--
   function Find
            (  Store     : not null access
                           Gtk_Abstract_Directory_Record'Class;
               Row       : Gtk_Tree_Iter;
               Directory : Item_Path;
               Item      : Directory_Item
            )  return Gtk_Tree_Iter;
--
-- Find -- An item by its descriptor
--
--    Store     - The directory store
--    Row       - An iterator to the directory node (unfiltered)
--    Directory - The directory path (ignored if Row is Null_Iter)
--    Item      - The item to find
--    Position  - The item position
--    Size      - Number of items
--
-- The function searches through the cache only. Row is an  iterator  to
-- the  directory  containing the item. Because the cache is sorted this
-- function  is  O(log2).  Position  is 1.. of the item, if the item was
-- found. When item was not found it is the negated  position  where  to
-- place missing item. Size is the number of items.
--
   procedure Find
             (  Store     : not null access
                            Gtk_Abstract_Directory_Record'Class;
                Row       : Gtk_Tree_Iter;
                Directory : Item_Path;
                Item      : Directory_Item;
                Position  : out GInt;
                Size      : out GInt
             );
--
-- From_Item -- From unfiltered model conversion
--
--    Store - The directory store
--    Item  - An unfiltered iterator
--
-- Returns :
--
--    The corresponding iterator (filtered)
--
   function From_Item
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class;
               Item  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Get_Item -- From an iterator
--
--    Store - The directory store
--    Item  - An iterator to the item (unfiltered)
--
-- This function gets item description from the cache by an iterator to.
-- The field Policy is set to Cache_Expanded.
--
-- Returns :
--
--    Item description
--
   function Get_Item
            (  Store : not null access
                       Gtk_Abstract_Directory_Record'Class;
               Item  : Gtk_Tree_Iter
            )  return Directory_Item;
--
-- Get_Path -- From an iterator
--
--    Store - The directory store
--    Item  - An iterator to the item (unfiltered)
--
-- This function gets the full item name from its iterator. When Item is
-- Null_Iter the result is the full name of the root directory.
--
-- Returns :
--
--    The path of
--
   function Get_Path
            (  Store  : not null access
                        Gtk_Abstract_Directory_Record'Class;
               Item   : Gtk_Tree_Iter
            )  return Item_Path;
--
-- Refresh -- The cache
--
--    Store - The directory store
--    Item  - The item to refresh (an unfiltered iterator to)
--
-- The  parameter  Item can be Null_Iter to indicate the root directory.
-- The procedure refreshes all directory items.
--
   procedure Refresh
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Item  : in out Gtk_Tree_Iter
             );
--
-- Set_Item -- Modify item
--
--    Store - The directory store
--    Row   - An iterator to the directory node (unfiltered)
--    Item  - A description of
--
   procedure Set_Item
             (  Store : not null access
                        Gtk_Abstract_Directory_Record'Class;
                Row   : Gtk_Tree_Iter;
                Item  : Directory_Item
             );
--
-- To_Item -- To unfiltered model conversion
--
--    Store     - The directory store
--    Directory - A filtered iterator
--
-- Returns :
--
--    The corresponding iterator (all items unsorted)
--
   function To_Item
            (  Store     : not null access
                           Gtk_Abstract_Directory_Record'Class;
               Directory : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
------------------------------------------------------------------------
   type Gtk_Directory_Tree_View_Record is
      new Gtk_Tree_View_Record with
   record
      Cache         : Gtk_Abstract_Directory;
      Name_Renderer : Gtk_Cell_Renderer_Text;
      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
      This, Last    : Item_Path_Reference;
   end record;
--
-- Directory_Activated -- Callback on directory click
--
   procedure Directory_Activated
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             );
--
-- Directory_Changed -- Callback on row expanded
--
   procedure Directory_Changed
             (  Object  : access GObject_Record'Class;
                Browser : Gtk_Directory_Tree_View
             );
--
-- Directory_Expanded -- Callback on row expanded
--
   procedure Directory_Expanded
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             );
--
-- Directory_Refreshed -- Callback on refresh initiation
--
   procedure Directory_Refreshed
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             );
--
-- Edited_Directory -- Callback on edited directory name
--
   procedure Edited_Directory
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Tree_View
             );
--
-- Set -- Set current directory to path reference
--
--    Object - The reference object
--    Widget - The widget
--
   procedure Set
             (  Object : in out Item_Path_Reference;
                Widget : not null access
                         Gtk_Directory_Tree_View_Record'Class
             );

   package Tree_Functions is
      new Set_Column_Cell_Data (Gtk_Directory_Tree_View);
--
-- Set_Tree_Icon -- Column data function to handle cell rendering of the
--                  directory tree view icons
--
   procedure Set_Tree_Icon
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Gtk_Directory_Tree_View
             );
--
-- Set_Tree_Name -- Column data function to handle cell rendering of the
--                  directory tree view names
--
   procedure Set_Tree_Name
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Gtk_Directory_Tree_View
             );
------------------------------------------------------------------------
   type Gtk_Selection_Store_Record is
      new Gtk_Extension_Store_Record with
   record
      Selected : Natural := 0;     -- Total rows selected
      Changed  : Boolean := False; -- Transient selection change flag
      Mode     : Gtk_Selection_Mode := Selection_Multiple;
      View     : Gtk_Directory_Items_View;
   end record;

   type Gtk_Selection_Store is
      access all Gtk_Selection_Store_Record'Class;
--
-- Deleted -- Overrides Gtk.Tree_Model.Extension_Store...
--
   overriding
      procedure Deleted
                (  Model : not null access Gtk_Selection_Store_Record;
                   Path  : Gtk_Tree_Path
                );
--
-- Deleting -- Overrides Gtk.Tree_Model.Extension_Store...
--
   overriding
      procedure Deleting
                (  Model : not null access Gtk_Selection_Store_Record;
                   Path  : Gtk_Tree_Path;
                   Iter  : Gtk_Tree_Iter
                );
--
-- Inserted -- Overrides Gtk.Tree_Model.Extension_Store...
--
   overriding
   procedure Inserted
             (  Model : not null access Gtk_Selection_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             );

   type Column_Data is record
      Browser       : Gtk_Directory_Items_View;
      Column        : Positive;
      Text_Renderer : Gtk_Cell_Renderer_Text;
      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
   end record;
------------------------------------------------------------------------
-- Gtk_Directory_Items_Store_Record -- Directory of items
--
   type Gtk_Directory_Items_Store_Record is
      new Gtk_Abstract_Model_Record with
   record
      Cache   : Gtk_Abstract_Directory;
      View    : Gtk_Directory_Items_View;
      Root    : Gtk_Tree_Path; -- Current root unfiltered
      Deleted : GInt := -1;    -- Row being deleted, filtered zero based
      Count   : GInt := 0;     -- Number of items found
   end record;
   type Gtk_Directory_Items_Store is
      access all Gtk_Directory_Items_Store_Record'Class;
--
-- Check_Iter -- Check iterator
--
--    Model - The model
--    Iter  - The iterator to check
--
-- Returns :
--
--    True if Iter is possibly of the model
--
   function Check_Iter
            (  Model : Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
--
-- Gtk_New -- Construction
--
--    Model - The model to create
--    Cache - The cache
--    View  - The parent widget
--
   procedure Gtk_New
             (  Model : out Gtk_Directory_Items_Store;
                Cache : not null access
                        Gtk_Abstract_Directory_Record'Class;
                View  : not null access
                        Gtk_Directory_Items_View_Record'Class
             );
--
-- Finalize -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Finalize
             (  Model : not null access Gtk_Directory_Items_Store_Record
             );
--
-- Implementations of abstracts from Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Children
            (  Model  : not null access
                        Gtk_Directory_Items_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   procedure From_Filtered
             (  Model      : not null access
                             Gtk_Directory_Items_Store_Record;
                Unfiltered : out Gtk_Tree_Iter;
                Filtered   : Gtk_Tree_Iter
             );
   overriding
   function Get_Column_Type
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Index : GInt
            )  return GType;
   overriding
   function Get_Flags
            (  Model : not null access Gtk_Directory_Items_Store_Record
            )  return Tree_Model_Flags;
   overriding
   function Get_Iter
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter;
   overriding
   function Get_N_Columns
            (  Model : not null access Gtk_Directory_Items_Store_Record
            )  return GInt;
   overriding
   function Get_Path
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
   function Get_Path_Before
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
   overriding
   procedure Get_Value
             (  Model  : not null access
                         Gtk_Directory_Items_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
   overriding
   function Has_Child
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
   overriding
   procedure Next
             (  Model : not null access
                        Gtk_Directory_Items_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
   overriding
   function Nth_Child
            (  Model  : not null access
                        Gtk_Directory_Items_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter;
   overriding
   function N_Children
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt;
   overriding
   function Parent
            (  Model : not null access Gtk_Directory_Items_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   overriding
   procedure Previous
             (  Model : not null access
                        Gtk_Directory_Items_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- To_Filtered -- Iterator conversion
--
--    Model      - The model
--    Unfiltered - The unfiltered path
--
-- Returns :
--
--    Filtered zero-based row number or -1
--
   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Path
            )  return GInt;
--
-- To_Filtered -- Iterator conversion
--
--    Model      - The model
--    Unfiltered - The unfiltered path
--
-- Returns :
--
--    Filtered zero-based row number or -1
--
   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Iter
            )  return GInt;
--
-- To_Filtered -- Iterator conversion
--
--    Model      - The model
--    Unfiltered - The unfiltered iterator
--
-- Returns :
--
--    Filtered iterator or null iterator
--
   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- To_Filtered -- Iterator conversion
--
--    Model      - The model
--    Unfiltered - The unfiltered iterator
--
-- Returns :
--
--    Filtered path or null path
--
   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
--
-- To_Filtered -- Iterator conversion
--
--    Model      - The model
--    Unfiltered - The unfiltered path
--
-- Returns :
--
--    Filtered path
--
   function To_Filtered
            (  Model      : not null access
                            Gtk_Directory_Items_Store_Record;
               Unfiltered : Gtk_Tree_Path
            )  return Gtk_Tree_Path;
--
-- Item_Deleted -- Event callback
--
   procedure Item_Deleted
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             );
--
-- Item_Deleted -- Event callback
--
   procedure Item_Deleting
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             );
--
-- Item_Inserted -- Event callback
--
   procedure Item_Inserted
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             );
--
-- Item_Renamed -- Event callback
--
   procedure Item_Renamed
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Store  : Gtk_Directory_Items_Store
             );
------------------------------------------------------------------------
   type Gtk_Cell_Renderer_Text_Array is
      array (Integer range <>) of Gtk_Cell_Renderer_Text;
   type Gtk_Cell_Renderer_Text_Array_Ptr is
      access Gtk_Cell_Renderer_Text_Array;

   type Gtk_Directory_Items_View_Record is
      new Gtk_Tree_View_Record with
   record
      Content        : Gtk_Directory_Items_Store;
      Markup         : Gtk_Selection_Store;
      Columns        : Gtk_Columned_Store;
      Directories    : Gtk_Directory_Tree_View;
      Last_Key       : GUnichar;
      Last_Position  : Natural := 0;
      Name_Renderers : Gtk_Cell_Renderer_Text_Array_Ptr;
   end record;
--
-- Change_Selection -- To unfiltered model conversion
--
--    Widget - The directory items view
--    Index  - Of the item
--    State  - The selection state
--
   procedure Change_Selection
             (  Widget : access Gtk_Directory_Items_View_Record;
                Index  : Positive;
                State  : Boolean
             );
--
-- Destroy -- Callback on destroy
--
   procedure Destroy
             (  Object  : access GObject_Record'Class;
                Browser : Gtk_Directory_Items_View
             );
--
-- Directory_Collapsed -- Callback on row collapsed
--
   procedure Directory_Collapsed
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Items_View
             );
--
-- Edited_Item -- Callback on edited item
--
   procedure Edited_Item
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Column : Column_Data
             );
--
-- From_Marked -- To markup model conversion
--
--    Widget - The directory items view
--    Item   - An iterator in the markup model
--
-- Returns :
--
--    An unfiltered iterator to the corresponding item
--
   function From_Marked
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Item   : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Get_Directory -- Get the current directory
--
--    Widget - The directory items view
--
-- Returns :
--
--    An iterator to the directory in the cache (unfiltered)
--
   function Get_Directory
            (  Widget : not null access Gtk_Directory_Items_View_Record
            )  return Gtk_Tree_Iter;
--
-- Get_Iter -- Of an item by its index
--
--    Widget - The directory items view
--    Index  - The number of 1..
--
-- Returns :
--
--    An iterator to the item in the markup model
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function Get_Iter
            (  Widget : not null access Gtk_Directory_Items_View_Record;
               Index  : Positive
            )  return Gtk_Tree_Iter;
--
-- Get_Position -- Of an item by its index
--
--    Widget - The directory items view
--    Index  - The number of 1..
--    Row    - Of the item
--    Column - Of the item
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   procedure Get_Position
             (  Widget : not null access
                         Gtk_Directory_Items_View_Record;
                Index  : Positive;
                Row    : out Positive;
                Column : out Positive
             );
--
-- Item_Activated -- Callback on item click
--
   procedure Item_Activated
             (  Object  : access GObject_Record'Class;
                Params  : GValues;
                Browser : Gtk_Directory_Items_View
             );
--
-- Key_Press -- Event handler (key and button presses
--
   function Key_Press
            (  Object  : access GObject_Record'Class;
               Event   : Gdk_Event;
               Browser : Gtk_Directory_Items_View
            )  return Boolean;
--
-- Scan -- An item that matches
--
--    Widget - The directory items view
--    Prefix - Of an item to find
--    Index  - To start scan from (0 from the beginning)
--
-- The  procedure  starts  scanning  items  from  the  item  next to one
-- specified by Index. The item shall have name starting with Prefix.
--
-- Returns :
--
--    The index of an item found or 0
--
   function Scan
            (  Widget : not null access
                        Gtk_Directory_Items_View_Record'Class;
               Prefix : Item_Name;
               Index  : Natural
            )  return Natural;
--
-- Set_Current -- Change the directory
--
--    Widget    - The directory items view
--    Directory - The directory to switch to
--    Changed   - Directory changed flag
--
-- When Changed is True the directory is set  forcibly,  otherwise  only
-- when  Directory differs from the current one. In this case Changed is
-- set to True by Set_Current.
--
   procedure Set_Current
             (  Widget    : not null access
                            Gtk_Directory_Items_View_Record'Class;
                Directory : Gtk_Tree_Iter;
                Changed   : in out Boolean
             );
--
-- To_Marked -- To markup model conversion
--
--    Widget - The directory items view
--    Item   - An unfiltered iterator to
--
-- Returns :
--
--    The corresponding iterator in the markup model
--
   function To_Marked
            (  Widget : not null access
                        Gtk_Directory_Items_View_Record;
               Item   : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Selection_Changed -- Callback on directory selection
--
   procedure Selection_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Browser   : Gtk_Directory_Items_View
             );
------------------------------------------------------------------------
   package Abstract_Directory_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Abstract_Directory
          );
   package Directory_Tree_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Directory_Tree_View
          );
   package Directory_Items_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Directory_Items_View
          );
   package Directory_Items_Store_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Directory_Items_Store
          );
   package Directory_Items_Commit_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Column_Data
          );
   package Directory_Selection_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_Selection_Record,
             Gtk_Directory_Items_View
          );
   package Directory_Items_Result_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  GObject_Record,
             Boolean,
             Gtk_Directory_Items_View
          );
   package Column_Functions is
      new Set_Column_Cell_Data (Column_Data);
--
-- Set_Column_Data -- Column data function to handle cell  rendering  of
--                    the directory items view
--
   procedure Set_Column_Data
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Column_Data
             );
--
-- Set_Null -- Set null icon into the renderer
--
--    Cell - The renderer to set
--
   procedure Set_Null
             (  Cell : not null access Gtk_Cell_Renderer_Record'Class
             );

   Cached_New                : constant GInt := 0;
   Cached_Never_Directory    : constant GInt := 1;
   Cached_Ahead_Directory    : constant GInt := 2;
   Cached_Expanded_Directory : constant GInt := 3;
   Cached_Item               : constant GInt := 4;
   subtype Cached_Directory is GInt
      range Cached_Never_Directory..Cached_Expanded_Directory;
   subtype Cached_Children is GInt
      range Cached_Ahead_Directory..Cached_Expanded_Directory;
   subtype Cached_Node is GInt range Cached_New..Cached_Item;
--
-- Get_Abstract_Directory_Type -- Type of the store
--
   function Get_Cache_Model_Type return Gtk_Type;
   function Get_Items_Model_Type return Gtk_Type;

   pragma Inline (From_Filtered);
   pragma Inline (Set_Item);
   pragma Inline (To_Filtered);

end Gtk.Abstract_Browser;
