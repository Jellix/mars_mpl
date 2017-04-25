--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Directory_Browser                       Luebeck            --
--  Interface                                      Autumn, 2007       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with GIO.Mount;             use GIO.Mount;
with Gtk.Abstract_Browser;  use Gtk.Abstract_Browser;
with Gtk.Paned;             use Gtk.Paned;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Widget;            use Gtk.Widget;

with Ada.Finalization;
with Tables;

package Gtk.Directory_Browser is
--
-- Gtk_Directory_Record -- A  directory  cache,  which  can be used as a
--                         tree model.
--
   type Gtk_Directory_Record is
      new Gtk_Abstract_Directory_Record with private;
   type Gtk_Directory is access all Gtk_Directory_Record'Class;
--
-- Compare -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Compare
            (  Store     : not null access Gtk_Directory_Record;
               Directory : Item_Path;
               A, B      : Directory_Item;
               By_Name   : Boolean
            )  return Row_Order;
--
-- Delete -- A file or directory
--
--    Store - The directory cache
--    File  - A full name of
--
-- The  file is physically removed and then the cache is synchronized as
-- necessary. File a directories can be removed this way.
--
-- Exceptions :
--
--    Name_Error - Illegal file name
--    Use_Error  - Deletion error
--
   procedure Delete
             (  Store : not null access Gtk_Directory_Record;
                File  : UTF8_String
             );
--
-- Get_Directory -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Get_Directory
            (  Store : not null access Gtk_Directory_Record;
               Item  : Item_Path
            )  return Item_Path;
--
-- Get_Name -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Get_Name
            (  Store : not null access Gtk_Directory_Record;
               Item  : Item_Path
            )  return Item_Name;
--
-- Get_Path -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Get_Path
            (  Store     : not null access Gtk_Directory_Record;
               Directory : Item_Path;
               Item      : Item_Name
            )  return Item_Path;
--
-- Gtk_New -- Factory
--
--    Store   - The result
--    Policy  - Caching policy to use
--    Tracing - Desired tracing
--
-- The caching policy used is Cache_Expanded.
--
   procedure Gtk_New
             (  Store   : out Gtk_Directory;
                Policy  : Caching_Policy := Cache_Expanded;
                Tracing : Traced_Actions := Trace_Nothing
             );
--
-- Initialize -- To be called by any derived type
--
--    Store   - A newly created object to initialize
--    Policy  - Caching policy to use
--    Tracing - Desired tracing
--
   procedure Initialize
             (  Store   : not null access Gtk_Directory_Record'Class;
                Policy  : Caching_Policy;
                Tracing : Traced_Actions
             );
--
-- Read -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Read
            (  Store : not null access Gtk_Directory_Record
            )  return Directory_Item;
--
-- Rewind -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Rewind
            (  Store  : not null access Gtk_Directory_Record;
               Folder : Item_Path
            )  return Directory_Item;
------------------------------------------------------------------------
--
-- Gtk_Directory_Browser_Record -- A   directory   browser  widget.  The
--                                 widget is a descendant of paned.  One
-- of its children is a tree view of the directory tree browsed  another
-- is the list of files in the currently viewed directory.
--
   type Gtk_Directory_Browser_Record is
      new Gtk_Paned_Record with private;
   type Gtk_Directory_Browser is
      access all Gtk_Directory_Browser_Record'Class;
--
-- Filter -- Items filtering
--
--    Widget    - The widget
--    Directory - True if Simple_Name is a directory name
--    Name      - The item name
--    Kind      - The item type
--
-- This function can be overridden to  prevent  some  items  from  being
-- shown  in  the  directory items pane. The default implementation lets
-- all items in.
--
-- Returns :
--
--    True if the item has to be in the model
--
   function Filter
            (  Widget    : not null access Gtk_Directory_Browser_Record;
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
            (  Widget : not null access Gtk_Directory_Browser_Record
            )  return Gtk_Directory;
--
-- Get_Files_View -- Files view widget
--
--    Widget - The widget
--
-- Returns :
--
--    The widget responsible for files list representation
--
   function Get_Files_View
            (  Widget : not null access Gtk_Directory_Browser_Record
            )  return Gtk_Directory_Items_View;
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
            (  Widget : not null access Gtk_Directory_Browser_Record
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
            (  Widget : not null access Gtk_Directory_Browser_Record
            )  return Gtk_Directory_Tree_View;
--
-- Gtk_New -- Factory
--
--    Widget    - The result
--    File      - A file in the file system to browse
--    Columns   - The number of columns to use in the files list
--    Vertical  - The vertically vs. horizontally panned
--    Tree_Size - The maximal size of the tree pane upon start
--    List_Size - The maximal size of the list pane upon start
--    Store     - The cache to use
--    Tracing   - Desired tracing
--
-- When the parameter Store  is  null  a  new  Gtk_Directory  object  is
-- created.  Otherwise,  the  specified  one  is  used.  The  parameters
-- Tree_Size and List_Size controls the size of the  panes  upon  start.
-- The  panes will initially try to show all content without scroll bars
-- but  not  larger  than  the  size  specified  by  the   corresponding
-- parameter.
--
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
             );
--
-- Initialize -- To be called by any derived type
--
--    Widget   - The result
--    File     - A file in the file system to browse
--    Columns  - The number of columns to use in the files list
--    Vertical - The vertically vs. horizontally panned
--    Store    - The cache to use
--    Tracing  - Desired tracing
--
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
             );
--
-- Set_Tracing -- Change tracing
--
--    Widget  - The widget
--    Tracing - To set
--
   procedure Set_Tracing
             (  Widget  : not null access Gtk_Directory_Browser_Record;
                Tracing : Traced_Actions
             );
--
-- Get_Root_Directory -- The root directory
--
-- Returns :
--
--    The ultimate ancestor of the current directory
--
-- Exceptions :
--
--    Name_Error - I/O error
--
   function Get_Root_Directory (File : String) return String;

private
   package Root_Tables is new Tables (GMount);
   use Root_Tables;

   type Search_Mode is (None, Root, Directory);
--
-- Data -- The directory browsing data.  This needs to be  limited so it
--         cannot  be  put  into  the  object  as is, because the object
--         itself is non-limited.
--
   type Browsing_Data is limited record
      Mode         : Search_Mode := None;
      Search       : GDir;
      Policy       : Caching_Policy := Cache_Expanded;
      Roots        : Table;
      Current_Root : Positive := 1;
      Folder       : String_Ptr; -- Current folder name, being read
      Length       : Natural;    -- Name length
   end record;
   type Browsing_Data_Ptr is access Browsing_Data;

   type Browsing_Data_Ref is new Ada.Finalization.Controlled with record
      Ptr : Browsing_Data_Ptr;
   end record;

   procedure Finalize (Reference : in out Browsing_Data_Ref);

   type Gtk_Directory_Record is
      new Gtk_Abstract_Directory_Record with
   record
      Data : Browsing_Data_Ref;
   end record;
--
-- Is_Root -- Check if the path specifies a root
--
--    Store - The store
--    Item  - The path
--
-- Returns :
--
--    True if Item is a root
--
   function Is_Root
            (  Store : not null access Gtk_Directory_Record;
               Item  : Item_Path
            )  return Boolean;

   type Gtk_Folders_View_Record is
      new Gtk_Directory_Tree_View_Record with null record;
   type Gtk_Folders_View is access all Gtk_Folders_View_Record'Class;
--
-- Get_Icon -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Get_Icon
            (  Widget       : not null access Gtk_Folders_View_Record;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Icon_Data;
--
-- Get_Name -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Get_Name
            (  Widget       : not null access Gtk_Folders_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Expanded     : Boolean;
               Has_Children : Boolean;
               Topmost      : Boolean
            )  return Item_Name;
--
-- Name_Commit -- Overrides Gtk.Abstract_Browser...
--
   overriding
   procedure Name_Commit
             (  Widget   : not null access Gtk_Folders_View_Record;
                Old_Path : Item_Path;
                New_Name : Item_Name
             );

   type Gtk_Files_View_Record is
      new Gtk_Directory_Items_View_Record with
   record
      Browser : Gtk_Directory_Browser;
   end record;
   type Gtk_Files_View is access all Gtk_Files_View_Record'Class;
--
-- Filter -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Filter
            (  Widget    : not null access Gtk_Files_View_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean;
--
-- Get_Icon -- Overrides Gtk.Abstract_Browser...
--
   overriding
   function Get_Icon
            (  Widget       : not null access Gtk_Files_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Directory    : Boolean;
               Has_Children : Boolean
            )  return Icon_Data;
--
-- Name_Commit -- Overrides Gtk.Abstract_Browser...
--
   overriding
   procedure Name_Commit
             (  Widget : not null access Gtk_Files_View_Record;
                Index  : Positive;
                Name   : Item_Name
             );

   type Gtk_Directory_Browser_Record is
      new Gtk_Paned_Record with
   record
      Cache : Gtk_Directory;
      Tree  : Gtk_Folders_View;
      Items : Gtk_Files_View;
   end record;

end Gtk.Directory_Browser;
