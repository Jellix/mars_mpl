--                                                                    --
--  package Gtk.Missed              Copyright (c)  Maxim Reznik       --
--  Interface                                      Summer, 2006       --
--                                                                    --
--                                Last revision :  16:49 28 Feb 2016  --
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

with Cairo;                     use Cairo;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Cursor;                use Gdk.Cursor;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Types;                 use Gdk.Types;
with Glib.Error;                use Glib.Error;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with Glib.Values;               use Glib.Values;
with Gtk.Button;                use Gtk.Button;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Container;             use Gtk.Container;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Text_Buffer;           use Gtk.Text_Buffer;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Ada.Finalization;
with Gdk.Event;
with Gdk.Window;
with Glib;
with Glib.Object;
with Glib.Properties;
with Glib.Wrappers;
with Gtk.Cell_Renderer;

package Gtk.Missed is
   --
   -- GtkAda_Contributions_Domain -- The domain name used for the  error
   --                                log messages  coming  out  of  this
   --                                library.
   --
   GtkAda_Contributions_Domain : constant String := "GtkAda+";

   type Row_Order is (Before, Equal, After);

   type User_Directory is               -- The user's
        (  User_Directory_Desktop,      -- Desktop directory
           User_Directory_Documents,    -- Documents directory
           User_Directory_Download,     -- Downloads directory
           User_Directory_Music,        -- Music directory
           User_Directory_Pictures,     -- Pictures directory
           User_Directory_Public_Share, -- shared directory
           User_Directory_Templates,    -- Templates directory
           User_Directory_Videos        -- Movies directory
        );

   type GFileTest is new GUInt;
   File_Test_Is_Regular    : constant GFileTest := 2**0;
   File_Test_Is_Symlink    : constant GFileTest := 2**1;
   File_Test_Is_Dir        : constant GFileTest := 2**2;
   File_Test_Is_Executable : constant GFileTest := 2**3;
   File_Test_Exists        : constant GFileTest := 2**4;

   type GDir is new Glib.C_Proxy;
--
-- Add_Button_From_Stock -- A replacement for deprecated Add_Button
--
--    Dialog    - The dialog
--    Response  - The dialog response the button will generate
--    Label     - The button label
--    Icon      - The button stock ID for the button to add
--    Icon_Left - Icon left of the label
--    Size      - The icon size
--    Spacing   - The spacing between the icon and label
--    Tip       - The tooltip text
--    Relief    - The button relief
--
   procedure Add_Button_From_Stock
             (  Dialog     : not null access Gtk_Dialog_Record'Class;
                Response   : Gtk_Response_Type;
                Label      : UTF8_String   := "";
                Icon       : UTF8_String   := "";
                Icon_Left  : Boolean       := True;
                Size       : Gtk_Icon_Size := Icon_Size_Button;
                Spacing    : GUInt         := 3;
                Tip        : UTF8_String   := "";
                Relief     : Gtk_Relief_Style := Relief_Normal
             );
   function Add_Button_From_Stock
            (  Dialog     : not null access Gtk_Dialog_Record'Class;
               Response   : Gtk_Response_Type;
               Label      : UTF8_String   := "";
               Icon       : UTF8_String   := "";
               Icon_Left  : Boolean       := True;
               Size       : Gtk_Icon_Size := Icon_Size_Button;
               Spacing    : GUInt         := 3;
               Tip        : UTF8_String   := "";
               Relief     : Gtk_Relief_Style := Relief_Normal
            )  return Gtk_Button;
--
-- Add_Named -- Add a named stock image
--
--    Name - Of the image
--    Icon - The pixbuf (static)
--
-- This procedure is a replacement to deprecated Add_Static.  It is used
-- in combination with Add_Stock_Attribute.
--
   procedure Add_Named
             (  Name : UTF8_String;
                Icon : Gdk_Pixbuf
             );
--
-- Add_Stock_Attribute -- Add stock image specified by the name
--
--    Cell_Layout - The tree view column
--    Cell        - The cell renderer
--    Column      - The tree model column (string containing icon name)
--
-- The procedure is a replacement to deprecated:
--
---   Cell_Layout.Add_Attribute (Cell, "stock-id", Column)
--
-- The  icon is first searched in the  list of images added by Add_Named
-- and then in the list of themed icons.
--
   procedure Add_Stock_Attribute
             (  Cell_Layout : not null access
                              Gtk_Tree_View_Column_Record'Class;
                Cell        : not null access
                              Gtk_Cell_Renderer_Pixbuf_Record'Class;
                Column      : GInt
             );
--
-- Build_Filename -- File base name
--
--    First_Element  - The prefix of a file name
--    Second_Element - The suffix of
--    Third_Element  - The suffix of the suffix
--    Fourth_Element - ...
--    Fifth_Element  - ...
--
-- This  function  creates  a  filename  from  two of elements using the
-- correct  separator  for  filenames.  On  Unix,  this function behaves
-- identically to g_build_path (G_DIR_SEPARATOR_S, first_element, ....).
-- On Windows, it takes into account that either  the  backslash  (\  or
-- slash (/) can be  used  as  separator  in  filenames,  but  otherwise
-- behaves as  on  Unix.  When  file  pathname  separators  need  to  be
-- inserted, the one that last previously  occurred  in  the  parameters
-- (reading from left to right) is used. No attempt is made to force the
-- resulting filename to be an absolute path. If the first element is  a
-- relative path, the result will be a relative path.
--
-- Returns :
--
--    The file name
--
   function Build_Filename
            (  First_Element  : UTF8_String;
               Second_Element : UTF8_String
            )  return UTF8_String;
   function Build_Filename
            (  First_Element  : UTF8_String;
               Second_Element : UTF8_String;
               Third_Element  : UTF8_String
            )  return UTF8_String;
   function Build_Filename
            (  First_Element  : UTF8_String;
               Second_Element : UTF8_String;
               Third_Element  : UTF8_String;
               Fourth_Element : UTF8_String
            )  return UTF8_String;
   function Build_Filename
            (  First_Element  : UTF8_String;
               Second_Element : UTF8_String;
               Third_Element  : UTF8_String;
               Fourth_Element : UTF8_String;
               Fifth_Element  : UTF8_String
            )  return UTF8_String;
--
-- Check -- Cairo context for pending errors
--
--    Context - The context
--
-- Exceptions :
--
--    Status_Error - If status is not success
--
   procedure Check (Context : Cairo_Context);
--
-- Class_From_Type -- Get class object from its type
--
   function Class_From_Type (Typ : GType) return GObject_Class;
--
-- Class_Install_Property -- Install property for a class
--
   procedure Class_Install_Property
             (  Class_Record  : GObject_Class;
                Prop_Id       : Property_Id;
                Property_Spec : Param_Spec
             );

   function Compare
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Path
            )  return Row_Order;

   function Compare
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Path
            )  return Row_Order;

   function Compare
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Iter
            )  return Row_Order;

   function Compare
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Iter
            )  return Row_Order;
--
-- Delete_Event_Handler -- Typical handler of the "delete-event" signal
--
-- This function is used as follows:
--
--    Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
--
-- The implementation returns False
--
   function Delete_Event_Handler
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event
            )  return Boolean;
--
-- Destroy_Handler -- Typical handler of the "destroy" signal
--
-- This function is used as follows:
--
--    Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);
--
-- The implementation calls Gtk.Main.Main_Quit
--
   procedure Destroy_Handler
             (  Widget : access Gtk_Widget_Record'Class
             );
--
-- Dir_Close -- Close directory opened by Dir_Open
--
--    Dir - To close
--
-- After  completion  Dir  is  set  to null. nothing happens when Dir is
-- null.
--
   procedure Dir_Close (Dir : in out GDir);
--
-- Dir_Open -- Open a directory to enumerate its entities
--
--    Path  - The directory path
--    Dir   - The result upon success
--    Error - If failed
--
-- When successful Error is set to null and Dir is  the  search  object.
-- Dir must be passed to Dir_Close when no  more  used.  Upon  an  error
-- Directory is null and Error is set according to the error. Error_Free
-- must be called on it.
--
   procedure Dir_Open
             (  Path  : UTF8_String;
                Dir   : out GDir;
                Error : out GError
             );
--
-- Dir_Read_Name -- Read directory
--
--    Dir - To read
--
-- Returns :
--
--    Name of the next directory entry
--
-- Exceptions :
--
--    End_Error - No more items in the directory
--
   function Dir_Read_Name (Dir : GDir) return UTF8_String;
--
-- Dir_Rewind -- Reset the directory
--
--    Dir - To rewind
--
-- This  procedure  resets  the  given  directory.  The  next  call   to
-- Dir_Read_Name will return the first entry again.
--
   procedure Dir_Rewind (Dir : GDir);
--
-- Erase -- Elements of the container
--
--    Container - The container
--
   procedure Erase
             (  Container : not null access Gtk_Container_Record'Class
             );
--
-- File_Test -- Test file status
--
--    File_Name - The file name
--    Flags     - To test for
--
-- Returns :
--
--    True if any of the Flags is set
--
   function File_Test
            (  File_Name : UTF8_String;
               Flags     : GFileTest
            )  return Boolean;
--
-- File_Test -- Test file status
--
--    File_Name - The file name
--
-- This function provided for convenience it tests for status bits.
--
-- Returns :
--
--    Flags tested
--
   function File_Test (File_Name : UTF8_String) return GFileTest;
--
-- Find_Program_In_Path -- Find an executable program by name
--
--    Program - The program name
--
-- This function locates the  first  executable  named  Program  in  the
-- user's path.
--
-- Returns :
--
--    Absolute path of the program or else empty string
--
   function Find_Program_In_Path (Program : UTF8_String)
      return UTF8_String;
--
-- Find_Property -- Find property of a class or an object
--
--    Class / Object - To look for the property
--    Name           - The property name
--
-- Returns :
--
--    The property specification or null if there is none
--
   function Find_Property
            (  Class : GObject_Class;
               Name  : UTF8_String
            )  return Param_Spec;
   function Find_Property
            (  Object : not null access GObject_Record'Class;
               Name   : UTF8_String
            )  return Param_Spec;
--
-- Freeze_Notify -- Freeze emitting signals
--
--     Object - The object
--
-- Increases the  freeze  count  on  object.  If  the  freeze  count  is
-- non-zero, the emission of "notify" signals on object is stopped.  The
-- signals  are  queued  until  the  freeze  count is decreased to zero.
-- Duplicate notifications are squashed so that  at  most  one  "notify"
-- signal is emitted for each property  modified  while  the  object  is
-- frozen.
-- This  is  necessary  for accessors that modify multiple properties to
-- prevent premature  notification  while  the  object  is  still  being
-- modified.
--
   procedure Freeze_Notify
             (  Object : not null access GObject_Record'Class
             );
--
-- From_RGBA -- Conversion from RGBA to color
--
--    Color - To convert
--
-- Returns :
--
--    The coresponding color
--
   function From_RGBA (Color : Gdk_RGBA) return Gdk_Color;
--
-- Get -- Replacement of Gtk.Tree_Store.Get_String which has bugs
--
--    Store  - The tree store
--    Row    - The row iterator
--    Column - The column
--
-- Returns :
--
--    The cell value
--
   function Get
            (  Store  : not null access Gtk_List_Store_Record'Class;
               Row    : Gtk_Tree_Iter;
               Column : GInt
            )  return String;
   function Get
            (  Store  : Gtk_Tree_Model;
               Row    : Gtk_Tree_Iter;
               Column : GInt
            )  return String;
   function Get
            (  Store  : not null access Gtk_Tree_Store_Record'Class;
               Row    : Gtk_Tree_Iter;
               Column : GInt
            )  return String;
--
-- Insert_Alt -- Alternative version of text buffer insert
--
--    Buffer - Text buffer
--    Iter   - Where to insert the text
--    Text   - The text to insert
--
-- This implementation does not allocate a copy of the text in the pool.
-- It also does not use NUL as terminator.
--
   procedure Insert_Alt
             (  Buffer : not null access Gtk_Text_Buffer_Record'Class;
                Iter   : in out Gtk_Text_Iter;
                Text   : UTF8_String
             );
--
-- Is_In -- Elements of the container
--
--    Container - The container
--    Element   - Of the container
--
-- Returns :
--
--    True if the
--
   function Is_In
            (  Container : not null access Gtk_Container_Record'Class;
               Element   : not null access Gtk_Widget_Record'Class
            )  return Boolean;
--
-- Get_Application_Name -- Returns application name
--
-- Returns :
--
--    The application name in a human readable form
--
   function Get_Application_Name return UTF8_String;
--
-- Get_Clip_Rectangle -- Get approximation of clipping rectangle
--
--    Context   - The cairo context
--    Rectangle - The clipping rectange if not Empty
--    Empty     - No clipping rectangle set
--
   procedure Get_Clip_Rectangle
             (  Context   : Cairo_Context;
                Rectangle : out Gdk_Rectangle;
                Empty     : out Boolean
             );
--
-- Get_Clip_Rectangle -- Get approximation of clipping rectangle
--
--    Context   - The cairo context
--
-- Returns :
--
--    True if there is a clipping region
--
   function Get_Clip_Rectangle (Context : Cairo_Context) return Boolean;
--
-- Get_Current_Dir -- Returns current directory
--
-- Returns :
--
--    Current directory
--
   function Get_Current_Dir return UTF8_String;
--
-- Get_PRGName -- Returns program name
--
-- Returns :
--
--    The program name
--
   function Get_PRGName return UTF8_String;

   function Is_A (Derived, Ancestor : GType) return Boolean;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Path
            )  return Boolean;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Path
            )  return Boolean;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Iter
            )  return Boolean;

   function Is_In
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Iter
            )  return Boolean;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Path
            )  return Boolean;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Path
            )  return Boolean;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Iter
            )  return Boolean;

   function Is_Parent
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Iter
            )  return Boolean;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Path
            )  return Boolean;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Path
            )  return Boolean;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Path;
               B     : Gtk_Tree_Iter
            )  return Boolean;

   function Is_Sibling
            (  Model : Gtk_Tree_Model;
               A     : Gtk_Tree_Iter;
               B     : Gtk_Tree_Iter
            )  return Boolean;

   function Get_Background_Area
            (  Tree_View : not null access Gtk_Tree_View_Record'Class;
               Path      : Gtk_Tree_Path;
               Column    : Gtk_Tree_View_Column := null
            )  return Gdk_Rectangle;
--
-- Get_Background_Color -- Get background color
--
--    Context - The style context
--    State   - The state
--
-- This function is used instead deprecated Get_Background_Color.
--
-- Returns :
--
--    The background color
--
   function Get_Background_Color
            (  Context : not null access Gtk_Style_Context_Record'Class;
               State   : Gtk_State_Flags
            )  return Gdk_RGBA;
--
-- Set_Background_Color -- Set background color
--
--    Widget - The widget
--    Color  - The color to set
--
-- This procedure uses CSS  provider to change the background color of a
-- widget.
--
   procedure Set_Background_Color
             (  Widget : not null access Gtk_Widget_Record'Class;
                Color  : Gdk_RGBA
             );
--
-- Get_Basename -- File base name
--
--    File_Name - A file name
--
-- Gets the last component of the filename. If  file_name  ends  with  a
-- directory  separator  it gets the component before the last slash. If
-- file_name consists only of  directory  separators  (and  on  Windows,
-- possibly  a  drive  letter),  a  single  separator  is  returned.  If
-- file_name is empty, it gets ".".
--
-- Returns :
--
--    The base name
--
   function Get_Basename (File_Name : UTF8_String) return UTF8_String;
--
-- Get_Column -- Get column from a value
--
--   Value - A column value
--
-- This function can be  used  within  a  handler  to  obtain  a  column
-- parameter  from  GLib  value. The list of parameters is passed to the
-- handler as GValues. The function Nth is may  be  used  to  extract  a
-- parameter. The result is GValue to which Get_Column is then applied.
--
-- Returns :
--
--    The column
--
   function Get_Column (Value : GValue) return Gtk_Tree_View_Column;
--
-- Get_Column_No -- Get column number
--
--   Tree_View - A tree view widget
--   Column    - A column of
--
-- Returns :
--
--    The number of Column 0.. or else -1
--
   function Get_Column_No
            (  Tree_View : not null access Gtk_Tree_View_Record'Class;
               Column    : not null access
                           Gtk_Tree_View_Column_Record'Class
            )  return GInt;
--
-- Get_Dirname -- File directory name
--
--    File_Name - A file name
--
-- Gets the directory components of a file name. If the file name has no
-- directory components "." is returned.
--
-- Returns :
--
--    The directory name
--
   function Get_Dirname (File_Name : UTF8_String) return UTF8_String;
--
-- Get_Root -- Of a file name
--
--    File_Name - A file name
--
-- This function returns the root component,of a name, i.e. the  "/"  in
-- UNIX or "C:\" under Windows.
--
-- Returns :
--
--    The file name's root component
--
-- Exceptions :
--
--    Use_Error - File_Name is not an absolute name
--
   function Get_Root (File_Name : UTF8_String) return UTF8_String;
--
-- Get_Row_No -- Get row number
--
--   Tree_View   - A tree view model
--   Iter / Path - An iterator or path in the model
--
-- Returns :
--
--    The row number 0.. or else -1
--
   function Get_Row_No
            (  Model : Gtk_Tree_Model;
               Path  : Gtk_Tree_Path
            )  return GInt;
   function Get_Row_No
            (  Model : Gtk_Tree_Model;
               Iter  : Gtk_Tree_Iter
            )  return GInt;
--
-- GType_Icon -- Type of GIcon
--
   function GType_Icon return GType;
--
-- Get_Screen_Position -- Get screen coordinates of a widget
--
--    Widget - The widget (must be realized)
--    X, Y   - Widget position in screen coordinates
--
-- This procedure  gets screen  coordinates of a widget.  Note  that the
-- widget may share  its window with other widgets.  In other words, the
-- coordinates  of a widget are not  necessarily the coordinates  of its
-- window. This procedure takes this into account.
--
   procedure Get_Screen_Position
             (  Widget : not null access Gtk_Widget_Record'Class;
                X      : out GInt;
                Y      : out GInt
             );
--
-- Get_User_Special_Dir -- Directories associated with the user
--
--    Directory - The directory
--
-- Returns :
--
--    The directory requested
--
   function Get_User_Special_Dir (Directory : User_Directory)
      return UTF8_String;

   procedure Get_Visible_Range
             (  Tree_View  : not null access Gtk_Tree_View_Record'Class;
                Start_Path : out Gtk_Tree_Path;
                End_Path   : out Gtk_Tree_Path
             );
--
-- Is_Absolute -- File path check
--
--    File_Name - A file name
--
-- The  function returns true if the given File_Name is an absolute file
-- name, i.e. it contains a full path from the root  directory  such  as
-- "/usr/local" on UNIX or "C:\windows" on Windows systems.
--
-- Returns :
--
--    True if the name is absolute
--
   function Is_Absolute (File_Name : UTF8_String) return Boolean;

   function Keyval_To_Unicode (Key_Val : Gdk_Key_Type) return GUnichar;
--
-- Keyval_To_UTF8 -- Key to UTF8 conversion
--
--    Key_Val - A key value associated with an event
--
-- When Key_Val does not have a corresponding Unicode  character  it  is
-- treated as NUL (code position 0),
--
-- Retunrs :
--
--    The value UTF-8 encoded
--
   function Keyval_To_UTF8 (Key_Val : Gdk_Key_Type) return UTF8_String;
--
-- Message_Dialog -- Replacement for GtkAda.Dialog
--
--    Message       - The message to show
--    Parent        - The parent widget/window
--    Title         - The dialog title (guessed from Mode if empty)
--    Mode          - The stock image ID
--    Justification - Of the text within the dialog
--    Response      - The dialog completion
--
-- When Mode is  Stock_Dialog_Question  the dialog  has two buttons.  In
-- other cases the dialog has one button.  With two buttons the Response
-- is Gtk_Response_Yes when yes button is pressed and Gtk_Response_No if
-- No is pressed. In other cases it is Gtk_Response_OK.
--
   procedure Message_Dialog
             (  Message       : UTF8_String;
                Parent        : not null access Gtk_Widget_Record'Class;
                Title         : UTF8_String       := "";
                Mode          : UTF8_String       := Stock_Dialog_Error;
                Justification : Gtk_Justification := Justify_Center;
                Response      : access Gtk_Response_Type := null
             );
--
-- Remove -- Delete a file or a directory
--
--    File_Name - To delete
--
-- Exceptions :
--
--    Name_Error - File name does not specify an existing file
--    Use_Error  - The file cannot be deleted
--
   procedure Remove (File_Name : UTF8_String);
--
-- Rename -- Rename a file or a directory
--
--    Old_File_Name - To rename from
--    New_File_Name - To rename into
--
-- Exceptions :
--
--    Name_Error - Old_File_Name does not specify an existing file
--    Use_Error  - The file cannot be renamed
--
   procedure Rename (Old_File_Name, New_File_Name : UTF8_String);
--
-- RGB -- Color construction
--
--    Red, Green, Blue - Components of the color 0.0..1.0 (saturated)
--
-- Returns :
--
--    The coresponding color
--
   function RGB (Red, Green, Blue : GDouble) return Gdk_Color;
--
-- Set_Tip -- Remove tooltip
--
   procedure Set_Tip
             (  Widget : not null access
                         Gtk.Widget.Gtk_Widget_Record'Class
             );
--
-- Set -- Replacement of Set_String which have bugs
--
--    Store  - The tree store / model
--    Row    - The row iterator
--    Column - The column
--    Value  - The value to set
--
   procedure Set
             (  Store  : not null access Gtk_List_Store_Record'Class;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Value  : String
             );
   procedure Set
             (  Store  : not null access Gtk_Tree_Store_Record'Class;
                Row    : Gtk_Tree_Iter;
                Column : GInt;
                Value  : String
             );
--
-- Set -- Missing in 2.14.0, GLib.Value.Set_Object since 2.14.2
--
   procedure Set (Value : in out GValue; Object : GObject);
--
-- Skip_Root -- In a file name
--
--    File_Name - A file name
--
-- This function returns a File_Name part after the root component, i.e.
-- after the "/" in UNIX or "C:\" under Windows.
--
-- Returns :
--
--    The file name without root component
--
-- Exceptions :
--
--    Use_Error - File_Name is not an absolute name
--
   function Skip_Root (File_Name : UTF8_String) return UTF8_String;
--
-- Thaw_Notify -- Thaw emitting signals
--
--     Object - The object
--
-- Reverts the effect of a previous call to  Freeze_Notify.  The  freeze
-- count  is  decreased  on  object  and  when  it  reaches zero, queued
-- "notify" signals are emitted.
-- Duplicate notifications for each property are  squashed  so  that  at
-- most one "notify" signal is emitted for each property, in the reverse
-- order in which they have been queued.
-- It is an error to call this function when the freeze count is zero.
--
   procedure Thaw_Notify
             (  Object : not null access GObject_Record'Class
             );
--
-- Themed_Icon_New -- Create icon by name
--
--    Icon_Name - The icon name
--
-- Returns :
--
--    The icon or null
--
   function Themed_Icon_New (Icon_Name : UTF8_String) return GObject;
--
-- Themed_Icon_New_With_Default_Fallbacks -- Create icon by name
--
--    Icon_Name - The icon name
--
-- Returns :
--
--    The icon
--
   function Themed_Icon_New_With_Default_Fallbacks
            (  Icon_Name : UTF8_String
            )  return GObject;
--
-- To_RGBA -- Conversion to RGBA
--
--    Color - To convert
--
-- Returns :
--
--    The coresponding color in RGBA
--
   function To_RGBA (Color : Gdk_Color) return Gdk_RGBA;
--
-- To_String -- Convert cairo status to string
--
--    Status - The status
--
-- Returns :
--
--    The corresponding string
--
   function To_String (Status : Cairo_Status) return String;

   function Unicode_To_Keyval (WC : GUnichar) return Gdk_Key_Type;
--
-- Wait_Cursor -- Temporarily show wait cursor
--
--    Widget - To show wait cursor at
--
   type Wait_Cursor
        (  Widget : not null access Gtk_Widget_Record'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
   procedure Finalize (Cursor : in out Wait_Cursor);
   procedure Initialize (Cursor : in out Wait_Cursor);
--
-- Gtk.Generic_Enum_Combo_Box styles:
--
--  Mixed_Case       - First letter of each word is capitalized
--  All_Caps         - All letters are capitalized
--  All_Small        - All letters are in lower case
--  Capitalize_First - The first letter is capitalized
--
   type Enum_Style_Type is
        (  Mixed_Case,
           All_Caps,
           All_Small,
           Capitalize_First
        );
   No_Selection : exception; -- Used in Gtk.Generic_Enum_Combo_Box
--
-- Set_Column_Cell_Data -- Column cell data functions
--
--    User_Data - To attach
--    Destroy   - Callback used when the function is detached
--
-- The package provides  a facility to attach a user-defined function to
-- a column of tree view.
--
   generic
      type User_Data (<>) is private;
      with procedure Destroy (Data : in out User_Data) is null;
   package Set_Column_Cell_Data is
      use Gtk.Cell_Renderer;
   --
   -- Cell_Data_Function -- The function to call
   --
   --    Column - The column
   --    Cell   - The renderer
   --    Model  - The tree model
   --    Iter   - The iterator
   --    Data   - The user data
   --
   -- This function is  used instead of the standard attributes  mapping
   -- for setting  the column value,  and should  set the value  of tree
   -- column's cell renderer as appropriate.
   --
      type Cell_Data_Function is access procedure
           (  Column : not null access
                       Gtk_Tree_View_Column_Record'Class;
              Cell   : not null access Gtk_Cell_Renderer_Record'Class;
              Model  : Gtk_Tree_Model;
              Iter   : Gtk_Tree_Iter;
              Data   : User_Data
           );
   --
   -- Set_Cell_Data_Func -- Set callback function
   --
   --    Column - The column
   --    Cell   - The renderer
   --    Func   - The function to use
   --    Data   - The user data
   --
   -- When  set to  null the old function is removed.
   --
      procedure Set_Cell_Data_Func
                (  Column : not null access
                            Gtk_Tree_View_Column_Record'Class;
                   Cell   : not null access
                            Gtk_Cell_Renderer_Record'Class;
                   Func   : Cell_Data_Function;
                   Data   : User_Data
                );
   end Set_Column_Cell_Data;

private
   pragma Inline (Check);
   pragma Inline (Is_A);

   pragma Import (C, Class_From_Type,   "g_type_class_peek");
   pragma Import (C, Keyval_To_Unicode, "gdk_keyval_to_unicode");
   pragma Import (C, Unicode_To_Keyval, "gdk_unicode_to_keyval");
   pragma Import (C, Dir_Rewind,        "g_dir_rewind");
   pragma Import (C, GType_Icon,        "g_icon_get_type");
   pragma Import (C, Class_Install_Property,
                                     "g_object_class_install_property");
--
-- Search_Data -- To enumerate elements of a container
--
   type Search_Data is record
      Item  : Gtk_Widget;
      Found : Boolean;
   end record;
   type Search_Data_Ptr is not null access all Search_Data;

   package For_Test is
      new Gtk.Container.Foreach_User_Data (Search_Data_Ptr);
   procedure Test
             (  Item : not null access Gtk_Widget_Record'Class;
                Data : Search_Data_Ptr
             );

   type Wait_Cursor
        (  Widget : not null access Gtk_Widget_Record'Class
        )  is new Ada.Finalization.Limited_Controlled with
   record
      Realized : Boolean;
      Window   : Gdk.Gdk_Window;
   end record;

end Gtk.Missed;
