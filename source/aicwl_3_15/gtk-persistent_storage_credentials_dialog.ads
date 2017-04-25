--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Persistent_Storage_                    Luebeck            --
--          Credentials_Dialog                     Winter, 2008       --
--  Interface                                                         --
--                                Last revision :  10:30 31 May 2014  --
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
--  This package provides credential  dialogs  implementing  queries  to
--  connect to a persistent storage data source.
--
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Gtk.Button;             use Gtk.Button;
with Gtk.Check_Button;       use Gtk.Check_Button;
with Gtk.Dialog;             use Gtk.Dialog;
with Gtk.GEntry;             use Gtk.GEntry;
with Gtk.Label;              use Gtk.Label;
with Gtk.Missed;             use Gtk.Missed;
with Gtk.Table;              use Gtk.Table;
with Persistent.Handle;      use Persistent.Handle;

with Gtk.Handlers;

with Gtk.Persistent_Storage_Browser;
use  Gtk.Persistent_Storage_Browser;

package Gtk.Persistent_Storage_Credentials_Dialog is
   pragma Elaborate_Body (Gtk.Persistent_Storage_Credentials_Dialog);
--
-- Class name
--
   Persistent_Storage_Credentials_Dialog_Class_Name : constant String :=
      "GtkPersistentStorageCredentialsDialog";
--
-- Gtk_Persistent_Storage_Credentials_Dialog_Record -- The dialog
--
-- Style properties :
--
--    cancel-button-label   - The label of the  cancel  button.  String.
--                            Default: _Cancel.
--    column-spacing        - Column spacing  of  the  dialog  elements.
--                            GInt. Default: 3.
--    confirm-button-label  - The  label  of the confirm button. String.
--                            Default: _OK.
--    credentials-error     - The  message  on data source access error.
--                            String.
--    empty-name-error      - The  message  when  data  source  name  is
--                            empty. String
--    name-label            - The label of the data source  name  entry.
--                            String. Default: Name.
--    open-error            - The  message  on  data  source open error.
--                            String.
--    password-label        - The  label  of the password entry. String.
--                            Default: Password.
--    row-spacing           - Row spacing of the dialog elements.  GInt.
--                            Default: 3.
--    stored-password-label - The label of the  password  storing  check
--                            box. String. Default: Remember password.
--    user-label            - The  label of the user name entry. String.
--                            Default: user.
--    title                 - The  dialog  title.  String. Default: User
--                            credentials.
--
   type Gtk_Persistent_Storage_Credentials_Dialog_Record is
      new Gtk_Dialog_Record with private;
   type Gtk_Persistent_Storage_Credentials_Dialog is
      access all Gtk_Persistent_Storage_Credentials_Dialog_Record'Class;
--
-- Get_Type -- The widget type
--
-- Returns :
--
--     GTK+ widget type
--
   function Get_Type return GType;
--
-- Gtk_New -- Dialog factory
--
--    Dialog - The result
--  [ Name ] - The persistent storage name
--    User   - The user name (initial)
--    Stored - Password storing flag (initial)
--
-- When  the  parameter  Name  is  specified, the corresponding entry is
-- insensitive. Otherwise, it is sensitive and empty.
--
   procedure Gtk_New
             (  Dialog : out Gtk_Persistent_Storage_Credentials_Dialog;
                Name   : UTF8_String;
                User   : UTF8_String;
                Stored : Boolean
             );
   procedure Gtk_New
             (  Dialog : out Gtk_Persistent_Storage_Credentials_Dialog;
                User   : UTF8_String;
                Stored : Boolean
             );
--
-- Initialize -- Initialization
--
--    Dialog - The result
--  [ Name ] - The persistent storage name
--    User   - The user name (initial)
--    Stored - Password storing flag (initial)
--
-- This  procedure  has  to  be  called  by  any  derived  type upon its
-- initialization.
--
   procedure Initialize
             (  Dialog : not null access
                Gtk_Persistent_Storage_Credentials_Dialog_Record'Class;
                Name   : UTF8_String;
                User   : UTF8_String;
                Stored : Boolean
             );
   procedure Initialize
             (  Dialog : not null access
                Gtk_Persistent_Storage_Credentials_Dialog_Record'Class;
                User   : UTF8_String;
                Stored : Boolean
             );
--
-- Get_Name -- Get storage name
--
--    Dialog - The dialog
--
-- Returns :
--
--    The storage name (data source or server name)
--
   function Get_Name
            (  Dialog : not null access
                        Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return UTF8_String;
--
-- Get_User -- Get the user name
--
--    Dialog - The dialog
--
-- Returns :
--
--    The user name
--
   function Get_User
            (  Dialog : not null access
                        Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return UTF8_String;
--
-- Get_Password -- Get the password
--
--    Dialog - The dialog
--
-- Returns :
--
--    The password
--
   function Get_Password
            (  Dialog : not null access
                        Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return UTF8_String;
--
-- Get_Storing_Flag -- Get the password storing flag
--
--    Dialog - The dialog
--
-- Returns :
--
--    True if stored password
--
   function Get_Storing_Flag
            (  Dialog : not null access
                        Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return Boolean;
--
-- Error -- Show error message in the dialog
--
--    Dialog - The dialog
--    Text   - The message to show
--
   procedure Error
             (  Dialog : not null access
                   Gtk_Persistent_Storage_Credentials_Dialog_Record;
                Text   : UTF8_String
             );
--
-- Dialog_Credentials_Query -- Query  object  based  on  the dialog. The
--                             type is abstract and has single operation
--                             Create to override.
--
   type Dialog_Credentials_Query is abstract
      new Abstract_Credentials_Query with private;
--
-- Connect -- Opens a connection to the persistent storage
--
--    Query    - The object
--    Name     - The storage name (data source or server name)
--    User     - The user name
--    Password - The password
--
-- Returns :
--
--    A handle to the storage
--
-- Exceptions :
--
--    Data_Error - Storage error
--    Use_Error  - Credentials error
--
   function Connect
            (  Query    : Dialog_Credentials_Query;
               Name     : UTF8_String;
               User     : UTF8_String;
               Password : UTF8_String
            )  return Storage_Handle is abstract;
private
--
-- Gtk_Persistent_Storage_Credentials_Dialog_Record
--
   type Gtk_Persistent_Storage_Credentials_Dialog_Record is
      new Gtk_Dialog_Record with
   record
      Grid           : Gtk_Table;
      OK_Button      : Gtk_Button;
      Cancel_Button  : Gtk_Button;
      Name_Label     : Gtk_Label;
      User_Label     : Gtk_Label;
      Password_Label : Gtk_Label;
      Error_Label    : Gtk_Label;
      Name_Entry     : Gtk_Entry;
      User_Entry     : Gtk_Entry;
      Password_Entry : Gtk_Entry;
      Store_Password : Gtk_Check_Button;
   end record;
--
-- Run -- Execute the dialog
--
--    Dialog - The dialog
--
-- This  function  executes  the dialog. The result is a valid handle to
-- the storage when the name and credentials were accepted. Otherwise is
-- invalid.
--
-- Returns :
--
--    A handle to the persistent storage
--
   function Run
            (  Dialog : access
                  Gtk_Persistent_Storage_Credentials_Dialog_Record;
               Query  : Dialog_Credentials_Query'Class
            )  return Storage_Handle;
--
-- Style_Updated -- Handler of "style-updated"
--
   procedure Style_Updated
             (  Dialog : access
                Gtk_Persistent_Storage_Credentials_Dialog_Record'Class
             );
   package Dialog_Handlers is
      new Gtk.Handlers.Callback
          (  Gtk_Persistent_Storage_Credentials_Dialog_Record
          );

   type Dialog_Credentials_Query is abstract
      new Abstract_Credentials_Query with null record;
--
-- Create -- Overrides Gtk.Persistent_Storage_Browser...
--
   procedure Create
             (  Query           : in out Dialog_Credentials_Query;
                Scheme          : out Scheme_Type;
                Name            : out Unbounded_String;
                User            : out Unbounded_String;
                Password        : out Unbounded_String;
                Stored_Password : out Boolean;
                Storage         : out Storage_Handle
             );
--
-- Get -- Overrides Gtk.Persistent_Storage_Browser...
--
   procedure Get
             (  Query           : in out Dialog_Credentials_Query;
                Scheme          : Scheme_Type;
                Name            : UTF8_String;
                User            : in out Unbounded_String;
                Password        : in out Unbounded_String;
                Stored_Password : in out Boolean;
                Storage         : out Storage_Handle
             );
end Gtk.Persistent_Storage_Credentials_Dialog;
