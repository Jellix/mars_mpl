--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Recent_Manager_Alt                      Luebeck            --
--  Interface                                      Winter, 2008       --
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

with Ada.Calendar;  use Ada.Calendar;
with Gdk.Pixbuf;    use Gdk.Pixbuf;
with Gdk.Screen;    use Gdk.Screen;
with GLib.Error;    use GLib.Error;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Missed;    use Gtk.Missed;
with Gtkada.Types;  use Gtkada.Types;

with Interfaces.C.Pointers;

package Gtk.Recent_Manager_Alt is
--
-- Gtk_Recent_Info -- Contains  informations  found  when  looking up an
--                    entry of the recently used files list
--
   type Gtk_Recent_Info is new GLib.C_Proxy;
   type Gtk_Recent_Info_Array is
      array (Positive range <>) of Gtk_Recent_Info;
--
-- Application_Info -- Application infromation, a wrapper type used with
--                     Get_Application_Info.
--
   type Application_Info
        (  Registered      : Boolean;
           App_Exec_Length : Natural
        )  is
   record
      case Registered is
         when True =>
            Count     : GUInt;
            Last_Time : Time;
            App_Exec  : UTF8_String (1..App_Exec_Length);
         when False =>
            null;
      end case;
   end record;
--
-- Exists -- Checks  whether  the resource pointed by info still exists.
--           At the moment this check is done only on resources pointing
--           to local files.
--
--    Info - Recent info item
--
-- Returns :
--
--    True if the resource exists
--
   function Exists (Info : Gtk_Recent_Info) return Boolean;
--
-- Get_Added -- Gets the time when the resource was added
--
--    Info - Recent info item
--
-- Returns :
--
--    The time
--
-- Excpetions :
--
--    Time_Error - No time available
--
   function Get_Added (Info : Gtk_Recent_Info) return Time;
--
-- Get_Age -- Gets the duration elapsed since the  last  update  of  the
--            resource pointed
--
--    Info - Recent info item
--
-- Returns :
--
--    The update duration
--
   function Get_Age (Info : Gtk_Recent_Info) return Duration;
--
-- Get_Application_Info -- Gets the data regarding the application  that
--                         has registered the resource pointed
--
--    Info     - Recent info item
--    App_Name - The application name
--
-- Returns :
--
--    Application data
--
   function Get_Application_Info
            (  Info : Gtk_Recent_Info;
               App_Name : UTF8_String
            )  return Application_Info;
--
-- Get_Applications -- The  list  of  applications  that have registered
--                     this resource
--
--    Info - Recent info item
--
-- The result has to be freed using GtkAda.Types.Free
--
-- Returns :
--
--    Application names list
--
   function Get_Applications (Info : Gtk_Recent_Info)
      return Chars_Ptr_Array;
--
-- Get_Description -- Short description of the resource.
--
--    Info - Recent info item
--
-- Returns :
--
--    The description of
--
   function Get_Description (Info : Gtk_Recent_Info)
      return UTF8_String;
--
-- Get_Display_Name -- The display name of the resource
--
--    Info - Recent info item
--
-- Returns :
--
--    The display name of
--
   function Get_Display_Name (Info : Gtk_Recent_Info)
      return UTF8_String;
--
-- Get_MIME_Type -- The MIME type of the resource
--
--    Info - Recent info item
--
-- Returns :
--
--    The MIME type of
--
   function Get_MIME_Type (Info : Gtk_Recent_Info)
      return UTF8_String;
--
-- Get_Groups -- The groups registered for
--
--    Info - Recent info item
--
-- The result has to be freed using GtkAda.Types.Free
--
-- Returns :
--
--    Group names list
--
   function Get_Groups (Info : Gtk_Recent_Info)
      return Chars_Ptr_Array;
--
-- Get_Icon -- Retrieves  the  icon  of  size  Size  associated  to  the
--             resource MIME type
--
--    Info - Recent info item
--    Size - of the icon in pixels
--
-- Unref has to be called on the result
--
-- Returns :
--
--    A Gdk_Pixbuf containing the icon, or null
--
   function Get_Icon
            (  Info : Gtk_Recent_Info;
               Size : GInt
            )  return Gdk_Pixbuf;
--
-- Get_Modified -- Resource modification time
--
--    Info - Recent info item
--
-- Returns :
--
--    The modifiection time
--
-- Excpetions :
--
--    Time_Error - No time available
--
   function Get_Modified (Info : Gtk_Recent_Info) return Time;
--
-- Get_URI -- Gets the URI of the resource
--
--    Info - Recent info item
--
-- Returns :
--
--    URI
--
   function Get_URI (Info : Gtk_Recent_Info) return UTF8_String;
--
-- Get_URI_Display -- Gets  the  name  of the resource. If none has been
--                    defined, the basename of the resource is obtained
--
--    Info - Recent info item
--
-- Returns :
--
--    URI display
--
   function Get_URI_Display (Info : Gtk_Recent_Info) return UTF8_String;
--
-- Get_Private_Hint -- Gets the value of the "private" flag
--
--    Info - Recent info item
--
-- Returns :
--
--    The private flag
--
   function Get_Private_Hint (Info : Gtk_Recent_Info) return Boolean;
--
-- Get_Short_Name -- The short name of the resource
--
--    Info - Recent info item
--
-- The function computes a valid UTF-8 string that can be  used  as  the
-- name  of  the  item  in  a  menu  or  list. For example, calling this
-- function on an item that refers to "file:///foo/bar.txt"  will  yield
-- "bar.txt".
--
-- Returns :
--
--    The name
--
   function Get_Short_Name (Info : Gtk_Recent_Info) return UTF8_String;
--
-- Get_Visited -- The last time the resource was visited
--
--    Info - Recent info item
--
-- Returns :
--
--    The time
--
-- Excpetions :
--
--    Time_Error - No time available
--
   function Get_Visited (Info : Gtk_Recent_Info) return Time;
--
-- Has_Application -- Checks  whether  an  application  registered  this
--                    resource
--
--    Info     - Recent info item
--    App_Name - Application name
--
-- Returns :
--
--    True if Info was registered by the application
--
   function Has_Application
            (  Info       : Gtk_Recent_Info;
               App_Name : UTF8_String
            )  return Boolean;
--
-- Has_Group -- Checks whether an group was specified for the resource
--
--    Info  - Recent info item
--    Group - The group
--
-- Returns :
--
--    True if Info has the group
--
   function Has_Group
            (  Info       : Gtk_Recent_Info;
               Group_Name : UTF8_String
            )  return Boolean;
--
-- Is_Local -- Checks whether the resource is local or not
--
--    Info  - Recent info item
--
-- Returns :
--
--    True if Info is local
--
   function Is_Local (Info : Gtk_Recent_Info) return Boolean;
--
-- Last_Application -- The  name  of  the  last  application  that  have
--                     registered the recently used resource represented
--                     by info
--
--    Info  - Recent info item
--
-- Returns :
--
--    The application name
--
   function Last_Application (Info : Gtk_Recent_Info)
      return UTF8_String;
--
-- Match -- Check if two items points the same resource
--
--    Info_A  - A recent info item
--    Info_B  - A recent info item
--
-- Returns :
--
--    True if the resources are same
--
   function Match (Info_A, Info_B : Gtk_Recent_Info) return Boolean;
--
-- Ref -- Increase the reference count
--
--    Info  - Recent info item
--
   procedure Ref (Info : Gtk_Recent_Info);
--
-- Unref -- Decrease the reference count
--
--    Info  - Recent info item
--
   procedure Unref (Info : Gtk_Recent_Info);
--
-- Gtk_Recent_Manager_Record -- Acts as a database of information  about
--                              the list of recently used files
--
   type Gtk_Recent_Manager_Record is
      new Glib.Object.GObject_Record with null record;
   type Gtk_Recent_Manager is
      access all Gtk_Recent_Manager_Record'Class;
--
-- Item_Info -- A helper type used with Lookup_Item
--
   type Item_Disposition is (Found, Not_Found, Error);
   type Item_Info (Status : Item_Disposition) is record
      case Status is
         when Found =>
            Info : Gtk_Recent_Info;
         when Not_Found =>
            null;
         when Error =>
            Error : GError;
      end case;
   end record;
--
-- Add_Full -- Add new resource
--
--    Manager      - A manager
--    URI          - Of the resource
--    Display_Name
--    Description
--    MIME_Type
--    App_Name
--    App_Exec
--    Groups
--    Is_Private
--
-- Returns :
--
--    True if an item was successfully added
--
   function Add_Full
           (  Manager      : not null access Gtk_Recent_Manager_Record;
              URI          : UTF8_String;
              Display_Name : UTF8_String;
              Description  : UTF8_String;
              MIME_Type    : UTF8_String := "application/octet-stream";
              App_Name     : UTF8_String := Get_Application_Name;
              App_Exec     : UTF8_String := " " & Get_PRGName & "%u";
              Groups       : Chars_Ptr_Array := Null_Array;
              Is_Private   : Boolean         := False
           )  return Boolean;
--
-- Add_Item -- Add new resource
--
--    Manager - A manager
--    URI     - Of the resource
--
-- Returns :
--
--    True if an item was successfully added
--
   function Add_Item
           (  Manager : not null access Gtk_Recent_Manager_Record;
              URI     : UTF8_String
           )  return Boolean;
--
-- Get_Default -- The default manager
--
-- Returns :
--
--    The default manager
--
   function Get_Default return Gtk_Recent_Manager;
--
-- Get_Items -- Gets the list of recently used resources
--
--    Manager - A manager
--
-- The result elements have to be deallocated by calling Uref on each of
-- them.
--
-- Returns :
--
--    The array of newly allocated Gtk_Recent_Info objects
--
   function Get_Items
            (  Manager : not null access Gtk_Recent_Manager_Record
            )  return Gtk_Recent_Info_Array;
--
-- Gtk_New -- Factory
--
--    Manager - New manager
--
   procedure Gtk_New (Manager : out Gtk_Recent_Manager);
--
-- Has_Item -- Check if an item exists
--
--    Manager - A manager
--    URI     - Of the resource
--
-- Returns :
--
--    True if Manager has an item for URI
--
   function Has_Item
           (  Manager : not null access Gtk_Recent_Manager_Record;
              URI     : UTF8_String
           )  return Boolean;
--
-- Initialize -- Construction
--
--    Manager - A manager
--
-- This procedure has to be called by any derived type
--
   procedure Initialize
             (  Manager : not null access
                          Gtk_Recent_Manager_Record'Class
             );
--
-- Lookup_Item -- Get an item
--
--    Manager - A manager
--    URI     - Of the resource
--
-- The fields Info and Error of the result have to be passed to Unref or
-- Error_Free as appropriate.
--
-- Returns :
--
--    The lookup result
--
   function Lookup_Item
            (  Manager : not null access Gtk_Recent_Manager_Record;
               URI     : UTF8_String
            )  return Item_Info;
--
-- Move_Item -- Changes the location of a recently used resource
--
--    Manager - A manager
--    URI     - Of the resource
--    New_URI - Of the resource
--    Error   - The error
--
-- When Error is not null it indicates an error and has to be passed  to
-- Error_Free.
--
   procedure Move_Item
             (  Manager : not null access Gtk_Recent_Manager_Record;
                URI     : UTF8_String;
                New_URI : UTF8_String;
                Error   : out GError
             );
--
-- Purge_Items -- Purges every item from the resources list
--
--    Manager - A manager
--    Error   - The error
--    Removed - Items count
--
-- When Error is not null it indicates an error and has to be passed  to
-- Error_Free.
--
   procedure Purge_Items
             (  Manager : not null access Gtk_Recent_Manager_Record;
                Error   : out GError;
                Removed : out GInt
             );
--
-- Remove_Item -- Remove the resource from the list
--
--    Manager - A manager
--    URI     - Of the resource
--    Error   - The error
--
-- When Error is not null it indicates an error and has to be passed  to
-- Error_Free.
--
   procedure Remove_Item
             (  Manager : not null access Gtk_Recent_Manager_Record;
                URI     : UTF8_String;
                Error   : out GError
             );
private
--
-- !!WARNING!!  The following depends on  the  C  run-time  library  and
--              should be  corrected  when  necessary.  We  assume  four
-- things about time_t here:
--
-- 1. time_t has signed long's size
-- 2. It is in seconds
-- 3. Its epoch is 1 January 1970
-- 4. Its time zone is same as one of Ada.Calendar
--
   type Time_T is new Interfaces.C.Long;
   function To_Time (T : Time_T) return Time;

   pragma Import (C, Unref, "gtk_recent_info_unref");

end Gtk.Recent_Manager_Alt;
