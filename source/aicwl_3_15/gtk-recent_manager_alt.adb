--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Recent_Manager_Alt                      Luebeck            --
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

with Gdk.Pixbuf.Conversions;
with GLib.Chars_Ptr_Vectors;
with Interfaces.C.Strings;

package body Gtk.Recent_Manager_Alt is
   use Interfaces.C;
   use type Interfaces.C.Strings.Chars_Ptr;

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
           )  return Boolean is
      type Gtk_Recent_Data is record
         Display_Name : System.Address;
         Description  : System.Address;
         MIME_Type    : System.Address;
         App_Name     : System.Address;
         App_Exec     : System.Address;
         Groups       : System.Address;
         Is_Private   : GBoolean;
      end record;
      pragma Convention (C, Gtk_Recent_Data);
      function Internal
               (  Manager     : System.Address;
                  URI         : Char_Array;
                  Recent_Data : Gtk_Recent_Data
               )  return GBoolean;
      pragma Import (C, Internal, "gtk_recent_manager_add_full");
      C_Display_Name : aliased Char_Array := To_C (Display_Name);
      C_Description  : aliased Char_Array := To_C (Description);
      C_MIME_Type    : aliased Char_Array := To_C (MIME_Type);
      C_App_Name     : aliased Char_Array := To_C (App_Name);
      C_App_Exec     : aliased Char_Array := To_C (App_Exec);
      C_Groups       : aliased Chars_Ptr_Array (0..Groups'Length);
      C_Is_Private   : GBoolean;
      use Interfaces.C.Strings;
   begin
      if Is_Private then
         C_Is_Private := 1;
      else
         C_Is_Private := 0;
      end if;
      C_Groups (C_Groups'Last) := Interfaces.C.Strings.Null_Ptr;
      if Groups'Length > 0 then
         C_Groups (C_Groups'First..C_Groups'Last - 1) := Groups;
      end if;
      return
      (  0
      /= Internal
         (  Get_Object (Manager),
            To_C (URI),
            (  Display_Name => C_Display_Name'Address,
               Description  => C_Description'Address,
               MIME_Type    => C_MIME_Type'Address,
               App_Name     => C_App_Name'Address,
               App_Exec     => C_App_Exec'Address,
               Groups       => C_Groups'Address,
               Is_Private   => C_Is_Private
      )  )  );
   end Add_Full;

   function Add_Item
           (  Manager : not null access Gtk_Recent_Manager_Record;
              URI     : UTF8_String
           )  return Boolean is
      function Internal
               (  Manager : System.Address;
                  URI     : Char_Array
               )  return GBoolean;
      pragma Import (C, Internal, "gtk_recent_manager_add_item");
   begin
      return Internal (Get_Object (Manager), To_C (URI)) /= 0;
   end Add_Item;

   function Exists (Info : Gtk_Recent_Info) return Boolean is
      function Internal (Info : Gtk_Recent_Info) return GBoolean;
      pragma Import (C, Internal, "gtk_recent_info_exists");
   begin
      return Internal (Info) /= 0;
   end Exists;

   function Get_Added (Info : Gtk_Recent_Info) return Time is
      function Internal (Info : Gtk_Recent_Info) return Time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_added");
   begin
      return To_Time (Internal (Info));
   end Get_Added;

   function Get_Age (Info : Gtk_Recent_Info) return Duration is
      function Internal (Info : Gtk_Recent_Info) return GInt;
      pragma Import (C, Internal, "gtk_recent_info_get_age");
      Day_Seconds : constant := 60.0 * 60.0 * 24.0;
   begin
      return Duration (Float (Internal (Info)) * Day_Seconds);
   end Get_Age;

   function Get_Application_Info
            (  Info     : Gtk_Recent_Info;
               App_Name : UTF8_String
            )  return Application_Info is
      function Internal
               (  Info     : Gtk_Recent_Info;
                  App_Name : Char_Array;
                  App_Exec : access Gtkada.Types.Chars_Ptr;
                  Count    : access GUInt;
                  Time     : access Time_t
               )  return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_recent_info_get_application_info"
             );
      App_Exec : aliased Gtkada.Types.Chars_Ptr;
      Count    : aliased GUInt;
      Time     : aliased Time_T;
   begin
      if (  0
         =  Internal
            (  Info,
               Interfaces.C.To_C (App_Name),
               App_Exec'Access,
               Count'Access,
               Time'Access
         )  )
      then
         if App_Exec = Null_Ptr then
            return (True, 0, Count, To_Time (Time), "");
         else
            declare
               Exec   : constant String :=
                           Interfaces.C.Strings.Value (App_Exec);
               Result : constant Application_Info :=
                  (True, Exec'Length, Count, To_Time (Time), Exec);
            begin
               Gtkada.Types.G_Free (App_Exec);
               return Result;
            end;
         end if;
      else
         return (False, 0);
      end if;
   end Get_Application_Info;

   function Get_Applications (Info : Gtk_Recent_Info)
      return Chars_Ptr_Array is
      use GLib.Chars_Ptr_Vectors;
      function Internal
               (  Info   : Gtk_Recent_Info;
                  Length : access GSize
               )  return Chars_Ptr_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_applications");
      Length : aliased GSize;
   begin
      return Convert_And_Free (Internal (Info, Length'Access));
   end Get_Applications;

   function Get_Default return Gtk_Recent_Manager is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_get_default");
      Stub : Gtk_Recent_Manager_Record;
   begin
      return Gtk_Recent_Manager (Get_User_Data_Fast (Internal, Stub));
   end Get_Default;

   function Get_Description (Info : Gtk_Recent_Info)
      return UTF8_String is
      function Internal (Info : Gtk_Recent_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_description");
      Result : constant Chars_Ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end Get_Description;

   function Get_Display_Name (Info : Gtk_Recent_Info)
      return UTF8_String is
      function Internal (Info : Gtk_Recent_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_display_name");
      Result : constant Chars_Ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end Get_Display_Name;

   function Get_Groups (Info : Gtk_Recent_Info)
      return Chars_Ptr_Array is
      use GLib.Chars_Ptr_Vectors;
      function Internal
               (  Info   : Gtk_Recent_Info;
                  Length : access GSize
               )  return Chars_Ptr_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_groups");
      Length : aliased GSize;
   begin
      return Convert_And_Free (Internal (Info, Length'Access));
   end Get_Groups;

   function Get_Icon
            (  Info : Gtk_Recent_Info;
               Size : GInt
            )  return Gdk_Pixbuf is
      function Internal
               (  Info : Gtk_Recent_Info;
                  Size : GInt
               )  return System.Address;
      pragma Import (C, Internal, "gtk_recent_info_get_icon");
   begin
      return
         Gdk.Pixbuf.Conversions.From_Address (Internal (Info, Size));
   end Get_Icon;

   function Get_MIME_Type (Info : Gtk_Recent_Info)
      return UTF8_String is
      function Internal (Info : Gtk_Recent_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_mime_type");
      Result : constant Chars_Ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end Get_MIME_Type;

   function Get_Items
            (  Manager : not null access Gtk_Recent_Manager_Record
            )  return Gtk_Recent_Info_Array is
      type GList;
      type GList_Ptr is access all GList;
      pragma Convention (c, GList_Ptr);
      type GList is record
         Data : Gtk_Recent_Info;
         Next : GList_Ptr;
         Prev : GList_Ptr;
      end record;
      pragma Convention (C, GList);
      procedure Free (List : GList_Ptr);
      pragma Import (C, Free, "g_list_free");
      function Length (List : GList_Ptr) return GUInt;
      pragma Import (C, Length, "g_list_length");

      function Internal (Manager : System.Address)
         return GList_Ptr;
      pragma Import (C, Internal, "gtk_recent_manager_get_items");
      List : constant GList_Ptr := Internal (Get_Object (Manager));
   begin
      if List = null then
         return (1..0 => null);
      else
         declare
            This   : GList_Ptr := List;
            Result : Gtk_Recent_Info_Array
                        (1..Positive (Length (List)));
         begin
            for Index in Result'Range loop
               Result (Index) := This.Data;
               This := This.Next;
            end loop;
            Free (List);
            return Result;
         end;
      end if;
   end Get_Items;

   function Get_Modified (Info : Gtk_Recent_Info) return Time is
      function Internal (Info : Gtk_Recent_Info) return Time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_modified");
   begin
      return To_Time (Internal (Info));
   end Get_Modified;

   function Get_Private_Hint (Info : Gtk_Recent_Info) return Boolean is
      function Internal (Info : Gtk_Recent_Info) return GBoolean;
      pragma Import (C, Internal, "gtk_recent_info_get_private_hint");
   begin
      return Internal (Info) /= 0;
   end Get_Private_Hint;

   function Get_Short_Name (Info : Gtk_Recent_Info)
      return UTF8_String is
      function Internal (Info : Gtk_Recent_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_short_name");
      Ptr    : constant Chars_Ptr   := Internal (Info);
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Get_Short_Name;

   function Get_URI (Info : Gtk_Recent_Info) return UTF8_String is
      function Internal (Info : Gtk_Recent_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_uri");
      Result : constant Chars_Ptr := Internal (Info);
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end Get_URI;

   function Get_URI_Display (Info : Gtk_Recent_Info)
      return UTF8_String is
      function Internal (Info : Gtk_Recent_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_get_uri_display");
      Ptr    : constant Chars_Ptr   := Internal (Info);
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Get_URI_Display;

   function Get_Visited (Info : Gtk_Recent_Info) return Time is
      function Internal (Info : Gtk_Recent_Info) return Time_t;
      pragma Import (C, Internal, "gtk_recent_info_get_visited");
   begin
      return To_Time (Internal (Info));
   end Get_Visited;

   procedure Gtk_New (Manager : out Gtk_Recent_Manager) is
   begin
      Manager := new Gtk_Recent_Manager_Record;
      Gtk.Recent_Manager_Alt.Initialize (Manager);
   end Gtk_New;

   function Has_Application
            (  Info     : Gtk_Recent_Info;
               App_Name : UTF8_String
            )  return Boolean is
      function Internal
               (  Info       : Gtk_Recent_Info;
                  Group_Name : Char_Array
               )  return GBoolean;
      pragma Import
             (C, Internal, "gtk_recent_info_has_application");
   begin
      return Internal (Info, To_C (App_Name)) /= 0;
   end Has_Application;

   function Has_Group
            (  Info       : Gtk_Recent_Info;
               Group_Name : UTF8_String
            )  return Boolean is
      function Internal
               (  Info       : Gtk_Recent_Info;
                  Group_Name : Char_Array
               )  return GBoolean;
      pragma Import (C, Internal, "gtk_recent_info_has_group");
   begin
      return Internal (Info, To_C (Group_Name)) /= 0;
   end Has_Group;

   function Has_Item
           (  Manager : not null access Gtk_Recent_Manager_Record;
              URI     : UTF8_String
           )  return Boolean is
      function Internal
               (  Manager : System.Address;
                  URI     : Char_Array
               )  return GBoolean;
      pragma Import (C, Internal, "gtk_recent_manager_has_item");
   begin
      return 0 /= Internal (Get_Object (Manager), To_C (URI));
   end Has_Item;

   procedure Initialize
             (  Manager : not null access
                          Gtk_Recent_Manager_Record'Class
             )  is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_manager_new");
   begin
      Set_Object (Manager, Internal);
   end Initialize;

   function Is_Local (Info : Gtk_Recent_Info) return Boolean is
      function Internal (Info : Gtk_Recent_Info) return GBoolean;
      pragma Import (C, Internal, "gtk_recent_info_is_local");
   begin
      return Internal (Info) /= 0;
   end Is_Local;

   function Last_Application (Info : Gtk_Recent_Info)
      return UTF8_String is
      function Internal (Info : Gtk_Recent_Info) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_recent_info_last_application");
      Ptr    : constant Chars_Ptr   := Internal (Info);
      Result : constant UTF8_String := Interfaces.C.Strings.Value (Ptr);
   begin
      G_Free (Ptr);
      return Result;
   end Last_Application;

   function Lookup_Item
            (  Manager  : not null access Gtk_Recent_Manager_Record;
               URI     : UTF8_String
            )  return Item_Info is
      function Internal
               (  Manager : System.Address;
                  URI     : Char_Array;
                  Error   : access GError
               )  return Gtk_Recent_Info;
      pragma Import (C, Internal, "gtk_recent_manager_lookup_item");
      Code : aliased GError;
      Item : constant Gtk_Recent_Info :=
                Internal
                (  Get_Object (Manager),
                   To_C (URI),
                   Code'Access
                );
   begin
      if Item = null then
         if Code = null then
            return (Status => Not_Found);
         else
            return (Error, Code);
         end if;
      else
         return (Found, Item);
      end if;
   end Lookup_Item;

   function Match (Info_A, Info_B : Gtk_Recent_Info) return Boolean is
      function Internal (A, B : Gtk_Recent_Info) return GBoolean;
      pragma Import (C, Internal, "gtk_recent_info_match");
   begin
      return Internal (Info_A, Info_B) /= 0;
   end Match;

   procedure Move_Item
             (  Manager : not null access Gtk_Recent_Manager_Record;
                URI     : UTF8_String;
                New_URI : UTF8_String;
                Error   : out GError
             )  is
      function Internal
               (  Manager : System.Address;
                  URI     : Char_Array;
                  New_URI : Char_Array;
                  Error   : access GError
               )  return GBoolean;
      pragma Import (C, Internal, "gtk_recent_manager_move_item");
      Code : aliased GError;
   begin
      if (  0
         =  Internal
            (  Get_Object (Manager),
               To_C (URI),
               To_C (New_URI),
               Code'Access
         )  )
     then
         Error := Code;
      else
         Error := null;
      end if;
   end Move_Item;

   procedure Purge_Items
             (  Manager : not null access Gtk_Recent_Manager_Record;
                Error   : out GError;
                Removed : out GInt
             )  is
      function Internal
               (  Manager : System.Address;
                  Error   : access GError
               )  return GInt;
      pragma Import (C, Internal, "gtk_recent_manager_purge_items");
      Code : aliased GError := null;
   begin
      Removed := Internal (Get_Object (Manager), Code'Access);
      Error   := Code;
   end Purge_Items;

   procedure Ref (Info : Gtk_Recent_Info) is
      function Internal (Info : Gtk_Recent_Info) return Gtk_Recent_Info;
      pragma Import (C, Internal, "gtk_recent_info_ref");
      Self : Gtk_Recent_Info;
   begin
      Self := Internal (Info);
   end Ref;

   procedure Remove_Item
             (  Manager : not null access Gtk_Recent_Manager_Record;
                URI     : UTF8_String;
                Error   : out GError
             )  is
      function Internal
               (  Manager : System.Address;
                  URI     : Char_Array;
                  Error   : access GError
               )  return GBoolean;
      pragma Import (C, Internal, "gtk_recent_manager_remove_item");
      Code : aliased GError;
   begin
      if (  0
         =  Internal
            (  Get_Object (Manager),
               To_C (URI),
               Code'Access
         )  )
     then
         Error := Code;
      else
         Error := null;
      end if;
   end Remove_Item;

   function To_Time (T : Time_T) return Time is
      Epoch : constant Time := Time_Of (1970, 1, 1, 0.0);
   begin
      if T = -1 then
         raise Time_Error;
      else
         return Epoch + Duration (T);
      end if;
   end To_Time;

end Gtk.Recent_Manager_Alt;
