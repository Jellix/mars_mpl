--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Persistent_Storage_                     Luebeck            --
--         Credentials_Dialog                      Winter, 2008       --
--  Implementation                                                    --
--                                Last revision :  09:44 08 Oct 2016  --
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
with GtkAda.Types;              use GtkAda.Types;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties;           use GLib.Properties;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with GLib.Object.Checked_Destroy;

package body Gtk.Persistent_Storage_Credentials_Dialog is

   Credentials_Dialog_Class_Record : aliased Ada_GObject_Class :=
                                     Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Persistent_Storage_Credentials_Dialog." & Name;
   end Where;

   procedure Create
             (  Query           : in out Dialog_Credentials_Query;
                Scheme          : out Scheme_Type;
                Name            : out Unbounded_String;
                User            : out Unbounded_String;
                Password        : out Unbounded_String;
                Stored_Password : out Boolean;
                Storage         : out Storage_Handle
             )  is
      Dialog : Gtk_Persistent_Storage_Credentials_Dialog;
   begin
      Gtk_New (Dialog, "", True);
      Storage := Run (Dialog, Query);
      if Is_Valid (Storage) then
         Scheme := DSN_Scheme;
         Name   := To_Unbounded_String (Get_Name (Dialog));
         User   := To_Unbounded_String (Get_User (Dialog));
         Password := To_Unbounded_String (Get_Password (Dialog));
         Stored_Password := Get_Storing_Flag (Dialog);
      end if;
      GLib.Object.Checked_Destroy (Dialog);
   exception
      when others =>
         if Dialog /= null then
            GLib.Object.Checked_Destroy (Dialog);
         end if;
   end Create;

   procedure Error
             (  Dialog : not null access
                   Gtk_Persistent_Storage_Credentials_Dialog_Record;
                Text   : UTF8_String
             )  is
      Frame : Gtk_Frame;
   begin
      if Dialog.Error_Label = null then
         Gtk_New (Frame);
         Frame.Set_Border_Width (Style_Get (Dialog, "row-spacing"));
         Gtk_New (Dialog.Error_Label, Text);
         Frame.Add (Dialog.Error_Label);
         Dialog.Error_Label.Set_Halign (Align_Center);
         Dialog.Error_Label.Set_Valign (Align_Center);
         Dialog.Error_Label.Set_Justify (Justify_Left);
         Set_Property
         (  Dialog.Error_Label,
            Single_Line_Mode_Property,
            False
         );
         Dialog.Get_Content_Area.Pack_Start (Frame);
         Frame.Show_All;
      else
         Set_Text (Dialog.Error_Label, Text);
      end if;
   end Error;

   procedure Get
             (  Query           : in out Dialog_Credentials_Query;
                Scheme          : Scheme_Type;
                Name            : UTF8_String;
                User            : in out Unbounded_String;
                Password        : in out Unbounded_String;
                Stored_Password : in out Boolean;
                Storage         : out Storage_Handle
             )  is
      procedure Ask (Message : UTF8_String := "") is
         Dialog : Gtk_Persistent_Storage_Credentials_Dialog;
      begin
         Gtk_New
         (  Dialog,
            Name,
            To_String (User),
            Stored_Password
         );
         if Message'Length > 0 then
            Error (Dialog, Message);
         end if;
         Storage := Run (Dialog, Query);
         if Is_Valid (Storage) then
            User := To_Unbounded_String (Get_User (Dialog));
            Password := To_Unbounded_String (Get_Password (Dialog));
            Stored_Password := Get_Storing_Flag (Dialog);
         end if;
         GLib.Object.Checked_Destroy (Dialog);
      exception
         when others =>
            if Dialog /= null then
               GLib.Object.Checked_Destroy (Dialog);
            end if;
      end Ask;
   begin
      if Scheme /= DSN_Scheme then
         raise Status_Error with "Not a DSN scheme";
      end if;
      if Stored_Password then
         Storage :=
            Connect
            (  Query    => Dialog_Credentials_Query'Class (Query),
               Name     => Name,
               User     => To_String (User),
               Password => To_String (Password)
            );
      else
         Ask;
      end if;
   exception
      when Error : Ada.IO_Exceptions.Data_Error =>
         Ask (Exception_Message (Error));
      when Status_Error =>
         raise;
      when Use_Error =>
         Ask;
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Unexpected exeption "
            &  Exception_Information (Error)
            &  Where ("Get")
         )  );
         Ask (Exception_Information (Error));
   end Get;

   function Get_Name
            (  Dialog : not null access
                  Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return UTF8_String is
   begin
      return Dialog.Name_Entry.Get_Text;
   end Get_Name;

   function Get_User
            (  Dialog : not null access
                  Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return UTF8_String is
   begin
      return Dialog.User_Entry.Get_Text;
   end Get_User;

   function Get_Password
            (  Dialog : not null access
                  Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return UTF8_String is
   begin
      return Get_Text (Dialog.Password_Entry);
   end Get_Password;

   function Get_Storing_Flag
            (  Dialog : not null access
                  Gtk_Persistent_Storage_Credentials_Dialog_Record
            )  return Boolean is
   begin
      return Dialog.Store_Password.Get_Active;
   end Get_Storing_Flag;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Gtk.Dialog.Get_Type,
            Credentials_Dialog_Class_Record'Access,
            Persistent_Storage_Credentials_Dialog_Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "cancel-button-label",
               Nick    => "Cancel",
               Blurb   => "The label of the cancel button",
               Default => "_Cancel"
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_Int
            (  Name    => "column-spacing",
               Nick    => "Column spacing",
               Minimum => 0,
               Maximum => GInt'Last,
               Blurb   => "Column spacing of the dialog elements",
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "confirm-button-label",
               Nick    => "Confirm",
               Blurb   => "The label of the confirm button",
               Default => "_OK"
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "credentials-error",
               Nick    => "Credentials error",
               Blurb   => "The message on data source access error",
               Default =>
                  (  "The data source cannot be connected to. Check "
                  &  "the user name and/or password"
         )  )     );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "empty-name-error",
               Nick    => "Empty name",
               Blurb   => "The message when data source name is empty",
               Default => "Data source name may not be empty"
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "name-label",
               Nick    => "Name",
               Blurb   => "The label of the data source name entry",
               Default => "Data source name"
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "open-error",
               Nick    => "Open error",
               Blurb   => "The message on data source open error",
               Default =>
                  (  "Error connecting to thw data source. "
                  &  "Please, check the ODBC settings: "
         )  )     );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "password-label",
               Nick    => "Password",
               Blurb   => "The label of the password entry",
               Default => "Password"
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_Int
            (  Name    => "row-spacing",
               Nick    => "Row spacing",
               Minimum => 0,
               Maximum => GInt'Last,
               Blurb   => "Row spacing of the dialog elements",
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "stored-password-label",
               Nick    => "Remember password",
               Blurb   => "The label of the password storing check box",
               Default => "Remember password"
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "user-label",
               Nick    => "User",
               Blurb   => "The label of the user name entry",
               Default => "User"
         )  );
         Install_Style_Property
         (  Class_Ref (Credentials_Dialog_Class_Record.The_Type),
            Gnew_String
            (  Name    => "title",
               Nick    => "Title",
               Blurb   => "The dialog title",
               Default => "User credentials"
         )  );
      end if;
      return Credentials_Dialog_Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Dialog : out Gtk_Persistent_Storage_Credentials_Dialog;
                Name   : UTF8_String;
                User   : UTF8_String;
                Stored : Boolean
             )  is
   begin
      Dialog := new Gtk_Persistent_Storage_Credentials_Dialog_Record;
      begin
         Initialize (Dialog, Name, User, Stored);
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where
                  (  "Gtk_New "
                  &  "(Gtk_Persistent_Storage_Credentials_Dialog)"
            )  )  );
            GLib.Object.Checked_Destroy (Dialog);
            Dialog := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Dialog : out Gtk_Persistent_Storage_Credentials_Dialog;
                User   : UTF8_String;
                Stored : Boolean
             )  is
   begin
      Dialog := new Gtk_Persistent_Storage_Credentials_Dialog_Record;
      begin
         Initialize (Dialog, User, Stored);
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where
                  (  "Gtk_New "
                  &  "(Gtk_Persistent_Storage_Credentials_Dialog)"
            )  )  );
            GLib.Object.Checked_Destroy (Dialog);
            Dialog := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Dialog : not null access
                Gtk_Persistent_Storage_Credentials_Dialog_Record'Class;
                User   : UTF8_String;
                Stored : Boolean
             )  is
   begin
      Initialize (Dialog, "", User, Stored);
      Dialog.Name_Entry.Set_Sensitive (True);
   end Initialize;

   procedure Initialize
             (  Dialog : not null access
                Gtk_Persistent_Storage_Credentials_Dialog_Record'Class;
                Name   : UTF8_String;
                User   : UTF8_String;
                Stored : Boolean
             )  is
   begin
      G_New (Dialog, Get_Type);
      Gtk.Dialog.Initialize (Dialog);
      Dialog_Handlers.Connect
      (  Dialog,
         "style-updated",
         Dialog_Handlers.To_Marshaller (Style_Updated'Access)
      );

      Gtk_New (Dialog.Grid, 4, 2, False);

      Gtk_New (Dialog.Name_Label, "");
      Dialog.Grid.Attach (Dialog.Name_Label, 0, 1, 0, 1, Fill, 0);
      Dialog.Name_Label.Set_Justify (Justify_Right);
      Dialog.Name_Label.Set_Halign (Align_End);
      Dialog.Name_Label.Set_Valign (Align_Center);
      Gtk_New (Dialog.User_Label, "");
      Dialog.Grid.Attach (Dialog.User_Label, 0, 1, 1, 2, Fill, 0);
      Dialog.User_Label.Set_Justify (Justify_Right);
      Dialog.User_Label.Set_Halign (Align_End);
      Dialog.User_Label.Set_Valign (Align_Center);
      Gtk_New (Dialog.Password_Label, "");
      Dialog.Grid.Attach (Dialog.Password_Label, 0, 1, 2, 3, Fill, 0);
      Dialog.Password_Label.Set_Justify (Justify_Right);
      Dialog.Password_Label.Set_Halign (Align_End);
      Dialog.Password_Label.Set_Valign (Align_Center);

      Gtk_New (Dialog.Name_Entry);
      Set_Sensitive (Dialog.Name_Entry, False);
      Dialog.Grid.Attach
      (  Dialog.Name_Entry,
         1, 2, 0, 1,
         Yoptions => 0
      );
      Gtk_New (Dialog.User_Entry);
      Dialog.Grid.Attach
      (  Dialog.User_Entry,
         1, 2, 1, 2,
         Yoptions => 0
      );
      Gtk_New (Dialog.Password_Entry);
      Dialog.Grid.Attach
      (  Dialog.Password_Entry,
         1, 2, 2, 3,
         Yoptions => 0
      );
      Set_Property (Dialog.Password_Entry, Visibility_Property, False);

      Gtk_New (Dialog.Store_Password);
      Dialog.Grid.Attach
      (  Dialog.Store_Password,
         0, 2, 3, 4,
         Xoptions => Fill,
         YOptions => 0
      );

      Dialog.Get_Content_Area.Pack_Start (Dialog.Grid);
      Dialog.Grid.Show_All;

      Dialog.Name_Entry.Set_Text (Name);
      Dialog.User_Entry.Set_Text (User);
      Dialog.Store_Password.Set_Active (Stored);
      Dialog.Cancel_Button :=
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_Cancel,
            Icon     => Stock_Cancel,
            Label    => "_Cancel"
         );
      Dialog.OK_Button :=
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Response => Gtk_Response_OK,
            Icon     => Stock_OK,
            Label    => "_OK"
         );
      Dialog.OK_Button.Set_Can_Default (True);
      Style_Updated (Dialog);
   end Initialize;

   function Run
            (  Dialog : access
                  Gtk_Persistent_Storage_Credentials_Dialog_Record;
               Query  : Dialog_Credentials_Query'Class
            )  return Storage_Handle is
   begin
      loop
         case Run (Dialog) is
            when Gtk_Response_OK =>
               declare
                  Name : constant UTF8_String := Dialog.Get_Name;
               begin
                  if Name'Length = 0 then
                     -- Wrong name
                     Dialog.Error
                     (  Style_Get (Dialog, "empty-name-error")
                     );
                  else
                     return
                        Connect
                        (  Query    => Query,
                           Name     => Name,
                           User     => Get_User (Dialog),
                           Password => Get_Password (Dialog)
                        );
                  end if;
               exception
                  when Use_Error =>
                     Dialog.Error
                     (  Style_Get (Dialog, "credentials-error")
                     );
                  when Ada.IO_Exceptions.Data_Error =>
                     Dialog.Error
                     (  Style_Get (Dialog, "open-error")
                     );
                  when Reason : others =>
                     Dialog.Error (Exception_Message (Reason));
               end;
            when others =>
               declare
                  Aborted : Storage_Handle;
               begin
                  return Aborted;
               end;
         end case;
      end loop;
   exception
      when others =>
         GLib.Object.Checked_Destroy (Dialog);
         raise;
   end Run;

   procedure Style_Updated
             (  Dialog : access
                Gtk_Persistent_Storage_Credentials_Dialog_Record'Class
             )  is
   begin
      Dialog.Set_Title (Style_Get (Dialog, "title"));
      Dialog.Name_Label.Set_Text (Style_Get (Dialog, "name-label"));
      Dialog.User_Label.Set_Text (Style_Get (Dialog, "user-label"));
      Dialog.Password_Label.Set_Text
      (  Style_Get (Dialog, "password-label")
      );
      Dialog.Store_Password.Set_Label
      (  Style_Get (Dialog, "stored-password-label")
      );
      Dialog.OK_Button.Set_Label
      (  Style_Get (Dialog, "confirm-button-label")
      );
      Dialog.Cancel_Button.Set_Label
      (  Style_Get (Dialog, "cancel-button-label")
      );
      Dialog.Grid.Set_Col_Spacings
      (  Style_Get (Dialog, "column-spacing")
      );
      Dialog.Grid.Set_Row_Spacings
      (  Style_Get (Dialog, "row-spacing")
      );
   end Style_Updated;

end Gtk.Persistent_Storage_Credentials_Dialog;
