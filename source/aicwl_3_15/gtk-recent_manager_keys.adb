--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Recent_Manager_Keys                     Luebeck            --
--  Implementation                                 Autumn, 2011       --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with GLib.Messages;      use GLib.Messages;
with GLib.Values;        use GLib.Values;
with Gtk.Missed;         use Gtk.Missed;
with Gtk.Tree_Model;     use Gtk.Tree_Model;

package body Gtk.Recent_Manager_Keys is

   function Where (Text : String) return String is
   begin
      return " in Gtk.Recent_Manager_Keys." & Text;
   end Where;

   procedure Enumerate
             (  Enumerator : in out Key_Enumerator'Class;
                Prefix     : UTF8_String;
                Manager    : Gtk_Recent_Manager := Get_Default
             )  is
      List : Gtk_Recent_Info_Array renames Get_Items (Manager);
      Name : constant String := Get_Application_Name;
   begin
      for Index in List'Range loop
         if Has_Application (List (Index), Name) then
            declare
               Key : constant UTF8_String :=
                     Get_Description (List (Index));
            begin
               if (  Key'Length >= Prefix'Length
                  and then
                     (  Key (Key'First..Key'First + Prefix'Length - 1)
                     =  Prefix
                  )  )
               then
                  Process
                  (  Enumerator,
                     Key,
                     Get_URI (List (Index)),
                     List (Index)
                  );
               end if;
            end;
         end if;
      end loop;
      for Index in List'Range loop
         Unref (List (Index));
      end loop;
   exception
      when End_Error =>
         for Index in List'Range loop
            Unref (List (Index));
         end loop;
      when Error : others =>
         for Index in List'Range loop
            Unref (List (Index));
         end loop;
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Enumerate")
         )  );
   end Enumerate;

   procedure Process
             (  Enumerator : in out Model_Enumerator;
                Key        : UTF8_String;
                Value      : UTF8_String;
                Info       : Gtk_Recent_Info
             )  is
      Pointer : constant Integer := Key'First + Enumerator.Length;
      Row     : GInt;
      This    : Gtk_Tree_Iter;
   begin
      if Pointer >= Key'Last or else Key (Pointer) /= '_' then
         return;
      end if;
      begin
         Row := GInt'Value (Key (Pointer + 1..Key'Last));
      exception
         when others =>
            return;
      end;
      if Row not in 1..Enumerator.Max_Row then
         return;
      end if;
      if N_Children (Enumerator.Model) < Row then
         for Position in N_Children (Enumerator.Model)..Row - 1 loop
            Insert (Enumerator.Model, This, Position);
         end loop;
      else
         This := Nth_Child (Enumerator.Model, Null_Iter, Row - 1);
      end if;
      Gtk.Missed.Set (Enumerator.Model, This, Enumerator.Column, Value);
   end Process;

   function Restore
            (  Key     : UTF8_String;
               Default : UTF8_String;
               Manager : Gtk_Recent_Manager := Get_Default
            )  return UTF8_String is
      List : Gtk_Recent_Info_Array renames Get_Items (Manager);
      Name : constant String := Get_Application_Name;
   begin
      for Index in List'Range loop
         if (  Has_Application (List (Index), Name)
            and then
               Get_URI (List (Index)) = Key
            )
         then
            return Result : constant UTF8_String :=
                            Get_Display_Name (List (Index)) do
               for Rest in Index..List'Last loop
                  Unref (List (Rest));
               end loop;
            end return;
         end if;
         Unref (List (Index));
      end loop;
      return Default;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Restore")
         )  );
         return "";
   end Restore;

   procedure Restore
             (  Key     : UTF8_String;
                Model   : Gtk_List_Store;
                Column  : GInt;
                Max_Row : Positive := 10;
                Manager : Gtk_Recent_Manager := Get_Default
             )  is
      Enumerator : Model_Enumerator;
   begin
      Enumerator.Model   := Model;
      Enumerator.Column  := Column;
      Enumerator.Length  := Key'Length;
      Enumerator.Max_Row := GInt (Max_Row);
      Enumerate (Enumerator, Key, Manager);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Restore (list store)")
         )  );
   end Restore;

   procedure Store
             (  Key     : UTF8_String;
                Value   : UTF8_String;
                Manager : Gtk_Recent_Manager := Get_Default
             )  is
   begin
      if Add_Full
         (  Manager      => Manager,
            URI          => Key,
            Display_Name => Value,
            Description  => "stored value for " & Key
         )
      then
         null;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Store")
         )  );
   end Store;

   procedure Store
             (  Key     : UTF8_String;
                Model   : Gtk_List_Store;
                Column  : GInt;
                Max_Row : Positive := 10;
                Manager : Gtk_Recent_Manager := Get_Default
             )  is
      This : Gtk_Tree_Iter := Get_Iter_First (Model);
   begin
      for Row in 1..Max_Row loop
         exit when This = Null_Iter;
         declare
            Text : String := Key & Positive'Image (Row);
            Data : GValue;
         begin
            Text (Text'First + Key'Length) := '_';
            Get_Value (Model, This, Column, Data);
            Store (Text, Get_String (Data), Manager);
            Unset (Data);
            Next (Model, This);
         end;
      end loop;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Store (list store)")
         )  );
   end Store;

end Gtk.Recent_Manager_Keys;
