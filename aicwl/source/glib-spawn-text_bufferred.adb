--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Spawn.Text_Bufferred                   Luebeck            --
--  Implementation                                 Spring, 2009       --
--                                                                    --
--                                Last revision :  15:24 01 Apr 2015  --
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
-- __________________________________________________________________ --

with Gtk.Missed;
with Gtk.Text_Iter;

with System.Address_To_Access_Conversions;

package body Glib.Spawn.Text_Bufferred is

   overriding procedure Error
     (Process : in out Text_Bufferred_Process;
      Data    : UTF8_String)
   is
      use type Gtk.Text_Buffer.Gtk_Text_Buffer;
   begin
      if Process.Error /= null or else Data'Length > 0 then
         declare
            Write :
            Request_Write
              (Process'Access,
               True,
               Data'Length);
         begin
            Write.Text := Data'Address;
            Gtk.Main.Router.Request (Write);
         end;
      end if;
   end Error;

   overriding procedure Finalize (Process : in out Text_Bufferred_Process)
   is
      use type Gtk.Text_Buffer.Gtk_Text_Buffer;
   begin
      if Process.Input /= null then
         Gtk.Text_Buffer.Unref (Process.Input);
         Process.Input := null;
      end if;
      if Process.Output /= null then
         Gtk.Text_Buffer.Unref (Process.Output);
         Process.Output := null;
      end if;
      if Process.Error /= null then
         Gtk.Text_Buffer.Unref (Process.Error);
         Process.Error := null;
      end if;
      Glib.Spawn.Asynchronous.Finalize
        (Glib.Spawn.Asynchronous.Asynchronous_Process (Process));
   end Finalize;

   procedure Initiate
     (Process : in out Text_Bufferred_Process;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer)
   is
      use type Gtk.Text_Buffer.Gtk_Text_Buffer;
   begin
      if Process.Input /= null then
         Gtk.Text_Buffer.Unref (Process.Input);
      end if;
      Process.Input := Input;
      if Process.Input /= null then
         Gtk.Text_Buffer.Ref (Process.Input);
         Process.Position := 0;
      end if;
      if Process.Output /= null then
         Gtk.Text_Buffer.Unref (Process.Output);
      end if;
      Process.Output := Output;
      if Process.Output /= null then
         Gtk.Text_Buffer.Ref (Process.Output);
      end if;
      if Process.Error /= null then
         Gtk.Text_Buffer.Unref (Process.Error);
      end if;
      Process.Error := Error;
      if Process.Error /= null then
         Gtk.Text_Buffer.Ref (Process.Error);
      end if;
   end Initiate;

   overriding procedure Input
     (Process : in out Text_Bufferred_Process;
      Data    : out UTF8_String;
      Count   : out Natural)
   is
      use type Gtk.Text_Buffer.Gtk_Text_Buffer;
   begin
      if Process.Input = null or else Data'Length = 0 then
         Count := 0;
      else
         declare
            Read : Request_Read (Process'Access, Data'Length);
         begin
            Read.Text := Data'Address;
            Gtk.Main.Router.Request (Read);
            Count := Read.Count;
         end;
      end if;
   end Input;

   procedure Insert
     (Process : in out Text_Bufferred_Process;
      Buffer  : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Error   : Boolean;
      Text    : String)
   is
      pragma Unreferenced (Error, Process);

      End_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Text_Buffer.Get_End_Iter (Buffer, End_Iter);
      Gtk.Missed.Insert_Alt (Buffer, End_Iter, Text);
      pragma Unreferenced (End_Iter);
   end Insert;

   overriding procedure Output
     (Process : in out Text_Bufferred_Process;
      Data    : UTF8_String)
   is
      use type Gtk.Text_Buffer.Gtk_Text_Buffer;
   begin
      if Process.Output /= null or else Data'Length > 0 then
         declare
            Write :
            Request_Write
              (Process'Access,
               False,
               Data'Length);
         begin
            Write.Text := Data'Address;
            Gtk.Main.Router.Request (Write);
         end;
      end if;
   end Output;

   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Array;
      EnvP              : Chars_Ptr_Array;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process           =>
           Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV,
         EnvP              => EnvP);
   end Run;

   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Gtk.Enums.String_List.Glist;
      EnvP              : Gtk.Enums.String_List.Glist;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process           =>
           Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV,
         EnvP              => EnvP);
   end Run;

   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Array;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process           =>
           Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV);
   end Run;

   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Gtk.Enums.String_List.Glist;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process           =>
           Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV);
   end Run;

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Chars_Ptr_Array;
      EnvP    : Chars_Ptr_Array;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process => Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV,
         EnvP    => EnvP);
   end Run;

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Gtk.Enums.String_List.Glist;
      EnvP    : Gtk.Enums.String_List.Glist;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process => Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV,
         EnvP    => EnvP);
   end Run;

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Chars_Ptr_Array;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process => Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV);
   end Run;

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Gtk.Enums.String_List.Glist;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null) is
   begin
      Initiate (Process, Input, Output, Error);
      Glib.Spawn.Asynchronous.Run
        (Process => Glib.Spawn.Asynchronous.Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV);
   end Run;

   overriding procedure Service (Data : in out Request_Read) is
      subtype Text is UTF8_String (1 .. Data.Length);
      package Conversion is
         new System.Address_To_Access_Conversions (Text);
      Buffer : Text renames Conversion.To_Pointer (Data.Text).all;
      From   : Gtk.Text_Iter.Gtk_Text_Iter;
      To     : Gtk.Text_Iter.Gtk_Text_Iter;
      Got_It : Boolean;
   begin
      Data.Count := 0;
      Gtk.Text_Buffer.Get_Iter_At_Offset
        (Data.Process.all.Input,
         From,
         Data.Process.all.Position);
      if Gtk.Text_Iter.Is_End (From) then
         return;
      end if;
      Read_Loop : loop
         Gtk.Text_Iter.Copy (From, To);
         Gtk.Text_Iter.Forward_Char (To, Got_It);
         exit Read_Loop when not Got_It;
         declare
            Slice : constant String := Gtk.Text_Iter.Get_Text (From, To);
         begin
            exit Read_Loop when Slice'Length > Data.Length - Data.Count;
            Buffer (Data.Count .. Data.Count + Slice'Length) := Slice;
            Data.Count := Data.Count + Slice'Length;
            Gtk.Text_Iter.Copy (To, From);
         end;
      end loop Read_Loop;
      Data.Process.all.Position := Gtk.Text_Iter.Get_Offset (From);
   end Service;

   overriding procedure Service (Data : in out Request_Write) is
      subtype Text is UTF8_String (1 .. Data.Length);
      package Conversion is
         new System.Address_To_Access_Conversions (Text);
   begin
      if Data.Error then
         Insert
           (Data.Process.all,
            Data.Process.all.Error,
            True,
            Conversion.To_Pointer (Data.Text).all);
      else
         Insert
           (Data.Process.all,
            Data.Process.all.Output,
            False,
            Conversion.To_Pointer (Data.Text).all);
      end if;
   end Service;

end Glib.Spawn.Text_Bufferred;
