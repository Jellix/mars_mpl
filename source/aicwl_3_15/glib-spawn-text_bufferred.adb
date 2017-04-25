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
--____________________________________________________________________--

with Gtk.Missed;  use Gtk.Missed;

with System.Address_To_Access_Conversions;

package body GLib.Spawn.Text_Bufferred is

   procedure Error
             (  Process : in out Text_Bufferred_Process;
                Data    : UTF8_String
             )  is
   begin
      if Process.Error /= null or else Data'Length > 0 then
         declare
            Write : Request_Write
                    (  Process'Access,
                       True,
                       Data'Length
                    );
         begin
            Write.Text := Data'Address;
            Request (Write);
         end;
      end if;
   end Error;

   procedure Finalize (Process : in out Text_Bufferred_Process) is
   begin
      if Process.Input /= null then
         Unref (Process.Input);
         Process.Input := null;
      end if;
      if Process.Output /= null then
         Unref (Process.Output);
         Process.Output := null;
      end if;
      if Process.Error /= null then
         Unref (Process.Error);
         Process.Error := null;
      end if;
      Finalize (Asynchronous_Process (Process));
   end Finalize;

   procedure Initiate
             (  Process : in out Text_Bufferred_Process;
                Input   : Gtk_Text_Buffer;
                Output  : Gtk_Text_Buffer;
                Error   : Gtk_Text_Buffer
             )  is
   begin
      if Process.Input /= null then
         Unref (Process.Input);
      end if;
      Process.Input := Input;
      if Process.Input /= null then
         Ref (Process.Input);
         Process.Position := 0;
      end if;
      if Process.Output /= null then
         Unref (Process.Output);
      end if;
      Process.Output := Output;
      if Process.Output /= null then
         Ref (Process.Output);
      end if;
      if Process.Error /= null then
         Unref (Process.Error);
      end if;
      Process.Error := Error;
      if Process.Error /= null then
         Ref (Process.Error);
      end if;
   end Initiate;

   procedure Input
             (  Process : in out Text_Bufferred_Process;
                Data    : out UTF8_String;
                Count   : out Natural
             )  is
   begin
      if Process.Input = null or else Data'Length = 0 then
         Count := 0;
      else
         declare
            Read : Request_Read (Process'Access, Data'Length);
         begin
            Read.Text := Data'Address;
            Request (Read);
            Count := Read.Count;
         end;
      end if;
   end Input;

   procedure Insert
             (  Process : in out Text_Bufferred_Process;
                Buffer  : access Gtk_Text_Buffer_Record'Class;
                Error   : Boolean;
                Text    : String
             )  is
      End_Iter : Gtk_Text_Iter;
   begin
      Get_End_Iter (Buffer, End_Iter);
      Insert_Alt (Buffer, End_Iter, Text);
   end Insert;

   procedure Output
             (  Process : in out Text_Bufferred_Process;
                Data    : UTF8_String
             )  is
   begin
      if Process.Output /= null or else Data'Length > 0 then
         declare
            Write : Request_Write
                    (  Process'Access,
                       False,
                       Data'Length
                    );
         begin
            Write.Text := Data'Address;
            Request (Write);
         end;
      end if;
   end Output;

   procedure Run
             (  Process           : in out Text_Bufferred_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : Chars_Ptr_Array;
                EnvP              : Chars_Ptr_Array;
                Input             : Gtk_Text_Buffer := null;
                Output            : Gtk_Text_Buffer := null;
                Error             : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process           => Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV,
         EnvP              => EnvP
      );
   end Run;

   procedure Run
             (  Process           : in out Text_Bufferred_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : GList;
                EnvP              : GList;
                Input             : Gtk_Text_Buffer := null;
                Output            : Gtk_Text_Buffer := null;
                Error             : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process           => Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV,
         EnvP              => EnvP
      );
   end Run;

   procedure Run
             (  Process           : in out Text_Bufferred_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : Chars_Ptr_Array;
                Input             : Gtk_Text_Buffer := null;
                Output            : Gtk_Text_Buffer := null;
                Error             : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process           => Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV
      );
   end Run;

   procedure Run
             (  Process           : in out Text_Bufferred_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : GList;
                Input             : Gtk_Text_Buffer := null;
                Output            : Gtk_Text_Buffer := null;
                Error             : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process           => Asynchronous_Process (Process),
         Name              => Name,
         Working_Directory => Working_Directory,
         ArgV              => ArgV
      );
   end Run;

   procedure Run
             (  Process : in out Text_Bufferred_Process;
                Name    : UTF8_String;
                ArgV    : Chars_Ptr_Array;
                EnvP    : Chars_Ptr_Array;
                Input   : Gtk_Text_Buffer := null;
                Output  : Gtk_Text_Buffer := null;
                Error   : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process => Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV,
         EnvP    => EnvP
      );
   end Run;

   procedure Run
             (  Process : in out Text_Bufferred_Process;
                Name    : UTF8_String;
                ArgV    : GList;
                EnvP    : GList;
                Input   : Gtk_Text_Buffer := null;
                Output  : Gtk_Text_Buffer := null;
                Error   : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process => Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV,
         EnvP    => EnvP
      );
   end Run;

   procedure Run
             (  Process : in out Text_Bufferred_Process;
                Name    : UTF8_String;
                ArgV    : Chars_Ptr_Array;
                Input   : Gtk_Text_Buffer := null;
                Output  : Gtk_Text_Buffer := null;
                Error   : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process => Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV
      );
   end Run;

   procedure Run
             (  Process : in out Text_Bufferred_Process;
                Name    : UTF8_String;
                ArgV    : GList;
                Input   : Gtk_Text_Buffer := null;
                Output  : Gtk_Text_Buffer := null;
                Error   : Gtk_Text_Buffer := null
             )  is
   begin
      Initiate (Process, Input, Output, Error);
      Run
      (  Process => Asynchronous_Process (Process),
         Name    => Name,
         ArgV    => ArgV
      );
   end Run;

   procedure Service (Data : in out Request_Read) is
      subtype Text is UTF8_String (1..Data.Length);
      package Conversion is
         new System.Address_To_Access_Conversions (Text);
      Buffer : Text renames Conversion.To_Pointer (Data.Text).all;
      From   : Gtk_Text_Iter;
      To     : Gtk_Text_Iter;
      Got_It : Boolean;
   begin
      Data.Count := 0;
      Get_Iter_At_Offset
      (  Data.Process.Input,
         From,
         Data.Process.Position
      );
      if Is_End (From) then
         return;
      end if;
      loop
         Copy (From, To);
         Forward_Char (To, Got_It);
         exit when not Got_It;
         declare
            Slice : constant String := Get_Text (From, To);
         begin
            exit when Slice'Length > Data.Length - Data.Count;
            Buffer (Data.Count..Data.Count + Slice'Length) := Slice;
            Data.Count := Data.Count + Slice'Length;
            Copy (To, From);
         end;
      end loop;
      Data.Process.Position := Get_Offset (From);
   end Service;

   procedure Service (Data : in out Request_Write) is
      subtype Text is UTF8_String (1..Data.Length);
      package Conversion is
         new System.Address_To_Access_Conversions (Text);
   begin
      if Data.Error then
         Insert
         (  Data.Process.all,
            Data.Process.Error,
            True,
            Conversion.To_Pointer (Data.Text).all
         );
      else
         Insert
         (  Data.Process.all,
            Data.Process.Output,
            False,
            Conversion.To_Pointer (Data.Text).all
         );
      end if;
   end Service;

end GLib.Spawn.Text_Bufferred;
