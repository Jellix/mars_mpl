--                                                                    --
--  package GLib.Spawn.Asynchronous Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2009       --
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
-- __________________________________________________________________ --

with Ada.Calendar;
with Ada.Exceptions;
with Ada.IO_Exceptions;

with Glib.Messages;

with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with Gtk.Missed;

with Interfaces.C.Strings;

with System.Address_To_Access_Conversions;

package body Glib.Spawn.Asynchronous is

   function Where (Text : String) return String is
   begin
      return " in GLib.Spawn.Asynchronous." & Text;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation (Reader, Reader_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Writer, Writer_Ptr);

   procedure Free (List : in out Chars_Ptr_Array_Ptr);

   procedure On_Exit
     (PID    : GPID;
      Status : Gint;
      Data   : System.Address);
   pragma Convention (C, On_Exit);

   package Conversions is
     new System.Address_To_Access_Conversions
       (Asynchronous_Process'Class);

   procedure Completed
     (Process : in out Asynchronous_Process;
      Status  : Gint)
   is
      pragma Unreferenced (Status);
   begin
      Free (Process.ArgV);
      Free (Process.EnvP);
   end Completed;

   function Copy (List : Chars_Ptr_Array) return Chars_Ptr_Array_Ptr is
      Result : constant Chars_Ptr_Array_Ptr :=
                 new Chars_Ptr_Array (List'First .. List'Last + 1);
   begin
      for Index in Result'Range loop
         Result.all (Index) :=
           Interfaces.C.Strings.New_Char_Array (Interfaces.C.Strings.Value (List (Index)));
      end loop;
      Result.all (Result'Last) := Interfaces.C.Strings.Null_Ptr;
      return Result;
   end Copy;

   function Copy
     (Name : UTF8_String;
      List : Chars_Ptr_Array) return Chars_Ptr_Array_Ptr
   is
      Result : constant Chars_Ptr_Array_Ptr :=
                 new Chars_Ptr_Array (List'First .. List'Last + 2);
   begin
      Result.all (List'First) := Interfaces.C.Strings.New_String (Name);
      for Index in List'Range loop
         Result.all (Index + 1) :=
           Interfaces.C.Strings.New_Char_Array
             (Interfaces.C.Strings.Value (List (Index)));
      end loop;
      Result.all (Result'Last) := Interfaces.C.Strings.Null_Ptr;
      return Result;
   end Copy;

   function Copy (List : Gtk.Enums.String_List.Glist)
                  return Chars_Ptr_Array_Ptr
   is
      Ptr   : Gtk.Enums.String_List.Glist := List;
      Count : Natural                     := 0;

      use type Gtk.Enums.String_List.Glist;
   begin
      while Ptr /= Gtk.Enums.String_List.Null_List loop
         Count := Count + 1;
         Ptr   := Gtk.Enums.String_List.Next (Ptr);
      end loop;
      declare
         Result : constant Chars_Ptr_Array_Ptr :=
                    new Chars_Ptr_Array (1 .. Count + 1);
      begin
         Ptr := List;
         for Index in 1 .. Count loop
            Result.all (Index) :=
              Interfaces.C.Strings.New_String
                (Gtk.Enums.String_List.Get_Data (Ptr));
            Ptr := Gtk.Enums.String_List.Next (Ptr);
         end loop;
         Result.all (Result'Last) := Interfaces.C.Strings.Null_Ptr;
         return Result;
      end;
   end Copy;

   function Copy
     (Name : UTF8_String;
      List : Gtk.Enums.String_List.Glist) return Chars_Ptr_Array_Ptr
   is
      Ptr   : Gtk.Enums.String_List.Glist := List;
      Count : Natural                     := 0;

      use type Gtk.Enums.String_List.Glist;
   begin
      while Ptr /= Gtk.Enums.String_List.Null_List loop
         Count := Count + 1;
         Ptr   := Gtk.Enums.String_List.Next (Ptr);
      end loop;
      declare
         Result : constant Chars_Ptr_Array_Ptr :=
                    new Chars_Ptr_Array (1 .. Count + 2);
      begin
         Result.all (1) := Interfaces.C.Strings.New_String (Name);
         Ptr := List;
         for Index in 2 .. Count + 1  loop
            Result.all (Index) :=
              Interfaces.C.Strings.New_String
                (Gtk.Enums.String_List.Get_Data (Ptr));
            Ptr := Gtk.Enums.String_List.Next (Ptr);
         end loop;
         Result.all (Result'Last) := Interfaces.C.Strings.Null_Ptr;
         return Result;
      end;
   end Copy;

   procedure Error
     (Process : in out Asynchronous_Process;
      Data    : UTF8_String) is
   begin
      null;
   end Error;

   procedure Failed
     (Process : in out Asynchronous_Process;
      Error   : Glib.Error.GError) is
   begin
      null;
   end Failed;

   overriding procedure Finalize (Process : in out Asynchronous_Process)
   is
      use type Glib.Error.GError;
   begin
      select
         Process.Status.Wait_All;
      or delay 1.0;
      end select;
      if Process.Error /= null then
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
   end Finalize;

   procedure Free (List : in out Chars_Ptr_Array_Ptr)
   is
      procedure Release is
        new Ada.Unchecked_Deallocation
          (Chars_Ptr_Array,
           Chars_Ptr_Array_Ptr);
   begin
      if List /= null then
         for Index in List'Range loop
            Interfaces.C.Strings.Free (List.all (Index));
         end loop;
         Release (List);
      end if;
   end Free;

   function Get_Error
     (Process : Asynchronous_Process) return Glib.Error.GError is
   begin
      return Process.Error;
   end Get_Error;

   function Get_State (Process : Asynchronous_Process)
      return Process_State is
   begin
      return Process.Status.Get;
   end Get_State;

   function Get_Exit_Status (Process : Asynchronous_Process)
      return Gint is
   begin
      return Process.Status.Get;
   end Get_Exit_Status;

   procedure Input
     (Process : in out Asynchronous_Process;
      Data    : out UTF8_String;
      Count   : out Natural)
   is
      pragma Unreferenced (Count, Data, Process);
   begin
      Count := 0;
   end Input;

   procedure On_Exit
     (PID    : GPID;
      Status : Gint;
      Data   : System.Address)
   is
      pragma Unreferenced (PID);
   begin
      Conversions.To_Pointer (Data).all.Status.Set (Process_Completed, Status);
   end On_Exit;

   procedure Output
     (Process : in out Asynchronous_Process;
      Data    : UTF8_String) is
   begin
      null;
   end Output;

   protected body Process_Status is
      function Completed_Tasks_Count return Natural is
      begin
         return Count;
      end Completed_Tasks_Count;

      function Get return Process_State is
      begin
         return State;
      end Get;

      function Get return Gint is
      begin
         return Status;
      end Get;

      procedure Set (State : Process_State) is
      begin
         if
           Process_Status.State = Process_Running and then
           State = Process_Completed and then
           Count < 3
         then
            Count := Count + 1;
         else
            Process_Status.State := State;
            Count := 0;
         end if;
      end Set;

      procedure Set (State : Process_State; Status : Gint) is
      begin
         Process_Status.Status := Status;
         Set (State);
      end Set;

      entry Wait_IO when State = Process_Running and then Count = 3 is
      begin
         null;
      end Wait_IO;

      entry Wait_All when State /= Process_Running is
      begin
         null;
      end Wait_All;

   end Process_Status;

   task body Reader is
      Buffer : UTF8_String (1 .. 1024);
      Byte   : aliased Character;
      Count  : Natural := 0;
   begin
      begin
         while
           0 < GNAT.OS_Lib.Read (GNAT.OS_Lib.File_Descriptor (Pipe),
                                 Byte'Address,
                                 1)
         loop
            Count := Count + 1;
            Buffer (Count) := Byte;
            if
              Count = Buffer'Length or else
              Buffer (Count) = Character'Val (10)
            then
               if Pipe = Process.all.Standard_Output then
                  Output (Process.all, Buffer (1 .. Count));
               else
                  Error (Process.all, Buffer (1 .. Count));
               end if;
               Count := 0;
            end if;
         end loop;
         if Count > 0 then
            if Pipe = Process.all.Standard_Output then
               Output (Process.all, Buffer (1 .. Count));
            else
               Error (Process.all, Buffer (1 .. Count));
            end if;
         end if;
      exception
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               Ada.Exceptions.Exception_Message (Error)
               &  Where ("task Reader"));
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
      end;
      Process.all.Status.Set (Process_Completed);
   end Reader;

   procedure Run
     (Process           : in out Asynchronous_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Array;
      EnvP              : Chars_Ptr_Array)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Process.EnvP := Copy (EnvP);
      Start
        (Process,
         Async_With_Pipes
         (Working_Directory => Working_Directory,
          ArgV              => Process.ArgV.all (1)'Access,
          EnvP              => Process.EnvP.all (1)'Access,
          Flags             => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Run
     (Process           : in out Asynchronous_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Gtk.Enums.String_List.Glist;
      EnvP              : Gtk.Enums.String_List.Glist)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Process.EnvP := Copy (EnvP);
      Start
        (Process,
         Async_With_Pipes
           (Working_Directory => Working_Directory,
            ArgV              => Process.ArgV.all (1)'Access,
            EnvP              => Process.EnvP.all (1)'Access,
            Flags             => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Run
     (Process           : in out Asynchronous_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Array)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Start
        (Process,
         Async_With_Pipes
           (Working_Directory => Working_Directory,
            ArgV              => Process.ArgV.all (1)'Access,
            Flags             => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Run
     (Process           : in out Asynchronous_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Gtk.Enums.String_List.Glist)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Start
        (Process,
         Async_With_Pipes
           (Working_Directory => Working_Directory,
            ArgV              => Process.ArgV.all (1)'Access,
            Flags             => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Run
     (Process : in out Asynchronous_Process;
      Name    : UTF8_String;
      ArgV    : Chars_Ptr_Array;
      EnvP    : Chars_Ptr_Array)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Process.EnvP := Copy (EnvP);
      Start
        (Process,
         Async_With_Pipes
           (ArgV        => Process.ArgV.all (1)'Access,
            EnvP        => Process.EnvP.all (1)'Access,
            Flags       => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Run
     (Process : in out Asynchronous_Process;
      Name    : UTF8_String;
      ArgV    : Gtk.Enums.String_List.Glist;
      EnvP    : Gtk.Enums.String_List.Glist)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Process.EnvP := Copy (EnvP);
      Start
        (Process,
         Async_With_Pipes
           (ArgV  => Process.ArgV.all (1)'Access,
            EnvP  => Process.EnvP.all (1)'Access,
            Flags => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Run
     (Process : in out Asynchronous_Process;
      Name    : UTF8_String;
      ArgV    : Chars_Ptr_Array)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Start
        (Process,
         Async_With_Pipes
           (ArgV  => Process.ArgV.all (1)'Access,
            Flags => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Run
     (Process : in out Asynchronous_Process;
      Name    : UTF8_String;
      ArgV    : Gtk.Enums.String_List.Glist)
   is
      use type Glib.Error.GError;
   begin
      if Process.Status.Get = Process_Running then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
      if Process.Error /= null then
         Process.Status.Set (Process_Completed);
         Glib.Error.Error_Free (Process.Error);
         Process.Error := null;
      end if;
      Process.ArgV := Copy (Name, ArgV);
      Start
        (Process,
         Async_With_Pipes
           (ArgV  => Process.ArgV.all (1)'Access,
            Flags => Spawn_Do_Not_Reap_Child or Spawn_Search_Path));
   end Run;

   procedure Service_Exit (Process : in out Asynchronous_Process_Ptr) is
   begin
      Free (Process.all.Input_Writer);
      Free (Process.all.Output_Reader);
      Free (Process.all.Error_Reader);
      GNAT.OS_Lib.Close
        (GNAT.OS_Lib.File_Descriptor (Process.all.Standard_Error));
      GNAT.OS_Lib.Close
        (GNAT.OS_Lib.File_Descriptor (Process.all.Standard_Input));
      GNAT.OS_Lib.Close
        (GNAT.OS_Lib.File_Descriptor (Process.all.Standard_Output));
      Free (Process.all.ArgV);
      Free (Process.all.EnvP);
      Close_PID (Process.all.PID);
      begin
         Completed (Process.all, Process.all.Status.Get);
      exception
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               Ada.Exceptions.Exception_Message (Error)
               &  Where ("Service_Exit"));
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
      end;
      Process.all.Status.Set (Process_Completed);
   end Service_Exit;

   procedure Start
     (Process : in out Asynchronous_Process'Class;
      Result  : Async_Result) is
   begin
      if Result.Running then
         Process.Status.Set (Process_Running);
         Process.PID             := Result.PID;
         Process.Standard_Input  := Result.Standard_Input;
         Process.Standard_Output := Result.Standard_Output;
         Process.Standard_Error  := Result.Standard_Error;
         Process.Input_Writer :=
           new Writer
             (Process'Unchecked_Access,
              Process.Standard_Input);
         Process.Output_Reader :=
           new Reader
             (Process'Unchecked_Access,
              Process.Standard_Output);
         Process.Error_Reader :=
           new Reader
             (Process'Unchecked_Access,
              Process.Standard_Error);
         Child_Watch_Add
           (Process.PID,
            On_Exit'Access,
            Process'Address);
      else
         Process.Error := Result.Error;
         Process.Status.Set (Process_Failed_To_Start);
         Free (Process.ArgV);
         Free (Process.EnvP);
         Failed (Process, Process.Error);
      end if;
   end Start;

   procedure Wait
     (Process  : in out Asynchronous_Process;
      Time_Out : Duration := Duration'Last) is
   begin
      if Time_Out = Duration'Last then
         Process.Status.Wait_All;
      else
         select
            Process.Status.Wait_All;
         or delay Time_Out;
            raise Ada.Calendar.Time_Error;
         end select;
      end if;
   end Wait;

   task body Writer is
      Bytes  : Integer;
      Buffer : UTF8_String (1 .. 1024 * 10);
      Count  : Natural;
   begin
      begin
         loop
            Input (Process.all, Buffer, Count);
            exit when Count = 0;
            Bytes :=
               GNAT.OS_Lib.Write
                (GNAT.OS_Lib.File_Descriptor (Pipe),
                 Buffer'Address,
                 Integer (Count));
            exit when Bytes /= Integer (Count);
         end loop;
      exception
         when Error : others =>
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               Ada.Exceptions.Exception_Message (Error)
               &  Where ("task Writer"));
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
      end;
      Process.all.Status.Wait_IO;
      Messages.Send (Service_Exit'Access, Process.all'Unchecked_Access);
   end Writer;

end Glib.Spawn.Asynchronous;
