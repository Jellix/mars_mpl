--                                                                    --
--  package GLib.Spawn.Asynchronous Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2009       --
--                                                                    --
--                                Last revision :  09:38 09 Apr 2010  --
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
--  This package is provided as a higher-level binding to GLib  function
--  g_spawn_async_with_pipes.  Since  a  correct  use of the function is
--  very  difficult,  especially with regard to deadlocking. The typical
--  use of g_spawn_async_with_pipes in GLib applications is built around
--  single-threaded model of GLib. Ada provides multiple  tasks and  the
--  provided binding takes an  advantage  of  this  allowing  concurrent
--  access to the pipes of the asynchronous process. The notification of
--  the process completion is processed in a way that prevents premature
--  closing of the output and error  pipes,  which  might  contain  data
--  after the process exit. The notification of exit is  postponed until
--  all  pipes  are  closed in proper state. The notification is done at
--  the context of the main GTK+ task.
--
with Gtk.Enums;        use Gtk.Enums.String_List;
with Gtk.Main.Router;  use Gtk.Main.Router;
with Interfaces.C;     use Interfaces.C;

with Ada.Finalization;

package GLib.Spawn.Asynchronous is
--
-- Process_State
--
   type Process_State is
        (  Process_Running,
           Process_Completed,
           Process_Failed_To_Start
        );
--
-- Asynchronous_Process -- Encapsulates a process
--
   type Asynchronous_Process is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- To be called when overridden
--
   procedure Finalize (Process : in out Asynchronous_Process);
--
-- Completed -- Called upon process completion
--
--    Process - The process
--    Status  - Exit status of the process
--
-- This procedure is called on the context of the main  GTK+  task  upon
-- process  exit,  when  all  pipes  are  already  closed.  The  default
-- implementation does nothing.
--
   procedure Completed
             (  Process : in out Asynchronous_Process;
                Status  : GInt
             );
--
-- Error -- Accept a new portion of error stream
--
--    Process - The process
--    Data    - Error data
--
-- The  default implementation does nothing. Note that this procedure is
-- called from an independent task. Use  Gtk.Main.Router  facilities  if
-- you want to call GTK+ operations from Error.
--
   procedure Error
             (  Process : in out Asynchronous_Process;
                Data    : UTF8_String
             );
--
-- Failed -- Called when process spawn was failed
--
--    Process - The process
--    Error   - Of process spawning
--
-- The default implementation does nothing. Failed is called  from  Run,
-- i.e. on the context of the main GTK+ task.
--
   procedure Failed
             (  Process : in out Asynchronous_Process;
                Error   : GError
             );
--
-- Get_Error -- Error of process spawning
--
--    Process - The process
--
-- Returns :
--
--    The error or null
--
   function Get_Error (Process : Asynchronous_Process) return GError;
--
-- Get_Exit_Status -- Error of process spawning
--
--    Process - The process
--
-- Returns :
--
--    The last exit status
--
   function Get_Exit_Status (Process : Asynchronous_Process)
      return GInt;
--
-- Get_State -- The process state
--
--    Process - The process
--
-- Returns :
--
--    The current process state
--
   function Get_State (Process : Asynchronous_Process)
      return Process_State;
--
-- Input -- Get a new portion of input
--
--    Process - The process
--    Data    - Buffer to put input data into
--    Count   - The number character written into Data
--
-- The  implementation  signals  end of the input by setting Count to 0.
-- The default implementation does this. Note  that  this  procedure  is
-- called from an independent task. Use  Gtk.Main.Router  facilities  if
-- you want to call GTK+ operations from Input.
--
   procedure Input
             (  Process : in out Asynchronous_Process;
                Data    : out UTF8_String;
                Count   : out Natural
             );
--
-- Output -- Accept a new portion of output
--
--    Process - The process
--    Data    - Output data
--
-- The  default implementation does nothing. Note that this procedure is
-- called from an independent task. Use  Gtk.Main.Router  facilities  if
-- you want to call GTK+ operations from Output.
--
   procedure Output
             (  Process : in out Asynchronous_Process;
                Data    : UTF8_String
             );
--
-- Run -- Run asynchronously
--
--    Process             - The object
--    Name                - The process name to call
--  [ Working_Directory ] - The working directory name
--    ArgV                - Arguments list
--  [ EnvP ]              - Environment
--
-- Exceptions :
--
--    Use_Error - Process is running
--
   procedure Run
             (  Process           : in out Asynchronous_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : Chars_Ptr_Array;
                EnvP              : Chars_Ptr_Array
             );
   procedure Run
             (  Process           : in out Asynchronous_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : GList;
                EnvP              : GList
             );
   procedure Run
             (  Process           : in out Asynchronous_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : Chars_Ptr_Array
             );
   procedure Run
             (  Process           : in out Asynchronous_Process;
                Name              : UTF8_String;
                Working_Directory : UTF8_String;
                ArgV              : GList
             );
   procedure Run
             (  Process : in out Asynchronous_Process;
                Name    : UTF8_String;
                ArgV    : Chars_Ptr_Array;
                EnvP    : Chars_Ptr_Array
             );
   procedure Run
             (  Process : in out Asynchronous_Process;
                Name    : UTF8_String;
                ArgV    : GList;
                EnvP    : GList
             );
   procedure Run
             (  Process : in out Asynchronous_Process;
                Name    : UTF8_String;
                ArgV    : Chars_Ptr_Array
             );
   procedure Run
             (  Process : in out Asynchronous_Process;
                Name    : UTF8_String;
                ArgV    : GList
             );
--
-- Wait -- For process completion
--
--    Process  - The object
--    Time_Out - Time to wait
--
-- Exceptions :
--
--    Time_Error - Timed out
--
   procedure Wait
             (  Process  : in out Asynchronous_Process;
                Time_Out : Duration := Duration'Last
             );
private
   pragma Inline (Get_Error);
   pragma Inline (Get_State);

   task type Reader
             (  Process : access Asynchronous_Process'Class;
                Pipe    : GInt
             );
   type Reader_Ptr is access Reader;

   task type Writer
             (  Process : access Asynchronous_Process'Class;
                Pipe    : GInt
             );
   type Writer_Ptr is access Writer;

   protected type Process_Status is
      function Get return Process_State;
      function Get return GInt;
      function Completed_Tasks_Count return Natural;
      procedure Set (State : Process_State);
      procedure Set (State : Process_State; Status : GInt);
      entry Wait_IO;
      entry Wait_All;
   private
      State  : Process_State := Process_Completed;
      Count  : Natural       := 0; -- Number of completed tasks
      Status : GInt;
   end Process_Status;
   type Chars_Ptr_Array_Ptr is access Chars_Ptr_Array;

   type Asynchronous_Process is
      new Ada.Finalization.Limited_Controlled with
   record
      Input_Writer    : Writer_Ptr;
      Output_Reader   : Reader_Ptr;
      Error_Reader    : Reader_Ptr;
      Standard_Input  : GInt;
      Standard_Output : GInt;
      Standard_Error  : GInt;
      PID             : GPID;
      Error           : GError;
      ArgV            : Chars_Ptr_Array_Ptr;
      EnvP            : Chars_Ptr_Array_Ptr;
      Status          : Process_Status;
   end record;

   procedure Start
             (  Process : in out Asynchronous_Process'Class;
                Result  : Async_Result
             );

   type Asynchronous_Process_Ptr is access all Asynchronous_Process'Class;
   package Messages is new Generic_Message (Asynchronous_Process_Ptr);
   use Messages;

   procedure Service_Exit (Process : in out Asynchronous_Process_Ptr);

end GLib.Spawn.Asynchronous;
