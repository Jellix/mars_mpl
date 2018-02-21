--                                                                    --
--  package GLib.Spawn              Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2009       --
--                                                                    --
--                                Last revision :  20:53 22 May 2009  --
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

with Glib.Error;

with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with System;

package Glib.Spawn is

   type GSpawnFlags is new Guint;

   Spawn_Leave_Descriptors_Open : constant GSpawnFlags := 2 ** 0;
   Spawn_Do_Not_Reap_Child      : constant GSpawnFlags := 2 ** 1;
   Spawn_Search_Path            : constant GSpawnFlags := 2 ** 2;
   Spawn_Stdout_To_Dev_Null     : constant GSpawnFlags := 2 ** 3;
   Spawn_Stderr_To_Dev_Null     : constant GSpawnFlags := 2 ** 4;
   Spawn_Child_Inherits_Stdin   : constant GSpawnFlags := 2 ** 5;
   Spawn_File_And_ArgV_Zero     : constant GSpawnFlags := 2 ** 6;

   type GPID is new Interfaces.C.int;

   --
   -- Child_Setup -- A procedure used to set up the child
   --
   type GSpawnChildSetupFunc is access procedure (Data : System.Address);
   pragma Convention (C, GSpawnChildSetupFunc);

   --
   -- Child_Watch_Func -- A procedure to be called upon child completion
   --
   type Child_Watch_Func is access procedure
     (PID    : GPID;
      Status : Gint;
      Data   : System.Address);
   pragma Convention (C, Child_Watch_Func);

   type Chars_Ptr_Array is
     array (Positive range <>) of aliased Interfaces.C.Strings.chars_ptr;
   package Chars_Ptr_Lists is
     new Interfaces.C.Pointers
       (Index              => Positive,
        Element            => Interfaces.C.Strings.Chars_Ptr,
        Element_Array      => Chars_Ptr_Array,
        Default_Terminator => Interfaces.C.Strings.Null_Ptr);

   --
   -- Async_Result -- Result of a synchronous spawn
   --
   --   Running    - True if the process has been spawned
   --   Pipelined  - True if pipes were created
   --
   type Async_Result
     (Running   : Boolean := False;
      Pipelined : Boolean := False) is
      record
         case Running is
            when True =>
               PID : GPID;
               case Pipelined is
                  when True =>
                     Standard_Input  : Gint;
                     Standard_Output : Gint;
                     Standard_Error  : Gint;
                  when False =>
                     null;
               end case;
            when False =>
               Error : Glib.Error.GError;
         end case;
      end record;

   --
   -- Sync_Result -- Result of a synchronous spawn
   --
   --   Executed - True if the process was spawned
   --
   type Sync_Result (Executed : Boolean := False) is
      record
         case Executed is
            when True =>
               Exit_Status     : Gint;
               Standard_Output : Interfaces.C.Strings.chars_ptr;
               Standard_Error  : Interfaces.C.Strings.chars_ptr;
            when False =>
               Error           : Glib.Error.GError;
         end case;
      end record;

   --
   -- Async -- Spawns a process asynchronously
   --
   --  [ Working_Directory ] - The working directory name
   --    ArgV                - Arguments list (null terminated)
   --    EnvP                - Environment (null terminated)
   --    Flags               - Spawn flags
   --    Child_Setup         - Setup data object
   --
   -- Upon process invokation Setup  procedure  is  called  on  Child_Setup
   -- object. Child_Setup and user_data are a function and  user  data.  On
   -- POSIX platforms, the function is called in the child after  GLib  has
   -- performed all the setup  it  plans  to  perform  (including  creating
   -- pipes,  closing  file  descriptors,  etc.) but before calling exec().
   -- That is, Child_Setup is called just  before  calling  exec()  in  the
   -- child. Obviously actions taken in this function will only affect  the
   -- child, not the parent.  EnvP can be null when current environment has
   -- to be used.
   --
   -- Returns :
   --
   --    Spawning result
   --
   function Async
     (Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Lists.Pointer;
      EnvP              : Chars_Ptr_Lists.Pointer := null;
      Flags             : GSpawnFlags;
      Child_Setup       : GSpawnChildSetupFunc    := null;
      Data              : System.Address          := System.Null_Address)
      return Async_Result;

   function Async
     (ArgV              : Chars_Ptr_Lists.Pointer;
      EnvP              : Chars_Ptr_Lists.Pointer := null;
      Flags             : GSpawnFlags;
      Child_Setup       : GSpawnChildSetupFunc    := null;
      Data              : System.Address          := System.Null_Address)
      return Async_Result;

   --
   -- Async_With_Pipes -- Spawns a process asynchronously
   --
   --  [ Working_Directory ] - The working directory name
   --    ArgV                - Arguments list (null terminated)
   --  [ EnvP ]              - Environment (null terminated)
   --    Flags               - Spawn flags
   --    Child_Setup         - Setup procedure
   --    User_Data           - Data for the setup procedure
   --
   -- Upon process invokation Setup  procedure  is  called  on  Child_Setup
   -- object. Child_Setup and user_data are a function and  user  data.  On
   -- POSIX platforms, the function is called in the child after  GLib  has
   -- performed all the setup  it  plans  to  perform  (including  creating
   -- pipes,  closing  file  descriptors,  etc.) but before calling exec().
   -- That is, Child_Setup is called just  before  calling  exec()  in  the
   -- child. Obviously actions taken in this function will only affect  the
   -- child, not the parent.  EnvP can be null when current environment has
   -- to be used.
   --
   -- Returns :
   --
   --    Spawning result
   --
   function Async_With_Pipes
     (Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Lists.Pointer;
      EnvP              : Chars_Ptr_Lists.Pointer := null;
      Flags             : GSpawnFlags;
      Child_Setup       : GSpawnChildSetupFunc    := null;
      User_Data         : System.Address          := System.Null_Address)
      return Async_Result;

   function Async_With_Pipes
     (ArgV              : Chars_Ptr_Lists.Pointer;
      EnvP              : Chars_Ptr_Lists.Pointer := null;
      Flags             : GSpawnFlags;
      Child_Setup       : GSpawnChildSetupFunc    := null;
      User_Data         : System.Address          := System.Null_Address)
      return Async_Result;

   --
   -- Child_Watch_Add -- Child process completion callback
   --
   --    PID  - Process ID
   --    Func - To call
   --    Data - User data to pass
   --
   procedure Child_Watch_Add
     (PID  : GPID;
      Func : Child_Watch_Func;
      Data : System.Address);

   --
   -- Close_PID -- Process identifier
   --
   --    PID - Process ID
   --
   -- On  some  platforms,  notably  Windows,  the  GPid  type represents a
   -- resource  which  must  be  closed  to   prevent   resource   leaking.
   -- g_spawn_close_pid() is provided for this purpose. It should  be  used
   -- on all platforms, even though it doesn't do anything under UNIX.
   --
   procedure Close_PID (PID : GPID);

   --
   -- Command_Line_Async -- Spawns a process asynchronously
   --
   --    Command_Line - The command line
   --
   -- When the result is not null, it has to be freed using Error_Free.
   --
   -- Returns :
   --
   --    null on success, error otherwise.
   --
   function Command_Line_Async
     (Command_Line : UTF8_String) return Glib.Error.GError;

   --
   -- Command_Line_Sync -- Spawns a process synchronously
   --
   --    Command_Line - The command line
   --
   -- Returns :
   --
   --    Execution result
   --
   function Command_Line_Sync
     (Command_Line : UTF8_String) return Sync_Result;

   --
   -- Sync -- Spawns a process synchronously
   --
   --  [ Working_Directory ] - The working directory name
   --    ArgV                - Arguments list (null terminated)
   --  [ EnvP ]              - Environment (null terminated)
   --    Flags               - Spawn flags
   --    Child_Setup         - Setup procedure
   --    User_Data           - Data for the setup procedure
   --
   -- Upon process invokation Setup  procedure  is  called  on  Child_Setup
   -- object. Child_Setup and user_data are a function and  user  data.  On
   -- POSIX platforms, the function is called in the child after  GLib  has
   -- performed all the setup  it  plans  to  perform  (including  creating
   -- pipes,  closing  file  descriptors,  etc.) but before calling exec().
   -- That is, Child_Setup is called just  before  calling  exec()  in  the
   -- child. Obviously actions taken in this function will only affect  the
   -- child, not the parent.  EnvP can be null when current environment has
   -- to be used.
   --
   -- Returns :
   --
   --    Execution result
   --
   function Sync
     (Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Lists.Pointer;
      EnvP              : Chars_Ptr_Lists.Pointer := null;
      Flags             : GSpawnFlags;
      Child_Setup       : GSpawnChildSetupFunc    := null;
      User_Data         : System.Address          := System.Null_Address)
      return Sync_Result;

   function Sync
     (ArgV              : Chars_Ptr_Lists.Pointer;
      EnvP              : Chars_Ptr_Lists.Pointer := null;
      Flags             : GSpawnFlags;
      Child_Setup       : GSpawnChildSetupFunc    := null;
      User_Data         : System.Address          := System.Null_Address)
      return Sync_Result;

private

   pragma Import (C, Child_Watch_Add, "g_child_watch_add");
   pragma Import (C, Close_PID, "g_spawn_close_pid");

end Glib.Spawn;
