--                                                                    --
--  package GLib.Spawn              Copyright (c)  Dmitry A. Kazakov  --
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
--____________________________________________________________________--

with GLib.Wrappers;
with System.Address_To_Access_Conversions;

package body GLib.Spawn is
   use Interfaces.C;

   function G_Spawn_Command_Line_Async_UTF8
            (  Command_Line : Char_Array;
               Error        : access GError
            )  return GBoolean;
   pragma Import
          (  C,
             G_Spawn_Command_Line_Async_UTF8,
             "g_spawn_command_line_async_utf8"
          );

   function G_Spawn_Command_Line_Sync_UTF8
            (  Command_Line    : Char_Array;
               Standard_Output : access Chars_Ptr;
               Standard_Error  : access Chars_Ptr;
               Exit_Status     : access GInt;
               Error           : access GError
            )  return GBoolean;
   pragma Import
          (  C,
             G_Spawn_Command_Line_Sync_UTF8,
             "g_spawn_command_line_sync_utf8"
          );

   function G_Spawn_Async_UTF8
            (  Working_Directory : Address;
               ArgV              : Chars_Ptr_Lists.Pointer;
               EnvP              : Chars_Ptr_Lists.Pointer;
               Flags             : GSpawnFlags;
               Child_Setup       : GSpawnChildSetupFunc;
               User_Data         : Address;
               Child_PID         : access GPID;
               Error             : access GError
            )  return GBoolean;
   pragma Import (C, G_Spawn_Async_UTF8, "g_spawn_async_utf8");

   function G_Spawn_Async_With_Pipes_UTF8
            (  Working_Directory : Address;
               ArgV              : Chars_Ptr_Lists.Pointer;
               EnvP              : Chars_Ptr_Lists.Pointer;
               Flags             : GSpawnFlags;
               Child_Setup       : GSpawnChildSetupFunc;
               User_Data         : Address;
               Child_PID         : access GPID;
               Standard_Input    : access GInt;
               Standard_Output   : access GInt;
               Standard_Error    : access GInt;
               Error             : access GError
            )  return GBoolean;
   pragma Import
          (  C,
             G_Spawn_Async_With_Pipes_UTF8,
             "g_spawn_async_with_pipes_utf8"
          );

   function G_Spawn_Sync_UTF8
            (  Working_Directory : Address;
               ArgV              : Chars_Ptr_Lists.Pointer;
               EnvP              : Chars_Ptr_Lists.Pointer;
               Flags             : GSpawnFlags;
               Child_Setup       : GSpawnChildSetupFunc;
               User_Data         : Address;
               Standard_Output   : access Chars_Ptr;
               Standard_Error    : access Chars_Ptr;
               Exit_Status       : access GInt;
               Error             : access GError
            )  return GBoolean;
   pragma Import (C, G_Spawn_Sync_UTF8, "g_spawn_sync_utf8");

   function Async
            (  Working_Directory : UTF8_String;
               ArgV              : Chars_Ptr_Lists.Pointer;
               EnvP              : Chars_Ptr_Lists.Pointer := null;
               Flags             : GSpawnFlags;
               Child_Setup       : GSpawnChildSetupFunc := null;
               Data              : Address              := Null_Address
            )  return Async_Result is
      PID    : aliased GPID;
      Error  : aliased GError;
      Result : GBoolean;
   begin
      Result :=
         G_Spawn_Async_UTF8
         (  Working_Directory => To_C (Working_Directory)'Address,
            ArgV              => ArgV,
            EnvP              => EnvP,
            Flags             => Flags,
            Child_Setup       => Child_Setup,
            User_Data         => Data,
            Child_PID         => PID'Unchecked_Access,
            Error             => Error'Unchecked_Access
         );
      if Result = 0 then
         return
         (  Running   => False,
            Pipelined => False,
            Error     => Error
         );
      else
         return
         (  Running   => True,
            Pipelined => False,
            PID       => PID
         );
      end if;
   end Async;

   function Async
            (  ArgV        : Chars_Ptr_Lists.Pointer;
               EnvP        : Chars_Ptr_Lists.Pointer := null;
               Flags       : GSpawnFlags;
               Child_Setup : GSpawnChildSetupFunc := null;
               Data        : Address              := Null_Address
            )  return Async_Result is
      PID    : aliased GPID;
      Error  : aliased GError;
      Result : GBoolean;
   begin
      Result :=
         G_Spawn_Async_UTF8
         (  Working_Directory => Null_Address,
            ArgV              => ArgV,
            EnvP              => EnvP,
            Flags             => Flags,
            Child_Setup       => Child_Setup,
            User_Data         => Data,
            Child_PID         => PID'Unchecked_Access,
            Error             => Error'Unchecked_Access
         );
      if Result = 0 then
         return
         (  Running   => False,
            Pipelined => False,
            Error     => Error
         );
      else
         return
         (  Running   => True,
            Pipelined => False,
            PID       => PID
         );
      end if;
   end Async;

   function Async_With_Pipes
            (  Working_Directory : UTF8_String;
               ArgV              : Chars_Ptr_Lists.Pointer;
               EnvP              : Chars_Ptr_Lists.Pointer := null;
               Flags             : GSpawnFlags;
               Child_Setup       : GSpawnChildSetupFunc := null;
               User_Data         : Address              := Null_Address
            )  return Async_Result is
      PID             : aliased GPID;
      Standard_Input  : aliased GInt;
      Standard_Output : aliased GInt;
      Standard_Error  : aliased GInt;
      Error           : aliased GError;
      Result          : GBoolean;
   begin
      Result :=
         G_Spawn_Async_With_Pipes_UTF8
         (  Working_Directory => To_C (Working_Directory)'Address,
            ArgV              => ArgV,
            EnvP              => EnvP,
            Flags             => Flags,
            Child_Setup       => Child_Setup,
            User_Data         => User_Data,
            Child_PID         => PID'Unchecked_Access,
            Standard_Input    => Standard_Input'Unchecked_Access,
            Standard_Output   => Standard_Output'Unchecked_Access,
            Standard_Error    => Standard_Error'Unchecked_Access,
            Error             => Error'Unchecked_Access
         );
      if Result = 0 then
         return
         (  Running   => False,
            Pipelined => False,
            Error     => Error
         );
      else
         return
         (  Running         => True,
            Pipelined       => True,
            PID             => PID,
            Standard_Input  => Standard_Input,
            Standard_Output => Standard_Output,
            Standard_Error  => Standard_Error
         );
      end if;
   end Async_With_Pipes;

   function Async_With_Pipes
            (  ArgV        : Chars_Ptr_Lists.Pointer;
               EnvP        : Chars_Ptr_Lists.Pointer := null;
               Flags       : GSpawnFlags;
               Child_Setup : GSpawnChildSetupFunc := null;
               User_Data   : Address              := Null_Address
            )  return Async_Result is
      PID             : aliased GPID;
      Standard_Input  : aliased GInt;
      Standard_Output : aliased GInt;
      Standard_Error  : aliased GInt;
      Error           : aliased GError;
      Result          : GBoolean;
   begin
      Result :=
         G_Spawn_Async_With_Pipes_UTF8
         (  Working_Directory => Null_Address,
            ArgV              => ArgV,
            EnvP              => EnvP,
            Flags             => Flags,
            Child_Setup       => Child_Setup,
            User_Data         => User_Data,
            Child_PID         => PID'Unchecked_Access,
            Standard_Input    => Standard_Input'Unchecked_Access,
            Standard_Output   => Standard_Output'Unchecked_Access,
            Standard_Error    => Standard_Error'Unchecked_Access,
            Error             => Error'Unchecked_Access
         );
      if Result = 0 then
         return
         (  Running   => False,
            Pipelined => False,
            Error     => Error
         );
      else
         return
         (  Running         => True,
            Pipelined       => True,
            PID             => PID,
            Standard_Input  => Standard_Input,
            Standard_Output => Standard_Output,
            Standard_Error  => Standard_Error
         );
      end if;
   end Async_With_Pipes;

   function Command_Line_Async
            (  Command_Line : UTF8_String
            )  return GError is
      Error  : aliased GError;
      Result : GBoolean;
   begin
      Result :=
         G_Spawn_Command_Line_Async_UTF8
         (  To_C (Command_Line),
            Error'Unchecked_Access
         );
      return Error;
   end Command_Line_Async;

   function Command_Line_Sync (Command_Line : UTF8_String)
      return Sync_Result is
      Exit_Status     : aliased GInt;
      Standard_Output : aliased Chars_Ptr := Null_Ptr;
      Standard_Error  : aliased Chars_Ptr := Null_Ptr;
      Error           : aliased GError;
      Result          : GBoolean;
   begin
      Result :=
         G_Spawn_Command_Line_Sync_UTF8
         (  Command_Line    => To_C (Command_Line),
            Standard_Output => Standard_Output'Unchecked_Access,
            Standard_Error  => Standard_Error'Unchecked_Access,
            Exit_Status     => Exit_Status'Unchecked_Access,
            Error           => Error'Unchecked_Access
         );
      if  Result = 0 then
         return
         (  Executed => False,
            Error    => Error
         );
      else
         return
         (  Executed        => True,
            Exit_Status     => Exit_Status,
            Standard_Output => Standard_Output,
            Standard_Error  => Standard_Error
         );
      end if;
   end Command_Line_Sync;

   function Sync
            (  Working_Directory : UTF8_String;
               ArgV              : Chars_Ptr_Lists.Pointer;
               EnvP              : Chars_Ptr_Lists.Pointer := null;
               Flags             : GSpawnFlags;
               Child_Setup       : GSpawnChildSetupFunc := null;
               User_Data         : Address              := Null_Address
            )  return Sync_Result is
      Exit_Status     : aliased GInt;
      Standard_Output : aliased Chars_Ptr;
      Standard_Error  : aliased Chars_Ptr;
      Error           : aliased GError;
      Result          : GBoolean;
   begin
      Result :=
         G_Spawn_Sync_UTF8
         (  Working_Directory => To_C (Working_Directory)'Address,
            ArgV              => ArgV,
            EnvP              => EnvP,
            Flags             => Flags,
            Child_Setup       => Child_Setup,
            User_Data         => User_Data,
            Standard_Output   => Standard_Output'Unchecked_Access,
            Standard_Error    => Standard_Error'Unchecked_Access,
            Exit_Status       => Exit_Status'Unchecked_Access,
            Error             => Error'Unchecked_Access
         );
      if Result = 0 then
         return
         (  Executed => False,
            Error    => Error
         );
      else
         return
         (  Executed        => True,
            Exit_Status     => Exit_Status,
            Standard_Output => Standard_Output,
            Standard_Error  => Standard_Error
         );
      end if;
   end Sync;

   function Sync
            (  ArgV        : Chars_Ptr_Lists.Pointer;
               EnvP        : Chars_Ptr_Lists.Pointer := null;
               Flags       : GSpawnFlags;
               Child_Setup : GSpawnChildSetupFunc := null;
               User_Data   : Address              := Null_Address
            )  return Sync_Result is
      Exit_Status     : aliased GInt;
      Standard_Output : aliased Chars_Ptr;
      Standard_Error  : aliased Chars_Ptr;
      Error           : aliased GError;
      Result          : GBoolean;
   begin
      Result :=
         G_Spawn_Sync_UTF8
         (  Working_Directory => Null_Address,
            ArgV              => ArgV,
            EnvP              => EnvP,
            Flags             => Flags,
            Child_Setup       => Child_Setup,
            User_Data         => User_Data,
            Standard_Output   => Standard_Output'Unchecked_Access,
            Standard_Error    => Standard_Error'Unchecked_Access,
            Exit_Status       => Exit_Status'Unchecked_Access,
            Error             => Error'Unchecked_Access
         );
      if Result = 0 then
         return
         (  Executed => False,
            Error    => Error
         );
      else
         return
         (  Executed        => True,
            Exit_Status     => Exit_Status,
            Standard_Output => Standard_Output,
            Standard_Error  => Standard_Error
         );
      end if;
   end Sync;

end GLib.Spawn;
