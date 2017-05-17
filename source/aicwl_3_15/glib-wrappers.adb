--                                                                    --
--  package GLib.Wrappers           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2009       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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

package body Glib.Wrappers is

   function G_Dir_Open
     (Path  : System.Address;
      Flags : Guint;
      Error : System.Address) return System.Address;
   pragma Import (C, G_Dir_Open, "g_dir_open");
   pragma Weak_External (G_Dir_Open);

   function G_Dir_Read_Name (Dir : System.Address) return System.Address;
   pragma Import (C, G_Dir_Read_Name, "g_dir_read_name");
   pragma Weak_External (G_Dir_Read_Name);

   function G_File_Test
     (File_Name : System.Address;
      Test      : Gint) return Gboolean;
   pragma Import (C, G_File_Test, "g_file_test");
   pragma Weak_External (G_File_Test);

   function G_Find_Program_In_Path
     (Program : System.Address) return System.Address;
   pragma Import (C, G_Find_Program_In_Path, "g_find_program_in_path");
   pragma Weak_External (G_Find_Program_In_Path);

   function G_Get_Current_Dir return System.Address;
   pragma Import (C, G_Get_Current_Dir, "g_get_current_dir");
   pragma Weak_External (G_Get_Current_Dir);

   function G_Spawn_Async
     (Working_Directory : System.Address;
      ArgV              : System.Address;
      EnvP              : System.Address;
      Flags             : Guint;
      Child_Setup       : System.Address;
      User_Data         : System.Address;
      Child_PID         : System.Address;
      Error             : System.Address) return Gboolean;
   pragma Import (C, G_Spawn_Async, "g_spawn_async");
   pragma Weak_External (G_Spawn_Async);

   function G_Spawn_Async_With_Pipes
     (Working_Directory : System.Address;
      ArgV              : System.Address;
      EnvP              : System.Address;
      Flags             : Guint;
      Child_Setup       : System.Address;
      User_Data         : System.Address;
      Child_PID         : System.Address;
      Standard_Input    : System.Address;
      Standard_Output   : System.Address;
      Standard_Error    : System.Address;
      Error             : System.Address) return Gboolean;
   pragma Import (C, G_Spawn_Async_With_Pipes, "g_spawn_async_with_pipes");
   pragma Weak_External (G_Spawn_Async_With_Pipes);

   function G_Spawn_Command_Line_Async
     (Command_Line : System.Address;
      Error        : System.Address) return Gboolean;
   pragma Import (C, G_Spawn_Command_Line_Async, "g_spawn_command_line_async");
   pragma Weak_External (G_Spawn_Command_Line_Async);

   function G_Spawn_Command_Line_Sync
     (Command_Line    : System.Address;
      Standard_Output : System.Address;
      Standard_Error  : System.Address;
      Exit_Status     : System.Address;
      Error           : System.Address) return Gboolean;
   pragma Import (C, G_Spawn_Command_Line_Sync, "g_spawn_command_line_sync");
   pragma Weak_External (G_Spawn_Command_Line_Sync);

   function G_Spawn_Sync
     (Working_Directory : System.Address;
      ArgV              : System.Address;
      EnvP              : System.Address;
      Flags             : Guint;
      Child_Setup       : System.Address;
      User_Data         : System.Address;
      Standard_Output   : System.Address;
      Standard_Error    : System.Address;
      Exit_Status       : System.Address;
      Error             : System.Address) return Gboolean;
   pragma Import (C, G_Spawn_Sync, "g_spawn_sync");
   pragma Weak_External (G_Spawn_Sync);

   function G_Dir_Open_UTF8
     (Path  : System.Address;
      Flags : Guint;
      Error : System.Address) return System.Address is
   begin
      return G_Dir_Open (Path, Flags, Error);
   end G_Dir_Open_UTF8;

   function G_Dir_Read_Name_UTF8 (Dir : System.Address) return System.Address is
   begin
      return G_Dir_Read_Name (Dir);
   end G_Dir_Read_Name_UTF8;

   function G_File_Test_UTF8
     (File_Name : System.Address;
      Test      : Gint) return Gboolean is
   begin
      return G_File_Test (File_Name, Test);
   end G_File_Test_UTF8;

   function G_Find_Program_In_Path_UTF8 (Program : System.Address)
      return System.Address is
   begin
      return G_Find_Program_In_Path (Program);
   end G_Find_Program_In_Path_UTF8;

   function G_Get_Current_Dir_UTF8 return System.Address is
   begin
      return G_Get_Current_Dir;
   end G_Get_Current_Dir_UTF8;

   function G_Spawn_Async_UTF8
     (Working_Directory : System.Address;
      ArgV              : System.Address;
      EnvP              : System.Address;
      Flags             : Guint;
      Child_Setup       : System.Address;
      User_Data         : System.Address;
      Child_PID         : System.Address;
      Error             : System.Address) return Gboolean is
   begin
      return
        G_Spawn_Async
          (Working_Directory => Working_Directory,
           ArgV              => ArgV,
           EnvP              => EnvP,
           Flags             => Flags,
           Child_Setup       => Child_Setup,
           User_Data         => User_Data,
           Child_PID         => Child_PID,
           Error             => Error);
   end G_Spawn_Async_UTF8;

   function G_Spawn_Async_With_Pipes_UTF8
     (Working_Directory : System.Address;
      ArgV              : System.Address;
      EnvP              : System.Address;
      Flags             : Guint;
      Child_Setup       : System.Address;
      User_Data         : System.Address;
      Child_PID         : System.Address;
      Standard_Input    : System.Address;
      Standard_Output   : System.Address;
      Standard_Error    : System.Address;
      Error             : System.Address) return Gboolean is
   begin
      return
        G_Spawn_Async_With_Pipes
          (Working_Directory => Working_Directory,
           ArgV              => ArgV,
           EnvP              => EnvP,
           Flags             => Flags,
           Child_Setup       => Child_Setup,
           User_Data         => User_Data,
           Child_PID         => Child_PID,
           Standard_Input    => Standard_Input,
           Standard_Output   => Standard_Output,
           Standard_Error    => Standard_Error,
           Error             => Error);
   end G_Spawn_Async_With_Pipes_UTF8;

   function G_Spawn_Sync_UTF8
     (Working_Directory : System.Address;
      ArgV              : System.Address;
      EnvP              : System.Address;
      Flags             : Guint;
      Child_Setup       : System.Address;
      User_Data         : System.Address;
      Standard_Output   : System.Address;
      Standard_Error    : System.Address;
      Exit_Status       : System.Address;
      Error             : System.Address) return Gboolean is
   begin
      return
        G_Spawn_Sync
          (Working_Directory => Working_Directory,
           ArgV              => ArgV,
           EnvP              => EnvP,
           Flags             => Flags,
           Child_Setup       => Child_Setup,
           User_Data         => User_Data,
           Standard_Output   => Standard_Output,
           Standard_Error    => Standard_Error,
           Exit_Status       => Exit_Status,
           Error             => Error);
   end G_Spawn_Sync_UTF8;

   function G_Spawn_Command_Line_Async_UTF8
     (Command_Line : System.Address;
      Error        : System.Address) return Gboolean is
   begin
      return G_Spawn_Command_Line_Async (Command_Line, Error);
   end G_Spawn_Command_Line_Async_UTF8;

   function G_Spawn_Command_Line_Sync_UTF8
     (Command_Line    : System.Address;
      Standard_Output : System.Address;
      Standard_Error  : System.Address;
      Exit_Status     : System.Address;
      Error           : System.Address) return Gboolean is
   begin
      return
        G_Spawn_Command_Line_Sync
          (Command_Line      => Command_Line,
           Standard_Output   => Standard_Output,
           Standard_Error    => Standard_Error,
           Exit_Status       => Exit_Status,
           Error             => Error);
   end G_Spawn_Command_Line_Sync_UTF8;

   function SetErrorMode (Mode : Interfaces.Unsigned_32)
                          return Interfaces.Unsigned_32
   is
      pragma Unreferenced (Mode);
   begin
      return 0;
   end SetErrorMode;

end Glib.Wrappers;
