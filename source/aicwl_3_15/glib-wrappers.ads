--                                                                    --
--  package GLib.Wrappers           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2009       --
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
--
--  The package is provided as a workaround to GLib design that has some
--  functions   named   differently   in   distributions  for  different
--  platforms. Though in the header files the functions are named  same,
--  they  are  renamed  using  #define  so  that  the  link names become
--  different.   For   example  g_file_test  is  g_file_test_utf8  under
--  Windows.  The  workaround  is that for such functions the references
--  are consistently made to the *_utf8 variant. This  package  provides
--  backups of these functions declared weak  externals.  Under  Windows
--  they are ignored. Under Linux the linker picks  them  up  since  the
--  native  *_utf8  functions  are  absent. The implemntations here call
--  weakly reference non-utf8 variants. The following figure illustrates
--  the way it works:
--
--                        g_file_test_utf8 (reference)
--                         /           \
--                        /             \
--           glibc-x.xx.a (Windows)    GLib.Wrappers
--                                       |
--                                  g_file_test_utf8 (weak external)
--                                       |
--                                  g_file_test (weak reference)
--                                       |
--                                  glibc-x.xx.a (Linux)
--
with System;        use System;
with Interfaces.C;  use Interfaces.C;

package Glib.Wrappers is

   function G_Dir_Open_UTF8
            (  Path  : Address;
               Flags : GUInt;
               Error : Address
            )  return Address;
   pragma External (C, G_Dir_Open_UTF8, "g_dir_open_utf8");
   pragma Weak_External (G_Dir_Open_UTF8);

   function G_Dir_Read_Name_UTF8 (Dir : Address) return Address;
   pragma External (C, G_Dir_Read_Name_UTF8, "g_dir_read_name_utf8");
   pragma Weak_External (G_Dir_Read_Name_UTF8);

   function G_File_Test_UTF8
            (  File_Name : Address;
               Test      : GInt
            )  return GBoolean;
   pragma External
          (  C,
             G_File_Test_UTF8,
             "g_file_test_utf8"
          );
   pragma Weak_External (G_File_Test_UTF8);

   function G_Find_Program_In_Path_UTF8 (Program : Address)
      return Address;
   pragma External
          (  C,
             G_Find_Program_In_Path_UTF8,
             "g_find_program_in_path_utf8"
          );
   pragma Weak_External (G_Find_Program_In_Path_UTF8);

   function G_Get_Current_Dir_UTF8 return Address;
   pragma External
          (  C,
             G_Get_Current_Dir_UTF8,
             "g_get_current_dir_utf8"
          );
   pragma Weak_External (G_Get_Current_Dir_UTF8);

   function G_Spawn_Async_UTF8
            (  Working_Directory : Address;
               ArgV              : Address;
               EnvP              : Address;
               Flags             : GUInt;
               Child_Setup       : Address;
               User_Data         : Address;
               Child_PID         : Address;
               Error             : Address
            )  return GBoolean;
   pragma External
          (  C,
             G_Spawn_Async_UTF8,
             "g_spawn_async_utf8"
          );
   pragma Weak_External (G_Spawn_Async_UTF8);

   function G_Spawn_Async_With_Pipes_UTF8
            (  Working_Directory : Address;
               ArgV              : Address;
               EnvP              : Address;
               Flags             : GUInt;
               Child_Setup       : Address;
               User_Data         : Address;
               Child_PID         : Address;
               Standard_Input    : Address;
               Standard_Output   : Address;
               Standard_Error    : Address;
               Error             : Address
            )  return GBoolean;
   pragma External
          (  C,
             G_Spawn_Async_With_Pipes_UTF8,
             "g_spawn_async_with_pipes_utf8"
          );
   pragma Weak_External (G_Spawn_Async_With_Pipes_UTF8);

   function G_Spawn_Sync_UTF8
            (  Working_Directory : Address;
               ArgV              : Address;
               EnvP              : Address;
               Flags             : GUInt;
               Child_Setup       : Address;
               User_Data         : Address;
               Standard_Output   : Address;
               Standard_Error    : Address;
               Exit_Status       : Address;
               Error             : Address
            )  return GBoolean;
   pragma External
          (  C,
             G_Spawn_Sync_UTF8,
             "g_spawn_sync_utf8"
          );
   pragma Weak_External (G_Spawn_Sync_UTF8);

   function G_Spawn_Command_Line_Async_UTF8
            (  Command_Line : Address;
               Error        : Address
            )  return GBoolean;
   pragma External
          (  C,
             G_Spawn_Command_Line_Async_UTF8,
             "g_spawn_command_line_async_utf8"
          );
   pragma Weak_External (G_Spawn_Command_Line_Async_UTF8);

   function G_Spawn_Command_Line_Sync_UTF8
            (  Command_Line    : Address;
               Standard_Output : Address;
               Standard_Error  : Address;
               Exit_Status     : Address;
               Error           : Address
            )  return GBoolean;
   pragma External
          (  C,
             G_Spawn_Command_Line_Sync_UTF8,
             "g_spawn_command_line_sync_utf8"
          );
   pragma Weak_External (G_Spawn_Command_Line_Sync_UTF8);

   function SetErrorMode (Mode : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32;
   pragma External (Stdcall, SetErrorMode, "SetErrorMode");
   pragma Weak_External (SetErrorMode);

end GLib.Wrappers;
