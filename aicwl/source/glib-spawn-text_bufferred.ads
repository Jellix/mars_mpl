--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Spawn.Text_Bufferred                   Luebeck            --
--  Interface                                      Spring, 2009       --
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

with Glib.Spawn.Asynchronous;

with Gtk.Enums;
with Gtk.Main.Router;
with Gtk.Text_Buffer;

package Glib.Spawn.Text_Bufferred is

   --
   -- Text_Bufferred_Process -- Asynchronous process with input, output and
   --                           error assigned to text buffers
   --
   type Text_Bufferred_Process is
     new Glib.Spawn.Asynchronous.Asynchronous_Process with private;

   --
   -- Finalize -- To be called when overridden
   --
   overriding procedure Finalize (Process : in out Text_Bufferred_Process);

   --
   -- Error -- Overrides GLib.Spawn.Asynchronous...
   --
   overriding procedure Error
     (Process : in out Text_Bufferred_Process;
      Data    : UTF8_String);

   --
   -- Input -- Overrides GLib.Spawn.Asynchronous...
   --
   overriding procedure Input
     (Process : in out Text_Bufferred_Process;
      Data    : out UTF8_String;
      Count   : out Natural);

   --
   -- Insert -- The procedure called to insert content into output buffer
   --
   --    Process - The object
   --    Buffer  - The text buffer to insert content (Output or Error)
   --    Error   - True if it is the standard error buffer
   --    Text    - The text to insert at the buffer end
   --
   -- This procedure is called on the context of GTK main loop. The default
   -- implementation adds Text at the end of Buffer.
   --
   procedure Insert
     (Process : in out Text_Bufferred_Process;
      Buffer  : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class;
      Error   : Boolean;
      Text    : UTF8_String);

   --
   -- Output -- Overrides GLib.Spawn.Asynchronous...
   --
   overriding procedure Output
     (Process : in out Text_Bufferred_Process;
      Data    : UTF8_String);

   --
   -- Run -- Spawn a process asynchronously with text buffers
   --
   --    Process             - The object
   --    Name                - The process name to call
   --  [ Working_Directory ] - The working directory name
   --    ArgV                - Arguments list
   --  [ EnvP   ]            - Environment
   --  [ Input  ]            - The text buffer containing the input
   --  [ Output ]            - The text buffer to accept the output
   --  [ Error  ]            - The text buffer to accept the output
   --
   -- Exceptions :
   --
   --    Use_Error - Process is running
   --
   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Array;
      EnvP              : Chars_Ptr_Array;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Gtk.Enums.String_List.Glist;
      EnvP              : Gtk.Enums.String_List.Glist;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Chars_Ptr_Array;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

   procedure Run
     (Process           : in out Text_Bufferred_Process;
      Name              : UTF8_String;
      Working_Directory : UTF8_String;
      ArgV              : Gtk.Enums.String_List.Glist;
      Input             : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output            : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error             : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Chars_Ptr_Array;
      EnvP    : Chars_Ptr_Array;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Gtk.Enums.String_List.Glist;
      EnvP    : Gtk.Enums.String_List.Glist;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Chars_Ptr_Array;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

   procedure Run
     (Process : in out Text_Bufferred_Process;
      Name    : UTF8_String;
      ArgV    : Gtk.Enums.String_List.Glist;
      Input   : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Output  : Gtk.Text_Buffer.Gtk_Text_Buffer := null;
      Error   : Gtk.Text_Buffer.Gtk_Text_Buffer := null);

private

   type Request_Read
     (Process : access Text_Bufferred_Process'Class;
      Length  : Positive) is
     new Gtk.Main.Router.Request_Data with
      record
         Count : Natural;
         Text  : System.Address;
      end record;

   overriding procedure Service (Data : in out Request_Read);

   type Request_Write
     (Process : access Text_Bufferred_Process'Class;
      Error   : Boolean;
      Length  : Positive) is
     new Gtk.Main.Router.Request_Data with
      record
         Text : System.Address;
      end record;

   overriding procedure Service (Data : in out Request_Write);

   type Text_Bufferred_Process is
     new Glib.Spawn.Asynchronous.Asynchronous_Process with
      record
         Input    : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Output   : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Error    : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Position : Gint;
      end record;

end Glib.Spawn.Text_Bufferred;
