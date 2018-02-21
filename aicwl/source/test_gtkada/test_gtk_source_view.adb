--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Gtk_Source_View                        Luebeck            --
--  Test for Gtk.Source_View                       Summer, 2009       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with GLib;                         use GLib;
with Glib.Properties;              use Glib.Properties;
with Gdk.Event;                    use Gdk.Event;
with Gtk.Main.Router.GNAT_Stack;   use Gtk.Main.Router;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Source_Buffer;            use Gtk.Source_Buffer;
with Gtk.Source_Language;          use Gtk.Source_Language;
with Gtk.Source_Language_Manager;  use Gtk.Source_Language_Manager;
with Gtk.Source_View;              use Gtk.Source_View;
with Gtk.Table;                    use Gtk.Table;
with Gtk.Text_Buffer;              use Gtk.Text_Buffer;
with Gtk.Text_Iter;                use Gtk.Text_Iter;
with Gtk.Window;                   use Gtk.Window;
with Gtk.Widget;                   use Gtk.Widget;
with Pango.Font;                   use Pango.Font;

with Interfaces.C.Strings;
with GLib.Messages;

procedure Test_Gtk_Source_View is
   Window  : Gtk_Window;
   Grid    : Gtk_Table;
   Scroll  : Gtk_Scrolled_Window;
   View    : Gtk_Source_View;
   Buffer  : Gtk_Source_Buffer;
   Windows : Boolean := False;
begin
   Gtk.Main.Init;
   GNAT_Stack.Set_Log_Trace
   (  "",
      GLib.Messages.Log_Level_Flags'Last
   );
   Gtk.Window.Gtk_New (Window);
   Init (Window);
   Set_Title (Window, "Test Source View");
   Window.On_Delete_Event (Gtk.Missed.Delete_Event_Handler'Access);
   Window.On_Destroy (Gtk.Missed.Destroy_Handler'Access);

   Gtk_New (Grid, 1, 1, False);
   Add (Window, Grid);
   Gtk_New (Scroll);
   Attach (Grid, Scroll, 0, 1, 0, 1);

   declare
      use Interfaces.C.Strings;
      Dirs : constant Chars_Ptr_Array := Get_Default.Get_Search_Path;
   begin
      Put_Line ("Language search paths:");
      for Index in Dirs'Range loop
         declare
            Path : constant String := Value (Dirs (Index));
         begin
            Windows := (  Windows
                       or else
                          (  Path'Length > 1
                          and then
                             Path (Path'First + 1) = ':'
                        )  );
            Put_Line ("   " & Path);
         end;
      end loop;
   end;
   declare
      use Interfaces.C.Strings;
      Language : constant Gtk_Source_Language :=
                          Get_Language (Get_Default, "ada");
   begin
      Put_Line ("Language Ada info:");
      Put_Line ("         Name:" & Language.Get_Name);
      Put_Line ("           ID:" & Language.Get_ID);
      Put_Line ("      Section:" & Language.Get_Section);
      Put_Line ("   MIME Types:");
      declare
         MIME : Chars_Ptr_Array := Language.Get_Mime_Types;
      begin
         for Index in MIME'Range loop
            Put_Line ("      " & Value (MIME (Index)));
            Free (MIME (Index));
         end loop;
      end;
      Put_Line ("        Globs:");
      declare
         Globs : Chars_Ptr_Array := Language.Get_Globs;
      begin
         for Index in Globs'Range loop
            Put_Line ("      " & Value (Globs (Index)));
            Free (Globs (Index));
         end loop;
      end;
      Put_Line ("    Style IDs:");
      declare
         Styles : Chars_Ptr_Array := Language.Get_Style_IDs;
      begin
         for Index in Styles'Range loop
            Put_Line ("      " & Value (Styles (Index)));
            Free (Styles (Index));
         end loop;
      end;
      Put_Line ("--------------");
   end;
   Gtk_New (Buffer, Get_Language (Get_Default, "ada"));
   Gtk_New (View, Buffer);
   Unref (Buffer);
   Add (Scroll, View);

   -- Customizing the way the widget acts
   Set_Right_Margin_Position (View, 72);
   Set_Show_Right_Margin (View, True);
   Set_Show_Line_Numbers (View, True);
   Set_Highlight_Current_Line (View, True);
   Set_Highlight_Matching_Brackets (Buffer, True);

   -- Setting a fixed font into the widget
   declare
      Font : Pango_Font_Description :=
                From_String ("fixed,Monospace 10");
   begin
      Modify_Font (View, Font);
      Free (Font);
   end;
   --
   -- Loading   the   source  file  into  the  buffer.  Note  that  this
   -- implementation  is  simplified  with  respect to file encoding. It
   -- assumes that the file is UTF-8. Loading Latin-1  files  will cause
   -- errors.  Latin-1  file  context,  like  under  Windows  should  be
   -- transcoded into UTF-8 before inserting into the buffer.
   --
   declare
      File         : File_Type;
      Input_Buffer : UTF8_String (1..1024);
      End_Iter     : Gtk_Text_Iter;
      Input_Last   : Natural := 0;
   begin
      Open (File, In_File, "test_gtk_source_view.adb");
      loop
         Get_Line
         (  File,
            Input_Buffer (Input_Last + 1..Input_Buffer'Last),
            Input_Last
         );
         if Input_Last < Input_Buffer'Last then
            Input_Last := Input_Last + 1;
            Input_Buffer (Input_Last) := Character'Val (10);
         end if;
         if Input_Last = Input_Buffer'Last then
            Get_End_Iter (Get_Buffer (View), End_Iter);
            Insert
            (  Buffer,
               End_Iter,
               Input_Buffer (1..Input_Last)
            );
            Input_Last := 0;
         end if;
      end loop;
   exception
      when End_Error =>
         Get_End_Iter (Buffer, End_Iter);
         Insert
         (  Get_Buffer (View),
            End_Iter,
            Input_Buffer (1..Input_Last)
         );
         Close (File);
      when Error : others =>
         Close (File);
         Trace ("Cannot load file: " & Exception_Information (Error));
   end;
   Show_All (Grid);
   Show (Window);

   Gtk.Main.Main;
exception
   when Error : others =>
      Trace ("Error: " & Exception_Information (Error));
end Test_Gtk_Source_View;
