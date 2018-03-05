with Gtk.Scrolled_Window;
with Gtk.Text_Iter;

package body Gtk.Frame.Log_Viewer is

   use type GNAT.Expect.Expect_Match;

   function Gtk_Frame_Log_Viewer_New
     (Label   : in String;
      Process : in GNAT.Expect.Process_Descriptor_Access) return not null access
     Gtk_Frame_Log_Viewer_Record'Class
   is
      This : constant Gtk_Frame_Log_Viewer := new Gtk_Frame_Log_Viewer_Record;
   begin
      This.all.Initialize (Label   => Label,
                           Process => Process);

      return This;
   end Gtk_Frame_Log_Viewer_New;

   procedure Initialize
     (This    : not null access Gtk_Frame_Log_Viewer_Record;
      Label   : in String;
      Process : in GNAT.Expect.Process_Descriptor_Access)
   is
      Log_Window : constant Gtk.Scrolled_Window.Gtk_Scrolled_Window :=
                     Gtk.Scrolled_Window.Gtk_Scrolled_Window_New;
      Log_View   : constant Gtk.Text_View.Gtk_Text_View :=
                     Gtk.Text_View.Gtk_Text_View_New;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Frame.Initialize (Frame => This,
                            Label => Label);

      This.all.Process := Process;
      This.all.Text_Buffer := Log_View.all.Get_Buffer;
      This.all.Text_View   := Log_View;
      This.all.Text_Buffer.all.Get_End_Iter (Iter => End_Iter);
      This.all.End_Mark    :=
        This.all.Text_Buffer.all.Create_Mark (Where        => End_Iter,
                                              Left_Gravity => False);

      Log_View.all.Set_Editable (Setting => False);
      Log_Window.all.Add (Widget => Log_View);
      This.all.Add (Widget => Log_Window);
   end Initialize;

   not overriding procedure Update
     (This : not null access Gtk_Frame_Log_Viewer_Record)
   is
      Match_Result : GNAT.Expect.Expect_Match := 0;
      End_Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      This.all.Text_Buffer.all.Get_End_Iter (Iter => End_Iter);

      Read_Line_From_Process :
      while Match_Result /= GNAT.Expect.Expect_Timeout loop
         GNAT.Expect.Expect (Descriptor  => This.all.Process.all,
                             Result      => Match_Result,
                             Regexp      => New_Line,
                             Timeout     => 1, --  essentially polling
                             Full_Buffer => False);

         if Match_Result /= GNAT.Expect.Expect_Timeout then
            This.all.Text_Buffer.all.Insert
              (Text =>
                 GNAT.Expect.Expect_Out_Match
                   (Descriptor => This.all.Process.all),
               Iter => End_Iter);
         end if;
      end loop Read_Line_From_Process;

      This.all.Text_View.all.Scroll_To_Mark (Mark          => This.all.End_Mark,
                                             Within_Margin => 0.0,
                                             Use_Align     => True,
                                             Xalign        => 1.0,
                                             Yalign        => 1.0);
   exception
      when GNAT.Expect.Process_Died =>
         null;
   end Update;

end Gtk.Frame.Log_Viewer;
