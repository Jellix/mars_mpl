with GNAT.Expect;
with GNAT.Regpat;
with Gtk.Text_Buffer;
with Gtk.Text_Mark;
with Gtk.Text_View;

package Gtk.Frame.Log_Viewer is

   type Gtk_Frame_Log_Viewer_Record is new Gtk.Frame.Gtk_Frame_Record with
     private;

   type Gtk_Frame_Log_Viewer is access all Gtk_Frame_Log_Viewer_Record;

   function Gtk_Frame_Log_Viewer_New
     (Label   : in String;
      Process : in GNAT.Expect.Process_Descriptor_Access) return not null access
     Gtk_Frame_Log_Viewer_Record'Class;

   procedure Initialize (This    : not null access Gtk_Frame_Log_Viewer_Record;
                         Label   : in String;
                         Process : in GNAT.Expect.Process_Descriptor_Access);

   not overriding
   procedure Update (This : not null access Gtk_Frame_Log_Viewer_Record);

private

   New_Line : constant GNAT.Regpat.Pattern_Matcher :=
                GNAT.Regpat.Compile (Expression => ".*\n");

   type Gtk_Frame_Log_Viewer_Record is new Gtk.Frame.Gtk_Frame_Record with
      record
         Process     : GNAT.Expect.Process_Descriptor_Access;
         Text_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Text_View   : Gtk.Text_View.Gtk_Text_View;
         End_Mark    : Gtk.Text_Mark.Gtk_Text_Mark;
      end record;

end Gtk.Frame.Log_Viewer;
