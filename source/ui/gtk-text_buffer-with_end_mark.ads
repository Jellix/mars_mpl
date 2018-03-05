package Gtk.Text_Buffer.With_End_Mark is

   type Gtk_Text_Buffer_With_End_Mark_Record is
     new Gtk_Text_Buffer_Record with private;

   type Gtk_Text_Buffer_With_End_Mark is
     access all Gtk_Text_Buffer_With_End_Mark_Record'Class;

   function Gtk_Text_Buffer_With_End_Mark_New
     (Table : in Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
      return Gtk_Text_Buffer_With_End_Mark;

   function End_Mark
     (Buffer : not null access Gtk_Text_Buffer_With_End_Mark_Record'Class)
      return not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;

private

   procedure Initialize
     (Buffer : not null access Gtk_Text_Buffer_With_End_Mark_Record'Class;
      Table  : in              Gtk.Text_Tag_Table.Gtk_Text_Tag_Table);

   type Gtk_Text_Buffer_With_End_Mark_Record is
     new Gtk_Text_Buffer_Record with
      record
         End_Mark : Text_Mark.Gtk_Text_Mark;
      end record;

end Gtk.Text_Buffer.With_End_Mark;
