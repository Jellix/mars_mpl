package body Gtk.Text_Buffer.With_End_Mark is

   function End_Mark
     (Buffer : not null access Gtk_Text_Buffer_With_End_Mark_Record'Class)
      return not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class is
     (Buffer.all.End_Mark);

   function Gtk_Text_Buffer_With_End_Mark_New
     (Table : in Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null)
      return Gtk_Text_Buffer_With_End_Mark
   is
      Buffer : constant Gtk_Text_Buffer_With_End_Mark :=
                 new Gtk_Text_Buffer_With_End_Mark_Record;
   begin
      Gtk.Text_Buffer.With_End_Mark.Initialize (Buffer => Buffer,
                                                Table  => Table);
      return Buffer;
   end Gtk_Text_Buffer_With_End_Mark_New;

   procedure Initialize
     (Buffer : not null access Gtk_Text_Buffer_With_End_Mark_Record'Class;
      Table  : in              Gtk.Text_Tag_Table.Gtk_Text_Tag_Table)
   is
      End_Of_Buffer : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Gtk.Text_Buffer.Initialize (Buffer => Buffer,
                                  Table  => Table);
      Buffer.all.Get_End_Iter (Iter => End_Of_Buffer);
      Buffer.all.End_Mark :=
        Buffer.all.Create_Mark (Where        => End_Of_Buffer,
                                Left_Gravity => False);
   end Initialize;

end Gtk.Text_Buffer.With_End_Mark;
