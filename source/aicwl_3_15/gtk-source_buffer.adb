--                                                                    --
--  package Gtk.Source_Buffer       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2009       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

with Ada.Unchecked_Deallocation;

with Interfaces.C;

with System;

package body Gtk.Source_Buffer is

   pragma Warnings (Off, "declaration hides ""Buffer""");
   pragma Warnings (Off, "declaration hides ""Category""");
   pragma Warnings (Off, "declaration hides ""Highlight""");
   pragma Warnings (Off, "declaration hides ""Iter""");
   pragma Warnings (Off, "declaration hides ""Language""");
   pragma Warnings (Off, "declaration hides ""Line""");
   pragma Warnings (Off, "declaration hides ""Max_Undo_Levels""");
   pragma Warnings (Off, "declaration hides ""Name""");
   pragma Warnings (Off, "declaration hides ""Scheme""");
   pragma Warnings (Off, "declaration hides ""Start""");
   pragma Warnings (Off, "declaration hides ""Stop""");
   pragma Warnings (Off, "declaration hides ""Table""");
   pragma Warnings (Off, "declaration hides ""Where""");

   procedure Backward_Iter_To_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved    : out Boolean;
      Category : UTF8_String)
   is
      function Internal
        (Buffer   : System.Address;
         Iter     : System.Address;
         Category : Interfaces.C.char_array) return Gboolean;
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_backward_iter_to_source_mark");
   begin
      Moved :=
        0 /=
          Internal
            (Get_Object (Buffer),
             Iter'Address,
             Interfaces.C.To_C (Category));
   end Backward_Iter_To_Source_Mark;

   procedure Backward_Iter_To_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved    : out Boolean)
   is
      function Internal
        (Buffer   : System.Address;
         Iter     : System.Address;
         Category : System.Address := System.Null_Address) return Gboolean;
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_backward_iter_to_source_mark");
   begin
      Moved := 0 /= Internal (Get_Object (Buffer), Iter'Address);
   end Backward_Iter_To_Source_Mark;

   procedure Begin_Not_Undoable_Action
     (Buffer : not null access Gtk_Source_Buffer_Record)
   is
      procedure Internal (Buffer : System.Address);
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_begin_not_undoable_action");
   begin
      Internal (Get_Object (Buffer));
   end Begin_Not_Undoable_Action;

   function Can_Redo
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_source_buffer_can_redo");
   begin
      return 0 /= Internal (Get_Object (Buffer));
   end Can_Redo;

   function Can_Undo
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_source_buffer_can_undo");
   begin
      return 0 /= Internal (Get_Object (Buffer));
   end Can_Undo;

   function Convert_And_Release (Pointer : System.Address)
                                 return Gtk.Source_Mark.Gtk_Source_Marks_Array;
   function Convert_And_Release (Pointer : System.Address)
                                 return Gtk.Source_Mark.Gtk_Source_Marks_Array
   is
      List : Gtk.Source_Mark.Marks_List.GSlist;

      use type System.Address;
   begin
      if Pointer = System.Null_Address then
         return (1 .. 0 => null);
      end if;
      Gtk.Source_Mark.Marks_List.Set_Object (List, Pointer);
      declare
         Result :
         Gtk.Source_Mark.Gtk_Source_Marks_Array
           (1 .. Natural (Gtk.Source_Mark.Marks_List.Length (List)));
         This   : Gtk.Source_Mark.Marks_List.GSlist := List;
      begin
         for Index in Result'Range loop
            Result (Index) := Gtk.Source_Mark.Marks_List.Get_Data (This);
            This := Gtk.Source_Mark.Marks_List.Next (This);
         end loop;
         Gtk.Source_Mark.Marks_List.Free (List);
         return Result;
      end;
   end Convert_And_Release;

   function Create_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Name     : UTF8_String;
      Category : UTF8_String;
      Where    : Gtk.Text_Iter.Gtk_Text_Iter)
      return Gtk.Source_Mark.Gtk_Source_Mark
   is
      function Internal
        (Buffer   : System.Address;
         Name     : Interfaces.C.char_array;
         Category : Interfaces.C.char_array;
         Where    : Gtk.Text_Iter.Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_create_source_mark");
      Stub : Gtk.Source_Mark.Gtk_Source_Mark_Record;
   begin
      return
        Gtk.Source_Mark.Gtk_Source_Mark
          (Get_User_Data_Fast
             (Internal
                (Get_Object (Buffer),
                 Interfaces.C.To_C (Name),
                 Interfaces.C.To_C (Category),
                 Where),
              Stub));
   end Create_Source_Mark;

   function Create_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Category : UTF8_String;
      Where    : Gtk.Text_Iter.Gtk_Text_Iter)
      return Gtk.Source_Mark.Gtk_Source_Mark
   is
      function Internal
        (Buffer   : System.Address;
         Name     : System.Address;
         Category : Interfaces.C.char_array;
         Where    : Gtk.Text_Iter.Gtk_Text_Iter) return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_create_source_mark");
      Stub : Gtk.Source_Mark.Gtk_Source_Mark_Record;
   begin
      return
        Gtk.Source_Mark.Gtk_Source_Mark
          (Get_User_Data_Fast
             (Internal
                (Get_Object (Buffer),
                 System.Null_Address,
                 Interfaces.C.To_C (Category),
                 Where),
              Stub));
   end Create_Source_Mark;

   procedure End_Not_Undoable_Action
     (Buffer : not null access Gtk_Source_Buffer_Record)
   is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_source_buffer_end_not_undoable_action");
   begin
      Internal (Get_Object (Buffer));
   end End_Not_Undoable_Action;

   procedure Ensure_Highlight
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Start    : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop     : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer      : System.Address;
         Start, Stop : Gtk.Text_Iter.Gtk_Text_Iter);
      pragma Import (C, Internal, "gtk_source_buffer_ensure_highlight");
   begin
      Internal (Get_Object (Buffer), Start, Stop);
   end Ensure_Highlight;

   procedure Forward_Iter_To_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved    : out Boolean;
      Category : UTF8_String)
   is
      function Internal
        (Buffer   : System.Address;
         Iter     : System.Address;
         Category : Interfaces.C.char_array) return Gboolean;
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_forward_iter_to_source_mark");
   begin
      Moved :=
        0 /=
          Internal
            (Get_Object (Buffer),
             Iter'Address,
             Interfaces.C.To_C (Category));
   end Forward_Iter_To_Source_Mark;

   procedure Forward_Iter_To_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved    : out Boolean)
   is
      function Internal
        (Buffer   : System.Address;
         Iter     : System.Address;
         Category : System.Address := System.Null_Address) return Gboolean;
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_forward_iter_to_source_mark");
   begin
      Moved := 0 /= Internal (Get_Object (Buffer), Iter'Address);
   end Forward_Iter_To_Source_Mark;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Gtk_Source_Buffer_Record'Class,
        Gtk_Source_Buffer);

   function Get_Highlight_Matching_Brackets
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Gboolean;
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_get_highlight_matching_brackets");
   begin
      return 0 /= Internal (Get_Object (Buffer));
   end Get_Highlight_Matching_Brackets;

   function Get_Highlight_Syntax
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean
   is
      function Internal (Buffer : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_source_buffer_get_highlight_syntax");
   begin
      return 0 /= Internal (Get_Object (Buffer));
   end Get_Highlight_Syntax;

   function Get_Language
     (Buffer : not null access Gtk_Source_Buffer_Record)
      return Gtk.Source_Language.Gtk_Source_Language
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_get_language");
      Stub : Gtk.Source_Language.Gtk_Source_Language_Record;
   begin
      return
        Gtk.Source_Language.Gtk_Source_Language
          (Get_User_Data_Fast
             (Internal (Get_Object (Buffer)),
              Stub));
   end Get_Language;

   function Get_Max_Undo_Levels
     (Buffer : not null access Gtk_Source_Buffer_Record) return Gint
   is
      function Internal (Buffer : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_source_buffer_get_max_undo_levels");
   begin
      return Internal (Get_Object (Buffer));
   end Get_Max_Undo_Levels;

   function Get_Source_Marks_At_Iter
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
      Category : UTF8_String) return Gtk.Source_Mark.Gtk_Source_Marks_Array
   is
      function Internal
        (Buffer   : System.Address;
         Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
         Category : Interfaces.C.char_array) return System.Address;
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_get_source_marks_at_iter");
   begin
      return
        Convert_And_Release
          (Internal (Get_Object (Buffer), Iter, Interfaces.C.To_C (Category)));
   end Get_Source_Marks_At_Iter;

   function Get_Source_Marks_At_Iter
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : Gtk.Text_Iter.Gtk_Text_Iter)
      return Gtk.Source_Mark.Gtk_Source_Marks_Array
   is
      function Internal
        (Buffer   : System.Address;
         Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
         Category : System.Address := System.Null_Address)
         return System.Address;
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_get_source_marks_at_iter");
   begin
      return Convert_And_Release (Internal (Get_Object (Buffer), Iter));
   end Get_Source_Marks_At_Iter;

   function Get_Source_Marks_At_Line
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Line     : Gint;
      Category : UTF8_String) return Gtk.Source_Mark.Gtk_Source_Marks_Array
   is
      function Internal
        (Buffer   : System.Address;
         Line     : Gint;
         Category : Interfaces.C.char_array) return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_get_source_marks_at_line");
   begin
      return
        Convert_And_Release
          (Internal (Get_Object (Buffer), Line, Interfaces.C.To_C (Category)));
   end Get_Source_Marks_At_Line;

   function Get_Source_Marks_At_Line
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Line     : Gint) return Gtk.Source_Mark.Gtk_Source_Marks_Array
   is
      function Internal
        (Buffer   : System.Address;
         Line     : Gint;
         Category : System.Address := System.Null_Address)
         return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_get_source_marks_at_line");
   begin
      return Convert_And_Release (Internal (Get_Object (Buffer), Line));
   end Get_Source_Marks_At_Line;

   function Get_Style_Scheme
     (Buffer : not null access Gtk_Source_Buffer_Record)
      return Gtk.Source_Style_Scheme.Gtk_Source_Style_Scheme
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_get_style_scheme");
      Stub : Gtk.Source_Style_Scheme.Gtk_Source_Style_Scheme_Record;
   begin
      return
        Gtk.Source_Style_Scheme.Gtk_Source_Style_Scheme
          (Get_User_Data_Fast
             (Internal (Get_Object (Buffer)),
              Stub));
   end Get_Style_Scheme;

   procedure Gtk_New
     (Buffer   : out Gtk_Source_Buffer;
      Language : not null access Gtk.Source_Language.Gtk_Source_Language_Record'Class) is
   begin
      Buffer := new Gtk_Source_Buffer_Record;
      Initialize (Buffer, Language);
   exception
      when others =>
         Free (Buffer);
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Buffer : out Gtk_Source_Buffer;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null) is
   begin
      Buffer := new Gtk_Source_Buffer_Record;
      Gtk.Source_Buffer.Initialize (Buffer, Table);
   exception
      when others =>
         Free (Buffer);
         raise;
   end Gtk_New;

   procedure Initialize
     (Buffer   : not null access Gtk_Source_Buffer_Record'Class;
      Language : not null access Gtk.Source_Language.Gtk_Source_Language_Record'Class)
   is
      function Internal (Language : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_new_with_language");
   begin
      Set_Object (Buffer, Internal (Get_Object (Language)));
   end Initialize;

   procedure Initialize
     (Buffer : not null access Gtk_Source_Buffer_Record'Class;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table)
   is
      function Internal (Table : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_source_buffer_new");

      use type Gtk.Text_Tag_Table.Gtk_Text_Tag_Table;
   begin
      if Table = null then
         Set_Object (Buffer, Internal (System.Null_Address));
      else
         Set_Object (Buffer, Internal (Get_Object (Table)));
      end if;
   end Initialize;

   procedure Redo (Buffer : not null access Gtk_Source_Buffer_Record) is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_source_buffer_redo");
   begin
      Internal (Get_Object (Buffer));
   end Redo;

   procedure Remove_Source_Marks
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Start    : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop     : Gtk.Text_Iter.Gtk_Text_Iter;
      Category : UTF8_String)
   is
      procedure Internal
        (Buffer   : System.Address;
         Start    : Gtk.Text_Iter.Gtk_Text_Iter;
         Stop     : Gtk.Text_Iter.Gtk_Text_Iter;
         Category : Interfaces.C.char_array);
      pragma Import (C, Internal, "gtk_source_buffer_remove_source_marks");
   begin
      Internal (Get_Object (Buffer), Start, Stop, Interfaces.C.To_C (Category));
   end Remove_Source_Marks;

   procedure Remove_Source_Marks
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Start    : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop     : Gtk.Text_Iter.Gtk_Text_Iter)
   is
      procedure Internal
        (Buffer   : System.Address;
         Start    : Gtk.Text_Iter.Gtk_Text_Iter;
         Stop     : Gtk.Text_Iter.Gtk_Text_Iter;
         Category : System.Address := System.Null_Address);
      pragma Import (C, Internal, "gtk_source_buffer_remove_source_marks");
   begin
      Internal (Get_Object (Buffer), Start, Stop);
   end Remove_Source_Marks;

   procedure Set_Highlight_Matching_Brackets
     (Buffer    : not null access Gtk_Source_Buffer_Record;
      Highlight : Boolean)
   is
      procedure Internal (Buffer : System.Address; Highlight : Gboolean);
      pragma Import (C,
                     Internal,
                     "gtk_source_buffer_set_highlight_matching_brackets");
   begin
      if Highlight then
         Internal (Get_Object (Buffer), 1);
      else
         Internal (Get_Object (Buffer), 0);
      end if;
   end Set_Highlight_Matching_Brackets;

   procedure Set_Highlight_Syntax
     (Buffer    : not null access Gtk_Source_Buffer_Record;
      Highlight : Boolean)
   is
      procedure Internal (Buffer : System.Address; Highlight : Gboolean);
      pragma Import (C, Internal, "gtk_source_buffer_set_highlight_syntax");
   begin
      if Highlight then
         Internal (Get_Object (Buffer), 1);
      else
         Internal (Get_Object (Buffer), 0);
      end if;
   end Set_Highlight_Syntax;

   procedure Set_Language
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Language : Gtk.Source_Language.Gtk_Source_Language)
   is
      procedure Internal (Buffer, Language : System.Address);
      pragma Import (C, Internal, "gtk_source_buffer_set_language");

      use type Gtk.Source_Language.Gtk_Source_Language;
   begin
      if Language = null then
         Internal (Get_Object (Buffer), System.Null_Address);
      else
         Internal (Get_Object (Buffer), Get_Object (Language));
      end if;
   end Set_Language;

   procedure Set_Max_Undo_Levels
     (Buffer : not null access Gtk_Source_Buffer_Record;
      Max_Undo_Levels : Gint := -1)
   is
      procedure Internal (Buffer : System.Address; Max_Undo_Levels : Gint);
      pragma Import (C, Internal, "gtk_source_buffer_set_max_undo_levels");
   begin
      Internal (Get_Object (Buffer), Max_Undo_Levels);
   end Set_Max_Undo_Levels;

   procedure Set_Style_Scheme
     (Buffer : not null access Gtk_Source_Buffer_Record;
      Scheme : not null access Gtk.Source_Style_Scheme.Gtk_Source_Style_Scheme_Record'Class)
   is
      procedure Internal (Buffer, Scheme : System.Address);
      pragma Import (C, Internal, "gtk_source_buffer_set_style_scheme");
   begin
      Internal (Get_Object (Buffer), Get_Object (Scheme));
   end Set_Style_Scheme;

   procedure Undo (Buffer : not null access Gtk_Source_Buffer_Record) is
      procedure Internal (Buffer : System.Address);
      pragma Import (C, Internal, "gtk_source_buffer_undo");
   begin
      Internal (Get_Object (Buffer));
   end Undo;

   pragma Warnings (On, "declaration hides ""Buffer""");
   pragma Warnings (On, "declaration hides ""Category""");
   pragma Warnings (On, "declaration hides ""Highlight""");
   pragma Warnings (On, "declaration hides ""Iter""");
   pragma Warnings (On, "declaration hides ""Language""");
   pragma Warnings (On, "declaration hides ""Line""");
   pragma Warnings (On, "declaration hides ""Max_Undo_Levels""");
   pragma Warnings (On, "declaration hides ""Name""");
   pragma Warnings (On, "declaration hides ""Scheme""");
   pragma Warnings (On, "declaration hides ""Start""");
   pragma Warnings (On, "declaration hides ""Stop""");
   pragma Warnings (On, "declaration hides ""Table""");
   pragma Warnings (On, "declaration hides ""Where""");

end Gtk.Source_Buffer;
