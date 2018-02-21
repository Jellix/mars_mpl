--                                                                    --
--  package Gtk.Source_Buffer       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
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

with Gtk.Source_Language;
with Gtk.Source_Mark;
with Gtk.Source_Style_Scheme;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Tag_Table;

package Gtk.Source_Buffer is

   type Gtk_Source_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record with private;
   type Gtk_Source_Buffer is access all Gtk_Source_Buffer_Record'Class;

   use type Gtk.Text_Iter.Gtk_Text_Search_Flags;

   Case_Insensitive : constant Gtk.Text_Iter.Gtk_Text_Search_Flags := 2 ** 2;

   --
   -- Backward_Iter_To_Source_Mark -- In the buffer
   --
   --    Buffer     - The buffer
   --    Iter       - To move
   --    Moved      - True if Iter was changed, otherwise  set to False
   --  [ Category ] - Bound for the search (the buffer end by default)
   --
   -- This  procedure  moves  Iter  to  the  position   of   the   previous
   -- Gtk_Source_Mark of the given category. Moved  is  True  if  Iter  was
   -- moved. If category is omitted, the previous source mark can be of any
   -- Category.
   --
   procedure Backward_Iter_To_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved    : out Boolean;
      Category : UTF8_String);

   procedure Backward_Iter_To_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved    : out Boolean);

   --
   -- Begin_Not_Undoable_Action -- Disable undo manager
   --
   --    Buffer - The buffer
   --
   -- This  procedure  marks  the beginning of a not undoable action on the
   -- buffer, disabling the undo manager. Typically  you  would  call  this
   -- function  before  initially  setting the contents of the buffer (e.g.
   -- when   loading   a   file   in   a   text   editor).   You  may  nest
   -- Begin_Not_Undoable_Action / End_Not_Undoable_Action blocks.
   --
   procedure Begin_Not_Undoable_Action
     (Buffer : not null access Gtk_Source_Buffer_Record);

   --
   -- Can_Redo -- Whether a source buffer can redo the last action
   --
   --    Buffer - The buffer
   --
   -- Returns :
   --
   --    True if a redo is possible.
   --
   function Can_Redo
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean;

   --
   -- Can_Undo -- Whether a source buffer can undo the last action
   --
   --    Buffer - The buffer
   --
   -- Returns :
   --
   --    True if a undo is possible.
   --
   function Can_Undo
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean;

   --
   -- Create_Source_Mark -- Create a source mark
   --
   --    Buffer   - The buffer
   --  [ Name ]   - The name of the mark
   --    Category - A string defining the mark category
   --    Where    - Location to place the mark
   --
   -- This  function  creates  a  source  mark in the buffer of category. A
   -- source mark  is  a  Gtk_Text_Mark,  but  organized  into  categories.
   -- Depending  on  the  category  a  pixbuf can be specified that will be
   -- displayed  along  the  line  of  the  mark.  Like  a Gtk_Text_Mark, a
   -- Gtk_Source_Mark can be anonymous. Also, the buffer owns the marks  so
   -- you shouldn't unreference it. Marks always have left gravity and  are
   -- moved  to  the  beginning  of the line when the user deletes the line
   -- they  were  in.  Typical  uses  for  a  source  mark  are  bookmarks,
   -- breakpoints, current executing instruction  indication  in  a  source
   -- file, etc.
   --
   function Create_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Name     : UTF8_String;
      Category : UTF8_String;
      Where    : Gtk.Text_Iter.Gtk_Text_Iter)
      return Gtk.Source_Mark.Gtk_Source_Mark;

   function Create_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Category : UTF8_String;
      Where    : Gtk.Text_Iter.Gtk_Text_Iter)
      return Gtk.Source_Mark.Gtk_Source_Mark;

   --
   -- End_Not_Undoable_Action -- Enable undo manager
   --
   --    Buffer - The buffer
   --
   -- This procedure marks the end of a not undoable action on the  buffer.
   -- When  the  last not undoable block is closed through the call to this
   -- procedure, the list of undo actions is cleared and the  undo  manager
   -- is re-enabled.
   --
   procedure End_Not_Undoable_Action
     (Buffer : not null access Gtk_Source_Buffer_Record);

   --
   -- Ensure_Highlight -- Force highlighting
   --
   --    Buffer - The buffer
   --    Start  - Start of the area to highlight.
   --    Stop   - End of the area to highlight.
   --
   -- This procedure forces buffer to analyze and highlight the given  area
   -- synchronously.
   --
   -- Notes :
   --
   --    This  is a potentially slow operation and should be used only when
   --    you  need  to  make  sure  that some text not currently visible is
   --    highlighted, for instance before printing.
   --
   procedure Ensure_Highlight
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Start    : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop     : Gtk.Text_Iter.Gtk_Text_Iter);

   --
   -- Forward_Iter_To_Source_Mark -- In the buffer
   --
   --    Buffer     - The buffer
   --    Iter       - To move
   --    Moved      - True if Iter was changed, otherwise  set to False
   --  [ Category ] - Bound for the search (the buffer end by default)
   --
   -- This procedure moves Iter to the position of the next Gtk_Source_Mark
   -- of  the  given category. Moved is True if Iter was moved. If category
   -- is omitted, the previous source mark can be of any category.
   --
   procedure Forward_Iter_To_Source_Mark
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved    : out Boolean;
      Category : UTF8_String);

   procedure Forward_Iter_To_Source_Mark
     (Buffer : not null access Gtk_Source_Buffer_Record;
      Iter   : in out Gtk.Text_Iter.Gtk_Text_Iter;
      Moved  : out Boolean);

   --
   -- Get_Highlight_Matching_Brackets -- Get bracket highlighting mode
   --
   --    Buffer - The buffer
   --
   -- Returns :
   --
   --    True if the source buffer will highlight matching brackets
   --
   function Get_Highlight_Matching_Brackets
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean;

   --
   -- Get_Highlight_Syntax -- Syntax highlighting activated flag
   --
   --    Buffer - The buffer
   --
   -- Returns :
   --
   --    True if syntax highlighting is activated
   --
   function Get_Highlight_Syntax
     (Buffer : not null access Gtk_Source_Buffer_Record) return Boolean;

   --
   -- Get_Language -- Get language
   --
   --    Buffer - The buffer
   --
   -- The  returned  language object is owned by the buffer. It must not be
   -- unref'ed.
   --
   -- Returns :
   --
   --    The language associated with the buffer or null
   --
   function Get_Language
     (Buffer : not null access Gtk_Source_Buffer_Record)
      return Gtk.Source_Language.Gtk_Source_Language;

   --
   -- Get_Max_Undo_Levels -- Get the number of undo levels the buffer  will
   --                        track for buffer edits
   --
   --    Buffer - The buffer
   --
   -- Returns :
   --
   --    The maximum number of possible undo levels, -1 if no limit is set
   --
   function Get_Max_Undo_Levels
     (Buffer : not null access Gtk_Source_Buffer_Record) return Gint;

   --
   -- Get_Source_Marks_At_Line -- Get source marks by line number
   --
   --    Buffer     - The buffer
   --    Line       - The line number
   --  [ Category ] - The category to search for
   --
   -- Returns :
   --
   --    The array of marks
   --
   function Get_Source_Marks_At_Line
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Line     : Gint;
      Category : UTF8_String) return Gtk.Source_Mark.Gtk_Source_Marks_Array;

   function Get_Source_Marks_At_Line
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Line     : Gint) return Gtk.Source_Mark.Gtk_Source_Marks_Array;

   --
   -- Get_Source_Marks_At_Iter -- Get source marks by iterator
   --
   --    Buffer     - The buffer
   --    Iter       - The iterator
   --  [ Category ] - The category to search for
   --
   -- Returns :
   --
   --    The array of marks
   --
   function Get_Source_Marks_At_Iter
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
      Category : UTF8_String) return Gtk.Source_Mark.Gtk_Source_Marks_Array;

   function Get_Source_Marks_At_Iter
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Iter     : Gtk.Text_Iter.Gtk_Text_Iter)
      return Gtk.Source_Mark.Gtk_Source_Marks_Array;

   --
   -- Get_Style_Scheme -- Get style scheme
   --
   --    Buffer - The buffer
   --
   -- The returned style scheme object is owned by the buffer. It must  not
   -- be unref'ed.
   --
   -- Returns :
   --
   --    The style scheme or null
   --
   function Get_Style_Scheme
     (Buffer : not null access Gtk_Source_Buffer_Record)
      return Gtk.Source_Style_Scheme.Gtk_Source_Style_Scheme;

   --
   -- Gtk_New -- Object creation
   --
   --   Buffer   - The result
   --   Language - The language used for highlighting
   --
   procedure Gtk_New
     (Buffer   : out Gtk_Source_Buffer;
      Language : not null access Gtk.Source_Language.Gtk_Source_Language_Record'Class);

   --
   -- Gtk_New -- Object creation
   --
   --   Buffer - The result
   --   Table  - The text tag table
   --
   procedure Gtk_New
     (Buffer : out Gtk_Source_Buffer;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table := null);

   --
   -- Initialize -- To be called by any derived object
   --
   --   Buffer   - The buffer object to initialize
   --   Language - The language used for highlighting
   --
   procedure Initialize
     (Buffer   : not null access Gtk_Source_Buffer_Record'Class;
      Language : not null access Gtk.Source_Language.Gtk_Source_Language_Record'Class);

   --
   -- Initialize -- To be called by any derived object
   --
   --   Buffer - The buffer object to initialize
   --   Table  - The text tag table
   --
   procedure Initialize
     (Buffer : not null access Gtk_Source_Buffer_Record'Class;
      Table  : Gtk.Text_Tag_Table.Gtk_Text_Tag_Table);

   --
   -- Redo -- The last undo operation
   --
   --    Buffer - The buffer
   --
   procedure Redo
     (Buffer : not null access Gtk_Source_Buffer_Record);

   --
   -- Remove_Source_Marks -- Remove marks
   --
   --    Buffer     - The buffer
   --    Start      - The first position where marks are to be removed
   --    Stop       - The last position
   --  [ Category ] - The category of the removed marks
   --
   -- This  procedure  removes all marks of category between Start and Stop
   -- from the buffer. If category is omitted, all marks in the range  will
   -- be removed.
   --
   procedure Remove_Source_Marks
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Start    : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop     : Gtk.Text_Iter.Gtk_Text_Iter;
      Category : UTF8_String);

   procedure Remove_Source_Marks
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Start    : Gtk.Text_Iter.Gtk_Text_Iter;
      Stop     : Gtk.Text_Iter.Gtk_Text_Iter);

   --
   -- Set_Highlight_Matching_Brackets -- Set highlighting mode
   --
   --    Buffer    - The buffer
   --    Highlight - The flag
   --
   -- This procedure controls the bracket match  highlighting  function  in
   -- the buffer. If activated,  when  you  position  your  cursor  over  a
   -- bracket  character  (a  parenthesis,  a  square  bracket,  etc.)  the
   -- matching opening or closing bracket character  will  be  highlighted.
   --
   procedure Set_Highlight_Matching_Brackets
     (Buffer    : not null access Gtk_Source_Buffer_Record;
      Highlight : Boolean);

   --
   -- Set_Highlight_Syntax -- Whether syntax is highlighted in the buffer.
   --
   --    Buffer    - The buffer
   --    Highlight - The flag
   --
   -- If Highlight is True, the text will be highlighted according  to  the
   -- syntax patterns specified in the language set with  Set_language.  If
   -- highlight is False, syntax  highlighting  is  disabled  and  all  the
   -- Gtk_Text_Tag  objects that have been added by the syntax highlighting
   -- engine are removed from the buffer.
   --
   procedure Set_Highlight_Syntax
     (Buffer    : not null access Gtk_Source_Buffer_Record;
      Highlight : Boolean);

   --
   -- Set_Language -- Set language
   --
   --    Buffer   - The buffer
   --    Language - The language or null
   --
   -- This procedure  associates  a  Gtk_Source_Language  with  the  source
   -- buffer.  If  language is null and syntax highlighting is enabled (see
   -- Set_Highlight_Syntax, the syntax patterns defined in language will be
   -- used  to  highlight  the text contained in the buffer. If language is
   -- null, the text contained in the buffer is not highlighted.
   --
   procedure Set_Language
     (Buffer   : not null access Gtk_Source_Buffer_Record;
      Language : Gtk.Source_Language.Gtk_Source_Language);

   --
   -- Set_Max_Undo_Levels -- Set number of undo levels to track
   --
   --    Buffer          - The buffer
   --    Max_Undo_Levels - The maximum number of possible undo levels
   --
   -- The value -1 of Max_Undo_Levels indicates no limit is set.
   --
   procedure Set_Max_Undo_Levels
     (Buffer          : not null access Gtk_Source_Buffer_Record;
      Max_Undo_Levels : Gint := -1);

   --
   -- Set_Style_Scheme -- Sets style scheme used by the buffer
   --
   --    Buffer - The buffer
   --    Scheme - The style scheme to set
   --
   procedure Set_Style_Scheme
     (Buffer : not null access Gtk_Source_Buffer_Record;
      Scheme : not null access Gtk.Source_Style_Scheme.Gtk_Source_Style_Scheme_Record'Class);

   --
   -- Undo -- The last user action which modified the buffer
   --
   --    Buffer - The buffer
   --
   -- Actions  are  defined  as  groups  of  operations  between  a call to
   -- Begin_User_Action  and End_User_Action, or sequences of similar edits
   -- (inserts or deletes) on the same line.
   --
   procedure Undo (Buffer : not null access Gtk_Source_Buffer_Record);

private

   type Gtk_Source_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record with null record;

end Gtk.Source_Buffer;
