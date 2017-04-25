--                                                                    --
--  package Gtk.Source_Mark         Copyright (c)  Dmitry A. Kazakov  --
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
--____________________________________________________________________--

with Gtk.Text_Mark;  use Gtk.Text_Mark;
with System;         use System;

with GLib.GSlist;

package Gtk.Source_Mark is
--
-- Gtk_Source_Mark_Record -- Mark object for Gtk_Source_Buffer_Record
--
   type Gtk_Source_Mark_Record is
      new Gtk_Text_Mark_Record with private;
   type Gtk_Source_Mark is access all Gtk_Source_Mark_Record'Class;
--
-- Gtk_Source_Marks_Array -- Array of marks
--
   type Gtk_Source_Marks_Array is
      array (Positive range <>) of Gtk_Source_Mark;
--
-- Get_Category -- Of a mark
--
--    Mark - The source mark
--
-- Returns :
--
--    The category of the mark
--
   function Get_Category
            (  Mark : not null access Gtk_Source_Mark_Record
            )  return UTF8_String;
--
-- Gtk_New -- Object creation
--
--    Mark     - The result
--  [ Name ]   - Of the mark
--    Category - Of the mark
--
   procedure Gtk_New
             (  Mark     : out Gtk_Source_Mark;
                Name     : UTF8_String;
                Category : UTF8_String
             );
   procedure Gtk_New
             (  Mark     : out Gtk_Source_Mark;
                Category : UTF8_String
             );
--
-- Initialize -- To be called by any derived object
--
--    Mark     - The source mark
--  [ Name ]   - Of the mark
--    Category - Of the mark
--
   procedure Initialize
             (  Mark     : not null access Gtk_Source_Mark_Record'Class;
                Name     : UTF8_String;
                Category : UTF8_String
             );
   procedure Initialize
             (  Mark     : not null access Gtk_Source_Mark_Record'Class;
                Category : UTF8_String
             );
--
-- Next -- Next mark in the buffer
--
--    Mark       - The source mark
--  [ Category ] - To look after
--
-- This function returns the next Gtk_Source_Mark in the buffer or  null
-- if the mark was not added to a buffer. If there is no next mark, null
-- will  be  returned. If category is omitted, it looks for marks of any
-- category.
--
-- Returns :
--
--    Next mark or null
--
   function Next
            (  Mark     : not null access Gtk_Source_Mark_Record;
               Category : UTF8_String
            )  return Gtk_Source_Mark;
   function Next
            (  Mark : not null access Gtk_Source_Mark_Record
            )  return Gtk_Source_Mark;
--
-- Prev -- Previous mark in the buffer
--
--    Mark       - The source mark
--  [ Category ] - To look after
--
-- This function returns the previous Gtk_Source_Mark in the  buffer  or
-- null  if  the mark was not added to a buffer. If there is no previous
-- mark, null will be returned. If category is  omitted,  it  looks  for
-- marks of any category.
--
-- Returns :
--
--    Previous mark or null
--
   function Prev
            (  Mark     : not null access Gtk_Source_Mark_Record;
               Category : UTF8_String
            )  return Gtk_Source_Mark;
   function Prev
            (  Mark : not null access Gtk_Source_Mark_Record
            )  return Gtk_Source_Mark;
--
-- Internally  lists  of  marks.  The  binding  procedures  consequently
-- convert lists of marks to Ada arrays
--
   function Convert (Mark : Address) return Gtk_Source_Mark;
   function Convert (Mark : Gtk_Source_Mark) return Address;

   package Marks_List is
      new Glib.GSlist.Generic_SList (Gtk_Source_Mark);

private
   type Gtk_Source_Mark_Record is
      new Gtk_Text_Mark_Record with null record;

end Gtk.Source_Mark;
