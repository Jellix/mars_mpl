--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_Style_Scheme                     Luebeck            --
--  Interface                                      Summer, 2009       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Gtk.Source_Style;      use Gtk.Source_Style;

with Interfaces.C.Pointers;

package Gtk.Source_Style_Scheme is
--
-- Gtk_Source_Style_Scheme_Record -- Object  controlling   apperance  of
--                                   Gtk_Source_View
--
   type Gtk_Source_Style_Scheme_Record is
      new GObject_Record with private;
   type Gtk_Source_Style_Scheme is
      access all Gtk_Source_Style_Scheme_Record'Class;
--
-- Get_Authors -- Get authors of the style scheme
--
--    Scheme - The style scheme
--
-- Elements of the result are owned by the style scheme. They shall  not
-- modified or freed.
--
-- Returns :
--
--    Array of C-strings
--
   function Get_Authors
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return Chars_Ptr_Array;
--
-- Get_Description -- Get description of the style scheme
--
--    Scheme - The style scheme
--
-- Returns :
--
--    Style scheme description or empty string if none
--
   function Get_Description
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String;
--
-- Get_Filename -- Get the file name parsed to create the scheme
--
--    Scheme - The style scheme
--
-- Returns :
--
--    The file name or empty string if none
--
   function Get_Filename
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String;
--
-- Get_ID -- Get identifier of the style scheme
--
--    Scheme - The style scheme
--
-- Returns :
--
--    Style scheme identifier
--
   function Get_ID
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String;
--
-- Get_Name -- Get name of the style scheme
--
--    Scheme - The style scheme
--
-- Returns :
--
--    The name
--
   function Get_Name
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String;
--
-- Get_Style -- Get source style by identifier
--
--    Scheme - The style scheme
--    Style  - ID of the style to retrieve
--
-- The result is owned by scheme and may not be unref'ed.
--
-- Returns :
--
--    The style corresponding to Style or null
--
   function Get_Style
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record;
               Style  : UTF8_String
            )  return Gtk_Source_Style;

private
   type Gtk_Source_Style_Scheme_Record is
      new GObject_Record with null record;

end Gtk.Source_Style_Scheme;
