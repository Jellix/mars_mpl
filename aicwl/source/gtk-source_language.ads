--                                                                    --
--  package Gtk.Source_Language     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2009       --
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
-- __________________________________________________________________ --

with Interfaces.C.Strings;

package Gtk.Source_Language is

   --
   -- Gtk_Source_Language_Record -- Syntax highlighting language
   --
   type Gtk_Source_Language_Record is
     new GObject_Record with private;

   type Gtk_Source_Language is
     access all Gtk_Source_Language_Record'Class;

   --
   -- Get_Globs -- Globs associated to this language
   --
   --    Language - The language
   --
   -- This  is  just an utility wrapper around Get_Metadata to retrieve the
   -- "globs" metadata property and split it into an array. The elements of
   -- the  result  have to be freed using Interfaces.C.Strings.Free or else
   -- Gtkada.Types.Free.
   --
   -- Returns :
   --
   --    The globs associated to this language.
   --
   function Get_Globs
     (Language : not null access Gtk_Source_Language_Record)
      return Interfaces.C.Strings.chars_ptr_array;

   --
   -- Get_Hidden -- Get hidden flag
   --
   --    Language - The language
   --
   -- Returns :
   --
   --    True if the language should be hidden from the user
   --
   function Get_Hidden
     (Language : not null access Gtk_Source_Language_Record) return Boolean;

   --
   -- Get_ID -- Get the ID of the language
   --
   --    Language - The language
   --
   -- Returns :
   --
   --    The ID
   --
   function Get_ID
     (Language : not null access Gtk_Source_Language_Record) return UTF8_String;

   --
   -- Get_Metadata -- Get metadata of the language
   --
   --    Language - The language
   --    Name     - Metadata property name
   --
   -- Returns :
   --
   --    Value of property Name stored in the metadata
   --
   function Get_Metadata
     (Language : not null access Gtk_Source_Language_Record;
      Name     : UTF8_String) return UTF8_String;

   --
   -- Get_Mime_Types -- Get MIME types
   --
   --    Language - The language
   --
   -- The mime types associated to this language. This is just  an  utility
   -- wrapper  around  Get_Metadata  to  retrieve  the "mimetypes" metadata
   -- property and  split it into an array. The elements of the result have
   -- to    be    freed    using    Interfaces.C.Strings.Free    or    else
   -- Gtkada.Types.Free.
   --
   -- Returns :
   --
   --   The mime types
   --
   function Get_Mime_Types
     (Language : not null access Gtk_Source_Language_Record)
      return Interfaces.C.Strings.chars_ptr_array;

   --
   -- Get_Name -- Get the localized name of the language
   --
   --    Language - The language
   --
   -- Returns :
   --
   --    The name
   --
   function Get_Name
     (Language : not null access Gtk_Source_Language_Record) return UTF8_String;

   --
   -- Get_Section -- Get the localized section of the language
   --
   --    Language - The language
   --
   -- This function returns the localized section of the language.
   --
   -- Returns :
   --
   --    The section
   --
   function Get_Section
     (Language : not null access Gtk_Source_Language_Record) return UTF8_String;

   --
   -- Get_Style_Fallback -- Get the localized section of the language
   --
   --    Language - The language
   --    Style    - A style ID
   --
   -- This function returns  the ID of the  style to  use  if the specified
   -- style is not present in the current style scheme.
   --
   -- Returns :
   --
   --    The style ID
   --
   function Get_Style_Fallback
     (Language : not null access Gtk_Source_Language_Record;
      Style    : UTF8_String) return UTF8_String;

   --
   -- Get_Style_IDs -- Get the ids of the styles defined by this language
   --
   --    Language - The language
   --
   -- The    elements    of    the   result   have  to   be   freed   using
   -- Interfaces.C.Strings.Free or else Gtkada.Types.Free.
   --
   -- Returns :
   --
   --    The styles
   --
   function Get_Style_IDs
     (Language : not null access Gtk_Source_Language_Record)
      return Interfaces.C.Strings.chars_ptr_array;

   --
   -- Get_Style_Name -- Get the name of the style with ID
   --
   --    Language - The language
   --    Style_ID - The style
   --
   -- Returns :
   --
   --    The style name or empty string
   --
   function Get_Style_Name
     (Language : not null access Gtk_Source_Language_Record;
      Style    : UTF8_String) return UTF8_String;

private

   type Gtk_Source_Language_Record is
     new GObject_Record with null record;

end Gtk.Source_Language;
