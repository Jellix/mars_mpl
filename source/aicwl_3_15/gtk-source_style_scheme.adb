--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_Style_Scheme                     Luebeck            --
--  Implementation                                 Summer, 2009       --
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

with System;  use System;

with Glib.Chars_Ptr_Vectors;
with Gtkada.Types;

package body Gtk.Source_Style_Scheme is

   function Get_Authors
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return chars_ptr_array is
      use Glib.Chars_Ptr_Vectors;
      function Internal (Object : Address) return Chars_Ptr_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_style_scheme_get_authors"
             );
   begin
      return Convert (Internal (Get_Object (Scheme)));
   end Get_Authors;

   function Get_Description
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String is
      function Internal (Object : Address) return chars_ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_style_scheme_get_description"
             );
      Result : constant chars_ptr := Internal (Get_Object (Scheme));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Description;

   function Get_Filename
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String is
      function Internal (Object : Address) return chars_ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_style_scheme_get_filename"
             );
      Result : constant chars_ptr := Internal (Get_Object (Scheme));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Filename;

   function Get_ID
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String is
      function Internal (Object : Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_source_style_scheme_get_id");
      Result : constant chars_ptr := Internal (Get_Object (Scheme));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_ID;

   function Get_Name
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record
            )  return UTF8_String is
      function Internal (Object : Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_source_style_scheme_get_name");
      Result : constant chars_ptr := Internal (Get_Object (Scheme));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Name;

   function Get_Style
            (  Scheme : not null access Gtk_Source_Style_Scheme_Record;
               Style  : UTF8_String
            )  return Gtk_Source_Style is
      use Interfaces.C;
      function Internal (Object : Address; Style : char_array)
         return Address;
      pragma Import (C, Internal, "gtk_source_style_scheme_get_style");
      Stub : Gtk_Source_Style_Record;
   begin
      return
         Gtk_Source_Style
         (  Get_User_Data_Fast
            (  Internal (Get_Object (Scheme), To_C (Style)),
               Stub
         )  );
   end Get_Style;

end Gtk.Source_Style_Scheme;
