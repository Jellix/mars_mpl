--                                                                    --
--  package Gtk.Source_Language     Copyright (c)  Dmitry A. Kazakov  --
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
--____________________________________________________________________--

with Interfaces.C;  use Interfaces.C;
with System;        use System;

with GLib.Chars_Ptr_Vectors;
with GtkAda.Types;

package body Gtk.Source_Language is

   function Get_Globs
            (  Language : not null access Gtk_Source_Language_Record
            )  return Chars_Ptr_Array is
      use GLib.Chars_Ptr_Vectors;
      function Internal (Object : Address) return Chars_Ptr_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_globs");
   begin
      return Convert_And_Free (Internal (Get_Object (Language)));
   end Get_Globs;

   function Get_Hidden
            (  Language : not null access Gtk_Source_Language_Record
            )  return Boolean is
      function Internal (Object : Address) return GBoolean;
      pragma Import (C, Internal, "gtk_source_language_get_hidden");
   begin
      return Internal (Get_Object (Language)) /= 0;
   end Get_Hidden;

   function Get_ID
            (  Language : not null access Gtk_Source_Language_Record
            )  return UTF8_String is
      function Internal (Object : Address) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_id");
      Result : constant Chars_Ptr := Internal (Get_Object (Language));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_ID;

   function Get_Metadata
            (  Language : not null access Gtk_Source_Language_Record;
               Name     : UTF8_String
            )  return UTF8_String is
      function Internal
               (  Object : Address;
                  Name   : Char_Array
               )  return Chars_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_metadata");
   begin
      return Value (Internal (Get_Object (Language), To_C (Name)));
   end Get_Metadata;

   function Get_Mime_Types
            (  Language : not null access Gtk_Source_Language_Record
            )  return Chars_Ptr_Array is
      use GLib.Chars_Ptr_Vectors;
      function Internal (Object : Address) return Chars_Ptr_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_mime_types");
   begin
      return Convert_And_Free (Internal (Get_Object (Language)));
   end Get_Mime_Types;

   function Get_Name
            (  Language : not null access Gtk_Source_Language_Record
            )  return UTF8_String is
      function Internal (Object : Address) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_name");
      Result : constant Chars_Ptr := Internal (Get_Object (Language));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Name;

   function Get_Section
            (  Language : not null access Gtk_Source_Language_Record
            )  return UTF8_String is
      function Internal (Object : Address) return Chars_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_section");
      Result : constant Chars_Ptr := Internal (Get_Object (Language));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Section;

   function Get_Style_Fallback
            (  Language : not null access Gtk_Source_Language_Record;
               Style    : UTF8_String
            )  return UTF8_String is
      function Internal
               (  Object : Address;
                  Style  : Char_Array
               )  return Chars_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_language_get_style_fallback"
             );
      Result : constant Chars_Ptr :=
                  Internal (Get_Object (Language), To_C (Style));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Style_Fallback;

   function Get_Style_IDs
            (  Language : not null access Gtk_Source_Language_Record
            )  return Chars_Ptr_Array is
      use GLib.Chars_Ptr_Vectors;
      function Internal (Object : Address) return Chars_Ptr_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_style_ids");
   begin
      return Convert_And_Free (Internal (Get_Object (Language)));
   end Get_Style_IDs;

   function Get_Style_Name
            (  Language : not null access Gtk_Source_Language_Record;
               Style    : UTF8_String
            )  return UTF8_String is
      function Internal
               (  Language : Address;
                  Style    : Char_Array
               )  return Chars_Ptr;
      pragma Import (C, Internal, "gtk_source_language_get_style_name");
      Result : constant Chars_Ptr :=
                  Internal (Get_Object (Language), To_C (Style));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Style_Name;

end Gtk.Source_Language;
