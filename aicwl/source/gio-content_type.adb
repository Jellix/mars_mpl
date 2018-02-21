--                                                                    --
--  package GIO.Content_Type        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Summer, 2010       --
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
with System;

package body GIO.Content_Type is

   procedure Free (Ptr : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, Free, "g_free");

   function Can_Be_Executable (Instance : Glib.UTF8_String) return Boolean
   is
      function Internal (Instance : Interfaces.C.char_array)
                         return Glib.Gboolean;
      pragma Import (C, Internal, "g_content_type_can_be_executable");

      use type Glib.Gboolean;
   begin
      return 0 /= Internal (Interfaces.C.To_C (Instance));
   end Can_Be_Executable;

   function Equals (Type_1, Type_2 : Glib.UTF8_String) return Boolean
   is
      function Internal (Type_1, Type_2 : Interfaces.C.char_array)
                         return Glib.Gboolean;
      pragma Import (C, Internal, "g_content_type_equals");

      use type Glib.Gboolean;
   begin
      return 0 /= Internal (Interfaces.C.To_C (Type_1),
                            Interfaces.C.To_C (Type_2));
   end Equals;

   function From_MIME_Type (MIME : Glib.UTF8_String) return Glib.UTF8_String
   is
      function Internal (Instance : Interfaces.C.char_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_content_type_from_mime_type");
      Ptr : constant Interfaces.C.Strings.chars_ptr :=
              Internal (Interfaces.C.To_C (MIME));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
            Result : constant Glib.UTF8_String :=
                       Interfaces.C.Strings.Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end From_MIME_Type;

   function Get_Description
     (Instance : Glib.UTF8_String) return Glib.UTF8_String
   is
      function Internal (Instance : Interfaces.C.char_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_content_type_get_description");
      Ptr : constant Interfaces.C.Strings.chars_ptr :=
              Internal (Interfaces.C.To_C (Instance));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
            Result : constant Glib.UTF8_String :=
                       Interfaces.C.Strings.Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end Get_Description;

   function Get_Icon (Instance : Glib.UTF8_String) return Glib.Object.GObject
   is
      function Internal (Instance : Interfaces.C.char_array)
                         return System.Address;
      pragma Import (C, Internal, "g_content_type_get_icon");
   begin
      return Glib.Object.Convert (Internal (Interfaces.C.To_C (Instance)));
   end Get_Icon;

   function Get_MIME_Type (Instance : Glib.UTF8_String) return Glib.UTF8_String
   is
      function Internal (Instance : Interfaces.C.char_array)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_content_type_get_mime_type");
      Ptr : constant Interfaces.C.Strings.chars_ptr :=
              Internal (Interfaces.C.To_C (Instance));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Ptr);
      end if;
   end Get_MIME_Type;

   function Guess (File_Name : Glib.UTF8_String) return Glib.UTF8_String
   is
      function Internal
        (File_Name : Interfaces.C.char_array;
         Data      : System.Address := System.Null_Address;
         Size      : Glib.Gsize     := 0;
         Uncertain : System.Address := System.Null_Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "g_content_type_guess");
      Ptr : constant Interfaces.C.Strings.chars_ptr :=
              Internal (Interfaces.C.To_C (File_Name));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         declare
            Result : constant Glib.UTF8_String :=
                       Interfaces.C.Strings.Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end Guess;

   function Is_A (Instance, Supertype : Glib.UTF8_String) return Boolean
   is
      function Internal (Instance, Supertype : Interfaces.C.char_array)
         return Glib.Gboolean;
      pragma Import (C, Internal, "g_content_type_is_a");

      use type Glib.Gboolean;
   begin
      return 0 /= Internal (Interfaces.C.To_C (Instance),
                            Interfaces.C.To_C (Supertype));
   end Is_A;

   function Is_Unknown (Instance : Glib.UTF8_String) return Boolean
   is
      function Internal (Instance : Interfaces.C.char_array)
                         return Glib.Gboolean;
      pragma Import (C, Internal, "g_content_type_is_unknown");

      use type Glib.Gboolean;
   begin
      return 0 /= Internal (Interfaces.C.To_C (Instance));
   end Is_Unknown;

end GIO.Content_Type;
