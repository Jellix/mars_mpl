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
--____________________________________________________________________--

with Interfaces.C;          use Interfaces.C;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with System;                use System;

package body GIO.Content_Type is

   procedure Free (Ptr : Chars_Ptr);
   pragma Import (C, Free, "g_free");

   function Can_Be_Executable (Instance : UTF8_String) return Boolean is
      function Internal (Instance : Char_Array) return GBoolean;
      pragma Import (C, Internal, "g_content_type_can_be_executable");
   begin
      return 0 /= Internal (To_C (Instance));
   end Can_Be_Executable;

   function Equals (Type_1, Type_2 : UTF8_String) return Boolean is
      function Internal (Type_1, Type_2 : Char_Array) return GBoolean;
      pragma Import (C, Internal, "g_content_type_equals");
   begin
      return 0 /= Internal (To_C (Type_1), To_C (Type_2));
   end Equals;

   function From_MIME_Type (MIME : UTF8_String) return UTF8_String is
      function Internal (Instance : Char_Array) return Chars_Ptr;
      pragma Import (C, Internal, "g_content_type_from_mime_type");
      Ptr : constant Chars_Ptr := Internal (To_C (MIME));
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         declare
            Result : constant UTF8_String := Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end From_MIME_Type;

   function Get_Description (Instance : UTF8_String)
      return UTF8_String is
      function Internal (Instance : Char_Array) return Chars_Ptr;
      pragma Import (C, Internal, "g_content_type_get_description");
      Ptr : constant Chars_Ptr := Internal (To_C (Instance));
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         declare
            Result : constant UTF8_String := Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end Get_Description;

   function Get_Icon (Instance : UTF8_String) return GObject is
      function Internal (Instance : Char_Array) return Address;
      pragma Import (C, Internal, "g_content_type_get_icon");
   begin
      return Convert (Internal (To_C (Instance)));
   end Get_Icon;

   function Get_MIME_Type (Instance : UTF8_String) return UTF8_String is
      function Internal (Instance : Char_Array) return Chars_Ptr;
      pragma Import (C, Internal, "g_content_type_get_mime_type");
      Ptr : constant Chars_Ptr := Internal (To_C (Instance));
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         return Value (Ptr);
      end if;
   end Get_MIME_Type;

   function Guess (File_Name : UTF8_String) return UTF8_String is
      function Internal
               (  File_Name : Char_Array;
                  Data      : Address := Null_Address;
                  Size      : GSize   := 0;
                  Uncertain : Address := Null_Address
               )  return Chars_Ptr;
      pragma Import (C, Internal, "g_content_type_guess");
      Ptr : constant Chars_Ptr := Internal (To_C (File_Name));
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         declare
            Result : constant UTF8_String := Value (Ptr);
         begin
            Free (Ptr);
            return Result;
         end;
      end if;
   end Guess;

   function Is_A (Instance, Supertype : UTF8_String) return Boolean is
      function Internal (Instance, Supertype : Char_Array)
         return GBoolean;
      pragma Import (C, Internal, "g_content_type_is_a");
   begin
      return 0 /= Internal (To_C (Instance), To_C (Supertype));
   end Is_A;

   function Is_Unknown (Instance : UTF8_String) return Boolean is
      function Internal (Instance : Char_Array) return GBoolean;
      pragma Import (C, Internal, "g_content_type_is_unknown");
   begin
      return 0 /= Internal (To_C (Instance));
   end Is_Unknown;

end GIO.Content_Type;
