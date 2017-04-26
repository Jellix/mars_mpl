--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Chars_Ptr_Vectors                      Luebeck            --
--  Implementation                                 Spring, 2009       --
--                                                                    --
--                                Last revision :  22:06 23 Jul 2014  --
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

with Interfaces.C.Strings;  use Interfaces.C;

package body GLib.Chars_Ptr_Vectors is

   function Convert_And_Free (Vector : Chars_Ptr_Ptr)
      return Chars_Ptr_Array is
   begin
      if Vector = null then
         return Null_Array;
      else
         declare
            Size : constant Size_T := Size_T (Virtual_Length (Vector));
         begin
            if Size = 0 then
               Free (Vector);
               return Null_Array;
            else
               declare
                  Result : Chars_Ptr_Array (0..Size - 1);
                  Ptr    : Chars_Ptr_Ptr := Vector;
                  use Interfaces.C.Strings;
               begin
                  for Index in Result'Range loop
                     Result (Index) := New_String (Value (Ptr.all));
                     Increment (Ptr);
                  end loop;
                  Free (Vector);
                  return Result;
               end;
            end if;
         end;
      end if;
   end Convert_And_Free;

   function Convert (Vector : Chars_Ptr_Ptr) return Chars_Ptr_Array is
   begin
      if Vector = null then
         return Null_Array;
      else
         declare
            Size : constant Size_T := Size_T (Virtual_Length (Vector));
         begin
            if Size = 0 then
               return Null_Array;
            else
               declare
                  Result : Chars_Ptr_Array (0..Size - 1);
                  Ptr    : Chars_Ptr_Ptr := Vector;
--                  use Interfaces.C.Strings;
               begin
                  for Index in Result'Range loop
                     Result (Index) := Ptr.all;
                     Increment (Ptr);
                  end loop;
                  return Result;
               end;
            end if;
         end;
      end if;
   end Convert;

end GLib.Chars_Ptr_Vectors;
