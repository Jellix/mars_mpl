--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Chars_Ptr_Vectors                      Luebeck            --
--  Interface                                      Spring, 2009       --
--                                                                    --
--                                Last revision :  09:26 07 Aug 2009  --
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

with Gtkada.Types;

with Interfaces.C.Pointers;

package Glib.Chars_Ptr_Vectors is

   type Chars_Ptr_Vector is
     array (Interfaces.C.size_t range <>) of aliased Gtkada.Types.Chars_Ptr;

   package Chars_Ptr_Pointers is
     new Interfaces.C.Pointers
       (Interfaces.C.Size_t,
        Gtkada.Types.Chars_Ptr,
        Chars_Ptr_Vector,
        null);
   type Chars_Ptr_Ptr is new Chars_Ptr_Pointers.Pointer;

   --
   -- Convert_And_Free -- Conversion of  an object of gchar **  as returned
   --                     from some subprograms
   --
   --    Vector - Array of C-strings allocated by GTK+
   --
   -- This function creates an equivalent Chars_Ptr_Array  and  then  frees
   -- the  argument  using  g_strfreev.  The  result   does   not   contain
   -- terminating NUL. The caller is responsible to free array elements.
   --
   -- Returns :
   --
   --    An equivalent of Chars_Ptr_Array type
   --
   function Convert_And_Free (Vector : Chars_Ptr_Ptr)
                              return Gtkada.Types.Chars_Ptr_Array;

   --
   -- Convert -- Conversion of an object of const gchar * const *
   --
   --    Vector - Array of C-strings from GTK+
   --
   -- The elements of the result shall not be freed.
   --
   -- Returns :
   --
   --    An equivalent of Chars_Ptr_Array type
   --
   function Convert (Vector : Chars_Ptr_Ptr) return Gtkada.Types.Chars_Ptr_Array;

   procedure Free (Vector : Chars_Ptr_Ptr);
   pragma Import (C, Free, "g_strfreev");

end Glib.Chars_Ptr_Vectors;
