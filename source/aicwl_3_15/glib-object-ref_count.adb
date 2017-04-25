--                                                                    --
--  package GLib.Object.Ref_Count   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2007       --
--                                                                    --
--                                Last revision :  10:00 13 Oct 2007  --
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

with Ada.Unchecked_Conversion;

with System;  use System;

function GLib.Object.Ref_Count
         (  Object : access GObject_Record'Class
         )  return GUInt is
   type C_GObject_Record is record -- The layout of a GObject
      G_Type_Interface : Address;
      Ref_Count        : GUInt;
      pragma Volatile (Ref_Count);
   end record;
   pragma Pack (C_GObject_Record);
   pragma Convention (C, C_GObject_Record);
   type C_GObject_Record_Ptr is access all C_GObject_Record;
   function To_GObject is
      new Ada.Unchecked_Conversion (Address, C_GObject_Record_Ptr);
begin
   return To_GObject (Get_Object (Object)).Ref_Count;
end GLib.Object.Ref_Count;
