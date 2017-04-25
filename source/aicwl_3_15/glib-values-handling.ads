--                                                                    --
--  package GLib.Values.Handling    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2006       --
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

with GLib.Object;  use GLib.Object;

package GLib.Values.Handling is
--
-- Copy -- A value
--
--    Src_Value - The value to copy
--
-- This function creates a copy of  the  value.  For  reference  counted
-- objects it increases the count.
--
-- Returns :
--
--    A copy of the argument
--
   function Copy (Src_Value : GValue) return GValue;
--
-- Copy -- A value
--
--    Src_Value  - The value to copy
--    Dest_Value - The value to set
--
-- This  procedure  makes  a  copy  of  Src_Value  and  stores  it  into
-- Dest_Value.
--
   procedure Copy (Src_Value : GValue; Dest_Value : in out GValue);
--
-- Get_Type -- Of a value
--
--    Value - The value
--
-- Returns :
--
--    The GTK+ type of
--
   function Get_Type (Value : GValue) return GType;
   pragma Inline (Get_Type);

end GLib.Values.Handling;
