--                                                                    --
--  package GLib.Values.Handling    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body GLib.Values.Handling is

   function Copy (Src_Value : GValue) return GValue is
      procedure Internal
                 (  Src_Value  : access GValue;
                    Dest_Value : in out GValue
                 );
      pragma Import (C, Internal, "g_value_copy");
      Result : GValue;
      Temp   : aliased GValue := Src_Value;
   begin
      Init (Result, Src_Value.G_Type);
      Internal (Temp'Access, Result);
      return Result;
   end Copy;

   procedure Copy (Src_Value : GValue; Dest_Value : in out GValue) is
      procedure Internal
                 (  Src_Value  : access GValue;
                    Dest_Value : in out GValue
                 );
      pragma Import (C, Internal, "g_value_copy");
      Temp : aliased GValue := Src_Value;
   begin
      if Dest_Value.G_Type /= Src_Value.G_Type then
         Unset (Dest_Value);
         Init (Dest_Value, Src_Value.G_Type);
      end if;
      Internal (Temp'Access, Dest_Value);
   end Copy;

   function Get_Type (Value : GValue) return GType is
   begin
      return Value.G_Type;
   end Get_Type;

end GLib.Values.Handling;
