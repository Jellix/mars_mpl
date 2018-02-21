--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Streams                        Luebeck            --
--  Implementation                                 Spring, 2009       --
--                                                                    --
--                                Last revision :  22:44 07 Apr 2016  --
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
--
with Ada.IO_Exceptions;

package body Strings_Edit.Streams is

   function Get (Stream : String_Stream) return String is
   begin
      return Stream.Data (1 .. Stream.Position - 1);
   end Get;

   function Get_Size (Stream : String_Stream)
                      return Ada.Streams.Stream_Element_Count
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      return
        Ada.Streams.Stream_Element_Offset
          (Stream.Data'Last - Stream.Position + 1) / Char_Count;
   end Get_Size;

   procedure Increment
     (Left  : in out Integer;
      Right : Ada.Streams.Stream_Element_Offset) with Inline;
   procedure Increment
     (Left  : in out Integer;
      Right : Ada.Streams.Stream_Element_Offset) is
   begin
      Left := Left + Integer (Right) * Char_Count;
   end Increment;

   overriding procedure Read
     (Stream : in out String_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      if Stream.Position > Stream.Length then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      declare
         subtype Space is
           Ada.Streams.Stream_Element_Array (1 .. Get_Size (Stream));
         Data : Space;
         pragma Import (Ada, Data);
         for Data'Address use Stream.Data (Stream.Position)'Address;

         use type Ada.Streams.Stream_Element_Offset;
      begin
         if Space'Length >= Item'Length then
            Last := Item'Last;
            Item := Data (1 .. Item'Length);
            Increment (Stream.Position, Item'Length);
         else
            Last := Item'First + Data'Length - 1;
            Item (Item'First .. Last) := Data;
            Stream.Position := Stream.Data'Last + 1;
         end if;
      end;
   end Read;

   procedure Rewind (Stream : in out String_Stream) is
   begin
      Stream.Position := 1;
   end Rewind;

   procedure Set (Stream : in out String_Stream; Content : String) is
   begin
      if Content'Length > Stream.Length then
         raise Constraint_Error;
      end if;
      Stream.Position := Stream.Length - Content'Length + 1;
      Stream.Data (Stream.Position .. Stream.Length) := Content;
   end Set;

   overriding procedure Write
     (Stream : in out String_Stream;
      Item   : Ada.Streams.Stream_Element_Array) is
   begin
      if Stream.Position > Stream.Length then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      declare
         subtype Space is
           Ada.Streams.Stream_Element_Array (1 .. Get_Size (Stream));
         Data : Space;
         pragma Import (Ada, Data);
         for Data'Address use Stream.Data (Stream.Position)'Address;
      begin
         if Item'Length > Space'Length then
            raise Ada.IO_Exceptions.End_Error;
         end if;
         Data (1 .. Item'Length) := Item;
         Increment (Stream.Position, Item'Length);
      end;
   end Write;

end Strings_Edit.Streams;
