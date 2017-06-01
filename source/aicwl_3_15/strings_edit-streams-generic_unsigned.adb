--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Streams.                       Luebeck            --
--        Generic_Unsigned                         Autumn, 2014       --
--  Implementation                                                    --
--                                                                    --
--                                Last revision :  16:33 18 Nov 2016  --
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

with Ada.Exceptions;
with Ada.IO_Exceptions;

package body Strings_Edit.Streams.Generic_Unsigned is

   procedure Get
     (Data    : Ada.Streams.Stream_Element_Array;
      Pointer : in out Ada.Streams.Stream_Element_Offset;
      Value   : out Number)
   is
      Result : Number  := 0;
      Power  : Natural := 0;

      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if
        Pointer < Data'First or else
        (Pointer > Data'Last and then
         Pointer - 1 > Data'Last)
      then
         Ada.Exceptions.Raise_Exception
           (Ada.IO_Exceptions.Layout_Error'Identity,
            "Pointer is out of bounds");
      end if;
      while Pointer <= Data'Last loop
         begin
            Result :=
               Result + 2**Power * Number (Data (Pointer) and 16#7F#);
         exception
            when Constraint_Error =>
               Ada.Exceptions.Raise_Exception
                 (Ada.IO_Exceptions.Data_Error'Identity,
                  "Number is too large");
         end;
         if 0 = (Data (Pointer) and 16#80#) then
            Pointer := Pointer + 1;
            Value   := Result;
            return;
         end if;
         Power   := Power + 7;
         Pointer := Pointer + 1;
      end loop;
      Ada.Exceptions.Raise_Exception
        (Ada.IO_Exceptions.End_Error'Identity,
         "Non-terminated chain sequence");
   end Get;

   function Input
     (Stream : access Ada.Streams.Root_Stream_Type'Class) return Number
   is
      Result : Number  := 0;
      Power  : Natural := 0;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
      This   : Ada.Streams.Stream_Element renames Buffer (1);
      Last   : Ada.Streams.Stream_Element_Offset;

      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      loop
         Ada.Streams.Read (Stream.all, Buffer, Last);
         exit when Last /= 1;
         begin
            Result :=
               Result + 2 ** Power * Number (This and 16#7F#);
         exception
            when Constraint_Error =>
               Ada.Exceptions.Raise_Exception
                 (Ada.IO_Exceptions.Data_Error'Identity,
                  "Number is too large");
         end;
         if 0 = (This and 16#80#) then
            return Result;
         end if;
         Power := Power + 7;
      end loop;
      Ada.Exceptions.Raise_Exception
        (Ada.IO_Exceptions.End_Error'Identity,
         "Non-terminated chain sequence");
   end Input;

   procedure Output
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Value  : Number)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Buffer  : Ada.Streams.Stream_Element_Array (1 .. Number'Size + 6 / 7);
      Pointer : Ada.Streams.Stream_Element_Offset := 1;
   begin
      Put (Buffer, Pointer, Value);
      Ada.Streams.Write (Stream.all, Buffer (1 .. Pointer - 1));
   end Output;

   procedure Put
     (Data    : in out Ada.Streams.Stream_Element_Array;
      Pointer : in out Ada.Streams.Stream_Element_Offset;
      Value   : Number)
   is
      Item : Number := Value;

      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Pointer not in Data'Range then
         Ada.Exceptions.Raise_Exception
           (Ada.IO_Exceptions.Layout_Error'Identity,
            "Pointer is out of bounds");
      elsif Item < 0 then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "Negative value");
      end if;
      while Pointer <= Data'Last loop
         Data (Pointer) :=
           Ada.Streams.Stream_Element (Item mod 16#80#) or 16#80#;
         Pointer := Pointer + 1;
         Item := Item / 16#80#;
         if Item = 0 then
            Data (Pointer - 1) := Data (Pointer - 1) and 16#7F#;
            return;
         end if;
      end loop;
      Ada.Exceptions.Raise_Exception
        (Ada.IO_Exceptions.Layout_Error'Identity,
         "No room for output");
   end Put;

end Strings_Edit.Streams.Generic_Unsigned;
