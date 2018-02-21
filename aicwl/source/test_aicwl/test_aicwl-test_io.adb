--                                                                    --
--  procedure Test_IO               Copyright (c)  Dmitry A. Kazakov  --
--  Separate body                                  Luebeck            --
--                                                 Spring, 2011       --
--                                                                    --
--                                Last revision :  07:54 21 Jul 2016  --
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

separate (Test_AICWL) procedure Test_IO is
begin
   --
   -- Test integer I/O
   --
   declare
      Buffer : String_Stream (1024*8);
      Value  : GUInt;
   begin
      for I in GUInt range 0..512 loop
         Store (Buffer, I);
      end loop;
      Rewind (Buffer);
      for I in GUInt range 0..512 loop
         Restore (Buffer, Value);
         if I /= Value then
            Gtk.Main.Router.Trace
            (  "GUInt I/O error: read"
            &  GUInt'Image (Value)
            &  ", expected"
            &  GUInt'Image (I)
            );
            exit;
         end if;
      end loop;
   exception
      when Error : others =>
         Gtk.Main.Router.Trace
         (  "GUInt I/O error:"
         &  Exception_Information (Error)
         );
   end;
   --
   -- Test GDouble I/O
   --
   declare
      procedure Test (Value : GDouble) is
         Buffer : String_Stream (1024);
         Data   : GDouble;
      begin
         Store (Buffer, Value);
         Rewind (Buffer);
         Restore (Buffer, Data);
         if abs (Data - Value) * 1_000_000.0 > abs Value then
            Gtk.Main.Router.Trace
            (  "GUInt I/O error: read"
            &  GDouble'Image (Data)
            &  ", expected"
            &  GDouble'Image (Value)
            );
         end if;
      end Test;
   begin
      Test (0.0);
      Test (0.001);
      Test (-0.5);
      Test (1.0E23);
      Test (-7.770E20);
      Test (-7.770E-20);
      Test (7.770E20);
   exception
      when Error : others =>
         Gtk.Main.Router.Trace
         (  "GDouble I/O error:"
         &  Exception_Information (Error)
         );
   end;
   --
   -- Test enumerations
   --
   declare
      Buffer : String_Stream (1024);
      Data   : Cairo_Line_Cap;
   begin
      for Value in Cairo_Line_Cap'Range loop
         Rewind (Buffer);
         Store (Buffer, Value);
         Rewind (Buffer);
         Restore (Buffer, Data);
         if Data /= Value then
            Gtk.Main.Router.Trace
            (  "Enumeration I/O error: read "
            &  Cairo_Line_Cap'Image (Data)
            &  ", expected "
            &  Cairo_Line_Cap'Image (Value)
            );
         end if;
      end loop;
   end;
   --
   -- Test strings
   --
   declare
      Buffer : aliased String_Stream (1024);
      Text   : constant String := "qwertyuiopasdfghjklzxcvbnm";
   begin
      Store (Buffer, Text);
      Rewind (Buffer);
      declare
         Value : constant String := Restore (Buffer'Access);
      begin
         if Text /= Value then
            Gtk.Main.Router.Trace
            (  "String I/O error: read '"
            &  Value
            &  "', expected '"
            &  Text
            &  '''
            );
         end if;
      end;
   end;
end Test_IO;
