--                                                                    --
--  procedure Test_Ring_Buffer      Copyright (c)  Dmitry A. Kazakov  --
--  Separate body                                  Luebeck            --
--                                                 Spring, 2011       --
--                                                                    --
--                                Last revision :  21:42 01 Oct 2011  --
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

with Ada.IO_Exceptions;           use Ada.IO_Exceptions;
with Gtk.Layered.Waveform;        use Gtk.Layered.Waveform;
with Gtk.Main.Router.GNAT_Stack;  use Gtk.Main.Router.GNAT_Stack;

with Gtk.Layered.Waveform.Ring_Data_Buffer;
use  Gtk.Layered.Waveform.Ring_Data_Buffer;

separate (Test_AICWL) procedure Test_Ring_Buffer is
   Buffer : aliased Gtk_Wavefrom_Ring_Data_Buffer;
begin
   Gtk_New (Buffer, 500);
   for Index in 1..200 loop
      Buffer.Put (X_Axis (Index), Y_Axis (Index));
   end loop;
   declare
      S : Waveform_Data_Scanner'Class := Buffer.Create;
      T : X_Axis;
      V : Y_Axis;
   begin
      for Index in reverse 1..199 loop
         T := X_Axis (Index);
         S.Forward (T, V);
         if Index + 1 /= Integer (V) then
            raise Constraint_Error with
                  (  "Ring buffer error, forward scan "
                  &  Y_Axis'Image (V)
                  &  " expected"
                  &  Integer'Image (Index + 1)
                  );
         end if;
      end loop;
      for Index in 2..200 loop
         T := X_Axis (Index);
         S.Backward (T, V);
         if Index - 1 /= Integer (V) then
            raise Constraint_Error with
                  (  "Ring buffer error, backward scan "
                  &  Y_Axis'Image (V)
                  &  " expected"
                  &  Integer'Image (Index - 1)
                  );
         end if;
      end loop;
      T := 300.0;
      for Index in reverse 1..200 loop
         S.Backward (T, V);
         if Index /= Integer (V) then
            raise Constraint_Error with
                  (  "Ring buffer error, backward scan "
                  &  Y_Axis'Image (V)
                  &  " expected"
                  &  Integer'Image (Index)
                  );
         end if;
      end loop;
      T := 0.0;
      for Index in 1..200 loop
         S.Forward (T, V);
         if Index /= Integer (V) or else Y_Axis (T) /= V then
            raise Constraint_Error with
                  (  "Ring buffer error, forward scan "
                  &  Y_Axis'Image (V)
                  &  " expected"
                  &  Integer'Image (Index)
                  );
         end if;
      end loop;
      begin
         S.Forward (T, V);
         raise Constraint_Error with
               "Unexpected value for T =" & X_Axis'Image (T);
      exception
         when End_Error =>
            null;
      end;
   exception
      when Error : End_Error =>
         Trace (Error);
         raise Constraint_Error with
               "Unexpected End_Error T =" & X_Axis'Image (T);
   end;
   Buffer.Unref;
exception
   when Error : others =>
      Trace (Error);
end Test_Ring_Buffer;
