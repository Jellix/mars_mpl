--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.                       Luebeck            --
--        Ring_Data_Buffer                         Winter, 2011       --
--  Implementation                                                    --
--                                Last revision :  22:46 07 Apr 2016  --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with GLib.Messages;      use GLib.Messages;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.Waveform.Ring_Data_Buffer is

   procedure Free is
      new Ada.Unchecked_Deallocation (Layers_List, Layers_List_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Gtk_Wavefrom_Ring_Data_Buffer_Record'Class,
             Gtk_Wavefrom_Ring_Data_Buffer
          );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Waveform.Ring_Data_Buffer." & Name;
   end Where;

   procedure Backward
             (  Source : in out Source_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Index  : Reference := Source.Index;
      This   : Point;
      Got_It : Boolean;
   begin
      if Buffer.Length = 0 then
         raise End_Error;
      end if;
      Buffer.Get (Index, This, Got_It);
      if Got_It and then This.T = T then
         if Index = 0 then
            raise End_Error;
         end if;
         Index := Index - 1;
         Scan_Backward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            raise End_Error;
         end if;
      else
         Buffer.Find (T, True, Index, Got_It);
         if not Got_It then
            raise End_Error;
         end if;
         Scan_Backward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            raise End_Error;
         end if;
      end if;
      Source.Index := Index;
      Source.T := T;
   end Backward;

   procedure Backward
             (  Source : in out Source_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Index  : Reference := Source.Index;
      This   : Point;
   begin
      if Buffer.Length = 0 then
         Got_It := False;
         return;
      end if;
      Buffer.Get (Index, This, Got_It);
      if Got_It and then This.T = T then
         if Index = 0 then
            Got_It := False;
         end if;
         Index := Index - 1;
         Scan_Backward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            return;
         end if;
      else
         Buffer.Find (T, True, Index, Got_It);
         if not Got_It then
            return;
         end if;
         Scan_Backward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            return;
         end if;
      end if;
      Source.Index := Index;
      Source.T := T;
      Got_It   := True;
   exception
      when End_Error =>
         Got_It := False;
   end Backward;

   procedure Connected
             (  Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record;
                Layer  : in out Waveform_Layer'Class
             )  is
   begin
      if Source.Layers.Connected > 0 then
         declare
            Old_Ptr : Layers_List_Ptr := Source.Layers.Ptr;
            Layers  : Layers_List renames Source.Layers.Ptr.all;
         begin
            for Index in 1..Source.Layers.Connected loop
               if Layers (Index) = Layer'Unchecked_Access then
                  return;
               end if;
            end loop;
            if Source.Layers.Connected = Layers'Length then
               Source.Layers.Ptr :=
                  new Layers_List
                      (  1
                      .. (Source.Layers.Connected * 3) / 2
                      );
               Source.Layers.Ptr (1..Source.Layers.Connected) :=
                  Layers (1..Source.Layers.Connected);
               Free (Old_Ptr);
            end if;
         end;
      end if;
      Source.Layers.Connected := Source.Layers.Connected + 1;
      Source.Layers.Ptr (Source.Layers.Connected) :=
         Layer'Unchecked_Access;
   end Connected;

   function Create
            (  Source : not null access
                        Gtk_Wavefrom_Ring_Data_Buffer_Record
            )  return Waveform_Data_Scanner'Class is
   begin
      if Source.Length > 0 then
         return Source_Scanner'
                (  Index  => Source.First,
                   Source => Source.all'Unchecked_Access,
                   T      => Source.Data
                             (  Positive
                                (  (Source.First mod Source.Wrap)
                                +  1
                             )  ) .T
                );
      else
         return Source_Scanner'
                (  Index  => Source.First,
                   Source => Source.all'Unchecked_Access,
                   T      => X_Axis'Last
                );
      end if;
   end Create;

   procedure Disconnected
             (  Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record;
                Layer  : in out Waveform_Layer'Class
             )  is
      Layers : Layers_List renames Source.Layers.Ptr.all;
   begin
      if Source.Layers.Connected > 0 then
         for Index in reverse 1..Source.Layers.Connected loop
            if Layers (Index) = Layer'Unchecked_Access then
               Layers (Index) := null;
               Source.Layers.Connected := Source.Layers.Connected - 1;
               Layers (Index..Source.Layers.Connected) :=
                  Layers (Index + 1..Source.Layers.Connected + 1);
               return;
            end if;
         end loop;
      end if;
   end Disconnected;

   procedure Erase
             (  Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record
             )  is
      First, Last : Point;
   begin
      if Source.Length > 0 then
         First :=
            Source.Data (Integer (Source.First mod Source.Wrap + 1));
         Last :=
            Source.Data
            (  Integer
               (  (Source.First + Source.Length - 1) mod Source.Wrap
               +  1
            )  );
         Source.Length := 0;
         Notify (Source, First.T, Last.T);
      end if;
   end Erase;

   procedure Finalize (Object : in out List) is
   begin
      while Object.Connected > 0 loop
         Object.Ptr (Object.Connected).Set_Source;
      end loop;
      Free (Object.Ptr);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   procedure Find
             (  Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
                T      : X_Axis;
                Above  : Boolean;
                Index  : out Reference;
                Got_It : out Boolean
             )  is
      Lower   : Reference := Source.First;
      Upper   : Reference := Source.First + Source.Length - 1;
      Current : Reference;
      This    : Point;
   begin
      loop
         Current := (Lower + Upper) / 2;
         Source.Get (Current, This, Got_It);
         if not Got_It then
            return;
         elsif This.T < T then
            if Upper <= Current then
               if Above and then Current < Upper then
                  Index := Current + 1;
               else
                  Index := Current;
               end if;
               return;
            end if;
            Lower := Current + 1;
         elsif This.T > T then
            if Lower >= Current then
               if Above or else Lower >= Current then
                  Index := Current;
               else
                  Index := Current - 1;
               end if;
               return;
            end if;
            Upper := Current - 1;
         else
            Index := Current;
            return;
         end if;
      end loop;
   end Find;

   procedure First
             (  Source : in out Source_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Data   : Point;
      Got_It : Boolean;
   begin
      loop
         if Buffer.Length = 0 then
            raise End_Error;
         end if;
         Get (Buffer, Buffer.First, Data, Got_It);
         if Got_It then
            T := Data.T;
            V := Data.V;
            return;
         end if;
      end loop;
   end First;

   procedure First
             (  Source : in out Source_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Data   : Point;
   begin
      loop
         if Buffer.Length = 0 then
            Got_It := False;
            return;
         end if;
         Get (Buffer, Buffer.First, Data, Got_It);
         if Got_It then
            T := Data.T;
            V := Data.V;
            return;
         end if;
      end loop;
   end First;

   procedure Forward
             (  Source : in out Source_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Index  : Reference := Source.Index;
      This   : Point;
      Got_It : Boolean;
   begin
      if Buffer.Length = 0 then
         raise End_Error;
      end if;
      Buffer.Get (Index, This, Got_It);
      if Got_It and then This.T = T then
         Index := Index + 1;
         Scan_Forward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            raise End_Error;
         end if;
      else
         Buffer.Find (T, False, Index, Got_It);
         if not Got_It then
            raise End_Error;
         end if;
         Scan_Forward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            raise End_Error;
         end if;
      end if;
      Source.Index := Index;
      Source.T := T;
   end Forward;

   procedure Forward
             (  Source : in out Source_Scanner;
                T      : in out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Index  : Reference := Source.Index;
      This   : Point;
   begin
      if Buffer.Length = 0 then
         Got_It := False;
      end if;
      Buffer.Get (Index, This, Got_It);
      if Got_It and then This.T = T then
         Index := Index + 1;
         Scan_Forward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            return;
         end if;
      else
         Buffer.Find (T, False, Index, Got_It);
         if not Got_It then
            return;
         end if;
         Scan_Forward (Buffer, Index, T, V, Got_It);
         if not Got_It then
            return;
         end if;
      end if;
      Source.Index := Index;
      Source.T := T;
      Got_It := True;
   end Forward;

   procedure Get
             (  Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
                Index  : Reference;
                Data   : out Point;
                Got_It : out Boolean
             )  is
   begin
      if Source.Is_In (Index) then
         Data   := Source.Data (Integer (Index mod Source.Wrap + 1));
         Got_It := Source.Is_In (Index);
      else
         Got_It := False;
      end if;
   end Get;

   function Get_Source
            (  Scanner : Source_Scanner
            )  return not null access Waveform_Data_Source'Class is
   begin
      return Scanner.Source;
   end Get_Source;

   procedure Gtk_New
             (  Source : out Gtk_Wavefrom_Ring_Data_Buffer;
                Size   : Positive
             )  is

   begin
      Source := new Gtk_Wavefrom_Ring_Data_Buffer_Record (Size);
      Gtk.Layered.Waveform.Ring_Data_Buffer.Initialize (Source);
   exception
      when others =>
         Free (Source);
         raise;
   end Gtk_New;

   procedure Initialize
             (  Source : not null access
                         Gtk_Wavefrom_Ring_Data_Buffer_Record'Class
             )  is
   begin
      GLib.Object.Initialize (Source);
   end Initialize;

   function Is_In
            (  Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
               Index  : Reference
            )  return Boolean is
   begin
      return
      (  Source.Length > 0
      and then
         Index in Source.First..Source.First + Source.Length - 1
      );
   end Is_In;

   function Is_In
            (  Source : Source_Scanner;
               T      : X_Axis
            )  return Boolean is
      Buffer   : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
                 Source.Source.all;
      From, To : Point;
      Got_It   : Boolean;
   begin
      loop
         if Buffer.Length = 0 then
            return False;
         end if;
         Get (Buffer, Buffer.First, From, Got_It);
         if Got_It then
            Get (Buffer, Buffer.First + Buffer.Length - 1, To, Got_It);
            if Got_It then
               return T >= From.T and then T <= To.T;
            end if;
         end if;
      end loop;
   end Is_In;

   procedure Last
             (  Source : in out Source_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Data   : Point;
      Got_It : Boolean;
   begin
      loop
         if Buffer.Length = 0 then
            raise End_Error;
         end if;
         Get (Buffer, Buffer.First + Buffer.Length - 1, Data, Got_It);
         if Got_It then
            T := Data.T;
            V := Data.V;
            return;
         end if;
      end loop;
   end Last;

   procedure Last
             (  Source : in out Source_Scanner;
                T      : out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
      Buffer : Gtk_Wavefrom_Ring_Data_Buffer_Record renames
               Source.Source.all;
      Data   : Point;
   begin
      loop
         if Buffer.Length = 0 then
            Got_It := False;
            return;
         end if;
         Get (Buffer, Buffer.First + Buffer.Length - 1, Data, Got_It);
         if Got_It then
            T := Data.T;
            V := Data.V;
            return;
         end if;
      end loop;
   end Last;

   procedure Notify
             (  Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
                From   : X_Axis;
                To     : X_Axis
             )  is
   begin
      if (  Source.Layers.Connected > 0
         and then
            Source.Layers.Ptr /= null
         )
      then
         declare
            Layers : Layers_List renames Source.Layers.Ptr.all;
         begin
            for Index in 1..Source.Layers.Connected loop
               declare
                  Layer : constant Waveform_Layer_Ptr := Layers (Index);
               begin
                  if Layer /= null then
                     Layer.Changed (From, To);
                  end if;
               exception
                  when Error : others =>
                     Log
                     (  GtkAda_Contributions_Domain,
                        Log_Level_Critical,
                        (  "Fault: "
                        &  Exception_Information (Error)
                        &  Where ("Notify")
                     )  );
               end;
            end loop;
         end;
      end if;
   end Notify;

   procedure Put
             (  Source : in out Gtk_Wavefrom_Ring_Data_Buffer_Record;
                T      : X_Axis;
                V      : Y_Axis
             )  is
      Data : Points_Array renames Source.Data;
      This : Point;
   begin
      for Index in reverse Source.First
                        .. Source.First + Source.Length - 1 loop
         This := Data (Positive (Index mod Source.Wrap + 1));
         if This.T <= T then
            if This.T = T then
                -- Replace this point
               if This.V /= V then
                  Data (Positive (Index mod Source.Wrap + 1)).V := V;
                  Source.Notify (T, T);
               end if;
            else
               -- Place right after this point
               if Source.Length = Source.Wrap then
                  Source.Length := Source.Length - 1;
                  Source.First  := Source.First  + 1;
               end if;
               declare
                  From : Positive :=
                            Positive
                            (  (  (Source.First + Source.Length)
                               mod
                                  Source.Wrap
                               )
                            +  1
                            );
                  To   : Positive;
                  Here : Positive;
               begin
                  Here := Positive ((Index + 1) mod Source.Wrap + 1);
                  while From /= Here loop
                     To := From;
                     if To = 1 then
                        From := Source.Size;
                     else
                        From := To - 1;
                     end if;
                     Data (To) := Data (From);
                  end loop;
                  Data (Here) := (T, V);
               end;
               Source.Length := Source.Length + 1;
               Source.Notify (T, T);
            end if;
            return;
         end if;
      end loop;
      if Source.Length < Source.Wrap then
         -- Insert at the buffer beginning
         Data (Positive ((Source.First - 1) mod Source.Wrap + 1)) :=
            (T, V);
         Source.First  := Source.First - 1;
         Source.Length := Source.Length + 1;
         Source.Notify (T, T);
      end if;
   end Put;

   procedure Scan_Backward
             (  Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
                Index  : in out Reference;
                T      : in out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
      This : Point;
   begin
      loop
         Get (Source, Index, This, Got_It);
         exit when not Got_It;
         if This.T < T then
            T := This.T;
            V := This.V;
            return;
         end if;
         Index := Index - 1;
      end loop;
   end Scan_Backward;

   procedure Scan_Forward
             (  Source : Gtk_Wavefrom_Ring_Data_Buffer_Record;
                Index  : in out Reference;
                T      : in out X_Axis;
                V      : out Y_Axis;
                Got_It : out Boolean
             )  is
      This : Point;
   begin
      loop
         Get (Source, Index, This, Got_It);
         exit when not Got_It;
         if This.T > T then
            T := This.T;
            V := This.V;
            return;
         end if;
         Index := Index + 1;
      end loop;
   end Scan_Forward;

end Gtk.Layered.Waveform.Ring_Data_Buffer;
