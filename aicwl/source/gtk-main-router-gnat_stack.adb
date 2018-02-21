--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Main.Router.GNAT_Stack                  Luebeck            --
--  Implementation                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Most_Recent_Exception;
with GNAT.Traceback.Symbolic;

with Interfaces.C.Strings;

with System;

package body Gtk.Main.Router.GNAT_Stack is

   Traceback_Depth : constant := 1_000;
   Filters_List    : Log_Filter_Ptr;

   procedure Dump_Call_Stack (Prefix : String := "")
   is
      TB  : GNAT.Traceback.Tracebacks_Array (1 .. Traceback_Depth);
      Len : Natural;

      use type Ada.Exceptions.Exception_Occurrence_Access;
   begin
      GNAT.Traceback.Call_Chain (TB, Len);
      if Prefix'Length > 0 then
         Ada.Text_IO.Put_Line (Prefix);
      end if;
      Ada.Text_IO.Put_Line
        (GNAT.Traceback.Symbolic.Symbolic_Traceback (TB (1 .. Len)));
      if GNAT.Most_Recent_Exception.Occurrence_Access /= null then
         Ada.Text_IO.Put_Line
           ("Most recent exception: " &
            Ada.Exceptions.Exception_Information
              (GNAT.Most_Recent_Exception.Occurrence_Access.all));
      end if;
   end Dump_Call_Stack;

   overriding procedure Finalize (Filter : in out Log_Filter) is
   begin
      if Filter.Next /= null then
         if Filters_List = Filter'Unchecked_Access then
            if Filter.Next = Filter'Unchecked_Access then
               Filters_List := null;
            else
               Filters_List := Filter.Next;
               Filter.Previous.all.Next := Filter.Next;
               Filter.Next.all.Previous := Filter.Previous;
            end if;
         else
            Filter.Previous.all.Next := Filter.Next;
            Filter.Next.all.Previous := Filter.Previous;
         end if;
         Filter.Next := null;
      end if;
   end Finalize;

   procedure Indent
     (Message : UTF8_String;
      Break   : Boolean  := Standard.False;
      Step    : Positive := 2)
   is
      TB  : GNAT.Traceback.Tracebacks_Array (1 .. Traceback_Depth);
      Len : Natural;

      function "*"
        (Left  : Natural;
         Right : Character) return String renames Ada.Strings.Fixed."*";
   begin
      GNAT.Traceback.Call_Chain (TB, Len);
      pragma Unreferenced (TB);
      Len := Integer'Max (0, Len - 1) * Step;
      Gtk.Main.Router.Trace
        (Message => Len * ' ' & Message,
         Break   => Break);
   end Indent;

   overriding procedure Initialize (Filter : in out Log_Filter) is
   begin
      if Filters_List = null then
         Filters_List    := Filter'Unchecked_Access;
         Filter.Previous := Filters_List;
         Filter.Next     := Filters_List;
      else
         Filter.Previous := Filters_List.all.Previous;
         Filter.Next     := Filters_List;
         Filters_List.all.Previous      := Filter'Unchecked_Access;
         Filters_List.all.Previous.all.Next := Filter'Unchecked_Access;
      end if;
   end Initialize;

   procedure Say
     (Message       : UTF8_String;
      Title         : UTF8_String                    := "";
      Mode          : UTF8_String                    := Gtk.Stock.Stock_Dialog_Info;
      Justification : Gtk.Enums.Gtk_Justification    := Gtk.Enums.Justify_Left;
      Parent        : access Gtk_Widget_Record'Class := null)
   is
      TB  : GNAT.Traceback.Tracebacks_Array (1 .. Traceback_Depth);
      Len : Natural;
   begin
      GNAT.Traceback.Call_Chain (TB, Len);
      Gtk.Main.Router.Say
        (Message =>
           Message & Ada.Characters.Latin_1.LF &
           GNAT.Traceback.Symbolic.Symbolic_Traceback (TB (1 .. Len)),
         Title   => Title,
         Mode    => Mode,
         Parent  => Parent,
         Justification => Justification);
   end Say;

   procedure Trace
     (Message : UTF8_String;
      Break   : Boolean := Standard.False)
   is
      TB  : GNAT.Traceback.Tracebacks_Array (1 .. Traceback_Depth);
      Len : Natural;
   begin
      GNAT.Traceback.Call_Chain (TB, Len);
      Gtk.Main.Router.Trace
        (Message =>
           Message & Ada.Characters.Latin_1.LF &
           GNAT.Traceback.Symbolic.Symbolic_Traceback (TB (1 .. Len)),
         Break   => Break);
   end Trace;

   procedure Trace
     (Error : Ada.Exceptions.Exception_Occurrence;
      Break : Boolean := Standard.True) is
   begin
      Gtk.Main.Router.Trace
        (Message =>
           Ada.Exceptions.Exception_Information (Error) &
           Ada.Characters.Latin_1.LF &
           GNAT.Traceback.Symbolic.Symbolic_Traceback (Error),
         Break   => Break);
   end Trace;

   type Log_Function_Ptr is access procedure
     (Domain  : String;
      Level   : Glib.Messages.Log_Level_Flags;
      Message : UTF8_String);

   procedure Log_Function
     (Domain  : String;
      Level   : Glib.Messages.Log_Level_Flags;
      Message : UTF8_String)
   is
      Head : constant Log_Filter_Ptr := Filters_List;
   begin
      if Head /= null then
         declare
            This : Log_Filter_Ptr := Head;
            Next : Log_Filter_Ptr;
            Done : Boolean;
         begin
            loop
               Next := This.all.Next;
               Done := Next = Head;
               if This.all.Ignore (Domain, Level, Message) then
                  return;
               end if;
               exit when Done;
               This := Next;
            end loop;
         end;
      end if;
      declare
         TB  : GNAT.Traceback.Tracebacks_Array (1 .. Traceback_Depth);
         Len : Natural;
      begin
         GNAT.Traceback.Call_Chain (TB, Len);
         Glib.Messages.Log_Default_Handler (Domain, Level, Message);
         Gtk.Main.Router.Trace
           (Message =>
              Domain & " " & Message & Ada.Characters.Latin_1.LF
              & GNAT.Traceback.Symbolic.Symbolic_Traceback (TB (1 .. Len)),
            Break   => Standard.True);
      end;
   end Log_Function;

   procedure Set_Log_Trace
     (Domain : String;
      Level  : Glib.Messages.Log_Level_Flags :=
        Glib.Messages.Log_Fatal_Mask or Glib.Messages.Log_Level_Critical)
   is
      ID : Glib.Messages.Log_Handler_Id;
   begin
      ID := Glib.Messages.Log_Set_Handler (Domain, Level, Log_Function'Access);
      pragma Unreferenced (ID);
   end Set_Log_Trace;

   type Log_Func_Ptr is access procedure
     (Domain  : Interfaces.C.Strings.chars_ptr;
      Level   : Glib.Messages.Log_Level_Flags;
      Message : Interfaces.C.Strings.chars_ptr;
      Handler : Log_Function_Ptr);
   pragma Convention (C, Log_Func_Ptr);

   procedure Log_Func
     (Domain  : Interfaces.C.Strings.chars_ptr;
      Level   : Glib.Messages.Log_Level_Flags;
      Message : Interfaces.C.Strings.chars_ptr;
      Handler : Log_Function_Ptr);
   pragma Convention (C, Log_Func);

   procedure Log_Func
     (Domain  : Interfaces.C.Strings.chars_ptr;
      Level   : Glib.Messages.Log_Level_Flags;
      Message : Interfaces.C.Strings.chars_ptr;
      Handler : Log_Function_Ptr)
   is
      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Domain = Interfaces.C.Strings.Null_Ptr then
         Handler ("", Level, Interfaces.C.Strings.Value (Message));
      else
         Handler
           (Interfaces.C.Strings.Value (Domain),
            Level,
            Interfaces.C.Strings.Value (Message));
      end if;
   end Log_Func;

   procedure Set_Log_Trace is
      function Internal
        (Func   : Log_Func_Ptr   := Log_Func'Access;
         Data   : System.Address := Log_Function'Address)
         return Glib.Messages.Log_Handler_Id;
      pragma Import (C, Internal, "g_log_set_default_handler");
      ID : Glib.Messages.Log_Handler_Id;
   begin
      ID := Internal;
      pragma Unreferenced (ID);
   end Set_Log_Trace;

end Gtk.Main.Router.GNAT_Stack;
