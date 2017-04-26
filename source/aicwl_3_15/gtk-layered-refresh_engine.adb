--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Refresh_Engine                  Luebeck            --
--  Implementation                                 Winter, 2011       --
--                                                                    --
--                                Last revision :  09:08 05 Mar 2017  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with Glib.Messages;      use Glib.Messages;
with Gtk.Missed;         use Gtk.Missed;

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with Gdk.Window;

package body Gtk.Layered.Refresh_Engine is

   package Conversions is
      new System.Address_To_Access_Conversions (Layered_Refresh_Engine);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Refresh_Engine" & Name;
   end Where;

   procedure Add
             (  Engine : in out Layered_Refresh_Engine;
                Widget : not null access Gtk_Layered_Record'Class
             )  is
      Element : List_Element_Ptr;
   begin
      if Engine.Active then
         raise Use_Error with
               "Adding a widget while engine is refreshing";
      end if;
      if Engine.List = null then
         Element := new List_Element;
         Engine.List := Element;
      else
         declare
            This : not null access List_Element := Engine.List;
         begin
            loop
               if (  Is_Valid (This.Widget)
                  and then
                     Get (This.Widget) = Widget
                  )
               then
                  return;
               end if;
               exit when This.Next = Engine.List;
               This := This.Next;
            end loop;
         end;
         Element := new List_Element;
         Element.Prev := Engine.List.Prev;
         Element.Next := Engine.List;
         Element.Prev.Next := Element;
         Element.Next.Prev := Element;
      end if;
      Element.Widget.Set (Widget.all'Unchecked_Access);
   end Add;

   procedure Delete
             (  Engine : in out Layered_Refresh_Engine'Class;
                This   : List_Element_Ptr
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  List_Element,
                List_Element_Ptr
             );
      Ptr : List_Element_Ptr := This;
   begin
      if Engine.List = This then
         if This.Next = This then
            Engine.List := null;
         else
            Engine.List := This.Next;
         end if;
      end if;
      This.Prev.Next := This.Next;
      This.Next.Prev := This.Prev;
      Free (Ptr);
   end Delete;

   procedure Delete
             (  Engine : in out Layered_Refresh_Engine;
                Widget : not null access Gtk_Layered_Record'Class
             )  is
   begin
      if Engine.Active then
         raise Use_Error with
               "Deleting a widget while engine is refreshing";
      end if;
      if Engine.List /= null then
         declare
            This : not null access List_Element := Engine.List;
         begin
            loop
               if (  Is_Valid (This.Widget)
                  and then
                     Get (This.Widget) = Widget
                  )
               then
                  Delete (Engine, This.all'Unchecked_Access);
                  return;
               end if;
               exit when This.Next = Engine.List;
               This := This.Next;
            end loop;
         end;
      end if;
   end Delete;

   procedure Finalize (Engine : in out Layered_Refresh_Engine) is
   begin
      if Engine.Timer /= 0 then
         if 0 = Remove (Engine.Timer) then
            null;
         end if;
         Engine.Timer := 0;
      end if;
   end Finalize;

   function Get_Period (Engine : Layered_Refresh_Engine)
      return Duration is
   begin
      return Engine.Period;
   end Get_Period;

   procedure Set_Period
             (  Engine : in out Layered_Refresh_Engine;
                Period : Duration
             )  is
   begin
      if Period /= Engine.Period then
         declare
            Interval : constant Guint :=
                       Guint (Gdouble (Period) * 1_000.0);
         begin
            if Engine.Timer /= 0 then
               if 0 = Remove (Engine.Timer) then
                  null;
               end if;
               Engine.Timer := 0;
            end if;
            Engine.Timer :=
               Timeout_Add
               (  Interval,
                  Timer'Access,
                  Engine'Address
               );
            Engine.Period := Period;
         end;
      end if;
   end Set_Period;

   function Timer (Data : System.Address) return Gboolean is
      Engine : Layered_Refresh_Engine renames
                  Conversions.To_Pointer (Data).all;
   begin
      if Engine.List /= null then
         Engine.Active := True;
         declare
            This : not null access List_Element := Engine.List;
            Next : not null access List_Element := This;
         begin
            loop
               Next := This.Next;
               if Is_Valid (This.Widget) then
                  Queue_Draw (Get (This.Widget));
               else
                  Delete (Engine, This.all'Unchecked_Access);
                  exit when Engine.List = null;
               end if;
               exit when Next = Engine.List;
               This := Next;
            end loop;
         end;
         Engine.Active := False;
      end if;
      return 1;
   end Timer;

end Gtk.Layered.Refresh_Engine;
