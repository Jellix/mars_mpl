--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Handlers.References                     Luebeck            --
--  Implementation                                 Spring, 2007       --
--                                                                    --
--                                Last revision :  15:55 10 Sep 2011  --
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

with System;  use System;

with System.Address_To_Access_Conversions;

package body Gtk.Handlers.References is

   package Conversions is
      new System.Address_To_Access_Conversions (Handler_Reference);

   type GClosureNotify is access
      procedure (Data : Address; Closure : GClosure);
   pragma Convention (C, GClosureNotify);

   procedure Add_Invalidate_Notifier
             (  Closure     : GClosure;
                Notify_Data : Address;
                Notify_Func : GClosureNotify
             );
   pragma Import
          (  C,
             Add_Invalidate_Notifier,
             "g_closure_add_invalidate_notifier"
          );

   procedure Invalidate (Closure : GClosure);
   pragma Import (C, Invalidate, "g_closure_invalidate");

   procedure Invalidated (Data : Address; Closure : GClosure);
   pragma Convention (C, Invalidated);

   function Get (Reference : Handler_Reference) return Handler_ID is
   begin
      return (Reference.ID, Reference.Closure);
   end Get;

   procedure Invalidated (Data : Address; Closure : GClosure) is
   begin
      Conversions.To_Pointer (Data).ID := Null_Handler_ID;
   end Invalidated;

   procedure Set
             (  Reference : in out Handler_Reference;
                Handler   : Handler_ID
             )  is
   begin
      if (  Reference.ID /= Handler.ID
         or else
            Reference.Closure /= Handler.Closure
         )
      then
         Finalize (Reference);
         if Handler.ID /= Null_Handler_ID then
            Reference.ID      := Handler.ID;
            Reference.Closure := Handler.Closure;
            Add_Invalidate_Notifier
            (  Handler.Closure,
               Reference'Address,
               Invalidated'Access
            );
         end if;
      end if;
   end Set;

   procedure Set (Reference : in out Handler_Reference) is
   begin
      Finalize (Reference);
   end Set;

   procedure Adjust (Reference : in out Handler_Reference) is
   begin
      if Reference.ID /= Null_Handler_ID then
         Add_Invalidate_Notifier
         (  Reference.Closure,
            Reference'Address,
            Invalidated'Access
         );
      end if;
   end Adjust;

   procedure Finalize (Reference : in out Handler_Reference) is
   begin
      if Reference.ID /= Null_Handler_ID then
         Invalidate (Reference.Closure);
         Reference.ID := Null_Handler_ID;
      end if;
   end Finalize;

end Gtk.Handlers.References;
