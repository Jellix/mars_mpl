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
-- __________________________________________________________________ --

with System.Address_To_Access_Conversions;

package body Gtk.Handlers.References is

   package Conversions is
      new System.Address_To_Access_Conversions (Handler_Reference);

   type GClosureNotify is access
      procedure (Data : System.Address; Closure : GClosure);
   pragma Convention (C, GClosureNotify);

   procedure Invalidate (Closure : GClosure);
   pragma Import (C, Invalidate, "g_closure_invalidate");

   procedure Invalidated (Data : System.Address; Closure : GClosure);
   pragma Convention (C, Invalidated);

   procedure Add_Invalidate_Notifier
     (Closure     : GClosure;
      Notify_Data : System.Address;
      Notify_Func : GClosureNotify);
   pragma Import (C,
                  Add_Invalidate_Notifier,
                  "g_closure_add_invalidate_notifier");

   overriding procedure Adjust (Reference : in out Handler_Reference) is
   begin
      if Reference.ID /= Null_Handler_Id then
         Add_Invalidate_Notifier
           (Reference.Closure,
            Reference'Address,
            Invalidated'Access);
      end if;
   end Adjust;

   overriding procedure Finalize (Reference : in out Handler_Reference) is
   begin
      if Reference.ID /= Null_Handler_Id then
         Invalidate (Reference.Closure);
         Reference.ID := Null_Handler_Id;
      end if;
   end Finalize;

   function Get (Reference : Handler_Reference) return Handler_Id is
   begin
      return (Reference.ID, Reference.Closure);
   end Get;

   procedure Invalidated (Data : System.Address; Closure : GClosure)
   is
      pragma Unreferenced (Closure);
   begin
      Conversions.To_Pointer (Data).all.ID := Null_Handler_Id;
   end Invalidated;

   procedure Set
     (Reference : in out Handler_Reference;
      Handler   : Handler_Id) is
   begin
      if
        Reference.ID /= Handler.Id or else
        Reference.Closure /= Handler.Closure
      then
         Finalize (Reference);
         if Handler.Id /= Null_Handler_Id then
            Reference.ID      := Handler.Id;
            Reference.Closure := Handler.Closure;
            Add_Invalidate_Notifier
              (Handler.Closure,
               Reference'Address,
               Invalidated'Access);
         end if;
      end if;
   end Set;

   procedure Set (Reference : in out Handler_Reference) is
   begin
      Finalize (Reference);
   end Set;

end Gtk.Handlers.References;
