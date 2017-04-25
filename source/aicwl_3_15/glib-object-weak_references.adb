--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Object.Weak_References                 Luebeck            --
--  Implementation                                 Spring, 2007       --
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
--____________________________________________________________________--

with System.Address_To_Access_Conversions;

package body GLib.Object.Weak_References is

   package Address_To_Access is
      new System.Address_To_Access_Conversions (Weak_Reference'Class);
   use Address_To_Access;

   procedure Adjust (Reference : in out Weak_Reference) is
   begin
      if Reference.Object /= null then
         Weak_Ref
         (  Reference.Object,
            Notifier_Ptr,
            Weak_Reference'Class (Reference)'Address
         );
      end if;
   end Adjust;

   procedure Finalize (Reference : in out Weak_Reference)
      renames Invalidate;

   function Get (Reference : Weak_Reference)
      return access Object_Type'Class is
   begin
      return Reference.Object;
   end Get;

   procedure Invalidate (Reference : in out Weak_Reference) is
   begin
      if Reference.Object /= null then
         Weak_Unref
         (  Reference.Object,
            Notifier_Ptr,
            Weak_Reference'Class (Reference)'Address
         );
         Reference.Object := null;
      end if;
   end Invalidate;

   function Is_Valid (Reference : Weak_Reference) return Boolean is
   begin
      return Reference.Object /= null;
   end Is_Valid;

   function Ref (Object : not null access Object_Type'Class)
      return Weak_Reference is
      Reference : Weak_Reference;
   begin
      Weak_Ref
      (  Object,
         Notifier_Ptr,
         Weak_Reference'Class (Reference)'Address
      );
      Reference.Object := Object.all'Access;
      return Reference;
   end Ref;

   procedure Notifier
             (  Data                 : System.Address;
                Where_The_Object_Was : System.Address
             )  is
      Ptr : constant Object_Pointer := To_Pointer (Data);
   begin
      if Ptr /= null then
         Ptr.Object := null;
         Ptr.Notify;
      end if;
   end Notifier;

   procedure Set
             (  Reference : in out Weak_Reference;
                Object    : not null access Object_Type'Class
             )  is
   begin
      if Object /= Reference.Object then
         Invalidate (Reference);
      end if;
      Weak_Ref
      (  Object,
         Notifier_Ptr,
         Weak_Reference'Class (Reference)'Address
      );
      Reference.Object := Object.all'Access;
   end Set;

end GLib.Object.Weak_References;
