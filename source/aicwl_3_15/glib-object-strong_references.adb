--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Object.Strong_References               Luebeck            --
--  Implementation                                 Spring, 2007       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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

package body Glib.Object.Strong_References is

   overriding procedure Adjust (Reference : in out Strong_Reference) is
   begin
      if Reference.Object /= null then
         Reference.Object.all.Ref;
      end if;
   end Adjust;

   overriding procedure Finalize (Reference : in out Strong_Reference)
      renames Invalidate;

   function Get (Reference : Strong_Reference)
      return access Object_Type'Class is
   begin
      return Reference.Object;
   end Get;

   procedure Invalidate (Reference : in out Strong_Reference) is
   begin
      if Reference.Object /= null then
         Reference.Object.all.Unref;
         Reference.Object := null;
      end if;
   end Invalidate;

   function Is_Valid (Reference : Strong_Reference) return Boolean is
   begin
      return Reference.Object /= null;
   end Is_Valid;

   function Ref (Object : not null access Object_Type'Class)
      return Strong_Reference is
   begin
      return Result : Strong_Reference do
         Object.all.Ref;
         Result.Object := Object.all'Unchecked_Access;
      end return;
   end Ref;

   procedure Set
     (Reference : in out Strong_Reference;
      Object    : access Object_Type'Class) is
   begin
      if Object /= Reference.Object then
         Reference.Invalidate;
         if Object /= null then
            Object.all.Ref;
            Reference.Object := Object.all'Unchecked_Access;
         end if;
      end if;
   end Set;

end Glib.Object.Strong_References;
