--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Object.Strong_References               Luebeck            --
--  Interface                                      Spring, 2007       --
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
--____________________________________________________________________--
--
--  This generic package provides a easier way of using GTK  references.
--  It  provides  a  controlled type Strong_Reference encapsulating GTK+
--  reference. When set to an object it calls to Ref. When finalized  it
--  calls  to  Unref.  Strong_Reference  can  be  used to prevent a GTK+
--  object from premature destruction. You can use Strong_Reference when
--  you don't want to derive your widget from  a  widget  container  but
--  wish it acted as a container. The type is non-limited,  and  can  be
--  put into widget records.
--
with Ada.Finalization;

generic
   type Object_Type is new GObject_Record with private;
package GLib.Object.Strong_References is
--
-- Strong_Reference -- Strong reference to an object
--
   type Strong_Reference is
      new Ada.Finalization.Controlled with private;
--
-- Adjust -- Assignment
--
--    Reference - Being copied
--
-- When overridden, it must be called from the override.
--
   procedure Adjust (Reference : in out Strong_Reference);
--
-- Finalize -- Destruction
--
--    Reference - To destroy
--
-- A derived type is responsible to call to  Finalize  if  it  overrides
-- this procedure.
--
   procedure Finalize (Reference : in out Strong_Reference);
--
-- Get -- Get the referenced object
--
--    Reference - Object reference
--
-- Returns :
--
--    A pointer to the object, or null, if the reference is invalid
--
   function Get (Reference : Strong_Reference)
      return access Object_Type'Class;
--
-- Invalidate -- Reset reference
--
--    Reference - Object reference
--
-- The reference becomes invalid. As a result the referenced object  can
-- be destroyed.
--
   procedure Invalidate (Reference : in out Strong_Reference);
--
-- Is_Invalid -- Check if the reference points to an object
--
--    Reference - Object reference
--
-- Returns :
--
--    True if the reference is valid
--
   function Is_Valid (Reference : Strong_Reference) return Boolean;
--
-- Ref -- Get a reference to the object
--
--    Object - The object to reference
--
-- Returns :
--
--    A strong reference to the object
--
   function Ref (Object : not null access Object_Type'Class)
      return Strong_Reference;
--
-- Set -- To another object
--
--    Reference - Object reference
--    Object    - The object to reference
--
-- Nothing happens if Object is already referenced by Reference.
--
   procedure Set
             (  Reference : in out Strong_Reference;
                Object    : access Object_Type'Class
             );
private
   pragma Inline (Get, Invalidate, Is_Valid, Ref, Set);

   type Object_Ptr is access all Object_Type'Class;

   type Strong_Reference is new Ada.Finalization.Controlled with record
      Object : Object_Ptr;
   end record;

end GLib.Object.Strong_References;
