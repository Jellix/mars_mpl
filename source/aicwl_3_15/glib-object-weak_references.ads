--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Object.Weak_References                 Luebeck            --
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
-- __________________________________________________________________ --
--
--  This  generic  package  provides  a  easier  way  of  using GTK weak
--  references.   It   provides   a   controlled   type   Weak_Reference
--  encapsulating  a  GTK+ weak reference. Weak references don't prevent
--  the object from destruction, but become a notification upon. It is a
--  more easy way than to connect to a signal. The type is  non-limited,
--  and can be put into widget records.
--
with Ada.Finalization;
with System;

generic
   type Object_Type (<>) is new GObject_Record with private;
package Glib.Object.Weak_References is

   --
   -- Weak_Reference -- Strong reference to an object
   --
   type Weak_Reference is
     new Ada.Finalization.Controlled with private;
   --
   -- Adjust -- Assignment
   --
   --    Reference - Being copied
   --
   -- When overridden, it must be called from the override.
   --
   overriding
   procedure Adjust (Reference : in out Weak_Reference);
   --
   -- Finalize -- Destruction
   --
   --    Reference - To destroy
   --
   -- A derived type is responsible to call to  Finalize  if  it  overrides
   -- this procedure.
   --
   overriding
   procedure Finalize (Reference : in out Weak_Reference);
   --
   -- Get -- Get the referenced object
   --
   --    Reference - Object reference
   --
   -- Returns :
   --
   --    A pointer to the object, or null, if the reference is invalid
   --
   function Get (Reference : Weak_Reference)
                 return access Object_Type'Class;
   --
   -- Invalidate -- Reset reference
   --
   --    Reference - Object reference
   --
   -- The reference becomes invalid.
   --
   procedure Invalidate (Reference : in out Weak_Reference);
   --
   -- Is_Invalid -- Check if the reference points to an object
   --
   --    Reference - Object reference
   --
   -- Returns :
   --
   --    True if the reference is valid
   --
   function Is_Valid (Reference : Weak_Reference) return Boolean;
   --
   -- Notify -- Object destruction notification
   --
   --    Reference - Object reference
   --
   -- At  the  call  time  the  object  does not exist and the reference is
   -- already  invalid.  The default implementation does nothing. It should
   -- be overridden to provide desired functionality.
   --
   procedure Notify (Reference : in out Weak_Reference) is null;
   --
   -- Ref -- Get a reference to the object
   --
   --    Object - The object to reference
   --
   -- Returns :
   --
   --    A weak reference to the object
   --
   function Ref (Object : not null access Object_Type'Class)
                 return Weak_Reference;
   --
   -- Set -- To another object
   --
   --    Reference - Object reference
   --    Object    - The object to reference
   --
   -- Nothing happens if Object is already referenced by Reference.
   --
   procedure Set
     (Reference : in out Weak_Reference;
      Object    : not null access Object_Type'Class);

private

   pragma Inline (Get, Invalidate, Is_Valid, Ref, Set);

   type Object_Ptr is access all Object_Type'Class;

   --
   -- Weak_Reference -- Contains  a  pointer to the object when valid. GLib
   --                   weak  reference  is  associated with the object. As
   -- the data  for  the  reference  the  address  of  the  Ada  object  is
   -- specified. The callback is the procedure Notifier. Upon Adjust the  a
   -- new GLib weak reference is create with the address of the Ada  object
   -- as the data. Upon finalization the GLib weak reference is removed.
   --
   type Weak_Reference is new Ada.Finalization.Controlled with record
      Object : Object_Ptr;
   end record;

   procedure Notifier
     (Data                 : System.Address;
      Where_The_Object_Was : System.Address);
   pragma Convention (C, Notifier);

   Notifier_Ptr : constant Weak_Notify := Notifier'Access;

end Glib.Object.Weak_References;
