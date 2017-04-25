--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Handle                          Luebeck            --
--  Interface                                      Summer, 2006       --
--                                                                    --
--                                Last revision :  11:15 17 Oct 2010  --
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
--  This  generic  package  provides  a  way  to place objects of simple
--  components  into  GTK+  values. The package has the following formal
--  parameters:
--
--     Type_Name       - The GTK+ type name of the values
--     Object_Type     - The type of objects
--     Object_Type_Ptr - The pointer type of the object compatible with
--     Handle_Type     - The type of handles to the object
--     Ptr             - Pointer to handle conversion
--     Ref             - Handle to pointer conversion
--
--  The package automatically registers a GTK+ type for  the  values  of
--  the objects. The name of the type is  determined  by  the  parameter
--  Type_Name.
--
with System;  use System;
with Object;  use Object;

generic
   Type_Name : in String;
   type Object_Type (<>) is abstract new Entity with private;
   type Object_Type_Ptr  is access Object_Type'Class;
   type Handle_Type (<>) is private;
   with function Ptr (Refernece : Handle_Type)
      return Object_Type_Ptr is <>;
   with function Ref (Object : Object_Type_Ptr)
      return Handle_Type is <>;
package GLib.Values.Handle is
--
-- Get_Type -- The GTK+ type used with values
--
   function Get_Type return GType;
--
-- Get_Handle -- Get handle to an object from a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  must  have  been  initialized  using  Init  with the type
-- Get_Type.
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Constraint_Error - The value is not an objct
--
   function Get_Handle (Value : GValue) return Handle_Type;
--
-- Get_Ptr -- Get pointer to an object from a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  must  have  been  initialized  using  Init  with the type
-- Gtk_Type. The result is valid no longer Unset is called to Value. The
-- result is null when value is not an object.
--
-- Returns :
--
--    A pointer to the object or null
--
   function Get_Ptr (Value : GValue) return Object_Type_Ptr;
--
-- Set_Handle -- Set a value
--
--    Value     - To set
--    Reference - A handle to the feature
--
-- This  procedure sets an object into GTK+ value previously initialized
-- using Init with the parameter Gtk_Type.
--
-- Exceptions :
--
--    Constraint_Error - Not an object value, invalid handle
--
   procedure Set_Handle
             (  Value     : in out GValue;
                Reference : Handle_Type
             );
--
-- Set_Ptr -- Set a feature
--
--    Value - To set
--    Ptr   - A pointer to the object
--
-- This  procedure sets a feature into GTK+ value previously initialized
-- using Init with the parameter Gtk_Type.
--
-- Exceptions :
--
--    Constraint_Error - Not  a  properly initialized object value, null
--                       pointer
--
   procedure Set_Ptr (Value : in out GValue; Ptr : Object_Type_Ptr);

private
   pragma Inline (Get_Handle);
   pragma Inline (Get_Ptr);
   pragma Inline (Set_Handle);
   pragma Inline (Get_Type);
   pragma Inline (Get_Ptr);

   function Copy_Handle (Boxed : Address) return Address;
   pragma Convention (C, Copy_Handle);

   procedure Free_Handle (Boxed : Address);
   pragma Convention (C, Free_Handle);

   Copy : constant Boxed_Copy := Copy_Handle'Access;
   Free : constant Boxed_Free := Free_Handle'Access;

end GLib.Values.Handle;
