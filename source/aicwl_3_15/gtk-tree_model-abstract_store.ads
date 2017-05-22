--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Abstract_Store               Luebeck            --
--  Interface                                      Summer, 2006       --
--                                                                    --
--                                Last revision :  10:05 22 Nov 2014  --
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
--  This  package  provides  abstract  base  type  for developing custom
--  Tree_View_Model. The code was inspired by  an  implementation  of  a
--  custom model by Maxim Resznik.
--
--  The  GTK+  'virtual'  functions  are  mapped in a natural way to the
--  abstract primitive operations of the base type. A derived type shall
--  provide an implementation for them. Finalization is supported.
--
with GLib.Values;   use GLib.Values;
with GtkAda.Types;  use GtkAda.Types;

package Gtk.Tree_Model.Abstract_Store is
--
-- Gtk_Abstract_Model_Record -- Abstract base type for  custom  TreeView
--                              models
--
   type Gtk_Abstract_Model_Record is
      abstract new Gtk_Root_Tree_Model_Record with private;
--
-- Register -- GTK type and interface registration
--
--    Name       - Of the type
--    Signals    - The array of additional signal names
--    Parameters - Of the signals (an array of)
--
-- For  each non-abstract derived type of Gtk_Abstract_Model_Record this
-- function shall be called once before creation of the first object of.
-- For  each element of Signals a parameterless signal with this name is
-- added to the registered type.
--
-- Returns :
--
--    The GTK type corresponding to the Ada type
--
   function Register
            (  Name       : String;
               Signals    : Chars_Ptr_Array := Null_Array;
               Parameters : Signal_Parameter_Types :=
                            Null_Parameter_Types
            )  return GType;
--
-- Children -- Next level
--
--    Model  - A pointer to
--    Parent - The iterator
--
-- The children of a node are not columns. A simple rows x columns table
-- has only nodes of the first level. These nodes are rows. When  Parent
-- is Null_Iter, the first top node should be the result.
--
-- Returns :
--
--    The first child of Parent or Null_Iter
--
   function Children
            (  Model  : not null access Gtk_Abstract_Model_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is abstract;
--
-- Finalize -- Custom finalization
--
--    Model  - A pointer to
--
-- This  procedure  is  called upon object destruction. The override, if
-- any, shall call the parent's version.
--
   procedure Finalize
             (  Model : not null access Gtk_Abstract_Model_Record
             )  is null;
--
-- Get_Column_Type -- The type of a column in the model
--
--    Model  - A pointer to
--    Column - The column number (zero based)
--
-- Returns :
--
--    The type of the column
--
   function Get_Column_Type
            (  Model : not null access Gtk_Abstract_Model_Record;
               Index : GInt
            )  return GType is abstract;
--
-- Get_Flags -- The GTK flags associated with the model
--
--    Model - A pointer to
--
-- Returns :
--
--    The GTK+ flags
--
   function Get_Flags
            (  Model : not null access Gtk_Abstract_Model_Record
            )  return Tree_Model_Flags is abstract;
--
-- Get_Iter -- The iterator associated with a path
--
--    Model - A pointer to
--    Path  - The path
--
-- Returns :
--
--    The iterator or Null_Iter when Path is invalid
--
   function Get_Iter
            (  Model : not null access Gtk_Abstract_Model_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is abstract;
--
-- Get_N_Columns -- The number of columns
--
--    Model - A pointer to
--
-- Returns :
--
--    The number of columns
--
   function Get_N_Columns
            (  Model : not null access Gtk_Abstract_Model_Record
            )  return GInt is abstract;
--
-- Get_Path -- The iterator associated with the path
--
--    Model - A pointer to
--    Iter  - The iterator
--
-- This function gets the path from an iterator. A path  is  dynamically
-- allocated and has to be freed later using Path_Free.
--
-- Returns :
--
--    The path of the iterator
--
   function Get_Path
            (  Model : not null access Gtk_Abstract_Model_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is abstract;
--
-- Get_Value -- Value at the path's column specified
--
--    Model  - A pointer to
--    Iter   - The iterator
--    Column - The column number, zero based
--    Value  - The result
--
-- This procedure is used to query a value from the store. The result is
-- returned in Value. Values are freed by the caller using Unset.
--
   procedure Get_Value
             (  Model  : not null access Gtk_Abstract_Model_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             )  is abstract;
--
-- Has_Child -- Next level
--
--    Model - A pointer to
--    Iter  - The iterator
--
-- Returns :
--
--    True if Iter has children
--
   function Has_Child
            (  Model : not null access Gtk_Abstract_Model_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is abstract;
--
-- Initialize -- Construction
--
--    Model   - A pointer to a newly allocated object
--    Type_Of - The type obtained by a call to Register
--
-- This procedure has to be called  by  any  derived  type  upon  object
-- construction. Normally it is the first call of its Initialize,  which
-- in  turn  is  called  from a Gtk_New. The parameter Type_Of must be a
-- value  returned by Register called with the name assigned to the GTK+
-- type of the derived type. Note that Register  shall  be  called  only
-- once.  So  its  result  must  be stored somewhere in the package that
-- derives the type.
--
   procedure Initialize
             (  Model   : not null access
                          Gtk_Abstract_Model_Record'Class;
                Type_Of : GType
             );
--
-- Next -- Same level
--
--    Model - A pointer to
--    Iter  - The iterator
--
-- Moves Iter to the next sibling node. Null_Iter  is  the  result  when
-- there is no more siblings.
--
   procedure Next
             (  Model : not null access Gtk_Abstract_Model_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is abstract;
--
-- Nth_Child -- Child by index
--
--    Model  - A pointer to
--    Parent - Which child is asked for
--    N      - The child number, zero based
--
-- Returns :
--
--    An iterator to the child
--
   function Nth_Child
            (  Model  : not null access Gtk_Abstract_Model_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is abstract;
--
-- N_Children -- Of an iterator
--
--    Model - A pointer to
--    Iter  - The iterator (Null_Iter for the top)
--
-- Returns :
--
--    The number of children
--
   function N_Children
            (  Model : not null access Gtk_Abstract_Model_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt is abstract;
--
-- Parent -- Level up
--
--    Model - A pointer to
--    Child - The iterator
--
-- Returns :
--
--    The iterator to the parent, or Null_Iter
--
   function Parent
            (  Model : not null access Gtk_Abstract_Model_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is abstract;
--
-- Previous -- Same level previous node
--
--    Model - A pointer to
--    Iter  - The iterator
--
-- Moves Iter  to the previous  sibling node. Null_Iter  is  the  result
-- when there is no more siblings.
--
   procedure Previous
             (  Model : not null access Gtk_Abstract_Model_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is abstract;
--
-- Ref_Node -- Of an iterator
--
--    Model - A pointer to
--    Iter  - The iterator
--
-- The default implementation does nothing.
--
   procedure Ref_Node
             (  Model : not null access Gtk_Abstract_Model_Record;
                Iter  : Gtk_Tree_Iter
             )  is null;
--
-- Unref_Node -- Of an iterator
--
--    Model - A pointer to
--    Iter  - The iterator
--
-- The default implementation does nothing.
--
   procedure Unref_Node
             (  Model : not null access Gtk_Abstract_Model_Record;
                Iter  : Gtk_Tree_Iter
             )  is null;
------------------------------------------------------------------------
--
-- Image -- Textual representation of an iterator
--
--    Iter - The iterator
--
-- Returns :
--
--    Iterator fields in numeric format
--
   function Image (Iter : Gtk_Tree_Iter) return String;

private
   use System;

   type Gtk_Abstract_Model_Record is
      abstract new Gtk_Root_Tree_Model_Record with null record;
   type Gtk_Abstract_Model_Record_Ptr is
      access all Gtk_Abstract_Model_Record'Class;
--
-- C_Model_Record -- Gtk object associated with the store
--
-- All  C  interface functions refer to an instance of this type. It can
-- also be obtained using Get_Object.
--
   type GObject is record -- GObject
      G_Type_Instance : Address;
      Ref_Count       : GUInt;
      QData           : Address;
   end record;
   pragma Convention (C, GObject);
--
-- Register_Type -- GTK type and interface registration
--
--    Name       - Of the type
--    Size       - Of C instance
--    Signals    - To add (an array of names)
--    Parameters - Of the signals (an array of)
--
-- This function can be used to  register  a  derived  type  instead  of
-- public Register, if C_Model_Record  is  extended.  In  fact  Register
-- calls to this function.
--
-- Returns :
--
--    The GTK type corresponding to the Ada type
--
   function Register_Type
            (  Name       : String;
               Size       : Positive := GObject'Size;
               Signals    : Chars_Ptr_Array := Null_Array;
               Parameters : Signal_Parameter_Types :=
                               Null_Parameter_Types
            )  return GType;

end Gtk.Tree_Model.Abstract_Store;
