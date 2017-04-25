--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Extension_Store              Luebeck            --
--  Interface                                      Autumn, 2007       --
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
--  This package provides a model derived from its  reference  model  by
--  adding new columns to it. The reference model itself is not changed,
--  it is the derived model which keeps new columns. It is possible both
--  to add different columns to the same reference model and to add  new
--  columns to an extended one. Internally the derived model is  a  tree
--  store containing new columns.
--     The  extended  model does not have own row insertion and deletion
--  operations. Instead rows are manipulated in the reference model. The
--  effect  of  these operations mirrors on all extended models of. When
--  rows get inserted, their values in the extended columns can  be  set
--  using Set_Extension. For this the reference model iterator should be
--  converted to an iterator of the extension model with To_Extension.
--
with GLib.Values;                    use GLib.Values;
with Gtk.Handlers;                   use Gtk.Handlers;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;

with Gtk.Tree_Store;

package Gtk.Tree_Model.Extension_Store is
--
-- Gtk_Extension_Store_Record -- An extension model
--
   type Gtk_Extension_Store_Record is
      new Gtk_Abstract_Model_Record with private;
   type Gtk_Extension_Store is
      access all Gtk_Extension_Store_Record'Class;
--
-- Changed -- Row modification notification
--
--    Model - The extension model
--    Path  - To the row changed
--    Iter  - To the row changed
--
-- This procedure is called upon modification of a reference model  row.
-- It can be used instead of handling row-changed  signal.  Iter  is  an
-- extension  model's  iterator  to  the  changed   row.   The   default
-- implementation emits row-changed.

   procedure Changed
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             );
--
-- Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Children
               (  Model  : not null access Gtk_Extension_Store_Record;
                  Parent : Gtk_Tree_Iter
               )  return Gtk_Tree_Iter;
--
-- Deleted -- Row deletion notification
--
--    Model - The extension model
--    Path  - To the row deleted
--
-- This  procedure is called upon deletion of a reference model row. The
-- row is already deleted in both models at the time  call.  Path  is  a
-- (now invalid) path to the deleted row in  the  extension  model.  The
-- default implementation eimits row-deleted.

   procedure Deleted
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path
             );
--
-- Deleting -- Row deletion notification
--
--    Model - The extension model
--    Path  - To the row begin deleted
--    Iter  - To the row being deleted
--
-- Differently  to Deleted this procedure is called just before deleting
-- the  extension  row.  An  implementation  shall not modify either the
-- reference  or  the extension model here. Note also that the parameter
-- Iter cannot be converted to the reference model or used to access  it
-- in any other way, because the  corresponding  row  there  is  already
-- deleted.  The primary objective of this procedure is to save the data
-- of the extension columns upon deletion.  The  default  implementation
-- does nothing.

   procedure Deleting
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             );
--
-- Finalize -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- This  procedure  is  called upon object destruction. The override, if
-- any, shall call the parent's version.
--
   overriding
      procedure Finalize
                (  Model : not null access Gtk_Extension_Store_Record
                );
--
-- From_Extension -- Iterator / path conversion
--
--    Model       - The extension model
--    Iter / Path - An iterator or path in it (extension model)
--
-- Note that returned non-null path has to be freed by the caller  using
-- Path_Free.
--
-- Returns :
--
--    The reference model iterator or path
--
   function From_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   function From_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Path;
--
-- Get_Column_Type -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_Column_Type
               (  Model : not null access Gtk_Extension_Store_Record;
                  Index : GInt
               )  return GType;
--
-- Get_Extension_Types -- Get the reference model
--
--    Model - The extension model
--
-- The result is indexed by the zero-based columns number.
--
-- Returns :
--
--    The types of the extension columns
--
   function Get_Extension_Types
            (  Model : not null access Gtk_Extension_Store_Record
            )  return GType_Array;
--
-- Get_Flags -- Overrides Gtk.Tree_Model.Abstract_Store...
--
--    Model - A pointer to
--
-- Returns :
--
--    The GTK+ flags
--
   overriding
      function Get_Flags
               (  Model : not null access Gtk_Extension_Store_Record
               )  return Tree_Model_Flags;
--
-- Get_Iter -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_Iter
               (  Model : not null access Gtk_Extension_Store_Record;
                  Path  : Gtk_Tree_Path
               )  return Gtk_Tree_Iter;
--
-- Get_N_Columns -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_N_Columns
               (  Model : not null access Gtk_Extension_Store_Record
               )  return GInt;
--
-- Get_Path -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_Path
               (  Model : not null access Gtk_Extension_Store_Record;
                  Iter  : Gtk_Tree_Iter
               )  return Gtk_Tree_Path;
--
-- Get_Reference -- Get the reference model
--
--    Model - The extension model
--
-- Returns :
--
--    The reference model
--
   function Get_Reference
            (  Model : not null access Gtk_Extension_Store_Record
            )  return Gtk_Tree_Model;
--
-- Get_Value -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Get_Value
             (  Model  : not null access Gtk_Extension_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
--
-- Gtk_New -- Factory
--
--    Model       - The result
--  [ Reference ] - The reference model, the new one will be based on
--    Types       - The types of the columns to add
--
-- The paramerter Reference is the model the result is based  upon.  The
-- parameter Types is the array of types of the columns added.
--
   procedure Gtk_New
             (  Model     : out Gtk_Extension_Store;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class;
                Types     : GType_Array
             );
   procedure Gtk_New
             (  Model : out Gtk_Extension_Store;
                Types : GType_Array
             );
--
-- Has_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Has_Child
               (  Model : not null access Gtk_Extension_Store_Record;
                  Iter  : Gtk_Tree_Iter
               )  return Boolean;
--
-- Initialize -- Construction
--
--    Model       - The extension model being initialized
--  [ Reference ] - The reference model, the new one will be based on
--    Types       - The types of the columns to add
--
-- One of these procedures shall be called by the derived type.
--
   procedure Initialize
             (  Model     : not null access
                            Gtk_Extension_Store_Record'Class;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class;
                Types     : GType_Array
             );
   procedure Initialize
             (  Model : not null access
                        Gtk_Extension_Store_Record'Class;
                Types : GType_Array
             );
--
-- Inserted -- Row insertion notification
--
--    Model - The extension model
--    Path  - To the row inserted
--    Iter  - To the row inserted
--
-- This procedure is called upon insertion of a new reference model row.
-- It  can  be  used instead of handling row-inserted signal. Iter is an
-- extension model's iterator to the new row. An implementation can  use
-- Set_Extended   to   initialize   extension   columns.   The   default
-- implementation eimits row-inserted.

   procedure Inserted
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             );
--
-- Next -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      procedure Next
                (  Model : not null access Gtk_Extension_Store_Record;
                   Iter  : in out Gtk_Tree_Iter
                );
--
-- Nth_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Nth_Child
               (  Model  : not null access Gtk_Extension_Store_Record;
                  Parent : Gtk_Tree_Iter;
                  N      : GInt
               )  return Gtk_Tree_Iter;
--
-- N_Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function N_Children
               (  Model : not null access Gtk_Extension_Store_Record;
                  Iter  : Gtk_Tree_Iter := Null_Iter
               )  return GInt;
--
-- Parent -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Parent
               (  Model : not null access Gtk_Extension_Store_Record;
                  Child : Gtk_Tree_Iter
               )  return Gtk_Tree_Iter;
--
-- Previous -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Previous
             (  Model : not null access Gtk_Extension_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- Ref_Node -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Ref_Node
             (  Model : not null access Gtk_Extension_Store_Record;
                Iter  : Gtk_Tree_Iter
             );
--
-- Set_Extension -- The data of an extended model cell
--
--    Model  - The extension model
--    Iter   - An iterator to the row (extension model)
--    Column - The extension column number 1..
--    Value  - The value to set into
--
-- These  procedures  are  similar  to ones of list store. The parameter
-- Iter  is  a  model  iterator.  The  parameter  column  identifies  an
-- extension column 1.. Note that the reference model columns cannot  be
-- modified  this way because there is no universal way to do this for a
-- general Gtk_Tree_Model.
--
   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : Boolean
             );
   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : GInt
             );
   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : UTF8_String
             );
   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : GValue
             );
--
-- Set_Null_Reference -- Set empty reference model
--
--    Model - The extension model
--
-- This  procedure  sets  the reference model empty. This can be used to
-- undertake massive changes in the reference model. First the reference
-- model  is  set  to  null,  then  changes  are  applied  and   finally
-- Set_Reference is used to assign the reference model back.
--
   procedure Set_Null_Reference
             (  Model : not null access Gtk_Extension_Store_Record
             );
--
-- Set_Reference -- Change the reference model
--
--    Model     - The extension model
--    Reference - The reference model
--
-- This  procedure  changes the reference model. The side effect of this
-- operation is  that  the  extension  columns  become  empty.  The  new
-- reference model should have exactly same  types  of  columns  as  its
-- predecessor,  because  clients  using  the model might not anticipate
-- column number and types change.
--
   procedure Set_Reference
             (  Model     : not null access Gtk_Extension_Store_Record;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class
             );
--
-- To_Extension -- Iterator / path conversion
--
--    Model       - The extension model
--    Iter / Path - An iterator or path in the reference model
--
-- Note that  returned  path  has  to  be  freed  by  the  caller  using
-- Path_Free.
--
-- Returns :
--
--    The columned model iterator or path
--
   function To_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   function To_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Path;
--
-- Unref_Node -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Unref_Node
             (  Model : not null access Gtk_Extension_Store_Record;
                Iter  : Gtk_Tree_Iter
             );
private
   type Row_Callbacks is (Changed, Inserted, Reordered, Deleted);
   type Callback_ID is array (Row_Callbacks) of Handler_ID;

   type Gtk_Extension_Store_Record is
      new Gtk_Abstract_Model_Record with
   record                                                -- Reference
      Reference : Gtk_Tree_Model := Null_Gtk_Tree_Model; -- model
      Columns   : Gtk.Tree_Store.Gtk_Tree_Store; -- The columns added
      Offset    : GInt := 0;                     -- Columns in the
      Callbacks : Callback_ID;                   -- reference model
   end record;
--
-- Erase -- Make the model empty
--
--    Model - The model
--
-- This  procedure  is  called  upon  root change in order to notify the
-- clients. It simulates removal of all rows from the model.
--
   procedure Erase (Model : access Gtk_Extension_Store_Record'Class);
--
-- On_Changed_Row -- Callback
--
   procedure On_Changed_Row
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             );
--
-- On_Deleted_Row -- Callback
--
   procedure On_Deleted_Row
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             );
--
-- On_Row_Has_Child_Toggled -- Callback
--
   procedure On_Row_Has_Child_Toggled
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             );
--
-- On_Row_Inserted -- Callback
--
   procedure On_Row_Inserted
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             );
--
-- On_Rows_Reordered -- Callback
--
   procedure On_Rows_Reordered
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             );

   package Tree_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Root_Tree_Model_Record,
             Gtk_Extension_Store
          );
end Gtk.Tree_Model.Extension_Store;
