--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Columned_Store               Luebeck            --
--  Interface                                      Autumn, 2007       --
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
--  This  package provides a multi-columned Tree_View_Model derived from
--  a  reference  model. The new model has n columns filled by the cells
--  of the reference model top-down, left-to-right. When reference model
--  has  multiple  columns  x,  y,  z,  then  the result model will have
--  columns x1, y1, z1, x2, y2, z2, ... xn, yn,  zn.  The  result  model
--  contains only the direct children of a node in the reference model.
--
--  Additional signals :
--
--      root-changed - This   emitted   when  the  columned  store  root
--                     directory has been changed
--
with GLib.Values;                    use GLib.Values;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;
with Gtk.Handlers;                   use Gtk.Handlers;

package Gtk.Tree_Model.Columned_Store is
--
-- Gtk_Columned_Store_Record -- A multicolumn TreeView model based on  a
--                              single column one
--
   type Gtk_Columned_Store_Record is
      new Gtk_Abstract_Model_Record with private;
   type Gtk_Columned_Store is
      access all Gtk_Columned_Store_Record'Class;
--
-- Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Children
            (  Model  : not null access Gtk_Columned_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Finalize -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- This  procedure  is  called upon object destruction. The override, if
-- any, shall call the parent's version.
--
   overriding
   procedure Finalize
             (  Model : not null access Gtk_Columned_Store_Record
             );
--
-- From_Columned -- Iterator / path conversion
--
--    Model       - The columned tree model
--    Iter / Path - An iterator or path in it
--    Column      - The major column 1..Get_Major_Columns
--
-- Null_Iter  or null is returned when the second and third arguments do
-- not denote a valid iterator or path. Note that returned non-null path
-- has to be freed by the caller using Path_Free.
--
-- Returns :
--
--    The reference model iterator or path
--
   function From_Columned
            (  Model  : not null access Gtk_Columned_Store_Record;
               Iter   : Gtk_Tree_Iter;
               Column : Positive
            )  return Gtk_Tree_Iter;
   function From_Columned
            (  Model  : not null access Gtk_Columned_Store_Record;
               Path   : Gtk_Tree_Path;
               Column : Positive
            )  return Gtk_Tree_Path;
--
-- Get_Column_Height -- Get the number of rows in a major column
--
--    Model  - The columned tree model
--    Column - The major column number
--
-- The result is 0 when Column number is out of range
--
-- Returns :
--
--    The number of rows in
--
   function Get_Column_Height
            (  Model  : not null access Gtk_Columned_Store_Record;
               Column : Positive
            )  return Natural;
--
-- Get_Column_Type -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Get_Column_Type
            (  Model : not null access Gtk_Columned_Store_Record;
               Index : GInt
            )  return GType;
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
            (  Model : not null access Gtk_Columned_Store_Record
            )  return Tree_Model_Flags;
--
-- Get_Iter -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Get_Iter
            (  Model : not null access Gtk_Columned_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter;
--
-- Get_Major_Columns -- Get the number of columns
--
--    Model - The columned tree model
--
-- Returns :
--
--    The number of major columns as specified in Gtk_New
--
   function Get_Major_Columns
            (  Model : not null access Gtk_Columned_Store_Record
            )  return Positive;
--
-- Get_N_Columns -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Get_N_Columns
            (  Model : not null access Gtk_Columned_Store_Record
            )  return GInt;
--
-- Get_Path -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Get_Path
            (  Model : not null access Gtk_Columned_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
--
-- Get_Reference -- Get the reference model
--
--    Model - The columned tree model
--
-- Returns :
--
--    The reference model
--
   function Get_Reference
            (  Model : not null access Gtk_Columned_Store_Record
            )  return Gtk_Tree_Model;
--
-- Get_Reference_Iter -- Get iterator by row and column
--
--    Model  - The columned tree model
--    Row    - The row number 1..
--    Column - The major column 1..Get_Major_Columns
--
-- Returns :
--
--    The iterator to or else Null_Iter
--
   function Get_Reference_Iter
            (  Model  : not null access Gtk_Columned_Store_Record;
               Row    : Positive;
               Column : Positive
            )  return Gtk_Tree_Iter;
--
-- Get_Root -- Get the root node
--
--    Model - The columned tree model
--
-- The result is Null_Iter if the model is rooted in the root node. When
-- path is returned, the caller shall free it using Path_Free.
--
-- Returns :
--
--    The reference model iterator or path to the root
--
   function Get_Root
            (  Model : not null access Gtk_Columned_Store_Record
            )  return Gtk_Tree_Iter;
   function Get_Root
            (  Model : not null access Gtk_Columned_Store_Record
            )  return Gtk_Tree_Path;
--
-- Get_Rows -- Get the number of rows
--
--    Model  - The columned tree model
--    Filled - True if incompleter rows do not count
--
-- The parameter Filled when true instructs not to count rows which have
-- unset cells. Otherwise each row counts.
--
-- Returns :
--
--    The number of rows in the model
--
   function Get_Rows
            (  Model  : not null access Gtk_Columned_Store_Record;
               Filled : Boolean
            )  return Natural;
--
-- Get_Row_Width -- Get the number of rows in a row
--
--    Model - The columned tree model
--    Row   - The row number 1..
--
-- The result is 0 when Row number is out of range
--
-- Returns :
--
--    The number of rows in
--
   function Get_Row_Width
            (  Model : not null access Gtk_Columned_Store_Record;
               Row   : Positive
            )  return Natural;
--
-- Get_Value -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Get_Value
             (  Model  : not null access Gtk_Columned_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
--
-- Gtk_New -- Factory
--
--    Model     - The result
--    Reference - The reference model, the new one will be based on
--    Columns   - The nubmer of columns
--    Root      - The parent node iterator
--
-- The  paramerter  Reference  is the model the result is based upon. It
-- will contain the direct children of the node Root arranged in columns
-- which  number  is Columns x Reference model columns number. When Root
-- is Null_Iter, the model contains the root nodes of Reference.
--
   procedure Gtk_New
             (  Model     : out Gtk_Columned_Store;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class;
                Columns   : Positive;
                Root      : Gtk_Tree_Iter := Null_Iter
             );
   procedure Gtk_New (Model : out Gtk_Columned_Store);
--
-- Has_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Has_Child
            (  Model : not null access Gtk_Columned_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
--
-- Initialize -- Construction
--
--    Model     - The result
--  [ Reference - The reference model, the new one will be based on
--    Columns   - The nubmer of columns
--    Root ]    - The parent node iterator
--
-- This procedure shall be called by the derived type.
--
   procedure Initialize
             (  Model     : not null access
                            Gtk_Columned_Store_Record'Class;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class;
                Columns   : Positive;
                Root      : Gtk_Tree_Iter
             );
   procedure Initialize
             (  Model : not null access Gtk_Columned_Store_Record'Class
             );
--
-- Is_Ancestor -- Reference iterator/path check
--
--    Model       - The columned tree model
--    Iter / Path - An iterator or path in the reference model
--
-- Returns :
--
--    True if the root of Model is an ancestor of Iter / Path
--
   function Is_Ancestor
            (  Model : not null access Gtk_Columned_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
   function Is_Ancestor
            (  Model : not null access Gtk_Columned_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Boolean;
--
-- Is_Descendant -- Reference iterator/path check
--
--    Model       - The columned tree model
--    Iter / Path - An iterator or path in the reference model
--
-- Returns :
--
--    True if the root of Model is a descendant of Iter / Path
--
   function Is_Descendant
            (  Model : not null access Gtk_Columned_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
   function Is_Descendant
            (  Model : not null access Gtk_Columned_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Boolean;
--
-- Next -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Next
             (  Model : not null access Gtk_Columned_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- Nth_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Nth_Child
            (  Model  : not null access Gtk_Columned_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter;
--
-- N_Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function N_Children
            (  Model : not null access Gtk_Columned_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt;
--
-- Parent -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   function Parent
            (  Model : not null access Gtk_Columned_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Previous -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
   procedure Previous
             (  Model : not null access Gtk_Columned_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- To_Columned -- Iterator / path conversion
--
--    Model       - The columned tree model
--    Iter / Path - An iterator or path in the reference model
--
-- Null_Iter or null is returned when the second argument is not a valid
-- iterator or path. Note that returned path has  to  be  freed  by  the
-- caller using Path_Free.
--
-- Returns :
--
--    The columned model iterator or path
--
   function To_Columned
            (  Model : not null access Gtk_Columned_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   function To_Columned
            (  Model : not null access Gtk_Columned_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Path;
--
-- To_Columned  -- Iterator conversion
--
--    Model  - The columned tree model
--    Iter   - An iterator in the reference model
--    Column - The column of
--
-- This procedure converts Iter to the columnated model and returns  the
-- column of.
--
   procedure To_Columned
             (  Model  : not null access Gtk_Columned_Store_Record;
                Iter   : in out Gtk_Tree_Iter;
                Column : out Positive
             );
--
-- Set_Null_Reference -- Make the model empty
--
--    Model        - The columned tree model
--    Root_Changed - Emit root-changed signal
--
-- This procedure sets the reference null. In effect the columned  model
-- becomes empty.
--
   procedure Set_Null_Reference
             (  Model : not null access Gtk_Columned_Store_Record;
                Root_Changed : Boolean := True
             );
--
-- Set_Reference -- Change the reference model
--
--    Model     - The columned tree model
--    Reference - The new reference model
--    Columns   - The number of columns
--    Root      - An iterator in the reference model to the new root
--
-- This procedure changes the reference model. It should  be  used  with
-- care because the clients might be unable to anticipate changes of the
-- column types.
--
   procedure Set_Reference
             (  Model     : not null access Gtk_Columned_Store_Record;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class;
                Columns   : Positive;
                Root      : Gtk_Tree_Iter
             );
--
-- Set_Root -- Change the model's root
--
--    Model - The columned tree model
--    Root  - An iterator in the reference model to the new root
--
-- This procedure changes the model root.
--
   procedure Set_Root
             (  Model : not null access Gtk_Columned_Store_Record;
                Root  : Gtk_Tree_Iter
             );
private
   type Row_Callbacks is (Changed, Inserted, Reordered, Deleted);
   type Callback_ID is array (Row_Callbacks) of Handler_ID;

   type Gtk_Columned_Store_Record is
      new Gtk_Abstract_Model_Record with
   record                                       -- Reference model
      Reference : Gtk_Tree_Model := Null_Gtk_Tree_Model;
      Columns   : GInt           := 1;          -- The number of columns
      Rows      : GInt           := 0;          -- The number of rows
      Path      : Gtk_Tree_Path  := Null_Gtk_Tree_Path; -- The root path
      Stamp     : GInt           := GInt'First;       -- Iterators stamp
      Callbacks : Callback_ID;
   end record;
--
-- Changed_Root -- Called when the root is changed
--
--    Model - The model
--
   procedure Changed_Root
             (  Model : not null access Gtk_Columned_Store_Record
             );
--
-- Erase -- Make the model empty
--
--    Model - The model
--
-- This  procedure  is  called  upon  root change in order to notify the
-- clients. It simulates removal of all rows from the model.
--
   procedure Erase (Model : not null access Gtk_Columned_Store_Record);
--
-- On_Changed_Row -- Callback
--
   procedure On_Changed_Row
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Columned_Store
             );
--
-- On_Deleted_Row -- Callback
--
   procedure On_Deleted_Row
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Columned_Store
             );
--
-- On_Row_Inserted -- Callback
--
   procedure On_Row_Inserted
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Columned_Store
             );
--
-- On_Rows_Reordered -- Callback
--
   procedure On_Rows_Reordered
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Columned_Store
             );

   package Tree_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Root_Tree_Model_Record,
             Gtk_Columned_Store
          );
end Gtk.Tree_Model.Columned_Store;
