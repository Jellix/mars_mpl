--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Generic_Sort                 Luebeck            --
--  Interface                                      Summer, 2006       --
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
--  This generic package provides a GtkAda interface to GtkTreeSortable.
--  The package is instantiated with a custom tree  model  derived  from
--  Gtk_Abstract_Model_Record. A user-defined sort operation can be then
--  provided.
--
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;

with Gtk.Missed;

generic
   type Tree_Model_Record (<>) is
      new Gtk_Abstract_Model_Record with private;
   type Tree_Model is access all Tree_Model_Record'Class;
package Gtk.Tree_Model.Generic_Sort is
   Default_Sort_Column_ID  : GInt := -1;
   Unsorted_Sort_Column_ID : GInt := -2;
--
-- Gtk_Tree_Model_Sort_Record -- A   sorted   Tree_View_Model  based  on
--                               Tree_Model_Record
--
   type Gtk_Tree_Model_Sort_Record is
      new Gtk_Root_Tree_Model_Record with private;
   type Gtk_Tree_Model_Sort is
      access all Gtk_Tree_Model_Sort_Record'Class;
--
-- Clear_Cache -- Removes cache
--
--    Store - The model (sorted)
--
   procedure Clear_Cache
             (  Store : not null access Gtk_Tree_Model_Sort_Record
             );
--
-- Compare -- User-defined rows comparison
--
--    Store - The model (sorted)
--    Left   - An iterator of a row
--    Right  - An iterator of another row
--
-- This function is used for sorting if Set_Sort_Func was called for the
-- current sort column or as a default. Left and Right are the iterators
-- in the unsorted model. The current sort column can be  queried  using
-- Get_Sort_Column_ID.   The   unsorted  model  can  be  obtained  using
-- Get_Model. Note that the sort order as returned by Get_Sort_Column_ID
-- should  not  influence  the  result  of  this  function.  The  caller
-- automatically   translates   the  result  into  descending  order  if
-- necessary.
--
-- Returns :
--
--    Before if Left < Right
--    Equal  if Left = Right
--    After  if Left > Right
--
   function Compare
            (  Store  : not null access Gtk_Tree_Model_Sort_Record;
               Left   : Gtk_Tree_Iter;
               Right  : Gtk_Tree_Iter
            )  return Gtk.Missed.Row_Order;
--
-- Convert_Child_Iter_To_Iter -- Iterator downcast
--
--    Store - The model (sorted)
--    Iter  - An unsorted model interator to convert
--
-- Returns :
--
--    The corresponding sorted iterator
--
   function Convert_Child_Iter_To_Iter
            (  Store : not null access Gtk_Tree_Model_Sort_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Convert_Child_Path_To_Path -- Path downcast
--
--    Store - The model (sorted)
--    Path  - An unsorted model path to convert
--
   procedure Convert_Child_Path_To_Path
             (  Store : not null access Gtk_Tree_Model_Sort_Record;
                Path  : Gtk_Tree_Path
             );
--
-- Convert_Iter_To_Child_Iter -- Iterator upcast
--
--    Store - The model (sorted)
--    Iter  - A sorted interator to convert
--
-- Returns :
--
--    The corresponding model iterator
--
   function Convert_Iter_To_Child_Iter
            (  Store : not null access Gtk_Tree_Model_Sort_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
--
-- Convert_Path_To_Child_Path -- Path upcast
--
--    Store - The model (sorted)
--    Path  - A sorted model path to convert
--
   procedure Convert_Path_To_Child_Path
             (  Store : not null access Gtk_Tree_Model_Sort_Record;
                Path  : Gtk_Tree_Path
             );
--
-- Iter_Is_Valid -- Check if a sorted iterator is valid
--
--    Store - The model (sorted)
--    Iter  - A sorted interator to convert
--
-- GTK documentation warns that this function might be  slow  and  isn't
-- recommended to be used for purposes other than debugging and testing.
--
-- Returns :
--
--    True if the iterator is valid
--
   function Iter_Is_Valid
            (  Store : not null access Gtk_Tree_Model_Sort_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
--
-- Get_Model -- Get the underlying model
--
--    Store - A pointer to
--
-- Returns :
--
--    The model beneath
--
   function Get_Model
            (  Store : not null access Gtk_Tree_Model_Sort_Record
            )  return Tree_Model;
--
-- Get_Sort_Column_ID -- Get the current sort column and order
--
--    Store  - The model (sorted)
--    Column - The sort column ID
--    Order  - The sort order
--
-- The  sort  column  can  be  returned  as  Default_Sort_Column_ID   or
-- Unsorted_Sort_Column_ID.
--
   procedure Get_Sort_Column_ID
             (  Store  : not null access Gtk_Tree_Model_Sort_Record;
                Column : out GInt;
                Order  : out Gtk_Sort_Type
             );
--
-- Gtk_New -- Object creation
--
--    Store - The result
--    Model - The underlying model (to sort)
--
   procedure Gtk_New
             (  Store : out Gtk_Tree_Model_Sort;
                Model : not null access Tree_Model_Record'Class
             );
--
-- Has_Default_Sort_Func -- Check if a default sort function set
--
--    Store - The model (sorted)
--
-- Returns :
--
--    True if there is a default sort function
--
   function Has_Default_Sort_Func
            (  Store : not null access Gtk_Tree_Model_Sort_Record
            )  return Boolean;
--
-- Initialize -- Construction
--
--    Store - The model (sorted)
--    Model - The underlying model (to sort)
--
-- The parameter Model is usually the result of a call to Gtk_New.
--
   procedure Initialize
             (  Store : not null access
                        Gtk_Tree_Model_Sort_Record'Class;
                Model : not null access Tree_Model_Record'Class
             );
--
-- Set_Sort_Column_ID -- Set the current sort column and order
--
--    Store  - The model (sorted)
--    Column - The sort column ID
--    Order  - The sort order
--
-- The sort column can be Default_Sort_Column_ID.
--
   procedure Set_Sort_Column_ID
             (  Store  : not null access Gtk_Tree_Model_Sort_Record;
                Column : GInt;
                Order  : Gtk_Sort_Type
             );
--
-- Set_Sort_Func -- Activate sort function for a column
--
--    Store    - The model (sorted)
--  [ Column ] - To use Compare with
--
-- By  default  Compare  is  not used for sorting. When Set_Sort_Func is
-- called it instructs to use Compare  for  the  specified  column.  The
-- column  is  not  necessarily  a  column  number,  but  just an column
-- identifier. When Column is omitted Compare is activated  for  default
-- sorting.
--
   procedure Set_Sort_Func
             (  Store  : not null access
                         Gtk_Tree_Model_Sort_Record'Class;
                Column : GInt
             );
   procedure Set_Sort_Func
             (  Store  : not null access
                         Gtk_Tree_Model_Sort_Record'Class
             );
--
-- Sort_Column_Changed -- Emit signal
--
--    Store - The model (sorted)
--
   procedure Sort_Column_Changed
             (  Store : not null access Gtk_Tree_Model_Sort_Record'Class
             );
private
   type Gtk_Tree_Model_Sort_Record is
      new Gtk_Root_Tree_Model_Record with
   record
      Model : Tree_Model; -- The model underneath
   end record;

end Gtk.Tree_Model.Generic_Sort;
