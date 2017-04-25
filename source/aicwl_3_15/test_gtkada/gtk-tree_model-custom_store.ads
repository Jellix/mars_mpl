--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Custom_Store                 Luebeck            --
--  Interface                                      Winter, 2007       --
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

with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Calendar;                   use Ada.Calendar;
with GLib;                           use GLib;
with GLib.Values;                    use GLib.Values;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;

package Gtk.Tree_Model.Custom_Store is
   type Account_No is range 1..100_000_000;
   type Currency is delta 0.01 range -1_000_000_000.0..1_000_000_000.0;
--
-- Gtk_Transaction_Store_Record -- The type of the model
--
   type Gtk_Transaction_Store_Record is
      new Gtk_Abstract_Model_Record with private;
--
-- Gtk_Transaction_Store -- The access type to deal with the objects of
--
   type Gtk_Transaction_Store is
      access all Gtk_Transaction_Store_Record'Class;
--
-- Insert -- Add a new row into the model
--
   procedure Insert
             (  Model   : not null access Gtk_Transaction_Store_Record;
                Account : Account_No;
                User    : String;
                Amount  : Currency;
                Date    : Time
             );
--
-- Gtk_New -- Create a new object
--
--    Model - The result
--
   procedure Gtk_New (Model : out Gtk_Transaction_Store);

private
   type Transaction_Record;
   type Transaction_Record_Ptr is access all Transaction_Record;
--
-- Gtk_Transaction_Store_Record -- Implemenbtation  is  a  doubly-linked
--                                 list of Transaction_Record. The store
-- holds a pointer to the first element in the list.
--
   type Gtk_Transaction_Store_Record is
      new Gtk_Abstract_Model_Record with
   record
      First : Transaction_Record_Ptr;
   end record;
--
-- Now, the implementation of  the  Gtk_Abstract_Model_Record  primitive
-- operations follow.
--
   overriding
   function Children
            (  Model  : not null access Gtk_Transaction_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   overriding
   function Get_Column_Type
            (  Model : not null access Gtk_Transaction_Store_Record;
               Index : GInt
            )  return GType;
   overriding
   function Get_Flags
            (  Model : not null access Gtk_Transaction_Store_Record
            )  return Tree_Model_Flags;
   overriding
   function Get_Iter
            (  Model : not null access Gtk_Transaction_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter;
   overriding
   function Get_N_Columns
            (  Model : not null access Gtk_Transaction_Store_Record
            )  return GInt;
   overriding
   function Get_Path
            (  Model : access Gtk_Transaction_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path;
   overriding
   procedure Get_Value
             (  Model  : access Gtk_Transaction_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             );
   overriding
   procedure Finalize (Model : access Gtk_Transaction_Store_Record);
   overriding
   function Has_Child
            (  Model : access Gtk_Transaction_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean;
   overriding
   procedure Next
             (  Model : not null access Gtk_Transaction_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
   overriding
   function Nth_Child
            (  Model  : not null access Gtk_Transaction_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter;
   overriding
   function N_Children
            (  Model : not null access Gtk_Transaction_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt;
   overriding
   function Parent
            (  Model : not null access Gtk_Transaction_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter;
   overriding
   procedure Previous
             (  Model : not null access Gtk_Transaction_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             );
--
-- Transaction_Record -- Describes one row of the store
--
   type Transaction_Record is record
      Account  : Account_No;
      User     : Unbounded_String;
      Amount   : Currency;
      Date     : Time;
      Previous : Transaction_Record_Ptr;
      Next     : Transaction_Record_Ptr;
   end record;

end Gtk.Tree_Model.Custom_Store;
