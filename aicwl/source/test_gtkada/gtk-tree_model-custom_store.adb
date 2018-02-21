--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Custom_Store                 Luebeck            --
--  Implementation                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body Gtk.Tree_Model.Custom_Store is

   GTK_Type : GType := GType_Invalid;

   function To_Iter (Node : Transaction_Record_Ptr)
      return Gtk_Tree_Iter is
      function To_Address is
         new Ada.Unchecked_Conversion
             (  Transaction_Record_Ptr,
                System.Address
             );
   begin
      return
      (  Stamp      => 1,
         User_Data  => To_Address (Node),
         User_Data2 => System.Null_Address,
         User_Data3 => System.Null_Address
      );
   end To_Iter;

   function To_Ptr (Node : Gtk_Tree_Iter)
      return Transaction_Record_Ptr is
      package Conversions is
         new System.Address_To_Access_Conversions (Transaction_Record);
   begin
      if Node = Null_Iter then
         return null;
      else
         return Conversions.
                To_Pointer (Node.User_Data).all'Unchecked_Access;
      end if;
   end To_Ptr;

   function Children
            (  Model  : not null access Gtk_Transaction_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Null_Iter;
   end Children;

   function Get_Column_Type
            (  Model : not null access Gtk_Transaction_Store_Record;
               Index : GInt
            )  return GType is
   begin
      case Index is
         when 0      => return GType_Int;     -- Account_No
         when 1      => return GType_String;  -- User
         when 2      => return GType_Double;  -- Amount
         when 3      => return GType_Int;     -- Year
         when 4      => return GType_Int;     -- Month
         when 5      => return GType_Int;     -- Day
         when 6      => return GType_Int;     -- Hour
         when 7      => return GType_Int;     -- Minute
         when 8      => return GType_Double;  -- Seconds
         when others => return GType_Invalid;
      end case;
   end Get_Column_Type;

   function Get_Flags
            (  Model : not null access Gtk_Transaction_Store_Record
            )  return Tree_Model_Flags is
   begin
      return Tree_Model_Iters_Persist + Tree_Model_List_Only;
   end Get_Flags;

   function Get_Iter
            (  Model : not null access Gtk_Transaction_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
   begin
      if Get_Depth (Path) = 1 and Model.First /= null then
         declare
            Indices : GInt_Array renames Get_Indices (Path);
            This    : Transaction_Record_Ptr := Model.First;
         begin
            for Row in 0..Indices (Indices'First) - 1 loop
               if This.Next = Model.First then
                  return Null_Iter;
               end if;
               This := This.Next;
            end loop;
            return To_Iter (This);
         end;
      end if;
      return Null_Iter;
   end Get_Iter;

   function Get_N_Columns
            (  Model : not null access Gtk_Transaction_Store_Record
            )  return GInt is
   begin
      return 9;
   end Get_N_Columns;

   function Get_Path
            (  Model : access Gtk_Transaction_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
      This : Transaction_Record_Ptr := Model.First;
      That : constant Transaction_Record_Ptr := To_Ptr (Iter);
      No   : GInt := 0;
   begin
      if This = null or else That = null then
         return Null_Gtk_Tree_Path;
      end if;
      while This /= That loop
         if This.Next = Model.First then
            return Null_Gtk_Tree_Path;
         end if;
         This := This.Next;
         No   := No + 1;
      end loop;
      return Path : Gtk_Tree_Path do
         Gtk_New (Path);
         Append_Index (Path, No);
      end return;
   end Get_Path;

   procedure Get_Value
             (  Model  : access Gtk_Transaction_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             )  is
      Node    : constant Transaction_Record_Ptr := To_Ptr (Iter);
      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Duration;
      function Complete_Seconds return GInt is
      begin
         return GInt (GDouble'Truncation (GDouble (Seconds)));
      end Complete_Seconds;
   begin
      if Node /= null then
         case Column is
            when 0 =>   -- Account_No
               Init (Value, GType_Int);
               Set_Int (Value, GInt (Node.Account));
            when 1 =>   -- User
               Init (Value, GType_String);
               Set_String (Value, To_String (Node.User));
            when 2 =>   -- Amount
               Init (Value, GType_Double);
               Set_Double (Value, GDouble (Node.Amount));
            when 3 =>   -- Time, year
               Split (Node.Date, Year, Month, Day, Seconds);
               Init (Value, GType_Int);
               Set_Int (Value, GInt (Year));
            when 4 =>   -- Time, month
               Split (Node.Date, Year, Month, Day, Seconds);
               Init (Value, GType_Int);
               Set_Int (Value, GInt (Month));
            when 5 =>   -- Time, day
               Split (Node.Date, Year, Month, Day, Seconds);
               Init (Value, GType_Int);
               Set_Int (Value, GInt (Day));
            when 6 =>   -- Time, hour
               Split (Node.Date, Year, Month, Day, Seconds);
               Init (Value, GType_Int);
               Set_Int (Value, Complete_Seconds / 3_600);
            when 7 =>   -- Time, minute
               Split (Node.Date, Year, Month, Day, Seconds);
               Init (Value, GType_Int);
               Set_Int (Value, (Complete_Seconds / 60) mod 60);
            when 8 =>   -- Time, seconds
               Split (Node.Date, Year, Month, Day, Seconds);
               Init (Value, GType_Double);
               Set_Double
               (  Value,
                  (  GDouble (Seconds)
                  -  GDouble (Complete_Seconds / 60) * 60.0
               )  );
            when others =>
               Init (Value, GType_Invalid);
         end case;
      end if;
   end Get_Value;

   procedure Gtk_New (Model : out Gtk_Transaction_Store) is
   begin
      if GTK_Type = GType_Invalid then
         GTK_Type := Register ("GtkTransactionStore");
      end if;
      Model := new Gtk_Transaction_Store_Record;
      Initialize (Model, GTK_Type);
   end Gtk_New;

   procedure Finalize (Model : access Gtk_Transaction_Store_Record) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Transaction_Record,
                Transaction_Record_Ptr
             );
      This : Transaction_Record_Ptr := Model.First;
      Next : Transaction_Record_Ptr := This;
   begin
      Finalize (Gtk_Abstract_Model_Record'Class (Model.all)'Access);
      while This /= null loop
         Next := This.Next;
         Free (This);
         This := Next;
      end loop;
   end Finalize;

   function Has_Child
            (  Model : access Gtk_Transaction_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      return False;
   end Has_Child;

   procedure Insert
             (  Model   : not null access Gtk_Transaction_Store_Record;
                Account : Account_No;
                User    : String;
                Amount  : Currency;
                Date    : Time
             )  is
      Node : constant Transaction_Record_Ptr :=
                new Transaction_Record'
                    (  Account  => Account,
                       User     => To_Unbounded_String (User),
                       Amount   => Amount,
                       Date     => Date,
                       Next     => null,
                       Previous => null
                    );
   begin
      if Model.First = null then
         Model.First   := Node;
         Node.Next     := Node;
         Node.Previous := Node;
      else
         Node.Next          := Model.First;
         Node.Previous      := Model.First.Previous;
         Node.Next.Previous := Node;
         Node.Previous.Next := Node;
      end if;
      Row_Inserted  -- Notify about changes made
      (  To_Interface (Model),
         Model.Get_Path (To_Iter (Node)),
         To_Iter (Node)
      );
   end Insert;

   procedure Next
             (  Model : not null access Gtk_Transaction_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
      Node : constant Transaction_Record_Ptr := To_Ptr (Iter);
   begin
      if Node = null or else Node.Next = Model.First then
         Iter := Null_Iter;
      else
         Iter := To_Iter (Node.Next);
      end if;
   end Next;

   function Nth_Child
            (  Model  : not null access Gtk_Transaction_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : GInt
            )  return Gtk_Tree_Iter is
      This : Transaction_Record_Ptr := Model.First;
   begin
      if Parent = Null_Iter then
         for Index in 0..N - 1 loop
            if This.Next = Model.First then
               return Null_Iter;
            end if;
            This := This.Next;
         end loop;
         return To_Iter (This);
      end if;
      return Null_Iter;
   end Nth_Child;

   function N_Children
            (  Model : not null access Gtk_Transaction_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return GInt is
      This  : Transaction_Record_Ptr := Model.First;
      Count : GInt                   := 0;
   begin
      if Iter = Null_Iter and then This /= null then
         loop
            Count := Count + 1;
            exit when This.Next = Model.First;
            This := This.Next;
         end loop;
      end if;
      return Count;
   end N_Children;

   function Parent
            (  Model : not null access Gtk_Transaction_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Null_Iter;
   end Parent;

   procedure Previous
             (  Model : not null access Gtk_Transaction_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
      Node : constant Transaction_Record_Ptr := To_Ptr (Iter);
   begin
      if Node = null or else Node = Model.First then
         Iter := Null_Iter;
      else
         Iter := To_Iter (Node.Previous);
      end if;
   end Previous;

end Gtk.Tree_Model.Custom_Store;
