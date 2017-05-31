--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Columned_Store               Luebeck            --
--  Implementation                                 Autumn, 2007       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

with Glib.Messages;

with GNAT.Traceback.Symbolic;

with Gtk.Missed;

with Interfaces.C.Pointers;
with Interfaces.C.Strings;

with System;

package body Gtk.Tree_Model.Columned_Store is

   pragma Warnings (Off, "declaration hides ""Changed""");
   pragma Warnings (Off, "declaration hides ""Deleted""");
   pragma Warnings (Off, "declaration hides ""GTK_Type""");
   pragma Warnings (Off, "declaration hides ""Inserted""");
   pragma Warnings (Off, "declaration hides ""Params""");
   pragma Warnings (Off, "declaration hides ""Parent""");
   pragma Warnings (Off, "declaration hides ""Path""");
   pragma Warnings (Off, "declaration hides ""Root_Changed""");

   GTK_Type     : GType := GType_Invalid;
   Root_Changed : constant String := "root-changed" & ASCII.NUL;

   -----------------------------------------------------------------------------
   function Compose_Columned
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Row   : Gint) return Gtk_Tree_Iter;

   function Compose_Reference
     (Model  : not null access Gtk_Columned_Store_Record'Class;
      Row    : Gint;
      Column : Gint) return Gtk_Tree_Iter;

   procedure Emit_Root_Changed
     (Store : access Gtk_Columned_Store_Record'Class);

   procedure Set_Path (Path : in out Gtk_Tree_Path; Row : Gint);

   procedure Split_Columned
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Iter  : Gtk_Tree_Iter;
      Row   : out Gint);

   function Where (Text : String) return String;

   function From_Addr is
     new Ada.Unchecked_Conversion (System.Address, Gint);

   function To_Addr is
     new Ada.Unchecked_Conversion (Gint, System.Address);
   -----------------------------------------------------------------------------

   procedure Changed_Root
     (Model : not null access Gtk_Columned_Store_Record)
   is
      Path : Gtk_Tree_Path;
   begin
      Gtk_New_First (Path);
      Model.all.Rows :=
        ((N_Children (Model.all.Reference, Model.all.Get_Root)
         +  Model.all.Columns
         -  1)
         /  Model.all.Columns);
      -- Simulation of children insertion
      for Row in 0 .. Model.all.Rows - 1 loop
         Set_Path (Path, Row);
         Row_Inserted
           (To_Interface (Model),
            Path,
            Compose_Columned (Model, Row));
      end loop;
      Path_Free (Path);
   end Changed_Root;

   overriding function Children
     (Model  : not null access Gtk_Columned_Store_Record;
      Parent : Gtk_Tree_Iter) return Gtk_Tree_Iter is
   begin
      if Parent = Null_Iter then
         return Compose_Columned (Model, 0);
      else
         return Null_Iter;
      end if;
   end Children;

   function Compose_Columned
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Row   : Gint) return Gtk_Tree_Iter is
   begin
      if Row in 0 .. Model.all.Rows - 1 then
         return
           (Model.all.Stamp,
            To_Addr (Row),
            Model.all'Address,
            System.Null_Address);
      else
         return Null_Iter;
      end if;
   end Compose_Columned;

   function Compose_Reference
     (Model  : not null access Gtk_Columned_Store_Record'Class;
      Row    : Gint;
      Column : Gint) return Gtk_Tree_Iter is
   begin
      if Row >= Model.all.Rows or else Column >= Model.all.Columns then
         return Null_Iter;
      else
         return
           Nth_Child
             (Model.all.Reference,
              Model.all.Get_Root,
              Row + Column * Model.all.Rows);
      end if;
   end Compose_Reference;

   procedure Do_Set_Root
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Root  : Gtk_Tree_Iter);
   procedure Do_Set_Root
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Root  : Gtk_Tree_Iter)
   is
      Path : Gtk_Tree_Path;
   begin
      if Root = Null_Iter then
         Gtk_New (Path);
      else
         Path := Get_Path (Model.all.Reference, Root);
      end if;
      if Model.all.Path = Null_Gtk_Tree_Path then
         Model.all.Stamp := Model.all.Stamp + 1;
         Model.all.Path  := Path;
         Changed_Root (Model);
         Emit_Root_Changed (Model);
      elsif Compare (Model.all.Path, Path) = 0 then
         Path_Free (Path);
      else
         Path_Free (Model.all.Path);
         Model.all.Stamp := Model.all.Stamp + 1;
         Model.all.Path  := Path;
         Changed_Root (Model);
         Emit_Root_Changed (Model);
      end if;
   end Do_Set_Root;

   procedure Emit_Root_Changed
     (Store : access Gtk_Columned_Store_Record'Class)
   is
      procedure Internal (Object : System.Address; Name : String);
      pragma Import (C, Internal, "ada_g_signal_emit_by_name");
   begin
      Internal (Get_Object (Store), Root_Changed);
   end Emit_Root_Changed;

   procedure Erase
     (Model : not null access Gtk_Columned_Store_Record)
   is
      Item_Path : Gtk_Tree_Path;
      Last_Row  : constant Gint := Model.all.Rows - 1;
   begin
      Gtk_New_First (Item_Path);
      for Child in reverse 0 .. Last_Row loop
         Model.all.Rows := Child;
         Set_Path (Item_Path, Child);
         Row_Deleted (To_Interface (Model), Item_Path);
      end loop;
      Path_Free (Item_Path);
      if Model.all.Path /= Null_Gtk_Tree_Path then
         Path_Free (Model.all.Path);
         Model.all.Path := Null_Gtk_Tree_Path;
      end if;
   end Erase;

   overriding procedure Finalize
     (Model : not null access Gtk_Columned_Store_Record) is
   begin
      if Model.all.Path /= Null_Gtk_Tree_Path then
         Path_Free (Model.all.Path);
      end if;
      if Model.all.Reference /= Null_Gtk_Tree_Model then
         Unref (-Model.all.Reference);
      end if;
   end Finalize;

   function From_Columned
     (Model  : not null access Gtk_Columned_Store_Record;
      Iter   : Gtk_Tree_Iter;
      Column : Positive) return Gtk_Tree_Iter
   is
      Row : Gint;
   begin
      Split_Columned (Model, Iter, Row);
      return Compose_Reference (Model, Row, Gint (Column) - 1);
   end From_Columned;

   function From_Columned
     (Model  : not null access Gtk_Columned_Store_Record;
      Path   : Gtk_Tree_Path;
      Column : Positive) return Gtk_Tree_Path
   is
      Row : constant Gtk_Tree_Iter :=
              From_Columned (Model, Get_Iter (Model, Path), Column);
   begin
      if Row = Null_Iter then
         return Null_Gtk_Tree_Path;
      else
         return Get_Path (Model.all.Reference, Row);
      end if;
   end From_Columned;

   function Get_Column_Height
     (Model  : not null access Gtk_Columned_Store_Record;
      Column : Positive) return Natural is
   begin
      if Gint (Column) < Model.all.Columns then
         return Natural (Model.all.Rows);
      elsif Gint (Column) > Model.all.Columns or else Model.all.Rows = 0 then
         return 0;
      end if;
      declare
         Last : constant Gint :=
                  (N_Children (Model.all.Reference, Model.all.Get_Root)
                   mod Model.all.Rows);
      begin
         if Last = 0 then
            return Natural (Model.all.Rows);
         else
            return Natural (Last);
         end if;
      end;
   end Get_Column_Height;

   overriding function Get_Column_Type
     (Model : not null access Gtk_Columned_Store_Record;
      Index : Gint) return GType is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null reference model" & Where ("Get_Column_Type"));
         return GType_Invalid;
      else
         return
           Get_Column_Type
             (Model.all.Reference,
              Index mod Get_N_Columns (Model.all.Reference));
      end if;
   end Get_Column_Type;

   overriding function Get_Flags
     (Model : not null access Gtk_Columned_Store_Record)
      return Tree_Model_Flags is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return Tree_Model_List_Only;
      else
         return Get_Flags (Model.all.Reference) or Tree_Model_List_Only;
      end if;
   end Get_Flags;

   overriding function Get_Iter
     (Model : not null access Gtk_Columned_Store_Record;
      Path  : Gtk_Tree_Path) return Gtk_Tree_Iter is
   begin
      if
        Path = Null_Gtk_Tree_Path or else
        Model.all.Reference = Null_Gtk_Tree_Model
      then
         return Null_Iter;
      else
         declare
            Indices : constant Gint_Array := Get_Indices (Path);
         begin
            if Indices'Length = 1 then
               return Compose_Columned (Model, Indices (Indices'First));
            else
               return Null_Iter;
            end if;
         end;
      end if;
   end Get_Iter;

   function Get_Major_Columns
     (Model : not null access Gtk_Columned_Store_Record) return Positive is
   begin
      return Positive (Model.all.Columns);
   end Get_Major_Columns;

   overriding function Get_N_Columns
     (Model : not null access Gtk_Columned_Store_Record) return Gint is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return 0;
      else
         return Get_N_Columns (Model.all.Reference) * Model.all.Columns;
      end if;
   end Get_N_Columns;

   overriding function Get_Path
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : Gtk_Tree_Iter) return Gtk_Tree_Path
   is
      Row : Gint;
   begin
      Split_Columned (Model, Iter, Row);
      if Row >= 0 then
         declare
            Result : Gtk_Tree_Path;
         begin
            Gtk_New (Result);
            Append_Index (Result, Row);
            return Result;
         end;
      else
         return Null_Gtk_Tree_Path;
      end if;
   end Get_Path;

   function Get_Position
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Path  : Gtk_Tree_Path) return Gint;
   function Get_Position
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Path  : Gtk_Tree_Path) return Gint is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return -1;
      elsif Path = Null_Gtk_Tree_Path then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null path in the reference model" & Where ("Get_Position"));
         return -1;
      end if;
      declare
         Indices : Gint_Array renames Get_Indices (Path);
      begin
         if Model.all.Path = Null_Gtk_Tree_Path then
            if Indices'Length /= 1 then
               return -1;
            end if;
         else
            if
              Get_Indices (Model.all.Path) /=
              Indices (Indices'First .. Indices'Last - 1)
            then
               return -1;
            end if;
         end if;
         return Indices (Indices'Last);
      end;
   end Get_Position;

   function Get_Reference
     (Model : not null access Gtk_Columned_Store_Record)
      return Gtk_Tree_Model is
   begin
      return Model.all.Reference;
   end Get_Reference;

   function Get_Reference_Iter
     (Model  : not null access Gtk_Columned_Store_Record;
      Row    : Positive;
      Column : Positive) return Gtk_Tree_Iter is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return Null_Iter;
      else
         return
           Compose_Reference
             (Model,
              Gint (Row) - 1,
              Gint (Column) - 1);
      end if;
   end Get_Reference_Iter;

   function Get_Root
     (Model : not null access Gtk_Columned_Store_Record) return Gtk_Tree_Iter is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return Null_Iter;
      elsif Model.all.Path = Null_Gtk_Tree_Path then
         return Null_Iter;
      elsif Get_Depth (Model.all.Path) = 0 then -- Original root
         return Null_Iter;
      else
         return Get_Iter (Model.all.Reference, Model.all.Path);
      end if;
   end Get_Root;

   function Get_Root
     (Model : not null access Gtk_Columned_Store_Record) return Gtk_Tree_Path is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return Null_Gtk_Tree_Path;
      elsif Model.all.Path = Null_Gtk_Tree_Path then
         return Null_Gtk_Tree_Path;
      else
         return Copy (Model.all.Path);
      end if;
   end Get_Root;

   function Get_Row_Width
     (Model : not null access Gtk_Columned_Store_Record;
      Row   : Positive) return Natural is
   begin
      if Row <= Get_Column_Height (Model, Positive (Model.all.Columns)) then
         return Natural (Model.all.Columns);
      elsif Gint (Row) <= Model.all.Rows then
         return Natural (Model.all.Columns - 1);
      else
         return 0;
      end if;
   end Get_Row_Width;

   function Get_Rows
     (Model  : not null access Gtk_Columned_Store_Record;
      Filled : Boolean) return Natural is
   begin
      if Filled then
         return Get_Column_Height (Model, Positive (Model.all.Columns));
      else
         return Natural (Model.all.Rows);
      end if;
   end Get_Rows;

   overriding procedure Get_Value
     (Model  : not null access Gtk_Columned_Store_Record;
      Iter   : Gtk_Tree_Iter;
      Column : Gint;
      Value  : out GValue) is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null reference model" & Where ("Get_Value"));
      else
         declare
            Minors : constant Gint := Get_N_Columns (Model.all.Reference);
            Row    : constant Gtk_Tree_Iter :=
                       From_Columned
                         (Model,
                          Iter,
                          Positive (Column / Minors + 1));
         begin
            if Row = Null_Iter then
               Init (Value, Get_Column_Type (Model, Column));
            else
               Get_Value
                 (Model.all.Reference,
                  Row,
                  Column mod Minors,
                  Value);
            end if;
         end;
      end if;
   end Get_Value;

   procedure Gtk_New
     (Model     : out Gtk_Columned_Store;
      Reference : not null access Gtk_Root_Tree_Model_Record'Class;
      Columns   : Positive;
      Root      : Gtk_Tree_Iter := Null_Iter) is
   begin
      Model := new Gtk_Columned_Store_Record;
      Initialize (Model, Reference.all'Access, Columns, Root);
   exception
      when others =>
         Unref (Model);
         raise;
   end Gtk_New;

   procedure Gtk_New (Model : out Gtk_Columned_Store) is
   begin
      Model := new Gtk_Columned_Store_Record;
      Gtk.Tree_Model.Columned_Store.Initialize (Model);
   exception
      when others =>
         Unref (Model);
         raise;
   end Gtk_New;

   overriding function Has_Child
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Iter);
      pragma Unreferenced (Model);
   begin
      return False;
   end Has_Child;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
               (0 => Interfaces.C.Strings.New_String ("root-changed"));

   procedure Initialize
     (Model     : not null access Gtk_Columned_Store_Record'Class;
      Reference : not null access Gtk_Root_Tree_Model_Record'Class;
      Columns   : Positive;
      Root      : Gtk_Tree_Iter) is
   begin
      if GTK_Type = GType_Invalid then
         GTK_Type :=
           Gtk.Tree_Model.Abstract_Store.Register ("GtkColumnedStore", Signals);
      end if;
      Gtk.Tree_Model.Abstract_Store.Initialize (Model, GTK_Type);
      Set_Reference (Model, Reference, Columns, Root);
   end Initialize;

   procedure Initialize
     (Model : not null access Gtk_Columned_Store_Record'Class) is
   begin
      if GTK_Type = GType_Invalid then
         GTK_Type :=
           Gtk.Tree_Model.Abstract_Store.Register ("GtkColumnedStore", Signals);
      end if;
      Gtk.Tree_Model.Abstract_Store.Initialize (Model, GTK_Type);
   end Initialize;

   function Is_Ancestor
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : Gtk_Tree_Iter) return Boolean is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return False;
      end if;
      declare
         Path   : constant Gtk_Tree_Path :=
                    Get_Path (Model.all.Reference, Iter);
         Result : constant Boolean := Is_Ancestor (Model.all.Path, Path);
      begin
         Path_Free (Path);
         return Result;
      end;
   end Is_Ancestor;

   function Is_Ancestor
     (Model : not null access Gtk_Columned_Store_Record;
      Path  : Gtk_Tree_Path) return Boolean is
   begin
      return Is_Ancestor (Model.all.Path, Path);
   end Is_Ancestor;

   function Is_Descendant
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : Gtk_Tree_Iter) return Boolean is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         return False;
      end if;
      declare
         Path   : constant Gtk_Tree_Path :=
                    Get_Path (Model.all.Reference, Iter);
         Result : constant Boolean := Is_Descendant (Model.all.Path, Path);
      begin
         Path_Free (Path);
         return Result;
      end;
   end Is_Descendant;

   function Is_Descendant
     (Model : not null access Gtk_Columned_Store_Record;
      Path  : Gtk_Tree_Path) return Boolean is
   begin
      return Is_Descendant (Model.all.Path, Path);
   end Is_Descendant;

   overriding function N_Children
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : Gtk_Tree_Iter := Null_Iter) return Gint is
   begin
      if Iter = Null_Iter then
         return Model.all.Rows;
      else
         return 0;
      end if;
   end N_Children;

   overriding procedure Next
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : in out Gtk_Tree_Iter)
   is
      Row : Gint;
   begin
      Split_Columned (Model, Iter, Row);
      if Row >= 0 then
         Iter := Compose_Columned (Model, Row + 1);
      else
         Iter := Null_Iter;
      end if;
   end Next;

   overriding function Nth_Child
     (Model  : not null access Gtk_Columned_Store_Record;
      Parent : Gtk_Tree_Iter;
      N      : Gint) return Gtk_Tree_Iter
   is
      pragma Unreferenced (Parent);
   begin
      return Compose_Columned (Model, N);
   end Nth_Child;

   procedure On_Changed_Row
     (Reference  : access Gtk_Root_Tree_Model_Record'Class;
      Params     : GValues;
      Model      : Gtk_Columned_Store)
   is
      Path : constant Gtk_Tree_Path :=
             Convert (Get_Address (Nth (Params, 1)));
      Row  : Gint := Get_Position (Model, Path);
   begin
      if Row >= 0 then
         if Model.all.Rows > 0 then
            Row := Row mod Model.all.Rows;
            declare
               Path : Gtk_Tree_Path;
            begin
               Gtk_New_First (Path);
               Set_Path (Path, Row);
               Row_Changed
                 (To_Interface (Model),
                  Path,
                  Compose_Columned (Model, Row));
               Path_Free (Path);
            end;
         else
            --
            -- Changing an unknown item is treated as insertion
            --
            On_Row_Inserted (Reference, Params, Model);
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Changed_Row"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Changed_Row;

   procedure On_Deleted_Row
     (Reference : access Gtk_Root_Tree_Model_Record'Class;
      Params    : GValues;
      Model     : Gtk_Columned_Store)
   is
      pragma Unreferenced (Reference);
      Path : Gtk_Tree_Path := Convert (Get_Address (Nth (Params, 1)));
      Row  : Gint;
   begin
      if Path = Null_Gtk_Tree_Path or else Get_Depth (Path) = 0 then
         return;
      end if;
      case Gtk.Missed.Compare (To_Interface (Model), Path, Model.all.Path) is
         when Gtk.Missed.After =>
            --
            -- A node following the root has been deleted.  Iterator  to
            -- the root has to be requested again, anyway.
            --
            Row := Get_Position (Model, Path);
            if Row >= 0 then
               declare
                  Rows_Before : constant Gint := Model.all.Rows;
                  Column : Gint;
                  Size   : constant Gint :=
                                  N_Children
                                    (Model.all.Reference,
                                     Model.all.Get_Root);
               begin
                  Model.all.Rows := -- The new number of
                     (Size + Model.all.Columns - 1) / Model.all.Columns;
                  if Rows_Before /= 0 then
                     Column := Row / Rows_Before;
                     Row := Row mod Rows_Before;
                     --
                     -- Row,  Column  is where an item was deleted. This
                     -- changes  all following items. When Column is not
                     -- the last  column  then  it  is  all  rows  which
                     -- change. Otherwise it is the rows  starting  from
                     -- Row. The last row is deleted as requred.
                     --
                     if Column < Model.all.Columns - 1 then
                        Row := 0;
                     end if;
                     Gtk_New_First (Path);
                     for Changed in Row .. Model.all.Rows - 1 loop
                        Set_Path (Path, Changed);
                        Row_Changed
                          (To_Interface (Model),
                           Path,
                           Compose_Columned (Model, Changed));
                     end loop;
                     for Deleted in Model.all.Rows .. Rows_Before - 1 loop
                        Set_Path (Path, Deleted);
                        Row_Deleted (To_Interface (Model), Path);
                     end loop;
                     Path_Free (Path);
                  end if;
               end;
            end if;
         when Gtk.Missed.Equal =>
            --
            -- The root node itself has been deleted, move to its parent
            --
            Path := Copy (Model.all.Path);
            if Up (Path) and then Get_Depth (Path) > 0 then
               Set_Root (Model, Get_Iter (Model.all.Reference, Path));
            else
               Set_Root (Model, Null_Iter);
            end if;
            Path_Free (Path);
         when Gtk.Missed.Before =>
            --
            -- A  row  was deleted before the root, only the iterator to
            -- the root is evaluated again as it might get lost.
            --
            if Is_Ancestor (Path, Model.all.Path) then
               --
               -- A  parent  node  of  the  root was deleted. Its parent
               -- becomes new root.
               --
               Path := Copy (Path);
               if Up (Path) and then Get_Depth (Path) > 0 then
                  Set_Root (Model, Get_Iter (Model.all.Reference, Path));
               else
                  Set_Root (Model, Null_Iter);
               end if;
               Path_Free (Path);
            elsif
              Gtk.Missed.Is_Sibling (Model.all.Reference, Path, Model.all.Path)
            then
               --
               -- A sibling node of the root was deleted which itself is
               -- before the root. The  path  of  the  root  has  to  be
               -- decremented.
               --
               if not Prev (Model.all.Path) then
                  Glib.Messages.Log
                    (Gtk.Missed.GtkAda_Contributions_Domain,
                     Glib.Messages.Log_Level_Critical,
                     "Inconsitent path" & Where ("On_Deleted_Row"));
               end if;
            end if;
      end case;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Deleted_Row"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Deleted_Row;

   procedure On_Row_Inserted
     (Reference  : access Gtk_Root_Tree_Model_Record'Class;
      Params     : GValues;
      Model      : Gtk_Columned_Store)
   is
      pragma Unreferenced (Reference);
      Path : Gtk_Tree_Path := Convert (Get_Address (Nth (Params, 1)));
      Row  : Gint;
   begin
      case Gtk.Missed.Compare (To_Interface (Model), Path, Model.all.Path) is
         when Gtk.Missed.Before =>
            --
            -- A row was inserted before the root. The iterator to  root
            -- has to be evaluated again.
            --
            if
              Gtk.Missed.Is_Sibling (Model.all.Reference, Path, Model.all.Path)
            then
               --
               -- A sibling node was insered before the root  node.  The
               -- path of has to be incremented.
               --
               Next (Model.all.Path);
            end if;
         when Gtk.Missed.Equal =>
            --
            -- A sibling node was insered at the root node. The path  of
            -- has to be incremented.
            --
            Next (Model.all.Path);
         when Gtk.Missed.After =>
            --
            -- Some  other node has been inserted. Iterator the the root
            -- has to be requested again, anyway.
            --
            Row := Get_Position (Model, Path);
            if Row >= 0 then
               declare
                  Rows_Before : constant Gint := Model.all.Rows;
                  Column : Gint;
                  Size   : constant Gint :=
                                  N_Children
                                    (Model.all.Reference,
                                     Model.all.Get_Root);
               begin
                  if Size = 0 then
                     return;
                  end if;
                  Model.all.Rows := -- The new number of rows
                     (Size + Model.all.Columns - 1) / Model.all.Columns;
                  Column := Row / Model.all.Rows;
                  Row := Row mod Model.all.Rows;
                  --
                  -- Row  and Column is where an item was inserted. This
                  -- changes it all following items. When it is the last
                  -- column  then  only  rows  starting  with  Row   and
                  -- involved. Otherwise it is  all  rows.  One  row  is
                  -- possibly inserted when necessary.
                  --
                  if Column < Model.all.Columns - 1 then
                     Row := 0;
                  end if;
                  Gtk_New_First (Path);
                  for Changed in Row .. Rows_Before - 1 loop
                     Set_Path (Path, Changed);
                     Row_Changed
                       (To_Interface (Model),
                        Path,
                        Compose_Columned (Model, Changed));
                  end loop;
                  for Inserted in Rows_Before .. Model.all.Rows - 1 loop
                     Set_Path (Path, Inserted);
                     Row_Inserted
                       (To_Interface (Model),
                        Path,
                        Compose_Columned (Model, Inserted));
                  end loop;
                  Path_Free (Path);
               end;
            end if;
      end case;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Row_Inserted"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Row_Inserted;

   type Index_Array is array (Natural range <>) of aliased Gint;
   pragma Convention (C, Index_Array);

   package Index_Array_Pointers is
     new Interfaces.C.Pointers (Natural, GInt, Index_Array, 0);

   function To_Pointer is
     new Ada.Unchecked_Conversion
       (System.Address,
        Index_Array_Pointers.Pointer);

   procedure On_Rows_Reordered
     (Reference  : access Gtk_Root_Tree_Model_Record'Class;
      Params     : GValues;
      Model      : Gtk_Columned_Store)
   is
      pragma Unreferenced (Reference);
      use type Gtk.Missed.Row_Order;
   begin
      if
        Gtk.Missed.Compare
          (To_Interface (Model),
           Convert (Get_Address (Nth (Params, 1))),
           Model.all.Path) /= Gtk.Missed.Equal
      then
         return;
      end if;
      declare
         Order : Index_Array_Pointers.Pointer :=
                   To_Pointer (Get_Address (Nth (Params, 2)));
         Rows  : constant Gint := N_Children (Model, Null_Iter) - 1;
         List  : array (0 .. Model.all.Rows - 1) of Boolean :=
                   (others => False);
         Path  : Gtk_Tree_Path;

         use type Index_Array_Pointers.Pointer;
      begin
         Gtk_New_First (Path);
         for Row in 0 .. Rows loop
            --
            -- Checking for permutations of  the  reference  rows.  Each
            -- moved  rows  causes  a change in the corresponding row of
            -- the columned model.
            --
            if Row /= Order.all then
               List (Row mod Model.all.Rows) := True;
            end if;
            Order := Order + 1;
         end loop;
         for Row in List'Range loop
            if List (Row) then
               Row_Changed
                 (To_Interface (Model),
                  Path,
                  Compose_Columned (Model, Row));
            end if;
            Next (Path);
         end loop;
         Path_Free (Path);
      end;
   end On_Rows_Reordered;

   overriding function Parent
     (Model : not null access Gtk_Columned_Store_Record;
      Child : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      pragma Unreferenced (Child);
      pragma Unreferenced (Model);
   begin
      return Null_Iter;
   end Parent;

   overriding procedure Previous
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : in out Gtk_Tree_Iter)
   is
      Row : Gint;
   begin
      Split_Columned (Model, Iter, Row);
      if Row >= 0 then
         Iter := Compose_Columned (Model, Row - 1);
      else
         Iter := Null_Iter;
      end if;
   end Previous;

   procedure Set_Null_Reference
     (Model : not null access Gtk_Columned_Store_Record;
      Root_Changed : Boolean := True) is
   begin
      if Model.all.Reference /= Null_Gtk_Tree_Model then
         -- Disconnect callbacks
         for Index in Model.all.Callbacks'Range loop
            Gtk.Handlers.Disconnect
              (-Model.all.Reference, Model.all.Callbacks (Index));
         end loop;
         Erase (Model);
         Unref (-Model.all.Reference);
         Model.all.Reference := Null_Gtk_Tree_Model;
         Model.all.Columns   := 1;
         if Root_Changed then
            Emit_Root_Changed (Model);
         end if;
      end if;
   end Set_Null_Reference;

   procedure Set_Path (Path : in out Gtk_Tree_Path; Row : Gint) is
   begin
      Prepend_Index (Path, Row);
      if Up (Path) then
         null;
      end if;
   end Set_Path;

   procedure Set_Reference
     (Model     : not null access Gtk_Columned_Store_Record;
      Reference : not null access Gtk_Root_Tree_Model_Record'Class;
      Columns   : Positive;
      Root      : Gtk_Tree_Iter) is
   begin
      if
        To_Interface (Reference) = Model.all.Reference and then
        Gint (Columns) = Model.all.Columns
      then
         --
         -- This is the same model and same columns number, so  we  just
         -- change the root
         --
         Set_Root (Model, Root);
      else
         --
         -- Everything  get changed. First we reference the new model so
         -- that it will not get occasionally collected. Then we set the
         -- reference model to null.
         --
         Ref (Reference);
         Set_Null_Reference (Model, False);
         Model.all.Columns   := Gint (Columns);
         Model.all.Reference := To_Interface (Reference);
         Do_Set_Root (Model, Root);
         Model.all.Callbacks (Changed) :=
           Tree_Handlers.Connect
             (Reference,
              "row-changed",
              On_Changed_Row'Access,
              Model.all'Access);
         Model.all.Callbacks (Deleted) :=
           Tree_Handlers.Connect
             (Reference,
              "row-deleted",
              On_Deleted_Row'Access,
              Model.all'Access);
         Model.all.Callbacks (Inserted) :=
           Tree_Handlers.Connect
             (Reference,
              "row-inserted",
              On_Row_Inserted'Access,
              Model.all'Access);
         Model.all.Callbacks (Reordered) :=
           Tree_Handlers.Connect
             (Reference,
              "rows-reordered",
              On_Rows_Reordered'Access,
              Model.all'Access);
      end if;
   end Set_Reference;

   procedure Set_Root
     (Model : not null access Gtk_Columned_Store_Record;
      Root  : Gtk_Tree_Iter) is
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         if Root /= Null_Iter then
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               "Wrong iterator" & Where ("Set_Root"));
         end if;
         return;
      end if;
      Erase (Model);
      Do_Set_Root (Model, Root);
   end Set_Root;

   procedure Split_Columned
     (Model : not null access Gtk_Columned_Store_Record'Class;
      Iter  : Gtk_Tree_Iter;
      Row   : out Gint)
   is
      use type System.Address;
   begin
      if Model.all.Reference = Null_Gtk_Tree_Model then
         Row := -1;
      elsif Model.all'Address /= Iter.User_Data2 then
         Row := -1;
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Model mismatch" & Where ("Split_Columned"));
      elsif Model.all.Stamp /= Iter.Stamp then
         Row := -1;
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Stamp mismatch" & Where ("Split_Columned"));
      else
         Row := From_Addr (Iter.User_Data);
         if Row not in 0 .. Model.all.Rows - 1 then
            Row := -1;
            Glib.Messages.Log
              (Gtk.Missed.GtkAda_Contributions_Domain,
               Glib.Messages.Log_Level_Critical,
               "Illegal row in columned iterator");
         end if;
      end if;
   end Split_Columned;

   procedure Split_Reference
     (Model  : access Gtk_Columned_Store_Record'Class;
      Path   : Gtk_Tree_Path;
      Row    : out Gint;
      Column : out Gint);
   procedure Split_Reference
     (Model  : access Gtk_Columned_Store_Record'Class;
      Path   : Gtk_Tree_Path;
      Row    : out Gint;
      Column : out Gint) is
   begin
      Row := Get_Position (Model, Path);
      if Row >= 0 then
         if 0 /= Model.all.Rows then
            Column := Row / Model.all.Rows;
            Row    := Row mod Model.all.Rows;
            if Column >= Model.all.Columns then
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Unexpected reference row" & Where ("Split_Reference"));
               Row    := -1;
               Column := 0;
            end if;
         end if;
      else
         Column := 0;
      end if;
   end Split_Reference;

   procedure To_Columned
     (Model  : not null access Gtk_Columned_Store_Record;
      Iter   : in out Gtk_Tree_Iter;
      Column : out Positive)
   is
      Path : constant Gtk_Tree_Path := Get_Path (Model.all.Reference, Iter);
      Row  : Gint;
      Col  : Gint;
   begin
      Split_Reference (Model, Path, Row, Col);
      Iter := Compose_Columned (Model, Row);
      Column := Positive (Col + 1);
      if Path /= Null_Gtk_Tree_Path then
         Path_Free (Path);
      end if;
   end To_Columned;

   function To_Columned
     (Model : not null access Gtk_Columned_Store_Record;
      Iter  : Gtk_Tree_Iter) return Gtk_Tree_Iter
   is
      Row    : Gtk_Tree_Iter := Iter;
      Column : Positive;
   begin
      To_Columned (Model, Row, Column);
      return Row;
   end To_Columned;

   function To_Columned
     (Model : not null access Gtk_Columned_Store_Record;
      Path  : Gtk_Tree_Path) return Gtk_Tree_Path
   is
      Row    : Gint;
      Col    : Gint;
      Result : Gtk_Tree_Path;
   begin
      Split_Reference (Model, Path, Row, Col);
      if Row > 0 then
         Gtk_New (Result);
         Append_Index (Result, Row);
         return Result;
      else
         return Null_Gtk_Tree_Path;
      end if;
   end To_Columned;

   function Where (Text : String) return String is
   begin
      return " in Gtk.Tree_Model.Columned_Store." & Text;
   end Where;

   pragma Warnings (On, "declaration hides ""Changed""");
   pragma Warnings (On, "declaration hides ""Deleted""");
   pragma Warnings (On, "declaration hides ""GTK_Type""");
   pragma Warnings (On, "declaration hides ""Inserted""");
   pragma Warnings (On, "declaration hides ""Params""");
   pragma Warnings (On, "declaration hides ""Parent""");
   pragma Warnings (On, "declaration hides ""Path""");
   pragma Warnings (On, "declaration hides ""Root_Changed""");

end Gtk.Tree_Model.Columned_Store;
