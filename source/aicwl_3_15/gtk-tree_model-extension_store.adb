--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Extension_Store              Luebeck            --
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
--____________________________________________________________________--

with Ada.Exceptions;  use Ada.Exceptions;
with Glib.Messages;   use Glib.Messages;
with Gtk.Missed;      use Gtk.Missed;
with System;          use System;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with GNAT.Traceback.Symbolic;
with System.Address_To_Access_Conversions;

package body Gtk.Tree_Model.Extension_Store is

   GTK_Type : GType := GType_Invalid;

   function Where (Text : String) return String is
   begin
      return " in Gtk.Tree_Model.Extension_Store." & Text;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Gtk_Extension_Store_Record'Class,
             Gtk_Extension_Store
          );

   subtype Flat_GInt_Array is Gint_Array (Natural'Range);
   package Address_To_GInt_Array is
      new System.Address_To_Access_Conversions (Flat_GInt_Array);

   function From_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if Iter = Null_Iter then
         return Null_Iter;
      else
         declare
            Path   : constant Gtk_Tree_Path :=
                     Model.Columns.Get_Path (Iter);
            Result : Gtk_Tree_Iter;
         begin
            if Path = Null_Gtk_Tree_Path then
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Critical,
                  "Invalid iterator" & Where ("From_Extension")
               );
               return Null_Iter;
            else
               Result := Get_Iter (Model.Reference, Path);
               if Result = Null_Iter then
                  Log
                  (  GtkAda_Contributions_Domain,
                     Log_Level_Critical,
                     (  "Path "
                     &  To_String (Path)
                     &  " has no iterator"
                     &  Where ("From_Extension")
                  )  );
               end if;
               Path_Free (Path);
               return Result;
            end if;
         end;
      end if;
   end From_Extension;

   function From_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Path is
   begin
      if Path = Null_Gtk_Tree_Path then
         return Null_Gtk_Tree_Path;
      else
         return Copy (Path);
      end if;
   end From_Extension;

   procedure Changed
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      if Get_Depth (Path) > 0 then
         Row_Changed
         (  To_Interface (Model),
            Path,
            Model.Columns.Get_Iter (Path)
         );
      end if;
   end Changed;

   function Children
            (  Model  : not null access Gtk_Extension_Store_Record;
               Parent : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Model.Columns.Children (Parent);
   end Children;

   procedure Deleted
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path
             )  is
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row_Deleted (To_Interface (Model), Path);
      end if;
   end Deleted;

   procedure Deleting
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      null;
   end Deleting;

   procedure Erase (Model : access Gtk_Extension_Store_Record'Class) is
      Path : Gtk_Tree_Path;

      procedure Remove (Row : Gtk_Tree_Iter) is
      begin
         for Count in reverse 0..Model.Columns.N_Children (Row) - 1
         loop
            Append_Index (Path, Count);
            Remove (Model.Columns.Nth_Child (Row, Count));
            if Up (Path) then
               null;
            end if;
         end loop;
         if Row /= Null_Iter then
            declare
               Removed : Gtk_Tree_Iter := Row;
            begin
               Model.Deleting (Path, Row);
               Model.Columns.Remove (Removed);
               Row_Deleted (To_Interface (Model), Path);
            end;
         end if;
      end Remove;
   begin
      Gtk_New (Path);
      Remove (Null_Iter);
      Path_Free (Path);
   end Erase;

   procedure Finalize
             (  Model : not null access Gtk_Extension_Store_Record
             )  is
   begin
      Unref (-Model.Reference);
      Model.Columns.Unref;
   end Finalize;

   function Get_Column_Type
            (  Model : not null access Gtk_Extension_Store_Record;
               Index : Gint
            )  return GType is
   begin
      if Index < Model.Offset then
         return Get_Column_Type (Model.Reference, Index);
      else
         return Model.Columns.Get_Column_Type (Index - Model.Offset);
      end if;
   end Get_Column_Type;

   function Get_Extension_Types
            (  Model : not null access Gtk_Extension_Store_Record
            )  return GType_Array is
      Length : constant Gint := Model.Columns.Get_N_Columns;
   begin
      if Length = 0 then
         return (1..0 => GType_Invalid);
      else
         declare
            Result : GType_Array
                     (  Guint (Model.Offset)
                     .. Guint (Model.Offset + Length - 1)
                     );
         begin
            for Index in Result'Range loop
               Result (Index) :=
                  Model.Columns.Get_Column_Type
                  (  Gint (Index) - Model.Offset
                  );
            end loop;
            return Result;
         end;
      end if;
   end Get_Extension_Types;

   function Get_Flags
            (  Model : not null access Gtk_Extension_Store_Record
            )  return Tree_Model_Flags is
   begin
      return Get_Flags (Model.Reference);
   end Get_Flags;

   function Get_Iter
            (  Model : not null access Gtk_Extension_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Iter is
   begin
      if Path = Null_Gtk_Tree_Path or else Get_Depth (Path) = 0 then
         return Null_Iter;
      else
         return Model.Columns.Get_Iter (Path);
      end if;
   end Get_Iter;

   function Get_N_Columns
            (  Model : not null access Gtk_Extension_Store_Record
            )  return Gint is
   begin
      return Model.Columns.Get_N_Columns + Model.Offset;
   end Get_N_Columns;

   function Get_Path
            (  Model : not null access Gtk_Extension_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Path is
   begin
      if Iter = Null_Iter then
         return Null_Gtk_Tree_Path;
      else
         return Model.Columns.Get_Path (Iter);
      end if;
   end Get_Path;

   function Get_Reference
            (  Model : not null access Gtk_Extension_Store_Record
            )  return Gtk_Tree_Model is
   begin
      return Model.Reference;
   end Get_Reference;

   procedure Get_Value
             (  Model  : not null access Gtk_Extension_Store_Record;
                Iter   : Gtk_Tree_Iter;
                Column : Gint;
                Value  : out GValue
             )  is
   begin
      if Iter = Null_Iter then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Null extension model iterator"
            &  Gint'Image (Column)
            &  Where ("Get_Value")
         )  );
      elsif Column < Model.Offset then
         declare
            Row : constant Gtk_Tree_Iter :=
                  From_Extension (Model, Iter);
         begin
            if Row = Null_Iter then
               Log
               (  GtkAda_Contributions_Domain,
                  Log_Level_Critical,
                  (  "Null iterator of the reference column"
                  &  Gint'Image (Column)
                  &  Where ("Get_Value")
               )  );
               Init (Value, GType_Invalid);
            else
               Get_Value (Model.Reference, Row, Column, Value);
            end if;
         end;
      else
         Model.Columns.Get_Value (Iter, Column - Model.Offset, Value);
      end if;
   end Get_Value;

   procedure Gtk_New
             (  Model     : out Gtk_Extension_Store;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class;
                Types     : GType_Array
             )  is
   begin
      Model := new Gtk_Extension_Store_Record;
      Initialize (Model, Reference, Types);
   exception
      when others =>
         Free (Model);
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Model : out Gtk_Extension_Store;
                Types : GType_Array
             )  is
   begin
      Model := new Gtk_Extension_Store_Record;
      Gtk.Tree_Model.Extension_Store.Initialize (Model, Types);
   exception
      when others =>
         Free (Model);
         raise;
   end Gtk_New;

   function Has_Child
            (  Model : not null access Gtk_Extension_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Boolean is
   begin
      return Model.Columns.Has_Child (Iter);
   end Has_Child;

   procedure Initial_Sync
             (  Model      : not null access
                             Gtk_Extension_Store_Record'Class;
                Ref_Parent : Gtk_Tree_Iter;
                Ext_Parent : Gtk_Tree_Iter
             )  is
      Ref_Child : Gtk_Tree_Iter;
      Ext_Child : Gtk_Tree_Iter;
      Position  : Gint := 0;
   begin
      if Ref_Parent = Null_Iter then
         Ref_Child := Get_Iter_First (Model.Reference);
      else
         Ref_Child := Children (Model.Reference, Ref_Parent);
      end if;
      while Ref_Child /= Null_Iter loop
         Model.Columns.Insert (Ext_Child, Ext_Parent, Position);
         Position := Position + 1;
         Initial_Sync (Model, Ref_Child, Ext_Child);
         Next (Model.Reference, Ref_Child);
      end loop;
   end Initial_Sync;

   procedure Initialize
             (  Model     : not null access
                            Gtk_Extension_Store_Record'Class;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class;
                Types     : GType_Array
             )  is
   begin
      Initialize (Model, Types);
      Set_Reference (Model, Reference);
   end Initialize;

   procedure Initialize
             (  Model : not null access
                        Gtk_Extension_Store_Record'Class;
                Types : GType_Array
             )  is
   begin
      if GTK_Type = GType_Invalid then
         GTK_Type := Register ("GtkExtensionStore");
      end if;
      Initialize (Model, GTK_Type);
      Gtk.Tree_Store.Gtk_New (Model.Columns, Types);
      Tree_Handlers.Connect
      (  Model.Columns,
         "rows-reordered",
         On_Rows_Reordered'Access,
         Model.all'Access
      );
   end Initialize;

   procedure Inserted
             (  Model : not null access Gtk_Extension_Store_Record;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row_Inserted (To_Interface (Model), Path, Iter);
      end if;
   end Inserted;

   procedure Next
             (  Model : not null access Gtk_Extension_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      Model.Columns.Next (Iter);
   end Next;

   function Nth_Child
            (  Model  : not null access Gtk_Extension_Store_Record;
               Parent : Gtk_Tree_Iter;
               N      : Gint
            )  return Gtk_Tree_Iter is
   begin
      return Model.Columns.Nth_Child (Parent, N);
   end Nth_Child;

   function N_Children
            (  Model : not null access Gtk_Extension_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return Gint is
   begin
      return Model.Columns.N_Children (Iter);
   end N_Children;

   procedure On_Changed_Row
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             )  is
      Path : constant Gtk_Tree_Path :=
             Convert (Get_Address (Nth (Params, 1)));
      Row  : Gtk_Tree_Iter;
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row := Model.Columns.Get_Iter (Path);
         if Row = Null_Iter then
            On_Row_Inserted (Reference, Params, Model);
         end if;
         Changed (Model, Path, Row);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Changed_Row")
         )  );
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
         );
   end On_Changed_Row;

   procedure On_Deleted_Row
             (  Reference : access Gtk_Root_Tree_Model_Record'Class;
                Params    : GValues;
                Model     : Gtk_Extension_Store
             )  is
      Path : constant Gtk_Tree_Path :=
             Convert (Get_Address (Nth (Params, 1)));
      Row  : Gtk_Tree_Iter;
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row := Model.Columns.Get_Iter (Path);
         if Row /= Null_Iter then
            Deleting (Model, Path, Row);
            Model.Columns.Remove (Row);
            Row_Deleted (To_Interface (Model), Path);
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Deleted_Row")
         )  );
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
         );
   end On_Deleted_Row;

   procedure On_Row_Has_Child_Toggled
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             )  is
      Path : constant Gtk_Tree_Path :=
             Convert (Get_Address (Nth (Params, 1)));
   begin
      Row_Has_Child_Toggled
      (  To_Interface (Model),
         Path,
         Model.Columns.Get_Iter (Path)
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Row_Has_Child_Toggled")
         )  );
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
         );
   end On_Row_Has_Child_Toggled;

   procedure On_Row_Inserted
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             )  is
      Path : constant Gtk_Tree_Path :=
                Convert (Get_Address  (Nth (Params, 1)));
      No   : constant Gint :=
                Gint'Max
                (  0,
                   Get_Row_No (To_Interface (Model.Columns), Path)
                );
      This : constant Gtk_Tree_Path := Copy (Path);
      Row  : Gtk_Tree_Iter;
   begin
      if Up (This) and then Get_Depth (This) > 0 then
         Model.Columns.Insert
         (  Row,
            Model.Columns.Get_Iter (This),
            No
         );
      else
         Model.Columns.Insert (Row, Null_Iter, No);
      end if;
      if This /= Null_Gtk_Tree_Path then
         Path_Free (This);
      end if;
      Inserted (Model, Path, Row);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Row_Inserted")
         )  );
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
         );
   end On_Row_Inserted;

   procedure On_Rows_Reordered
             (  Reference  : access Gtk_Root_Tree_Model_Record'Class;
                Params     : GValues;
                Model      : Gtk_Extension_Store
             )  is
      use Address_To_GInt_Array;
      Path    : constant Gtk_Tree_Path :=
                   Convert (Get_Address (Nth (Params, 1)));
      Indices : Gint_Array renames
                   To_Pointer (Get_Address (Nth (Params, 3))).all;
   begin
      Model.Columns.Reorder
      (  Model.Columns.Get_Iter (Path),
         Indices
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Rows_Reordered")
         )  );
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
         );
   end On_Rows_Reordered;

   function Parent
            (  Model : not null access Gtk_Extension_Store_Record;
               Child : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      return Model.Columns.Parent (Child);
   end Parent;

   procedure Previous
             (  Model : not null access Gtk_Extension_Store_Record;
                Iter  : in out Gtk_Tree_Iter
             )  is
   begin
      Model.Columns.Previous (Iter);
   end Previous;

   procedure Ref_Node
             (  Model : not null access Gtk_Extension_Store_Record;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      Ref_Node (Model.Reference, From_Extension (Model, Iter));
      Ref_Node (Model.Reference, Iter);
   end Ref_Node;

   procedure Set_Null_Reference
             (  Model : not null access Gtk_Extension_Store_Record
             )  is
   begin
      if Model.Reference /= Null_Gtk_Tree_Model then
         -- Disconnect callbacks
         for Index in Model.Callbacks'Range loop
            Disconnect (-Model.Reference, Model.Callbacks (Index));
         end loop;
         Erase (Model);
         Unref (-Model.Reference);
         Model.Reference := Null_Gtk_Tree_Model;
      end if;
   end Set_Null_Reference;

   procedure Set_Reference
             (  Model     : not null access Gtk_Extension_Store_Record;
                Reference : not null access
                            Gtk_Root_Tree_Model_Record'Class
             )  is
   begin
      Ref (Reference);
      Set_Null_Reference (Model);
      Model.Reference := To_Interface (Reference);
      Model.Offset := Get_N_Columns (Model.Reference);
      Initial_Sync (Model, Null_Iter, Null_Iter);
      Model.Callbacks (Changed) :=
         Tree_Handlers.Connect
         (  Reference,
            "row-changed",
            On_Changed_Row'Access,
            Model.all'Access,
            True
         );
      Model.Callbacks (Deleted) :=
         Tree_Handlers.Connect
         (  Reference,
            "row-deleted",
            On_Deleted_Row'Access,
            Model.all'Access,
            True
         );
      Model.Callbacks (Inserted) :=
         Tree_Handlers.Connect
         (  Reference,
            "row-inserted",
            On_Row_Inserted'Access,
            Model.all'Access,
            True
         );
   exception
      when others =>
         Model.Reference := Null_Gtk_Tree_Model;
         Unref (Reference);
         raise;
   end Set_Reference;

   procedure Unref_Node
             (  Model : not null access Gtk_Extension_Store_Record;
                Iter  : Gtk_Tree_Iter
             )  is
   begin
      Unref_Node (Model.Reference, From_Extension (Model, Iter));
      Model.Columns.Unref_Node (Iter);
   end Unref_Node;

   function To_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Gtk_Tree_Iter is
   begin
      if Iter = Null_Iter then
         return Null_Iter;
      else
         declare
            Path   : constant Gtk_Tree_Path :=
                     Get_Path (Model.Reference, Iter);
            Result : Gtk_Tree_Iter := Null_Iter;
         begin
            if Path /= Null_Gtk_Tree_Path then
               if Get_Depth (Path) = 0 then
                  Result := Null_Iter;
               else
                  Result := Model.Columns.Get_Iter (Path);
               end if;
               Path_Free (Path);
            end if;
            return Result;
         end;
      end if;
   end To_Extension;

   function To_Extension
            (  Model : not null access Gtk_Extension_Store_Record;
               Path  : Gtk_Tree_Path
            )  return Gtk_Tree_Path is
   begin
      if Path = Null_Gtk_Tree_Path then
         return Null_Gtk_Tree_Path;
      else
         return Copy (Path);
      end if;
   end To_Extension;

   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : Boolean
             )  is
   begin
      Model.Columns.Set (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : Gint
             )  is
   begin
      Model.Columns.Set (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : UTF8_String
             )  is
   begin
      Model.Columns.Set (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

   procedure Set_Extension
             (  Model  : not null access
                         Gtk_Extension_Store_Record'Class;
                Iter   : Gtk_Tree_Iter;
                Column : Positive;
                Value  : GValue
             )  is
   begin
      Model.Columns.Set_Value (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

end Gtk.Tree_Model.Extension_Store;
