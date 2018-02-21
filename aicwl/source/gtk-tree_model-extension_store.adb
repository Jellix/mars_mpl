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
-- __________________________________________________________________ --

with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Glib.Messages;

with GNAT.Traceback.Symbolic;

with Gtk.Missed;

with System.Address_To_Access_Conversions;

package body Gtk.Tree_Model.Extension_Store is

   pragma Warnings (Off, "declaration hides ""GTK_Type""");
   pragma Warnings (Off, "declaration hides ""Params""");
   pragma Warnings (Off, "declaration hides ""Parent""");
   pragma Warnings (Off, "declaration hides ""Types""");

   GTK_Type : GType := GType_Invalid;

   function Where (Text : String) return String;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Gtk_Extension_Store_Record'Class,
        Gtk_Extension_Store);

   subtype Flat_GInt_Array is Gint_Array (Natural'Range);
   package Address_To_GInt_Array is
      new System.Address_To_Access_Conversions (Flat_GInt_Array);

   procedure Changed
     (Model : not null access Gtk_Extension_Store_Record;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Iter);
   begin
      if Get_Depth (Path) > 0 then
         Row_Changed
           (To_Interface (Model),
            Path,
            Model.all.Columns.all.Get_Iter (Path));
      end if;
   end Changed;

   overriding function Children
     (Model  : not null access Gtk_Extension_Store_Record;
      Parent : Gtk_Tree_Iter) return Gtk_Tree_Iter is
   begin
      return Model.all.Columns.all.Children (Parent);
   end Children;

   procedure Deleted
     (Model : not null access Gtk_Extension_Store_Record;
      Path  : Gtk_Tree_Path) is
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row_Deleted (To_Interface (Model), Path);
      end if;
   end Deleted;

   procedure Deleting
     (Model : not null access Gtk_Extension_Store_Record;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter) is
   begin
      null;
   end Deleting;

   procedure Erase (Model : access Gtk_Extension_Store_Record'Class) is
      Path : Gtk_Tree_Path;

      procedure Remove (Row : Gtk_Tree_Iter);
      procedure Remove (Row : Gtk_Tree_Iter) is
      begin
         for Count in reverse 0 .. Model.all.Columns.all.N_Children (Row) - 1
         loop
            Append_Index (Path, Count);
            Remove (Model.all.Columns.all.Nth_Child (Row, Count));
            if Up (Path) then
               null;
            end if;
         end loop;
         if Row /= Null_Iter then
            declare
               Removed : Gtk_Tree_Iter := Row;
            begin
               Model.all.Deleting (Path, Row);
               Model.all.Columns.all.Remove (Removed);
               Row_Deleted (To_Interface (Model), Path);
            end;
         end if;
      end Remove;
   begin
      Gtk_New (Path);
      Remove (Null_Iter);
      Path_Free (Path);
   end Erase;

   overriding procedure Finalize
     (Model : not null access Gtk_Extension_Store_Record) is
   begin
      Unref (-Model.all.Reference);
      Model.all.Columns.all.Unref;
   end Finalize;

   function From_Extension
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : Gtk_Tree_Iter) return Gtk_Tree_Iter is
   begin
      if Iter = Null_Iter then
         return Null_Iter;
      else
         declare
            Path   : constant Gtk_Tree_Path :=
                       Model.all.Columns.all.Get_Path (Iter);
            Result : Gtk_Tree_Iter;
         begin
            if Path = Null_Gtk_Tree_Path then
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Invalid iterator" & Where ("From_Extension"));
               return Null_Iter;
            else
               Result := Get_Iter (Model.all.Reference, Path);
               if Result = Null_Iter then
                  Glib.Messages.Log
                    (Gtk.Missed.GtkAda_Contributions_Domain,
                     Glib.Messages.Log_Level_Critical,
                     "Path "
                     & To_String (Path)
                     & " has no iterator"
                     & Where ("From_Extension"));
               end if;
               Path_Free (Path);
               return Result;
            end if;
         end;
      end if;
   end From_Extension;

   function From_Extension
     (Model : not null access Gtk_Extension_Store_Record;
      Path  : Gtk_Tree_Path) return Gtk_Tree_Path
   is
      pragma Unreferenced (Model);
   begin
      if Path = Null_Gtk_Tree_Path then
         return Null_Gtk_Tree_Path;
      else
         return Copy (Path);
      end if;
   end From_Extension;

   overriding function Get_Column_Type
     (Model : not null access Gtk_Extension_Store_Record;
      Index : Gint) return GType is
   begin
      if Index < Model.all.Offset then
         return Get_Column_Type (Model.all.Reference, Index);
      else
         return Model.all.Columns.all.Get_Column_Type (Index - Model.all.Offset);
      end if;
   end Get_Column_Type;

   function Get_Extension_Types
     (Model : not null access Gtk_Extension_Store_Record) return GType_Array
   is
      Length : constant Gint := Model.all.Columns.all.Get_N_Columns;
   begin
      if Length = 0 then
         return (1 .. 0 => GType_Invalid);
      else
         declare
            Result : GType_Array
              (Guint (Model.all.Offset) ..
                 Guint (Model.all.Offset + Length - 1));
         begin
            for Index in Result'Range loop
               Result (Index) :=
                  Model.all.Columns.all.Get_Column_Type
                   (Gint (Index) - Model.all.Offset);
            end loop;
            return Result;
         end;
      end if;
   end Get_Extension_Types;

   overriding function Get_Flags
     (Model : not null access Gtk_Extension_Store_Record)
      return Tree_Model_Flags is
   begin
      return Get_Flags (Model.all.Reference);
   end Get_Flags;

   overriding function Get_Iter
     (Model : not null access Gtk_Extension_Store_Record;
      Path  : Gtk_Tree_Path) return Gtk_Tree_Iter is
   begin
      if Path = Null_Gtk_Tree_Path or else Get_Depth (Path) = 0 then
         return Null_Iter;
      else
         return Model.all.Columns.all.Get_Iter (Path);
      end if;
   end Get_Iter;

   overriding function Get_N_Columns
     (Model : not null access Gtk_Extension_Store_Record) return Gint is
   begin
      return Model.all.Columns.all.Get_N_Columns + Model.all.Offset;
   end Get_N_Columns;

   overriding function Get_Path
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : Gtk_Tree_Iter) return Gtk_Tree_Path is
   begin
      if Iter = Null_Iter then
         return Null_Gtk_Tree_Path;
      else
         return Model.all.Columns.all.Get_Path (Iter);
      end if;
   end Get_Path;

   function Get_Reference
     (Model : not null access Gtk_Extension_Store_Record)
      return Gtk_Tree_Model is
   begin
      return Model.all.Reference;
   end Get_Reference;

   overriding procedure Get_Value
     (Model  : not null access Gtk_Extension_Store_Record;
      Iter   : Gtk_Tree_Iter;
      Column : Gint;
      Value  : out GValue) is
   begin
      if Iter = Null_Iter then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null extension model iterator"
            & Gint'Image (Column)
            & Where ("Get_Value"));
      elsif Column < Model.all.Offset then
         declare
            Row : constant Gtk_Tree_Iter :=
                  From_Extension (Model, Iter);
         begin
            if Row = Null_Iter then
               Glib.Messages.Log
                 (Gtk.Missed.GtkAda_Contributions_Domain,
                  Glib.Messages.Log_Level_Critical,
                  "Null iterator of the reference column"
                  & Gint'Image (Column)
                  & Where ("Get_Value"));
               Init (Value, GType_Invalid);
            else
               Get_Value (Model.all.Reference, Row, Column, Value);
            end if;
         end;
      else
         Model.all.Columns.all.Get_Value (Iter, Column - Model.all.Offset, Value);
      end if;
   end Get_Value;

   procedure Gtk_New
     (Model     : out Gtk_Extension_Store;
      Reference : not null access Gtk_Root_Tree_Model_Record'Class;
      Types     : GType_Array) is
   begin
      Model := new Gtk_Extension_Store_Record;
      Initialize (Model, Reference, Types);
   exception
      when others =>
         Free (Model);
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Model : out Gtk_Extension_Store;
      Types : GType_Array) is
   begin
      Model := new Gtk_Extension_Store_Record;
      Gtk.Tree_Model.Extension_Store.Initialize (Model, Types);
   exception
      when others =>
         Free (Model);
         raise;
   end Gtk_New;

   overriding function Has_Child
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : Gtk_Tree_Iter) return Boolean is
   begin
      return Model.all.Columns.all.Has_Child (Iter);
   end Has_Child;

   procedure Initial_Sync
     (Model      : not null access Gtk_Extension_Store_Record'Class;
      Ref_Parent : Gtk_Tree_Iter;
      Ext_Parent : Gtk_Tree_Iter);
   procedure Initial_Sync
     (Model      : not null access Gtk_Extension_Store_Record'Class;
      Ref_Parent : Gtk_Tree_Iter;
      Ext_Parent : Gtk_Tree_Iter)
   is
      Ref_Child : Gtk_Tree_Iter;
      Ext_Child : Gtk_Tree_Iter;
      Position  : Gint := 0;
   begin
      if Ref_Parent = Null_Iter then
         Ref_Child := Get_Iter_First (Model.all.Reference);
      else
         Ref_Child := Children (Model.all.Reference, Ref_Parent);
      end if;
      while Ref_Child /= Null_Iter loop
         Model.all.Columns.all.Insert (Ext_Child, Ext_Parent, Position);
         Position := Position + 1;
         Initial_Sync (Model, Ref_Child, Ext_Child);
         Next (Model.all.Reference, Ref_Child);
      end loop;
   end Initial_Sync;

   procedure Initialize
     (Model     : not null access Gtk_Extension_Store_Record'Class;
      Reference : not null access Gtk_Root_Tree_Model_Record'Class;
      Types     : GType_Array) is
   begin
      Initialize (Model, Types);
      Set_Reference (Model, Reference);
   end Initialize;

   procedure Initialize
     (Model : not null access Gtk_Extension_Store_Record'Class;
      Types : GType_Array) is
   begin
      if GTK_Type = GType_Invalid then
         GTK_Type :=
           Gtk.Tree_Model.Abstract_Store.Register ("GtkExtensionStore");
      end if;
      Gtk.Tree_Model.Abstract_Store.Initialize (Model, GTK_Type);
      Gtk.Tree_Store.Gtk_New (Model.all.Columns, Types);
      Tree_Handlers.Connect
        (Model.all.Columns,
         "rows-reordered",
         On_Rows_Reordered'Access,
         Model.all'Access);
   end Initialize;

   procedure Inserted
     (Model : not null access Gtk_Extension_Store_Record;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter) is
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row_Inserted (To_Interface (Model), Path, Iter);
      end if;
   end Inserted;

   overriding function N_Children
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : Gtk_Tree_Iter := Null_Iter) return Gint is
   begin
      return Model.all.Columns.all.N_Children (Iter);
   end N_Children;

   overriding procedure Next
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : in out Gtk_Tree_Iter) is
   begin
      Model.all.Columns.all.Next (Iter);
   end Next;

   overriding function Nth_Child
     (Model  : not null access Gtk_Extension_Store_Record;
      Parent : Gtk_Tree_Iter;
      N      : Gint) return Gtk_Tree_Iter is
   begin
      return Model.all.Columns.all.Nth_Child (Parent, N);
   end Nth_Child;

   procedure On_Changed_Row
     (Reference  : access Gtk_Root_Tree_Model_Record'Class;
      Params     : GValues;
      Model      : Gtk_Extension_Store)
   is
      Path : constant Gtk_Tree_Path :=
             Convert (Get_Address (Nth (Params, 1)));
      Row  : Gtk_Tree_Iter;
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row := Model.all.Columns.all.Get_Iter (Path);
         if Row = Null_Iter then
            On_Row_Inserted (Reference, Params, Model);
         end if;
         Changed (Model, Path, Row);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Changed_Row"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Changed_Row;

   procedure On_Deleted_Row
     (Reference : access Gtk_Root_Tree_Model_Record'Class;
      Params    : GValues;
      Model     : Gtk_Extension_Store)
   is
      pragma Unreferenced (Reference);

      Path : constant Gtk_Tree_Path :=
             Convert (Get_Address (Nth (Params, 1)));
      Row  : Gtk_Tree_Iter;
   begin
      if Path /= Null_Gtk_Tree_Path and then Get_Depth (Path) > 0 then
         Row := Model.all.Columns.all.Get_Iter (Path);
         if Row /= Null_Iter then
            Deleting (Model, Path, Row);
            Model.all.Columns.all.Remove (Row);
            Row_Deleted (To_Interface (Model), Path);
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Deleted_Row"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Deleted_Row;

   procedure On_Row_Has_Child_Toggled
     (Reference  : access Gtk_Root_Tree_Model_Record'Class;
      Params     : GValues;
      Model      : Gtk_Extension_Store)
   is
      pragma Unreferenced (Reference);

      Path : constant Gtk_Tree_Path :=
             Convert (Get_Address (Nth (Params, 1)));
   begin
      Row_Has_Child_Toggled
        (To_Interface (Model),
         Path,
         Model.all.Columns.all.Get_Iter (Path));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Row_Has_Child_Toggled"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Row_Has_Child_Toggled;

   procedure On_Row_Inserted
     (Reference  : access Gtk_Root_Tree_Model_Record'Class;
      Params     : GValues;
      Model      : Gtk_Extension_Store)
   is
      pragma Unreferenced (Reference);

      Path : constant Gtk_Tree_Path :=
                Convert (Get_Address  (Nth (Params, 1)));
      No   : constant Gint :=
               Gint'Max
                (0,
                 Gtk.Missed.Get_Row_No
                   (To_Interface (Model.all.Columns), Path));
      This : constant Gtk_Tree_Path := Copy (Path);
      Row  : Gtk_Tree_Iter;
   begin
      if Up (This) and then Get_Depth (This) > 0 then
         Model.all.Columns.all.Insert
           (Row,
            Model.all.Columns.all.Get_Iter (This),
            No);
      else
         Model.all.Columns.all.Insert (Row, Null_Iter, No);
      end if;
      if This /= Null_Gtk_Tree_Path then
         Path_Free (This);
      end if;
      Inserted (Model, Path, Row);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Row_Inserted"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Row_Inserted;

   procedure On_Rows_Reordered
     (Reference  : access Gtk_Root_Tree_Model_Record'Class;
      Params     : GValues;
      Model      : Gtk_Extension_Store)
   is
      pragma Unreferenced (Reference);

      Path    : constant Gtk_Tree_Path :=
                   Convert (Get_Address (Nth (Params, 1)));
      Indices : Gint_Array renames
                  Address_To_GInt_Array.To_Pointer
                    (Get_Address (Nth (Params, 3))).all;
   begin
      Model.all.Columns.all.Reorder
        (Model.all.Columns.all.Get_Iter (Path),
         Indices);
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("On_Rows_Reordered"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Rows_Reordered;

   overriding function Parent
     (Model : not null access Gtk_Extension_Store_Record;
      Child : Gtk_Tree_Iter) return Gtk_Tree_Iter is
   begin
      return Model.all.Columns.all.Parent (Child);
   end Parent;

   overriding procedure Previous
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : in out Gtk_Tree_Iter) is
   begin
      Model.all.Columns.all.Previous (Iter);
   end Previous;

   overriding procedure Ref_Node
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : Gtk_Tree_Iter) is
   begin
      Ref_Node (Model.all.Reference, From_Extension (Model, Iter));
      Ref_Node (Model.all.Reference, Iter);
   end Ref_Node;

   procedure Set_Extension
     (Model  : not null access Gtk_Extension_Store_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Positive;
      Value  : Boolean) is
   begin
      Model.all.Columns.all.Set (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

   procedure Set_Extension
     (Model  : not null access Gtk_Extension_Store_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Positive;
      Value  : Gint) is
   begin
      Model.all.Columns.all.Set (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

   procedure Set_Extension
     (Model  : not null access Gtk_Extension_Store_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Positive;
      Value  : UTF8_String) is
   begin
      Model.all.Columns.all.Set (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

   procedure Set_Extension
     (Model  : not null access Gtk_Extension_Store_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Column : Positive;
      Value  : GValue) is
   begin
      Model.all.Columns.all.Set_Value (Iter, Gint (Column) - 1, Value);
   end Set_Extension;

   procedure Set_Null_Reference
     (Model : not null access Gtk_Extension_Store_Record) is
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
      end if;
   end Set_Null_Reference;

   procedure Set_Reference
     (Model     : not null access Gtk_Extension_Store_Record;
      Reference : not null access Gtk_Root_Tree_Model_Record'Class) is
   begin
      Ref (Reference);
      Set_Null_Reference (Model);
      Model.all.Reference := To_Interface (Reference);
      Model.all.Offset := Get_N_Columns (Model.all.Reference);
      Initial_Sync (Model, Null_Iter, Null_Iter);
      Model.all.Callbacks (Changed) :=
        Tree_Handlers.Connect
          (Reference,
           "row-changed",
           On_Changed_Row'Access,
           Model.all'Access,
           True);
      Model.all.Callbacks (Deleted) :=
        Tree_Handlers.Connect
          (Reference,
           "row-deleted",
           On_Deleted_Row'Access,
           Model.all'Access,
           True);
      Model.all.Callbacks (Inserted) :=
         Tree_Handlers.Connect
          (Reference,
           "row-inserted",
           On_Row_Inserted'Access,
           Model.all'Access,
           True);
   exception
      when others =>
         Model.all.Reference := Null_Gtk_Tree_Model;
         Unref (Reference);
         raise;
   end Set_Reference;

   function To_Extension
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : Gtk_Tree_Iter) return Gtk_Tree_Iter is
   begin
      if Iter = Null_Iter then
         return Null_Iter;
      else
         declare
            Path   : constant Gtk_Tree_Path :=
                     Get_Path (Model.all.Reference, Iter);
            Result : Gtk_Tree_Iter := Null_Iter;
         begin
            if Path /= Null_Gtk_Tree_Path then
               if Get_Depth (Path) = 0 then
                  Result := Null_Iter;
               else
                  Result := Model.all.Columns.all.Get_Iter (Path);
               end if;
               Path_Free (Path);
            end if;
            return Result;
         end;
      end if;
   end To_Extension;

   function To_Extension
     (Model : not null access Gtk_Extension_Store_Record;
      Path  : Gtk_Tree_Path) return Gtk_Tree_Path
   is
      pragma Unreferenced (Model);
   begin
      if Path = Null_Gtk_Tree_Path then
         return Null_Gtk_Tree_Path;
      else
         return Copy (Path);
      end if;
   end To_Extension;

   overriding procedure Unref_Node
     (Model : not null access Gtk_Extension_Store_Record;
      Iter  : Gtk_Tree_Iter) is
   begin
      Unref_Node (Model.all.Reference, From_Extension (Model, Iter));
      Model.all.Columns.all.Unref_Node (Iter);
   end Unref_Node;

   function Where (Text : String) return String is
   begin
      return " in Gtk.Tree_Model.Extension_Store." & Text;
   end Where;

   pragma Warnings (On, "declaration hides ""GTK_Type""");
   pragma Warnings (On, "declaration hides ""Params""");
   pragma Warnings (On, "declaration hides ""Parent""");
   pragma Warnings (On, "declaration hides ""Types""");

end Gtk.Tree_Model.Extension_Store;
