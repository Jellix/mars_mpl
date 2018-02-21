--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.Abstract_Store               Luebeck            --
--  Implementation                                 Summer, 2006       --
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

with Glib.Messages;

with GNAT.Traceback.Symbolic;

with Gtk.Missed;

with Interfaces.C.Strings;

with System.Address_To_Access_Conversions;
with System.Storage_Elements;

package body Gtk.Tree_Model.Abstract_Store is

   pragma Warnings (Off, "declaration hides ""Class""");
   pragma Warnings (Off, "declaration hides ""GInterface_Info""");
   pragma Warnings (Off, "declaration hides ""GType_Interface""");
   pragma Warnings (Off, "declaration hides ""Name""");
   pragma Warnings (Off, "declaration hides ""Object""");
   pragma Warnings (Off, "declaration hides ""Params""");
   pragma Warnings (Off, "declaration hides ""Parent""");
   pragma Warnings (Off, "declaration hides ""Query""");
   pragma Warnings (Off, "declaration hides ""Return_Type""");
   pragma Warnings (Off, "declaration hides ""Type_Name""");
   pragma Warnings (Off, "declaration hides ""Type_Of""");

   package Convert is
     new System.Address_To_Access_Conversions
       (Object => Gtk_Abstract_Model_Record'Class);

   function Where (Text : String) return String is
   begin
      return " in Gtk.Tree_Model.Abstract_Store." & Text;
   end Where;

   type C_Dispose is access procedure (Object : System.Address);
   pragma Convention (C, C_Dispose);

   type C_Finalize is access procedure (Object : System.Address);
   pragma Convention (C, C_Finalize);

   type Dummy is array (1 .. 6) of System.Address;
   pragma Convention (C, Dummy);

   type C_GObjectClass is
      record
         G_Type                      : GType;    -- GTypeClass
         Construct_Properties        : System.Address;  -- GObjectClass
         Constructor                 : System.Address;
         Set_Property                : System.Address;
         Get_Property                : System.Address;
         Dispose                     : C_Dispose;
         Finalize                    : C_Finalize;
         Dispatch_Properties_Changed : System.Address;
         Notify                      : System.Address;
         Flags                       : Gsize;
         P_Dummy                     : Dummy;
      end record;
   pragma Convention (C, C_GObjectClass);

   type C_GObjectClass_Ptr is access all C_GObjectClass;
   pragma Convention (C, C_GObjectClass_Ptr);

   Parent_Class : C_GObjectClass_Ptr := null;

   type C_Class_Init is access procedure (Class : C_GObjectClass_Ptr);
   pragma Convention (C, C_Class_Init);

   type GtkTreeModelIface;
   type GtkTreeModelIface_Ptr is access all GtkTreeModelIface;
   pragma Convention (C, GtkTreeModelIface_Ptr);

   type C_Interface_Init is access procedure
     (Iface      : GtkTreeModelIface_Ptr;
      Iface_Data : System.Address);
   pragma Convention (C, C_Interface_Init);

   type GType_Info is record
      Class_Size     : Guint16;
      Base_Init      : System.Address;
      Base_Finalize  : System.Address;
      Class_Init     : C_Class_Init;
      Class_Finalize : System.Address;
      Class_Data     : System.Address;
      Instance_Size  : Guint16;
      Preallocs      : Guint16;
      Instance_Init  : System.Address;
      Value_Table    : System.Address;
   end record;
   pragma Convention (C, GType_Info);

   type GTypeQuery is record
      Type_Of       : GType;
      Type_Name     : Interfaces.C.Strings.chars_ptr;
      Class_Size    : Guint;
      Instance_Size : Guint;
   end record;
   pragma Convention (C, GTypeQuery);

   procedure Type_Query (Type_Of : GType; Query : out GTypeQuery);
   pragma Import (C, Type_Query, "g_type_query");

   type Gtk_Tree_Iter_Ptr is access all Gtk_Tree_Iter;
   pragma Convention (C, Gtk_Tree_Iter_Ptr);

   type GInterface_Info is record
      Init : C_Interface_Init;
      Fin  : C_Interface_Init;
      Data : System.Address;
   end record;
   pragma Convention (C, GInterface_Info);

   type GType_Interface is record
      G_Type          : GType;
      G_Instance_Type : GType;
   end record;
   pragma Convention (C, GType_Interface);

   type Proc_Table is array (Positive range <>) of System.Address;
   pragma Convention (C, Proc_Table);
   pragma Unreferenced (Proc_Table);

   type C_Get_Flags is access function (Model : System.Address)
      return Tree_Model_Flags;
   pragma Convention (C, C_Get_Flags);

   type C_Get_N_Columns is access function (Model : System.Address)
      return Gint;
   pragma Convention (C, C_Get_N_Columns);

   type C_Get_Column_Type is access function
     (Model : System.Address;
      Index : Gint) return GType;
   pragma Convention (C, C_Get_Column_Type);

   type C_Get_Iter is access function
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter;
      Path  : System.Address) return Gboolean;
   pragma Convention (C, C_Get_Iter);

   type C_Get_Path is access function
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return System.Address;
   pragma Convention (C, C_Get_Path);

   type C_Get_Value is access procedure
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Column : Gint;
      Value  : access GValue);
   pragma Convention (C, C_Get_Value);

   type C_Children is access function
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter_Ptr) return Gboolean;
   pragma Convention (C, C_Children);

   type C_Has_Child is access function
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, C_Has_Child);

   type C_Next is access function
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, C_Next);

   type C_N_Children is access function
     (Model : System.Address;
      Iter  : Gtk_Tree_Iter_Ptr) return Gint;
   pragma Convention (C, C_N_Children);

   type C_Nth_Child is access function
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter_Ptr;
      N      : Gint) return Gboolean;
   pragma Convention (C, C_Nth_Child);

   type C_Parent is access function
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter;
      Child : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, C_Parent);

   type C_Previous is access function
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, C_Previous);

   type C_Ref_Node is access procedure
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter);
   pragma Convention (C, C_Ref_Node);

   type C_Unref_Node is access procedure
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter);
   pragma Convention (C, C_Unref_Node);

   type GtkTreeModelIface is record
      G_Iface : GType_Interface;
      -- Signals
      Row_Changed           : System.Address;
      Row_Inserted          : System.Address;
      Row_Has_Child_Toggled : System.Address;
      Row_Deleted           : System.Address;
      Rows_Reordered        : System.Address;
      -- Virtual Table
      Get_Flags             : C_Get_Flags;
      Get_N_Columns         : C_Get_N_Columns;
      Get_Column_Type       : C_Get_Column_Type;
      Get_Iter              : C_Get_Iter;
      Get_Path              : C_Get_Path;
      Get_Value             : C_Get_Value;
      Iter_Next             : C_Next;
      Iter_Previous         : C_Previous;
      Iter_Children         : C_Children;
      Iter_Has_Child        : C_Has_Child;
      Iter_N_Children       : C_N_Children;
      Iter_Nth_Child        : C_Nth_Child;
      Iter_Parent           : C_Parent;
      Ref_Node              : C_Ref_Node;
      Unref_Node            : C_Unref_Node;
   end record;
   pragma Convention (C, GtkTreeModelIface);

   procedure On_Delete (Model : System.Address);
   pragma Convention (C, On_Delete);

   function On_Get_Flags (Model : System.Address) return Tree_Model_Flags;
   pragma Convention (C, On_Get_Flags);

   function On_Get_N_Columns (Model : System.Address) return Gint;
   pragma Convention (C, On_Get_N_Columns);

   function On_Get_Column_Type
     (Model : System.Address;
      Index : Gint) return GType;
   pragma Convention (C, On_Get_Column_Type);

   function On_Get_Iter
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter;
      Path  : System.Address) return Gboolean;
   pragma Convention (C, On_Get_Iter);

   function On_Get_Path
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return System.Address;
   pragma Convention (C, On_Get_Path);

   procedure On_Get_Value
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Column : Gint;
      Value  : access GValue);
   pragma Convention (C, On_Get_Value);

   function On_Children
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter_Ptr) return Gboolean;
   pragma Convention (C, On_Children);

   function On_Has_Child
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, On_Has_Child);

   procedure On_Initialize_Interface
     (Iface      : GtkTreeModelIface_Ptr;
      Iface_Data : System.Address);
   pragma Convention (C, On_Initialize_Interface);

   function On_Next
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, On_Next);

   function On_N_Children
     (Model : System.Address;
      Iter  : Gtk_Tree_Iter_Ptr) return Gint;
   pragma Convention (C, On_N_Children);

   function On_Nth_Child
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter_Ptr;
      N      : Gint) return Gboolean;
   pragma Convention (C, On_Nth_Child);

   function On_Parent
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter;
      Child : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, On_Parent);

   function On_Previous
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean;
   pragma Convention (C, On_Previous);

   procedure On_Ref_Node
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter);
   pragma Convention (C, On_Ref_Node);

   procedure On_Unref_Node
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter);
   pragma Convention (C, On_Unref_Node);

   GtkAda_String : constant String := "_GtkAda" & ASCII.NUL;
   GtkAda_String_Quark : Glib.GQuark := Glib.Unknown_Quark;

   function To_Ada
     (Object : System.Address) return Gtk_Abstract_Model_Record_Ptr
   is
      function Internal (Object : System.Address; Quark : GQuark)
                         return System.Address;
      pragma Import (C, Internal, "g_object_get_qdata");

      use type System.Address;
   begin
      if Object = System.Null_Address then
         return null;
      end if;
      if GtkAda_String_Quark = Unknown_Quark then
         GtkAda_String_Quark := Quark_From_String (GtkAda_String);
      end if;
      return
        Gtk_Abstract_Model_Record_Ptr
          (Convert.To_Pointer (Internal (Object, GtkAda_String_Quark)));
   end To_Ada;

   function On_Get_Flags (Model : System.Address) return Tree_Model_Flags
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Get_Flags"));
         return 0;
      else
         return This.all.Get_Flags;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Get_Flags"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Get_Flags;

   function On_Get_N_Columns (Model : System.Address) return Gint
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Get_N_Columns"));
         return 0;
      else
         return This.all.Get_N_Columns;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Get_N_Columns"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Get_N_Columns;

   function On_Get_Column_Type (Model : System.Address; Index : Gint)
                                return GType
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Get_Column_Type"));
         return 0;
      else
         return This.all.Get_Column_Type (Index);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Get_Column_Type"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Get_Column_Type;

   function On_Get_Iter
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter;
      Path  : System.Address) return Gboolean
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Get_Iter"));
         return 0;
      else
         declare
            Route  : Gtk_Tree_Path;
            Result : Gtk_Tree_Iter;
         begin
            Route.Set_Object (Path);
            Result := This.all.Get_Iter (Route);
            if Result = Null_Iter then
               return 0;
            else
               Iter.all := Result;
               return 1;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Get_Iter"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Get_Iter;

   function On_Get_Path
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return System.Address
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Get_Path"));
         return System.Null_Address;
      else
         return Get_Object (This.all.Get_Path (Iter.all));
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Get_Path"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return System.Null_Address;
   end On_Get_Path;

   procedure On_Get_Value
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Column : Gint;
      Value  : access GValue)
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Get_Value"));
      else
         This.all.Get_Value (Iter.all, Column, Value.all);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Get_Value"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Get_Value;

   function On_Children
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter_Ptr) return Gboolean
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Children"));
         return 0;
      else
         if Parent = null then
            Iter.all := This.all.Children (Null_Iter);
         else
            Iter.all := This.all.Children (Parent.all);
         end if;
         if Iter.all = Null_Iter then
            return 0;
         else
            return 1;
         end if;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error)
            & Where ("On_Children"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Children;

   procedure On_Delete (Model : System.Address) is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This /= null then
         This.all.Finalize;
      end if;
      if Parent_Class.all.Finalize /= null then
         Parent_Class.all.Finalize (Model);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Delete"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Delete;

   function On_Has_Child
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Has_Child"));
         return 0;
      elsif Iter /= null and then This.all.Has_Child (Iter.all) then
         return 1;
      else
         return 0;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Has_Child"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Has_Child;

   procedure On_Initialize_Interface
     (Iface      : GtkTreeModelIface_Ptr;
      Iface_Data : System.Address)
   is
      pragma Unreferenced (Iface_Data);
   begin
      Iface.all.Get_Flags       := On_Get_Flags'Access;
      Iface.all.Get_N_Columns   := On_Get_N_Columns'Access;
      Iface.all.Get_Column_Type := On_Get_Column_Type'Access;
      Iface.all.Get_Iter        := On_Get_Iter'Access;
      Iface.all.Get_Path        := On_Get_Path'Access;
      Iface.all.Get_Value       := On_Get_Value'Access;
      Iface.all.Iter_Next       := On_Next'Access;
      Iface.all.Iter_Previous   := On_Previous'Access;
      Iface.all.Iter_Children   := On_Children'Access;
      Iface.all.Iter_Has_Child  := On_Has_Child'Access;
      Iface.all.Iter_N_Children := On_N_Children'Access;
      Iface.all.Iter_Nth_Child  := On_Nth_Child'Access;
      Iface.all.Iter_Parent     := On_Parent'Access;
      Iface.all.Ref_Node        := On_Ref_Node'Access;
      Iface.all.Unref_Node      := On_Unref_Node'Access;
   end On_Initialize_Interface;

   function On_Next
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Next"));
         return 0;
      else
         declare
            Result : Gtk_Tree_Iter := Iter.all;
         begin
            This.all.Next (Result);
            if Result = Null_Iter then
               return 0;
            else
               Iter.all := Result;
               return 1;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Next"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Next;

   function On_N_Children
     (Model : System.Address;
      Iter  : Gtk_Tree_Iter_Ptr) return Gint
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_N_Children"));
         return 0;
      elsif Iter = null then
         return This.all.N_Children (Null_Iter);
      else
         return This.all.N_Children (Iter.all);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_N_Children"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_N_Children;

   function On_Nth_Child
     (Model  : System.Address;
      Iter   : access Gtk_Tree_Iter;
      Parent : Gtk_Tree_Iter_Ptr;
      N      : Gint) return Gboolean
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Nth_Child"));
         return 0;
      else
         declare
            Result : Gtk_Tree_Iter;
         begin
            if Parent = null then
               Result := This.all.Nth_Child (Null_Iter, N);
            else
               Result := This.all.Nth_Child (Parent.all, N);
            end if;
            if Result = Null_Iter then
               return 0;
            else
               Iter.all := Result;
               return 1;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Nth_Child"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Nth_Child;

   function On_Parent
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter;
      Child : access Gtk_Tree_Iter) return Gboolean
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Parent"));
         return 0;
      else
         declare
            Result : constant Gtk_Tree_Iter := This.all.Parent (Child.all);
         begin
            if Result = Null_Iter then
               return 0;
            else
               Iter.all := Result;
               return 1;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Parent"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Parent;

   function On_Previous
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter) return Gboolean
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Previous"));
         return 0;
      else
         declare
            Result : Gtk_Tree_Iter := Iter.all;
         begin
            This.all.Previous (Result);
            if Result = Null_Iter then
               return 0;
            else
               Iter.all := Result;
               return 1;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Previous"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
         return 0;
   end On_Previous;

   procedure On_Ref_Node
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter)
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Ref_Node"));
      else
         This.all.Ref_Node (Iter.all);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Ref_Node"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Ref_Node;

   procedure On_Unref_Node
     (Model : System.Address;
      Iter  : access Gtk_Tree_Iter)
   is
      This : constant Gtk_Abstract_Model_Record_Ptr := To_Ada (Model);
   begin
      if This = null then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Null model" & Where ("On_Unref_Node"));
      else
         This.all.Unref_Node (Iter.all);
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            Ada.Exceptions.Exception_Message (Error) & Where ("On_Unref_Node"));
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            GNAT.Traceback.Symbolic.Symbolic_Traceback (Error));
   end On_Unref_Node;

   function Image (Iter : Gtk_Tree_Iter) return String is
   begin
      return
        Gint'Image (Iter.Stamp)
        & System.Storage_Elements.Integer_Address'Image
            (System.Storage_Elements.To_Integer (Iter.User_Data))
        & System.Storage_Elements.Integer_Address'Image
            (System.Storage_Elements.To_Integer (Iter.User_Data2))
        & System.Storage_Elements.Integer_Address'Image
            (System.Storage_Elements.To_Integer (Iter.User_Data3));
   end Image;

   procedure Initialize
     (Model   : not null access Gtk_Abstract_Model_Record'Class;
      Type_Of : GType)
   is
      function Object_New (Typ : GType) return System.Address;
      pragma Import (C, Object_New, "ada_g_object_new");
   begin
      Set_Object (Model, Object_New (Type_Of));
   end Initialize;

   procedure Class_Init (Class : C_GObjectClass_Ptr);
   pragma Convention (C, Class_Init);

   procedure Class_Init (Class : C_GObjectClass_Ptr) is
      function Class_Peek_Parent (Class : C_GObjectClass_Ptr)
         return C_GObjectClass_Ptr;
      pragma Import (C, Class_Peek_Parent, "g_type_class_peek_parent");
   begin
      if Parent_Class = null then
         Parent_Class := Class_Peek_Parent (Class);
      end if;
      Class.all.Finalize := On_Delete'Access;
   end Class_Init;

   function Register
     (Name       : String;
      Signals    : Gtkada.Types.Chars_Ptr_Array := Gtkada.Types.Null_Array;
      Parameters : Signal_Parameter_Types       := Null_Parameter_Types)
      return GType is
   begin
      return
        Register_Type
          (Name       => Name,
           Signals    => Signals,
           Parameters => Parameters);
   end Register;

   function Register_Type
     (Name       : String;
      Size       : Positive                     := GObject'Size;
      Signals    : Gtkada.Types.Chars_Ptr_Array := Gtkada.Types.Null_Array;
      Parameters : Signal_Parameter_Types       := Null_Parameter_Types)
      return GType
   is
      function Register_Static
        (Parent_Type : GType;
         Type_Name   : Interfaces.C.char_array;
         Type_Info   : access GType_Info;
         Type_Flags  : Guint) return Glib.GType;
      pragma Import (C, Register_Static, "g_type_register_static");
      procedure Add_Interface_Static
        (Instance_Type  : GType;
         Interface_Type : GType;
         Info           : access GInterface_Info);
      pragma Import
             (C, Add_Interface_Static, "g_type_add_interface_static");
      procedure Marshaller
        (Closure         : System.Address;
         Return_Value    : access GValue;
         N_Param_Values  : Guint;
         Param_Values    : access GValue;
         Invocation_Hint : System.Address;
         Marshal_Data    : System.Address);
      pragma Import (C, Marshaller, "g_cclosure_marshal_VOID__VOID");
      function Signal_NewV
        (Name         : Gtkada.Types.Chars_Ptr;
         IType        : GType;
         Signal_Flags : Guint;
         Closure      : System.Address;
         Accumulator  : System.Address;
         Accu_Data    : System.Address;
         C_Marshaller : System.Address;
         Return_Type  : GType;
         N_Params     : Guint;
         Params       : access GType) return Guint;
      pragma Import (C, Signal_NewV, "g_signal_newv");
      type Parameter_List is array (Natural range <>) of aliased GType;
      pragma Convention (C, Parameter_List);

      Result : GType;
      Info   : GTypeQuery;
   begin
      if Parameters /= Null_Parameter_Types then
         if Signals'Length /= Parameters'Length (1) then
            raise Constraint_Error with
                 "Signal and their parameter lists " &
                 "have different sizes" &
                 Integer'Image (Signals'Length) &
                 " and" &
                 Integer'Image (Parameters'Length (1));
         end if;
      end if;
      Type_Query (GType_Object, Info);
      if Guint (Size / Interfaces.C.char'Size) < Info.Instance_Size then
         raise Constraint_Error with
               "GTK object instance of '" &
               Name &
               "' is smaller than parent '" &
               Interfaces.C.Strings.Value (Info.Type_Name) &
               "' " &
               Integer'Image (Size / Interfaces.C.char'Size) &
               "<" &
               Guint'Image (Info.Instance_Size);
      end if;
      Result :=
        Register_Static
         (Parent_Type => GType_Object,
          Type_Name   => Interfaces.C.To_C (Name),
          Type_Flags  => 0,
          Type_Info   =>
             new GType_Info'(Class_Size     =>
                                   Guint16'Max
                               (C_GObjectClass'Size / Interfaces.C.char'Size,
                                Guint16 (Info.Class_Size)),
                             Base_Init      => System.Null_Address,
                             Base_Finalize  => System.Null_Address,
                             Class_Init     => Class_Init'Access,
                             Class_Finalize => System.Null_Address,
                             Class_Data     => System.Null_Address,
                             Preallocs      => 0,
                             Instance_Init  => System.Null_Address,
                             Value_Table    => System.Null_Address,
                             Instance_Size  =>
                               Guint16 (Size / Interfaces.C.char'Size)));
      Add_Interface_Static
        (Instance_Type  => Result,
         Interface_Type => Gtk.Tree_Model.Get_Type,
         Info           => new GInterface_Info'(On_Initialize_Interface'Access,
                                                null,
                                                System.Null_Address));
      if Parameters = Null_Parameter_Types then
         for Index in Signals'Range loop
            declare
               Dummy_ID : Guint;
            begin
               Dummy_ID :=
                 Signal_NewV
                   (Name         => Signals (Index),
                    IType        => Result,
                    Signal_Flags => 2, -- G_SIGNAL_RUN_LAST
                    Closure      => System.Null_Address,
                    Accumulator  => System.Null_Address,
                    Accu_Data    => System.Null_Address,
                    C_Marshaller => Marshaller'Address,
                    Return_Type  => GType_None,
                    N_Params     => 0,
                    Params       => null);
               pragma Unreferenced (Dummy_ID);
            end;
         end loop;
      else
         for Index in Signals'Range loop
            declare
               use type Interfaces.C.size_t;

               Count : Natural := 0;
               I     : constant Natural :=
                         Natural (Index - Signals'First) +
                         Parameters'First (1);
            begin
               for J in Parameters'Range (2) loop
                  exit when Parameters (I, J) = GType_None;
                  Count := Count + 1;
               end loop;
               declare
                  List : Parameter_List (1 .. Count);
               begin
                  for J in List'Range loop
                     List (J) :=
                       Parameters
                         (I, J - List'First + Parameters'First (2));
                  end loop;
                  declare
                     Dummy_ID : Guint;
                  begin
                     Dummy_ID :=
                       Signal_NewV
                         (Name         => Signals (Index),
                          IType        => Result,
                          Signal_Flags => 2, -- G_SIGNAL_RUN_LAST
                          Closure      => System.Null_Address,
                          Accumulator  => System.Null_Address,
                          Accu_Data    => System.Null_Address,
                          C_Marshaller => Marshaller'Address,
                          Return_Type  => GType_None,
                          N_Params     => Guint (Count),
                          Params       => List (1)'Access);
                     pragma Unreferenced (Dummy_ID);
                  end;
               end;
            end;
         end loop;
      end if;
      return Result;
   end Register_Type;

   pragma Warnings (On, "declaration hides ""Class""");
   pragma Warnings (On, "declaration hides ""GInterface_Info""");
   pragma Warnings (On, "declaration hides ""GType_Interface""");
   pragma Warnings (On, "declaration hides ""Name""");
   pragma Warnings (On, "declaration hides ""Object""");
   pragma Warnings (On, "declaration hides ""Params""");
   pragma Warnings (On, "declaration hides ""Parent""");
   pragma Warnings (On, "declaration hides ""Query""");
   pragma Warnings (On, "declaration hides ""Return_Type""");
   pragma Warnings (On, "declaration hides ""Type_Name""");
   pragma Warnings (On, "declaration hides ""Type_Of""");

end Gtk.Tree_Model.Abstract_Store;
