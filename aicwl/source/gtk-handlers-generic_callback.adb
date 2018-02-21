--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Handlers.Generic_Callback               Luebeck            --
--  Implementation                                 Autumn, 2011       --
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

with Ada.Unchecked_Deallocation;

package body Gtk.Handlers.Generic_Callback is

   pragma Warnings (Off, "declaration hides ""Callback""");
   pragma Warnings (Off, "declaration hides ""ID""");
   pragma Warnings (Off, "declaration hides ""Params""");
   pragma Warnings (Off, "declaration hides ""User_Data""");
   pragma Warnings (Off, "declaration hides ""Values""");

   --
   -- Closure_Data -- The data passed with the closure
   --
   type Closure_Data is record
      Callback    : Handler;
      Result_Type : GType;
      Data        : User_Type;
   end record;
   type Closure_Data_Ptr is access all Closure_Data;
   pragma Convention (C, Closure_Data_Ptr);

   type GCallback is access procedure (User_Data : User_Type);
   pragma Convention (C, GCallback);

   type GValue_Ptr is access all Glib.Values.GValue;
   pragma Convention (C, GValue_Ptr);

   type GClosureMarshal is access procedure
     (Closure         : GClosure;
      Return_Value    : GValue_Ptr;
      N_Params        : Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : Closure_Data_Ptr);
   pragma Convention (C, GClosureMarshal);

   type GClosureNotify is access procedure
     (Data    : Closure_Data_Ptr;
      Closure : GClosure);
   pragma Convention (C, GClosureNotify);

   function G_CClosure_New
     (Callback     : GCallback;
      User_Data    : Closure_Data_Ptr;
      Destroy_Data : GClosureNotify) return GClosure;
   pragma Import (C, G_CClosure_New, "g_cclosure_new");

   procedure Set_Meta_Marshal
     (Closure    : GClosure;
      User_Data  : Closure_Data_Ptr;
      Marshaller : GClosureMarshal);
   pragma Import (C, Set_Meta_Marshal, "g_closure_set_meta_marshal");

   function Parse_Name
     (Detailed_Signal    : Glib.Signal_Name;
      Itype              : GType;
      Signal_Id_Ptr      : access Signal_Id;
      Detail_Ptr         : access GQuark;
      Force_Detail_Quark : Gboolean) return Gboolean;
   pragma Import (C, Parse_Name, "g_signal_parse_name");

   function Connect_Closure_By_ID
     (Instance : System.Address;
      ID       : Signal_Id;
      Detail   : GQuark;
      Closure  : GClosure;
      After    : Gint := 0) return Gulong;
   pragma Import
     (C, Connect_Closure_By_ID, "g_signal_connect_closure_by_id");

   procedure Closure_Callback (User_Data : User_Type);
   pragma Convention (C, Closure_Callback);

   procedure Marshaller
     (Closure         : GClosure;
      Return_Value    : GValue_Ptr;
      N_Params        : Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : Closure_Data_Ptr);
   pragma Convention (C, Marshaller);

   procedure Notify
     (Data    : Closure_Data_Ptr;
      Closure : GClosure);
   pragma Convention (C, Notify);

   procedure Closure_Callback (User_Data : User_Type) is
   begin
      null;
   end Closure_Callback;

   procedure Connect
     (Object   : not null access Object_Type'Class;
      Name     : Glib.Signal_Name;
      Callback : Handler;
      Data     : User_Type;
      After    : Boolean := False)
   is
      ID : Handler_Id;
   begin
      ID :=
        Connect
          (Object   => Object,
           Name     => Name,
           Callback => Callback,
           Data     => Data,
           After    => After);
      pragma Unreferenced (ID);
   end Connect;

   function Connect
     (Object   : not null access Object_Type'Class;
      Name     : Glib.Signal_Name;
      Callback : Handler;
      Data     : User_Type;
      After    : Boolean := False) return Handler_Id
   is
      ID        : Handler_Id;
      Signal    : aliased Signal_Id;
      Detail    : aliased GQuark;
      User_Data : Closure_Data_Ptr;
      Info      : Signal_Query;
   begin
      if (0 =
            Parse_Name
              (Detailed_Signal    => Name & Character'Val (0),
               Itype              => Get_Type (Object),
               Signal_Id_Ptr      => Signal'Access,
               Detail_Ptr         => Detail'Access,
               Force_Detail_Quark => 0)) or else
          Signal = Invalid_Signal_Id
      then
         raise Constraint_Error with
           "Signal '" &
           String (Name) &
           "' is not defined on the type " &
           Type_Name (Get_Type (Object));
      end if;
      Query (Signal, Info);
      User_Data :=
        new Closure_Data'(Callback, Return_Type (Info), Data);
      ID.Closure :=
        G_CClosure_New
          (Closure_Callback'Access, -- Never called, actually
           User_Data,
           Notify'Access);
      Set_Meta_Marshal (ID.Closure, User_Data, Marshaller'Access);
      ID.Id :=
        Connect_Closure_By_ID
          (Instance => Get_Object (Object),
           ID       => Signal,
           Detail   => Detail,
           Closure  => ID.Closure,
           After    => Boolean'Pos (After));
      return ID;
   end Connect;

   procedure Marshaller
     (Closure         : GClosure;
      Return_Value    : GValue_Ptr;
      N_Params        : Guint;
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      User_Data       : Closure_Data_Ptr)
   is
      pragma Unreferenced (Closure);
      pragma Unreferenced (Invocation_Hint);

      Stub      : Object_Type;
      Values    : constant Glib.Values.GValues :=
                    Glib.Values.Make_Values (N_Params, Params);
      Arguments : Glib.Values.GValue_Array (1 .. Gint (N_Params) - 1);
   begin
      for Index in Arguments'Range loop
         Arguments (Index) := Glib.Values.Nth (Values, Guint (Index));
      end loop;
      if Return_Value = null then
         declare
            Result : Glib.Values.GValue;
         begin
            Glib.Values.Init (Result, User_Data.all.Result_Type);
            User_Data.all.Callback
              (Object_Type'Class
                 (Get_User_Data
                      (Glib.Values.Get_Address (Glib.Values.Nth (Values, 0)),
                       Stub).all)'Unchecked_Access,
               Arguments,
               Result,
               User_Data.all.Data);
            Glib.Values.Unset (Result);
         end;
      else
         Glib.Values.Init (Return_Value.all, User_Data.all.Result_Type);
         User_Data.all.Callback
           (Object_Type'Class
              (Get_User_Data
                   (Glib.Values.Get_Address (Glib.Values.Nth (Values, 0)),
                    Stub).all)'Unchecked_Access,
            Arguments,
            Return_Value.all,
            User_Data.all.Data);
      end if;
   end Marshaller;

   procedure Notify
     (Data    : Closure_Data_Ptr;
      Closure : GClosure)
   is
      pragma Unreferenced (Closure);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (Closure_Data,
           Closure_Data_Ptr);
      Ptr : Closure_Data_Ptr := Data;
   begin
      Free (Ptr);
      pragma Unreferenced (Ptr);
   end Notify;

   pragma Warnings (On, "declaration hides ""Values""");
   pragma Warnings (On, "declaration hides ""User_Data""");
   pragma Warnings (On, "declaration hides ""Params""");
   pragma Warnings (On, "declaration hides ""ID""");
   pragma Warnings (On, "declaration hides ""Callback""");

end Gtk.Handlers.Generic_Callback;
