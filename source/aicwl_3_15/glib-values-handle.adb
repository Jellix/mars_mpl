--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Handle                          Luebeck            --
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
--____________________________________________________________________--

with GLib.Messages;  use GLib.Messages;
with Gtk.Missed;     use Gtk.Missed;

package body GLib.Values.Handle is

   Gtk_Type : GType := GType_Invalid;

   function Where (Name : String) return String is
   begin
      return " in GLib.Values.Handle." & Name;
   end Where;

   function To_Object_Ptr is
      new Ada.Unchecked_Conversion (Address, Object_Type_Ptr);

   function Copy_Handle (Boxed : Address) return Address is
   begin
      if Boxed /= Null_Address then
         Increment_Count (To_Object_Ptr (Boxed).all);
      end if;
      return Boxed;
   end Copy_Handle;

   procedure Free_Handle (Boxed : Address) is
   begin
      if Boxed /= Null_Address then
         declare
            This : Entity_Ptr := To_Object_Ptr (Boxed).all'Unchecked_Access;
         begin
            Release (This);
         end;
      end if;
   end Free_Handle;

   function Get_Handle (Value : GValue) return Handle_Type is
      Ptr : constant Object_Type_Ptr := Get_Ptr (Value);
   begin
      return Ref (Ptr);
   end Get_Handle;

   function Get_Ptr (Value : GValue) return Object_Type_Ptr is
      Object_Address : Address;
   begin
      if Value.G_Type = Get_Type then
         Object_Address := Get_Boxed (Value);
         if Object_Address = Null_Address then
            return null;
         else
            return To_Object_Ptr (Object_Address);
         end if;
      else
         return null;
      end if;
   end Get_Ptr;

   function Get_Type return GType is
   begin
      if Gtk_Type = GType_Invalid then
         Gtk_Type := Boxed_Type_Register_Static (Type_Name, Copy, Free);
      end if;
      return Gtk_Type;
   end Get_Type;

   procedure Set_Handle
             (  Value     : in out GValue;
                Reference : Handle_Type
             )  is
   begin
      Set_Ptr (Value, Ptr (Reference));
   end Set_Handle;

   procedure Set_Ptr (Value : in out GValue; Ptr : Object_Type_Ptr) is
   begin
      if Value.G_Type /= Get_Type then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Warning,
            (  "Value is not initialized as "
            &  Type_Name
            &  Where ("Set_Ptr")
         )  );
         raise Constraint_Error;
      end if;
      --
      -- Set_Boxed calls internally Free_Handle on the target if that is
      -- not  null  and  then Copy_Handle if the source is not null. For
      -- this reason  we  don't  need  to  care  about  Use_Count  here.
      --
      if Ptr = null then
         Set_Boxed (Value, Null_Address);
      else
         declare
            Object_Address : constant Address := Ptr.all'Address;
         begin
            --
            -- Prevent  setting  the  same  address  again, because this
            -- might cause a premature object destruction.
            --
            if Get_Boxed (Value) /= Object_Address then
               Set_Boxed (Value, Object_Address);
            end if;
         end;
      end if;
   end Set_Ptr;

end GLib.Values.Handle;
