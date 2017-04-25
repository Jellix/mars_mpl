--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Object.Checked_Destroy                 Luebeck            --
--  Implementation                                 Autumn, 2011       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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

with System;  use System;

procedure GLib.Object.Checked_Destroy
          (  Object : not null access Gtk_Widget_Record'Class
          )  is
   procedure Gtk_Widget_Destroy (Object : Address);
   pragma Import (C, Gtk_Widget_Destroy, "gtk_widget_destroy");

   procedure G_Object_Ref (Object : Address);
   pragma Import (C, G_Object_Ref, "g_object_ref");

   procedure G_Object_Ref_Sink (Object : Address);
   pragma Import (C, G_Object_Ref_Sink, "g_object_ref_sink");

   procedure G_Object_Unref (Object : Address);
   pragma Import (C, G_Object_Unref, "g_object_unref");

   function G_Object_Is_Floating (Object : Address) return GBoolean;
   pragma Import (C, G_Object_Is_Floating, "g_object_is_floating");

   This : constant Address := Get_Object (Object);
begin
   if This /= Null_Address then
      G_Object_Ref (This);
      if 0 /= G_Object_Is_Floating (This) then
         G_Object_Ref_Sink (This);
      end if;
      Gtk_Widget_Destroy (This);
      Set_Object (Object, Null_Address);
      G_Object_Unref (This);
   end if;
end GLib.Object.Checked_Destroy;
