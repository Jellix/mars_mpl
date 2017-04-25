--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_Mark_Attributes                  Luebeck            --
--  Implementation                                 Summer, 2013       --
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

with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Interfaces.C;          use Interfaces.C;
with System;                use System;

with Ada.Unchecked_Deallocation;

package body Gtk.Source_Mark_Attributes is

   procedure g_free (String : Chars_Ptr);
   pragma Import (C, g_free, "g_free");

   function Get_Background
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return Gdk_RGBA is
      function Internal (Attributes : Address) return Gdk_RGBA;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_get_background"
             );
   begin
      return Internal (Get_Object (Attributes));
   end Get_Background;

   function Get_GIcon
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return Glib.G_Icon.G_Icon is
      function Internal (Attributes : Address) return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_get_gicon"
             );
   begin
      return Glib.G_Icon.G_Icon (Internal (Get_Object (Attributes)));
   end Get_GIcon;

   function Get_Icon_Name
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return UTF8_String is
      function Internal (Attributes : Address) return Chars_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_get_icon_name"
             );
      Result : constant Chars_Ptr := Internal (Get_Object (Attributes));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Icon_Name;

   function Get_Pxibuf
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return Gdk_Pixbuf is
      function Internal (Attributes : Address) return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_get_pixbuf"
             );
      Stub : Gdk_Pixbuf_Record;
   begin
      return
         Gdk_Pixbuf
         (  Get_User_Data_Fast
            (  Internal (Get_Object (Attributes)),
               Stub
         )  );
   end Get_Pxibuf;

   function Get_Stock_ID
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record
            )  return UTF8_String is
      function Internal (Attributes : Address) return Chars_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_get_stock_id"
             );
      Result : constant Chars_Ptr := Internal (Get_Object (Attributes));
   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Result);
      end if;
   end Get_Stock_ID;

   function Get_Tooltip_Markup
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record;
               Mark       : not null access Gtk_Source_Mark_Record
            )  return UTF8_String is
      function Internal
               (  Attributes : Address;
                  Mark       : Address
               )  return Chars_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_get_tooltip_markup"
             );
      Ptr : constant Chars_Ptr :=
            Internal (Get_Object (Attributes), Get_Object (Mark));
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         return Result : constant String := Value (Ptr) do
            g_free (Ptr);
         end return;
      end if;
   end Get_Tooltip_Markup;

   function Get_Tooltip_Text
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record;
               Mark       : not null access Gtk_Source_Mark_Record
            )  return UTF8_String is
      function Internal
               (  Attributes : Address;
                  Mark       : Address
               )  return Chars_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_get_tooltip_text"
             );
      Ptr : constant Chars_Ptr :=
            Internal (Get_Object (Attributes), Get_Object (Mark));
   begin
      if Ptr = Null_Ptr then
         return "";
      else
         return Result : constant String := Value (Ptr) do
            g_free (Ptr);
         end return;
      end if;
   end Get_Tooltip_Text;

   procedure Gtk_New (Attributes : out Gtk_Source_Mark_Atributes) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Gtk_Source_Mark_Atributes_Record'Class,
                Gtk_Source_Mark_Atributes
             );
   begin
      Attributes := new Gtk_Source_Mark_Atributes_Record;
      Gtk.Source_Mark_Attributes.Initialize (Attributes);
   exception
      when others =>
         Free (Attributes);
         raise;
   end Gtk_New;

   procedure Initialize
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record'Class
             )  is
      function Internal return Address;
      pragma Import (C, Internal, "gtk_source_mark_attributes_new");
   begin
     Set_Object (Attributes, Internal);
   end Initialize;

   function Render_Icon
            (  Attributes : not null access
                            Gtk_Source_Mark_Atributes_Record;
               Widget     : not null access Gtk_Widget_Record'Class;
               Size       : GInt
            )  return Gdk_Pixbuf is
      function Internal
               (  Attributes : Address;
                  Widget     : Address;
                  Size       : GInt
               )  return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_render_icon"
             );
      Stub : Gdk_Pixbuf_Record;
   begin
      return
         Gdk_Pixbuf
         (  Get_User_Data_Fast
            (  Internal
               (  Get_Object (Attributes),
                  Get_Object (Widget),
                  Size
               ),
               Stub
         )  );
   end Render_Icon;

   procedure Set_Background
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Background : Gdk_RGBA
             )  is
      procedure Internal
                (  Attributes : Address;
                   Background : Gdk_RGBA
                );
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_set_background"
             );
   begin
      Internal (Get_Object (Attributes), Background);
   end Set_Background;

   procedure Set_GIcon
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Icon       : Glib.G_Icon.G_Icon
             )  is
      procedure Internal
                (  Attributes : Address;
                   Icon       : Glib.G_Icon.G_Icon
                );
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_set_gicon"
             );
   begin
      Internal (Get_Object (Attributes), Icon);
   end Set_GIcon;

   procedure Set_Icon_Name
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Icon_Name  : UTF8_String
             )  is
      procedure Internal
                (  Attributes : Address;
                   Icon_Name  : Char_Array
                );
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_set_icon_name"
             );
   begin
      Internal (Get_Object (Attributes), To_C (Icon_Name));
   end Set_Icon_Name;

   procedure Set_Pixbuf
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Pixbuf     : not null access Gdk_Pixbuf_Record'Class
             )  is
      procedure Internal
                (  Attributes : Address;
                   Pixbuf     : Address
                );
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_set_pixbuf"
             );
   begin
      Internal (Get_Object (Attributes), Get_Object (Pixbuf));
   end Set_Pixbuf;

   procedure Set_Stock_ID
             (  Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record;
                Stock_ID   : UTF8_String
             )  is
      procedure Internal
                (  Attributes : Address;
                   Stock_ID   : Char_Array
                );
      pragma Import
             (  C,
                Internal,
                "gtk_source_mark_attributes_set_stock_id"
             );
   begin
      Internal (Get_Object (Attributes), To_C (Stock_ID));
   end Set_Stock_ID;

end Gtk.Source_Mark_Attributes;
