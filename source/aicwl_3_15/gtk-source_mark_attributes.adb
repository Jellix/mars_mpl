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
-- __________________________________________________________________ --

with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;

with System;

package body Gtk.Source_Mark_Attributes is

   pragma Warnings (Off, "declaration hides ""Attributes""");
   pragma Warnings (Off, "declaration hides ""Background""");
   pragma Warnings (Off, "declaration hides ""Icon""");
   pragma Warnings (Off, "declaration hides ""Icon_Name""");
   pragma Warnings (Off, "declaration hides ""Mark""");
   pragma Warnings (Off, "declaration hides ""Pixbuf""");
   pragma Warnings (Off, "declaration hides ""Size""");
   pragma Warnings (Off, "declaration hides ""Stock_ID""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   procedure g_free (S : Interfaces.C.Strings.chars_ptr);
   pragma Import (C, g_free, "g_free");

   function Get_Background
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record)
      return Gdk.RGBA.Gdk_RGBA
   is
      function Internal (Attributes : System.Address) return Gdk.RGBA.Gdk_RGBA;
      pragma Import (C, Internal, "gtk_source_mark_attributes_get_background");
   begin
      return Internal (Get_Object (Attributes));
   end Get_Background;

   function Get_GIcon
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record)
      return Glib.G_Icon.G_Icon
   is
      function Internal (Attributes : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_source_mark_attributes_get_gicon");
   begin
      return Glib.G_Icon.G_Icon (Internal (Get_Object (Attributes)));
   end Get_GIcon;

   function Get_Icon_Name
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record)
      return UTF8_String
   is
      function Internal
        (Attributes : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_source_mark_attributes_get_icon_name");
      Result : constant Interfaces.C.Strings.chars_ptr :=
                 Internal (Get_Object (Attributes));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Result = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end Get_Icon_Name;

   function Get_Pxibuf
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record)
      return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Attributes : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_source_mark_attributes_get_pixbuf");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return
        Gdk.Pixbuf.Gdk_Pixbuf
          (Get_User_Data_Fast
             (Internal (Get_Object (Attributes)),
              Stub));
   end Get_Pxibuf;

   function Get_Stock_ID
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record)
      return UTF8_String
   is
      function Internal
        (Attributes : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_source_mark_attributes_get_stock_id");
      Result : constant Interfaces.C.Strings.chars_ptr :=
                 Internal (Get_Object (Attributes));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Result = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (Result);
      end if;
   end Get_Stock_ID;

   function Get_Tooltip_Markup
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Mark       : not null access Gtk.Source_Mark.Gtk_Source_Mark_Record)
      return UTF8_String
   is
      function Internal
        (Attributes : System.Address;
         Mark       : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C,
                     Internal,
                     "gtk_source_mark_attributes_get_tooltip_markup");
      Ptr : constant Interfaces.C.Strings.chars_ptr :=
              Internal (Get_Object (Attributes), Get_Object (Mark));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Result : constant String := Interfaces.C.Strings.Value (Ptr) do
            g_free (Ptr);
         end return;
      end if;
   end Get_Tooltip_Markup;

   function Get_Tooltip_Text
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Mark       : not null access Gtk.Source_Mark.Gtk_Source_Mark_Record)
      return UTF8_String
   is
      function Internal
        (Attributes : System.Address;
         Mark       : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C,
                     Internal,
                     "gtk_source_mark_attributes_get_tooltip_text");
      Ptr : constant Interfaces.C.Strings.chars_ptr :=
              Internal (Get_Object (Attributes), Get_Object (Mark));

      use type Interfaces.C.Strings.chars_ptr;
   begin
      if Ptr = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Result : constant String := Interfaces.C.Strings.Value (Ptr) do
            g_free (Ptr);
         end return;
      end if;
   end Get_Tooltip_Text;

   procedure Gtk_New (Attributes : out Gtk_Source_Mark_Atributes)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Gtk_Source_Mark_Atributes_Record'Class,
           Gtk_Source_Mark_Atributes);
   begin
      Attributes := new Gtk_Source_Mark_Atributes_Record;
      Gtk.Source_Mark_Attributes.Initialize (Attributes);
   exception
      when others =>
         Free (Attributes);
         raise;
   end Gtk_New;

   procedure Initialize
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_source_mark_attributes_new");
   begin
      Set_Object (Attributes, Internal);
   end Initialize;

   function Render_Icon
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Widget     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Size       : Gint) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
        (Attributes : System.Address;
         Widget     : System.Address;
         Size       : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_source_mark_attributes_render_icon");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return
        Gdk.Pixbuf.Gdk_Pixbuf
          (Get_User_Data_Fast
             (Internal
                (Get_Object (Attributes),
                 Get_Object (Widget),
                 Size),
              Stub));
   end Render_Icon;

   procedure Set_Background
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Background : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
        (Attributes : System.Address;
         Background : Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_source_mark_attributes_set_background");
   begin
      Internal (Get_Object (Attributes), Background);
   end Set_Background;

   procedure Set_GIcon
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Icon       : Glib.G_Icon.G_Icon)
   is
      procedure Internal
        (Attributes : System.Address;
         Icon       : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_source_mark_attributes_set_gicon");
   begin
      Internal (Get_Object (Attributes), Icon);
   end Set_GIcon;

   procedure Set_Icon_Name
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Icon_Name  : UTF8_String)
   is
      procedure Internal
        (Attributes : System.Address;
         Icon_Name  : Interfaces.C.char_array);
      pragma Import (C, Internal, "gtk_source_mark_attributes_set_icon_name");
   begin
      Internal (Get_Object (Attributes), Interfaces.C.To_C (Icon_Name));
   end Set_Icon_Name;

   procedure Set_Pixbuf
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Pixbuf     : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
        (Attributes : System.Address;
         Pixbuf     : System.Address);
      pragma Import (C, Internal, "gtk_source_mark_attributes_set_pixbuf");
   begin
      Internal (Get_Object (Attributes), Get_Object (Pixbuf));
   end Set_Pixbuf;

   procedure Set_Stock_ID
     (Attributes : not null access Gtk_Source_Mark_Atributes_Record;
      Stock_ID   : UTF8_String)
   is
      procedure Internal
        (Attributes : System.Address;
         Stock_ID   : Interfaces.C.char_array);
      pragma Import (C, Internal, "gtk_source_mark_attributes_set_stock_id");
   begin
      Internal (Get_Object (Attributes), Interfaces.C.To_C (Stock_ID));
   end Set_Stock_ID;

   pragma Warnings (On, "declaration hides ""Attributes""");
   pragma Warnings (On, "declaration hides ""Background""");
   pragma Warnings (On, "declaration hides ""Icon""");
   pragma Warnings (On, "declaration hides ""Icon_Name""");
   pragma Warnings (On, "declaration hides ""Mark""");
   pragma Warnings (On, "declaration hides ""Pixbuf""");
   pragma Warnings (On, "declaration hides ""Size""");
   pragma Warnings (On, "declaration hides ""Stock_ID""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Source_Mark_Attributes;
