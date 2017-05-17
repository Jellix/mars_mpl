--                                                                    --
--  package Gtk.Widget.Styles       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2006       --
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

with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Gtk.Missed;                use Gtk.Missed;

with System.Address_To_Access_Conversions;

package body Gtk.Widget.Styles is
   use System;

   function Class_List_Style_Properties
            (  Class : GObject_Class
            )  return Param_Spec_Array is
      type Flat_Array is array (Positive) of aliased Param_Spec;
      pragma Convention (C, Flat_Array);
      type Flat_Array_Ptr is access all Flat_Array;
      pragma Convention (C, Flat_Array_Ptr);
      procedure G_Free (Mem : Flat_Array_Ptr);
      pragma Import (C, G_Free, "g_free");
      function Internal
               (  Class        : GObject_Class;
                  N_Properties : access Guint
               )  return Flat_Array_Ptr;
      pragma Import
             (  C,
                Internal,
                "gtk_widget_class_list_style_properties"
             );
      Count : aliased Guint;
      List  : constant Flat_Array_Ptr := Internal (Class, Count'Access);
   begin
      declare
         Result : Param_Spec_Array (1..Natural (Count));
      begin
         for Index in Result'Range loop
            Result (Index) := List (Index);
         end loop;
         G_Free (List);
         return Result;
      end;
   end Class_List_Style_Properties;

   function Generic_Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : String
            )  return Ada_Type is
      Value : GValue;
   begin
      Init (Value, GTK_Type);
      Style_Get_Property (Widget, Property_Name, Value);
      declare
         Result : constant Ada_Type := Get (Value);
      begin
         Unset (Value);
         return Result;
      end;
   end Generic_Style_Get;

   function Get_Path
            (  Widget   : not null access Gtk_Widget_Record'Class;
               Reversed : Boolean := False
            )  return UTF8_String is
      use Interfaces.C.Strings;
      procedure Get_Direct
                (  Widget        : Address;
                   Path_Length   : Address := Null_Address;
                   Path          : out chars_ptr;
                   Path_Reversed : Address := Null_Address
                );
      pragma Import (C, Get_Direct, "gtk_widget_path");
      procedure Get_Reversed
                (  Widget        : Address;
                   Path_Length   : Address := Null_Address;
                   Path          : Address := Null_Address;
                   Path_Reversed : out chars_ptr
                );
      pragma Import (C, Get_Reversed, "gtk_widget_path");
      Path : chars_ptr;
   begin
      if Reversed then
         Get_Reversed
         (  Get_Object (Widget),
            Path_Reversed => Path
         );
      else
         Get_Direct
         (  Get_Object (Widget),
            Path => Path
         );
      end if;
      declare
         Result : constant String := Value (Path);
      begin
         Free (Path);
         return Result;
      end;
   end Get_Path;

   function Get_Class_Path
            (  Widget   : not null access Gtk_Widget_Record'Class;
               Reversed : Boolean := False
            )  return UTF8_String is
      use Interfaces.C.Strings;
      procedure Get_Direct
                (  Widget        : Address;
                   Path_Length   : Address := Null_Address;
                   Path          : out chars_ptr;
                   Path_Reversed : Address := Null_Address
                );
      pragma Import (C, Get_Direct, "gtk_widget_class_path");
      procedure Get_Reversed
                (  Widget        : Address;
                   Path_Length   : Address := Null_Address;
                   Path          : Address := Null_Address;
                   Path_Reversed : out chars_ptr
                );
      pragma Import (C, Get_Reversed, "gtk_widget_class_path");
      Path : chars_ptr;
   begin
      if Reversed then
         Get_Reversed
         (  Get_Object (Widget),
            Path_Reversed => Path
         );
      else
         Get_Direct
         (  Get_Object (Widget),
            Path => Path
         );
      end if;
      declare
         Result : constant String := Value (Path);
      begin
         Free (Path);
         return Result;
      end;
   end Get_Class_Path;

   function Boolean_Type return GType is
   begin
      return GType_Boolean;
   end Boolean_Type;

   function Style_Get_Boolean_Impl is
      new Generic_Style_Get
          (  Boolean_Type,
             Boolean,
             Get_Boolean
          );

   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Boolean renames Style_Get_Boolean_Impl;

   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String;
               Default       : Gdk_Color
            )  return Gdk_Color is
      package Conversions is
         new System.Address_To_Access_Conversions (Gdk_Color);
      use Conversions;
      Result : Gdk_Color := Default;
      Value  : GValue;
      Ptr    : Address;
   begin
      Init (Value, Gdk_Color_Type);
      Style_Get_Property (Widget, Property_Name, Value);
      Ptr := Get_Boxed (Value);
      if Ptr /= Null_Address then
         Result := To_Pointer (Ptr).all;
      end if;
      Unset (Value);
      return Result;
--           declare
--              Value : Gdk_Color := Ptr.all;
--           begin
--              Color_Free (Ptr);
--              return Value;
--           end;
--        end if;

--        type Gdk_Color_Ptr is access all Gdk_Color;
--        pragma Convention (C, Gdk_Color_Ptr);
--        procedure Internal
--                  (  Widget        : Address;
--                     Property_Name : Property;
--                     Value         : out Gdk_Color_Ptr;
--                     List_End      : Address := Null_Address
--                  );
--        pragma Import (C, Internal, "gtk_widget_style_get");
--        procedure Color_Free (Color : Gdk_Color_Ptr);
--        pragma Import (C, Color_Free, "gdk_color_free");
--        Ptr : Gdk_Color_Ptr;
--     begin
--        Internal
--        (  Get_Object (Widget),
--           Build (Property_Name),
--           Ptr
--        );
--        if Ptr = null then
--           return Default;
--        else
--           declare
--              Value : Gdk_Color := Ptr.all;
--           begin
--              Color_Free (Ptr);
--              return Value;
--           end;
--        end if;
   end Style_Get;

   function Char_Type return GType is
   begin
      return GType_Char;
   end Char_Type;
   function Style_Get_Char_Impl is
      new Generic_Style_Get
          (  Char_Type,
             Gchar,
             Get_Char
          );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Gchar renames Style_Get_Char_Impl;

   function UChar_Type return GType is
   begin
      return GType_Uchar;
   end UChar_Type;
   function Style_Get_UChar_Impl is
      new Generic_Style_Get
          (  UChar_Type,
             Guchar,
             Get_Uchar
          );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Guchar renames Style_Get_UChar_Impl;

   function Int_Type return GType is
   begin
      return GType_Int;
   end Int_Type;
   function Style_Get_Int_Impl is
      new Generic_Style_Get
          (  Int_Type,
             Gint,
             Get_Int
          );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Gint renames Style_Get_Int_Impl;

   function UInt_Type return GType is
   begin
      return GType_Uint;
   end UInt_Type;
   function Style_Get_UInt_Impl is
      new Generic_Style_Get
          (  UInt_Type,
             Guint,
             Get_Uint
          );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Guint renames Style_Get_UInt_Impl;

   function Long_Type return GType is
   begin
      return GType_Long;
   end Long_Type;
   function Style_Get_Long_Impl is
      new Generic_Style_Get
          (  Long_Type,
             Glong,
             Get_Long
          );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Glong renames Style_Get_Long_Impl;

   function ULong_Type return GType is
   begin
      return GType_Ulong;
   end ULong_Type;
   function Style_Get_ULong_Impl is
      new Generic_Style_Get
          (  ULong_Type,
             Gulong,
             Get_ULong
          );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Gulong renames Style_Get_ULong_Impl;

   function Float_Type return GType is
   begin
      return GType_Float;
   end Float_Type;
   function Style_Get_Float_Impl is
      new Generic_Style_Get
          (  Float_Type,
             Gfloat,
             Get_Float
          );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Gfloat renames Style_Get_Float_Impl;

   function GDouble_Type return GType is
   begin
      return GType_Double;
   end GDouble_Type;
   function Style_Get_GDouble_Impl is
       new Generic_Style_Get
           (  GDouble_Type,
              Gdouble,
              Get_Double
           );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Gdouble renames Style_Get_GDouble_Impl;

   function String_Type return GType is
   begin
      return GType_String;
   end String_Type;
   function Style_Get_String_Impl is
       new Generic_Style_Get
           (  String_Type,
              UTF8_String,
              Get_String
           );
   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return UTF8_String renames Style_Get_String_Impl;

   function Style_Get
            (  Widget        : not null access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return GValue is
      Class     : constant GObject_Class :=
                  Class_From_Type (Widget.Get_Type);
      Parameter : Param_Spec;
      Value     : GValue;
   begin
      Parameter := Find_Style_Property (Class, Property_Name);
      if Parameter = null then
         Init (Value, GType_None);
      else
         Init (Value, Value_Type (Parameter));
         Style_Get_Property (Widget, Property_Name, Value);
      end if;
      return Value;
   end Style_Get;

end Gtk.Widget.Styles;
