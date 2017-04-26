--                                                                    --
--  package Pango.Cairo.Fonts       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  22:46 07 Apr 2016  --
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
with Cairo.Font_Face;
with Glib.Messages;
with Glib.Object;
with Gtk.Layered.Stream_IO;
with Gtk.Missed;
with Gtkada.Types;
with Pango.Layout;
with System;

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Glib.Error;

package body Pango.Cairo.Fonts is

   function Where (Name : String) return String;

   overriding procedure Adjust (Handle : in out Pango_Font_Description_Handle) is
   begin
      if Handle.Ptr /= null then
         Handle.Ptr.all.Count := Handle.Ptr.all.Count + 1;
      end if;
   end Adjust;

   overriding procedure Adjust (Handle : in out Cairo_Font_Face_Handle) is
   begin
      if Handle.Face /= Null_Font_Face then
         Handle.Face := Standard.Cairo.Font_Face.Reference (Handle.Face);
      end if;
   end Adjust;

   procedure Check (Font : Cairo_Font_Face_Handle) is
      use Ada.IO_Exceptions;
      Error : Cairo_Status;
   begin
      if Font.Face = Null_Font_Face then
         raise Status_Error with "Null font";
      else
         Error := Standard.Cairo.Font_Face.Status (Font.Face);
         if Error /= Cairo_Status_Success then
            raise Status_Error with Gtk.Missed.To_String (Error);
         end if;
      end if;
   end Check;

   procedure Check (Context : Cairo_Context);
   procedure Check (Context : Cairo_Context) is
      use Ada.IO_Exceptions;
      Error : Cairo_Status;
   begin
      Error := Status (Context);
      if Error /= Cairo_Status_Success then
         raise Status_Error with Gtk.Missed.To_String (Error);
      end if;
   end Check;

   function Create_Layout (Context : Cairo_Context) return Pango_Layout;
   function Create_Layout (Context : Cairo_Context) return Pango_Layout
   is
      function Internal (Context : Cairo_Context) return System.Address;
      pragma Import (C, Internal, "pango_cairo_create_layout");
      Stub : Pango_Layout_Record;
   begin
      return Pango_Layout (Glib.Object.Get_User_Data (Internal (Context), Stub));
   end Create_Layout;

   function Create_Markup_Layout (Handle  : Pango_Font_Description_Handle;
                                  Context : Cairo_Context;
                                  Text    : UTF8_String) return Pango_Layout
   is
      Description : constant Pango.Font.Pango_Font_Description :=
                    Handle.Ptr.all.Description;
      Layout : constant Pango_Layout := Create_Layout (Context);
   begin
      Layout.all.Set_Font_Description (Description);
      Layout.all.Set_Markup (Text);
      return Layout;
   end Create_Markup_Layout;

   function Create_Pango
     (Family  : String;
      Style   : Pango.Enums.Style   := Pango.Enums.Pango_Style_Normal;
      Variant : Pango.Enums.Variant := Pango.Enums.Pango_Variant_Normal;
      Weight  : Pango.Enums.Weight  := Pango.Enums.Pango_Weight_Normal;
      Stretch : Pango.Enums.Stretch := Pango.Enums.Pango_Stretch_Normal;
      Size    : Gint                := 12) return Pango_Cairo_Font is
   begin
      return
        (Mode => Pango_Font,
         Pango_Handle =>
           Ref
             (Pango.Font.To_Font_Description
                  (Family_Name => Family,
                   Style       => Style,
                   Variant     => Variant,
                   Weight      => Weight,
                   Stretch     => Stretch,
                   Size        => Size)));
   end Create_Pango;

   function Create_Pango_From_Description (Description : UTF8_String)
      return Pango_Cairo_Font is
   begin
      return
        (Mode         => Pango_Font,
         Pango_Handle => Ref (Pango.Font.From_String (Description)));
   end Create_Pango_From_Description;

   function Create_Text_Layout
     (Handle  : Pango_Font_Description_Handle;
      Context : Cairo_Context;
      Text    : UTF8_String) return Pango_Layout
   is
      Description : constant Pango.Font.Pango_Font_Description :=
                    Handle.Ptr.all.Description;
      Layout : constant Pango_Layout := Create_Layout (Context);
   begin
      Layout.all.Set_Font_Description (Description);
      Layout.all.Set_Text (Text);
      return Layout;
   end Create_Text_Layout;

   function Create_Toy
     (Family : UTF8_String;
      Slant  : Cairo_Font_Slant  := Cairo_Font_Slant_Normal;
      Weight : Cairo_Font_Weight := Cairo_Font_Weight_Normal)
      return Pango_Cairo_Font
   is
      Text : aliased Interfaces.C.char_array := Interfaces.C.To_C (Family);
   begin
      return Font : Pango_Cairo_Font (Toy_Font) do
         Font.Toy_Face.Face :=
           Standard.Cairo.Font_Face.Toy_Font_Face_Create -- Reference count is 1+
             (Family => Interfaces.C.Strings.To_Chars_Ptr (Text'Unchecked_Access),
              Slant  => Slant,
              Weight => Weight);
         Font.Toy_Face.Check;
      end return;
   end Create_Toy;

   overriding procedure Finalize (Handle : in out Pango_Font_Description_Handle)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation (Pango_Font_Description_Object,
                                         Pango_Font_Description_Object_Ptr);
   begin
      if Handle.Ptr /= null then
         Handle.Ptr.all.Count := Handle.Ptr.all.Count - 1;
         if Handle.Ptr.all.Count = 0 then
            Free (Handle.Ptr);
         end if;
      end if;
   end Finalize;

   overriding procedure Finalize
     (Object : in out Pango_Font_Description_Object)
   is
      use type Pango.Font.Pango_Font_Description;
   begin
      if Object.Description /= null then
         Pango.Font.Free (Object.Description);
         Object.Description := null;
      end if;
   end Finalize;

   overriding procedure Finalize (Handle : in out Cairo_Font_Face_Handle) is
   begin
      if Handle.Face /= Null_Font_Face then
         Standard.Cairo.Font_Face.Destroy (Handle.Face);
         Handle.Face := Null_Font_Face;
      end if;
   end Finalize;

   function Get_Family (Font : Pango_Cairo_Font) return UTF8_String is
   begin
      case Font.Mode is
         when Null_Font =>
            return "";
         when Toy_Font =>
            return Interfaces.C.Strings.Value (Font.Toy_Face.Get_Family);
         when Pango_Font =>
            return Pango.Font.Get_Family (Font.Pango_Handle.Ptr.all.Description);
      end case;
   end Get_Family;

   function Get_Family (Handle : Cairo_Font_Face_Handle)
      return UTF8_String is
   begin
      return
        Interfaces.C.Strings.Value
          (Standard.Cairo.Font_Face.Toy_Font_Face_Get_Family (Handle.Face));
   end Get_Family;

   function Get_Family (Handle : Cairo_Font_Face_Handle)
      return Interfaces.C.Strings.chars_ptr is
   begin
      if Handle.Face = Null_Font_Face then
         raise Constraint_Error with "Null toy font";
      else
         return Standard.Cairo.Font_Face.Toy_Font_Face_Get_Family (Handle.Face);
      end if;
   end Get_Family;

   procedure Get_Markup_Extents (Font    : Pango_Cairo_Font;
                                 Context : Cairo_Context;
                                 Text    : UTF8_String;
                                 Extents : out Cairo_Text_Extents) is
   begin
      case Font.Mode is
         when Null_Font =>
            Extents := (others => 0.0);
         when Toy_Font =>
            declare
               Data   : aliased Interfaces.C.char_array := Interfaces.C.To_C (Strip_Tags (Text));
               Result : aliased Cairo_Text_Extents;
            begin
               Set_Font (Context, Font.Toy_Face);
               Text_Extents
                 (Context,
                  Interfaces.C.Strings.To_Chars_Ptr (Data'Unchecked_Access),
                  Result'Access);
               Extents := Result;
            exception
               when Error : Gtkada.Types.Data_Error =>
                  Glib.Messages.Log
                    (Gtk.Missed.GtkAda_Contributions_Domain,
                     Glib.Messages.Log_Level_Warning,
                     "Pango markup: " &
                       Ada.Exceptions.Exception_Message (Error) &
                       Where ("Get_Markup_Extents"));
                  Extents := (others => 0.0);
            end;
         when Pango_Font =>
            declare
               Layout  : constant Pango_Layout :=
                           Create_Markup_Layout
                             (Font.Pango_Handle,
                              Context,
                              Text);
               Ink     : Pango_Rectangle;
               Logical : Pango_Rectangle;
            begin
               Layout.all.Get_Pixel_Extents (Ink, Logical);
               Extents := (Width     => Gdouble (Ink.Width),
                           Height    => Gdouble (Ink.Height),
                           X_Bearing => Gdouble (Ink.X),
                           Y_Bearing => Gdouble (Ink.Y),
                           others    => 0.0);
--                 Width  : GInt;
--                 Height : GInt;
--              begin
--                 Layout.Get_Pixel_Size (Width, Height);
--                 Extents := (  Width  => GDouble (Width),
--                               Height => GDouble (Height),
--                               others => 0.0
--                            );
               Layout.all.Unref;
            exception
               when others =>
                  Layout.all.Unref;
                  raise;
            end;
      end case;
   end Get_Markup_Extents;

   function Get_Size (Font : Pango_Cairo_Font) return Gint is
   begin
      case Font.Mode is
         when Null_Font | Toy_Font =>
            return 12; -- There is no size for these types of font
         when Pango_Font =>
            return Pango.Font.Get_Size (Font.Pango_Handle.Ptr.all.Description);
      end case;
   end Get_Size;

   function Get_Slant (Font : Pango_Cairo_Font)
      return Cairo_Font_Slant is
   begin
      case Font.Mode is
         when Null_Font =>
            return Cairo_Font_Slant_Normal;
         when Toy_Font =>
            return Font.Toy_Face.Get_Slant;
         when Pango_Font =>
            case Pango.Font.Get_Style (Font.Pango_Handle.Ptr.all.Description) is
               when Pango.Enums.Pango_Style_Normal =>
                  return Cairo_Font_Slant_Normal;
               when Pango.Enums.Pango_Style_Oblique =>
                  return Cairo_Font_Slant_Italic;
               when Pango.Enums.Pango_Style_Italic =>
                  return Cairo_Font_Slant_Oblique;
            end case;
      end case;
   end Get_Slant;

   function Get_Slant (Handle : Cairo_Font_Face_Handle)
      return Cairo_Font_Slant is
   begin
      if Handle.Face = Null_Font_Face then
         raise Constraint_Error with "Null toy font";
      else
         return Standard.Cairo.Font_Face.Toy_Font_Face_Get_Slant (Handle.Face);
      end if;
   end Get_Slant;

   procedure Get_Text_Extents (Font    : Pango_Cairo_Font;
                               Context : Cairo_Context;
                               Text    : UTF8_String;
                               Extents : out Cairo_Text_Extents) is
   begin
      case Font.Mode is
         when Null_Font =>
            Extents := (others => 0.0);
         when Toy_Font =>
            declare
               Data   : aliased Interfaces.C.char_array := Interfaces.C.To_C (Text);
               Result : aliased Cairo_Text_Extents;
            begin
               Set_Font (Context, Font.Toy_Face);
               Text_Extents
                 (Context,
                  Interfaces.C.Strings.To_Chars_Ptr (Data'Unchecked_Access),
                  Result'Access);
               Extents := Result;
            end;
         when Pango_Font =>
            declare
               Layout : constant Pango_Layout :=
                        Create_Text_Layout
                            (Font.Pango_Handle,
                             Context,
                             Text);
               Ink     : Pango_Rectangle;
               Logical : Pango_Rectangle;
            begin
               Layout.all.Get_Pixel_Extents (Ink, Logical);
               Extents := (Width     => Gdouble (Ink.Width),
                           Height    => Gdouble (Ink.Height),
                           X_Bearing => Gdouble (Ink.X),
                           Y_Bearing => Gdouble (Ink.Y),
                           others    => 0.0);
--                 Width  : GInt;
--                 Height : GInt;
--              begin
--                 Layout.Get_Pixel_Size (Width, Height);
--                 Extents := (  Width  => GDouble (Width),
--                               Height => GDouble (Height),
--                               others => 0.0
--                            );
               Layout.all.Unref;
            exception
               when others =>
                  Layout.all.Unref;
                  raise;
            end;
      end case;
   end Get_Text_Extents;

   function Get_Type (Font : Pango_Cairo_Font) return Font_Type is
   begin
      return Font.Mode;
   end Get_Type;

   function Get_Weight (Handle : Cairo_Font_Face_Handle)
      return Cairo_Font_Weight is
   begin
      if Handle.Face = Null_Font_Face then
         raise Constraint_Error with "Null toy font";
      else
         return Standard.Cairo.Font_Face.Toy_Font_Face_Get_Weight (Handle.Face);
      end if;
   end Get_Weight;

   function Get_Weight (Font : Pango_Cairo_Font)
      return Cairo_Font_Weight is
   begin
      case Font.Mode is
         when Null_Font =>
            return Cairo_Font_Weight_Normal;
         when Toy_Font =>
            return Font.Toy_Face.Get_Weight;
         when Pango_Font =>
            case Pango.Font.Get_Weight (Font.Pango_Handle.Ptr.all.Description) is
               when Pango.Enums.Pango_Weight_Thin .. Pango.Enums.Pango_Weight_Medium =>
                  return Cairo_Font_Weight_Normal;
               when Pango.Enums.Pango_Weight_Semibold .. Pango.Enums.Pango_Weight_Ultraheavy =>
                  return Cairo_Font_Weight_Bold;
            end case;
      end case;
   end Get_Weight;

   function Get_Weight (Font : Pango_Cairo_Font)
      return Pango.Enums.Weight is
   begin
      case Font.Mode is
         when Null_Font =>
            return Pango.Enums.Pango_Weight_Normal;
         when Toy_Font =>
            case Font.Toy_Face.Get_Weight is
               when Cairo_Font_Weight_Normal =>
                  return Pango.Enums.Pango_Weight_Normal;
               when Cairo_Font_Weight_Bold =>
                  return Pango.Enums.Pango_Weight_Bold;
            end case;
         when Pango_Font =>
            return Pango.Font.Get_Weight (Font.Pango_Handle.Ptr.all.Description);
      end case;
   end Get_Weight;

   function Ref (Description : Pango.Font.Pango_Font_Description)
      return Pango_Font_Description_Handle is
   begin
      return Result : Pango_Font_Description_Handle do
         Result.Ptr := new Pango_Font_Description_Object;
         Result.Ptr.all.Count := 1;
         Result.Ptr.all.Description := Description;
      end return;
   end Ref;

   procedure Restore (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                      Font   : out Pango_Cairo_Font) is
      Tag : Font_Type;
   begin
      declare
         Data : Guint;
      begin
         Gtk.Layered.Stream_IO.Restore (Stream, Data);
         Tag := Font_Type'Val (Data);
      exception
         when Constraint_Error =>
            raise Gtkada.Types.Data_Error with "Font type out of range";
      end;
      case Tag is
         when Null_Font =>
            Font := (Mode => Null_Font);
         when Toy_Font =>
            declare
               Face : Cairo_Font_Face;
            begin
               Gtk.Layered.Stream_IO.Restore (Stream, Face); -- Reference count is 1
               Font :=
                 (Mode     => Toy_Font,
                  Toy_Face => (Ada.Finalization.Controlled with Face));
            end;
         when Pango_Font =>
            Font :=
              (Mode         => Pango_Font,
               Pango_Handle => Ref (Pango.Font.From_String (Gtk.Layered.Stream_IO.Restore (Stream'Access))));
      end case;
   end Restore;

   procedure Set_Family (Font   : in out Pango_Cairo_Font;
                         Family : UTF8_String) is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Font :=
               Create_Toy
                (Family => Family,
                 Slant  => Font.Toy_Face.Get_Slant,
                 Weight => Font.Toy_Face.Get_Weight);
         when Pango_Font =>
            Pango.Font.Set_Family (Font.Pango_Handle.Ptr.all.Description, Family);
      end case;
   end Set_Family;

   procedure Set_Font (Context : Cairo_Context;
                       Font    : Cairo_Font_Face_Handle) is
   begin
      Check (Context);
      Font.Check;
      Set_Font_Face (Context, Font.Face);
      if Get_Font_Face (Context) /= Font.Face then
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Failed to set font: " &
              Gtk.Missed.To_String
              (Standard.Cairo.Font_Face.Status (Get_Font_Face (Context))) &
              Where ("Set_Font"));
      end if;
   end Set_Font;

   procedure Set_Size (Font : in out Pango_Cairo_Font; Size : Gint) is
   begin
      case Font.Mode is
         when Null_Font | Toy_Font =>
            null;
         when Pango_Font =>
            Pango.Font.Set_Size (Font.Pango_Handle.Ptr.all.Description, Size);
      end case;
   end Set_Size;

   procedure Set_Slant (Font  : in out Pango_Cairo_Font;
                        Slant : Cairo_Font_Slant) is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Font :=
               Create_Toy
                (Family => Font.Toy_Face.Get_Family,
                 Slant  => Slant,
                 Weight => Font.Toy_Face.Get_Weight);
         when Pango_Font =>
            case Slant is
               when Cairo_Font_Slant_Normal =>
                  Pango.Font.Set_Style
                    (Font.Pango_Handle.Ptr.all.Description,
                     Pango.Enums.Pango_Style_Normal);
               when Cairo_Font_Slant_Italic =>
                  Pango.Font.Set_Style
                    (Font.Pango_Handle.Ptr.all.Description,
                     Pango.Enums.Pango_Style_Oblique);
               when Cairo_Font_Slant_Oblique =>
                  Pango.Font.Set_Style
                    (Font.Pango_Handle.Ptr.all.Description,
                     Pango.Enums.Pango_Style_Italic);
            end case;
      end case;
   end Set_Slant;

   procedure Set_Type (Font : in out Pango_Cairo_Font;
                       Mode : Font_Type) is
   begin
      if Mode /= Font.Mode then
         case Mode is
            when Null_Font =>
               Font := (Mode => Null_Font);
            when Toy_Font =>
               Font :=
                  Create_Toy
                   (Family => Get_Family (Font),
                    Slant  => Get_Slant  (Font),
                    Weight => Get_Weight (Font));
            when Pango_Font =>
               case Get_Slant  (Font) is
                  when Cairo_Font_Slant_Normal =>
                     Font :=
                        Create_Pango
                         (Family => Get_Family (Font),
                          Style  => Pango.Enums.Pango_Style_Normal,
                          Weight => Get_Weight (Font));
                  when Cairo_Font_Slant_Italic =>
                     Font :=
                        Create_Pango
                         (Family => Get_Family (Font),
                          Style  => Pango.Enums.Pango_Style_Italic,
                          Weight => Get_Weight (Font));
                  when Cairo_Font_Slant_Oblique =>
                     Font :=
                        Create_Pango
                         (Family => Get_Family (Font),
                          Style  => Pango.Enums.Pango_Style_Oblique,
                          Weight => Get_Weight (Font));
               end case;
         end case;
      end if;
   end Set_Type;

   procedure Set_Weight (Font   : in out Pango_Cairo_Font;
                         Weight : Cairo_Font_Weight) is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Font :=
               Create_Toy
                (Family => Font.Toy_Face.Get_Family,
                 Slant  => Font.Toy_Face.Get_Slant,
                 Weight => Weight);
         when Pango_Font =>
            case Weight is
               when Cairo_Font_Weight_Normal =>
                  Pango.Font.Set_Weight
                    (Font.Pango_Handle.Ptr.all.Description,
                     Pango.Enums.Pango_Weight_Normal);
               when Cairo_Font_Weight_Bold =>
                  Pango.Font.Set_Weight
                    (Font.Pango_Handle.Ptr.all.Description,
                     Pango.Enums.Pango_Weight_Bold);
            end case;
      end case;
   end Set_Weight;

   procedure Set_Weight (Font   : in out Pango_Cairo_Font;
                         Weight : Pango.Enums.Weight) is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            case Weight is
               when Pango.Enums.Pango_Weight_Thin .. Pango.Enums.Pango_Weight_Medium =>
                  Font :=
                     Create_Toy
                      (Family => Font.Toy_Face.Get_Family,
                       Slant  => Font.Toy_Face.Get_Slant,
                       Weight => Cairo_Font_Weight_Normal);
               when Pango.Enums.Pango_Weight_Semibold .. Pango.Enums.Pango_Weight_Ultraheavy =>
                  Font :=
                     Create_Toy
                      (Family => Font.Toy_Face.Get_Family,
                       Slant  => Font.Toy_Face.Get_Slant,
                       Weight => Cairo_Font_Weight_Bold);
            end case;
         when Pango_Font =>
            Pango.Font.Set_Weight (Font.Pango_Handle.Ptr.all.Description, Weight);
      end case;
   end Set_Weight;

   procedure Show_Markup (Font    : Pango_Cairo_Font;
                          Context : Cairo_Context;
                          Text    : UTF8_String) is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            begin
               Set_Font (Context, Font.Toy_Face);
               Show_Text (Context, Strip_Tags (Text));
            exception
               when Error : Gtkada.Types.Data_Error =>
                  Glib.Messages.Log
                    (Gtk.Missed.GtkAda_Contributions_Domain,
                     Glib.Messages.Log_Level_Warning,
                     "Pango markup: " &
                       Ada.Exceptions.Exception_Message (Error) &
                       Where ("Set_Font"));
            end;
         when Pango_Font =>
            declare
               Layout : constant Pango_Layout :=
                          Create_Markup_Layout
                            (Font.Pango_Handle,
                             Context,
                             Text);
            begin
               Show_Layout (Context, Layout);
               Layout.all.Unref;
            end;
      end case;
   end Show_Markup;

   procedure Show_Text (Font    : Pango_Cairo_Font;
                        Context : Cairo_Context;
                        Text    : UTF8_String) is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Set_Font (Context, Font.Toy_Face);
            Show_Text (Context, Text);
         when Pango_Font =>
            declare
               Layout : constant Pango_Layout :=
                          Create_Text_Layout
                            (Font.Pango_Handle,
                             Context,
                             Text);
            begin
               Show_Layout (Context, Layout);
--             Show_Layout_Line (Context, Layout);
               Layout.all.Unref;
            end;
      end case;
   end Show_Text;

   procedure Store (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                    Font   : Pango_Cairo_Font) is
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Guint (Font_Type'Pos (Font.Mode)));
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Gtk.Layered.Stream_IO.Store (Stream, Font.Toy_Face.Face);
         when Pango_Font =>
            Gtk.Layered.Stream_IO.Store
              (Stream,
               Pango.Font.To_String (Font.Pango_Handle.Ptr.all.Description));
      end case;
   end Store;

   function Strip_Tags (Text : UTF8_String) return UTF8_String is
      use Glib.Error;
      use System;
      use Interfaces.C;

      function Internal (Markup_Text  : Address;
                         Length       : int;
                         Accel_Marker : Gunichar := 0;
                         Attr_List    : Address  := Null_Address;
                         Text         : access Interfaces.C.Strings.chars_ptr;
                         Accel_Char   : Address  := Null_Address;
                         Error        : access GError) return Gboolean;
      pragma Import (C, Internal, "pango_parse_markup");
      Result : aliased Interfaces.C.Strings.chars_ptr;
      Error  : aliased GError;
   begin
      if 0 =  Internal (Markup_Text => Text (Text'First)'Address,
                        Length      => Text'Length,
                        Text        => Result'Access,
                        Error       => Error'Access)
      then
         return Text;
      else
         return Value : constant String :=
                        Interfaces.C.Strings.Value (Result) do
            Gtkada.Types.g_free (Result);
         end return;
      end if;
   end Strip_Tags;

   function Where (Name : String) return String is
   begin
      return " in Pango.Cairo.Fonts." & Name;
   end Where;

--     procedure Show_Layout_Line
--               (  Context : Cairo_Context;
--                  Layout  : not null access Pango_Layout_Record'Class;
--                  Line    : Natural := 0
--               )  is
--        function Get
--                 (  Layout : Address;
--                    Line   : Gint
--                 )  return Address;
--        pragma Import (C, Get, "pango_layout_get_line_readonly");
--        procedure Show
--                  (  Context : Cairo_Context;
--                     Line    : Address
--                  );
--        pragma Import (C, Show, "pango_cairo_show_layout_line");
--        procedure Unref (Line : Address);
--        pragma Import (C, Unref, "pango_layout_line_unref");
--        This : Address := Get (Get_Object (Layout), GInt (Line));
--     begin
--        if This /= Null_Address then
--           Show (Context, This);
--           Unref (This);
--        end if;
--     end Show_Layout_Line;

end Pango.Cairo.Fonts;
