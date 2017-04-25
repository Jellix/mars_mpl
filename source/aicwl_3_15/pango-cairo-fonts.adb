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
--____________________________________________________________________--

with Ada.Exceptions;         use Ada.Exceptions;
with GLib.Messages;          use GLib.Messages;
with GLib.Object;            use GLib.Object;
with Gtk.Layered;            use Gtk.Layered;
with Gtk.Layered.Stream_IO;  use Gtk.Layered.Stream_IO;
with Gtk.Missed;             use Gtk.Missed;
with GtkAda.Types;           use GtkAda.Types;
with Interfaces.C;           use Interfaces.C;
with Interfaces.C.Strings;   use Interfaces.C.Strings;
with Pango.Layout;           use Pango.Layout;
with System;                 use System;

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Glib.Error;

package body Pango.Cairo.Fonts is

   function Where (Name : String) return String is
   begin
      return " in Pango.Cairo.Fonts." & Name;
   end Where;

   procedure Adjust (Handle : in out Pango_Font_Description_Handle) is
   begin
      if Handle.Ptr /= null then
         Handle.Ptr.Count := Handle.Ptr.Count + 1;
      end if;
   end Adjust;

   procedure Adjust (Handle : in out Cairo_Font_Face_Handle) is
   begin
      if Handle.Face /= Null_Font_Face then
         Handle.Face := Reference (Handle.Face);
      end if;
   end Adjust;

   procedure Check (Font : Cairo_Font_Face_Handle) is
      use Ada.IO_Exceptions;
      Error : Cairo_Status;
   begin
      if Font.Face = Null_Font_Face then
         raise Status_Error with "Null font";
      else
         Error := Status (Font.Face);
         if Error /= CAIRO_STATUS_SUCCESS then
            raise Status_Error with To_String (Error);
         end if;
      end if;
   end Check;

   procedure Check (Context : Cairo_Context) is
      use Ada.IO_Exceptions;
      Error : Cairo_Status;
   begin
      Error := Status (Context);
      if Error /= CAIRO_STATUS_SUCCESS then
         raise Status_Error with To_String (Error);
      end if;
   end Check;

   function Create_Layout (Context : Cairo_Context)
      return Pango_Layout is
      function Internal (Context : Cairo_Context) return Address;
      pragma Import (C, Internal, "pango_cairo_create_layout");
      Stub : Pango_Layout_Record;
   begin
     return Pango_Layout (Get_User_Data (Internal (Context), Stub));
   end Create_Layout;

   function Create_Markup_Layout
            (  Handle  : Pango_Font_Description_Handle;
               Context : Cairo_Context;
               Text    : UTF8_String
            )  return Pango_Layout is
      Description : constant Pango_Font_Description :=
                    Handle.Ptr.Description;
      Layout : constant Pango_Layout := Create_Layout (Context);
   begin
      Layout.Set_Font_Description (Description);
      Layout.Set_Markup (Text);
      return Layout;
   end Create_Markup_Layout;

   function Create_Pango
            (  Family  : String;
               Style   : Pango.Enums.Style   := Pango_Style_Normal;
               Variant : Pango.Enums.Variant := Pango_Variant_Normal;
               Weight  : Pango.Enums.Weight  := Pango_Weight_Normal;
               Stretch : Pango.Enums.Stretch := Pango_Stretch_Normal;
               Size    : GInt                := 12
            )  return Pango_Cairo_Font is
   begin
      return
      (  Mode =>
            Pango_Font,
         Pango_Handle =>
            Ref
            (  To_Font_Description
               (  Family_Name => Family,
                  Style       => Style,
                  Variant     => Variant,
                  Weight      => Weight,
                  Stretch     => Stretch,
                  Size        => Size
      )     )  );
   end Create_Pango;

   function Create_Pango_From_Description (Description : UTF8_String)
      return Pango_Cairo_Font is
   begin
      return
      (  Mode         => Pango_Font,
         Pango_Handle => Ref (From_String (Description))
      );
   end Create_Pango_From_Description;

   function Create_Text_Layout
            (  Handle  : Pango_Font_Description_Handle;
               Context : Cairo_Context;
               Text    : UTF8_String
            )  return Pango_Layout is
      Description : constant Pango_Font_Description :=
                    Handle.Ptr.Description;
      Layout : constant Pango_Layout := Create_Layout (Context);
   begin
      Layout.Set_Font_Description (Description);
      Layout.Set_Text (Text);
      return Layout;
   end Create_Text_Layout;

   function Create_Toy
            (  Family : UTF8_String;
               Slant  : Cairo_Font_Slant  := CAIRO_FONT_SLANT_NORMAL;
               Weight : Cairo_Font_Weight := CAIRO_FONT_WEIGHT_NORMAL
            )  return Pango_Cairo_Font is
      Text : aliased Char_Array := To_C (Family);
   begin
      return Font : Pango_Cairo_Font (Toy_Font) do
         Font.Toy_Face.Face :=
            Toy_Font_Face_Create -- Reference count is 1+
            (  Family => To_Chars_Ptr (Text'Unchecked_Access),
               Slant  => Slant,
               Weight => Weight
            );
         Font.Toy_Face.Check;
      end return;
   end Create_Toy;

   procedure Finalize
             (  Handle : in out Pango_Font_Description_Handle
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Pango_Font_Description_Object,
                Pango_Font_Description_Object_Ptr
             );
   begin
      if Handle.Ptr /= null then
         Handle.Ptr.Count := Handle.Ptr.Count - 1;
         if Handle.Ptr.Count = 0 then
            Free (Handle.Ptr);
         end if;
      end if;
   end Finalize;

   procedure Finalize
             (  Object : in out Pango_Font_Description_Object
             )  is
   begin
      if Object.Description /= null then
         Free (Object.Description);
         Object.Description := null;
      end if;
   end Finalize;

   procedure Finalize (Handle : in out Cairo_Font_Face_Handle) is
   begin
      if Handle.Face /= Null_Font_Face then
         Destroy (Handle.Face);
         Handle.Face := Null_Font_Face;
      end if;
   end Finalize;

   function Get_Family (Font : Pango_Cairo_Font) return UTF8_String is
   begin
      case Font.Mode is
         when Null_Font =>
            return "";
         when Toy_Font =>
            return Value (Font.Toy_Face.Get_Family);
         when Pango_Font =>
            return Get_Family (Font.Pango_Handle.Ptr.Description);
      end case;
   end Get_Family;

   function Get_Family (Handle : Cairo_Font_Face_Handle)
      return UTF8_String is
   begin
      return Value (Toy_Font_Face_Get_Family (Handle.Face));
   end Get_Family;

   function Get_Family (Handle : Cairo_Font_Face_Handle)
      return Interfaces.C.Strings.Chars_Ptr is
   begin
      if Handle.Face = Null_Font_Face then
         raise Constraint_Error with "Null toy font";
      else
         return Toy_Font_Face_Get_Family (Handle.Face);
      end if;
   end Get_Family;

   procedure Get_Markup_Extents
             (  Font    : Pango_Cairo_Font;
                Context : Cairo_Context;
                Text    : UTF8_String;
                Extents : out Cairo_Text_Extents
             )  is
   begin
      case Font.Mode is
         when Null_Font =>
            Extents := (others => 0.0);
         when Toy_Font =>
            declare
               Data   : aliased Char_Array := To_C (Strip_Tags (Text));
               Result : aliased Cairo_Text_Extents;
            begin
               Set_Font (Context, Font.Toy_Face);
               Text_Extents
               (  Context,
                  To_Chars_Ptr (Data'Unchecked_Access),
                  Result'Access
               );
               Extents := Result;
            exception
               when Error : Data_Error =>
                  Log
                  (  GtkAda_Contributions_Domain,
                     Log_Level_Warning,
                     (  "Pango markup: "
                     &  Exception_Message (Error)
                     &  Where ("Get_Markup_Extents")
                  )  );
                  Extents := (others => 0.0);
            end;
         when Pango_Font =>
            declare
               Layout  : constant Pango_Layout :=
                         Create_Markup_Layout
                         (  Font.Pango_Handle,
                            Context,
                            Text
                         );
               Ink     : Pango_Rectangle;
               Logical : Pango_Rectangle;
            begin
               Layout.Get_Pixel_Extents (Ink, Logical);
               Extents := (  Width     => GDouble (Ink.Width),
                             Height    => GDouble (Ink.Height),
                             X_Bearing => GDouble (Ink.X),
                             Y_Bearing => GDouble (Ink.Y),
                             others => 0.0
                          );
--                 Width  : GInt;
--                 Height : GInt;
--              begin
--                 Layout.Get_Pixel_Size (Width, Height);
--                 Extents := (  Width  => GDouble (Width),
--                               Height => GDouble (Height),
--                               others => 0.0
--                            );
               Layout.Unref;
            exception
               when others =>
                  Layout.Unref;
                  raise;
            end;
      end case;
   end Get_Markup_Extents;

   function Get_Slant (Handle : Cairo_Font_Face_Handle)
      return Cairo_Font_Slant is
   begin
      if Handle.Face = Null_Font_Face then
         raise Constraint_Error with "Null toy font";
      else
         return Toy_Font_Face_Get_Slant (Handle.Face);
      end if;
   end Get_Slant;

   function Get_Size (Font : Pango_Cairo_Font) return GInt is
   begin
      case Font.Mode is
         when Null_Font | Toy_Font =>
            return 12; -- There is no size for these types of font
         when Pango_Font =>
            return Get_Size (Font.Pango_Handle.Ptr.Description);
      end case;
   end Get_Size;

   function Get_Weight (Handle : Cairo_Font_Face_Handle)
      return Cairo_Font_Weight is
   begin
      if Handle.Face = Null_Font_Face then
         raise Constraint_Error with "Null toy font";
      else
         return Toy_Font_Face_Get_Weight (Handle.Face);
      end if;
   end Get_Weight;

   function Get_Slant (Font : Pango_Cairo_Font)
      return Cairo_Font_Slant is
   begin
      case Font.Mode is
         when Null_Font =>
            return CAIRO_FONT_SLANT_NORMAL;
         when Toy_Font =>
            return Font.Toy_Face.Get_Slant;
         when Pango_Font =>
            case Get_Style (Font.Pango_Handle.Ptr.Description) is
               when Pango_Style_Normal =>
                  return CAIRO_FONT_SLANT_NORMAL;
               when Pango_Style_Oblique =>
                  return CAIRO_FONT_SLANT_ITALIC;
               when Pango_Style_Italic =>
                  return CAIRO_FONT_SLANT_OBLIQUE;
            end case;
      end case;
   end Get_Slant;

   procedure Get_Text_Extents
             (  Font    : Pango_Cairo_Font;
                Context : Cairo_Context;
                Text    : UTF8_String;
                Extents : out Cairo_Text_Extents
             )  is
   begin
      case Font.Mode is
         when Null_Font =>
            Extents := (others => 0.0);
         when Toy_Font =>
            declare
               Data   : aliased Char_Array := To_C (Text);
               Result : aliased Cairo_Text_Extents;
            begin
               Set_Font (Context, Font.Toy_Face);
               Text_Extents
               (  Context,
                  To_Chars_Ptr (Data'Unchecked_Access),
                  Result'Access
               );
               Extents := Result;
            end;
         when Pango_Font =>
            declare
               Layout : constant Pango_Layout :=
                        Create_Text_Layout
                        (  Font.Pango_Handle,
                           Context,
                           Text
                        );
               Ink     : Pango_Rectangle;
               Logical : Pango_Rectangle;
            begin
               Layout.Get_Pixel_Extents (Ink, Logical);
               Extents := (  Width     => GDouble (Ink.Width),
                             Height    => GDouble (Ink.Height),
                             X_Bearing => GDouble (Ink.X),
                             Y_Bearing => GDouble (Ink.Y),
                             others => 0.0
                          );
--                 Width  : GInt;
--                 Height : GInt;
--              begin
--                 Layout.Get_Pixel_Size (Width, Height);
--                 Extents := (  Width  => GDouble (Width),
--                               Height => GDouble (Height),
--                               others => 0.0
--                            );
               Layout.Unref;
            exception
               when others =>
                  Layout.Unref;
                  raise;
            end;
      end case;
   end Get_Text_Extents;

   function Get_Type (Font : Pango_Cairo_Font) return Font_Type is
   begin
      return Font.Mode;
   end Get_Type;

   function Get_Weight (Font : Pango_Cairo_Font)
      return Cairo_Font_Weight is
   begin
      case Font.Mode is
         when Null_Font =>
            return CAIRO_FONT_WEIGHT_NORMAL;
         when Toy_Font =>
            return Font.Toy_Face.Get_Weight;
         when Pango_Font =>
            case Get_Weight (Font.Pango_Handle.Ptr.Description) is
               when Pango_Weight_Thin..Pango_Weight_Medium =>
                  return CAIRO_FONT_WEIGHT_NORMAL;
               when Pango_Weight_Semibold..Pango_Weight_Ultraheavy =>
                  return CAIRO_FONT_WEIGHT_BOLD;
            end case;
      end case;
   end Get_Weight;

   function Get_Weight (Font : Pango_Cairo_Font)
      return Pango.Enums.Weight is
   begin
      case Font.Mode is
         when Null_Font =>
            return Pango_Weight_Normal;
         when Toy_Font =>
            case Font.Toy_Face.Get_Weight is
               when CAIRO_FONT_WEIGHT_NORMAL =>
                  return Pango_Weight_Normal;
               when CAIRO_FONT_WEIGHT_BOLD =>
                  return Pango_Weight_Bold;
            end case;
         when Pango_Font =>
            return Get_Weight (Font.Pango_Handle.Ptr.Description);
      end case;
   end Get_Weight;

   function Ref (Description : Pango_Font_Description)
      return Pango_Font_Description_Handle is
   begin
      return Result : Pango_Font_Description_Handle do
         Result.Ptr := new Pango_Font_Description_Object;
         Result.Ptr.Count := 1;
         Result.Ptr.Description := Description;
      end return;
   end Ref;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Font   : out Pango_Cairo_Font
             )  is
      Tag : Font_Type;
   begin
      declare
         Data : GUInt;
      begin
         Restore (Stream, Data);
         Tag := Font_Type'Val (Data);
      exception
         when Constraint_Error =>
            raise Data_Error with "Font type out of range";
      end;
      case Tag is
         when Null_Font =>
            Font := (Mode => Null_Font);
         when Toy_Font =>
            declare
               Face : Cairo_Font_Face;
            begin
               Restore (Stream, Face); -- Reference count is 1
               Font :=
                  (  Mode     => Toy_Font,
                     Toy_Face => (Ada.Finalization.Controlled with Face)
                  );
            end;
         when Pango_Font =>
            Font :=
               (  Mode =>
                     Pango_Font,
                  Pango_Handle =>
                     Ref (From_String (Restore (Stream'Access)))
               );
      end case;
   end Restore;

   procedure Set_Family
             (  Font   : in out Pango_Cairo_Font;
                Family : UTF8_String
             )  is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Font :=
               Create_Toy
               (  Family => Family,
                  Slant  => Font.Toy_Face.Get_Slant,
                  Weight => Font.Toy_Face.Get_Weight
               );
         when Pango_Font =>
            Set_Family (Font.Pango_Handle.Ptr.Description, Family);
      end case;
   end Set_Family;

   procedure Set_Font
             (  Context : Cairo_Context;
                Font    : Cairo_Font_Face_Handle
             )  is
   begin
      Check (Context);
      Font.Check;
      Set_Font_Face (Context, Font.Face);
      if Get_Font_Face (Context) /= Font.Face then
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Failed to set font: "
            &  To_String (Status (Get_Font_Face (Context)))
            &  Where ("Set_Font")
         )  );
      end if;
   end Set_Font;

   procedure Set_Size (Font : in out Pango_Cairo_Font; Size : GInt) is
   begin
      case Font.Mode is
         when Null_Font | Toy_Font =>
            null;
         when Pango_Font =>
            Set_Size (Font.Pango_Handle.Ptr.Description, Size);
      end case;
   end Set_Size;

   procedure Set_Slant
             (  Font  : in out Pango_Cairo_Font;
                Slant : Cairo_Font_Slant
             )  is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Font :=
               Create_Toy
               (  Family => Font.Toy_Face.Get_Family,
                  Slant  => Slant,
                  Weight => Font.Toy_Face.Get_Weight
               );
         when Pango_Font =>
            case Slant is
               when CAIRO_FONT_SLANT_NORMAL =>
                  Set_Style
                  (  Font.Pango_Handle.Ptr.Description,
                     Pango_Style_Normal
                  );
               when CAIRO_FONT_SLANT_ITALIC =>
                  Set_Style
                  (  Font.Pango_Handle.Ptr.Description,
                     Pango_Style_Oblique
                  );
               when CAIRO_FONT_SLANT_OBLIQUE =>
                  Set_Style
                  (  Font.Pango_Handle.Ptr.Description,
                     Pango_Style_Italic
                  );
            end case;
      end case;
   end Set_Slant;

   procedure Set_Type
             (  Font : in out Pango_Cairo_Font;
                Mode : Font_Type
             )  is
   begin
      if Mode /= Font.Mode then
         case Mode is
            when Null_Font =>
               Font := (Mode => Null_Font);
            when Toy_Font =>
               Font :=
                  Create_Toy
                  (  Family => Get_Family (Font),
                     Slant  => Get_Slant  (Font),
                     Weight => Get_Weight (Font)
                  );
            when Pango_Font =>
               case Get_Slant  (Font) is
                  when CAIRO_FONT_SLANT_NORMAL =>
                     Font :=
                        Create_Pango
                        (  Family => Get_Family (Font),
                           Style  => Pango_Style_Normal,
                           Weight => Get_Weight (Font)
                        );
                  when CAIRO_FONT_SLANT_ITALIC =>
                     Font :=
                        Create_Pango
                        (  Family => Get_Family (Font),
                           Style  => Pango_Style_Italic,
                           Weight => Get_Weight (Font)
                        );
                  when CAIRO_FONT_SLANT_OBLIQUE =>
                     Font :=
                        Create_Pango
                        (  Family => Get_Family (Font),
                           Style  => Pango_Style_Oblique,
                           Weight => Get_Weight (Font)
                        );
               end case;
         end case;
      end if;
   end Set_Type;

   procedure Set_Weight
             (  Font   : in out Pango_Cairo_Font;
                Weight : Cairo_Font_Weight
             )  is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Font :=
               Create_Toy
               (  Family => Font.Toy_Face.Get_Family,
                  Slant  => Font.Toy_Face.Get_Slant,
                  Weight => Weight
               );
         when Pango_Font =>
            case Weight is
               when CAIRO_FONT_WEIGHT_NORMAL =>
                   Set_Weight
                   (  Font.Pango_Handle.Ptr.Description,
                      Pango_Weight_Normal
                   );
               when CAIRO_FONT_WEIGHT_BOLD =>
                   Set_Weight
                   (  Font.Pango_Handle.Ptr.Description,
                      Pango_Weight_Bold
                   );
            end case;
      end case;
   end Set_Weight;

   procedure Set_Weight
             (  Font   : in out Pango_Cairo_Font;
                Weight : Pango.Enums.Weight
             )  is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            case Weight is
               when Pango_Weight_Thin..Pango_Weight_Medium =>
                  Font :=
                     Create_Toy
                     (  Family => Font.Toy_Face.Get_Family,
                        Slant  => Font.Toy_Face.Get_Slant,
                        Weight => CAIRO_FONT_WEIGHT_NORMAL
                     );
               when Pango_Weight_Semibold..Pango_Weight_Ultraheavy =>
                  Font :=
                     Create_Toy
                     (  Family => Font.Toy_Face.Get_Family,
                        Slant  => Font.Toy_Face.Get_Slant,
                        Weight => CAIRO_FONT_WEIGHT_BOLD
                     );
            end case;
         when Pango_Font =>
            Set_Weight (Font.Pango_Handle.Ptr.Description, Weight);
      end case;
   end Set_Weight;

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

   procedure Show_Markup
             (  Font    : Pango_Cairo_Font;
                Context : Cairo_Context;
                Text    : UTF8_String
             )  is
   begin
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            begin
               Set_Font (Context, Font.Toy_Face);
               Show_Text (Context, Strip_Tags (Text));
            exception
               when Error : Data_Error =>
                  Log
                  (  GtkAda_Contributions_Domain,
                     Log_Level_Warning,
                     (  "Pango markup: "
                     &  Exception_Message (Error)
                     &  Where ("Set_Font")
                  )  );
            end;
         when Pango_Font =>
            declare
               Layout : constant Pango_Layout :=
                        Create_Markup_Layout
                        (  Font.Pango_Handle,
                           Context,
                           Text
                        );
            begin
               Show_Layout (Context, Layout);
               Layout.Unref;
            end;
      end case;
   end Show_Markup;

   procedure Show_Text
             (  Font    : Pango_Cairo_Font;
                Context : Cairo_Context;
                Text    : UTF8_String
             )  is
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
                        (  Font.Pango_Handle,
                           Context,
                           Text
                        );
            begin
               Show_Layout (Context, Layout);
--             Show_Layout_Line (Context, Layout);
               Layout.Unref;
            end;
      end case;
   end Show_Text;

   function Strip_Tags (Text : UTF8_String) return UTF8_String is
      use Glib.Error;
      use System;
      use Interfaces.C;

      function Internal
               (  Markup_Text  : Address;
                  Length       : int;
                  Accel_Marker : gunichar := 0;
                  Attr_List    : Address  := Null_Address;
                  Text         : access Interfaces.C.Strings.Chars_Ptr;
                  Accel_Char   : Address  := Null_Address;
                  Error        : access GError
               )  return GBoolean;
      pragma Import (C, Internal, "pango_parse_markup");
      Result : aliased Interfaces.C.Strings.Chars_Ptr;
      Error  : aliased GError;
   begin
      if (  0
         =  Internal
            (  Markup_Text => Text (Text'First)'Address,
               Length      => Text'Length,
               Text        => Result'Access,
               Error       => Error'Access
         )  )
      then
         return Text;
      else
         return Value : constant String :=
                        Interfaces.C.Strings.Value (Result) do
            G_Free (Result);
         end return;
      end if;
   end Strip_Tags;

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Font   : Pango_Cairo_Font
             )  is
   begin
      Store (Stream, GUInt (Font_Type'Pos (Font.Mode)));
      case Font.Mode is
         when Null_Font =>
            null;
         when Toy_Font =>
            Store (Stream, Font.Toy_Face.Face);
         when Pango_Font =>
            Store
            (  Stream,
               To_String (Font.Pango_Handle.Ptr.Description)
            );
      end case;
   end Store;

end Pango.Cairo.Fonts;
