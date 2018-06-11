--                                                                    --
--  package Pango.Cairo.Fonts       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2012       --
--                                                                    --
--                                Last revision :  13:51 30 May 2014  --
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
--
--  This package provides an  abstraction  layer  above  toy  and  Pango
--  fonts, which are differently handled in Cairo.
--
with Ada.Streams;

with Pango.Enums;
with Pango.Font;

with Ada.Finalization;
with Gtkada.Types;

package Pango.Cairo.Fonts is

   pragma Warnings (Off, "declaration hides ""Font""");

   --
   -- Font_Type -- Supported font back-ends
   --
   type Font_Type is (Null_Font, Toy_Font, Pango_Font)
     with Convention => C, Size => Glib.Gint'Size;

   --
   -- Pango_Cairo_Font -- A font object (font family)
   --
   type Pango_Cairo_Font is private;

   --
   -- Create_Pango -- Create pango font face
   --
   --    Family  - The font family
   --    Style   - Font style
   --    Variant - The variant
   --    Weight  - The weight
   --    Stretch - The stretch
   --    Size    - The font size in points
   --
   -- Returns :
   --
   --    The font face object
   --
   function Create_Pango
     (Family  : String;
      Style   : Pango.Enums.Style   := Pango.Enums.Pango_Style_Normal;
      Variant : Pango.Enums.Variant := Pango.Enums.Pango_Variant_Normal;
      Weight  : Pango.Enums.Weight  := Pango.Enums.Pango_Weight_Normal;
      Stretch : Pango.Enums.Stretch := Pango.Enums.Pango_Stretch_Normal;
      Size    : Glib.Gint           := 12) return Pango_Cairo_Font;

   --
   -- Create_Pango_From_Description -- Create pango font face
   --
   --    Description - The font description as in Pango.Font.From_String
   --
   -- Returns :
   --
   --    The font face object corresponding to the description
   --
   function Create_Pango_From_Description (Description : Glib.UTF8_String)
                                           return Pango_Cairo_Font;

   --
   -- Create_Toy -- Create toy font face
   --
   --    Family - The font family
   --    Slant  - The font slant
   --    Weight - The font weight
   --
   -- Returns :
   --
   --    The font face object corresponding to the parameters
   --
   function Create_Toy
     (Family : Glib.UTF8_String;
      Slant  : Cairo_Font_Slant  := Cairo_Font_Slant_Normal;
      Weight : Cairo_Font_Weight := Cairo_Font_Weight_Normal)
      return Pango_Cairo_Font;

   --
   -- Get_Family -- Get font face family
   --
   --    Font - The font face
   --
   -- Returns :
   --
   --    The family
   --
   function Get_Family (Font : Pango_Cairo_Font) return Glib.UTF8_String;

   --
   -- Get_Markup_Extents -- Get extents of a text
   --
   --    Font    - The font face
   --    Context - The cairo context
   --    Text    - The text to measure containing Pango markup commands
   --    Extents - The result
   --
   -- This procedure calculates the extents of a Pango markup text. For the
   -- toy  font the result is equivalent to the extent of the text with all
   -- tags  stripped.  Note  that the implementation may change the context
   -- settings.
   --
   procedure Get_Markup_Extents (Font    : Pango_Cairo_Font;
                                 Context : Cairo_Context;
                                 Text    : Glib.UTF8_String;
                                 Extents : out Cairo_Text_Extents);

   --
   -- Get_Size -- Get font size (relevant for pango only)
   --
   --    Font - The font face
   --
   -- Returns :
   --
   --    The font size in points
   --
   function Get_Size (Font : Pango_Cairo_Font) return Glib.Gint;

   --
   -- Get_Slant -- Get font face slant
   --
   --    Font - The font face
   --
   -- Returns :
   --
   --    The slant
   --
   function Get_Slant (Font : Pango_Cairo_Font) return Cairo_Font_Slant;

   --
   -- Get_Text_Extents -- Get extents of a text
   --
   --    Font    - The font face
   --    Context - The cairo context
   --    Text    - The text to measure
   --    Extents - The result
   --
   -- Note that the implementation may change the context settings.
   --
   procedure Get_Text_Extents (Font    : Pango_Cairo_Font;
                               Context : Cairo_Context;
                               Text    : Glib.UTF8_String;
                               Extents : out Cairo_Text_Extents);

   --
   -- Get_Type -- Get font type
   --
   --    Font - The font face
   --
   -- Returns :
   --
   --    The type of the font
   --
   function Get_Type (Font : Pango_Cairo_Font) return Font_Type;

   --
   -- Get_Weight -- Get font face weight
   --
   --    Font - The font face
   --
   -- Returns :
   --
   --    The weight
   --
   function Get_Weight (Font : Pango_Cairo_Font)
                        return Cairo_Font_Weight;

   function Get_Weight (Font : Pango_Cairo_Font)
                        return Pango.Enums.Weight;

   --
   -- Restore -- Restore font face from stream
   --
   --    Stream - The stream to read
   --    Font   - The font face to restore
   --
   procedure Restore (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                      Font   : out Pango_Cairo_Font);

   --
   -- Set_Family -- Set font face family
   --
   --    Font   - The font face
   --    Family - To set
   --
   procedure Set_Family (Font   : in out Pango_Cairo_Font;
                         Family : Glib.UTF8_String);

   --
   -- Get_Size -- Get font size (relevant for pango only)
   --
   --    Font - The font face
   --    Size - The font size in points
   --
   procedure Set_Size (Font : in out Pango_Cairo_Font; Size : Glib.Gint);

   --
   -- Set_Slant -- Set font slant
   --
   --    Font  - The font face
   --    Slant - To set
   --
   procedure Set_Slant (Font  : in out Pango_Cairo_Font;
                        Slant : Cairo_Font_Slant);

   --
   -- Set_Type -- Change font type
   --
   --    Font - The font face
   --    Mode - The font type to switch to
   --
   -- The procedure tries to retain the font family and other settings.
   --
   procedure Set_Type (Font : in out Pango_Cairo_Font;
                       Mode : Font_Type);

   --
   -- Set_Weight -- Set font weight
   --
   --    Font   - The font face
   --    Weight - To set
   --
   procedure Set_Weight (Font   : in out Pango_Cairo_Font;
                         Weight : Cairo_Font_Weight);

   procedure Set_Weight (Font   : in out Pango_Cairo_Font;
                         Weight : Pango.Enums.Weight);

   --
   -- Show_Markup -- Render text
   --
   --    Font    - The font face
   --    Context - The cairo context containing Pango markup commands
   --    Text    - The text to render
   --
   -- For the toy font the result  is equivalent  to the extent of the text
   -- with all tags stripped.
   --
   procedure Show_Markup (Font    : Pango_Cairo_Font;
                          Context : Cairo_Context;
                          Text    : Glib.UTF8_String);

   --
   -- Show_Text -- Get extents of a text
   --
   --    Font    - The font face
   --    Context - The cairo context
   --    Text    - The text to render
   --
   procedure Show_Text (Font    : Pango_Cairo_Font;
                        Context : Cairo_Context;
                        Text    : Glib.UTF8_String);

   --
   -- Store -- Store font face into the stream
   --
   --    Stream - The stream to write
   --    Font   - The font face to store
   --
   procedure Store (Stream : in out Ada.Streams.Root_Stream_Type'Class;
                    Font   : Pango_Cairo_Font);

private

   type Pango_Font_Description_Object is
     new Ada.Finalization.Limited_Controlled with
      record
         Count       : Natural := 0;
         Description : Pango.Font.Pango_Font_Description := null;
      end record;

   type Pango_Font_Description_Object_Ptr is
     access Pango_Font_Description_Object;

   overriding procedure Finalize
     (Object : in out Pango_Font_Description_Object);

   type Pango_Font_Description_Handle is
     new Ada.Finalization.Controlled with
      record
         Ptr : Pango_Font_Description_Object_Ptr;
      end record;

   function Create_Text_Layout (Handle  : Pango_Font_Description_Handle;
                                Context : Cairo_Context;
                                Text    : Glib.UTF8_String) return Pango_Layout;

   function Create_Markup_Layout (Handle  : Pango_Font_Description_Handle;
                                  Context : Cairo_Context;
                                  Text    : Glib.UTF8_String) return Pango_Layout;

   overriding procedure Adjust (Handle : in out Pango_Font_Description_Handle);

   overriding procedure Finalize
     (Handle : in out Pango_Font_Description_Handle);

   function Ref (Description : Pango.Font.Pango_Font_Description)
                 return Pango_Font_Description_Handle;

   type Cairo_Font_Face_Handle is
     new Ada.Finalization.Controlled with
      record
         Face : Cairo_Font_Face := Null_Font_Face;
      end record;

   procedure Check (Font : Cairo_Font_Face_Handle);

   function Get_Family (Handle : Cairo_Font_Face_Handle) return Glib.UTF8_String;

   function Get_Family (Handle : Cairo_Font_Face_Handle)
                        return Gtkada.Types.Chars_Ptr;

   function Get_Slant (Handle : Cairo_Font_Face_Handle) return Cairo_Font_Slant;

   function Get_Weight (Handle : Cairo_Font_Face_Handle)
                        return Cairo_Font_Weight;

   overriding procedure Adjust (Handle : in out Cairo_Font_Face_Handle);

   overriding procedure Finalize (Handle : in out Cairo_Font_Face_Handle);

   type Pango_Cairo_Font (Mode : Font_Type := Null_Font) is
      record
         case Mode is
            when Null_Font =>
               null;
            when Toy_Font =>
               Toy_Face     : Cairo_Font_Face_Handle;
            when Pango_Font =>
               Pango_Handle : Pango_Font_Description_Handle;
         end case;
      end record;

   procedure Set_Font (Context : Cairo_Context;
                       Font    : Cairo_Font_Face_Handle);

   function Strip_Tags (Text : Glib.UTF8_String) return Glib.UTF8_String;

   pragma Warnings (On, "declaration hides ""Font""");

end Pango.Cairo.Fonts;
