--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Flat_Annotation                 Luebeck            --
--  Implementation                                 Winter, 2011       --
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

with Ada.Unchecked_Deallocation;

with Cairo.Elementary_Functions;
with Cairo.Font_Slant_Property;

with Glib.Properties.Creation;

with Gtk.Layered.Alignment_Property;
with Gtk.Layered.Stream_IO;

with Pango.Cairo.Fonts.Font_Type_Property;
with Pango.Enums.Weight_Property;

package body Gtk.Layered.Flat_Annotation is

   type Annotation_Ptr is access all Flat_Annotation_Layer;

   type Layer_Property is
     (Property_Scaled,
      Property_Texts,
      Property_Markup,
      Property_Font_Type,
      Property_Family,
      Property_Slant,
      Property_Font_Size,
      Property_Weight,
      Property_Height,
      Property_Stretch,
      Property_From_X,
      Property_From_Y,
      Property_Length,
      Property_Scale_Angle,
      Property_Text_Angle,
      Property_Justify,
      Property_Tick_Step,
      Property_Tick_First,
      Property_Tick_Skipped,
      Property_Color);

   function "+" (Value : Annotation_Text_Ptr) return UTF8_String is
   begin
      if Value = null then
         return "";
      else
         return Value.all.Buffer (1 .. Value.all.Length);
      end if;
   end "+";

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Flat_Annotation_Layer,
        Annotation_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Annotation_Text,
        Annotation_Text_Ptr);

   procedure Free is
     new Ada.Unchecked_Deallocation
       (Annotation_List,
        Annotation_List_Ptr);

   function Get_List
     (Texts  : Gtk.Enums.String_List.Glist;
      Ticks  : Tick_Parameters;
      Markup : Boolean) return Annotation_List_Ptr;

   function Get_List
     (Texts     : UTF8_String;
      Delimiter : Character;
      Ticks     : Tick_Parameters;
      Markup    : Boolean) return Annotation_List_Ptr;

   procedure Delete (List : in out Annotation_List_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Flat_Annotation_Layer
   is
      Ptr : Annotation_Ptr := new Flat_Annotation_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   function Add_Annotation_Implementation
     (Under       : not null access Layer_Location'Class;
      Texts       : Annotation_List_Ptr;
      Step        : Gdouble;
      First       : Tick_Number;
      Skipped     : Tick_Number;
      From        : Cairo.Ellipses.Cairo_Tuple;
      Length      : Gdouble;
      Scale_Angle : Gdouble;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height      : Gdouble;
      Stretch     : Gdouble;
      Color       : Gdk.Color.Gdk_Color;
      Text_Angle  : Gdouble;
      Justify     : Ada.Strings.Alignment;
      Scaled      : Boolean) return Annotation_Ptr
   is
      Ptr   : Annotation_Ptr := new Flat_Annotation_Layer;
      Layer : Flat_Annotation_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Layer.Texts  := Texts;
      Add (Ptr, Under);
      Set
        (Layer       => Layer,
         Ticks       => (Step, Get_First_Tick (First, Skipped), Skipped),
         From        => From,
         Length      => Length,
         Scale_Angle => Scale_Angle,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Color       => Color,
         Text_Angle  => Text_Angle,
         Justify     => Justify);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Annotation_Implementation;

   procedure Add_Flat_Annotation
     (Under       : not null access Layer_Location'Class;
      Texts       : Gtk.Enums.String_List.Glist;
      Step        : Gdouble;
      First       : Tick_Number                        := Tick_Number'Last;
      Skipped     : Tick_Number                        := Tick_Number'Last;
      From        : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Length      : Gdouble                            := 1.0;
      Scale_Angle : Gdouble                            := 0.0;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "arial",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height      : Gdouble                            := 12.0;
      Stretch     : Gdouble                            := 1.0;
      Color       : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble                            := 0.0;
      Justify     : Ada.Strings.Alignment              := Ada.Strings.Center;
      Markup      : Boolean                            := False;
      Scaled      : Boolean                            := False)
   is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
        Add_Annotation_Implementation
          (Under       => Under,
           Step        => Step,
           First       => First,
           Skipped     => Skipped,
           From        => From,
           Length      => Length,
           Scale_Angle => Scale_Angle,
           Face        => Face,
           Height      => Height,
           Stretch     => Stretch,
           Text_Angle  => Text_Angle,
           Justify     => Justify,
           Color       => Color,
           Scaled      => Scaled,
           Texts       =>
             Get_List
               (Texts,
                (Step, First, Skipped),
                Markup));
      pragma Unreferenced (Ptr);
   end Add_Flat_Annotation;

   procedure Add_Flat_Annotation
     (Under       : not null access Layer_Location'Class;
      Texts       : Gtk.Enums.String_Lists.Controlled_String_List;
      Step        : Gdouble;
      First       : Tick_Number                        := Tick_Number'Last;
      Skipped     : Tick_Number                        := Tick_Number'Last;
      From        : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Length      : Gdouble                            := 1.0;
      Scale_Angle : Gdouble                            := 0.0;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "arial",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height      : Gdouble                            := 12.0;
      Stretch     : Gdouble                            := 1.0;
      Color       : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble                            := 0.0;
      Justify     : Ada.Strings.Alignment              := Ada.Strings.Center;
      Markup      : Boolean                            := False;
      Scaled      : Boolean                            := False) is
   begin
      Add_Flat_Annotation
        (Under       => Under,
         Step        => Step,
         First       => First,
         Skipped     => Skipped,
         From        => From,
         Length      => Length,
         Scale_Angle => Scale_Angle,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Text_Angle  => Text_Angle,
         Justify     => Justify,
         Color       => Color,
         Scaled      => Scaled,
         Markup      => Markup,
         Texts       => Gtk.Enums.String_Lists.Get_GList (Texts));
   end Add_Flat_Annotation;

   procedure Add_Flat_Annotation
     (Under       : not null access Layer_Location'Class;
      Texts       : UTF8_String;
      Step        : Gdouble;
      First       : Tick_Number                        := Tick_Number'Last;
      Skipped     : Tick_Number                        := Tick_Number'Last;
      From        : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Length      : Gdouble                            := 1.0;
      Scale_Angle : Gdouble                            := 0.0;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "arial",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height      : Gdouble                            := 12.0;
      Stretch     : Gdouble                            := 1.0;
      Color       : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble                            := 0.0;
      Justify     : Ada.Strings.Alignment              := Ada.Strings.Center;
      Delimiter   : Character                          := ' ';
      Markup      : Boolean                            := False;
      Scaled      : Boolean                            := False)
   is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
        Add_Annotation_Implementation
          (Under       => Under,
           Step        => Step,
           First       => First,
           Skipped     => Skipped,
           From        => From,
           Length      => Length,
           Scale_Angle => Scale_Angle,
           Face        => Face,
           Height      => Height,
           Stretch     => Stretch,
           Text_Angle  => Text_Angle,
           Justify     => Justify,
           Color       => Color,
           Scaled      => Scaled,
           Texts       =>
             Get_List
               (Texts,
                Delimiter,
                (Step, First, Skipped),
                Markup));
      pragma Unreferenced (Ptr);
   end Add_Flat_Annotation;

   function Add_Flat_Annotation
     (Under       : not null access Layer_Location'Class;
      Texts       : Gtk.Enums.String_List.Glist;
      Step        : Gdouble;
      First       : Tick_Number                        := Tick_Number'Last;
      Skipped     : Tick_Number                        := Tick_Number'Last;
      From        : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Length      : Gdouble                            := 1.0;
      Scale_Angle : Gdouble                            := 0.0;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "arial",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height      : Gdouble                            := 12.0;
      Stretch     : Gdouble                            := 1.0;
      Color       : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble                            := 0.0;
      Justify     : Ada.Strings.Alignment              := Ada.Strings.Center;
      Markup      : Boolean                            := False;
      Scaled      : Boolean                            := False)
      return not null access Flat_Annotation_Layer
   is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
        Add_Annotation_Implementation
          (Under       => Under,
           Step        => Step,
           First       => First,
           Skipped     => Skipped,
           From        => From,
           Length      => Length,
           Scale_Angle => Scale_Angle,
           Face        => Face,
           Height      => Height,
           Stretch     => Stretch,
           Text_Angle  => Text_Angle,
           Justify     => Justify,
           Color       => Color,
           Scaled      => Scaled,
           Texts       =>
             Get_List
               (Texts,
                (Step, First, Skipped),
                Markup));
      return Ptr.all'Unchecked_Access;
   end Add_Flat_Annotation;

   function Add_Flat_Annotation
     (Under       : not null access Layer_Location'Class;
      Texts       : Gtk.Enums.String_Lists.Controlled_String_List;
      Step        : Gdouble;
      First       : Tick_Number                        := Tick_Number'Last;
      Skipped     : Tick_Number                        := Tick_Number'Last;
      From        : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Length      : Gdouble                            := 1.0;
      Scale_Angle : Gdouble                            := 0.0;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "arial",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height      : Gdouble                            := 12.0;
      Stretch     : Gdouble                            := 1.0;
      Color       : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble                            := 0.0;
      Justify     : Ada.Strings.Alignment              := Ada.Strings.Center;
      Markup      : Boolean                            := False;
      Scaled      : Boolean                            := False)
      return not null access Flat_Annotation_Layer is
   begin
      return
        Add_Flat_Annotation
          (Under       => Under,
           Step        => Step,
           First       => First,
           Skipped     => Skipped,
           From        => From,
           Length      => Length,
           Scale_Angle => Scale_Angle,
           Face        => Face,
           Height      => Height,
           Stretch     => Stretch,
           Text_Angle  => Text_Angle,
           Justify     => Justify,
           Color       => Color,
           Scaled      => Scaled,
           Markup      => Markup,
           Texts       => Gtk.Enums.String_Lists.Get_GList (Texts));
   end Add_Flat_Annotation;

   function Add_Flat_Annotation
     (Under       : not null access Layer_Location'Class;
      Texts       : UTF8_String;
      Step        : Gdouble;
      First       : Tick_Number                        := Tick_Number'Last;
      Skipped     : Tick_Number                        := Tick_Number'Last;
      From        : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Length      : Gdouble                            := 1.0;
      Scale_Angle : Gdouble                            := 0.0;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "arial",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height      : Gdouble                            := 12.0;
      Stretch     : Gdouble                            := 1.0;
      Color       : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Text_Angle  : Gdouble                            := 0.0;
      Justify     : Ada.Strings.Alignment              := Ada.Strings.Center;
      Delimiter   : Character                          := ' ';
      Markup      : Boolean                            := False;
      Scaled      : Boolean                            := False)
      return not null access Flat_Annotation_Layer
   is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
        Add_Annotation_Implementation
          (Under       => Under,
           Step        => Step,
           First       => First,
           Skipped     => Skipped,
           From        => From,
           Length      => Length,
           Scale_Angle => Scale_Angle,
           Face        => Face,
           Height      => Height,
           Stretch     => Stretch,
           Text_Angle  => Text_Angle,
           Justify     => Justify,
           Color       => Color,
           Scaled      => Scaled,
           Texts       =>
             Get_List
               (Texts,
                Delimiter,
                (Step, First, Skipped),
                Markup));
      return Ptr.all'Unchecked_Access;
   end Add_Flat_Annotation;

   procedure Delete (List : in out Annotation_List_Ptr) is
   begin
      if List /= null then
         for Index in List'Range loop
            Free (List.all (Index));
         end loop;
         Free (List);
      end if;
   end Delete;

   overriding procedure Draw
     (Layer   : in out Flat_Annotation_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      X_Size  : Gdouble := Cairo.Elementary_Functions.Cos (Layer.Scale_Angle);
      Y_Size  : Gdouble := Cairo.Elementary_Functions.Sin (Layer.Scale_Angle);
      Gain    : Gdouble;
      This    : Gdouble;
      From    : Cairo.Ellipses.Cairo_Tuple;
      Extents : Cairo.Cairo_Text_Extents;
      Thick   : Natural := Layer.Ticks.First;
      Length  : constant Gdouble := Layer.Length
                  + Layer.Ticks.Step * 0.05;
      State   : Cairo.Ellipses.Context_State := Cairo.Ellipses.Save (Context);
      pragma Unreferenced (State);
   begin
      Cairo.New_Path (Context);
      Cairo.Set_Source_Rgb
        (Context,
         Gdouble (Gdk.Color.Red   (Layer.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Green (Layer.Color)) / Gdouble (Guint16'Last),
         Gdouble (Gdk.Color.Blue  (Layer.Color)) / Gdouble (Guint16'Last));
      if Layer.Scaled then
         declare
            Size : constant Gdouble := Layer.Widget.all.Get_Size;
         begin
            X_Size := X_Size * Size;
            Y_Size := Y_Size * Size;
            From.X := Layer.From.X * Size + Layer.Widget.all.Get_Center.X;
            From.Y := Layer.From.Y * Size + Layer.Widget.all.Get_Center.Y;
         end;
      else
         From := Layer.From;
      end if;
      for Index in Natural'Range loop
         This := Layer.Ticks.Step * Gdouble (Index);
         exit when abs This > Length;
         if Thick = Layer.Ticks.Skipped then
            Thick := 1;
         else
            Thick := Thick + 1;
            exit when
              Layer.Texts = null or else
              Index >= Layer.Texts'Length or else
              Layer.Texts.all (Index + 1) = null or else
              Layer.Texts.all (Index + 1).all.Length = 0;
            if Layer.Texts.all (Index + 1).all.Markup then
               Pango.Cairo.Fonts.Get_Markup_Extents
                 (Layer.Face,
                  Context,
                  +Layer.Texts.all (Index + 1),
                  Extents);
            else
               Pango.Cairo.Fonts.Get_Text_Extents
                 (Layer.Face,
                  Context,
                  +Layer.Texts.all (Index + 1),
                  Extents);
            end if;
            if Extents.Height > 0.0 and then Extents.Width > 0.0 then
               Gain := Layer.Height / Extents.Height;
               if Layer.Scaled then
                  Gain := Gain * Layer.Widget.all.Get_Size;
               end if;
               declare
                  State : Cairo.Ellipses.Context_State :=
                            Cairo.Ellipses.Save (Context);
                  pragma Unreferenced (State);
               begin
                  Cairo.Translate
                    (Cr => Context,
                     Tx => From.X + This * X_Size,
                     Ty => From.Y + This * Y_Size);
                  Cairo.Rotate (Context, Layer.Text_Angle);
                  Cairo.Scale (Context, Gain * Layer.Stretch, Gain);
                  case Layer.Justify is
                     when Ada.Strings.Left =>
                        Cairo.Move_To
                          (Cr => Context,
                           X  => -Extents.X_Bearing,
                           Y  => -Extents.Y_Bearing - Extents.Height * 0.5);
                     when Ada.Strings.Right =>
                        Cairo.Move_To
                          (Cr => Context,
                           X  => -Extents.X_Bearing - Extents.Width,
                           Y  => -Extents.Y_Bearing - Extents.Height * 0.5);
                     when Ada.Strings.Center =>
                        Cairo.Move_To
                          (Cr => Context,
                           X  => -Extents.X_Bearing - Extents.Width * 0.5,
                           Y  => -Extents.Y_Bearing - Extents.Height * 0.5);
                  end case;
                  if Layer.Texts.all (Index + 1).all.Markup then
                     Pango.Cairo.Fonts.Show_Markup
                       (Layer.Face,
                        Context,
                        +Layer.Texts.all (Index + 1));
                  else
                     Pango.Cairo.Fonts.Show_Text
                       (Layer.Face,
                        Context,
                        +Layer.Texts.all (Index + 1));
                  end if;
               end;
            end if;
         end if;
      end loop;
      Layer.Updated := False;
   end Draw;

   overriding procedure Finalize (Layer : in out Flat_Annotation_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      Delete (Layer.Texts);
   end Finalize;

   function Get_Color
     (Layer : Flat_Annotation_Layer) return Gdk.Color.Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   overriding function Get_Face
     (Layer : Flat_Annotation_Layer) return Pango.Cairo.Fonts.Pango_Cairo_Font
   is
   begin
      return Layer.Face;
   end Get_Face;

   function Get_From (Layer : Flat_Annotation_Layer)
                      return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.From;
   end Get_From;

   overriding function Get_Height (Layer : Flat_Annotation_Layer)
                                   return Gdouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Justify (Layer : Flat_Annotation_Layer)
                         return Ada.Strings.Alignment is
   begin
      return Layer.Justify;
   end Get_Justify;

   function Get_Length (Layer : Flat_Annotation_Layer)
                        return Gdouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_List
     (Texts  : Gtk.Enums.String_List.Glist;
      Ticks  : Tick_Parameters;
      Markup : Boolean) return Annotation_List_Ptr
   is
      pragma Unreferenced (Ticks);
      This  : Gtk.Enums.String_List.Glist := Texts;
      Count : Natural                     := 0;

      use type Gtk.Enums.String_List.Glist;
   begin
      while This /= Gtk.Enums.String_List.Null_List loop
         Count := Count + 1;
         This  := Gtk.Enums.String_List.Next (This);
      end loop;
      declare
         Result : constant Annotation_List_Ptr :=
                    new Annotation_List (1 .. Count);
         List   : Annotation_List renames Result.all;
      begin
         This := Texts;
         for Index in List'Range loop
            declare
               Text : constant UTF8_String :=
                        Gtk.Enums.String_List.Get_Data (This);
            begin
               List (Index) :=
                 new Annotation_Text'
                   (Size   => Text'Length,
                    Markup => Markup,
                    Length => Text'Length,
                    Buffer => Text);
               This := Gtk.Enums.String_List.Next (This);
            end;
         end loop;
         return Result;
      end;
   end Get_List;

   function Get_List
     (Texts     : UTF8_String;
      Delimiter : Character;
      Ticks     : Tick_Parameters;
      Markup    : Boolean) return Annotation_List_Ptr
   is
      pragma Unreferenced (Ticks);
      Count : Natural := 1;
   begin
      for Index in Texts'Range loop
         if Texts (Index) = Delimiter then
            Count := Count + 1;
         end if;
      end loop;
      declare
         Result : constant Annotation_List_Ptr :=
                    new Annotation_List (1 .. Count);
         List   : Annotation_List renames Result.all;
         Start  : Integer := Texts'First;
         Stop   : Integer;
      begin
         for Index in List'Range loop
            Stop := Start;
            while Stop <= Texts'Last and then Texts (Stop) /= Delimiter
            loop
               Stop := Stop + 1;
            end loop;
            List (Index) :=
              new Annotation_Text'
                (Size   => Stop - Start,
                 Length => Stop - Start,
                 Markup => Markup,
                 Buffer => Texts (Start .. Stop - 1));
            Start := Stop + 1;
         end loop;
         return Result;
      end;
   end Get_List;

   overriding function Get_Markup
     (Layer    : Flat_Annotation_Layer;
      Position : Positive) return Boolean is
   begin
      if Layer.Texts = null or else Position > Layer.Texts'Last then
         raise Constraint_Error with "No such text";
      elsif Layer.Texts.all (Position) = null then
         return False;
      else
         return Layer.Texts.all (Position).all.Markup;
      end if;
   end Get_Markup;

   overriding function Get_Properties_Number
     (Layer : Flat_Annotation_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         -  Layer_Property'Pos (Layer_Property'First)
         +  1);
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Flat_Annotation_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Font_Type =>
               return
                 Pango.Cairo.Fonts.Font_Type_Property.Gnew_Enum
                   (Name    => "font-type",
                    Nick    => "font type",
                    Default => Pango.Cairo.Fonts.Pango_Font,
                    Blurb   =>
                       "The backend used for the font, " &
                       "e.g. toy font, pango font");
            when Property_From_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x0",
                    Nick    => "x0",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The x-coordinate of the point " &
                       "corresponding to the location of " &
                       "the first annotation text");
            when Property_From_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y0",
                    Nick    => "y0",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   =>
                       "The y-coordinate of the point " &
                       "corresponding to the location of " &
                       "the first annotation text");
            when Property_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "length",
                    Nick    => "length",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The annotation length");
            when Property_Scale_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "annotation-angle",
                    Nick    => "annotation line angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   =>
                       "The angle of the line where " &
                       "annotation texts are located");
            when Property_Text_Angle =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "text-angle",
                    Nick    => "text angle",
                    Minimum => -2.0 * Ada.Numerics.Pi,
                    Maximum => 2.0 * Ada.Numerics.Pi,
                    Default => 0.0,
                    Blurb   => "The angle of the annotation texts base line");
            when Property_Tick_Step =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "step",
                    Nick    => "step",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The distance between two " &
                       "consequent annotation texts");
            when Property_Tick_First =>
               return
                 Glib.Properties.Creation.Gnew_Uint
                   (Name    => "first-tick",
                    Nick    => "first tick",
                    Minimum => Guint (Tick_Number'First),
                    Maximum => Guint (Tick_Number'Last),
                    Default => 1,
                    Blurb   => "The number of the first tick. " &
                      "The first tick is located at " &
                      "the beginning of the scale to which " &
                      "annotation texts are attached");
            when Property_Tick_Skipped =>
               return
                 Glib.Properties.Creation.Gnew_Uint
                   (Name    => "skipped-tick",
                    Nick    => "skipped tick",
                    Minimum => 2,
                    Maximum => Guint (Tick_Number'Last),
                    Default => Guint (Tick_Number'Last),
                    Blurb   =>
                       "The number of the skipped tick. " &
                       "The ticks are numbered from 1 to " &
                       "skipped-tick. For the ticks with " &
                       "this number annotations are not drawn");
            when Property_Stretch =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "stretch",
                    Nick    => "stretch",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 1.0,
                    Blurb   =>
                       "The relation of the rendered width " &
                       "of an annotation text to its " &
                       "original width. The stretch value " &
                       "1 keeps texts unchanged");
            when Property_Height =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "height",
                    Nick    => "height",
                    Minimum => 0.0,
                    Maximum => Gdouble'Last,
                    Default => 12.0,
                    Blurb   => "The annotation text font height");
            when Property_Family =>
               return
                 Glib.Properties.Creation.Gnew_String
                   (Name    => "font-familiy",
                    Nick    => "font famility",
                    Default => "arial",
                    Blurb   =>
                       "The annotation text font family, " &
                       "e.g. courier");
            when Property_Justify =>
               return
                 Gtk.Layered.Alignment_Property.Gnew_Enum
                   (Name    => "text-alignment",
                    Nick    => "text alignment",
                    Default => Ada.Strings.Center,
                    Blurb   =>
                       "The text alignment " &
                       "relatively to the annotation " &
                       "line");
            when Property_Slant =>
               return
                 Cairo.Font_Slant_Property.Gnew_Enum
                   (Name    => "font-slant",
                    Nick    => "font slant",
                    Default => Cairo.Cairo_Font_Slant_Normal,
                    Blurb   => "The annotation text font slant");
            when Property_Font_Size =>
               return
                 Glib.Properties.Creation.Gnew_Uint
                   (Name    => "font-size",
                    Nick    => "font size",
                    Minimum => 1,
                    Maximum => Guint (Gint'Last),
                    Default => 12,
                    Blurb   =>
                       "The font size in points. " &
                       "The value is only relevant for " &
                       "pango fonts. For cairo toy size " &
                       "is ignored");
            when Property_Texts =>
               return
                 Glib.Properties.Creation.Gnew_String
                   (Name    => "texts",
                    Nick    => "annotation texts",
                    Default => "",
                    Blurb   =>
                       "The list of annotation texts, " &
                       "separated by LFs");
            when Property_Markup =>
               return
                 Glib.Properties.Creation.Gnew_String
                   (Name    => "markup-flags",
                    Nick    => "annotation text markups",
                    Default => "",
                    Blurb   =>
                       "The list of annotation markup " &
                       "text flags. " &
                       "For each text it contains one " &
                       "character, which is " &
                       "T for plain text or " &
                       "M for markup");
            when Property_Weight =>
               return
                 Pango.Enums.Weight_Property.Gnew_Enum
                   (Name    => "font-weight",
                    Nick    => "font weight",
                    Default => Pango.Enums.Pango_Weight_Normal,
                    Blurb   => "The annotation text font weight");
            when Property_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "color",
                    Blurb      => "The annotation texts color");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   =>
                       "The annotation size is changed when " &
                       "the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Flat_Annotation_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Font_Type =>
                  Pango.Cairo.Fonts.Font_Type_Property.Set_Enum
                    (Value,
                     Pango.Cairo.Fonts.Get_Type (Layer.Face));
               when Property_From_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From.X);
               when Property_From_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From.Y);
               when Property_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Length);
               when Property_Scale_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Scale_Angle);
               when Property_Text_Angle =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Text_Angle);
               when Property_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Color);
               when Property_Tick_Step =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Ticks.Step);
               when Property_Tick_First =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint (Value, Guint (Layer.Ticks.First));
               when Property_Tick_Skipped =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint (Value, Guint (Layer.Ticks.Skipped));
               when Property_Height =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Height);
               when Property_Family =>
                  Glib.Values.Init (Value, GType_String);
                  Glib.Values.Set_String
                    (Value,
                     Pango.Cairo.Fonts.Get_Family (Layer.Face));
               when Property_Justify =>
                  Gtk.Layered.Alignment_Property.Set_Enum
                    (Value,
                     Layer.Justify);
               when Property_Slant =>
                  Cairo.Font_Slant_Property.Set_Enum
                    (Value,
                     Pango.Cairo.Fonts.Get_Slant (Layer.Face));
               when Property_Font_Size =>
                  Glib.Values.Init (Value, GType_Uint);
                  Glib.Values.Set_Uint
                    (Value,
                     Guint (Pango.Cairo.Fonts.Get_Size (Layer.Face)));
               when Property_Stretch =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Stretch);
               when Property_Texts =>
                  Glib.Values.Init (Value, GType_String);
                  if Layer.Texts = null then
                     Glib.Values.Set_String (Value, "");
                  else
                     declare
                        Length : Natural := 0;
                        List   : Annotation_List renames
                                   Layer.Texts.all;
                     begin
                        for Index in List'Range loop
                           if Index > List'First then
                              Length := Length + 1;
                           end if;
                           Length := Length + List (Index).all.Length;
                        end loop;
                        declare
                           Text    : String (1 .. Length);
                           Pointer : Integer := Text'First;
                        begin
                           for Index in List'Range loop
                              if Index > List'First then
                                 Text (Pointer) := Character'Val (10);
                                 Pointer := Pointer + 1;
                              end if;
                              Text
                                (Pointer ..
                                   Pointer + List (Index).all.Length - 1) :=
                                  +List (Index);
                              Pointer := Pointer + List (Index).all.Length;
                           end loop;
                           Glib.Values.Set_String (Value, Text);
                        end;
                     end;
                  end if;
               when Property_Markup =>
                  Glib.Values.Init (Value, GType_String);
                  if Layer.Texts = null then
                     Glib.Values.Set_String (Value, "");
                  else
                     declare
                        List : Annotation_List renames Layer.Texts.all;
                        Text : String (List'Range);
                     begin
                        for Index in Text'Range loop
                           if List (Index).all.Markup then
                              Text (Index) := 'M';
                           else
                              Text (Index) := 'T';
                           end if;
                        end loop;
                        Glib.Values.Set_String (Value, Text);
                     end;
                  end if;
               when Property_Scaled =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Scaled);
               when Property_Weight =>
                  Pango.Enums.Weight_Property.Set_Enum
                    (Value,
                     Pango.Cairo.Fonts.Get_Weight (Layer.Face));
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scale_Angle (Layer : Flat_Annotation_Layer)
                             return Gdouble is
   begin
      return Layer.Scale_Angle;
   end Get_Scale_Angle;

   overriding function Get_Scaled (Layer : Flat_Annotation_Layer)
                                   return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   overriding function Get_Stretch (Layer : Flat_Annotation_Layer)
                                    return Gdouble is
   begin
      return Layer.Stretch;
   end Get_Stretch;

   overriding function Get_Text
     (Layer    : Flat_Annotation_Layer;
      Position : Positive) return UTF8_String is
   begin
      if Layer.Texts = null or else Position > Layer.Texts'Last then
         raise Constraint_Error with "No such text";
      elsif Layer.Texts.all (Position) = null then
         return "";
      else
         return +Layer.Texts.all (Position);
      end if;
   end Get_Text;

   function Get_Text_Angle (Layer : Flat_Annotation_Layer)
                            return Gdouble is
   begin
      return Layer.Text_Angle;
   end Get_Text_Angle;

   overriding function Get_Texts_Number (Layer : Flat_Annotation_Layer)
                                         return Natural is
   begin
      if Layer.Texts = null then
         return 0;
      else
         return Layer.Texts'Length;
      end if;
   end Get_Texts_Number;

   overriding function Get_Ticks (Layer : Flat_Annotation_Layer)
                                  return Tick_Parameters is
   begin
      return Layer.Ticks;
   end Get_Ticks;

   overriding function Is_Updated (Layer : Flat_Annotation_Layer)
                                   return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding procedure Move
     (Layer  : in out Flat_Annotation_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.From.X  := Layer.From.X + Offset.X;
      Layer.From.Y  := Layer.From.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Flat_Annotation_Layer)
   is
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height      : Gdouble;
      Stretch     : Gdouble;
      From        : Cairo.Ellipses.Cairo_Tuple;
      Length      : Gdouble;
      Scale_Angle : Gdouble;
      Text_Angle  : Gdouble;
      Ticks       : Tick_Parameters;
      Color       : Gdk.Color.Gdk_Color;
      Justify     : Ada.Strings.Alignment;
   begin
      Pango.Cairo.Fonts.Restore (Stream, Face);
      Gtk.Layered.Stream_IO.Restore (Stream, Height);
      Gtk.Layered.Stream_IO.Restore (Stream, Stretch);
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, Length);
      Gtk.Layered.Stream_IO.Restore (Stream, Scale_Angle);
      Gtk.Layered.Stream_IO.Restore (Stream, Text_Angle);
      Gtk.Layered.Stream_IO.Restore (Stream, Ticks);
      Gtk.Layered.Stream_IO.Restore (Stream, Color);
      Gtk.Layered.Stream_IO.Restore (Stream, Justify);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled);
      Set
        (Layer       => Layer,
         Ticks       => Ticks,
         From        => From,
         Length      => Length,
         Scale_Angle => Scale_Angle,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Color       => Color,
         Text_Angle  => Text_Angle,
         Justify     => Justify);
      declare
         Markup : constant Gtk.Layered.Stream_IO.Bit_Array :=
                    Gtk.Layered.Stream_IO.Restore (Stream'Access);
      begin
         Free (Layer.Texts);
         Layer.Texts := new Annotation_List (Markup'Range);
         for Index in Markup'Range loop
            Layer.Set_Text
              (Index,
               Gtk.Layered.Stream_IO.Restore (Stream'Access),
               Markup (Index));
         end loop;
      end;
   end Restore;

   overriding procedure Scale
     (Layer  : in out Flat_Annotation_Layer;
      Factor : Gdouble)
   is
      Ticks  : Tick_Parameters := Layer.Ticks;
      Height : constant Gdouble := Layer.Height * Factor;
   begin
      Ticks.Step := Ticks.Step * Factor;
      if Ticks.Step < Min_Step then
         raise Constraint_Error with "Step is too small";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      end if;
      Layer.Length  := Layer.Length * Factor;
      Layer.Ticks   := Ticks;
      Layer.Height  := Height;
      Layer.Updated := True;
   end Scale;

   procedure Set
     (Layer       : in out Flat_Annotation_Layer;
      Ticks       : Tick_Parameters;
      From        : Cairo.Ellipses.Cairo_Tuple;
      Length      : Gdouble;
      Scale_Angle : Gdouble;
      Face        : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height      : Gdouble;
      Stretch     : Gdouble;
      Color       : Gdk.Color.Gdk_Color;
      Text_Angle  : Gdouble;
      Justify     : Ada.Strings.Alignment) is
   begin
      if Ticks.Step < Min_Step then
         raise Constraint_Error with "Step is too small";
      elsif Ticks.First > Ticks.Skipped then
         raise Constraint_Error with
           "First tick is greater than the skipped tick";
      elsif Height <= 0.0 then
         raise Constraint_Error with "Non-positive height";
      elsif Stretch <= 0.0 then
         raise Constraint_Error with "Non-positive stretch";
      end if;
      Layer.Ticks       := Ticks;
      Layer.From        := From;
      Layer.Length      := Length;
      Layer.Scale_Angle := Scale_Angle;
      Layer.Face        := Face;
      Layer.Height      := Height;
      Layer.Stretch     := Stretch;
      Layer.Color       := Color;
      Layer.Text_Angle  := Text_Angle;
      Layer.Justify     := Justify;
      Layer.Updated     := True;
   end Set;

   overriding procedure Set_Face
     (Layer : in out Flat_Annotation_Layer;
      Face  : Pango.Cairo.Fonts.Pango_Cairo_Font) is
   begin
      Layer.Face    := Face;
      Layer.Updated := True;
   end Set_Face;

   overriding procedure Set_Property_Value
     (Layer    : in out Flat_Annotation_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Font_Type =>
               Pango.Cairo.Fonts.Set_Type
                 (Layer.Face,
                  Pango.Cairo.Fonts.Font_Type_Property.Get_Enum (Value));
            when Property_From_X =>
               Layer.From.X := Glib.Values.Get_Double (Value);
            when Property_From_Y =>
               Layer.From.Y := Glib.Values.Get_Double (Value);
            when Property_Length =>
               Layer.Length := Glib.Values.Get_Double (Value);
            when Property_Color =>
               Layer.Color := Gdk.Color.Get_Value (Value);
            when Property_Stretch =>
               Layer.Stretch := Glib.Values.Get_Double (Value);
               if Layer.Stretch < 0.0 then
                  Layer.Stretch := 0.0;
               end if;
            when Property_Scale_Angle =>
               Layer.Scale_Angle := Glib.Values.Get_Double (Value);
               if
                 Layer.Scale_Angle not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Scale_Angle :=
                    Gdouble'Remainder (Layer.Scale_Angle,
                                       2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Text_Angle =>
               Layer.Text_Angle := Glib.Values.Get_Double (Value);
               if
                 Layer.Text_Angle not in
                   -2.0 * Ada.Numerics.Pi .. 2.0 * Ada.Numerics.Pi
               then
                  Layer.Text_Angle :=
                    Gdouble'Remainder (Layer.Text_Angle, 2.0 * Ada.Numerics.Pi);
               end if;
            when Property_Height =>
               Layer.Height := Glib.Values.Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Justify =>
               Layer.Justify :=
                 Gtk.Layered.Alignment_Property.Get_Enum (Value);
            when Property_Tick_Step =>
               Layer.Ticks.Step := Glib.Values.Get_Double (Value);
               if Layer.Ticks.Step < 1.0E-6 then
                  Layer.Ticks.Step := 1.0E-6;
               end if;
            when Property_Tick_First =>
               if Glib.Values.Get_Uint (Value) < 1 then
                  Layer.Ticks.First := 1;
               elsif Glib.Values.Get_Uint (Value) > Guint (Tick_Number'Last) then
                  Layer.Ticks.First := Tick_Number'Last;
               else
                  Layer.Ticks.First := Tick_Number (Glib.Values.Get_Uint (Value));
               end if;
            when Property_Tick_Skipped =>
               if Glib.Values.Get_Uint (Value) < 2 then
                  Layer.Ticks.Skipped := 2;
               elsif Glib.Values.Get_Uint (Value) > Guint (Tick_Number'Last) then
                  Layer.Ticks.Skipped := Tick_Number'Last;
               else
                  Layer.Ticks.Skipped := Tick_Number (Glib.Values.Get_Uint (Value));
               end if;
            when Property_Family =>
               Pango.Cairo.Fonts.Set_Family (Layer.Face, Glib.Values.Get_String (Value));
            when Property_Slant =>
               Pango.Cairo.Fonts.Set_Slant
                 (Layer.Face,
                  Cairo.Font_Slant_Property.Get_Enum (Value));
            when Property_Font_Size =>
               Pango.Cairo.Fonts.Set_Size
                 (Layer.Face,
                  Gint
                    (Guint'Max
                         (Guint'Min
                              (Glib.Values.Get_Uint (Value),
                               Guint (Gint'Last)),
                          1)));
            when Property_Texts =>
               Set_Texts
                 (Layer,
                  Glib.Values.Get_String (Value),
                  Character'Val (10),
                  False);
            when Property_Markup =>
               declare
                  Markup : constant String := Glib.Values.Get_String (Value);
               begin
                  if Layer.Texts /= null then
                     for Index in Markup'Range loop
                        exit when Index not in Layer.Texts'Range;
                        Layer.Texts.all (Index).all.Markup :=
                          Markup (Index) = 'M';
                     end loop;
                  end if;
               end;
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
            when Property_Weight =>
               Pango.Cairo.Fonts.Set_Weight
                 (Layer.Face,
                  Pango.Enums.Weight_Property.Get_Enum (Value));
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding procedure Set_Scaled
     (Layer  : in out Flat_Annotation_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Text
     (Layer    : in out Flat_Annotation_Layer;
      Position : Positive;
      Text     : UTF8_String;
      Markup   : Boolean := False)  is
   begin
      if Layer.Texts = null then
         if Position = 1 then
            Layer.Texts :=
              new Annotation_List'
                (1 .. 1 =>
                    new Annotation_Text'
                     (Size   => Text'Length,
                      Length => Text'Length,
                      Markup => Markup,
                      Buffer => Text));
            Layer.Updated := True;
            return;
         end if;
      elsif Position > Layer.Texts'Last then
         if Position = Layer.Texts'Last + 1 then
            declare
               Old_Texts : Annotation_List_Ptr := Layer.Texts;
            begin
               Layer.Texts := new Annotation_List (1 .. Position);
               Layer.Texts.all (Old_Texts'Range) := Old_Texts.all;
               Layer.Texts.all (Position) :=
                 new Annotation_Text'
                   (Size   => Text'Length,
                    Length => Text'Length,
                    Markup => Markup,
                    Buffer => Text);
               Free (Old_Texts);
               Layer.Updated := True;
               return;
            end;
         end if;
      else
         if
           Layer.Texts.all (Position) /= null and then
           Layer.Texts.all (Position).all.Size >= Text'Length
         then
            declare
               This : Annotation_Text renames
                        Layer.Texts.all (Position).all;
            begin
               This.Buffer (1 .. Text'Length) := Text;
               This.Length := Text'Length;
               This.Markup := Markup;
            end;
         else
            Free (Layer.Texts.all (Position));
            Layer.Texts.all (Position) :=
              new Annotation_Text'
                (Size   => Text'Length,
                 Length => Text'Length,
                 Markup => Markup,
                 Buffer => Text);
         end if;
         Layer.Updated := True;
         return;
      end if;
      raise Constraint_Error with "No such text";
   end Set_Text;

   overriding procedure Set_Texts
     (Layer  : in out Flat_Annotation_Layer;
      Texts  : Gtk.Enums.String_List.Glist;
      Markup : Boolean := False)
   is
      List : Annotation_List_Ptr :=
               Get_List (Texts, Layer.Ticks, Markup);
   begin
      Delete (Layer.Texts);
      Layer.Texts   := List;
      Layer.Updated := True;
   exception
      when others =>
         Delete (List);
   end Set_Texts;

   overriding procedure Set_Texts
     (Layer     : in out Flat_Annotation_Layer;
      Texts     : UTF8_String;
      Delimiter : Character := ' ';
      Markup    : Boolean := False)
   is
      List : Annotation_List_Ptr :=
               Get_List (Texts, Delimiter, Layer.Ticks, Markup);
   begin
      Delete (Layer.Texts);
      Layer.Texts   := List;
      Layer.Updated := True;
   exception
      when others =>
         Delete (List);
   end Set_Texts;

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Flat_Annotation_Layer) is
   begin
      Pango.Cairo.Fonts.Store (Stream, Layer.Face);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Height);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Stretch);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.From);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Length);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Scale_Angle);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Text_Angle);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Ticks);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Color);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Justify);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Scaled);
      declare
         Markup : Gtk.Layered.Stream_IO.Bit_Array (1 .. Layer.Get_Texts_Number);
      begin
         for Index in 1 .. Layer.Get_Texts_Number loop
            Markup (Index) := Layer.Get_Markup (Index);
         end loop;
         Gtk.Layered.Stream_IO.Store (Stream, Markup);
      end;
      for Index in 1 .. Layer.Get_Texts_Number loop
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Get_Text (Index));
      end loop;
   end Store;

end Gtk.Layered.Flat_Annotation;
