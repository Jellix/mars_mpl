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
--____________________________________________________________________--

with Cairo.Elementary_Functions;  use Cairo.Elementary_Functions;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with Gtk.Layered.Label;           use Gtk.Layered.Label;
with Gtk.Layered.Stream_IO;       use Gtk.Layered.Stream_IO;

with Ada.Unchecked_Deallocation;
with Cairo.Font_Slant_Property;
with Pango.Cairo.Fonts.Font_Type_Property;
with Pango.Enums.Weight_Property;
with Gtk.Layered.Alignment_Property;

package body Gtk.Layered.Flat_Annotation is
   type Annotation_Ptr is access all Flat_Annotation_Layer;

   type Layer_Property is
        (  Property_Scaled,
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
           Property_Color
        );

   function "+" (Value : Annotation_Text_Ptr) return UTF8_String is
   begin
      if Value = null then
         return "";
      else
         return Value.Buffer (1..Value.Length);
      end if;
   end "+";

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Flat_Annotation." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Flat_Annotation_Layer,
             Annotation_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Annotation_Text,
             Annotation_Text_Ptr
          );
   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Annotation_List,
             Annotation_List_Ptr
          );

   function Get_List
            (  Texts  : Gtk.Enums.String_List.GList;
               Ticks  : Tick_Parameters;
               Markup : Boolean
            )  return Annotation_List_Ptr;

   function Get_List
            (  Texts     : UTF8_String;
               Delimiter : Character;
               Ticks     : Tick_Parameters;
               Markup    : Boolean
            )  return Annotation_List_Ptr;

   procedure Delete (List : in out Annotation_List_Ptr);

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Flat_Annotation_Layer is
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
            (  Under       : not null access Layer_Location'Class;
               Texts       : Annotation_List_Ptr;
               Step        : GDouble;
               First       : Tick_Number;
               Skipped     : Tick_Number;
               From        : Cairo_Tuple;
               Length      : GDouble;
               Scale_Angle : GDouble;
               Face        : Pango_Cairo_Font;
               Height      : GDouble;
               Stretch     : GDouble;
               Color       : Gdk_Color;
               Text_Angle  : GDouble;
               Justify     : Alignment;
               Scaled      : Boolean
            )  return Annotation_Ptr is
      Ptr   : Annotation_Ptr := new Flat_Annotation_Layer;
      Layer : Flat_Annotation_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Layer.Texts  := Texts;
      Add (Ptr, Under);
      Set
      (  Layer  => Layer,
         Ticks  => (Step, Get_First_Tick (First, Skipped), Skipped),
         From   => From,
         Length => Length,
         Scale_Angle => Scale_Angle,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Color       => Color,
         Text_Angle  => Text_Angle,
         Justify     => Justify
      );
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Annotation_Implementation;

   procedure Add_Flat_Annotation
             (  Under       : not null access Layer_Location'Class;
                Texts       : Gtk.Enums.String_List.GList;
                Step        : GDouble;
                First       : Tick_Number := Tick_Number'Last;
                Skipped     : Tick_Number := Tick_Number'Last;
                From        : Cairo_Tuple := (0.0, 0.0);
                Length      : GDouble     := 1.0;
                Scale_Angle : GDouble     := 0.0;
                Face        : Pango_Cairo_Font :=
                                 Create_Toy
                                 (  Family => "arial",
                                    Slant  => CAIRO_FONT_SLANT_NORMAL,
                                    Weight => CAIRO_FONT_WEIGHT_NORMAL
                                 );
                Height      : GDouble     := 12.0;
                Stretch     : GDouble     := 1.0;
                Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble     := 0.0;
                Justify     : Alignment   := Center;
                Markup      : Boolean     := False;
                Scaled      : Boolean     := False
             )  is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
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
            Texts       => Get_List
                           (  Texts,
                              (Step, First, Skipped),
                              Markup
         )                 );
   end Add_Flat_Annotation;

   procedure Add_Flat_Annotation
             (  Under       : not null access Layer_Location'Class;
                Texts       : Controlled_String_List;
                Step        : GDouble;
                First       : Tick_Number := Tick_Number'Last;
                Skipped     : Tick_Number := Tick_Number'Last;
                From        : Cairo_Tuple := (0.0, 0.0);
                Length      : GDouble     := 1.0;
                Scale_Angle : GDouble     := 0.0;
                Face        : Pango_Cairo_Font :=
                                 Create_Toy
                                 (  Family => "arial",
                                    Slant  => CAIRO_FONT_SLANT_NORMAL,
                                    Weight => CAIRO_FONT_WEIGHT_NORMAL
                                 );
                Height      : GDouble     := 12.0;
                Stretch     : GDouble     := 1.0;
                Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble     := 0.0;
                Justify     : Alignment   := Center;
                Markup      : Boolean     := False;
                Scaled      : Boolean     := False
             )  is
   begin
      Add_Flat_Annotation
         (  Under       => Under,
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
            Texts       => Get_GList (Texts)
         );
   end Add_Flat_Annotation;

   procedure Add_Flat_Annotation
             (  Under       : not null access Layer_Location'Class;
                Texts       : UTF8_String;
                Step        : GDouble;
                First       : Tick_Number := Tick_Number'Last;
                Skipped     : Tick_Number := Tick_Number'Last;
                From        : Cairo_Tuple := (0.0, 0.0);
                Length      : GDouble     := 1.0;
                Scale_Angle : GDouble     := 0.0;
                Face        : Pango_Cairo_Font :=
                                 Create_Toy
                                 (  Family => "arial",
                                    Slant  => CAIRO_FONT_SLANT_NORMAL,
                                    Weight => CAIRO_FONT_WEIGHT_NORMAL
                                 );
                Height      : GDouble     := 12.0;
                Stretch     : GDouble     := 1.0;
                Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
                Text_Angle  : GDouble     := 0.0;
                Justify     : Alignment   := Center;
                Delimiter   : Character   := ' ';
                Markup      : Boolean     := False;
                Scaled      : Boolean     := False
             )  is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
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
            Texts       => Get_List
                           (  Texts,
                              Delimiter,
                              (Step, First, Skipped),
                              Markup
         )                 );
   end Add_Flat_Annotation;

   function Add_Flat_Annotation
            (  Under       : not null access Layer_Location'Class;
               Texts       : Gtk.Enums.String_List.GList;
               Step        : GDouble;
               First       : Tick_Number := Tick_Number'Last;
               Skipped     : Tick_Number := Tick_Number'Last;
               From        : Cairo_Tuple := (0.0, 0.0);
               Length      : GDouble     := 1.0;
               Scale_Angle : GDouble     := 0.0;
               Face        : Pango_Cairo_Font :=
                                Create_Toy
                                (  Family => "arial",
                                   Slant  => CAIRO_FONT_SLANT_NORMAL,
                                   Weight => CAIRO_FONT_WEIGHT_NORMAL
                                );
               Height      : GDouble     := 12.0;
               Stretch     : GDouble     := 1.0;
               Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble     := 0.0;
               Justify     : Alignment   := Center;
               Markup      : Boolean     := False;
               Scaled      : Boolean     := False
            )  return not null access Flat_Annotation_Layer is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
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
            Texts       => Get_List
                           (  Texts,
                              (Step, First, Skipped),
                              Markup
         )                 );
      return Ptr.all'Unchecked_Access;
   end Add_Flat_Annotation;

   function Add_Flat_Annotation
            (  Under       : not null access Layer_Location'Class;
               Texts       : Controlled_String_List;
               Step        : GDouble;
               First       : Tick_Number := Tick_Number'Last;
               Skipped     : Tick_Number := Tick_Number'Last;
               From        : Cairo_Tuple := (0.0, 0.0);
               Length      : GDouble     := 1.0;
               Scale_Angle : GDouble     := 0.0;
               Face        : Pango_Cairo_Font :=
                                Create_Toy
                                (  Family => "arial",
                                   Slant  => CAIRO_FONT_SLANT_NORMAL,
                                   Weight => CAIRO_FONT_WEIGHT_NORMAL
                                );
               Height      : GDouble     := 12.0;
               Stretch     : GDouble     := 1.0;
               Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble     := 0.0;
               Justify     : Alignment   := Center;
               Markup      : Boolean     := False;
               Scaled      : Boolean     := False
            )  return not null access Flat_Annotation_Layer is
   begin
      return
         Add_Flat_Annotation
         (  Under       => Under,
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
            Texts       => Get_GList (Texts)
         );
   end Add_Flat_Annotation;

   function Add_Flat_Annotation
            (  Under       : not null access Layer_Location'Class;
               Texts       : UTF8_String;
               Step        : GDouble;
               First       : Tick_Number := Tick_Number'Last;
               Skipped     : Tick_Number := Tick_Number'Last;
               From        : Cairo_Tuple := (0.0, 0.0);
               Length      : GDouble     := 1.0;
               Scale_Angle : GDouble     := 0.0;
               Face        : Pango_Cairo_Font :=
                                Create_Toy
                                (  Family => "arial",
                                   Slant  => CAIRO_FONT_SLANT_NORMAL,
                                   Weight => CAIRO_FONT_WEIGHT_NORMAL
                                );
               Height      : GDouble     := 12.0;
               Stretch     : GDouble     := 1.0;
               Color       : Gdk_Color   := RGB (0.0, 0.0, 0.0);
               Text_Angle  : GDouble     := 0.0;
               Justify     : Alignment   := Center;
               Delimiter   : Character   := ' ';
               Markup      : Boolean     := False;
               Scaled      : Boolean     := False
            )  return not null access Flat_Annotation_Layer is
      Ptr : Annotation_Ptr;
   begin
      Ptr :=
         Add_Annotation_Implementation
         (  Under       => Under,
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
            Texts       => Get_List
                           (  Texts,
                              Delimiter,
                              (Step, First, Skipped),
                              Markup
         )                 );
      return Ptr.all'Unchecked_Access;
   end Add_Flat_Annotation;

   procedure Delete (List : in out Annotation_List_Ptr) is
   begin
      if List /= null then
         for Index in List'Range loop
            Free (List (Index));
         end loop;
         Free (List);
      end if;
   end Delete;

   procedure Draw
             (  Layer   : in out Flat_Annotation_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      X_Size  : GDouble := cos (Layer.Scale_Angle);
      Y_Size  : GDouble := sin (Layer.Scale_Angle);
      Gain    : GDouble;
      This    : GDouble;
      From    : Cairo_Tuple;
      Extents : Cairo_Text_Extents;
      Thick   : Natural := Layer.Ticks.First;
      Length  : constant GDouble := Layer.Length
                                  + Layer.Ticks.Step * 0.05;
      State   : Context_State := Save (Context);
   begin
      New_Path (Context);
      Set_Source_RGB
      (  Context,
         GDouble (Red   (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Green (Layer.Color)) / GDouble (Guint16'Last),
         GDouble (Blue  (Layer.Color)) / GDouble (Guint16'Last)
      );
      if Layer.Scaled then
         declare
            Size : constant GDouble := Layer.Widget.Get_Size;
         begin
            X_Size := X_Size * Size;
            Y_Size := Y_Size * Size;
            From.X := Layer.From.X * Size + Layer.Widget.Get_Center.X;
            From.Y := Layer.From.Y * Size + Layer.Widget.Get_Center.Y;
         end;
      else
         From := Layer.From;
      end if;
      for Index in Natural'Range loop
         This := Layer.Ticks.Step * GDouble (Index);
         exit when abs This > Length;
         if Thick = Layer.Ticks.Skipped then
            Thick := 1;
         else
            Thick := Thick + 1;
            exit when
                 (  Layer.Texts = null
                 or else
                    Index >= Layer.Texts'Length
                 or else
                    Layer.Texts (Index + 1) = null
                 or else
                    Layer.Texts (Index + 1).Length = 0
                 );
             if Layer.Texts (Index + 1).Markup then
                Get_Markup_Extents
                (  Layer.Face,
                   Context,
                  +Layer.Texts (Index + 1),
                   Extents
                );
             else
                Get_Text_Extents
                (  Layer.Face,
                   Context,
                  +Layer.Texts (Index + 1),
                   Extents
                );
             end if;
            if Extents.Height > 0.0 and then Extents.Width > 0.0 then
               Gain := Layer.Height / Extents.Height;
               if Layer.Scaled then
                  Gain := Gain * Layer.Widget.Get_Size;
               end if;
               declare
                  State : Context_State := Save (Context);
               begin
                  Translate
                  (  Cr => Context,
                     Tx => From.X + This * X_Size,
                     Ty => From.Y + This * Y_Size
                  );
                  Rotate (Context, Layer.Text_Angle);
                  Scale (Context, Gain * Layer.Stretch, Gain);
                  case Layer.Justify is
                     when Left =>
                        Move_To
                        (  Cr => Context,
                           X  => -Extents.X_Bearing,
                           Y  => -Extents.Y_Bearing - Extents.Height*0.5
                        );
                     when Right =>
                        Move_To
                        (  Cr => Context,
                           X  => -Extents.X_Bearing - Extents.Width,
                           Y  => -Extents.Y_Bearing - Extents.Height*0.5
                        );
                     when Center =>
                        Move_To
                        (  Cr => Context,
                           X  => -Extents.X_Bearing - Extents.Width*0.5,
                           Y  => -Extents.Y_Bearing - Extents.Height*0.5
                        );
                  end case;
                  if Layer.Texts (Index + 1).Markup then
                     Show_Markup
                     (  Layer.Face,
                        Context,
                       +Layer.Texts (Index + 1)
                     );
                  else
                     Show_Text
                     (  Layer.Face,
                        Context,
                       +Layer.Texts (Index + 1)
                     );
                  end if;
               end;
            end if;
         end if;
      end loop;
      Layer.Updated := False;
   end Draw;

   procedure Finalize (Layer : in out Flat_Annotation_Layer) is
   begin
      Finalize (Abstract_Layer (Layer));
      Delete (Layer.Texts);
   end Finalize;

   function Get_Color (Layer : Flat_Annotation_Layer)
      return Gdk_Color is
   begin
      return Layer.Color;
   end Get_Color;

   function Get_Face (Layer : Flat_Annotation_Layer)
      return Pango_Cairo_Font is
   begin
      return Layer.Face;
   end Get_Face;

   function Get_From (Layer : Flat_Annotation_Layer)
      return Cairo_Tuple is
   begin
      return Layer.From;
   end Get_From;

   function Get_Height (Layer : Flat_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Height;
   end Get_Height;

   function Get_Justify (Layer : Flat_Annotation_Layer)
      return Alignment is
   begin
      return Layer.Justify;
   end Get_Justify;

   function Get_Length (Layer : Flat_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_List
            (  Texts  : Gtk.Enums.String_List.GList;
               Ticks  : Tick_Parameters;
               Markup : Boolean
            )  return Annotation_List_Ptr is
      use Gtk.Enums.String_List;
      This  : GList   := Texts;
      Count : Natural := 0;
   begin
      while This /= Null_List loop
         Count := Count + 1;
         This  := Next (This);
      end loop;
      declare
         Result : constant Annotation_List_Ptr :=
                  new Annotation_List (1..Count);
         List   : Annotation_List renames Result.all;
      begin
         This := Texts;
         for Index in List'Range loop
            declare
               Text : constant UTF8_String := Get_Data (This);
            begin
               List (Index) :=
                  new Annotation_Text'
                      (  Size   => Text'Length,
                         Markup => Markup,
                         Length => Text'Length,
                         Buffer => Text
                      );
               This := Next (This);
            end;
         end loop;
         return Result;
      end;
   end Get_List;

   function Get_List
            (  Texts     : UTF8_String;
               Delimiter : Character;
               Ticks     : Tick_Parameters;
               Markup    : Boolean
            )  return Annotation_List_Ptr is
      Count : Natural := 1;
   begin
      for Index in Texts'Range loop
         if Texts (Index) = Delimiter then
            Count := Count + 1;
         end if;
      end loop;
      declare
         Result : constant Annotation_List_Ptr :=
                  new Annotation_List (1..Count);
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
                   (  Size   => Stop - Start,
                      Length => Stop - Start,
                      Markup => Markup,
                      Buffer => Texts (Start..Stop - 1)
                   );
            Start := Stop + 1;
         end loop;
         return Result;
      end;
   end Get_List;

   function Get_Markup
            (  Layer    : Flat_Annotation_Layer;
               Position : Positive
            )  return Boolean is
   begin
      if Layer.Texts = null or else Position > Layer.Texts'Last then
         raise Constraint_Error with "No such text";
      elsif Layer.Texts (Position) = null then
         return False;
      else
         return Layer.Texts (Position).Markup;
      end if;
   end Get_Markup;

   function Get_Properties_Number
            (  Layer : Flat_Annotation_Layer
            )  return Natural is
   begin
      return
      (  Layer_Property'Pos (Layer_Property'Last)
      -  Layer_Property'Pos (Layer_Property'First)
      +  1
      );
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Flat_Annotation_Layer;
               Property : Positive
            )  return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Font_Type =>
               return
                  Pango.Cairo.Fonts.Font_Type_Property.Gnew_Enum
                  (  Name    => "font-type",
                     Nick    => "font type",
                     Default => Pango_Font,
                     Blurb   => "The backend used for the font, " &
                                "e.g. toy font, pango font"
                  );
            when Property_From_X =>
               return
                  Gnew_Double
                  (  Name    => "x0",
                     Nick    => "x0",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The x-coordinate of the point " &
                                "corresponding to the location of " &
                                "the first annotation text"
                  );
            when Property_From_Y =>
               return
                  Gnew_Double
                  (  Name    => "y0",
                     Nick    => "y0",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The y-coordinate of the point " &
                                "corresponding to the location of " &
                                "the first annotation text"
                  );
            when Property_Length =>
               return
                  Gnew_Double
                  (  Name    => "length",
                     Nick    => "length",
                     Minimum => GDouble'First,
                     Maximum => GDouble'Last,
                     Default => 0.0,
                     Blurb   => "The annotation length"
                  );
            when Property_Scale_Angle =>
               return
                  Gnew_Double
                  (  Name    => "annotation-angle",
                     Nick    => "annotation line angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the line where " &
                                "annotation texts are located"
                  );
            when Property_Text_Angle =>
               return
                  Gnew_Double
                  (  Name    => "text-angle",
                     Nick    => "text angle",
                     Minimum =>-2.0 * Pi,
                     Maximum => 2.0 * Pi,
                     Default => 0.0,
                     Blurb   => "The angle of the annotation texts " &
                                "base line"
                  );
            when Property_Tick_Step =>
               return
                  Gnew_Double
                  (  Name    => "step",
                     Nick    => "step",
                     Minimum => 1.0E-6,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The distance between two " &
                                "consequent annotation texts"
                  );
            when Property_Tick_First =>
               return
                  Gnew_UInt
                  (  Name    => "first-tick",
                     Nick    => "first tick",
                     Minimum => GUInt (Tick_Number'First),
                     Maximum => GUInt (Tick_Number'Last),
                     Default => 1,
                     Blurb   => "The number of the first tick. " &
                                "The first tick is located at " &
                                "the beginning of the scale to which " &
                                "annotation texts are attached"
                  );
            when Property_Tick_Skipped =>
               return
                  Gnew_UInt
                  (  Name    => "skipped-tick",
                     Nick    => "skipped tick",
                     Minimum => 2,
                     Maximum => GUInt (Tick_Number'Last),
                     Default => GUInt (Tick_Number'Last),
                     Blurb   => "The number of the skipped tick. " &
                                "The ticks are numbered from 1 to " &
                                "skipped-tick. For the ticks with " &
                                "this number annotations are not drawn"
                  );
            when Property_Stretch =>
               return
                  Gnew_Double
                  (  Name    => "stretch",
                     Nick    => "stretch",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 1.0,
                     Blurb   => "The relation of the rendered width " &
                                "of an annotation text to its " &
                                "original width. The stretch value " &
                                "1 keeps texts unchanged"
                  );
            when Property_Height =>
               return
                  Gnew_Double
                  (  Name    => "height",
                     Nick    => "height",
                     Minimum => 0.0,
                     Maximum => GDouble'Last,
                     Default => 12.0,
                     Blurb   => "The annotation text font height"
                  );
            when Property_Family =>
               return
                  Gnew_String
                  (  Name    => "font-familiy",
                     Nick    => "font famility",
                     Default => "arial",
                     Blurb   => "The annotation text font family, " &
                                "e.g. courier"
                  );
            when Property_Justify =>
               return
                  Gtk.Layered.Alignment_Property.Gnew_Enum
                  (  Name    => "text-alignment",
                     Nick    => "text alignment",
                     Default => Center,
                     Blurb   => "The text alignment " &
                                "relatively to the annotation " &
                                "line"
                  );
            when Property_Slant =>
               return
                  Cairo.Font_Slant_Property.Gnew_Enum
                  (  Name    => "font-slant",
                     Nick    => "font slant",
                     Default => CAIRO_FONT_SLANT_NORMAL,
                     Blurb   => "The annotation text font slant"
                  );
            when Property_Font_Size =>
               return
                  Gnew_UInt
                  (  Name    => "font-size",
                     Nick    => "font size",
                     Minimum => 1,
                     Maximum => GUInt (GInt'Last),
                     Default => 12,
                     Blurb   => "The font size in points. " &
                                "The value is only relevant for " &
                                "pango fonts. For cairo toy size " &
                                "is ignored"
                  );
            when Property_Texts =>
               return
                  Gnew_String
                  (  Name    => "texts",
                     Nick    => "annotation texts",
                     Default => "",
                     Blurb   => "The list of annotation texts, " &
                                "separated by LFs"
                  );
            when Property_Markup =>
               return
                  Gnew_String
                  (  Name    => "markup-flags",
                     Nick    => "annotation text markups",
                     Default => "",
                     Blurb   => "The list of annotation markup " &
                                "text flags. " &
                                "For each text it contains one " &
                                "character, which is " &
                                "T for plain text or " &
                                "M for markup"
                  );
            when Property_Weight =>
               return
                  Pango.Enums.Weight_Property.Gnew_Enum
                  (  Name    => "font-weight",
                     Nick    => "font weight",
                     Default => Pango.Enums.Pango_Weight_Normal,
                     Blurb   => "The annotation text font weight"
                  );
            when Property_Color =>
               return
                  Gnew_Boxed
                  (  Name       => "color",
                     Boxed_Type => Gdk_Color_Type,
                     Nick       => "color",
                     Blurb      => "The annotation texts color"
                  );
            when Property_Scaled =>
               return
                  Gnew_Boolean
                  (  Name    => "scaled",
                     Nick    => "scaled",
                     Default => False,
                     Blurb   => "The annotation size is changed when " &
                                "the widget is resized"
                  );
         end case;
      end if;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Flat_Annotation_Layer;
               Property : Positive
            )  return GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Font_Type =>
                  Pango.Cairo.Fonts.Font_Type_Property.Set_Enum
                  (  Value,
                     Get_Type (Layer.Face)
                  );
               when Property_From_X =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From.X);
               when Property_From_Y =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.From.Y);
               when Property_Length =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Length);
               when Property_Scale_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Scale_Angle);
               when Property_Text_Angle =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Text_Angle);
               when Property_Color =>
                  Set_Value (Value, Layer.Color);
               when Property_Tick_Step =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Ticks.Step);
               when Property_Tick_First =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Layer.Ticks.First));
               when Property_Tick_Skipped =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Layer.Ticks.Skipped));
               when Property_Height =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Height);
               when Property_Family =>
                  Init (Value, GType_String);
                  Set_String (Value, Get_Family (Layer.Face));
               when Property_Justify =>
                  Gtk.Layered.Alignment_Property.Set_Enum
                  (  Value,
                     Layer.Justify
                  );
               when Property_Slant =>
                  Cairo.Font_Slant_Property.Set_Enum
                  (  Value,
                     Get_Slant (Layer.Face)
                  );
               when Property_Font_Size =>
                  Init (Value, GType_UInt);
                  Set_UInt (Value, GUInt (Get_Size (Layer.Face)));
               when Property_Stretch =>
                  Init (Value, GType_Double);
                  Set_Double (Value, Layer.Stretch);
               when Property_Texts =>
                  Init (Value, GType_String);
                  if Layer.Texts = null then
                     Set_String (Value, "");
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
                           Length := Length + List (Index).Length;
                        end loop;
                        declare
                           Text    : String (1..Length);
                           Pointer : Integer := Text'First;
                        begin
                           for Index in List'Range loop
                              if Index > List'First then
                                 Text (Pointer) := Character'Val (10);
                                 Pointer := Pointer + 1;
                              end if;
                              Text
                              (  Pointer
                              .. Pointer + List (Index).Length - 1
                              )  := +List (Index);
                              Pointer := Pointer + List (Index).Length;
                           end loop;
                           Set_String (Value, Text);
                        end;
                     end;
                  end if;
               when Property_Markup =>
                  Init (Value, GType_String);
                  if Layer.Texts = null then
                     Set_String (Value, "");
                  else
                     declare
                        List : Annotation_List renames Layer.Texts.all;
                        Text : String (List'Range);
                     begin
                        for Index in Text'Range loop
                           if List (Index).Markup then
                              Text (Index) := 'M';
                           else
                              Text (Index) := 'T';
                           end if;
                        end loop;
                        Set_String (Value, Text);
                     end;
                  end if;
               when Property_Scaled =>
                  Init (Value, GType_Boolean);
                  Set_Boolean (Value, Layer.Scaled);
               when Property_Weight =>
                  Pango.Enums.Weight_Property.Set_Enum
                  (  Value,
                     Get_Weight (Layer.Face)
                  );
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Scale_Angle (Layer : Flat_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Scale_Angle;
   end Get_Scale_Angle;

   function Get_Scaled (Layer : Flat_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Stretch (Layer : Flat_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Stretch;
   end Get_Stretch;

   function Get_Text
            (  Layer    : Flat_Annotation_Layer;
               Position : Positive
            )  return UTF8_String is
   begin
      if Layer.Texts = null or else Position > Layer.Texts'Last then
         raise Constraint_Error with "No such text";
      elsif Layer.Texts (Position) = null then
         return "";
      else
         return +Layer.Texts (Position);
      end if;
   end Get_Text;

   function Get_Text_Angle (Layer : Flat_Annotation_Layer)
      return GDouble is
   begin
      return Layer.Text_Angle;
   end Get_Text_Angle;

   function Get_Texts_Number (Layer : Flat_Annotation_Layer)
      return Natural is
   begin
      if Layer.Texts = null then
         return 0;
      else
         return Layer.Texts'Length;
      end if;
   end Get_Texts_Number;

   function Get_Ticks (Layer : Flat_Annotation_Layer)
      return Tick_Parameters is
   begin
      return Layer.Ticks;
   end Get_Ticks;

   function Is_Updated (Layer : Flat_Annotation_Layer)
      return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   procedure Move
             (  Layer  : in out Flat_Annotation_Layer;
                Offset : Cairo_Tuple
             )  is
   begin
      Layer.From.X  := Layer.From.X + Offset.X;
      Layer.From.Y  := Layer.From.Y + Offset.Y;
      Layer.Updated := True;
   end Move;

   procedure Restore
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : in out Flat_Annotation_Layer
             )  is
      Face        : Pango_Cairo_Font;
      Height      : GDouble;
      Stretch     : GDouble;
      From        : Cairo_Tuple;
      Length      : GDouble;
      Scale_Angle : GDouble;
      Text_Angle  : GDouble;
      Ticks       : Tick_Parameters;
      Color       : Gdk_Color;
      Justify     : Alignment;
   begin
      Restore (Stream, Face);
      Restore (Stream, Height);
      Restore (Stream, Stretch);
      Restore (Stream, From);
      Restore (Stream, Length);
      Restore (Stream, Scale_Angle);
      Restore (Stream, Text_Angle);
      Restore (Stream, Ticks);
      Restore (Stream, Color);
      Restore (Stream, Justify);
      Restore (Stream, Layer.Scaled);
      Set
      (  Layer       => Layer,
         Ticks       => Ticks,
         From        => From,
         Length      => Length,
         Scale_Angle => Scale_Angle,
         Face        => Face,
         Height      => Height,
         Stretch     => Stretch,
         Color       => Color,
         Text_Angle  => Text_Angle,
         Justify     => Justify
      );
      declare
         use Gtk.Layered.Stream_IO;
         Markup : constant Bit_Array := Restore (Stream'Access);
      begin
         Free (Layer.Texts);
         Layer.Texts := new Annotation_List (Markup'Range);
         for Index in Markup'Range loop
            Layer.Set_Text
            (  Index,
               Restore (Stream'Access),
               Markup (Index)
            );
         end loop;
      end;
   end Restore;

   procedure Scale
             (  Layer  : in out Flat_Annotation_Layer;
                Factor : GDouble
             )  is
      Ticks  : Tick_Parameters := Layer.Ticks;
      Height : constant GDouble := Layer.Height * Factor;
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
             (  Layer       : in out Flat_Annotation_Layer;
                Ticks       : Tick_Parameters;
                From        : Cairo_Tuple;
                Length      : GDouble;
                Scale_Angle : GDouble;
                Face        : Pango_Cairo_Font;
                Height      : GDouble;
                Stretch     : GDouble;
                Color       : Gdk_Color;
                Text_Angle  : GDouble;
                Justify     : Alignment
             )  is
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

   procedure Set_Face
             (  Layer : in out Flat_Annotation_Layer;
                Face  : Pango_Cairo_Font
             )  is
   begin
      Layer.Face    := Face;
      Layer.Updated := True;
   end Set_Face;

   procedure Set_Property_Value
             (  Layer    : in out Flat_Annotation_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Font_Type =>
               Set_Type
               (  Layer.Face,
                  Pango.Cairo.Fonts.Font_Type_Property.Get_Enum (Value)
               );
            when Property_From_X =>
               Layer.From.X := Get_Double (Value);
            when Property_From_Y =>
               Layer.From.Y := Get_Double (Value);
            when Property_Length =>
               Layer.Length := Get_Double (Value);
            when Property_Color =>
               Layer.Color := Get_Value (Value);
            when Property_Stretch =>
               Layer.Stretch := Get_Double (Value);
               if Layer.Stretch < 0.0 then
                  Layer.Stretch := 0.0;
               end if;
            when Property_Scale_Angle =>
               Layer.Scale_Angle := Get_Double (Value);
               if Layer.Scale_Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Scale_Angle :=
                     GDouble'Remainder (Layer.Scale_Angle, 2.0 * Pi);
               end if;
            when Property_Text_Angle =>
               Layer.Text_Angle := Get_Double (Value);
               if Layer.Text_Angle not in -2.0 * Pi..2.0 * Pi then
                  Layer.Text_Angle :=
                     GDouble'Remainder (Layer.Text_Angle, 2.0 * Pi);
               end if;
            when Property_Height =>
               Layer.Height := Get_Double (Value);
               if Layer.Height < 0.0 then
                  Layer.Height := 0.0;
               end if;
            when Property_Justify =>
               Layer.Justify :=
                  Gtk.Layered.Alignment_Property.Get_Enum (Value);
            when Property_Tick_Step =>
               Layer.Ticks.Step := Get_Double (Value);
               if Layer.Ticks.Step < 1.0E-6 then
                  Layer.Ticks.Step := 1.0E-6;
               end if;
            when Property_Tick_First =>
               if Get_UInt (Value) < 1 then
                  Layer.Ticks.First := 1;
               elsif Get_UInt (Value) > GUInt (Tick_Number'Last) then
                  Layer.Ticks.First := Tick_Number'Last;
               else
                  Layer.Ticks.First := Tick_Number (Get_UInt (Value));
               end if;
            when Property_Tick_Skipped =>
               if Get_UInt (Value) < 2 then
                  Layer.Ticks.Skipped := 2;
               elsif Get_UInt (Value) > GUInt (Tick_Number'Last) then
                  Layer.Ticks.Skipped := Tick_Number'Last;
               else
                  Layer.Ticks.Skipped := Tick_Number (Get_UInt (Value));
               end if;
            when Property_Family =>
               Set_Family (Layer.Face, Get_String (Value));
            when Property_Slant =>
               Set_Slant
               (  Layer.Face,
                  Cairo.Font_Slant_Property.Get_Enum (Value)
               );
            when Property_Font_Size =>
               Set_Size
               (  Layer.Face,
                  GInt
                  (  GUInt'Max
                     (  GUInt'Min
                        (  Get_UInt (Value),
                           GUInt (GInt'Last)
                        ),
                        1
               )  )  );
            when Property_Texts =>
               Set_Texts
               (  Layer,
                  Get_String (Value),
                  Character'Val (10),
                  False
                );
            when Property_Markup =>
               declare
                  Markup : constant String := Get_String (Value);
               begin
                  if Layer.Texts /= null then
                     for Index in Markup'Range loop
                        exit when Index not in Layer.Texts'Range;
                        Layer.Texts (Index).Markup :=
                           Markup (Index) = 'M';
                     end loop;
                  end if;
               end;
            when Property_Scaled =>
               Layer.Scaled := Get_Boolean (Value);
            when Property_Weight =>
               Set_Weight
               (  Layer.Face,
                  Pango.Enums.Weight_Property.Get_Enum (Value)
               );
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   procedure Set_Scaled
             (  Layer  : in out Flat_Annotation_Layer;
                Scaled : Boolean
             )  is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   procedure Set_Text
             (  Layer    : in out Flat_Annotation_Layer;
                Position : Positive;
                Text     : UTF8_String;
                Markup   : Boolean := False
             )  is
   begin
      if Layer.Texts = null then
         if Position = 1 then
            Layer.Texts :=
               new Annotation_List'
                   (  1..1 => new Annotation_Text'
                                  (  Size   => Text'Length,
                                     Length => Text'Length,
                                     Markup => Markup,
                                     Buffer => Text
                   )              );
            Layer.Updated := True;
            return;
         end if;
      elsif Position > Layer.Texts'Last then
         if Position = Layer.Texts'Last + 1 then
            declare
               Old_Texts : Annotation_List_Ptr := Layer.Texts;
            begin
               Layer.Texts := new Annotation_List (1..Position);
               Layer.Texts (Old_Texts'Range) := Old_Texts.all;
               Layer.Texts (Position) :=
                  new Annotation_Text'
                      (  Size   => Text'Length,
                         Length => Text'Length,
                         Markup => Markup,
                         Buffer => Text
                      );
               Free (Old_Texts);
               Layer.Updated := True;
               return;
            end;
         end if;
      else
         if (  Layer.Texts (Position) /= null
            and then
               Layer.Texts (Position).Size >= Text'Length
            )
         then
            declare
               This : Annotation_Text renames
                      Layer.Texts (Position).all;
            begin
               This.Buffer (1..Text'Length) := Text;
               This.Length := Text'Length;
               This.Markup := Markup;
            end;
         else
            Free (Layer.Texts (Position));
            Layer.Texts (Position) :=
               new Annotation_Text'
                   (  Size   => Text'Length,
                      Length => Text'Length,
                      Markup => Markup,
                      Buffer => Text
                   );
         end if;
         Layer.Updated := True;
         return;
      end if;
      raise Constraint_Error with "No such text";
   end Set_Text;

   procedure Set_Texts
             (  Layer  : in out Flat_Annotation_Layer;
                Texts  : Gtk.Enums.String_List.GList;
                Markup : Boolean := False
             )  is
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

   procedure Set_Texts
             (  Layer     : in out Flat_Annotation_Layer;
                Texts     : UTF8_String;
                Delimiter : Character := ' ';
                Markup    : Boolean := False
             )  is
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

   procedure Store
             (  Stream : in out Root_Stream_Type'Class;
                Layer  : Flat_Annotation_Layer
             )  is
   begin
      Store (Stream, Layer.Face);
      Store (Stream, Layer.Height);
      Store (Stream, Layer.Stretch);
      Store (Stream, Layer.From);
      Store (Stream, Layer.Length);
      Store (Stream, Layer.Scale_Angle);
      Store (Stream, Layer.Text_Angle);
      Store (Stream, Layer.Ticks);
      Store (Stream, Layer.Color);
      Store (Stream, Layer.Justify);
      Store (Stream, Layer.Scaled);
      declare
         Markup : Bit_Array (1..Layer.Get_Texts_Number);
      begin
         for Index in 1..Layer.Get_Texts_Number loop
            Markup (Index) := Layer.Get_Markup (Index);
         end loop;
         Store (Stream, Markup);
      end;
      for Index in 1..Layer.Get_Texts_Number loop
         Store (Stream, Layer.Get_Text (Index));
      end loop;
   end Store;

end Gtk.Layered.Flat_Annotation;
