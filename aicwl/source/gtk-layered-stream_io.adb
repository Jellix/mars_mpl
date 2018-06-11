--                                                                    --
--  package Gtk.Layered.Stream_IO   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2011       --
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

with Ada.IO_Exceptions;
with Ada.Tags;

with Cairo.Elementary_Functions;
with Cairo.Font_Face;

with Gtk.Layered.Arc;
with Gtk.Layered.Bar;
with Gtk.Layered.Cache;
with Gtk.Layered.Cap;
with Gtk.Layered.Clock_Hand;
with Gtk.Layered.Elliptic_Annotation;
with Gtk.Layered.Elliptic_Background;
with Gtk.Layered.Elliptic_Bar;
with Gtk.Layered.Elliptic_Scale;
with Gtk.Layered.Flat_Annotation;
with Gtk.Layered.Flat_Needle;
with Gtk.Layered.Flat_Scale;
with Gtk.Layered.Label;
with Gtk.Layered.Needle;
with Gtk.Layered.Rectangular_Background;
with Gtk.Layered.Rectangular_Clip_Region;
with Gtk.Layered.Sector_Needle;
with Gtkada.Types;

with Interfaces;

package body Gtk.Layered.Stream_IO is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   package body Generic_Modular_IO is

      procedure Restore
        (Stream : in out Ada.Streams.Root_Stream_Type'Class;
         Value  : out Modular)
      is
         Factor : Modular := 1;
         Octet  : Character;
      begin
         Value := 0;
         loop
            Character'Read (Stream'Access, Octet);
            Value :=
               Value or Factor * (Character'Pos (Octet) and 16#7F#);
            Factor := Factor * 2**7;
            exit when Character'Pos (Octet) <= 127;
         end loop;
      end Restore;

      procedure Store
        (Stream : in out Ada.Streams.Root_Stream_Type'Class;
         Value  : Modular)
      is
         Data : Modular := Value;
      begin
         while Data > 127 loop
            Character'Write
              (Stream'Unchecked_Access,
               Character'Val ((Data and 16#7F#) or 16#80#));
            Data := Data / 2**7;
         end loop;
         Character'Write
           (Stream'Unchecked_Access,
            Character'Val (Data));
      end Store;

   end Generic_Modular_IO;

   pragma Warnings (Off, "declaration of ""Restore"" hides one");
   pragma Warnings (Off, "declaration of ""Store"" hides one");
   package GUInt_IO       is new Generic_Modular_IO (Guint);
   pragma Warnings (On, "declaration of ""Store"" hides one");
   pragma Warnings (On, "declaration of ""Restore"" hides one");

   package GUInt16_IO     is new Generic_Modular_IO (Guint16);
   package Unsigned_32_IO is new Generic_Modular_IO (Interfaces.Unsigned_32);
   package Unsigned_64_IO is new Generic_Modular_IO (Interfaces.Unsigned_64);

   function Get_Type (Layer : Abstract_Layer'Class)
      return Layer_Type is
      Name : constant String := Ada.Tags.Expanded_Name (Layer'Tag);
   begin
      for Index in reverse Name'Range loop
         if Name (Index) = '.' then
            return Layer_Type'Value (Name (Index + 1 .. Name'Last));
         end if;
      end loop;
      return Layer_Type'Value (Name);
   exception
      when Constraint_Error =>
         raise Constraint_Error with
            "Don't know how to store " & Name;
   end Get_Type;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Widget : not null access Gtk_Layered_Record'Class)
   is
      Under : access Layer_Location'Class := Widget;
      Ratio : Gdouble;
      This  : Layer_Type;
   begin
      Restore (Stream, Ratio);
      Widget.all.Set_Aspect_Ratio (Ratio);
      loop
         Restore (Stream, This);
         case This is
            when None =>
               exit;
            when Arc_Layer =>
               Under :=
                 Gtk.Layered.Arc.Add (Under, Stream'Access).all.Above;
            when Bar_Layer =>
               Under :=
                 Gtk.Layered.Bar.Add (Under, Stream'Access).all.Above;
            when Cache_Layer =>
               Under :=
                 Gtk.Layered.Cache.Add (Under, Stream'Access).all.Above;
            when Cap_Layer =>
               Under :=
                 Gtk.Layered.Cap.Add (Under, Stream'Access).all.Above;
            when Clock_Hand_Layer =>
               Under :=
                 Gtk.Layered.Clock_Hand.Add (Under, Stream'Access).all.Above;
            when Elliptic_Annotation_Layer =>
               Under :=
                 Gtk.Layered.Elliptic_Annotation.
                   Add (Under, Stream'Access).all.Above;
            when Elliptic_Background_Layer =>
               Under :=
                 Gtk.Layered.Elliptic_Background.
                   Add (Under, Stream'Access).all.Above;
            when Elliptic_Bar_Layer =>
               Under :=
                 Gtk.Layered.Elliptic_Bar.
                   Add (Under, Stream'Access).all.Above;
            when Elliptic_Scale_Layer =>
               Under :=
                 Gtk.Layered.Elliptic_Scale.
                   Add (Under, Stream'Access).all.Above;
            when Flat_Annotation_Layer =>
               Under :=
                 Gtk.Layered.Flat_Annotation.
                   Add (Under, Stream'Access).all.Above;
            when Flat_Needle_Layer =>
               Under :=
                 Gtk.Layered.Flat_Needle.
                   Add (Under, Stream'Access).all.Above;
            when Flat_Scale_Layer =>
               Under :=
                 Gtk.Layered.Flat_Scale.
                   Add (Under, Stream'Access).all.Above;
            when Label_Layer =>
               Under :=
                 Gtk.Layered.Label.
                   Add (Under, Stream'Access).all.Above;
            when Needle_Layer =>
               Under :=
                 Gtk.Layered.Needle.
                   Add (Under, Stream'Access).all.Above;
            when Rectangular_Background_Layer =>
               Under :=
                 Gtk.Layered.Rectangular_Background.
                   Add (Under, Stream'Access).all.Above;
            when Rectangular_Clip_Region_On_Layer =>
               declare
                  Region : Gtk.Layered.Rectangular_Clip_Region.
                    Rectangular_Clip_Region_On_Layer renames
                      Gtk.Layered.Rectangular_Clip_Region.
                        Add (Under, Stream'Access).all;
               begin
                  Under := Region.Above;
               end;
            when Sector_Needle_Layer =>
               Under :=
                 Gtk.Layered.Sector_Needle.
                   Add (Under, Stream'Access).all.Above;
            when Foreground_Layer | Rectangular_Clip_Region_Off_Layer =>
               if Under.all in Abstract_Layer'Class then
                  Under := Abstract_Layer'Class (Under.all).Above;
               end if;
         end case;
      end loop;
   end Restore;

   procedure Restore
     (Stream     : in out Ada.Streams.Root_Stream_Type'Class;
      Adjustment : out Gtk.Adjustment.Gtk_Adjustment)
   is
      Value          : Gdouble;
      Lower          : Gdouble;
      Upper          : Gdouble;
      Step_Increment : Gdouble;
      Page_Increment : Gdouble;
   begin
      Restore (Stream, Gdouble (Value));
      Restore (Stream, Gdouble (Lower));
      Restore (Stream, Gdouble (Upper));
      Restore (Stream, Gdouble (Step_Increment));
      Restore (Stream, Gdouble (Page_Increment));
      Gtk.Adjustment.Gtk_New
        (Adjustment,
         Value,
         Lower,
         Upper,
         Step_Increment,
         Page_Increment);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Ada.Strings.Alignment)
   is
      Position : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Position);
      Value := Ada.Strings.Alignment'Val (Position);
   exception
      when Constraint_Error =>
         raise Ada.IO_Exceptions.Data_Error with "Alignment out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Boolean)
   is
      Data : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Data);
      Value := 0 /= (Data and 1);
   end Restore;

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean)
   is
      Value : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Value);
      Value_1 := 0 /= (Value and 1);
      Value_2 := 0 /= (Value and 2);
   end Restore;

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean)
   is
      Value : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Value);
      Value_1 := 0 /= (Value and 1);
      Value_2 := 0 /= (Value and 2);
      Value_3 := 0 /= (Value and 4);
   end Restore;

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean)
   is
      Value : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Value);
      Value_1 := 0 /= (Value and 1);
      Value_2 := 0 /= (Value and 2);
      Value_3 := 0 /= (Value and 4);
      Value_4 := 0 /= (Value and 8);
   end Restore;

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean)
   is
      Value : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Value);
      Value_1 := 0 /= (Value and 1);
      Value_2 := 0 /= (Value and 2);
      Value_3 := 0 /= (Value and 4);
      Value_4 := 0 /= (Value and 8);
      Value_5 := 0 /= (Value and 16);
   end Restore;

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean;
      Value_6 : out Boolean)
   is
      Value : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Value);
      Value_1 := 0 /= (Value and 1);
      Value_2 := 0 /= (Value and 2);
      Value_3 := 0 /= (Value and 4);
      Value_4 := 0 /= (Value and 8);
      Value_5 := 0 /= (Value and 16);
      Value_6 := 0 /= (Value and 32);
   end Restore;

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean;
      Value_6 : out Boolean;
      Value_7 : out Boolean)
   is
      Value : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Value);
      Value_1 := 0 /= (Value and 1);
      Value_2 := 0 /= (Value and 2);
      Value_3 := 0 /= (Value and 4);
      Value_4 := 0 /= (Value and 8);
      Value_5 := 0 /= (Value and 16);
      Value_6 := 0 /= (Value and 32);
      Value_7 := 0 /= (Value and 64);
   end Restore;

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean;
      Value_6 : out Boolean;
      Value_7 : out Boolean;
      Value_8 : out Boolean)
   is
      Value : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Value);
      Value_1 := 0 /= (Value and 1);
      Value_2 := 0 /= (Value and 2);
      Value_3 := 0 /= (Value and 4);
      Value_4 := 0 /= (Value and 8);
      Value_5 := 0 /= (Value and 16);
      Value_6 := 0 /= (Value and 32);
      Value_7 := 0 /= (Value and 64);
      Value_8 := 0 /= (Value and 128);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Font_Face)
   is
      Family : aliased constant Gtkada.Types.Chars_Ptr :=
                 Gtkada.Types.New_String (UTF8_String'(Restore (Stream'Access)));
      Slant  : Cairo.Cairo_Font_Slant;
      Weight : Cairo.Cairo_Font_Weight;
   begin
      Restore (Stream, Slant);
      Restore (Stream, Weight);
      Value := Cairo.Font_Face.Toy_Font_Face_Create (Family => Family,
                                                     Slant  => Slant,
                                                     Weight => Weight);
      Gtkada.Types.g_free (Family);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Font_Slant)
   is
      Position : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Position);
      Value := Cairo.Cairo_Font_Slant'Val (Position);
   exception
      when Constraint_Error =>
         raise
           Ada.IO_Exceptions.Data_Error with "Cairo_Font_Slant out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Font_Weight)
   is
      Position : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Position);
      Value := Cairo.Cairo_Font_Weight'Val (Position);
   exception
      when Constraint_Error =>
         raise
           Ada.IO_Exceptions.Data_Error with "Cairo_Font_Weight out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Line_Cap)
   is
      Position : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Position);
      Value := Cairo.Cairo_Line_Cap'Val (Position);
   exception
      when Constraint_Error =>
         raise Ada.IO_Exceptions.Data_Error with "Cairo_Line_Cap out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Ellipses.Cairo_Tuple) is
   begin
      Restore (Stream, Value.X);
      Restore (Stream, Value.Y);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Elliptic_Arc_Closure)
   is
      Shape : Elliptic_Shape_Type;
   begin
      Restore (Stream, Shape);
      case Shape is
         when Sector =>
            declare
               Center : Cairo.Ellipses.Cairo_Tuple;
            begin
               Restore (Stream, Center);
               Value := (Sector, Center);
            end;
         when Bagel =>
            declare
               The_Arc : Cairo.Ellipses.Ellipse_Parameters;
            begin
               Restore (Stream, The_Arc);
               Value := (Bagel, The_Arc);
            end;
         when Segment =>
            Value := (Shape => Segment);
      end case;
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Elliptic_Shape_Type)
   is
      Position : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Position);
      Value := Elliptic_Shape_Type'Val (Position);
   exception
      when Constraint_Error =>
         raise
           Ada.IO_Exceptions.Data_Error with "Elliptic_Shape_Type out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out End_Parameters) is
   begin
      Restore (Stream, Value.Length);
      Restore (Stream, Value.Width);
      Restore (Stream, Value.Cap);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Gdk.Color.Gdk_Color)
   is
      R, G, B : Guint16;
   begin
      GUInt16_IO.Restore (Stream, R);
      GUInt16_IO.Restore (Stream, G);
      GUInt16_IO.Restore (Stream, B);
      Gdk.Color.Set_Rgb (Value, R, G, B);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Gtk.Enums.Gtk_Shadow_Type)
   is
      Position : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Position);
      Value := Gtk.Enums.Gtk_Shadow_Type'Val (Position);
   exception
      when Constraint_Error =>
         raise Ada.IO_Exceptions.Data_Error with "Gtk_Shadow_Type out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Interpolation_Mode)
   is
      Position : Guint16;
   begin
      GUInt16_IO.Restore (Stream, Position);
      Value := Interpolation_Mode'Val (Position);
   exception
      when Constraint_Error =>
         raise
           Ada.IO_Exceptions.Data_Error with "Interpolation_Mode out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Gdouble)
   is
      Fraction : Interfaces.Unsigned_32;
      Exponent : Interfaces.Unsigned_32;

      use type Interfaces.Unsigned_32;
   begin
      Unsigned_32_IO.Restore (Stream, Fraction);
      Unsigned_32_IO.Restore (Stream, Exponent);
      Value := Gdouble (Fraction) / 2.0**30;
      if 0 /= (Exponent and 1) then
         Value := -Value;
      end if;
      if 0 = (Exponent and 2) then
         Value := Value * 2.0**Integer (Exponent / 4);
      else
         Value := Value / 2.0**Integer (Exponent / 4);
      end if;
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Ellipses.Ellipse_Parameters) is
   begin
      Restore (Stream, Value.Center);
      Restore (Stream, Value.Major_Curvature);
      Restore (Stream, Value.Minor_Radius);
      Restore (Stream, Value.Angle);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Guint) renames GUInt_IO.Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Guint16) renames GUInt16_IO.Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Layer_Type)
   is
      Position : Guint16;
   begin
      Restore (Stream, Position);
      Value := Layer_Type'Val (Position);
   exception
      when Constraint_Error =>
         raise Ada.IO_Exceptions.Data_Error with "Layer_Type out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Line_Parameters) is
   begin
      Restore (Stream, Value.Width);
      Restore (Stream, Value.Color);
      Restore (Stream, Value.Line_Cap);
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Tick_Parameters) is
   begin
      Restore (Stream, Value.Step);
      Unsigned_64_IO.Restore (Stream, Interfaces.Unsigned_64 (Value.First));
      Unsigned_64_IO.Restore (Stream, Interfaces.Unsigned_64 (Value.Skipped));
   end Restore;

   function Restore
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Bit_Array
   is
      Count : Natural;
      Octet : Interfaces.Unsigned_8;
      Mask  : Interfaces.Unsigned_8 := 1;

      use type Interfaces.Unsigned_8;
   begin
      Unsigned_32_IO.Restore (Stream.all, Interfaces.Unsigned_32 (Count));
      return Result : Bit_Array (1 .. Count) do
         for Index in Result'Range loop
            if Mask = 1 then
               Interfaces.Unsigned_8'Read (Stream, Octet);
            end if;
            Result (Index) := 0 /= (Octet and Mask);
            if Mask = 2#1000_0000# then
               Mask := 1;
            else
               Mask := Mask * 2;
            end if;
         end loop;
      end return;
   end Restore;

   function Restore
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return UTF8_String
   is
      Count : Natural;
   begin
      Unsigned_32_IO.Restore (Stream.all, Interfaces.Unsigned_32 (Count));
      return Text : String (1 .. Count) do
         String'Read (Stream, Text);
      end return;
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Text_Transformation)
   is
      Position : Guint16;
   begin
      Restore (Stream, Position);
      Value := Text_Transformation'Val (Position);
   exception
      when Constraint_Error =>
         raise
           Ada.IO_Exceptions.Data_Error with "Text_Transformation out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Vertical_Alignment)
   is
      Position : Guint16;
   begin
      Restore (Stream, Position);
      Value := Vertical_Alignment'Val (Position);
   exception
      when Constraint_Error =>
         raise
           Ada.IO_Exceptions.Data_Error with "Vertical alignment out of range";
   end Restore;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Waveform_Drawing_Method)
   is
      Position : Guint16;
   begin
      Restore (Stream, Position);
      Value := Waveform_Drawing_Method'Val (Position);
   exception
      when Constraint_Error =>
         raise
           Ada.IO_Exceptions.Data_Error with
             "Waveform_Drawing_Method out of range";
   end Restore;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Widget : not null access constant Gtk_Layered_Record'Class)
   is
      This : Abstract_Layer_Ptr := Widget.all.Bottom;
   begin
      if This /= null then
         Store (Stream, Widget.all.Get_Aspect_Ratio);
         loop
            Store (Stream, Get_Type (This.all));
            Store (Stream, This.all);
            exit when This.all.Next = Widget.all.Bottom;
            This := This.all.Next;
         end loop;
      end if;
      Store (Stream, None);
   end Store;

   procedure Store
     (Stream     : in out Ada.Streams.Root_Stream_Type'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Store (Stream, Gtk.Adjustment.Get_Value (Adjustment));
      Store (Stream, Gtk.Adjustment.Get_Lower (Adjustment));
      Store (Stream, Gtk.Adjustment.Get_Upper (Adjustment));
      Store (Stream, Gtk.Adjustment.Get_Step_Increment (Adjustment));
      Store (Stream, Gtk.Adjustment.Get_Page_Increment (Adjustment));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Ada.Strings.Alignment) is
   begin
      GUInt16_IO.Store (Stream, Guint16 (Ada.Strings.Alignment'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Boolean)
   is
      Data : Guint16 := 0;
   begin
      if Value then
         Data := Data or 1;
      end if;
      GUInt16_IO.Store (Stream, Data);
   end Store;

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean)
   is
      Value : Guint16 := 0;
   begin
      if Value_1 then
         Value := Value or 1;
      end if;
      if Value_2 then
         Value := Value or 2;
      end if;
      GUInt16_IO.Store (Stream, Value);
   end Store;

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean)
   is
      Value : Guint16 := 0;
   begin
      if Value_1 then
         Value := Value or 1;
      end if;
      if Value_2 then
         Value := Value or 2;
      end if;
      if Value_3 then
         Value := Value or 4;
      end if;
      GUInt16_IO.Store (Stream, Value);
   end Store;

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean)
   is
      Value : Guint16 := 0;
   begin
      if Value_1 then
         Value := Value or 1;
      end if;
      if Value_2 then
         Value := Value or 2;
      end if;
      if Value_3 then
         Value := Value or 4;
      end if;
      if Value_4 then
         Value := Value or 8;
      end if;
      GUInt16_IO.Store (Stream, Value);
   end Store;

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean)
   is
      Value : Guint16 := 0;
   begin
      if Value_1 then
         Value := Value or 1;
      end if;
      if Value_2 then
         Value := Value or 2;
      end if;
      if Value_3 then
         Value := Value or 4;
      end if;
      if Value_4 then
         Value := Value or 8;
      end if;
      if Value_5 then
         Value := Value or 16;
      end if;
      GUInt16_IO.Store (Stream, Value);
   end Store;

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean;
      Value_6 : Boolean)
   is
      Value : Guint16 := 0;
   begin
      if Value_1 then
         Value := Value or 1;
      end if;
      if Value_2 then
         Value := Value or 2;
      end if;
      if Value_3 then
         Value := Value or 4;
      end if;
      if Value_4 then
         Value := Value or 8;
      end if;
      if Value_5 then
         Value := Value or 16;
      end if;
      if Value_6 then
         Value := Value or 32;
      end if;
      GUInt16_IO.Store (Stream, Value);
   end Store;

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean;
      Value_6 : Boolean;
      Value_7 : Boolean)
   is
      Value : Guint16 := 0;
   begin
      if Value_1 then
         Value := Value or 1;
      end if;
      if Value_2 then
         Value := Value or 2;
      end if;
      if Value_3 then
         Value := Value or 4;
      end if;
      if Value_4 then
         Value := Value or 8;
      end if;
      if Value_5 then
         Value := Value or 16;
      end if;
      if Value_6 then
         Value := Value or 32;
      end if;
      if Value_7 then
         Value := Value or 64;
      end if;
      GUInt16_IO.Store (Stream, Value);
   end Store;

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean;
      Value_6 : Boolean;
      Value_7 : Boolean;
      Value_8 : Boolean)
   is
      Value : Guint16 := 0;
   begin
      if Value_1 then
         Value := Value or 1;
      end if;
      if Value_2 then
         Value := Value or 2;
      end if;
      if Value_3 then
         Value := Value or 4;
      end if;
      if Value_4 then
         Value := Value or 8;
      end if;
      if Value_5 then
         Value := Value or 16;
      end if;
      if Value_6 then
         Value := Value or 32;
      end if;
      if Value_7 then
         Value := Value or 64;
      end if;
      if Value_8 then
         Value := Value or 128;
      end if;
      GUInt16_IO.Store (Stream, Value);
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : in     Cairo.Cairo_Font_Face) is
   begin
      Store (Stream => Stream,
             Value  =>
               Gtkada.Types.Value
                 (Cairo.Font_Face.Toy_Font_Face_Get_Family (Value)));
      Store (Stream => Stream,
             Value  => Cairo.Font_Face.Toy_Font_Face_Get_Slant  (Value));
      Store (Stream => Stream,
             Value  => Cairo.Font_Face.Toy_Font_Face_Get_Weight (Value));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Cairo_Font_Slant) is
   begin
      GUInt16_IO.Store (Stream, Guint16 (Cairo.Cairo_Font_Slant'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Cairo_Font_Weight) is
   begin
      GUInt16_IO.Store (Stream, Guint16 (Cairo.Cairo_Font_Weight'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Cairo_Line_Cap) is
   begin
      GUInt16_IO.Store (Stream, Guint16 (Cairo.Cairo_Line_Cap'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Store (Stream, Value.X);
      Store (Stream, Value.Y);
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Elliptic_Arc_Closure) is
   begin
      Store (Stream, Value.Shape);
      case Value.Shape is
         when Sector  => Store (Stream, Value.Center);
         when Bagel   => Store (Stream, Value.Arc);
         when Segment => null;
      end case;
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Elliptic_Shape_Type) is
   begin
      GUInt16_IO.Store (Stream, Guint16 (Elliptic_Shape_Type'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : End_Parameters) is
   begin
      Store (Stream, Value.Length);
      Store (Stream, Value.Width);
      Store (Stream, Value.Cap);
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Gdk.Color.Gdk_Color) is
   begin
      GUInt16_IO.Store (Stream, Gdk.Color.Red   (Value));
      GUInt16_IO.Store (Stream, Gdk.Color.Green (Value));
      GUInt16_IO.Store (Stream, Gdk.Color.Blue  (Value));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Guint) renames GUInt_IO.Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Guint16) renames GUInt16_IO.Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Gdouble)
   is
      Fraction : Gdouble  := abs Value;
      Exponent : Integer := 0;

      use type Interfaces.Unsigned_32;
   begin
      if Fraction > 0.0 then
         Exponent := Integer (Cairo.Elementary_Functions.Log (Fraction, 2.0));
      end if;
      Fraction := Fraction / 2.0**Exponent;
      Unsigned_32_IO.Store
        (Stream, Interfaces.Unsigned_32 (Fraction * 2.0 ** 30));
      if Exponent < 0 then
         if Value < 0.0 then
            Unsigned_32_IO.Store
              (Stream, Interfaces.Unsigned_32 ((-Exponent) * 4) + 3);
         else
            Unsigned_32_IO.Store
              (Stream, Interfaces.Unsigned_32 ((-Exponent) * 4) + 2);
         end if;
      else
         if Value < 0.0 then
            Unsigned_32_IO.Store
              (Stream, Interfaces.Unsigned_32 (Exponent * 4) + 1);
         else
            Unsigned_32_IO.Store
              (Stream, Interfaces.Unsigned_32 (Exponent * 4));
         end if;
      end if;
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Ellipses.Ellipse_Parameters) is
   begin
      Store (Stream, Value.Center);
      Store (Stream, Value.Major_Curvature);
      Store (Stream, Value.Minor_Radius);
      Store (Stream, Value.Angle);
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Gtk.Enums.Gtk_Shadow_Type) is
   begin
      Store (Stream, Guint16'(Gtk.Enums.Gtk_Shadow_Type'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Interpolation_Mode) is
   begin
      Store (Stream, Guint16'(Interpolation_Mode'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Layer_Type) is
   begin
      Store (Stream, Guint16 (Layer_Type'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Line_Parameters) is
   begin
      Store (Stream, Value.Width);
      Store (Stream, Value.Color);
      Store (Stream, Value.Line_Cap);
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Bit_Array)
   is
      Mask  : Interfaces.Unsigned_8 := 1;
      Octet : Interfaces.Unsigned_8 := 0;

      use type Interfaces.Unsigned_8;
   begin
      Unsigned_32_IO.Store (Stream, Interfaces.Unsigned_32 (Value'Length));
      for Index in Value'Range loop
         if Value (Index) then
            Octet := Octet or Mask;
         end if;
         if Mask = 2#1000_0000# then
            Interfaces.Unsigned_8'Write (Stream'Access, Octet);
            Mask  := 1;
            Octet := 0;
         else
            Mask := Mask * 2;
         end if;
      end loop;
      if Mask > 1 then
         Interfaces.Unsigned_8'Write (Stream'Access, Octet);
      end if;
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : UTF8_String) is
   begin
      Unsigned_32_IO.Store (Stream, Interfaces.Unsigned_32 (Value'Length));
      String'Write (Stream'Access, Value);
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Text_Transformation) is
   begin
      Store (Stream, Guint16'(Text_Transformation'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Tick_Parameters) is
   begin
      Store (Stream, Value.Step);
      Unsigned_64_IO.Store (Stream, Interfaces.Unsigned_64 (Value.First));
      Unsigned_64_IO.Store (Stream, Interfaces.Unsigned_64 (Value.Skipped));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Vertical_Alignment) is
   begin
      Store (Stream, Guint16 (Vertical_Alignment'Pos (Value)));
   end Store;

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Waveform_Drawing_Method) is
   begin
      Store (Stream, Guint16'(Waveform_Drawing_Method'Pos (Value)));
   end Store;

   pragma Warnings (On, "declaration hides ""Widget""");
   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Layered.Stream_IO;
