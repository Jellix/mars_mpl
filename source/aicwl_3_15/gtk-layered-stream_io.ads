--                                                                    --
--  package Gtk.Layered.Stream_IO   Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2011       --
--                                                                    --
--                                Last revision :  10:27 26 Mar 2016  --
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

with Ada.Strings;      use Ada.Strings;
with Gtk.Enums;        use Gtk.Enums;

package Gtk.Layered.Stream_IO is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   type Bit_Array is array (Positive range <>) of Boolean;

   --
   -- Restore -- Layers from stream
   --
   --    Stream - The stream to read from
   --    Widget - The widget to add layers on top of the widget
   --
   -- At the end of restoring you might wish to call Queue_Draw in order to
   -- make the changes visible.
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters of a layer
   --    I/O errors
   --
   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Widget : not null access Gtk_Layered_Record'Class);

   --
   -- Store -- A layered widget into stream
   --
   --    Stream - The stream to write into
   --    Widget - The widget which layers will be stored
   --
   -- Exceptions :
   --
   --    I/O errors

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Widget : not null access constant Gtk_Layered_Record'Class);

   ------------------------------------------------------------------------
   -- Restore -- A parameter from stream
   --
   --    Stream - The stream to read from
   --    Value  - The value to read
   --
   -- Exceptions :
   --
   --    I/O errors
   --
   procedure Restore
     (Stream     : in out Ada.Streams.Root_Stream_Type'Class;
      Adjustment : out Gtk_Adjustment);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Alignment);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Boolean);

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean);

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean);

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean);

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean);

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean;
      Value_6 : out Boolean);

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean;
      Value_6 : out Boolean;
      Value_7 : out Boolean);

   procedure Restore
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : out Boolean;
      Value_2 : out Boolean;
      Value_3 : out Boolean;
      Value_4 : out Boolean;
      Value_5 : out Boolean;
      Value_6 : out Boolean;
      Value_7 : out Boolean;
      Value_8 : out Boolean);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Font_Face);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Font_Slant);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Font_Weight);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Cairo_Line_Cap);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Ellipses.Cairo_Tuple);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Gdouble);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Elliptic_Arc_Closure);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Cairo.Ellipses.Ellipse_Parameters);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Elliptic_Shape_Type);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out End_Parameters);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Gdk.Color.Gdk_Color);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Gtk_Shadow_Type);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Guint);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Interpolation_Mode);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Line_Parameters);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Text_Transformation);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Tick_Parameters);

   function Restore
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return Bit_Array;

   function Restore
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return UTF8_String;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Vertical_Alignment);

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Waveform_Drawing_Method);

   ------------------------------------------------------------------------
   -- Store -- A parameter into stream
   --
   --    Stream - The stream to write into
   --    Value  - The value to write
   --
   -- Exceptions :
   --
   --    I/O errors

   procedure Store
     (Stream     : in out Ada.Streams.Root_Stream_Type'Class;
      Adjustment : Gtk_Adjustment);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Alignment);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Boolean);

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean);

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean);

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean);

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean);

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean;
      Value_6 : Boolean);

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean;
      Value_6 : Boolean;
      Value_7 : Boolean);

   procedure Store
     (Stream  : in out Ada.Streams.Root_Stream_Type'Class;
      Value_1 : Boolean;
      Value_2 : Boolean;
      Value_3 : Boolean;
      Value_4 : Boolean;
      Value_5 : Boolean;
      Value_6 : Boolean;
      Value_7 : Boolean;
      Value_8 : Boolean);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Cairo_Font_Face);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Cairo_Font_Slant);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Cairo_Font_Weight);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Cairo_Line_Cap);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Ellipses.Cairo_Tuple);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Gdouble);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Elliptic_Arc_Closure);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Cairo.Ellipses.Ellipse_Parameters);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Elliptic_Shape_Type);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : End_Parameters);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Gdk.Color.Gdk_Color);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Gtk_Shadow_Type);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Guint);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Interpolation_Mode);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Line_Parameters);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Text_Transformation);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Tick_Parameters);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Bit_Array);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : UTF8_String);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Vertical_Alignment);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Waveform_Drawing_Method);

   generic
      type Modular is mod <>;
   package Generic_Modular_IO is
      procedure Restore
        (Stream : in out Ada.Streams.Root_Stream_Type'Class;
         Value  : out Modular);

      procedure Store
        (Stream : in out Ada.Streams.Root_Stream_Type'Class;
         Value  : Modular);
   end Generic_Modular_IO;

private

   type Layer_Type is
     (None,
      Arc_Layer,
      Bar_Layer,
      Cache_Layer,
      Cap_Layer,
      Clock_Hand_Layer,
      Elliptic_Annotation_Layer,
      Elliptic_Background_Layer,
      Elliptic_Bar_Layer,
      Elliptic_Scale_Layer,
      Flat_Annotation_Layer,
      Flat_Needle_Layer,
      Flat_Scale_Layer,
      Foreground_Layer,
      Label_Layer,
      Needle_Layer,
      Rectangular_Background_Layer,
      Rectangular_Clip_Region_On_Layer,
      Rectangular_Clip_Region_Off_Layer,
      Sector_Needle_Layer);

   function Get_Type (Layer : Abstract_Layer'Class) return Layer_Type;

   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : out Layer_Type);

   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Layer_Type);

   pragma Warnings (On, "declaration hides ""Adjustment""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Layered.Stream_IO;
