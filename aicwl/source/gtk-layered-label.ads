--                                                                    --
--  package Gtk.Layered.Label       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2010       --
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

with Ada.Numerics;

with Gtk.Missed;

with Pango.Cairo.Fonts;

package Gtk.Layered.Label is

   --
   -- Label_Layer -- A layer consisting of  texts  drawn  at the  positions
   --                relative to an elliptic arc
   --
   type Label_Layer (<>) is
     new Abstract_Layer and Scalable_Layer with private;
   --
   -- Add_Label -- Add label text
   --
   --    Under    - The layer or widget where to place the label under
   --    Text     - The text
   --    Location - Text location
   --    Font     - The text font
   --    Height   - The text height
   --    Stretch  - The text width scale relative to its height
   --    Mode     - The way a text is transformed when drawn
   --    Color    - The text color
   --    Angle    - The rotation angle
   --    Skew     - The skew angle
   --    Markup   - The text uses pango markup
   --    Scaled   - The layer is scaled together with the parent widget
   --
   -- Angle is ignored when  Mode  is  Moved_Centered.  When  the  mode  is
   -- Moved_Inside  and  Moved_Outside  then  Angle  influences  the   text
   -- alignment.  When  the  mode  is  Rotated,  Angle  is the angle of the
   -- horizontal text axis. When Mode is Skewed, Angle is the angle of  the
   -- horizontal  text  axis  and  Skew is the angle between the horizontal
   -- text axis and the vertical axis of the text origin.  When  Scaled  is
   -- true the text is scaled to fit the  parent  widget.  The  scaling  is
   -- performed as follows:
   --
   -- (o)  The  text  center's  X  is  multiplied  by the widget's size and
   --      placed in the coorinate system centered in the widget's center;
   -- (o)  The  text  center's  Y  is  multiplied  by the widget's size and
   --      placed in the coorinate system centered in the widget's center;
   -- (o)  The text is scaled by multiplication by the widget's size.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Label
     (Under    : not null access Layer_Location'Class;
      Text     : UTF8_String                        := "";
      Location : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Face     : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "sans",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height   : Gdouble                            := 12.0;
      Stretch  : Gdouble                            := 1.0;
      Mode     : Text_Transformation                := Moved_Centered;
      Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle    : Gdouble                            := 3.0 * Ada.Numerics.Pi / 2.0;
      Skew     : Gdouble                            := 0.0;
      Markup   : Boolean                            := False;
      Scaled   : Boolean                            := False);

   function Add_Label
     (Under    : not null access Layer_Location'Class;
      Text     : UTF8_String                        := "";
      Location : Cairo.Ellipses.Cairo_Tuple         := (0.0, 0.0);
      Face     : Pango.Cairo.Fonts.Pango_Cairo_Font :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "sans",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height   : Gdouble                            := 12.0;
      Stretch  : Gdouble                            := 1.0;
      Mode     : Text_Transformation                := Moved_Centered;
      Color    : Gdk.Color.Gdk_Color                := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle    : Gdouble                            := 3.0 * Ada.Numerics.Pi / 2.0;
      Skew     : Gdouble                            := 0.0;
      Markup   : Boolean                            := False;
      Scaled   : Boolean                            := False)
      return not null access Label_Layer;

   --
   -- Get_Angle -- The angle to the horizontal text axis
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The angle to the horizontal text axis (when rotated or skewed)
   --
   function Get_Angle (Layer : Label_Layer) return Gdouble;

   --
   -- Get_Color -- The text color
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The text color
   --
   function Get_Color (Layer : Label_Layer) return Gdk.Color.Gdk_Color;

   --
   -- Get_Face -- Text font face
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The font face
   --
   function Get_Face (Layer : Label_Layer)
                      return Pango.Cairo.Fonts.Pango_Cairo_Font;

   --
   -- Get_Height -- The text height
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The text height
   --
   function Get_Height (Layer : Label_Layer) return Gdouble;

   --
   -- Get_Location -- The text location
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The location of the text
   --
   function Get_Location
     (Layer : Label_Layer) return Cairo.Ellipses.Cairo_Tuple;

   --
   -- Get_Markup -- The text markup
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    True if the text uses markup
   --
   function Get_Markup (Layer : Label_Layer) return Boolean;

   --
   -- Get_Mode -- The text transformation mode
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The mode the text is transformed when drawn
   --
   function Get_Mode (Layer : Label_Layer) return Text_Transformation;

   --
   -- Get_Stretch -- The text stretch
   --
   --    Layer - The label layer
   --
   -- The text stretch is how the text width should be scaled relatively to
   -- its height. For example, 2.0 means twice as wide than normal.
   --
   -- Returns :
   --
   --    The text stretch
   --
   function Get_Stretch (Layer : Label_Layer) return Gdouble;

   --
   -- Get_Skew -- The skew angle
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The skew angle
   --
   function Get_Skew (Layer : Label_Layer) return Gdouble;

   --
   -- Get_Text -- The text text
   --
   --    Layer - The label layer
   --
   -- Returns :
   --
   --    The text
   --
   -- Implementation note :
   --
   --    The function Get_Text is  task-safe,  it  can  be  called  on  the
   --    context of a task different from the GTK loop task.
   --
   function Get_Text (Layer : Label_Layer) return UTF8_String;

   --
   -- Set -- Parameters of the label
   --
   --    Layer    - The label layer
   --    Location - Text location
   --    Face     - The text font
   --    Height   - The text height
   --    Stretch  - The text stretch
   --    Mode     - The way a text is transformed when drawn at its tick
   --    Color    - The text color
   --    Angle    - Of the vertical text axis
   --    Skew     - The angle to the horizontal axis
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Set
     (Layer    : in out Label_Layer;
      Location : Cairo.Ellipses.Cairo_Tuple;
      Face     : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height   : Gdouble;
      Stretch  : Gdouble;
      Mode     : Text_Transformation;
      Color    : Gdk.Color.Gdk_Color;
      Angle    : Gdouble;
      Skew     : Gdouble);

   --
   -- Set_Text -- Set text
   --
   --    Layer  - The label layer
   --    Text   - The new text
   --    Markup - True if the text uses pango markup
   --
   -- Implementation note :
   --
   --    The  procedure  Set_Text  is  task-safe,  it  can be called on the
   --    context  of  a  task  different  from the GTK loop task. Note that
   --    changing  the  text has no immediate effect on the layer's widget.
   --    In order to redraw it you should call QueueDraw.
   --
   procedure Set_Text
     (Layer  : in out Label_Layer;
      Text   : UTF8_String;
      Markup : Boolean := False);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Label_Layer;

   overriding procedure Draw
     (Layer   : in out Label_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle);

   overriding procedure Finalize (Layer : in out Label_Layer);

   overriding function Get_Properties_Number
     (Layer : Label_Layer) return Natural;

   overriding function Get_Property_Specification
     (Layer    : Label_Layer;
      Property : Positive) return Param_Spec;

   overriding function Get_Property_Value
     (Layer    : Label_Layer;
      Property : Positive) return Glib.Values.GValue;

   overriding function Get_Scaled (Layer : Label_Layer) return Boolean;

   overriding function Is_Updated (Layer : Label_Layer) return Boolean;

   overriding procedure Move
     (Layer  : in out Label_Layer;
      Offset : Cairo.Ellipses.Cairo_Tuple);

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Label_Layer);

   overriding procedure Scale
     (Layer  : in out Label_Layer;
      Factor : Gdouble);

   overriding procedure Set_Property_Value
     (Layer    : in out Label_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue);

   overriding procedure Set_Scaled
     (Layer  : in out Label_Layer;
      Scaled : Boolean);

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Label_Layer);

private

   type Label_Layer is
     new Abstract_Layer and Scalable_Layer with
      record
         X, Y    : Gdouble;
         Face    : Pango.Cairo.Fonts.Pango_Cairo_Font;
         Height  : Gdouble;
         Stretch : Gdouble;
         Mode    : Text_Transformation;
         Color   : Gdk.Color.Gdk_Color;
         Angle   : Gdouble;
         Skew    : Gdouble;
         Text    : String_Ptr;
         Markup  : Boolean := False;
         Scaled  : Boolean := False;
         Updated : Boolean := True;
         pragma Atomic (Markup);
         pragma Atomic (Updated);
      end record;

end Gtk.Layered.Label;
