--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Label.Digital                   Luebeck            --
--  Interface                                      Summer, 2012       --
--                                                                    --
--                                Last revision :  09:08 27 Jun 2015  --
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

with Gtk.Handlers;

with Pango.Cairo.Fonts;

with Strings_Edit;

package Gtk.Layered.Label.Digital is

   pragma Warnings (Off, "declaration hides ""Adjustment""");

   --
   -- Digital_Layer -- A layer rendering value as a text
   --
   type Digital_Layer (<>) is new Label_Layer with private;
   --
   -- Add_Digital -- Add digital layer
   --
   --    Under      - The layer or widget where to place the label under
   --    Location   - Text location
   --    Font       - The text font
   --    Height     - The text height
   --    Stretch    - The text width scale relative to its height
   --    Mode       - The way a text is transformed when drawn
   --    Color      - The text color
   --    Angle      - The rotation angle
   --    Skew       - The skew angle
   --    Adjustment - The value source
   --    Base       - The base used for the value output
   --    Precision  - The precision of the value output
   --    Absolute   - True if precision is absolute, otherwise relative
   --    Put_Plus   - The plus should placed for positive numbers
   --    Scaled     - The layer is scaled together with the parent widget
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
   -- (o)  The text is scaled by multiplication by the widget;s size.
   --
   -- Returns :
   --
   --    The layer (optional)
   --
   -- Exceptions :
   --
   --    Constraint_Error - Wrong parameters
   --
   procedure Add_Digital
     (Under      : not null access Layer_Location'Class;
      Location   : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      Face       : Pango.Cairo.Fonts.Pango_Cairo_Font                :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "sans",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height     : Gdouble                                           := 12.0;
      Stretch    : Gdouble                                           := 1.0;
      Mode       : Text_Transformation                               := Rotated;
      Color      : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle      : Gdouble                                           := 0.0;
      Skew       : Gdouble                                           := 0.0;
      Base       : Strings_Edit.NumberBase                           := 10;
      Precision  : Integer                                           := 0;
      Absolute   : Boolean                                           := True;
      Put_Plus   : Boolean                                           := False;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean                                           := False);

   function Add_Digital
     (Under      : not null access Layer_Location'Class;
      Location   : Cairo.Ellipses.Cairo_Tuple                        := (0.0, 0.0);
      Face       : Pango.Cairo.Fonts.Pango_Cairo_Font                :=
        Pango.Cairo.Fonts.Create_Toy
          (Family => "sans",
           Slant  => Cairo.Cairo_Font_Slant_Normal,
           Weight => Cairo.Cairo_Font_Weight_Normal);
      Height     : Gdouble                                           := 12.0;
      Stretch    : Gdouble                                           := 1.0;
      Mode       : Text_Transformation                               := Rotated;
      Color      : Gdk.Color.Gdk_Color                               := Gtk.Missed.RGB (0.0, 0.0, 0.0);
      Angle      : Gdouble                                           := 0.0;
      Skew       : Gdouble                                           := 0.0;
      Base       : Strings_Edit.NumberBase                           := 10;
      Precision  : Integer                                           := 0;
      Absolute   : Boolean                                           := True;
      Put_Plus   : Boolean                                           := False;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean                                           := False)
      return not null access Digital_Layer;
   --
   -- Get_Absolute -- Value precision type
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    True if precision is absolute
   --
   function Get_Absolute (Layer : Digital_Layer) return Boolean;

   --
   -- Get_Adjustment -- The adjustment object used by the widget
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The adjustment object or else null
   --
   function Get_Adjustment (Layer : Digital_Layer)
                            return Gtk.Adjustment.Gtk_Adjustment;

   --
   -- Get_Angle -- The angle to the horizontal text axis
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The angle to the horizontal text axis (when rotated or skewed)
   --
   overriding function Get_Angle (Layer : Digital_Layer) return Gdouble;

   --
   -- Get_Base -- The base used for rendering values
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The base
   --
   function Get_Base (Layer : Digital_Layer) return Strings_Edit.NumberBase;

   --
   -- Get_Color -- The text color
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The text color
   --
   overriding function Get_Color (Layer : Digital_Layer) return Gdk.Color.Gdk_Color;

   --
   -- Get_Face -- Text font face
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The font face
   --
   overriding function Get_Face (Layer : Digital_Layer)
                                 return Pango.Cairo.Fonts.Pango_Cairo_Font;

   --
   -- Get_Height -- The text height
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The text height
   --
   overriding function Get_Height (Layer : Digital_Layer) return Gdouble;

   --
   -- Get_Location -- The text location
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The location of the text
   --
   overriding function Get_Location
     (Layer : Digital_Layer) return Cairo.Ellipses.Cairo_Tuple;

   --
   -- Get_Mode -- The text transformation mode
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The mode the text is transformed when drawn
   --
   overriding function Get_Mode (Layer : Digital_Layer) return Text_Transformation;

   --
   -- Get_Precision -- Get output precision
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The output precision
   --
   function Get_Precision (Layer : Digital_Layer) return Integer;

   --
   -- Get_Put_Plus -- Get policy regarding sign
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    True when + to be output
   --
   function Get_Put_Plus (Layer : Digital_Layer) return Boolean;

   --
   -- Get_Stretch -- The text stretch
   --
   --    Layer - The digital layer
   --
   -- The text stretch is how the text width should be scaled relatively to
   -- its height. For example, 2.0 means twice as wide than normal.
   --
   -- Returns :
   --
   --    The text stretch
   --
   overriding function Get_Stretch (Layer : Digital_Layer) return Gdouble;

   --
   -- Get_Skew -- The skew angle
   --
   --    Layer - The digital layer
   --
   -- Returns :
   --
   --    The skew angle
   --
   overriding function Get_Skew (Layer : Digital_Layer) return Gdouble;

   --
   -- Get_Text -- The indicated text
   --
   --    Layer - The digital layer
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
   overriding function Get_Text (Layer : Digital_Layer) return UTF8_String;

   --
   -- Get_Value -- Get the value indicated
   --
   --    Layer - The digital layer
   --
   -- The implementation  is task-safe,  callable on the context  of a task
   -- different from the GTK loop task.
   --
   -- Returns :
   --
   --    The value
   --
   function Get_Value (Layer : Digital_Layer) return Gdouble;

   --
   -- Render -- The value renderer
   --
   --    Layer - The digital layer
   --    Value - The value to render
   --
   -- This function is used to obtain the text indicated by the layer.  The
   -- function can be overridden to change the layer's behavior.
   --
   -- Returns :
   --
   --     The text to indicate
   --
   function Render (Layer : Digital_Layer; Value : Gdouble)
                    return UTF8_String;

   --
   -- Set -- Parameters of the digital layer
   --
   --    Layer     - The label layer
   --    Location  - Text location
   --    Face      - The text font
   --    Height    - The text height
   --    Stretch   - The text stretch
   --    Mode      - The way a text is transformed when drawn at its tick
   --    Color     - The text color
   --    Angle     - Of the vertical text axis
   --    Skew      - The angle to the horizontal axis
   --    Base      - The base used for the value output
   --    Precision - The precision of the value output
   --    Absolute  - True if precision is absolute, otherwise relative
   --    Put_Plus  - The plus should placed for positive numbers
   --    Renderer  - The renderer of the values
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
     (Layer     : in out Digital_Layer;
      Location  : Cairo.Ellipses.Cairo_Tuple;
      Face      : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height    : Gdouble;
      Stretch   : Gdouble;
      Mode      : Text_Transformation;
      Color     : Gdk.Color.Gdk_Color;
      Angle     : Gdouble;
      Skew      : Gdouble;
      Base      : Strings_Edit.NumberBase;
      Precision : Integer;
      Absolute  : Boolean;
      Put_Plus  : Boolean);

   --
   -- Set_Value -- Change the value indicated
   --
   --    Layer - The digital layer
   --    Value - The value
   --
   -- The implementation must be task-safe, callable on the  context  of  a
   -- task different from the GTK loop task.
   --
   procedure Set_Value
     (Layer : in out Digital_Layer;
      Value : Gdouble);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Digital_Layer;

   overriding procedure Finalize (Layer : in out Digital_Layer);

   overriding function Get_Properties_Number
     (Layer : Digital_Layer) return Natural;

   overriding function Get_Property_Specification
     (Layer    : Digital_Layer;
      Property : Positive) return Param_Spec;

   overriding function Get_Property_Value
     (Layer    : Digital_Layer;
      Property : Positive) return Glib.Values.GValue;

   overriding procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Digital_Layer);

   overriding procedure Set_Property_Value
     (Layer    : in out Digital_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue);

   overriding procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Digital_Layer);

private

   type Digital_Layer is new Label_Layer with
      record
         Value         : Gdouble := 0.0;
         Adjustment    : Gtk.Adjustment.Gtk_Adjustment;
         Changed       : Gtk.Handlers.Handler_Id;
         Value_Changed : Gtk.Handlers.Handler_Id;
         Base          : Strings_Edit.NumberBase;
         Precision     : Integer;
         Absolute      : Boolean;
         Put_Plus      : Boolean;
         pragma Atomic (Value);
      end record;

   overriding function Add_Label
     (Under    : not null access Layer_Location'Class;
      Text     : UTF8_String;
      Location : Cairo.Ellipses.Cairo_Tuple;
      Face     : Pango.Cairo.Fonts.Pango_Cairo_Font;
      Height   : Gdouble;
      Stretch  : Gdouble;
      Mode     : Text_Transformation;
      Color    : Gdk.Color.Gdk_Color;
      Angle    : Gdouble;
      Skew     : Gdouble;
      Markup   : Boolean;
      Scaled   : Boolean) return not null access Digital_Layer;

   pragma Warnings (On, "declaration hides ""Adjustment""");

end Gtk.Layered.Label.Digital;
