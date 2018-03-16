--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.Gtk_Led_Rectangular                         Luebeck            --
--  Implementation                                 Summer, 2012       --
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

with Gdk.Color.IHLS;

with Glib.Object.Checked_Destroy;

package body Gtk.Gauge.LED_Rectangular is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   -- Background_Color : constant Gdk.Color.Gdk_Color := Gtk.Missed.RGB (0.5, 0.5, 0.5);

   Reflection_X1    : constant := -0.19;
   Reflection_X2    : constant := -0.06;
   Reflection_Y1    : constant := -0.20;
   Reflection_Y2    : constant := -0.24;
   Reflection_Width : constant :=  0.32;

   Shadow_X1    : constant := -0.32;
   Shadow_X2    : constant :=  0.30;
   Shadow_Y1    : constant :=  0.18;
   Shadow_Y2    : constant :=  0.18;
   Shadow_Width : constant :=  0.35;

   use type Gdk.Color.IHLS.Gdk_Luminance;

   Reflection_Shift : constant Gdk.Color.IHLS.Gdk_Luminance :=
                        Gdk.Color.IHLS.Gdk_Luminance'Last / 2;
   Shadow_Shift     : constant Gdk.Color.IHLS.Gdk_Luminance :=
                        Gdk.Color.IHLS.Gdk_Luminance'Last / 8;
   Corner           : constant := 1.0 / 10.0;

   protected Lock is
      function On_Color
        (LED : Gtk_Gauge_LED_Rectangular_Record'Class)
         return Gdk.Color.Gdk_Color;

      function Off_Color
        (LED : Gtk_Gauge_LED_Rectangular_Record'Class)
         return Gdk.Color.Gdk_Color;

      procedure Set
        (LED : in out Gtk_Gauge_LED_Rectangular_Record'Class;
         On  : Gdk.Color.Gdk_Color;
         Off : Gdk.Color.Gdk_Color);
   end Lock;

   protected body Lock  is
      function Off_Color
        (LED : Gtk_Gauge_LED_Rectangular_Record'Class)
         return Gdk.Color.Gdk_Color is
      begin
         return LED.Off;
      end Off_Color;

      function On_Color
        (LED : Gtk_Gauge_LED_Rectangular_Record'Class)
         return Gdk.Color.Gdk_Color is
      begin
         return LED.On;
      end On_Color;

      procedure Set
        (LED : in out Gtk_Gauge_LED_Rectangular_Record'Class;
         On  : Gdk.Color.Gdk_Color;
         Off : Gdk.Color.Gdk_Color)
      is
         use type Gdk.Color.Gdk_Color;
      begin
         if LED.On /= On or else LED.Off /= Off then
            LED.Toggled := True;
            LED.On  := On;
            LED.Off := Off;
         end if;
      end Set;
   end Lock;

   function Get_Background
     (Widget : not null access Gtk_Gauge_LED_Rectangular_Record)
      return not null access Gtk.Layered.Rectangular_Background.Rectangular_Background_Layer
   is
   begin
      return Widget.all.Background;
   end Get_Background;

   function Get_Cache
     (Widget : not null access Gtk_Gauge_LED_Rectangular_Record)
      return not null access Gtk.Layered.Cache.Cache_Layer is
   begin
      return Widget.all.Cache;
   end Get_Cache;

   function Get_Off_Color
     (Widget : not null access Gtk_Gauge_LED_Rectangular_Record)
      return Gdk.Color.Gdk_Color is
   begin
      return Lock.Off_Color (Widget.all);
   end Get_Off_Color;

   function Get_On_Color
     (Widget : not null access Gtk_Gauge_LED_Rectangular_Record)
      return Gdk.Color.Gdk_Color is
   begin
      return Lock.On_Color (Widget.all);
   end Get_On_Color;

   function Get_State
     (Widget : not null access Gtk_Gauge_LED_Rectangular_Record) return Boolean
   is
   begin
      return Widget.all.State;
   end Get_State;

   function Get_Type return GType is
   begin
      if
        Initialize_Class_Record
          (Ancestor     => Gtk.Layered.Get_Type,
           Class_Record => Class_Record'Access,
           Type_Name    => Class_Name)
      then
         null;
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New
     (Widget        : out Gtk_Gauge_LED_Rectangular;
      On_Color      : Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (0.0, 1.0, 0.0);
      Off_Color     : Gdk.Color.Gdk_Color       := Gtk.Missed.RGB (0.5, 0.5, 0.5);
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type := Gtk.Enums.Shadow_In) is
   begin
      Widget := new Gtk_Gauge_LED_Rectangular_Record;
      Initialize (Widget, On_Color, Off_Color, Border_Shadow);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget        : not null access Gtk_Gauge_LED_Rectangular_Record'Class;
      On_Color      : Gdk.Color.Gdk_Color;
      Off_Color     : Gdk.Color.Gdk_Color;
      Border_Shadow : Gtk.Enums.Gtk_Shadow_Type) is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Lock.Set (Widget.all, On_Color, Off_Color);
      case Border_Shadow is
         when Gtk.Enums.Shadow_None =>
            Widget.all.Background :=
              Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
                (Under         => Widget,
                 Height        => 1.0,
                 Width         => 1.0,
                 Center        => (0.0, 0.0),
                 Corner_Radius => Corner,
                 Color         => Off_Color,
                 Border_Width  => 0.0,
                 Border_Depth  => 0.0,
                 Border_Shadow => Border_Shadow,
                 Deepened      => True,
                 Widened       => True,
                 Scaled        => True);
         when Gtk.Enums.Shadow_Etched_In | Gtk.Enums.Shadow_Etched_Out =>
            Widget.all.Background :=
              Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
                (Under         => Widget,
                 Height        => 1.0,
                 Width         => 1.0,
                 Center        => (0.0, 0.0),
                 Corner_Radius => Corner,
                 Color         => Off_Color,
                 Border_Width  => 0.025,
                 Border_Depth  => 0.075,
                 Border_Shadow => Border_Shadow,
                 Deepened      => True,
                 Widened       => True,
                 Scaled        => True);
         when Gtk.Enums.Shadow_In | Gtk.Enums.Shadow_Out =>
            Widget.all.Background :=
              Gtk.Layered.Rectangular_Background.Add_Rectangular_Background
                (Under         => Widget,
                 Height        => 1.0,
                 Width         => 1.0,
                 Center        => (0.0, 0.0),
                 Corner_Radius => Corner,
                 Color         => Off_Color,
                 Border_Width  => 0.05,
                 Border_Depth  => 0.15,
                 Border_Shadow => Border_Shadow,
                 Deepened      => True,
                 Widened       => True,
                 Scaled        => True);
      end case;
      Widget.all.Shadow :=
        Gtk.Layered.Line.Add_Line
          (Under    => Widget.all.Background.all.Get_Foreground,
           From     => (Shadow_X1, Shadow_Y1),
           To       => (Shadow_X2, Shadow_Y2),
           Width    => Shadow_Width,
           Line_Cap => Cairo.Cairo_Line_Cap_Round,
           Scaled   => True,
           Widened  => True,
           Color    =>
             Gdk.Color.IHLS.Darken
               (Lock.Off_Color (Widget.all),
                Shadow_Shift));
      Widget.all.Reflection :=
        Gtk.Layered.Line.Add_Line
          (Under    => Widget.all.Background.all.Get_Foreground,
           From     => (Reflection_X1, Reflection_Y1),
           To       => (Reflection_X2, Reflection_Y2),
           Width    => Reflection_Width,
           Line_Cap => Cairo.Cairo_Line_Cap_Round,
           Scaled   => True,
           Widened  => True,
           Color    => Lock.Off_Color (Widget.all));
      Widget.all.Cache :=
        Gtk.Layered.Cache.Add_Cache (Widget.all.Background.all.Get_Foreground);
      Widget.all.Update_State;
   end Initialize;

   overriding procedure Refresh
     (Widget  : not null access Gtk_Gauge_LED_Rectangular_Record;
      Context : Cairo.Cairo_Context) is
   begin
      if Widget.all.Toggled then
         Widget.all.Update_State;
      end if;
      Gtk.Layered.Gtk_Layered_Record (Widget.all).Refresh (Context);
   end Refresh;

   procedure Set_Colors
     (Widget    : not null access Gtk_Gauge_LED_Rectangular_Record;
      On_Color  : Gdk.Color.Gdk_Color;
      Off_Color : Gdk.Color.Gdk_Color) is
   begin
      Lock.Set (Widget.all, On_Color, Off_Color);
   end Set_Colors;

   procedure Set_State
     (Widget : not null access Gtk_Gauge_LED_Rectangular_Record;
      State  : Boolean) is
   begin
      if Widget.all.State /= State then
         Widget.all.State   := State;
         Widget.all.Toggled := True;
      end if;
   end Set_State;

   procedure Update_State
     (Widget : not null access Gtk_Gauge_LED_Rectangular_Record)
   is
      Colors : array (1 .. 3) of Gdk.Color.Gdk_Color;
   begin
      if Widget.all.State then
         Colors (1) := Lock.On_Color (Widget.all);
         Colors (2) := Gdk.Color.IHLS.Lighten (Colors (1), Reflection_Shift, True);
         Colors (3) := Gdk.Color.IHLS.Darken  (Colors (1), Shadow_Shift);
      else
         Colors (1) := Lock.Off_Color (Widget.all);
         Colors (2) := Colors (1);
         Colors (3) := Gdk.Color.IHLS.Darken (Colors (1), Shadow_Shift);
      end if;
      Widget.all.Background.all.Set
        (Height         => Widget.all.Background.all.Get_Height,
         Width          => Widget.all.Background.all.Get_Width,
         Center         => Widget.all.Background.all.Get_Center,
         Rotation_Angle => Widget.all.Background.all.Get_Rotation_Angle,
         Corner_Radius  => Widget.all.Background.all.Get_Corner_Radius,
         Border_Width   => Widget.all.Background.all.Get_Border_Width,
         Border_Depth   => Widget.all.Background.all.Get_Border_Depth,
         Border_Color   => Widget.all.Background.all.Get_Border_Color,
         Border_Shadow  => Widget.all.Background.all.Get_Border_Shadow,
         Color          => Colors (1));
      Widget.all.Reflection.all.Set
        (From => Widget.all.Reflection.all.Get_From,
         To   => Widget.all.Reflection.all.Get_To,
         Line => (Width    => Widget.all.Reflection.all.Get_Line.Width,
                  Line_Cap => Widget.all.Reflection.all.Get_Line.Line_Cap,
                  Color    => Colors (2)));
      Widget.all.Shadow.all.Set
        (From => Widget.all.Shadow.all.Get_From,
         To   => Widget.all.Shadow.all.Get_To,
         Line => (Width    => Widget.all.Shadow.all.Get_Line.Width,
                  Line_Cap => Widget.all.Shadow.all.Get_Line.Line_Cap,
                  Color    => Colors (3)));
   end Update_State;

end Gtk.Gauge.LED_Rectangular;
