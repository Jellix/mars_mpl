--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Gauge.LED_Round                         Luebeck            --
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
--____________________________________________________________________--

with Ada.Numerics;              use Ada.Numerics;
with Gdk.Color.IHLS;            use Gdk.Color.IHLS;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GtkAda.Types;              use GtkAda.Types;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Interfaces.C;              use Interfaces.C;

with GLib.Object.Checked_Destroy;

package body Gtk.Gauge.LED_Round is
   use Gtk.Enums;
   use Gtk.Missed;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Background_Color : constant Gdk_Color := RGB (0.5, 0.5, 0.5);

   Reflection_X1    : constant := -0.15;
   Reflection_X2    : constant := -0.15;
   Reflection_Y1    : constant := -0.15;
   Reflection_Y2    : constant := -0.15;
   Reflection_Width : constant :=  0.4;

   Shadow_X1    : constant := 0.05;
   Shadow_X2    : constant := 0.24;
   Shadow_Y1    : constant := 0.24;
   Shadow_Y2    : constant := 0.05;
   Shadow_Width : constant := 0.4;

   Reflection_Shift : constant Gdk_Luminance := Gdk_Luminance'Last / 2;
   Shadow_Shift     : constant Gdk_Luminance := Gdk_Luminance'Last / 8;

   protected Lock is
      function On_Color
               (  LED : Gtk_Gauge_LED_Round_Record'Class
               )  return Gdk_Color;
      function Off_Color
               (  LED : Gtk_Gauge_LED_Round_Record'Class
               )  return Gdk_Color;
      procedure Set
                (  LED : in out Gtk_Gauge_LED_Round_Record'Class;
                   On  : Gdk_Color;
                   Off : Gdk_Color
                );
   end Lock;

   protected body Lock  is
      function On_Color
               (  LED : Gtk_Gauge_LED_Round_Record'Class
               )  return Gdk_Color is
      begin
         return LED.On;
      end On_Color;

      function Off_Color
               (  LED : Gtk_Gauge_LED_Round_Record'Class
               )  return Gdk_Color is
      begin
         return LED.Off;
      end Off_Color;

      procedure Set
                (  LED : in out Gtk_Gauge_LED_Round_Record'Class;
                   On  : Gdk_Color;
                   Off : Gdk_Color
                )  is
      begin
         if LED.On /= On or else LED.Off /= Off then
            LED.Toggled := True;
            LED.On  := On;
            LED.Off := Off;
         end if;
      end Set;
   end Lock;

   function Get_Background
            (  Widget : not null access Gtk_Gauge_LED_Round_Record
            )  return not null access Elliptic_Background_Layer is
   begin
      return Widget.Background;
   end Get_Background;

   function Get_Cache
            (  Widget : not null access Gtk_Gauge_LED_Round_Record
            )  return not null access Cache_Layer is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Off_Color
            (  Widget : not null access Gtk_Gauge_LED_Round_Record
            )  return Gdk_Color is
   begin
      return Lock.Off_Color (Widget.all);
   end Get_Off_Color;

   function Get_On_Color
            (  Widget : not null access Gtk_Gauge_LED_Round_Record
            )  return Gdk_Color is
   begin
      return Lock.On_Color (Widget.all);
   end Get_On_Color;

   function Get_State
            (  Widget : not null access Gtk_Gauge_LED_Round_Record
            )  return Boolean is
   begin
      return Widget.State;
   end Get_State;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Layered.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         null;
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget        : out Gtk_Gauge_LED_Round;
                On_Color      : Gdk_Color := RGB (0.0, 1.0, 0.0);
                Off_Color     : Gdk_Color := RGB (0.5, 0.5, 0.5);
                Border_Shadow : Gtk_Shadow_Type := Shadow_In
             )  is
   begin
      Widget := new Gtk_Gauge_LED_Round_Record;
      Initialize (Widget, On_Color, Off_Color, Border_Shadow);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget        : not null access
                                Gtk_Gauge_LED_Round_Record'Class;
                On_Color      : Gdk_Color;
                Off_Color     : Gdk_Color;
                Border_Shadow : Gtk_Shadow_Type
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Layered.Initialize (Widget);
      Lock.Set (Widget.all, On_Color, Off_Color);
      case Border_Shadow is
         when Shadow_None =>
            Widget.Background :=
               Add_Elliptic_Background
               (  Under         => Widget,
                  Color         => Off_Color,
                  Outer         => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
                  Border_Width  => 0.0,
                  Border_Depth  => 0.0,
                  Border_Shadow => Border_Shadow,
                  Deepened      => True,
                  Widened       => True,
                  Scaled        => True
               );
         when Shadow_Etched_In | Shadow_Etched_Out =>
            Widget.Background :=
               Add_Elliptic_Background
               (  Under         => Widget,
                  Color         => Off_Color,
                  Outer         => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
                  Border_Width  => 0.025,
                  Border_Depth  => 0.075,
                  Border_Shadow => Border_Shadow,
                  Deepened      => True,
                  Widened       => True,
                  Scaled        => True
               );
         when Shadow_In | Shadow_Out =>
            Widget.Background :=
               Add_Elliptic_Background
               (  Under         => Widget,
                  Color         => Off_Color,
                  Outer         => ((0.0, 0.0), 1.0 / 0.5, 0.5, 0.0),
                  Border_Width  => 0.05,
                  Border_Depth  => 0.15,
                  Border_Shadow => Border_Shadow,
                  Deepened      => True,
                  Widened       => True,
                  Scaled        => True
               );
       end case;
       Widget.Shadow :=
          Add_Line
          (  Under    => Widget.Background.Get_Foreground,
             From     => (Shadow_X1, Shadow_Y1),
             To       => (Shadow_X2, Shadow_Y2),
             Width    => Shadow_Width,
             Line_Cap => CAIRO_LINE_CAP_ROUND,
             Scaled   => True,
             Widened  => True,
             Color    => Darken
                         (  Lock.Off_Color (Widget.all),
                            Shadow_Shift
          )              );
       Widget.Reflection :=
          Add_Line
          (  Under    => Widget.Background.Get_Foreground,
             From     => (Reflection_X1, Reflection_Y1),
             To       => (Reflection_X2, Reflection_Y2),
             Width    => Reflection_Width,
             Line_Cap => CAIRO_LINE_CAP_ROUND,
             Scaled   => True,
             Widened  => True,
             Color    => Lock.Off_Color (Widget.all)
          );
       Widget.Cache := Add_Cache (Widget.Background.Get_Foreground);
       Widget.Update_State;
   end Initialize;

   procedure Refresh
             (  Widget  : not null access Gtk_Gauge_LED_Round_Record;
                Context : Cairo_Context
             )  is
   begin
      if Widget.Toggled then
         Widget.Update_State;
      end if;
      Gtk_Layered_Record (Widget.all).Refresh (Context);
   end Refresh;

   procedure Set_Colors
             (  Widget    : not null access Gtk_Gauge_LED_Round_Record;
                On_Color  : Gdk_Color;
                Off_Color : Gdk_Color
             )  is
   begin
      Lock.Set (Widget.all, On_Color, Off_Color);
   end Set_Colors;

   procedure Set_State
             (  Widget : not null access Gtk_Gauge_LED_Round_Record;
                State  : Boolean
             )  is
   begin
      if Widget.State /= State then
         Widget.State   := State;
         Widget.Toggled := True;
      end if;
   end Set_State;

   procedure Update_State
             (  Widget : not null access Gtk_Gauge_LED_Round_Record
             )  is
      Colors : array (1..3) of Gdk_Color;
   begin
      if Widget.State then
         Colors (1) := Lock.On_Color (Widget.all);
         Colors (2) := Lighten (Colors (1), Reflection_Shift, True);
         Colors (3) := Darken  (Colors (1), Shadow_Shift);
      else
         Colors (1) := Lock.Off_Color (Widget.all);
         Colors (2) := Colors (1);
         Colors (3) := Darken (Colors (1), Shadow_Shift);
      end if;
      Widget.Background.Set
      (  Outer         => Widget.Background.Get_Outer,
         Inner         => Widget.Background.Get_Inner,
         From          => Widget.Background.Get_From,
         Length        => Widget.Background.Get_Length,
         Border_Width  => Widget.Background.Get_Border_Width,
         Border_Depth  => Widget.Background.Get_Border_Depth,
         Border_Color  => Widget.Background.Get_Border_Color,
         Border_Shadow => Widget.Background.Get_Border_Shadow,
         Color         => Colors (1)
      );
      Widget.Reflection.Set
      (  From => Widget.Reflection.Get_From,
         To   => Widget.Reflection.Get_To,
         Line => (  Width    => Widget.Reflection.Get_Line.Width,
                    Line_Cap => Widget.Reflection.Get_Line.Line_Cap,
                    Color    => Colors (2)
      )          );
      Widget.Shadow.Set
      (  From => Widget.Shadow.Get_From,
         To   => Widget.Shadow.Get_To,
         Line => (  Width    => Widget.Shadow.Get_Line.Width,
                    Line_Cap => Widget.Shadow.Get_Line.Line_Cap,
                    Color    => Colors (3)
      )          );
   end Update_State;

end Gtk.Gauge.LED_Round;
