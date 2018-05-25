--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Disk_Needle                   Luebeck              --
--  Implementation                                 Winter, 2011       --
--                                                                    --
--                                Last revision :  19:07 02 Jan 2018  --
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
with Glib.Properties.Creation;
with Gtk.Layered.Stream_IO;

package body Gtk.Layered.Disk_Needle is

   pragma Warnings (Off, "declaration hides ""Adjustment""");
   pragma Warnings (Off, "declaration hides ""Center""");
   pragma Warnings (Off, "declaration hides ""Handlers""");
   pragma Warnings (Off, "atomic synchronization set");

   type Needle_Ptr is access all Disk_Needle_Layer;

   type Layer_Property is
     (Property_Scaled,
      Property_Center_X,
      Property_Center_Y,
      Property_Radius,
      Property_From,
      Property_Length,
      Property_Sectors,
      Property_On_Color,
      Property_Off_Color,
      Property_Value);

   package Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, Needle_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Disk_Needle_Layer, Needle_Ptr);

   --
   procedure Add_Adjustment
     (Layer      : in out Disk_Needle_Layer;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --

   procedure Changed (Adjustment : access GObject_Record'Class;
                      Needle     : Needle_Ptr);

   overriding
   function Add (Under  : not null access Layer_Location'Class;
                 Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                 return not null access Disk_Needle_Layer
   is
      Ptr : Needle_Ptr := new Disk_Needle_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Adjustment
     (Layer      : in out Disk_Needle_Layer;
      Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
   begin
      Gtk.Adjustment.Ref (Adjustment);
      Layer.Adjustment := Adjustment.all'Unchecked_Access;
      Layer.Changed :=
        Handlers.Connect
          (Adjustment,
           "changed",
           Handlers.To_Marshaller (Changed'Access),
           Layer'Unchecked_Access);
      Layer.Value_Changed :=
        Handlers.Connect
          (Adjustment,
           "value_changed",
           Handlers.To_Marshaller (Changed'Access),
           Layer'Unchecked_Access);
      declare
         Lower : constant Gdouble := Adjustment.all.Get_Lower;
         Upper : constant Gdouble := Adjustment.all.Get_Upper;
         Value : constant Gdouble := Adjustment.all.Get_Value;
      begin
         if Upper <= Lower or else Value <= Lower then
            Layer.Set_Value (0.0);
         elsif Value >= Upper then
            Layer.Set_Value (1.0);
         else
            Layer.Set_Value ((Value - Lower) / (Upper - Lower));
         end if;
      end;
   end Add_Adjustment;

   procedure Add_Disk_Needle
     (Under      : not null access Layer_Location'Class;
      Center     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      Radius     : Gdouble     := 0.5;
      From       : Gdouble     := Pi;
      Length     : Gdouble     := Pi;
      Sectors    : Boolean     := False;
      On_Color   : Gdk.Color.Gdk_Color   := Needle_On_Color;
      Off_Color  : Gdk.Color.Gdk_Color   := Needle_Off_Color;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean     := False)
   is
      Ptr   : Needle_Ptr := new Disk_Needle_Layer;
      Layer : Disk_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
        (Layer     => Layer,
         Center    => Center,
         Radius    => Radius,
         From      => From,
         Length    => Length,
         Sectors   => Sectors,
         On_Color  => On_Color,
         Off_Color => Off_Color);
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Disk_Needle;

   function Add_Disk_Needle
     (Under      : not null access Layer_Location'Class;
      Center     : Cairo.Ellipses.Cairo_Tuple := (0.0, 0.0);
      Radius     : Gdouble     := 0.5;
      From       : Gdouble     := Pi;
      Length     : Gdouble     := Pi;
      Sectors    : Boolean     := False;
      On_Color   : Gdk.Color.Gdk_Color   := Needle_On_Color;
      Off_Color  : Gdk.Color.Gdk_Color   := Needle_Off_Color;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class := null;
      Scaled     : Boolean     := False)
      return not null access Disk_Needle_Layer
   is
      Ptr   : Needle_Ptr := new Disk_Needle_Layer;
      Layer : Disk_Needle_Layer renames Ptr.all;
   begin
      Layer.Scaled := Scaled;
      Add (Ptr, Under);
      Set
        (Layer     => Layer,
         Center    => Center,
         Radius    => Radius,
         From      => From,
         Length    => Length,
         Sectors   => Sectors,
         On_Color  => On_Color,
         Off_Color => Off_Color);
      if Adjustment /= null then
         Add_Adjustment (Ptr.all, Adjustment);
      end if;
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Disk_Needle;

   procedure Changed
     (Adjustment : access GObject_Record'Class;
      Needle     : in     Needle_Ptr)
   is
      pragma Unreferenced (Adjustment);

      Lower : constant Gdouble := Gtk.Adjustment.Get_Lower (Needle.all.Adjustment);
      Upper : constant Gdouble := Gtk.Adjustment.Get_Upper (Needle.all.Adjustment);
      Value : constant Gdouble := Gtk.Adjustment.Get_Value (Needle.all.Adjustment);
   begin
      if Upper <= Lower or else Value <= Lower then
         Needle.all.Set_Value (0.0);
      elsif Value >= Upper then
         Needle.all.Set_Value (1.0);
      else
         Needle.all.Set_Value ((Value - Lower) / (Upper - Lower));
      end if;
      if not Needle.all.Widget.all.Drawing and then Needle.all.Updated then
         Queue_Draw (Needle.all.Widget);  -- Signal draw to the widget
      end if;
   end Changed;

   overriding
   procedure Draw
     (Layer   : in out Disk_Needle_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);

      procedure Do_Half
        (Center : Cairo.Ellipses.Cairo_Tuple;
         Radius : Gdouble;
         From   : Gdouble;
         Color  : Gdk.Color.Gdk_Color);

      procedure Do_Half
        (Center : Cairo.Ellipses.Cairo_Tuple;
         Radius : Gdouble;
         From   : Gdouble;
         Color  : Gdk.Color.Gdk_Color)
      is
         X : aliased Gdouble;
         Y : aliased Gdouble;
      begin
         Cairo.New_Path (Context);
         Cairo.Arc
           (Cr     => Context,
            Xc     => Center.X,
            Yc     => Center.Y,
            Radius => Radius,
            Angle1 => From,
            Angle2 => From + Pi);
         Cairo.Get_Current_Point (Context, X'Access, Y'Access);
         Cairo.Line_To
           (Cr => Context,
            X  => Center.X - Radius * Cairo.Elementary_Functions.Cos (From),
            Y  => Center.Y - Radius * Cairo.Elementary_Functions.Sin (From));

         Cairo.Close_Path (Context);
         Cairo.Set_Source_Rgb
           (Context,
            Gdouble (Gdk.Color.Red   (Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Green (Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Blue  (Color)) / Gdouble (Guint16'Last));
         Cairo.Fill (Context);
      end Do_Half;

      procedure Do_Sector
        (Center : Cairo.Ellipses.Cairo_Tuple;
         Radius : Gdouble;
         From   : Gdouble;
         Color  : Gdk.Color.Gdk_Color);

      procedure Do_Sector
        (Center : Cairo.Ellipses.Cairo_Tuple;
         Radius : Gdouble;
         From   : Gdouble;
         Color  : Gdk.Color.Gdk_Color) is
      begin
         Cairo.New_Path (Context);
         Cairo.Move_To
           (Cr => Context,
            X  => Center.X,
            Y  => Center.Y);
         Cairo.Arc
           (Cr     => Context,
            Xc     => Center.X,
            Yc     => Center.Y,
            Radius => Radius,
            Angle1 => From,
            Angle2 => From + Pi / 2.0);
         Cairo.Arc_Negative
           (Cr     => Context,
            Xc     => Center.X,
            Yc     => Center.Y,
            Radius => Radius,
            Angle1 => From + Pi + Pi / 2.0,
            Angle2 => From + Pi);
         Cairo.Line_To
           (Cr => Context,
            X  => Center.X,
            Y  => Center.Y);
         Cairo.Close_Path (Context);
         Cairo.Set_Source_Rgb
           (Context,
            Gdouble (Gdk.Color.Red   (Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Green (Color)) / Gdouble (Guint16'Last),
            Gdouble (Gdk.Color.Blue  (Color)) / Gdouble (Guint16'Last));
         Cairo.Fill (Context);
      end Do_Sector;

      Start : constant Gdouble :=
                       Layer.From + Layer.Length * Layer.Value;
   begin
      if Get_Scaled (Layer) then
         declare
            Center : constant Cairo.Ellipses.Cairo_Tuple :=
                       Layer.Get_Widget.all.Get_Center;
            Size   : constant Gdouble := Layer.Get_Widget.all.Get_Size;
         begin
            if Layer.Sectors then
               Do_Sector
                 (Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start,
                  Color  => Layer.Off_Color);
               Do_Sector
                 (Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start + Pi / 2.0,
                  Color  => Layer.On_Color);
            else
               Do_Half
                 (Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start,
                  Color  => Layer.Off_Color);
               Do_Half
                 (Center => Center,
                  Radius => Layer.Radius * Size,
                  From   => Start + Pi,
                  Color  => Layer.On_Color);
            end if;
         end;
      else
         if Layer.Sectors then
            Do_Sector
              (Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start,
               Color  => Layer.Off_Color);
            Do_Sector
              (Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start + Pi / 2.0,
               Color  => Layer.On_Color);
         else
            Do_Half
              (Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start,
               Color  => Layer.Off_Color);
            Do_Half
              (Center => Layer.Center,
               Radius => Layer.Radius,
               From   => Start + Pi,
               Color  => Layer.On_Color);
         end if;
      end if;
      Layer.Updated := False;
   end Draw;

   overriding
   procedure Finalize (Layer : in out Disk_Needle_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Finalize (Abstract_Layer (Layer));
      if Layer.Adjustment /= null then
         Gtk.Handlers.Disconnect (Layer.Adjustment, Layer.Changed);
         Gtk.Handlers.Disconnect (Layer.Adjustment, Layer.Value_Changed);
         Gtk.Adjustment.Unref (Layer.Adjustment);
         Layer.Adjustment := null;
      end if;
   end Finalize;

   overriding
   function Get_Adjustment (Layer : Disk_Needle_Layer)
      return Gtk.Adjustment.Gtk_Adjustment is
   begin
      return Layer.Adjustment;
   end Get_Adjustment;

   function Get_Center (Layer : Disk_Needle_Layer)
                        return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Layer.Center;
   end Get_Center;

   function Get_From (Layer : Disk_Needle_Layer) return Gdouble is
   begin
      return Layer.From;
   end Get_From;

   function Get_Length  (Layer : Disk_Needle_Layer) return Gdouble is
   begin
      return Layer.Length;
   end Get_Length;

   function Get_Off_Color (Layer : Disk_Needle_Layer)
      return Gdk.Color.Gdk_Color is
   begin
      return Layer.Off_Color;
   end Get_Off_Color;

   function Get_On_Color (Layer : Disk_Needle_Layer)
      return Gdk.Color.Gdk_Color is
   begin
      return Layer.On_Color;
   end Get_On_Color;

   overriding
   function Get_Properties_Number (Layer : Disk_Needle_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return
        (Layer_Property'Pos (Layer_Property'Last)
         -  Layer_Property'Pos (Layer_Property'First)
         +  1);
   end Get_Properties_Number;

   overriding
   function Get_Property_Specification
     (Layer    : Disk_Needle_Layer;
      Property : Positive) return Param_Spec is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "x",
                    Nick    => "x",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The x-coordinate of the center");
            when Property_Center_Y =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "y",
                    Nick    => "y",
                    Minimum => Gdouble'First,
                    Maximum => Gdouble'Last,
                    Default => 0.0,
                    Blurb   => "The y-coordinate of the center");
            when Property_Radius =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "r",
                    Nick    => "r",
                    Minimum => 1.0E-6,
                    Maximum => Gdouble'Last,
                    Default => 0.5,
                    Blurb   => "The radius");
            when Property_From =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "from",
                    Nick    => "from",
                    Minimum => -2.0 * Pi,
                    Maximum => 2.0 * Pi,
                    Default => 0.0,
                    Blurb   => "The angle of corresponding to the " &
                               "value 0");
            when Property_Length =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "length",
                    Nick    => "length",
                    Minimum => -Pi,
                    Maximum => Pi,
                    Default => Pi,
                    Blurb   => "The length added to the value of " &
                               "the property from is the angle " &
                               "corresponding to the value 1");
            when Property_Value =>
               return
                 Glib.Properties.Creation.Gnew_Double
                   (Name    => "value",
                    Nick    => "value",
                    Minimum => 0.0,
                    Maximum => 1.0,
                    Default => 0.0,
                    Blurb   => "The indicated value");
            when Property_Sectors =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "sectors",
                    Nick    => "sectors",
                    Default => False,
                    Blurb   => "The shape of the needle. When True, " &
                               "it is four sectors of interleaving " &
                               "colors. When False, it is two " &
                               "halves");
            when Property_On_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "on-color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "on-color",
                    Blurb      => "The needle's on color");
            when Property_Off_Color =>
               return
                 Glib.Properties.Creation.Gnew_Boxed
                   (Name       => "off-color",
                    Boxed_Type => Gdk.Color.Gdk_Color_Type,
                    Nick       => "off-color",
                    Blurb      => "The needle's off color");
            when Property_Scaled =>
               return
                 Glib.Properties.Creation.Gnew_Boolean
                   (Name    => "scaled",
                    Nick    => "scaled",
                    Default => False,
                    Blurb   => "The needle size is changed when " &
                               "the widget is resized");
         end case;
      end if;
   end Get_Property_Specification;

   overriding
   function Get_Property_Value
     (Layer    : Disk_Needle_Layer;
      Property : Positive) return Glib.Values.GValue is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         declare
            Value : Glib.Values.GValue;
         begin
            case Layer_Property'Val (Property - 1) is
               when Property_Center_X =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Center.X);
               when Property_Center_Y =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Center.Y);
               when Property_Radius =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Radius);
               when Property_From =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.From);
               when Property_Length =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Length);
               when Property_Sectors =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Sectors);
               when Property_On_Color =>
                  Gdk.Color.Set_Value (Value, Layer.On_Color);
               when Property_Off_Color =>
                  Gdk.Color.Set_Value (Value, Layer.Off_Color);
               when Property_Scaled =>
                  Glib.Values.Init (Value, GType_Boolean);
                  Glib.Values.Set_Boolean (Value, Layer.Scaled);
               when Property_Value =>
                  Glib.Values.Init (Value, GType_Double);
                  Glib.Values.Set_Double (Value, Layer.Value);
            end case;
            return Value;
         end;
      end if;
   end Get_Property_Value;

   function Get_Radius (Layer : Disk_Needle_Layer) return Gdouble is
   begin
      return Layer.Radius;
   end Get_Radius;

   overriding
   function Get_Scaled (Layer : Disk_Needle_Layer) return Boolean is
   begin
      return Layer.Scaled;
   end Get_Scaled;

   function Get_Sectors (Layer : Disk_Needle_Layer) return Boolean is
   begin
      return Layer.Sectors;
   end Get_Sectors;

   overriding
   function Get_Value (Layer : Disk_Needle_Layer) return Gdouble is
   begin
      return Layer.Value;
   end Get_Value;

   overriding
   function Is_Updated (Layer : Disk_Needle_Layer) return Boolean is
   begin
      return Layer.Updated;
   end Is_Updated;

   overriding
   procedure Move (Layer  : in out Disk_Needle_Layer;
                   Offset : Cairo.Ellipses.Cairo_Tuple) is
   begin
      Layer.Center.X := Layer.Center.X + Offset.X;
      Layer.Center.Y := Layer.Center.Y + Offset.Y;
      Layer.Updated  := True;
   end Move;

   overriding
   procedure Restore
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : in out Disk_Needle_Layer)
   is
      Center     : Cairo.Ellipses.Cairo_Tuple;
      Radius     : Gdouble;
      From       : Gdouble;
      Length     : Gdouble;
      Sectors    : Boolean;
      On_Color   : Gdk.Color.Gdk_Color;
      Off_Color  : Gdk.Color.Gdk_Color;
      Adjustment : Boolean;
   begin
      Gtk.Layered.Stream_IO.Restore (Stream, Center);
      Gtk.Layered.Stream_IO.Restore (Stream, Radius);
      Gtk.Layered.Stream_IO.Restore (Stream, From);
      Gtk.Layered.Stream_IO.Restore (Stream, Length);
      Gtk.Layered.Stream_IO.Restore (Stream, On_Color);
      Gtk.Layered.Stream_IO.Restore (Stream, Off_Color);
      Gtk.Layered.Stream_IO.Restore (Stream, Layer.Scaled, Adjustment, Sectors);
      Set
        (Layer     => Layer,
         Center    => Center,
         Radius    => Radius,
         From      => From,
         Length    => Length,
         Sectors   => Sectors,
         On_Color  => On_Color,
         Off_Color => Off_Color);

      if Adjustment then
         declare
            Adjustment : Gtk.Adjustment.Gtk_Adjustment;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Adjustment);
            Add_Adjustment (Layer, Adjustment);
         end;
      else
         declare
            Value : Gdouble;
         begin
            Gtk.Layered.Stream_IO.Restore (Stream, Value);
            Set_Value (Layer, Value);
         end;
      end if;
   end Restore;

   overriding
   procedure Scale
     (Layer  : in out Disk_Needle_Layer;
      Factor : Gdouble)
   is
      Radius : constant Gdouble := Layer.Radius * Factor;
   begin
      if Radius <= 0.0 then
         raise Constraint_Error with "Non-positive radius";
      end if;
      Layer.Radius  := Radius;
      Layer.Updated := True;
   end Scale;

   procedure Set (Layer     : in out Disk_Needle_Layer;
                  Center    : Cairo.Ellipses.Cairo_Tuple;
                  Radius    : Gdouble;
                  From      : Gdouble;
                  Length    : Gdouble;
                  Sectors   : Boolean;
                  On_Color  : Gdk.Color.Gdk_Color;
                  Off_Color : Gdk.Color.Gdk_Color) is
   begin
      if Radius <= 0.0 then
         raise Constraint_Error with "Non-positive radius";
      end if;
      Layer.Center    := Center;
      Layer.Radius    := Radius;
      Layer.From      := From;
      Layer.Length    := Gdouble'Min (Gdouble'Max (Length, -Pi), Pi);
      Layer.Sectors   := Sectors;
      Layer.On_Color  := On_Color;
      Layer.Off_Color := Off_Color;
      Layer.Updated   := True;
   end Set;

   overriding
   procedure Set_Property_Value
     (Layer    : in out Disk_Needle_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      if Property > Get_Properties_Number (Layer) then
         raise Constraint_Error;
      else
         case Layer_Property'Val (Property - 1) is
            when Property_Center_X =>
               Layer.Center.X := Glib.Values.Get_Double (Value);
            when Property_Center_Y =>
               Layer.Center.Y := Glib.Values.Get_Double (Value);
            when Property_Radius =>
               Layer.Radius := Glib.Values.Get_Double (Value);
               if Layer.Radius < 1.0E-6 then
                  Layer.Radius := 1.0E-6;
               end if;
            when Property_From =>
               Layer.From := Glib.Values.Get_Double (Value);
               if Layer.From not in -2.0 * Pi .. 2.0 * Pi then
                  Layer.From :=
                     Gdouble'Remainder (Layer.From, 2.0 * Pi);
               end if;
            when Property_Length =>
               Layer.Length := Glib.Values.Get_Double (Value);
               if Layer.Length < -Pi then
                  Layer.Length := -Pi;
               elsif Layer.Length > -Pi then
                  Layer.Length := Pi;
               end if;
            when Property_Sectors =>
               Layer.Sectors := Glib.Values.Get_Boolean (Value);
            when Property_On_Color =>
               Layer.On_Color := Gdk.Color.Get_Value (Value);
            when Property_Off_Color =>
               Layer.Off_Color := Gdk.Color.Get_Value (Value);
            when Property_Value =>
               Set_Value (Layer, Glib.Values.Get_Double (Value));
            when Property_Scaled =>
               Layer.Scaled := Glib.Values.Get_Boolean (Value);
         end case;
      end if;
      Layer.Updated := True;
   end Set_Property_Value;

   overriding
   procedure Set_Scaled
     (Layer  : in out Disk_Needle_Layer;
      Scaled : Boolean) is
   begin
      Layer.Scaled  := Scaled;
      Layer.Updated := True;
   end Set_Scaled;

   overriding procedure Set_Value
     (Layer : in out Disk_Needle_Layer;
      Value : Gdouble) is
   begin
      if Value <= 0.0 then
         if Layer.Value /= 0.0 then
            Layer.Value   := 0.0;
            Layer.Updated := True;
         end if;
      elsif Value >= 1.0 then
         if Layer.Value /= 1.0 then
            Layer.Value   := 1.0;
            Layer.Updated := True;
         end if;
      else
         if Layer.Value /= Value then
            Layer.Value   := Value;
            Layer.Updated := True;
         end if;
      end if;
   end Set_Value;

   overriding
   procedure Store
     (Stream : in out Ada.Streams.Root_Stream_Type'Class;
      Layer  : Disk_Needle_Layer)
   is
      use type Gtk.Adjustment.Gtk_Adjustment;
   begin
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Center);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Radius);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.From);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Length);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.On_Color);
      Gtk.Layered.Stream_IO.Store (Stream, Layer.Off_Color);
      Gtk.Layered.Stream_IO.Store
        (Stream,
         Layer.Scaled,
         Layer.Adjustment /= null,
         Layer.Sectors);
      if Layer.Adjustment = null then
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Value);
      else
         Gtk.Layered.Stream_IO.Store (Stream, Layer.Adjustment);
      end if;
   end Store;

   pragma Warnings (On, "declaration hides ""Adjustment""");
   pragma Warnings (On, "declaration hides ""Center""");
   pragma Warnings (On, "declaration hides ""Handlers""");
   pragma Warnings (On, "atomic synchronization set");

end Gtk.Layered.Disk_Needle;
