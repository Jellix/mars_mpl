--                                                                    --
--  package Gtk.Layered             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2010       --
--                                                                    --
--                                Last revision :  09:08 05 Mar 2017  --
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

with Ada.Exceptions;
with Ada.Text_IO;

with Glib.Messages;
with Glib.Properties.Creation;
with Glib.Object;
with Gtkada.Types;
with Gtk.Missed;
with Interfaces.C.Strings;
with System.Storage_Elements;

with Glib.Object.Checked_Destroy;

package body Gtk.Layered is

   pragma Warnings (Off, "declaration hides ""Left""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered." & Name;
   end Where;

   Class : Ada_GObject_Class := Uninitialized_Class;

   Signal_Names : constant Gtkada.Types.Chars_Ptr_Array :=
                    (0 => Interfaces.C.Strings.New_String ("layer-added"),
                     1 => Interfaces.C.Strings.New_String ("layer-removed"));
   Layer_Added_ID   : Signal_Id := Invalid_Signal_Id;
   Layer_Removed_ID : Signal_Id := Invalid_Signal_Id;

   generic
      type Scalar is private;
      Value : GType;
      type Specification is new Param_Spec;
      with function Minimum (Param : Specification) return Scalar is <>;
      with function Maximum (Param : Specification) return Scalar is <>;
   function Match (Left, Right : Param_Spec) return Boolean;

   function Match (Left, Right : Param_Spec) return Boolean is
   begin
      return
        (Value /= Glib.Properties.Creation.Value_Type (Left)
         or else
           ((Minimum (Specification (Left)) = Minimum (Specification (Right)))
            and then
              (Maximum (Specification (Left)) =
                   Maximum (Specification (Right)))));
   end Match;

   function Match_Char is
     new Match (Gint8,
                GType_Char,
                Glib.Properties.Creation.Param_Spec_Char,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);
   function Match_UChar is
     new Match (Guint8,
                GType_Uchar,
                Glib.Properties.Creation.Param_Spec_Uchar,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);
   function Match_Int is
     new Match (Gint,
                GType_Int,
                Glib.Properties.Creation.Param_Spec_Int,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);
   function Match_UInt is
     new Match (Guint,
                GType_Uint,
                Glib.Properties.Creation.Param_Spec_Uint,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);
   function Match_Long is
     new Match (Glong,
                GType_Long,
                Glib.Properties.Creation.Param_Spec_Long,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);
   function Match_ULong is
     new Match (Gulong,
                GType_Ulong,
                Glib.Properties.Creation.Param_Spec_Ulong,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);
   function Match_Float is
     new Match (Gfloat,
                GType_Float,
                Glib.Properties.Creation.Param_Spec_Float,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);
   function Match_Double is
     new Match (Gdouble,
                GType_Double,
                Glib.Properties.Creation.Param_Spec_Double,
                Glib.Properties.Creation.Minimum,
                Glib.Properties.Creation.Maximum);

   function Above (Layer : Abstract_Layer)
      return access Abstract_Layer'Class is
   begin
      if
        Layer.Widget /= null and then Layer.Next /= Layer.Widget.all.Bottom
      then
         return Layer.Next;
      else
         return null;
      end if;
   end Above;

   overriding procedure Add
     (Layer : not null access Abstract_Layer;
      Under : not null access Layer_Location'Class)
   is
      After : Abstract_Layer_Ptr;
   begin
      if Layer.all.Prev /= null or else Layer.all.Next /= null then
         raise Constraint_Error with "Layer is already inserted";
      end if;
      if Under.all in Gtk_Layered_Record'Class then
         -- Inserting the topmost layer
         declare
            Widget : Gtk_Layered_Record'Class renames
                     Gtk_Layered_Record'Class (Under.all);
         begin
            Layer.all.Widget := Widget'Unchecked_Access;
            if Widget.Bottom = null then
               Widget.Bottom  := Layer.all'Unchecked_Access;
               Layer.all.Next     := Layer.all'Unchecked_Access;
               Layer.all.Prev     := Layer.all'Unchecked_Access;
               Widget.Depth   := 1;
               Widget.Updated := True;
               Emit (Widget'Access, Layer_Added_ID, 1);
               return;
            end if;
            After := Widget.Bottom.all.Prev;
         end;
      elsif Under.all in Abstract_Layer'Class then
         -- Inserting under another layer
         declare
            Location : Abstract_Layer'Class renames
                       Abstract_Layer'Class (Under.all);
         begin
            if Location.Prev = null or else Location.Next = null then
               raise Constraint_Error with
                     "Insertion under a layer of no widget";
            end if;
            if Location'Access = Location.Widget.all.Bottom then
               Location.Widget.all.Bottom := Layer.all'Unchecked_Access;
            end if;
            Layer.all.Widget := Location.Widget;
            After := Location.Prev;
         end;
      else
         raise Constraint_Error with
               "Unknown type of the layer location";
      end if;
      Layer.all.Prev := After;
      Layer.all.Next := After.all.Next;
      Layer.all.Prev.all.Next      := Layer.all'Unchecked_Access;
      Layer.all.Next.all.Prev      := Layer.all'Unchecked_Access;
      Layer.all.Widget.all.Depth   := Layer.all.Widget.all.Depth + 1;
      Layer.all.Widget.all.Updated := True;
      Emit (Layer.all.Widget, Layer_Added_ID, Guint (Layer.all.Get_Position));
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Add"));
         raise;
   end Add;

   function Atop (Layer : Abstract_Layer)
      return not null access Layer_Location'Class is
   begin
      if Layer.Widget = null then
         raise Constraint_Error with
               "Layer does not belong to any widget";
      end if;
      if Layer.Next = Layer.Widget.all.Bottom then
         return Layer.Widget;
      else
         return Layer.Next;
      end if;
   end Atop;

   function Below (Layer : Abstract_Layer)
      return access Abstract_Layer'Class is
   begin
      if
        Layer.Widget /= null and then
        Layer.Prev /= null and then
        Layer.Prev.all.Next /= Layer.Widget.all.Bottom
      then
         return Layer.Prev;
      else
         return null;
      end if;
   end Below;

   procedure Destroy
     (Widget : access Gtk_Layered_Record'Class)
   is
      This : Abstract_Layer_Ptr := Widget.all.Bottom;
   begin
      while This /= null loop
         Free (This);
         This := Widget.all.Bottom;
      end loop;
      Widget.all.Finalize;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Destroy"));
   end Destroy;

   function Draw
     (Widget  : access Gtk_Layered_Record'Class;
      Context : Cairo.Cairo_Context) return Boolean is
   begin
      -- Put_Line
      -- (  "+++ "
      -- &  Expanded_Name (Widget.all'Tag)
      -- &  Integer_Address'Image (To_Integer (Widget.all'Address))
      -- );
      Refresh (Widget, Context);
      -- Put_Line ("--- " & Expanded_Name (Widget.all'Tag));
      return True;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Draw"));
         return True;
   end Draw;

   procedure Emit
     (Widget : not null access Gtk_Layered_Record'Class;
      Signal : Signal_Id;
      Value  : Guint)
   is
      procedure EmitV
        (Params : System.Address;
         Signal : Signal_Id;
         Quark  : GQuark;
         Result : System.Address);
      pragma Import (C, EmitV, "g_signal_emitv");
      procedure Set_Object
        (Value  : in out Glib.Values.GValue;
         Object : System.Address);
      pragma Import (C, Set_Object, "g_value_set_object");
      Params : Glib.Values.GValue_Array (0 .. 1);
      Result : Glib.Values.GValue;
   begin
      if Class /= Glib.Object.Uninitialized_Class then
         declare
            This : constant GType := Get_Type;
         begin
            Glib.Values.Init (Params (0), This);
            Set_Object
              (Params (0),
               Gtk.Widget.Convert (Widget.all'Unchecked_Access));
            Glib.Values.Init (Params (1), GType_Uint);
            Glib.Values.Set_Uint (Params (1), Value);
            EmitV (Params (0)'Address, Signal, 0, Result'Address);
            Glib.Values.Unset (Params (0));
            Glib.Values.Unset (Params (1));
         end;
      end if;
   end Emit;

   procedure Erase (Widget : in out Gtk_Layered_Record) is
      Ptr : Abstract_Layer_Ptr;
   begin
      while Widget.Bottom /= null loop
         Ptr := Widget.Bottom;
         Free (Ptr);
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Erase"));
   end Erase;

   overriding procedure Finalize (Layer : in out Abstract_Layer) is
      Widget : constant Gtk_Layered := Layer.Widget;
   begin
      Remove (Layer);
      if Widget /= null then
         Queue_Draw (Widget);
      end if;
   end Finalize;

   function Find_Property
     (Layer      : Abstract_Layer'Class;
      Name       : String;
      Constraint : GType := GType_Invalid) return Natural is
   begin
      for Index in 1 .. Layer.Get_Properties_Number loop
         declare
            Property : constant Param_Spec :=
                       Get_Property_Specification (Layer, Index);
         begin
            if
              Glib.Properties.Creation.Nick_Name (Property) = Name and then
              (Constraint = GType_Invalid or else
               Constraint = Glib.Properties.Creation.Value_Type (Property))
            then
               Glib.Properties.Creation.Unref (Property);
               return Index;
            else
               Glib.Properties.Creation.Unref (Property);
            end if;
         end;
      end loop;
      return 0;
   end Find_Property;

   function Find_Property
     (Layer      : Abstract_Layer'Class;
      Constraint : Param_Spec) return Natural is
   begin
      for Index in 1 .. Layer.Get_Properties_Number loop
         declare
            Property : constant Param_Spec :=
                       Get_Property_Specification (Layer, Index);
         begin
            if
              Glib.Properties.Creation.Nick_Name (Property) = Glib.Properties.Creation.Nick_Name (Constraint) and then
              Glib.Properties.Creation.Value_Type (Property) = Glib.Properties.Creation.Value_Type (Constraint) and then
              Match_Char (Property, Constraint) and then
              Match_UChar (Property, Constraint) and then
              Match_Int (Property, Constraint) and then
              Match_UInt (Property, Constraint) and then
              Match_Long (Property, Constraint) and then
              Match_ULong (Property, Constraint) and then
              Match_Float (Property, Constraint) and then
              Match_Double (Property, Constraint)
            then
               Glib.Properties.Creation.Unref (Property);
               return Index;
            else
               Glib.Properties.Creation.Unref (Property);
            end if;
         end;
      end loop;
      return 0;
   end Find_Property;

   function Get_Aspect_Ratio
     (Widget : not null access constant Gtk_Layered_Record) return Gdouble is
   begin
      return Widget.all.Aspect_Ratio;
   end Get_Aspect_Ratio;

   function Get_Bottom (Widget : not null access Gtk_Layered_Record)
      return not null access Layer_Location'Class is
   begin
      if Widget.all.Bottom = null then
         return Widget;
      else
         return Widget.all.Bottom;
      end if;
   end Get_Bottom;

   function Get_Center
     (Widget : not null access constant Gtk_Layered_Record)
      return Cairo.Ellipses.Cairo_Tuple is
   begin
      return Widget.all.Center;
   end Get_Center;

   function Get_Depth
     (Widget : not null access constant Gtk_Layered_Record) return Natural is
   begin
      return Widget.all.Depth;
   end Get_Depth;

   function Get_Drawing_Time
     (Widget : not null access constant Gtk_Layered_Record)
      return Ada.Real_Time.Time is
   begin
      return Widget.all.Drawing_Time;
   end Get_Drawing_Time;

   function Get_First_Tick (First, Skipped : Tick_Number)
      return Tick_Number is
   begin
      if Skipped = Tick_Number'Last then
         return 1;
      elsif First > Skipped then
         return Skipped;
      else
         return First;
      end if;
   end Get_First_Tick;

   function Get_Layer
     (Widget : not null access Gtk_Layered_Record;
      Layer  : Positive) return access Abstract_Layer'Class
   is
      This : Abstract_Layer_Ptr := Widget.all.Bottom;
   begin
      for Position in 2 .. Layer loop
         if This.all.Next = Widget.all.Bottom then
            return null;
         end if;
         This := This.all.Next;
      end loop;
      return This;
   end Get_Layer;

   function Get_Lower (Widget : not null access Gtk_Layered_Record)
      return access Abstract_Layer'Class is
   begin
      return Widget.all.Bottom;
   end Get_Lower;

   function Get_Position (Layer : Abstract_Layer) return Natural is
   begin
      if Layer.Next = null then
         return 0;
      else
         declare
            This : access constant Abstract_Layer'Class :=
                   Layer.Widget.all.Bottom;
         begin
            for Position in 1 .. Layer.Widget.all.Depth loop
               if This = Layer'Access then
                  return Position;
               end if;
               This := This.all.Next;
            end loop;
            raise Program_Error;
         end;
      end if;
   end Get_Position;

   function Get_Size
     (Widget : not null access constant Gtk_Layered_Record) return Gdouble is
   begin
      return Widget.all.Size;
   end Get_Size;

   function Get_Type return GType is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Drawing_Area.Get_Type,
         Class_Record => Class,
         Type_Name    => "GtkLayered",
         Signals      => Signal_Names,
         Parameters   => (0 => (0 => GType_Uint),
                          1 => (0 => GType_Uint)));
      return Class.all.The_Type;
   end Get_Type;

   function Get_Upper (Widget : not null access Gtk_Layered_Record)
      return access Abstract_Layer'Class is
      This : constant Abstract_Layer_Ptr := Widget.all.Bottom;
   begin
      if This = null then
         return null;
      else
         return This.all.Prev;
      end if;
   end Get_Upper;

   function Get_Widget (Layer : Abstract_Layer)
      return not null access Gtk_Layered_Record'Class is
   begin
      return Layer.Widget;
   end Get_Widget;

   procedure Gtk_New (Widget : out Gtk_Layered) is
   --        procedure Free is
   --           new Ada.Unchecked_Deallocation (Gtk_Layered_Record'Class,
   --                                           Gtk_Layered);
   begin
      Widget := new Gtk_Layered_Record;
      Gtk.Layered.Initialize (Widget);
   exception
      when others =>
         Glib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Initialize
     (Widget : not null access Gtk_Layered_Record'Class) is
   begin
      G_New (Widget, Get_Type);
      Gtk.Drawing_Area.Initialize (Widget);
      if Layer_Added_ID = Invalid_Signal_Id then
         declare
            Widget_Type : constant GType := Get_Type (Widget);
         begin
            Layer_Added_ID := Lookup (Widget_Type, "layer-added");
         end;
      end if;
      if Layer_Removed_ID = Invalid_Signal_Id then
         declare
            Widget_Type : constant GType := Get_Type (Widget);
         begin
            Layer_Removed_ID := Lookup (Widget_Type, "layer-removed");
         end;
      end if;
      Widget_Callback.Connect
        (Widget,
         "destroy",
         Destroy'Access);
      Return_Boolean_Callback.Connect
        (Widget,
         "draw",
         Return_Boolean_Callback.To_Marshaller (Draw'Access));
      Widget_Callback.Connect
        (Widget,
         "notify",
         Notify'Access);
      Widget_Callback.Connect
        (Widget,
         "size_allocate",
         Allocation_Marshaller.To_Marshaller (Size_Allocate'Access),
         After => True);
      Widget_Callback.Connect
        (Widget,
         "style-updated",
         Style_Updated'Access);
      Style_Updated (Widget);
   end Initialize;

   procedure Insert
     (Widget   : not null access Gtk_Layered_Record'Class;
      Layer    : in out Abstract_Layer'Class;
      Position : Positive)
   is
      Under   : access Abstract_Layer'Class;
      Current : constant Natural := Layer.Get_Position;
   begin
      Layer.Remove;
      if Current > 0 or else Position < Current then
         Under := Widget.all.Get_Layer (Position);
      else
         Under := Widget.all.Get_Layer (Position + 1);
      end if;
      if Under = null then
         Layer.Add (Widget);
      else
         Layer.Add (Under);
      end if;
   end Insert;

   function Is_Caching (Layer : Abstract_Layer) return Boolean
   is
      pragma Unreferenced (Layer);
   begin
      return False;
   end Is_Caching;

   procedure Notify
     (Widget : access Gtk_Layered_Record'Class;
      Params : Glib.Values.GValues)
   is
      This  : Abstract_Layer_Ptr := Widget.all.Bottom;
      Param : constant Param_Spec :=
              Param_Spec (Glib.Values.Get_Proxy (Glib.Values.Nth (Params, 1)));
   begin
      if Widget.all.Get_Realized then
         for Position in 1 .. Widget.all.Depth loop
            Property_Set (This.all, Param);
            This := This.all.Next;
         end loop;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Notify"));
   end Notify;

   procedure Refresh
     (Widget  : not null access Gtk_Layered_Record;
      Context : Cairo.Cairo_Context)
   is
      This   : Abstract_Layer_Ptr := Widget.all.Bottom;
      From   : Integer            := 1;
      Bottom : Abstract_Layer_Ptr := Widget.all.Bottom;
      Area   : Gdk_Rectangle;
      Width  : Gdouble;
      Height : Gdouble;
   begin
      Widget.all.Get_Allocation (Area);
--        declare
--           use Cairo.Region;
--           use Gdk.Event;
--           use Gdk.Rectangle;
--           use Gtk.Main;
--           Event : Gdk_Event_Record renames Get_Current_Event.all;
--        begin
--           if Event.Any.The_Type = Expose then
--              declare
--                 Exposed : constant Gdk_Rectangle := Event.Expose.Area;
--                 Region  : Cairo_Rectangle_Int;
--              begin
--                 Get_Extents (Event.Expose.Region, Region);
--                 Put_Line
--                 (  "Draw on expose "
--                 &  Image (Integer (Exposed.X))
--                 &  ".."
--                 &  Image (Integer (Exposed.X + Exposed.Width - 1))
--                 &  " x "
--                 &  Image (Integer (Exposed.Y))
--                 &  ".."
--                 &  Image (Integer (Exposed.Y + Exposed.Height - 1))
--                 &  " count "
--                 &  Image (Integer (Event.Expose.Count))
--                 &  " explicit "
--                 &  Boolean'Image (Event.Expose.Send_Event /= 0)
--                 &  " "
--                 &  Ada.Tags.Expanded_Name
--                    (  Gtk_Layered_Record'Class (Widget.all)'Tag
--                    )
--                 &  " at "
--                 &  Image (Widget.all'Address)
--                 );
--                 Put_Line
--                 (  "Widget area    "
--                 &  Image (Integer (Area.X))
--                 &  ".."
--                 &  Image (Integer (Area.X + Area.Width - 1))
--                 &  " x "
--                 &  Image (Integer (Area.Y))
--                 &  ".."
--                 &  Image (Integer (Area.Y + Area.Height - 1))
--                 );
--                 Put_Line
--                 (  "Region         "
--                 &  Image (Integer (Region.X))
--                 &  ".."
--                 &  Image (Integer (Region.X + Region.Width - 1))
--                 &  " x "
--                 &  Image (Integer (Region.Y))
--                 &  ".."
--                 &  Image (Integer (Region.Y + Region.Height - 1))
--                 );
--                 if (  Exposed.X >= Area.X + Area.Width
--                    or else
--                       Exposed.X + Exposed.Width <= Area.X
--                    or else
--                       Exposed.Y >= Area.Y + Area.Height
--                    or else
--                       Exposed.Y + Exposed.Height <= Area.Y
--                    )
--                 then
--                    Put_Line ("   outside");
--                 end if;
--              end;
--           else
--              Put_Line
--              (  "Refresh on "
--              &  Gdk_Event_Type'Image (Event.Any.The_Type)
--              );
--           end if;
--        end;
      Width  := Gdouble (Area.Width);
      Height := Gdouble (Area.Height);
      Widget.all.Center := (X => Width * 0.5, Y => Height * 0.5);
      Widget.all.Size :=
         Gdouble'Min (Width, Height * Widget.all.Aspect_Ratio);
      Widget.all.Drawing := True;
      Widget.all.Drawing_Time := Ada.Real_Time.Clock;
      --
      -- Prepare layers to draw
      --
      if This /= null then
         for Layer in 1 .. Widget.all.Depth loop
            This.all.Prepare (Context, Area);
            This := This.all.Next;
         end loop;
         This := Bottom;
      end if;
      if not Widget.all.Updated then
         --
         -- Looking for the last opaque layer that was  not  updated.
         -- Since  anything under the layer is obscured, we can start
         -- drawing from this layer.
         --
         for Layer in 1 .. Widget.all.Depth loop
            exit when This.all.Is_Updated;
            if This.all.Is_Caching then
               From   := Layer;
               Bottom := This;
            end if;
            This := This.all.Next;
         end loop;
         Widget.all.Updated := False;
      end if;
      --
      -- Drawing updated layers
      --
      This := Bottom;
      if This /= null then
         for Layer in From .. Widget.all.Depth loop
            if This /= Bottom and then This.all.Is_Caching then
               This.all.Store (Context);
            else
               -- Put_Line("   +" & Expanded_Name (This'Tag));
               This.all.Draw (Context, Area);
               -- Put_Line("   -" & Expanded_Name (This'Tag));
            end if;
            This := This.all.Next;
         end loop;
      end if;
      Widget.all.Drawing := False;
   exception
      when Error : others =>
         Widget.all.Drawing := False;
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Refresh"));
   end Refresh;

   procedure Remove (Layer : in out Abstract_Layer) is
      Position : Guint;
      Widget   : access Gtk_Layered_Record'Class;
   begin
      if Layer.Next /= null then
         Position := Guint (Layer.Get_Position);
         if Layer.Next = Layer'Unchecked_Access then
            Layer.Widget.all.Bottom := null;
         else
            if Layer.Widget.all.Bottom = Layer'Unchecked_Access then
               Layer.Widget.all.Bottom := Layer.Next;
            end if;
            Layer.Next.all.Prev := Layer.Prev;
            Layer.Prev.all.Next := Layer.Next;
         end if;
         Layer.Next           := null;
         Layer.Prev           := null;
         Layer.Widget.all.Depth   := Layer.Widget.all.Depth - 1;
         Layer.Widget.all.Updated := True;
         Widget               := Layer.Widget;
         Layer.Widget         := null;
         if Position > 0 then
            Emit (Widget, Layer_Removed_ID, Position);
         end if;
      end if;
   end Remove;

   procedure Remove
     (Widget : not null access Gtk_Layered_Record;
      Layer  : Positive)
   is
      This : Abstract_Layer_Ptr := Widget.all.Bottom;
   begin
      for Position in 2 .. Layer loop
         if This.all.Next = Widget.all.Bottom then
            return;
         end if;
         This := This.all.Next;
      end loop;
      if This /= null then
         Remove (This.all);
         Free (This);
         Queue_Draw (Widget);
      end if;
   end Remove;

   procedure Set_Aspect_Ratio
     (Widget       : not null access Gtk_Layered_Record;
      Aspect_Ratio : Gdouble) is
   begin
      if Aspect_Ratio < 0.0 then
         raise Constraint_Error with "Non-positive aspect ratio";
      end if;
      Widget.all.Aspect_Ratio := Aspect_Ratio;
      if Widget.all.Get_Realized then
         declare
            Width  : constant Gdouble :=
                     Gdouble (Widget.all.Get_Allocated_Width);
            Height : constant Gdouble :=
                     Gdouble (Widget.all.Get_Allocated_Height);
         begin
            Widget.all.Size := Gdouble'Min (Width, Height * Aspect_Ratio);
         end;
      end if;
   end Set_Aspect_Ratio;

   procedure Set_Texts
     (Layer  : in out Annotation_Layer'Class;
      Texts  : Controlled_String_List;
      Markup : Boolean := False) is
   begin
      Layer.Set_Texts (Get_GList (Texts), Markup);
   end Set_Texts;

   procedure Size_Allocate
     (Widget     : access Gtk_Layered_Record'Class;
      Allocation : Gtk_Allocation_Access)
   is
      This : Abstract_Layer_Ptr := Widget.all.Bottom;
      Area : Gdk_Rectangle;
   begin
      Widget.all.Get_Allocation (Area);
      Widget.all.Center :=
        (X => Gdouble (Area.Width)  * 0.5,
         Y => Gdouble (Area.Height) * 0.5);
      Widget.all.Size :=
         Gdouble'Min
          (Gdouble (Area.Width),
           Gdouble (Area.Height) * Widget.all.Aspect_Ratio);
      Resized (Widget, Allocation.all);
      for Position in 1 .. Widget.all.Depth loop
         Resized (This.all, Area);
         This := This.all.Next;
      end loop;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Size_Allocate"));
   end Size_Allocate;

   procedure Snapshot
     (Widget : not null access Gtk_Layered_Record;
      Target : Cairo.Cairo_Context)
   is
      This   : Abstract_Layer_Ptr := Widget.all.Bottom;
      Bottom : Abstract_Layer_Ptr := Widget.all.Bottom;
      pragma Unreferenced (Bottom);
      Area   : Gdk_Rectangle;
   begin
      Widget.all.Drawing := True;
      Area.X      := 0;
      Area.Y      := 0;
      Area.Width  := Widget.all.Get_Allocated_Width;
      Area.Height := Widget.all.Get_Allocated_Height;
      for Layer in 1 .. Widget.all.Depth loop
         if not This.all.Is_Caching then
            This.all.Draw (Target, Area);
         end if;
         This := This.all.Next;
      end loop;
      Widget.all.Drawing := False;
   exception
      when Error : others =>
         Widget.all.Drawing := False;
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Snapshot (context)"));
   end Snapshot;

   procedure Snapshot
     (Widget : not null access Gtk_Layered_Record;
      Target : Cairo.Cairo_Surface)
   is
      Context : constant Cairo.Cairo_Context := Cairo.Create (Target);
   begin
      Widget.all.Snapshot (Context);
      Cairo.Destroy (Context);
   exception
      when Error : others =>
         Widget.all.Drawing := False;
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Snapshot (surface)"));
         Cairo.Destroy (Context);
   end Snapshot;

   procedure Style_Updated
     (Widget : access Gtk_Layered_Record'Class)
   is
      This : Abstract_Layer_Ptr := Widget.all.Bottom;
   begin
      if Widget.all.Get_Realized then
         Style_Changed (Widget);
         for Position in 1 .. Widget.all.Depth loop
            Style_Set (This.all);
            This := This.all.Next;
         end loop;
      end if;
   exception
      when Error : others =>
         Glib.Messages.Log
           (Gtk.Missed.GtkAda_Contributions_Domain,
            Glib.Messages.Log_Level_Critical,
            "Fault: "
            & Ada.Exceptions.Exception_Information (Error)
            & Where ("Style_Updated"));
   end Style_Updated;

   ---------------------------------------------------------------------
   File    : Ada.Text_IO.File_Type;
   Figures : constant String := "0123456789ABCDEF";

   function Image (Location : System.Address) return String
   is
      Buffer  : String (1 .. 20);
      Pointer : Integer := Buffer'Last;
      Value   : System.Storage_Elements.Integer_Address :=
                  System.Storage_Elements.To_Integer (Location);
      use type System.Storage_Elements.Integer_Address;
   begin
      loop
         Buffer (Pointer) := Figures (Natural (Value mod 16) + 1);
         Pointer := Pointer - 1;
         Value := Value / 16;
         exit when Value = 0;
      end loop;
      return Buffer (Pointer + 1 .. Buffer'Last);
   end Image;

   procedure Put (File  : Ada.Text_IO.File_Type;
                  Where : System.Address) is
   begin
      Ada.Text_IO.Put (File, Image (Where));
   end Put;

   procedure Trace (Data : System.Address; Text : String)
   is
      pragma Unreferenced (Data);
   begin
      if not Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Trace_File);
      end if;
      Ada.Text_IO.Put (File, Text);
   end Trace;

   procedure Trace_Line (Data : System.Address; Text : String) is
   begin
      if not Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Trace_File);
      end if;
      Ada.Text_IO.New_Line (File);
      Put (File, Data);
      Ada.Text_IO.Put (File, "# ");
      Ada.Text_IO.Put (File, Text);
   end Trace_Line;
   ---------------------------------------------------------------------

   pragma Warnings (On, "declaration hides ""Widget""");
   pragma Warnings (On, "declaration hides ""Left""");

end Gtk.Layered;
