--                                                                    --
--  package Gtk.Layered.Cache       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2010       --
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

with Cairo.Surface;   use Cairo.Surface;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.Cache is
   type Cache_Ptr is access all Cache_Layer;

   procedure Free is
      new Ada.Unchecked_Deallocation (Cache_Layer, Cache_Ptr);

   overriding function Add
     (Under  : not null access Layer_Location'Class;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return not null access Cache_Layer
   is
      Ptr : Cache_Ptr := new Cache_Layer;
   begin
      Restore (Stream.all, Ptr.all);
      Add (Ptr, Under);
      return Ptr;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add;

   procedure Add_Cache (Under : not null access Layer_Location'Class) is
      Ptr   : Cache_Ptr := new Cache_Layer;
   begin
      Add (Ptr, Under);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Cache;

   function Add_Cache
     (Under : not null access Layer_Location'Class)
      return not null access Cache_Layer
   is
      Ptr   : Cache_Ptr := new Cache_Layer;
      Layer : Cache_Layer renames Ptr.all;
   begin
      Add (Ptr, Under);
      return Layer'Unchecked_Access;
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Cache;

   overriding procedure Draw
     (Layer   : in out Cache_Layer;
      Context : Cairo.Cairo_Context;
      Area    : Gdk.Rectangle.Gdk_Rectangle)
   is
      pragma Unreferenced (Area);
      Height : constant Gint := Layer.Widget.all.Get_Allocated_Height;
      Width  : constant Gint := Layer.Widget.all.Get_Allocated_Width;
      use type Cairo.Cairo_Context;
   begin
      if
        Layer.Cache /= Cairo.Null_Context and then
        Layer.Height = Height and then
        Layer.Width = Width
      then
         Cairo.Set_Operator (Context, Cairo.Cairo_Operator_Source);
         Cairo.Set_Source_Surface
           (Cr      => Context,
            Surface => Cairo.Get_Target (Layer.Cache),
            X       => 0.0,
            Y       => 0.0);
         Cairo.Paint (Context);
         Layer.Widget.all.Center := Layer.Center;
         Layer.Widget.all.Size   := Layer.Size;
      end if;
   end Draw;

   overriding procedure Finalize (Layer : in out Cache_Layer)
   is
      use type Cairo.Cairo_Context;
   begin
      if Layer.Cache /= Cairo.Null_Context then
         Cairo.Destroy (Layer.Cache);
         Layer.Cache := Cairo.Null_Context;
      end if;
      Abstract_Layer (Layer).Finalize;
   end Finalize;

   overriding function Get_Properties_Number
     (Layer : Cache_Layer) return Natural
   is
      pragma Unreferenced (Layer);
   begin
      return 0;
   end Get_Properties_Number;

   overriding function Get_Property_Specification
     (Layer    : Cache_Layer;
      Property : Positive) return Param_Spec
   is
      Result : Param_Spec;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Specification;

   overriding function Get_Property_Value
     (Layer    : Cache_Layer;
      Property : Positive) return Glib.Values.GValue
   is
      Result : Glib.Values.GValue;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Value;

   overriding function Is_Caching (Layer : Cache_Layer) return Boolean
   is
      pragma Unreferenced (Layer);
   begin
      return True;
   end Is_Caching;

   overriding function Is_Updated (Layer : Cache_Layer) return Boolean is
   --        type Layer_Ptr is access all Abstract_Layer'Class;
      use type Cairo.Cairo_Context;
   begin
      return
        (Layer.Cache = Cairo.Null_Context or else
         Layer.Height /= Layer.Widget.all.Get_Allocated_Height or else
         Layer.Width /= Layer.Widget.all.Get_Allocated_Width);
   end Is_Updated;

   overriding procedure Set_Property_Value
     (Layer    : in out Cache_Layer;
      Property : Positive;
      Value    : Glib.Values.GValue) is
   begin
      raise Constraint_Error;
   end Set_Property_Value;

   overriding procedure Store
     (Layer   : in out Cache_Layer;
      Context : Cairo.Cairo_Context)
   is
      Height : constant Gint := Layer.Widget.all.Get_Allocated_Height;
      Width  : constant Gint := Layer.Widget.all.Get_Allocated_Width;
      use type Cairo.Cairo_Context;
   begin
      if
        Layer.Cache = Cairo.Null_Context or else
        Layer.Height /= Height or else
        Layer.Width /= Width
      then
         declare
            Surface : constant Cairo.Cairo_Surface :=
                         Create_Similar
                          (Cairo.Get_Target (Context),
                           Cairo.Cairo_Content_Color,
                           Width,
                           Height);
         begin
            Layer.Cache := Cairo.Create (Surface);
            Cairo.Surface.Destroy (Surface);
         end;
         Layer.Center := Layer.Widget.all.Center;
         Layer.Size   := Layer.Widget.all.Size;
         Layer.Height := Height;
         Layer.Width  := Width;
      end if;
      Cairo.Set_Operator (Layer.Cache, Cairo.Cairo_Operator_Source);
      Cairo.Set_Source_Surface
        (Layer.Cache,
         Cairo.Get_Target (Context),
         0.0,
         0.0);
      Cairo.Paint (Layer.Cache);
   end Store;

end Gtk.Layered.Cache;
