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
--____________________________________________________________________--

with Ada.Exceptions;  use Ada.Exceptions;
with Cairo.Surface;   use Cairo.Surface;

with Ada.Unchecked_Deallocation;

package body Gtk.Layered.Cache is
   type Cache_Ptr is access all Cache_Layer;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Layered.Cache." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation (Cache_Layer, Cache_Ptr);

   function Add
            (  Under  : not null access Layer_Location'Class;
               Stream : not null access Root_Stream_Type'Class
            )  return not null access Cache_Layer is
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
      Layer : Cache_Layer renames Ptr.all;
   begin
      Add (Ptr, Under);
   exception
      when others =>
         Free (Ptr);
         raise;
   end Add_Cache;

   function Add_Cache
            (  Under : not null access Layer_Location'Class
            )  return not null access Cache_Layer is
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

   procedure Draw
             (  Layer   : in out Cache_Layer;
                Context : Cairo_Context;
                Area    : Gdk_Rectangle
             )  is
      Height : constant GInt := Layer.Widget.Get_Allocated_Height;
      Width  : constant GInt := Layer.Widget.Get_Allocated_Width;
   begin
      if (  Layer.Cache /= Null_Context
         and then
            Layer.Height = Height
         and then
            Layer.Width = Width
         )
      then
         Set_Operator (Context, CAIRO_OPERATOR_SOURCE);
         Set_Source_Surface
         (  Cr      => Context,
            Surface => Get_Target (Layer.Cache),
            X       => 0.0,
            Y       => 0.0
         );
         Paint (Context);
         Layer.Widget.Center := Layer.Center;
         Layer.Widget.Size   := Layer.Size;
      end if;
   end Draw;

   procedure Finalize (Layer : in out Cache_Layer) is
   begin
      if Layer.Cache /= Null_Context then
         Destroy (Layer.Cache);
         Layer.Cache := Null_Context;
      end if;
      Abstract_Layer (Layer).Finalize;
   end Finalize;

   function Get_Properties_Number
            (  Layer : Cache_Layer
            )  return Natural is
   begin
      return 0;
   end Get_Properties_Number;

   function Get_Property_Specification
            (  Layer    : Cache_Layer;
               Property : Positive
            )  return Param_Spec is
      Result : Param_Spec;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Specification;

   function Get_Property_Value
            (  Layer    : Cache_Layer;
               Property : Positive
            )  return GValue is
      Result : GValue;
   begin
      raise Constraint_Error;
      return Result;
   end Get_Property_Value;

   function Is_Caching (Layer : Cache_Layer) return Boolean is
   begin
      return True;
   end Is_Caching;

   function Is_Updated (Layer : Cache_Layer) return Boolean is
      type Layer_Ptr is access all Abstract_Layer'Class;
   begin
      return
      (  Layer.Cache = Null_Context
      or else
         Layer.Height /= Layer.Widget.Get_Allocated_Height
      or else
         Layer.Width /= Layer.Widget.Get_Allocated_Width
      );
   end Is_Updated;

   procedure Set_Property_Value
             (  Layer    : in out Cache_Layer;
                Property : Positive;
                Value    : GValue
             )  is
   begin
      raise Constraint_Error;
   end Set_Property_Value;

   procedure Store
             (  Layer   : in out Cache_Layer;
                Context : Cairo_Context
             )  is
      Height : constant GInt := Layer.Widget.Get_Allocated_Height;
      Width  : constant GInt := Layer.Widget.Get_Allocated_Width;
   begin
      if (  Layer.Cache = Null_Context
         or else
            Layer.Height /= Height
         or else
            Layer.Width /= Width
         )
      then
         declare
            Surface : constant Cairo_Surface :=
                         Create_Similar
                         (  Get_Target (Context),
                            CAIRO_CONTENT_COLOR,
                            Width,
                            Height
                         );
         begin
            Layer.Cache := Create (Surface);
            Cairo.Surface.Destroy (Surface);
         end;
         Layer.Center := Layer.Widget.Center;
         Layer.Size   := Layer.Widget.Size;
         Layer.Height := Height;
         Layer.Width  := Width;
      end if;
      Set_Operator (Layer.Cache, CAIRO_OPERATOR_SOURCE);
      Set_Source_Surface
      (  Layer.Cache,
         Get_Target (Context),
         0.0,
         0.0
      );
      Paint (Layer.Cache);
   end Store;

end Gtk.Layered.Cache;
