--                                                                    --
--  package Gtk.Layered.Cache       Copyright (c)  Dmitry A. Kazakov  --
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
--____________________________________________________________________--

package Gtk.Layered.Cache is
--
-- Cache_Layer -- Layer caching the drawing  of  the  layers  below  it.
--                Usually  a layered widget has some layers which do not
--                change, like the background,  ticks,  texts  etc,  and
--                some layers above which change as the input  data  do,
--                e.g. arrows etc. A caching layer can be placed between
--                them in order to save drawing operations of the static
--                layers. The cache catches the state  of  these  layers
--                and  reproduces  it  each  time  the widget need to be
--                redrawn, e.g. when the arrow is moved.
--
   type Cache_Layer (<>) is new Abstract_Layer with private;
--
-- Add_Cache -- Add caching layer
--
--    Under - The layer or widget where to place the cache under
--
-- Returns :
--
--    The layer (optional)
--
   procedure Add_Cache (Under : not null access Layer_Location'Class);
   function Add_Cache (Under : not null access Layer_Location'Class)
      return not null access Cache_Layer;

   overriding
      function Add
               (  Under  : not null access Layer_Location'Class;
                  Stream : not null access Root_Stream_Type'Class
               )  return not null access Cache_Layer;
   overriding
      procedure Draw
                (  Layer   : in out Cache_Layer;
                   Context : Cairo_Context;
                   Area    : Gdk_Rectangle
                );
   overriding
      procedure Finalize (Layer : in out Cache_Layer);
   overriding
      function Get_Properties_Number
               (  Layer : Cache_Layer
               )  return Natural;
   overriding
      function Get_Property_Specification
               (  Layer    : Cache_Layer;
                  Property : Positive
               )  return Param_Spec;
   overriding
      function Get_Property_Value
               (  Layer    : Cache_Layer;
                  Property : Positive
               )  return GValue;
   overriding function Is_Caching (Layer : Cache_Layer) return Boolean;
   overriding function Is_Updated (Layer : Cache_Layer) return Boolean;
   overriding
      procedure Move
                (  Layer  : in out Cache_Layer;
                   Offset : Cairo_Tuple
                )  is null;
   overriding
      procedure Restore
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : in out Cache_Layer
                )  is null;
   overriding
      procedure Scale
                (  Layer  : in out Cache_Layer;
                   Factor : GDouble
                )  is null;
   overriding
      procedure Set_Property_Value
                (  Layer    : in out Cache_Layer;
                   Property : Positive;
                   Value    : GValue
                );
   overriding
      procedure Store
                (  Layer   : in out Cache_Layer;
                   Context : Cairo_Context
                );
   overriding
      procedure Store
                (  Stream : in out Root_Stream_Type'Class;
                   Layer  : Cache_Layer
                )  is null;
private
   type Cache_Layer is new Abstract_Layer with record
      Cache  : Cairo_Context;
      Size   : GDouble;
      Center : Cairo_Tuple;
      Height : GInt;
      Width  : GInt;
   end record;

end Gtk.Layered.Cache;
