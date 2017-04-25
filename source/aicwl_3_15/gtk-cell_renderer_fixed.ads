--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fixed                     Luebeck            --
--  Interface                                      Summer, 2006       --
--                                                                    --
--                                Last revision :  22:45 07 Apr 2016  --
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
--
--  This  package  provides  a  renderer to indicate fixed-point numbers
--  aligned along their decimal points. 
--
with Cairo;                     use Cairo;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Values;               use GLib.Values;
with Gtk.Cell_Renderer;         use Gtk.Cell_Renderer;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget;                use Gtk.Widget;
with Pango.Layout;              use Pango.Layout;

with Gtk.Cell_Renderer.Abstract_Renderer;

package Gtk.Cell_Renderer_Fixed is
   pragma Elaborate_Body (Gtk.Cell_Renderer_Fixed);
--
-- Gtk_Cell_Renderer_Fixed_Record -- The renderer type
--
-- Customary,  we  need  to  declare a representation record type and an
-- interface access type for dealing with renderer's objects. The record
-- type is never used directly, though all operations are defined in its
-- terms. 
--
   type Gtk_Cell_Renderer_Fixed_Record is
      new Gtk.Cell_Renderer.Abstract_Renderer.
          Gtk_Abstract_Renderer_Record with private;
   type Gtk_Cell_Renderer_Fixed is
      access all Gtk_Cell_Renderer_Fixed_Record'Class;
--
-- Finalize -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Finalize
             (  Cell : not null access Gtk_Cell_Renderer_Fixed_Record
             );
--
-- Get_Aligned_Area -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Aligned_Area
            (  Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Flags  : Gtk_Cell_Renderer_State;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
--
-- Get_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Get_Property
             (  Cell : not null access Gtk_Cell_Renderer_Fixed_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             );
--
-- Get_Size -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Get_Size
            (  Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle;
   overriding
   function Get_Size
            (  Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Gdk_Rectangle;
--
-- Get_Type -- Get the type of cell renderer
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Cell  - The result
--    After - The number of digits after decimal point
--
   procedure Gtk_New
             (  Cell  : out Gtk_Cell_Renderer_Fixed;
                After : Natural := 0
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Cell  - The renderer to initialize
--    After - The number of digits after decimal point
--
-- This procedure is never called directly, only from  Gtk_New  or  else
-- from  Initialize  of  a  derived  type.  In the latter case a call to
-- Initialize is obligatory. 
--
   procedure Initialize
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fixed_Record'Class;
                After : Natural
             );
--
-- Render -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Render
             (  Cell    : not null access
                          Gtk_Cell_Renderer_Fixed_Record;
                Context : Cairo_Context;
                Widget  : not null access Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             );
--
-- Set_Property -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   procedure Set_Property
             (  Cell : not null access Gtk_Cell_Renderer_Fixed_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             );
--
-- Start_Editing -- Overrides Gtk.Cell_Renderer.Abstract_Renderer...
--
   overriding
   function Start_Editing
            (  Cell   : not null access Gtk_Cell_Renderer_Fixed_Record;
               Event  : Gdk_Event;
               Widget : not null access Gtk_Widget_Record'Class;
               Path            : UTF8_String;
               Background_Area : Gdk_Rectangle;
               Cell_Area       : Gdk_Rectangle;
               Flags           : Gtk_Cell_Renderer_State
            )  return Gtk_Widget;
private
--
-- Gtk_Cell_Renderer_Fixed_Record -- Implementation
--
-- The  renderer  maintains  its  state global to the column it renders.
-- That is the text widget it uses to render the number, the  number  of
-- places  after  the  decimal point and the maximal width of the number
-- places  before  the point including the sign. This field is evaluated
-- dynamically and adjusted each time the renderer is  queried  for  its
-- size or asked to render a cell. This heuristics might not work if new
-- rows are added to the tree model after it was rendered once. 
--
   type Gtk_Cell_Renderer_Fixed_Record is
      new Gtk.Cell_Renderer.
          Abstract_Renderer.Gtk_Abstract_Renderer_Record with
   record
      Text       : Pango_Layout;       -- The text to display
      Value      : GDouble   := 0.0;   -- Current value
      Empty      : Boolean   := False; -- Leave it empty
      Color_Set  : Boolean   := False; -- Use text color
      After      : Natural   := 0;     -- Places after the point
      Max_Offset : GInt      := 0;     -- Pixel offset to the point
      Height     : GInt      := 0;     -- Current pixel height
      Width      : GInt      := 0;     -- Current pixel width
      Left_Width : GInt;               -- Current space before the point
      Focus_Out  : Handler_Id;         -- Current focus_out_event handler
      Color      : Gdk_Color := RGB (0.0, 0.0, 0.0);
   end record;
--
-- Editing_Done -- The handler of editing_done
--
   procedure Editing_Done
             (  Editor : access Gtk_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Fixed
             );
--
-- Focus_Out -- The handler of focus_out
--
   function Focus_Out
            (  Editor : access Gtk_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Fixed
            )  return Boolean;
--
-- Entry_Callbacks -- To handle editing_done
--
   package Entry_Callbacks is
      new Gtk.Handlers.User_Callback
          (  Widget_Type => Gtk_Entry_Record,
             User_Type   => Gtk_Cell_Renderer_Fixed
          );
--
-- Entry_Return_Callbacks -- To handle focus_out_event
--
   package Entry_Return_Callbacks is
      new Gtk.Handlers.User_Return_Callback
          (  Widget_Type => Gtk_Entry_Record,
             Return_Type => Boolean,
             User_Type   => Gtk_Cell_Renderer_Fixed
          );

end Gtk.Cell_Renderer_Fixed;
