--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope.Channels_Panel             Luebeck            --
--  Interface                                      Summer, 2011       --
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

with GLib.Values;           use GLib.Values;
with Gtk.Box;               use Gtk.Box;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Check_Button;      use Gtk.Check_Button;
with Gtk.Combo_Box;         use Gtk.Combo_Box;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Label;             use Gtk.Label;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Scale;             use Gtk.Scale;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Tree_View_Column;  use Gtk.Tree_View_Column;

package Gtk.Oscilloscope.Channels_Panel is
--
-- Class_Name - Of the widget
--
   Class_Name : constant String :=
                   Gtk.Oscilloscope.Class_Name & "ChannelPanel";
--
-- Gtk_Oscilloscope_Channels_Panel -- Amplifier control panel
--
   type Gtk_Oscilloscope_Channels_Panel_Record is
      new Gtk_Tree_View_Record with private;
   type Gtk_Oscilloscope_Channels_Panel is
      access all Gtk_Oscilloscope_Channels_Panel_Record'Class;
--
-- Get_Type -- The type of the widget
--
-- Returns :
--
--    The GTK type of the widget
--
   function Get_Type return GType;
--
-- Gtk_New -- Widget construction
--
--    Widget       - The result
--    Oscilloscope - The oscilloscope
--
   procedure Gtk_New
             (  Widget       : out Gtk_Oscilloscope_Channels_Panel;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class
             );
--
-- Initialize -- The widget initialization
--
--    Widget       - The widget to initialize
--    Oscilloscope - The oscilloscope
--
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Oscilloscope_Channels_Panel_Record'Class;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class
             );
private
   type Gtk_Oscilloscope_Channels_Panel_Record is
      new Gtk_Tree_View_Record with
   record
      Oscilloscope   : Gtk_Oscilloscope;
      Channel        : Channel_Count;
      Name_Column    : GInt;
      Group_Column   : GInt;
      Mode_Column    : GInt;
      Color_Column   : GInt;
      Values_Column  : GInt;
      Visible_Column : GInt;
   end record;

   function On_Button_Press
            (  Object : access GObject_Record'Class;
               Event  : Gdk_Event;
               Panel  : Gtk_Oscilloscope_Channels_Panel
            )  return Boolean;
   procedure On_Menu_Delete
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Menu_Down
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Menu_Select_Color
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Menu_Up
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Mode_Toggled
             (  Widget : access GObject_Record'Class;
                Values : GValues;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Render_Color
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Render_Group
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Visible_Toggled
             (  Widget : access GObject_Record'Class;
                Values : GValues;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );
   procedure On_Style_Updated
             (  Widget : access GObject_Record'Class;
                Panel  : Gtk_Oscilloscope_Channels_Panel
             );

   package Panel_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Oscilloscope_Channels_Panel
          );
   use Panel_Handlers;

   package Button_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  GObject_Record,
             Boolean,
             Gtk_Oscilloscope_Channels_Panel
          );
   use Button_Handlers;

   package Tree_Functions is
      new Set_Column_Cell_Data (Gtk_Oscilloscope_Channels_Panel);

end Gtk.Oscilloscope.Channels_Panel;
