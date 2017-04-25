--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope.Sweeper_Panel              Luebeck            --
--  Interface                                      Summer, 2011       --
--                                                                    --
--                                Last revision :  16:49 28 Feb 2016  --
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

with Gtk.Box;           use Gtk.Box;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Label;         use Gtk.Label;
with Gtk.Table;         use Gtk.Table;
with Glib.Values;       use Glib.Values;

with Gtk.Generic_Style_Button;
with Gtk.Handlers;

package Gtk.Oscilloscope.Sweeper_Panel is
--
-- Class_Name - Of the widget
--
   Class_Name : constant String :=
                   Gtk.Oscilloscope.Class_Name & "SweeperPanel";
--
-- Gtk_Oscilloscope_Sweeper_Panel -- Sweeper control panel
--
   type Gtk_Oscilloscope_Sweeper_Panel_Record is
      new Gtk_Table_Record with private;
   type Gtk_Oscilloscope_Sweeper_Panel is
      access all Gtk_Oscilloscope_Sweeper_Panel_Record'Class;
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
--    Sweeper      - The sweeper to use the panel with
--    Show_Buttons - Show buttons controlling graphpaper and axis
--    Flat         - Show all elements in one row
--
   procedure Gtk_New
             (  Widget       : out Gtk_Oscilloscope_Sweeper_Panel;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Sweeper      : Sweeper_Type := Lower;
                Show_Buttons : Boolean      := True;
                Flat         : Boolean      := False
             );
--
-- Initialize -- The widget initialization
--
--    Widget       - The widget to initialize
--    Oscilloscope - The oscilloscope
--    Sweeper      - The sweeper to use the panel with
--    Show_Buttons - Show buttons controlling graphpaper and axis
--    Flat         - Show all elements in one row
--
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Oscilloscope_Sweeper_Panel_Record'Class;
                Oscilloscope : not null access
                               Gtk_Oscilloscope_Record'Class;
                Sweeper      : Sweeper_Type;
                Show_Buttons : Boolean;
                Flat         : Boolean
             );
private
   package Forward_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ForwardButton",
             Icon       => "gtk-media-forward",
             Tip        => "Latest"
          );
   package Hold_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "HoldButton",
             Icon       => "gtk-media-pause",
             Tip        => "Hold"
          );
   package Run_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "RunButton",
             Icon       => "gtk-media-play",
             Tip        => "Run"
          );
   package Offset_Page_Left_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "OffsetPageLeftButton",
             Icon       => "gtk-media-previous",
             Tip        => "Older, moves the window left"
          );
   package Offset_Left_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "OffsetLeftButton",
             Icon       => "gtk-go-back",
             Tip        => "Back, moves the window left"
          );
   package Offset_Right_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "OffsetRightButton",
             Icon       => "gtk-go-forward",
             Tip        => "Forward, moves the window right"
          );
   package Offset_Page_Right_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "OffsetPageRightButton",
             Icon       => "gtk-media-next",
             Tip        => "More recent, moves the window right"
          );
   package Page_Faster_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "PageFasterButton",
             Icon       => "gtk-add",
             Tip        => "Faster"
          );
   package Page_Slower_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "PageSlowerButton",
             Icon       => "gtk-remove",
             Tip        => "Slower"
          );

   type Gtk_Oscilloscope_Sweeper_Panel_Record is
      new Gtk_Table_Record with
   record
      Sweeper           : Sweeper_Type;
      Oscilloscope      : Gtk_Oscilloscope;
      Axis_Button       : Gtk_Check_Button;
      Grid_Button       : Gtk_Check_Button;
      Forward_Button    : Forward_Buttons.Gtk_Style_Button;
      Hold_Button       : Hold_Buttons.Gtk_Style_Button;
      Run_Button        : Run_Buttons.Gtk_Style_Button;
      Page_Slower       : Page_Slower_Buttons.Gtk_Style_Button;
      Page_Edit         : Gtk_GEntry;
      Page_Unit         : Gtk_Label;
      Page_Faster       : Page_Faster_Buttons.Gtk_Style_Button;
      Offset_Page_Left  : Offset_Page_Left_Buttons.Gtk_Style_Button;
      Offset_Left       : Offset_Left_Buttons.Gtk_Style_Button;
      Offset_Edit       : Gtk_GEntry;
      Offset_Unit       : Gtk_Label;
      Offset_Right      : Offset_Right_Buttons.Gtk_Style_Button;
      Offset_Page_Right : Offset_Page_Right_Buttons.Gtk_Style_Button;
      Time_Stamp        : Gtk_Label;
   end record;

   procedure On_Axis_Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Frozen_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Grid_Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Offset_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Axis   : GUInt;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );

   procedure On_Changed_Offset
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Changed_Page
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Faster
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Forward
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Left
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Right
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Hold
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Page_Left
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Page_Right
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Run
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Click_Slower
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Position_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Values : GValues;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Toggled_Axis
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure On_Toggled_Grid
             (  Widget : access Gtk_Widget_Record'Class;
                Panel  : Gtk_Oscilloscope_Sweeper_Panel
             );
   procedure Show_Offset
             (  Widget : not null access
                         Gtk_Oscilloscope_Sweeper_Panel_Record
             );

   package Panel_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Oscilloscope_Sweeper_Panel
          );
   use Panel_Handlers;

end Gtk.Oscilloscope.Sweeper_Panel;
