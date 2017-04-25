--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Oscilloscope                            Luebeck            --
--  Implementation                                 Summer, 2011       --
--                                                                    --
--                                Last revision :  10:27 26 Mar 2016  --
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

with Glib.Types;  use Glib.Types;

separate (Gtk.Oscilloscope) function Get_Type return GType is
begin
   if Initialize_Class_Record
      (  Ancestor     => Gtk.Grid.Get_Type,
         Signals      => Signal_Names,
         Class_Record => Class_Record'Access,
         Type_Name    => Class_Name,
         Parameters   =>
               (  0 => (0 => GType_Uint,   1..2 => GType_None),
                  1 => (0 => GType_Uint,   1..2 => GType_None),
                  2 => (0 => GType_Uint,   1..2 => GType_None),
                  3 => (0 => GType_Uint,   1..2 => GType_None),
                  4 => (0 => GType_Uint,   1..2 => GType_None),
                  5 => (0 => GType_Uint,   1..2 => GType_None),
                  6 => (0 => GType_Uint,   1..2 => GType_None),
                  7 => (0 => GType_Uint,   1..2 => GType_None),
                  8 => (0 => GType_Uint,   1..2 => GType_None),
                  9 => (0 => GType_Uint,   1..2 => GType_None),
                 10 => (0 => GType_Uint,   1..2 => GType_None),
                 11 => (0 => GType_Uint,   1..2 => GType_Double),
                 12 => (0 => GType_Uint,   1..2 => GType_None),
                 13 => (0 => GType_Uint,   1..2 => GType_None),
                 14 => (0 => GType_String, 1..2 => GType_None),
                 15 => (0 => GType_Uint,   1..2 => GType_None)
      )    )
   then
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "background-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Background color",
            Blurb      => "The background color"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Double
         (  Name    => "background-opacity",
            Nick    => "Background opacity",
            Minimum => 0.0,
            Maximum => 1.0,
            Default => 1.0,
            Blurb   => "The background opacity"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name    => "waveform-proximity",
            Nick    => "Proximity",
            Minimum => 0,
            Maximum => Guint'Last,
            Default => 10,
            Blurb   => "The waveform poximity area used to detect " &
                       "when the tooltip should contain the value " &
                       "of the waveform"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "selection-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Selection color",
            Blurb      => "The color of selection rectangle"
      )  );
      --
      -- Menus
      --
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-cancel",
            Default => "Cancel selection",
            Nick    => "Cancel selection",
            Blurb   => "The menu item label causing deletion " &
                       "of the selection"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-copy-values",
            Default => "Get values at the mouse cursor",
            Nick    => "Copy values",
            Blurb   => "The menu item label causing copying " &
                       "values under the cursor"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-copy-differences",
            Default => "Subtract values at the mouse cursor",
            Nick    => "Copy differences",
            Blurb   => "The menu item label causing copying " &
                       "differences between stored and new values " &
                       "under the cursor"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-copy-range",
            Default => "Get differences between the margins",
            Nick    => "Copy range",
            Blurb   => "The menu item label causing copying " &
                       "the differences between values at " &
                       "the margins of the selection box"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-latest",
            Default => "Latest data",
            Nick    => "Latest",
            Blurb   => "The menu item label causing all sweepers " &
                       "showing the latest data"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-pause",
            Default => "Hold",
            Nick    => "Hold",
            Blurb   => "The menu item label causing freezing all " &
                       "sweepers"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-redo",
            Default => "Redo zooming",
            Nick    => "Redo",
            Blurb   => "The menu item label redo"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-release",
            Default => "Release",
            Nick    => "Release",
            Blurb   => "The menu item label causing releasing all " &
                       "sweepers"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-snapshot",
            Default => "Snapshot",
            Nick    => "Snatshot",
            Blurb   => "The menu item label for capturing " &
                       "oscilloscope's contents"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-toggle-grid",
            Default => "Toggle grid",
            Nick    => "Toggle grid",
            Blurb   => "The menu item label for toggling grid"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-toggle-interpolation",
            Default => "Toggle interpolation mode",
            Nick    => "Toggle interpolation",
            Blurb   => "The menu item label for toggling " &
                       "interpolation mode"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-undo",
            Default => "Undo zooming",
            Nick    => "Undo",
            Blurb   => "The menu item label undo"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-zoom-in",
            Default => "Zoom in",
            Nick    => "Zoom in",
            Blurb   => "The menu item label causing zooming into " &
                       "the selected area"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-zoom-in-t",
            Default => "Zoom in time",
            Nick    => "Zoom in t",
            Blurb   => "The menu item label causing zooming into " &
                       "the selected horizontal range"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-zoom-in-v",
            Default => "Zoom in values",
            Nick    => "Zoom in v",
            Blurb   => "The menu item label causing zooming into " &
                       "the selected vertical range"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-zoom-out",
            Default => "Zoom out",
            Nick    => "Zoom out",
            Blurb   => "The menu item label causing zooming out " &
                       "of the selected area"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-zoom-out-t",
            Default => "Zoom out time",
            Nick    => "Zoom out t",
            Blurb   => "The menu item label causing zooming out " &
                       "of the selected horizontal range"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_String
         (  Name    => "menu-zoom-out-v",
            Default => "Zoom out values",
            Nick    => "Zoom out v",
            Blurb   => "The menu item label causing zooming out " &
                       "of the selected vertical range"
      )  );
      --
      -- Time axis properties
      --
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "time-axis-height",
            Minimum    => 1,
            Maximum    => Guint'Last,
            Default    => 24,
            Nick       => "Time axis height",
            Blurb      => "The height of the box under or above " &
                          "of the waveforms box, where the time " &
                          "axis is shown"
      )  );
      Install_Style
      (  Class_Ref (Class_Record.The_Type),
         Cairo.Line_Cap_Property.Gnew_Enum
         (  Name    => "time-line-cap",
            Nick    => "Time line cap",
            Blurb   => "The style used for the time axis line",
            Default => Cairo_Line_Cap_Round
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "time-line-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Time line color",
            Blurb      => "The color of the time axis line"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "time-line-width",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 1,
            Nick       => "Time line width",
            Blurb      => "The width of the time axis line"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "time-major-tick-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Time major color",
            Blurb      => "The color of the time axis major tick"
      )  );
      Install_Style
      (  Class_Ref (Class_Record.The_Type),
         Cairo.Line_Cap_Property.Gnew_Enum
         (  Name    => "time-major-tick-cap",
            Nick    => "Time major cap",
            Default => Cairo_Line_Cap_Butt,
            Blurb   => "The style used for the line of the " &
                       "time axis major ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "time-major-tick-width",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 1,
            Nick       => "Time major width",
            Blurb      => "The width of the time axis major tick"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "time-minor-tick-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Time minor color",
            Blurb      => "The color of the time axis minor tick"
      )  );
      Install_Style
      (  Class_Ref (Class_Record.The_Type),
         Cairo.Line_Cap_Property.Gnew_Enum
         (  Name    => "time-minor-tick-cap",
            Nick    => "Time minor cap",
            Default => Cairo_Line_Cap_Butt,
            Blurb   => "The style used for the line of the " &
                       "time axis minor ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "time-minor-tick-width",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 1,
            Nick       => "Time minor width",
            Blurb      => "The width of the time axis minor tick"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "time-tick-height",
            Minimum    => 1,
            Maximum    => Guint'Last,
            Default    => 5,
            Nick       => "Time tick height",
            Blurb      => "The height of the time axis ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "time-tick-step",
            Minimum    => 1,
            Maximum    => Guint'Last,
            Default    => 50,
            Nick       => "The time step",
            Blurb      => "The approximate step of time ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "time-text-border-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Time border color",
            Blurb      => "The color of the rectangle under " &
                          "the annotation texts of the time axis"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Double
         (  Name       => "time-text-border-opacity",
            Minimum    => 0.0,
            Maximum    => 1.0,
            Default    => 1.0,
            Nick       => "Time border opacity",
            Blurb      => "The opacity of the rectangle under " &
                          "the annotation texts of the time axis"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "time-text-border",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 2,
            Nick       => "Time text border",
            Blurb      => "The space added to text annotation " &
                          "box to obtain the box laid down under " &
                          "the text of a time axis"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Int
         (  Name       => "time-text-overlap",
            Minimum    => Gint'First,
            Maximum    => Gint'Last,
            Default    => -2,
            Nick       => "Time text overlap",
            Blurb      => "The overlapping of time annotation " &
                          "text boxes. The value is added to the " &
                          "box left and top margins and " &
                          "subtracted from the right and bottom " &
                          "margins. If the obtained box overlaps " &
                          "the text box of the following text " &
                          "box, the latter is not shown"
      )  );
      --
      -- Values axis properties
      --
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "values-axis-width",
            Minimum    => 1,
            Maximum    => Guint'Last,
            Default    => 50,
            Nick       => "Values axis width",
            Blurb      => "The width of the box left or right " &
                          "of the waveforms box, where the values " &
                          "axis is shown"
      )  );
      Install_Style
      (  Class_Ref (Class_Record.The_Type),
         Cairo.Line_Cap_Property.Gnew_Enum
         (  Name    => "values-line-cap",
            Nick    => "Values line cap",
            Blurb   => "The style used for the values axis line",
            Default => Cairo_Line_Cap_Round
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "values-line-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Values line color",
            Blurb      => "The color of the values axis line"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "values-line-width",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 1,
            Nick       => "Values line width",
            Blurb      => "The width of the values axis line"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "values-major-tick-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Values major color",
            Blurb      => "The color of the values axis major tick"
      )  );
      Install_Style
      (  Class_Ref (Class_Record.The_Type),
         Cairo.Line_Cap_Property.Gnew_Enum
         (  Name    => "values-major-tick-cap",
            Nick    => "Values major cap",
            Default => Cairo_Line_Cap_Butt,
            Blurb   => "The style used for the line of the " &
                       "values axis major ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "values-major-tick-width",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 1,
            Nick       => "Values major width",
            Blurb      => "The width of the values axis major tick"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "values-minor-tick-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Values minor color",
            Blurb      => "The color of the values axis minor tick"
      )  );
      Install_Style
      (  Class_Ref (Class_Record.The_Type),
         Cairo.Line_Cap_Property.Gnew_Enum
         (  Name    => "values-minor-tick-cap",
            Nick    => "Values minor cap",
            Default => Cairo_Line_Cap_Butt,
            Blurb   => "The style used for the line of the " &
                       "values axis minor ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "values-minor-tick-width",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 1,
            Nick       => "Values minor width",
            Blurb      => "The width of the values axis minor tick"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "values-tick-width",
            Minimum    => 1,
            Maximum    => Guint'Last,
            Default    => 5,
            Nick       => "Values tick width",
            Blurb      => "The width of the values axis ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "values-tick-step",
            Minimum    => 1,
            Maximum    => Guint'Last,
            Default    => 50,
            Nick       => "The values step",
            Blurb      => "The approximate step of time ticks"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Boxed
         (  Name       => "values-text-border-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Values border color",
            Blurb      => "The color of the rectangle under " &
                          "the annotation texts of the values axis"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Double
         (  Name       => "values-text-border-opacity",
            Minimum    => 0.0,
            Maximum    => 1.0,
            Default    => 1.0,
            Nick       => "Values border opacity",
            Blurb      => "The opacity of the rectangle under " &
                          "the annotation texts of the values axis"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Uint
         (  Name       => "values-text-border",
            Minimum    => 0,
            Maximum    => Guint'Last,
            Default    => 2,
            Nick       => "Values text border",
            Blurb      => "The space added to text annotation " &
                          "box to obtain the box laid down under " &
                          "the text of a values axis"
      )  );
      Install_Style_Property
      (  Class_Ref (Class_Record.The_Type),
         Gnew_Int
         (  Name       => "values-text-overlap",
            Minimum    => Gint'First,
            Maximum    => Gint'Last,
            Default    => -2,
            Nick       => "Values text overlap",
            Blurb      => "The overlapping of values annotation " &
                          "text boxes. The value is added to the " &
                          "box left and top margins and " &
                          "subtracted from the right and bottom " &
                          "margins. If the obtained box overlaps " &
                          "the text box of the following text " &
                          "box, the latter is not shown"
      )  );
      --
      -- Waveform properties
      --
      Install_Style
      (  Class_Ref (Class_Record.The_Type),
         Cairo.Line_Cap_Property.Gnew_Enum
         (  Name    => "waveform-line-cap",
            Nick    => "Waeform line cap",
            Blurb   => "The style used for the waveform lines",
            Default => Cairo_Line_Cap_Round
      )  );
   end if;
   return Class_Record.The_Type;
end Get_Type;
