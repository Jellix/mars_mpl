--                                                                    --
--  package Gtk.Source_View         Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2009       --
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

with Gdk.Color;                   use Gdk.Color;
with Gdk.Pixbuf;                  use Gdk.Pixbuf;
with Gtk.Source_Buffer;           use Gtk.Source_Buffer;
with Gtk.Source_Mark_Attributes;  use Gtk.Source_Mark_Attributes;
with Gtk.Text_View;               use Gtk.Text_View;

package Gtk.Source_View is
--
-- Gtk_Source_Smart_Home_End_Type -- Treatment of HOME and END keys
--
   type Gtk_Source_Smart_Home_End_Type is
	(  Home_End_Disabled,
	   Home_End_Before,
	   Home_End_After,
	   Home_End_Always
        );
--
-- Gtk_Source_Draw_Spaces_Flags -- Space drawing flags
--
   type Gtk_Source_Draw_Spaces_Flags is mod 2**4;
   Draw_Spaces_Space    : constant Gtk_Source_Draw_Spaces_Flags := 2**0;
   Draw_Spaces_Tab      : constant Gtk_Source_Draw_Spaces_Flags := 2**1;
   Draw_Spaces_New_line : constant Gtk_Source_Draw_Spaces_Flags := 2**2;
   Draw_Spaces_NBSP     : constant Gtk_Source_Draw_Spaces_Flags := 2**3;
   Draw_Spaces_ALL      : constant Gtk_Source_Draw_Spaces_Flags :=
                             Gtk_Source_Draw_Spaces_Flags'Last;
--
-- Category_Background_Color -- Background color result
--
   type Category_Background_Color (Has_Color : Boolean) is record
      case Has_Color is
         when True  => Color : Gdk_Color;
         when False => null;
      end case;
   end record;
--
-- Gtk_Source_View_Record -- Text view widget with syntax highlighting
--
   type Gtk_Source_View_Record is
      new Gtk_Text_View_Record with private;
   type Gtk_Source_View is access all Gtk_Source_View_Record'Class;
--
-- Get_Auto_Indent -- Get auto identation flag
--
--    Widget - The widget
--
-- Returns :
--
--    True if auto indentation is enabled
--
   function Get_Auto_Indent
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean;
--
-- Get_Draw_Spaces -- Get space drawing flags
--
--    Widget - The widget
--
-- Returns :
--
--    The current flags
--
   function Get_Draw_Spaces
            (  Widget : not null access Gtk_Source_View_Record
            )  return Gtk_Source_Draw_Spaces_Flags;
--
-- Get_Highlight_Current_Line -- Get highlighting mode flag
--
--    Widget - The widget
--
-- Returns :
--
--    True if the current line is highlighted.
--
   function Get_Highlight_Current_Line
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean;
--
-- Get_Indent_On_Tab -- Get indentation mode flag
--
--    Widget - The widget
--
-- Returns :
--
--    True if the selection is indented when tab is pressed
--
   function Get_Indent_On_Tab
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean;
--
-- Get_Indent_Width -- Get identation width
--
--    Widget - The widget
--
-- Returns :
--
--    The number of spaces to use for each step of indent
--
   function Get_Indent_Width
            (  Widget : not null access Gtk_Source_View_Record
            )  return GInt;
--
-- Get_Insert_Spaces_Instead_Of_Tabs -- Get mode flag
--
--    Widget - The widget
--
-- Returns :
--
--    True if spaces are inserted instead of tabs.
--
   function Get_Insert_Spaces_Instead_Of_Tabs
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean;
--
-- Get_Mark_Attributes -- Set mark attributes
--
--   Widget   - The widget
--   Category - The category
--   Priority - The priority
--
-- The result is owned (no Unref)
--
-- Returns :
--
--    Gets attributes and priority for the category
--
   function Get_Mark_Attributes
            (  Widget   : not null access Gtk_Source_View_Record;
               Category : UTF8_String;
               Priority : GInt
            )  return Gtk_Source_Mark_Atributes;
--
-- Get_Right_Margin_Position -- Get right margin position
--
--    Widget - The widget
--
-- Returns :
--
--    The position of the right margin (width in characters)
--
   function Get_Right_Margin_Position
            (  Widget   : not null access Gtk_Source_View_Record
            )  return GUInt;
--
-- Get_Show_Line_Marks -- Get line marks display mode
--
--    Widget - The widget
--
-- Returns :
--
--    True if the line marks are displayed
--
   function Get_Show_Line_Marks
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean;
--
-- Get_Show_Line_Numbers -- Get line numbers display mode
--
--    Widget - The widget
--
-- Returns :
--
--    True if the line numbers are displayed
--
   function Get_Show_Line_Numbers
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean;
--
-- Get_Show_Right_Margin -- Get right margin display mode
--
--    Widget - The widget
--
-- Returns :
--
--    True if the right margin is displayed
--
   function Get_Show_Right_Margin
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean;
--
-- Get_Smart_Home_End -- Get treatment of HOME and END keys
--
--    Widget - The widget
--
-- Returns :
--
--    The behavior of HOME and END keys
--
   function Get_Smart_Home_End
            (  Widget : not null access Gtk_Source_View_Record
            )  return Gtk_Source_Smart_Home_End_Type;
--
-- Get_Tab_Width -- Get tab width in characters
--
--    Widget - The widget
--
-- Returns :
--
--    Width of a tab in characters
--
   function Get_Tab_Width
            (  Widget : not null access Gtk_Source_View_Record
            )  return GUInt;
--
-- Gtk_New -- Widget creation
--
--   Widget - The result
--   Buffer - To use
--
-- When no Buffer is specified a new buffer is created. A source  buffer
-- can be shared between several widgets.
--
   procedure Gtk_New
             (  Widget : out Gtk_Source_View;
                Buffer : Gtk_Source_Buffer := null
             );
--
-- Initialize -- To be called by any derived object
--
--   Widget - The widget
--   Buffer - To use with or null
--
   procedure Initialize
             (  Widget : not null access Gtk_Source_View_Record'Class;
                Buffer : Gtk_Source_Buffer
             );
--
-- Set_Auto_Indent -- Set flag
--
--   Widget - The widget
--   Enable - If True auto indentation of text is enabled
--
   procedure Set_Auto_Indent
             (  Widget : not null access Gtk_Source_View_Record;
                Enable : Boolean
             );
--
-- Set_Draw_Spaces -- Set indentation width
--
--   Widget - The widget
--   Flags  - Display flags
--
-- This  procedure  sets  if  and  how  the spaces should be visualized.
-- Specifying flags as 0 will disable display of spaces.
--
   procedure Set_Draw_Spaces
             (  Widget : not null access Gtk_Source_View_Record;
                Flags  : Gtk_Source_Draw_Spaces_Flags
             );
--
-- Set_Highlight_Current_Line -- Set highlighting flag
--
--   Widget - The widget
--   Show   - Whether to highlight the current line
--
   procedure Set_Highlight_Current_Line
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             );
--
-- Set_Indent_On_Tab -- Set flag
--
--   Widget - The widget
--   Enable - Whether to indent a block when tab is pressed
--
-- If enabled, when the tab key is pressed and there is a selection, the
-- selected text is indented of one level instead of being replaced with
-- the tab characters. Shift+Tab unindents the selection.
--
   procedure Set_Indent_On_Tab
             (  Widget : not null access Gtk_Source_View_Record;
                Enable : Boolean
             );
--
-- Set_Indent_Width -- Set indentation width
--
--   Widget - The widget
--   Width  - Indent width in characters
--
-- This procedure sets the number of spaces to  use  for  each  step  of
-- indent. If Width is -1, the value of the "tab-width" property will be
-- used.
--
   procedure Set_Indent_Width
             (  Widget : not null access Gtk_Source_View_Record;
                Width  : GInt := -1
             );
--
-- Set_Insert_Spaces_Instead_Of_Tabs -- Set flag
--
--   Widget - The widget
--   Enable - Whether to insert spaces instead of tabs
--
   procedure Set_Insert_Spaces_Instead_Of_Tabs
             (  Widget : not null access Gtk_Source_View_Record;
                Enable : Boolean
             );
--
-- Set_Mark_Attributes -- Set mark attributes
--
--   Widget     - The widget
--   Category   - The category
--   Attributes - The attributes to set
--   Priority   - The priority
--
-- Sets attributes and priority for the category.
--
   procedure Set_Mark_Attributes
             (  Widget     : not null access Gtk_Source_View_Record;
                Category   : UTF8_String;
                Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record'Class;
                Priority   : GInt
             );
--
-- Set_Right_Margin_Position -- Set right margin position
--
--   Widget   - The widget
--   Position - The position of the right margin (width in characters)
--
   procedure Set_Right_Margin_Position
             (  Widget   : not null access Gtk_Source_View_Record;
                Position : GUInt
             );
--
-- Set_Show_Line_Marks -- Set line marks displaying flag
--
--   Widget - The widget
--   Show   - Whether line marks should be displayed
--
   procedure Set_Show_Line_Marks
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             );
--
-- Set_Show_Line_Numbers -- Set line marks displaying flag
--
--   Widget - The widget
--   Show   - Whether line numbers should be displayed
--
   procedure Set_Show_Line_Numbers
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             );
--
-- Set_Show_Right_Margin -- Set right margin displaying flag
--
--   Widget - The widget
--   Show   - Whether to show a right margin
--
   procedure Set_Show_Right_Margin
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             );
--
-- Set_Smart_Home_End -- Set the desired movement on
--
--   Widget         - The widget
--   Smart_Home_End - Movement type
--
-- This procedure sets the desired movement of the cursor when  HOME and
-- END keys are pressed.
--
   procedure Set_Smart_Home_End
             (  Widget          : not null access
                                  Gtk_Source_View_Record;
                Smart_Home_End  : Gtk_Source_Smart_Home_End_Type
             );
--
-- Set_Tab_Width -- Set the width of tabulation in characters
--
--   Widget - The widget
--   Width  - Of tab in characters
--
   procedure Set_Tab_Width
             (  Widget : not null access Gtk_Source_View_Record;
                Width  : GUInt
             );
private
   pragma Import (C, Get_Type, "gtk_text_view_get_type");

   type Gtk_Source_View_Record is
      new Gtk_Text_View_Record with null record;
end Gtk.Source_View;
