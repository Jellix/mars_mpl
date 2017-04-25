--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Source_View                             Luebeck            --
--  Implementation                                 Summer, 2009       --
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

with Interfaces.C;  use Interfaces.C;
with System;        use System;

with Ada.Unchecked_Deallocation;
with Gdk.Pixbuf.Conversions;

package body Gtk.Source_View is

   function Get_Auto_Indent
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean is
      function Internal (Widget : Address) return GBoolean;
      pragma Import (C, Internal, "gtk_source_view_get_auto_indent");
   begin
      return 0 /= Internal (Get_Object (Widget));
   end Get_Auto_Indent;

   function Get_Draw_Spaces
            (  Widget : not null access Gtk_Source_View_Record
            )  return Gtk_Source_Draw_Spaces_Flags is
      function Internal (Widget : Address) return Unsigned;
      pragma Import (C, Internal, "gtk_source_view_set_draw_spaces");
   begin
      return
         Gtk_Source_Draw_Spaces_Flags'Val
         (  Internal (Get_Object (Widget))
         );
   end Get_Draw_Spaces;

   function Get_Highlight_Current_Line
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean is
      function Internal (Widget : Address) return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_get_highlight_current_line"
             );
   begin
      return 0 /= Internal (Get_Object (Widget));
   end Get_Highlight_Current_Line;

   function Get_Indent_On_Tab
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean is
      function Internal (Widget : Address) return GBoolean;
      pragma Import (C, Internal, "gtk_source_view_get_indent_on_tab");
   begin
      return 0 /= Internal (Get_Object (Widget));
   end Get_Indent_On_Tab;

   function Get_Indent_Width
            (  Widget : not null access Gtk_Source_View_Record
            )  return GInt is
      function Internal (Widget : Address) return GInt;
      pragma Import (C, Internal, "gtk_source_view_get_indent_width");
   begin
      return Internal (Get_Object (Widget));
   end Get_Indent_Width;

   function Get_Insert_Spaces_Instead_Of_Tabs
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean is
      function Internal (Widget : Address) return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_get_insert_spaces_instead_of_tabs"
             );
   begin
      return 0 /= Internal (Get_Object (Widget));
   end Get_Insert_Spaces_Instead_Of_Tabs;

   function Get_Mark_Attributes
            (  Widget   : not null access Gtk_Source_View_Record;
               Category : UTF8_String;
               Priority : GInt
            )  return Gtk_Source_Mark_Atributes is
      function Internal
               (  Widget   : Address;
                  Category : Char_Array;
                  Priority : GInt
               )  return Address;
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_get_mark_attributes"
             );
      Stub : Gtk_Source_Mark_Atributes_Record;
   begin
      return
         Gtk_Source_Mark_Atributes
         (  Get_User_Data_Fast
            (  Internal
               (  Get_Object (Widget),
                  To_C (Category),
                  Priority
               ),
               Stub
        )  );
   end Get_Mark_Attributes;

   function Get_Right_Margin_Position
            (  Widget : not null access Gtk_Source_View_Record
            )  return GUInt is
      function Internal (Widget : Address) return GUInt;
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_get_right_margin_position"
             );
   begin
      return Internal (Get_Object (Widget));
   end Get_Right_Margin_Position;

   function Get_Show_Line_Marks
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean is
      function Internal (Widget : Address) return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_get_show_line_marks"
             );
   begin
      return 0 /= Internal (Get_Object (Widget));
   end Get_Show_Line_Marks;

   function Get_Show_Line_Numbers
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean is
      function Internal (Widget : Address) return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_get_show_line_numbers"
             );
   begin
      return 0 /= Internal (Get_Object (Widget));
   end Get_Show_Line_Numbers;

   function Get_Show_Right_Margin
            (  Widget : not null access Gtk_Source_View_Record
            )  return Boolean is
      function Internal (Widget : Address) return GBoolean;
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_get_show_right_margin"
             );
   begin
      return 0 /= Internal (Get_Object (Widget));
   end Get_Show_Right_Margin;

   function Get_Smart_Home_End
            (  Widget : not null access Gtk_Source_View_Record
            )  return Gtk_Source_Smart_Home_End_Type is
      function Internal (Widget : Address) return Int;
      pragma Import (C, Internal, "gtk_source_view_get_smart_home_end");
   begin
      return
         Gtk_Source_Smart_Home_End_Type'Val
         (  Internal (Get_Object (Widget))
         );
   end Get_Smart_Home_End;

   function Get_Tab_Width
            (  Widget : not null access Gtk_Source_View_Record
            )  return GUInt is
      function Internal (Widget : Address) return GUInt;
      pragma Import (C, Internal, "gtk_source_view_get_tab_width");
   begin
      return Internal (Get_Object (Widget));
   end Get_Tab_Width;

   procedure Gtk_New
             (  Widget : out Gtk_Source_View;
                Buffer : Gtk_Source_Buffer := null
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Gtk_Source_View_Record'Class,
                Gtk_Source_View
             );
   begin
      Widget := new Gtk_Source_View_Record;
      Gtk.Source_View.Initialize (Widget, Buffer);
   exception
      when others =>
         Free (Widget);
         raise;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access Gtk_Source_View_Record'Class;
                Buffer : Gtk_Source_Buffer
             )  is
   begin
      if Buffer = null then
         declare
            function Internal return Address;
            pragma Import (C, Internal, "gtk_source_view_new");
         begin
            Set_Object (Widget, Internal);
         end;
      else
         declare
            function Internal (Buffer : Address) return Address;
            pragma Import
                   (  C,
                      Internal,
                      "gtk_source_view_new_with_buffer"
                   );
         begin
            Set_Object (Widget, Internal (Get_Object (Buffer)));
         end;
      end if;
   end Initialize;

   procedure Set_Auto_Indent
             (  Widget : not null access Gtk_Source_View_Record;
                Enable : Boolean
             )  is
      procedure Internal (Widget : Address; Enable : GBoolean);
      pragma Import (C, Internal, "gtk_source_view_set_auto_indent");
   begin
      if Enable then
         Internal (Get_Object (Widget), 1);
      else
         Internal (Get_Object (Widget), 0);
      end if;
   end Set_Auto_Indent;

   procedure Set_Draw_Spaces
             (  Widget : not null access Gtk_Source_View_Record;
                Flags  : Gtk_Source_Draw_Spaces_Flags
             )  is
      procedure Internal (Widget : Address; Flags : Unsigned);
      pragma Import (C, Internal, "gtk_source_view_set_draw_spaces");
   begin
      Internal
      (  Get_Object (Widget),
         Gtk_Source_Draw_Spaces_Flags'Pos (Flags)
      );
   end Set_Draw_Spaces;

   procedure Set_Highlight_Current_Line
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             )  is
      procedure Internal (Widget : Address; Enable : GBoolean);
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_set_highlight_current_line"
             );
   begin
      if Show then
         Internal (Get_Object (Widget), 1);
      else
         Internal (Get_Object (Widget), 0);
      end if;
   end Set_Highlight_Current_Line;

   procedure Set_Indent_On_Tab
             (  Widget : not null access Gtk_Source_View_Record;
                Enable : Boolean
             )  is
      procedure Internal (Widget : Address; Enable : GBoolean);
      pragma Import (C, Internal, "gtk_source_view_set_indent_on_tab");
   begin
      if Enable then
         Internal (Get_Object (Widget), 1);
      else
         Internal (Get_Object (Widget), 0);
      end if;
   end Set_Indent_On_Tab;

   procedure Set_Indent_Width
             (  Widget : not null access Gtk_Source_View_Record;
                Width  : GInt := -1
             )  is
      procedure Internal (Widget : Address; Width : GInt);
      pragma Import (C, Internal, "gtk_source_view_set_indent_width");
   begin
      Internal (Get_Object (Widget), Width);
   end Set_Indent_Width;

   procedure Set_Insert_Spaces_Instead_Of_Tabs
             (  Widget : not null access Gtk_Source_View_Record;
                Enable : Boolean
             )  is
      procedure Internal (Widget : Address; Enable : GBoolean);
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_set_insert_spaces_instead_of_tabs"
             );
   begin
      if Enable then
         Internal (Get_Object (Widget), 1);
      else
         Internal (Get_Object (Widget), 0);
      end if;
   end Set_Insert_Spaces_Instead_Of_Tabs;

   procedure Set_Mark_Attributes
             (  Widget     : not null access Gtk_Source_View_Record;
                Category   : UTF8_String;
                Attributes : not null access
                             Gtk_Source_Mark_Atributes_Record'Class;
                Priority   : GInt
             )  is
      procedure Internal
                (  Widget     : Address;
                   Category   : char_array;
                   Attributes : Address;
                   Priority   : GInt
                );
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_set_mark_attributes"
             );
   begin
      Internal
      (  Get_Object (Widget),
         To_C (Category),
         Get_Object (Attributes),
         Priority
      );
   end Set_Mark_Attributes;

   procedure Set_Right_Margin_Position
             (  Widget   : not null access Gtk_Source_View_Record;
                Position : GUInt
             )  is
      procedure Internal (Widget : Address; Pos : GUInt);
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_set_right_margin_position"
             );
   begin
      Internal (Get_Object (Widget), Position);
   end Set_Right_Margin_Position;

   procedure Set_Show_Line_Marks
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             )  is
      procedure Internal (Widget : Address; Enable : GBoolean);
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_set_show_line_marks"
             );
   begin
      if Show then
         Internal (Get_Object (Widget), 1);
      else
         Internal (Get_Object (Widget), 0);
      end if;
   end Set_Show_Line_Marks;

   procedure Set_Show_Line_Numbers
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             )  is
      procedure Internal (Widget : Address; Enable : GBoolean);
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_set_show_line_numbers"
             );
   begin
      if Show then
         Internal (Get_Object (Widget), 1);
      else
         Internal (Get_Object (Widget), 0);
      end if;
   end Set_Show_Line_Numbers;

   procedure Set_Show_Right_Margin
             (  Widget : not null access Gtk_Source_View_Record;
                Show   : Boolean
             )  is
      procedure Internal (Widget : Address; Enable : GBoolean);
      pragma Import
             (  C,
                Internal,
                "gtk_source_view_set_show_right_margin"
             );
   begin
      if Show then
         Internal (Get_Object (Widget), 1);
      else
         Internal (Get_Object (Widget), 0);
      end if;
   end Set_Show_Right_Margin;

   procedure Set_Smart_Home_End
             (  Widget : not null access Gtk_Source_View_Record;
                Smart_Home_End  : Gtk_Source_Smart_Home_End_Type
             )  is
      procedure Internal (Widget : Address; Smart_Home_End : Int);
      pragma Import (C, Internal, "gtk_source_view_set_smart_home_end");
   begin
      Internal
      (  Get_Object (Widget),
         Gtk_Source_Smart_Home_End_Type'Pos (Smart_Home_End)
      );
   end Set_Smart_Home_End;

   procedure Set_Tab_Width
             (  Widget : not null access Gtk_Source_View_Record;
                Width  : GUInt
             )  is
      procedure Internal (Widget : Address; Width : GUInt);
      pragma Import (C, Internal, "gtk_source_view_set_tab_width");
   begin
      Internal (Get_Object (Widget), Width);
   end Set_Tab_Width;

end Gtk.Source_View;
