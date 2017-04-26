-----------------------------------------------------------------------
--                    GtkAda Contributions                           --
--                                                                   --
--   Copyright (C) 2011 O. Kellogg <okellogg@users.sourceforge.net>  --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;
with GLib;
with GLib.Object.Checked_Destroy;

package body Gtk.Generic_Enum_Combo_Box is
   use Gtk.Combo_Box_Text;

   To_Space_Mapping : constant Ada.Strings.Maps.Character_Mapping :=
                               Ada.Strings.Maps.To_Mapping ("_", " ");

   function Apply_Style
            (  To    : String;
               Style : Gtk.Missed.Enum_Style_Type;
               Replace_Underscore_By_Space : Boolean
            )  return String is
      Output : String (To'Range);
   begin
      case Style is
         when Gtk.Missed.Mixed_Case =>
            declare
               package CH renames Ada.Characters.Handling;
               C : Character;
            begin
               for I in To'Range loop
                  if I = To'First or else To (I - 1) = '_' then
                     C := CH.To_Upper (To (I));
                  else
                     C := CH.To_Lower (To (I));
                  end if;
                  Output (I) := C;
               end loop;
            end;
         when Gtk.Missed.ALL_CAPS =>
            null;
         when Gtk.Missed.all_small =>
            declare
               Lc : constant Ada.Strings.Maps.Character_Mapping :=
                  Ada.Strings.Maps.Constants.Lower_Case_Map;
            begin
               Output := Ada.Strings.Fixed.Translate (To, Lc);
            end;
         when Gtk.Missed.Capitalize_First =>
            Output := Ada.Characters.Handling.To_Lower (To);
            Output (1) := Ada.Characters.Handling.To_Upper (Output (1));
      end case;
      if Replace_Underscore_By_Space then
         Output :=
            Ada.Strings.Fixed.Translate (Output, To_Space_Mapping);
      end if;
      return Output;
   end Apply_Style;

   procedure Initialize
             (  Combo : not null access Gtk_Enum_Combo_Box_Record'Class;
                Style : in Gtk.Missed.Enum_Style_Type :=
                           Gtk.Missed.Mixed_Case;
                Replace_Underscore_By_Space : in Boolean := False
             )  is
   begin
      Gtk.Combo_Box_Text.Initialize (Combo);
      for I in Enum_Type loop
         Combo.Append_Text
         (  Apply_Style
            (  Enum_Type'Image (I),
               Style,
               Replace_Underscore_By_Space
         )  );
      end loop;
   end Initialize;

   procedure Gtk_New
             (  Combo : out Gtk_Enum_Combo_Box;
                Style : in Gtk.Missed.Enum_Style_Type :=
                           Gtk.Missed.Mixed_Case;
                Replace_Underscore_By_Space : in Boolean := False
             )  is
   begin
      Combo := new Gtk_Enum_Combo_Box_Record;
      Initialize (Combo, Style, Replace_Underscore_By_Space);
   exception
      when others =>
         GLib.Object.Checked_Destroy (Combo);
         Combo := null;
         raise;
   end Gtk_New;

   procedure Set_Active_Value
             (  Combo : not null access Gtk_Enum_Combo_Box_Record;
                Value : in Enum_Type
             )  is
   begin
      Combo.Set_Active (Enum_Type'Pos (Value));
   end Set_Active_Value;

   function Get_Active_Value
            (  Combo : not null access Gtk_Enum_Combo_Box_Record
            )  return Enum_Type is
      Active_Index : constant Glib.Gint := Combo.Get_Active;
      use type Glib.Gint;
   begin
      if Active_Index < 0 then
         raise Gtk.Missed.No_Selection;
      end if;
      return Enum_Type'Val (Active_Index);
   end Get_Active_Value;

end Gtk.Generic_Enum_Combo_Box;
