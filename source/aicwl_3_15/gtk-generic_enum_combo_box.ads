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

with Gtk.Combo_Box_Text;
with Gtk.Missed;

generic
   type Enum_Type is (<>);

package Gtk.Generic_Enum_Combo_Box is

   type Gtk_Enum_Combo_Box_Record is
      new Gtk.Combo_Box_Text.Gtk_Combo_Box_Text_Record with null record;
   type Gtk_Enum_Combo_Box is access all Gtk_Enum_Combo_Box_Record'Class;

   procedure Gtk_New
             (  Combo : out Gtk_Enum_Combo_Box;
                Style : in Gtk.Missed.Enum_Style_Type :=
                           Gtk.Missed.Mixed_Case;
                Replace_Underscore_By_Space : in Boolean := False
             );

   -- Alternative form of initialization (using classwide access)
   procedure Initialize
             (  Combo : not null access Gtk_Enum_Combo_Box_Record'Class;
                Style : in Gtk.Missed.Enum_Style_Type :=
                           Gtk.Missed.Mixed_Case;
                Replace_Underscore_By_Space : in Boolean := False
             );

   procedure Set_Active_Value
             (  Combo : not null access Gtk_Enum_Combo_Box_Record;
                Value : in Enum_Type
             );

   function Get_Active_Value
            (  Combo : not null access Gtk_Enum_Combo_Box_Record
            )  return Enum_Type;
   -- Raises exception No_Selection if no value is active.

end Gtk.Generic_Enum_Combo_Box;
