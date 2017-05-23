--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Image_Button                            Luebeck            --
--  Implementation                                 Winter, 2007       --
--                                                                    --
--                                Last revision :  19:57 08 Aug 2015  --
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
-- __________________________________________________________________ --

with Glib.Object.Checked_Destroy;

with Gtk.Image;

package body Gtk.Image_Button is

   function Get_Box
     (Button : not null access Gtk_Image_Button_Record) return Gtk.Box.Gtk_Box
   is
   begin
      return Button.all.Box;
   end Get_Box;

   function Get_Label
     (Button : not null access Gtk_Image_Button_Record)
      return Gtk.Label.Gtk_Label is
   begin
      return Button.all.Label;
   end Get_Label;

   procedure Gtk_New
     (Button : out Gtk_Image_Button;
      Image  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Label  : UTF8_String := "")
   is
      Widget : Gtk_Image_Button;
   begin
      Widget := new Gtk_Image_Button_Record;
      Initialize (Widget, Image, Label);
      Button := Widget;
   exception
      when others =>
         Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
     (Button   : out Gtk_Image_Button;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size;
      Label    : UTF8_String := "")
   is
      Image : Gtk.Image.Gtk_Image;

      use type Gtk.Image.Gtk_Image;
   begin
      Gtk.Image.Gtk_New (Image, Stock_Id, Size);
      Gtk_New (Button, Image, Label);
   exception
      when others =>
         if Image /= null then
            Gtk.Image.Unref (Image);
            raise;
         end if;
   end Gtk_New;

   procedure Initialize
     (Button : not null access Gtk_Image_Button_Record'Class;
      Image  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      Label  : UTF8_String) is
   begin
      Gtk.Button.Initialize (Button, "");
      Gtk.Label.Gtk_New (Button.all.Label, Label);
      Gtk.Box.Gtk_New_Hbox (Button.all.Box, False, 0);
      Button.all.Box.all.Set_Border_Width (0);
      Button.all.Box.all.Pack_Start (Image, False, False);
      Button.all.Box.all.Pack_Start (Button.all.Label, False, False);
      Image.all.Show;
      Button.all.Label.all.Show;
      Button.all.Add (Button.all.Box);
   end Initialize;

end Gtk.Image_Button;
