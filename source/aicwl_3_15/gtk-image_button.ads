--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Image_Button                            Luebeck            --
--  Interface                                      Winter, 2007       --
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

with Gtk.Box;     use Gtk.Box;
with Gtk.Button;  use Gtk.Button;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Label;   use Gtk.Label;
with Gtk.Widget;  use Gtk.Widget;

package Gtk.Image_Button is
--
-- Gtk_Image_Button_Record -- The button type
--
-- The  button  is  a  button  with  a  label. From the container widget
-- Set_Style you might wish to set:
--
--    Set_Text (Get_Label (Button), ...);
--    Set_Tip  (..., Button, ...);
--    Set_Spacing (Get_Box (Button), ...); 
--
   type Gtk_Image_Button_Record is new Gtk_Button_Record with private;
   type Gtk_Image_Button is access all Gtk_Image_Button_Record'Class;
--
-- Get_Box -- Get the label box of the button
--
--    Button - The button
--
-- Returns :
--
--    The label box of the button
--
   function Get_Box
            (  Button : not null access Gtk_Image_Button_Record
            )  return Gtk_Box;
--
-- Get_Label -- Get the label of the button
--
--    Button - The button
--
-- Returns :
--
--    The label of the button
--
   function Get_Label
            (  Button : not null access Gtk_Image_Button_Record
            )  return Gtk_Label;
--
-- Gtk_New -- Factory
--
--    Button - The button (the result)
--    Image  - Of the button
--    Label  - The button label
--
   procedure Gtk_New
             (  Button : out Gtk_Image_Button;
                Image  : not null access Gtk_Widget_Record'Class;
                Label  : UTF8_String := ""
             );
--
-- Gtk_New -- Factory
--
--    Button   - The button (the result)
--    Stock_Id - Of the button's image
--    Size     - Of the image
--    Label    - The button label
--
   procedure Gtk_New
             (  Button   : out Gtk_Image_Button;
                Stock_Id : String;
                Size     : Gtk_Icon_Size;
                Label    : UTF8_String := ""
             );
--
-- Initialize -- Construction
--
--    Button - The button to initialize
--    Image  - Of the button
--    Label  - The button label
--
-- Each derived type is responsible to  call  this  procedure  upon  its
-- construction. 
--
   procedure Initialize
             (  Button : not null access Gtk_Image_Button_Record'Class;
                Image  : not null access Gtk_Widget_Record'Class;
                Label  : UTF8_String
             );

private
   type Gtk_Image_Button_Record is new Gtk_Button_Record with record
      Label : Gtk_Label;
      Box   : Gtk_Box;
   end record;

end Gtk.Image_Button;
