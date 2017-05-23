--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Style_Button                    Luebeck            --
--  Implementation                                 Spring, 2007       --
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
-- __________________________________________________________________ --

with Glib.Properties.Creation;
with Glib.Properties.Icon_Size;
with Glib.Properties.Relief_Style;
with Glib.Types;

with Gtk.Widget.Styles.Icon_Size;
with Gtk.Widget.Styles.Relief_Style;

package body Gtk.Generic_Style_Button is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Get_Box
     (Button : not null access Gtk_Style_Button_Record)
      return Gtk.Box.Gtk_Box is
   begin
      return Button.all.Box;
   end Get_Box;

   function Get_Label
     (Button : not null access Gtk_Style_Button_Record)
      return Gtk.Label.Gtk_Label is
   begin
      return Button.all.Label;
   end Get_Label;

   function Get_Type return GType is
   begin
      if
        Initialize_Class_Record
          (Ancestor     => Gtk.Button.Get_Type,
           Class_Record => Class_Record'Access,
           Type_Name    => Class_Name)
      then
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Boolean
              (Name    => "icon-left",
               Nick    => "Left",
               Blurb   => "Button icon located left of the label",
               Default => Icon_Left));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
            (Name    => "icon-id",
             Nick    => "Icon",
             Blurb   => "Button icon (stock id)",
             Default => Icon));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Icon_Size.Property.Gnew_Enum
              (Name    => "icon-size",
               Nick    => "Size",
               Blurb   => "Button icon size",
               Default =>
                 Glib.Properties.Icon_Size.Gtk_Icon_Size_Enum'Val (Size)));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "label",
               Nick    => "Label",
               Blurb   => "Button label",
               Default => Label));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Relief_Style.Property.Gnew_Enum
              (Name    => "relief-style",
               Nick    => "Relief",
               Blurb   => "Button relief style",
               Default => Relief));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_Uint
              (Name    => "spacing",
               Nick    => "Spacing",
               Blurb   => "Spacing between icon and label",
               Minimum => 0,
               Maximum => Guint (Gint'Last),
               Default => Spacing));
         Gtk.Widget.Install_Style_Property
           (Glib.Types.Class_Ref (Class_Record.all.The_Type),
            Glib.Properties.Creation.Gnew_String
              (Name    => "tip",
               Nick    => "Tip",
               Blurb   => "Button tip",
               Default => Tip));
      end if;
      return Class_Record.all.The_Type;
   end Get_Type;

   procedure Gtk_New (Button : out Gtk_Style_Button) is
      Widget : Gtk_Style_Button;
   begin
      Widget := new Gtk_Style_Button_Record;
      Gtk.Generic_Style_Button.Initialize (Widget);
      Button := Widget;
   exception
      when others =>
         if Widget /= null then
            Unref (Widget);
            raise;
         end if;
   end Gtk_New;

   procedure Initialize
     (Button : not null access Gtk_Style_Button_Record'Class) is
   begin
      G_New (Button, Get_Type);
      Gtk.Button.Initialize (Button, ""); -- Parent's initialization
      Gtk.Box.Gtk_New_Hbox (Button.all.Box, False, 0);
      Button.all.Box.all.Set_Border_Width (0);
      Button.all.Add (Button.all.Box);
      Style_Handlers.Connect
        (Button,
         "style-updated",
         Style_Handlers.To_Marshaller (Style_Updated'Access));
      Style_Updated (Button);
   end Initialize;

   procedure Style_Updated
     (Button : access Gtk_Style_Button_Record'Class)
   is
      Text : constant String := Gtk.Widget.Styles.Style_Get (Button, "label");

      use type Gtk.Image.Gtk_Image;
      use type Gtk.Label.Gtk_Label;
   begin
      Button.all.Set_Relief
        (Gtk.Widget.Styles.Relief_Style.Style_Get (Button, "relief-style"));
      if Text'Length = 0 then
         if Button.all.Label /= null then
            Gtk.Box.Remove (Button.all.Box, Button.all.Label);
            Button.all.Label := null;
         end if;
      else
         if Button.all.Label = null then
            Gtk.Label.Gtk_New (Button.all.Label, Text);
            Button.all.Label.all.Ref;
         else
            Button.all.Label.all.Ref;
            Gtk.Box.Remove (Button.all.Box, Button.all.Label);
            Button.all.Label.all.Set_Text (Text);
         end if;
      end if;
      if Button.all.Image /= null then
         Gtk.Box.Remove (Button.all.Box, Button.all.Image);
      end if;
      Gtk.Image.Gtk_New
        (Button.all.Image,
         Gtk.Widget.Styles.Style_Get (Button, "icon-id"),
         Glib.Properties.Icon_Size.Gtk_Icon_Size_Enum'Pos
           (Gtk.Widget.Styles.Icon_Size.Style_Get (Button, "icon-size")));
      if Gtk.Widget.Styles.Style_Get (Button, "icon-left") then
         Button.all.Box.all.Pack_Start (Button.all.Image, False, False);
         if Button.all.Label /= null then
            Button.all.Box.all.Pack_Start (Button.all.Label, False, False);
            Button.all.Label.all.Unref;
         end if;
      else
         if Button.all.Label /= null then
            Button.all.Box.all.Pack_Start (Button.all.Label, False, False);
            Button.all.Label.all.Unref;
         end if;
         Button.all.Box.all.Pack_Start (Button.all.Image, False, False);
      end if;
      Button.all.Box.all.Set_Spacing
        (Gint (Guint'(Gtk.Widget.Styles.Style_Get (Button, "spacing"))));
      Button.all.Set_Tooltip_Text (Gtk.Widget.Styles.Style_Get (Button, "tip"));
      Button.all.Box.all.Show_All;
   end Style_Updated;

end Gtk.Generic_Style_Button;
