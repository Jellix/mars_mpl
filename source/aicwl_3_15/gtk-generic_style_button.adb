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
--____________________________________________________________________--

with GLib.Properties.Creation;     use GLib.Properties.Creation;
with GLib.Properties.Icon_Size;    use GLib.Properties.Icon_Size;
with GtkAda.Types;                 use GtkAda.Types;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;  use Gtk.Widget.Styles.Icon_Size;

with GLib.Properties.Relief_Style;
with GLib.Types;
with Gtk.Widget.Styles.Relief_Style;

package body Gtk.Generic_Style_Button is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Get_Box
            (  Button : not null access Gtk_Style_Button_Record
            )  return Gtk_Box is
   begin
      return Button.Box;
   end Get_Box;

   function Get_Label
            (  Button : not null access Gtk_Style_Button_Record
            )  return Gtk_Label is
   begin
      return Button.Label;
   end Get_Label;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Button.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class_Record.The_Type),
            Gnew_Boolean
            (  Name    => "icon-left",
               Nick    => "Left",
               Blurb   => "Button icon located left of the label",
               Default => Icon_Left
         )  );
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "icon-id",
               Nick    => "Icon",
               Blurb   => "Button icon (stock id)",
               Default => Icon
         )  );
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class_Record.The_Type),
            GLib.Properties.Icon_Size.Property.Gnew_Enum
            (  Name    => "icon-size",
               Nick    => "Size",
               Blurb   => "Button icon size",
               Default => Gtk_Icon_Size_Enum'Val (Size)
         )  );
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "label",
               Nick    => "Label",
               Blurb   => "Button label",
               Default => Label
         )  );
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class_Record.The_Type),
            GLib.Properties.Relief_Style.Property.Gnew_Enum
            (  Name    => "relief-style",
               Nick    => "Relief",
               Blurb   => "Button relief style",
               Default => Relief
         )  );
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "spacing",
               Nick    => "Spacing",
               Blurb   => "Spacing between icon and label",
               Minimum => 0,
               Maximum => GUInt (GInt'Last),
               Default => Spacing
         )  );
         Install_Style_Property
         (  GLib.Types.Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "tip",
               Nick    => "Tip",
               Blurb   => "Button tip",
               Default => Tip
         )  );
      end if;
      return Class_Record.The_Type;
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
             (  Button : not null access Gtk_Style_Button_Record'Class
             )  is
   begin
      G_New (Button, Get_Type);
      Gtk.Button.Initialize (Button, ""); -- Parent's initialization
      Gtk_New_HBox (Button.Box, False, 0);
      Button.Box.Set_Border_Width (0);
      Button.Add (Button.Box);
      Style_Handlers.Connect
      (  Button,
         "style-updated",
         Style_Handlers.To_Marshaller (Style_Updated'Access)
      );
      Style_Updated (Button);
   end Initialize;

   procedure Style_Updated
             (  Button : access Gtk_Style_Button_Record'Class
             )  is
      use Gtk.Widget.Styles.Relief_Style;
      Text : constant String := Style_Get (Button, "label");
   begin
      Button.Set_Relief (Style_Get (Button, "relief-style"));
      if Text'Length = 0 then
         if Button.Label /= null then
            Remove (Button.Box, Button.Label);
            Button.Label := null;
         end if;
      else
         if Button.Label = null then
            Gtk_New (Button.Label, Text);
            Button.Label.Ref;
         else
            Button.Label.Ref;
            Remove (Button.Box, Button.Label);
            Button.Label.Set_Text (Text);
         end if;
      end if;
      if Button.Image /= null then
         Remove (Button.Box, Button.Image);
      end if;
      Gtk_New
      (  Button.Image,
         Style_Get (Button, "icon-id"),
         Gtk_Icon_Size_Enum'Pos (Style_Get (Button, "icon-size"))
      );
      if Style_Get (Button, "icon-left") then
         Button.Box.Pack_Start (Button.Image, False, False);
         if Button.Label /= null then
            Button.Box.Pack_Start (Button.Label, False, False);
            Button.Label.Unref;
         end if;
      else
         if Button.Label /= null then
            Button.Box.Pack_Start (Button.Label, False, False);
            Button.Label.Unref;
         end if;
         Button.Box.Pack_Start (Button.Image, False, False);
      end if;
      Button.Box.Set_Spacing
      (  GInt (GUInt'(Style_Get (Button, "spacing")))
      );
      Button.Set_Tooltip_Text (Style_Get (Button, "tip"));
      Button.Box.Show_All;
   end Style_Updated;

end Gtk.Generic_Style_Button;
