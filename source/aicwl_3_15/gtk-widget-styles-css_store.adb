--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Widget.Styles.CSS_Store                 Luebeck            --
--  Implementation                                 Spring, 2013       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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

with Gtk.Container;
with Gtk.Missed;

with System.Address_To_Access_Conversions;

package body Gtk.Widget.Styles.CSS_Store is

   function Style_Get
     (Widget        : access Gtk_Widget_Record'Class;
      Property_Name : UTF8_String) return Gtk_Border
   is
      package Conversions is
        new System.Address_To_Access_Conversions (Gtk_Border);
      Result : Gtk_Border;
      Value  : GValue;
      Ptr    : System.Address;

      use type System.Address;
   begin
      Init (Value, Border_Get_Type);
      Style_Get_Property (Widget, Property_Name, Value);
      Ptr := Get_Boxed (Value);
      if Ptr = System.Null_Address then
         Unset (Value);
         raise Constraint_Error;
      else
         Result := Conversions.To_Pointer (Ptr).all;
         Unset (Value);
         return Result;
      end if;
   end Style_Get;

   procedure Put
     (File    : Ada.Text_IO.File_Type;
      Widget  : access Gtk_Widget_Record'Class;
      Recurse : Boolean;
      Prefix  : String)
   is
      Figure : constant array (0 .. 15) of Character :=
                  "0123456789ABCDEF";
      procedure Put_String (Text : String) is
         Value : Integer;
      begin
         Ada.Text_IO.Put (File, '"');
         for Index in Text'Range loop
            Value := Character'Pos (Text (Index));
            if Text (Index) = '\' or else Value in 16#00# .. 16#1F# then
               Ada.Text_IO.Put (File, "\");
               Ada.Text_IO.Put (File, Figure (Value / 64));
               Value := Value / 8;
               Ada.Text_IO.Put (File, Figure (Value / 8));
               Value := Value / 8;
               Ada.Text_IO.Put (File, Figure (Value));
            else
               Ada.Text_IO.Put (File, Text (Index));
            end if;
         end loop;
         Ada.Text_IO.Put (File, '"');
      end Put_String;

      procedure Put_Flags
        (Class_Of : Glib.Properties.Creation.Flags_Class;
         Mask     : Glong)
      is
         Flag  : Glib.Properties.Creation.Flags_Value;
         First : Boolean := True;

         use type Glib.Properties.Creation.Flags_Int_Value;
         use type Glib.Properties.Creation.Flags_Value;
      begin
         for Index in 0 .. Guint'Last loop
            Flag := Glib.Properties.Creation.Nth_Value (Class_Of, Index);
            exit when Flag = null;
            if
              0 /= (Glib.Properties.Creation.Value (Flag) and
                        Glib.Properties.Creation.Flags_Int_Value (Mask))
            then
               if First then
                  First := False;
               else
                  Ada.Text_IO.Put (File, "| ");
               end if;
               Ada.Text_IO.Put
                 (File,
                  Glib.Properties.Creation.Name (Flag));
            end if;
         end loop;
      end Put_Flags;

      function Get_Prefix return String is
      begin
         if Prefix'Length = 0 then
            return Get_Name (Widget);
         else
            return Prefix & "->" & Get_Name (Widget);
         end if;
      end Get_Prefix;

      New_Prefix : constant String := Get_Prefix;
   begin
      if
        Recurse and then
        Widget.all in Gtk.Container.Gtk_Container_Record'Class
      then
         declare
            Children : Widget_List.Glist :=
                         Gtk.Container.Get_Children
                           (Gtk.Container.Gtk_Container_Record'Class
                              (Widget.all)'Access);
            Child    : Widget_List.Glist := Widget_List.First (Children);

            use type Widget_List.Glist;
         begin
            while Child /= Widget_List.Null_List loop
               Put (File, Widget_List.Get_Data (Child), Recurse, New_Prefix);
               Child := Widget_List.Next (Child);
            end loop;
            Widget_List.Free (Children);
         exception
            when others =>
               Widget_List.Free (Children);
               raise;
         end;
      end if;
      declare
         Class_Name : constant String := Type_Name (Get_Type (Widget));
         Class : constant GObject_Class :=
                 Gtk.Missed.Class_From_Type (Get_Type (Widget));
         List  : constant Param_Spec_Array :=
                 Class_List_Style_Properties (Class);
      begin
         Ada.Text_IO.Put_Line
           (File,
            Class_Name & '#' & Get_Name (Widget));
         Ada.Text_IO.Put_Line (File, "{ /* " & New_Prefix & " */");
         for Index in List'Range loop
            declare
               Param      : constant Param_Spec := List (Index);
               Name       : constant String :=
                              Glib.Properties.Creation.Pspec_Name (Param);
               Param_Type : constant GType  :=
                              Glib.Properties.Creation.Value_Type (Param);
            begin
               Ada.Text_IO.Put_Line
                 (File,
                  "      /* "
                  & Glib.Properties.Creation.Description (Param)
                  & " */");
               Ada.Text_IO.Put (File, "      /* " & Type_Name (Param_Type));
               if Param_Type = GType_String then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_String :=
                               Glib.Properties.Creation.Param_Spec_String (Param);
                     Value : constant String :=
                             Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put (File, " Default is ");
                     Put_String (Glib.Properties.Creation.Default (This));
                     Ada.Text_IO.Put (File, " */");
                     Ada.Text_IO.New_Line (File);
                     Ada.Text_IO.Put
                       (File,
                        "   -" & Class_Name & "-" & Name & ": ");
                     Put_String (Value);
                     Ada.Text_IO.Put (File, ";");
                     Ada.Text_IO.New_Line (File);
                  end;
               elsif Param_Type = GType_Char then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Char :=
                               Glib.Properties.Creation.Param_Spec_Char (Param);
                     Value : constant Gchar := Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Gint8'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Gint8'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Gint8'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name & "-" & Name
                        & ":"
                        & Integer'Image (Gchar'Pos (Value))
                        & ";");
                  end;
               elsif Param_Type = GType_Double then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Double :=
                               Glib.Properties.Creation.Param_Spec_Double (Param);
                     Value : constant Gdouble :=
                             Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Gdouble'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Gdouble'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Gdouble'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Gdouble'Image (Value)
                        & ";");
                  end;
               elsif Param_Type = GType_Float then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Float :=
                               Glib.Properties.Creation.Param_Spec_Float (Param);
                     Value : constant Gfloat :=
                             Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Gfloat'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Gfloat'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Gfloat'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Gfloat'Image (Value)
                        & ";");
                  end;
               elsif Param_Type = GType_Long then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Long :=
                               Glib.Properties.Creation.Param_Spec_Long (Param);
                     Value : constant Glong := Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Glong'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Glong'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Glong'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Glong'Image (Value)
                        & ";");
                  end;
               elsif Param_Type = GType_Int then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Int :=
                               Glib.Properties.Creation.Param_Spec_Int (Param);
                     Value : constant Gint := Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Gint'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Gint'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Gint'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Gint'Image (Value)
                        & ";");
                  end;
               elsif Param_Type = GType_Uchar then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Uchar :=
                               Glib.Properties.Creation.Param_Spec_Uchar (Param);
                     Value : constant Guchar :=
                             Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Guint8'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Guint8'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Guint8'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Integer'Image (Guchar'Pos (Value))
                        & ";");
                  end;
               elsif Param_Type = GType_Uint then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Uint :=
                               Glib.Properties.Creation.Param_Spec_Uint (Param);
                     Value : constant Guint := Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Guint'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Guint'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Guint'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Guint'Image (Value)
                        & ";");
                  end;
               elsif Param_Type = GType_Ulong then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Ulong :=
                               Glib.Properties.Creation.Param_Spec_Ulong (Param);
                     Value : constant Gulong :=
                             Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " range "
                        & Gulong'Image (Glib.Properties.Creation.Minimum (This))
                        & " .."
                        & Gulong'Image (Glib.Properties.Creation.Maximum (This))
                        & " Default is"
                        & Gulong'Image (Glib.Properties.Creation.Default (This))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Gulong'Image (Value)
                        & ";");
                  end;
               elsif Param_Type = GType_Boolean then
                  declare
                     This  : constant Glib.Properties.Creation.Param_Spec_Boolean :=
                               Glib.Properties.Creation.Param_Spec_Boolean (Param);
                     Value : constant Boolean :=
                             Style_Get (Widget, Name);
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        " Default is"
                        & Integer'Image (Boolean'Pos (Glib.Properties.Creation.Default (This)))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Integer'Image (Boolean'Pos (Value))
                        & ";");
                  end;
               elsif Param_Type = Gdk_Color_Type then
                  declare
                     Value   : Gdk_Color;
                     Default : Gdk_Color;
                  begin
                     Ada.Text_IO.Put (File, " */");
                     Ada.Text_IO.New_Line (File);
                     Set_Rgb (Default, 0, 0, 0);
                     Value := Style_Get (Widget, Name, Default);
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ": #"
                        & Figure (Integer (Red   (Value) / (256 * 16)))
                        & Figure (Integer (Red   (Value) rem 16))
                        & Figure (Integer (Green (Value) / (256 * 16)))
                        & Figure (Integer (Green (Value) rem 16))
                        & Figure (Integer (Blue  (Value) / (256 * 16)))
                        & Figure (Integer (Blue  (Value) rem 16))
                        & ";");
                  end;
               elsif Gtk.Missed.Is_A (Param_Type, GType_Enum) then
                  declare
                     This     : constant Glib.Properties.Creation.Param_Spec_Enum :=
                                  Glib.Properties.Creation.Param_Spec_Enum (Param);
                     Value    : Glib.Properties.Creation.Enum_Value;
                     First    : Boolean := True;
                     Class_Of : constant Glib.Properties.Creation.Enum_Class :=
                                  Glib.Properties.Creation.Enumeration (This);

                     use type Glib.Properties.Creation.Enum_Value;
                  begin
                     Ada.Text_IO.Put (File, " (");
                     for Index in 0 .. Guint'Last loop
                        Value :=
                          Glib.Properties.Creation.Nth_Value (Class_Of, Index);
                        exit when Value = null;
                        if First then
                           First := False;
                        else
                           Ada.Text_IO.Put (File, ", ");
                        end if;
                        Ada.Text_IO.Put
                          (File,
                           Glib.Properties.Creation.Name (Value));
                     end loop;
                     Ada.Text_IO.Put_Line
                       (File,
                        ") Default is "
                        & Glib.Properties.Creation.Name
                            (Glib.Properties.Creation.Get_Value
                               (Class_Of,
                                Glib.Properties.Creation.Default (This)))
                        & " */");
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ":"
                        & Glib.Properties.Creation.Name
                            (Glib.Properties.Creation.Get_Value
                               (Class_Of,
                                Style_Get (Widget, Name)))
                        & ";");
                  end;
               elsif Gtk.Missed.Is_A (Param_Type, GType_Flags) then
                  declare
                     This     : constant Glib.Properties.Creation.Param_Spec_Flags :=
                                  Glib.Properties.Creation.Param_Spec_Flags (Param);
                     Class_Of : constant Glib.Properties.Creation.Flags_Class :=
                                  Glib.Properties.Creation.Flags_Enumeration (This);
                     Value    : Glib.Properties.Creation.Flags_Value;
                     First    : Boolean := True;

                     use type Glib.Properties.Creation.Flags_Value;
                  begin
                     Ada.Text_IO.Put (File, " (");
                     for Index in 0 .. Guint'Last loop
                        Value :=
                          Glib.Properties.Creation.Nth_Value (Class_Of, Index);
                        exit when Value = null;
                        if First then
                           First := False;
                        else
                           Ada.Text_IO.Put (File, "or ");
                        end if;
                        Ada.Text_IO.Put
                          (File,
                           Glib.Properties.Creation.Name (Value));
                     end loop;
                     Ada.Text_IO.Put_Line (File, ") Default is ");
                     Put_Flags (Class_Of, Glib.Properties.Creation.Default (This));
                     Ada.Text_IO.Put (File, " */");
                     Ada.Text_IO.New_Line (File);
                     Ada.Text_IO.Put
                       (File,
                        "   -" & Class_Name & "-" & Name & ": ");
                     Put_Flags (Class_Of, Style_Get (Widget, Name));
                     Ada.Text_IO.Put (File, ";");
                     Ada.Text_IO.New_Line (File);
                  end;
               elsif Param_Type = Border_Get_Type then
                  Ada.Text_IO.Put (File, " */");
                  Ada.Text_IO.New_Line (File);
                  declare
                     Border : Gtk_Border;
                  begin
                     Border := Style_Get (Widget, Name);
                     Ada.Text_IO.Put_Line
                       (File,
                        "   -"
                        & Class_Name
                        & "-"
                        & Name
                        & ": "
                        & Gint16'Image (Border.Left)
                        & " "
                        & Gint16'Image (Border.Right)
                        & " "
                        & Gint16'Image (Border.Top)
                        & " "
                        & Gint16'Image (Border.Bottom)
                        & ";");
                  exception
                     when Constraint_Error =>
                        Ada.Text_IO.Put_Line
                          (File,
                           "   -"
                           & Class_Name
                           & "-"
                           & Name
                           & ": 1 1 1 1; /* Undefined */");
                  end;
               else
                  Ada.Text_IO.Put (File, " */");
                  Ada.Text_IO.New_Line (File);
                  Ada.Text_IO.Put_Line
                    (File,
                     "/***" & Name & ": the type is not supported */");
               end if;
            end;
         end loop;
      end;
      Ada.Text_IO.Put_Line (File, "}");
   end Put;

   procedure Put_CSS_Styles
     (File    : Ada.Text_IO.File_Type;
      Widget  : not null access Gtk_Widget_Record'Class;
      Recurse : Boolean := True) is
   begin
      Put (File, Widget, Recurse, "");
   end Put_CSS_Styles;

   procedure Put_CSS_Styles
     (Widget  : not null access Gtk_Widget_Record'Class;
      Recurse : Boolean := True) is
   begin
      Put (Ada.Text_IO.Standard_Output, Widget, Recurse, "");
   end Put_CSS_Styles;

end Gtk.Widget.Styles.CSS_Store;
