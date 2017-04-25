--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Widget.Styles.Store                     Luebeck            --
--  Implementation                                 Spring, 2007       --
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
--____________________________________________________________________--

with Gdk.Color;                 use Gdk.Color;
with Gtk.Container;             use Gtk.Container;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Style;                 use Gtk.Style;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with System;                    use System;

with Gtk.RC;
with System.Address_To_Access_Conversions;

package body Gtk.Widget.Styles.Store is

   function Style_Get
            (  Widget        : access Gtk_Widget_Record'Class;
               Property_Name : UTF8_String
            )  return Gtk_Border is
      package Conversions is
         new System.Address_To_Access_Conversions (Gtk_Border);
      use Conversions;
      Result : Gtk_Border;
      Value  : GValue;
      Ptr    : Address;
   begin
      Init (Value, Border_Get_Type);
      Style_Get_Property (Widget, Property_Name, Value);
      Ptr := Get_Boxed (Value);
      if Ptr = Null_Address then
         Unset (Value);
         raise Constraint_Error;
      else
         Result := To_Pointer (Ptr).all;
         Unset (Value);
         return Result;
      end if;
--        procedure Internal
--                  (  Widget        : System.Address;
--                     Property_Name : Property;
--                     Border        : out Gtk_Border_Ptr;
--                     Terminator    : System.Address := System.Null_Address
--                  );
--        pragma Import (C, Internal, "gtk_widget_style_get");
--        procedure G_Free (Border : Gtk_Border_Ptr);
--        pragma Import (C, G_Free, "g_free");
--        Ptr    : Gtk_Border_Ptr;
--        Result : Gtk_Border;
--     begin
--        Internal (Get_Object (Widget), Build (Property_Name), Ptr);
--        if Ptr = null then
--           raise Constraint_Error;
--        else
--           Result := Ptr.all;
--           G_Free (Ptr);
--           return Result;
--        end if;
   end Style_Get;

   procedure Put
             (  File    : File_Type;
                Widget  : access Gtk_Widget_Record'Class;
                Recurse : Boolean;
                Prefix  : String
             )  is
      Figure : constant array (0..15) of Character :=
                  "0123456789ABCDEF";
      procedure Put_String (Text : String) is
         Value : Integer;
      begin
         Put (File, '"');
         for Index in Text'Range loop
            Value := Character'Pos (Text (Index));
            if Text (Index) = '\' or else Value in 16#00#..16#1F# then
               Put (File, "\");
               Put (File, Figure (Value / 64));
               Value := Value / 8;
               Put (File, Figure (Value / 8));
               Value := Value / 8;
               Put (File, Figure (Value));
            else
               Put (File, Text (Index));
            end if;
         end loop;
         Put (File, '"');
      end Put_String;

      procedure Put_Flags
                (  Class_Of : Flags_Class;
                   Mask     : GLong
                )  is
         Flag  : Flags_Value;
         First : Boolean := True;
      begin
         for Index in 0..GUInt'Last loop
            Flag := Nth_Value (Class_Of, Index);
            exit when Flag = null;
            if 0 /= (Value (Flag) and Flags_Int_Value (Mask)) then
               if First then
                  First := False;
               else
                  Put (File, "| ");
               end if;
               Put
               (  File,
                  GLib.Properties.Creation.Name (Flag)
               );
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
      if Recurse and then Widget.all in Gtk_Container_Record'Class then
         declare
            use Widget_List;
            Children : Widget_List.GList :=
               Get_Children
               (  Gtk_Container_Record'Class (Widget.all)'Access
               );
            Child : Widget_List.GList := First (Children);
         begin
            while Child /= Null_List loop
               Put (File, Get_Data (Child), Recurse, New_Prefix);
               Child := Next (Child);
            end loop;
            Free (Children);
         exception
            when others =>
               Free (Children);
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
         Put_Line
         (  File,
            (  "style "
            &  '"'
            &  Get_Name (Widget)
            &  "_of_"
            &  Class_Name
            &  '"'
         )  );
         Put_Line (File, "{ # " & New_Prefix);
         for Index in List'Range loop
            declare
               Param      : constant Param_Spec := List (Index);
               Name       : constant String := PSpec_Name (Param);
               Param_Type : constant GType  := Value_Type (Param);
            begin
               Put_Line (File, "      # " & Description (Param));
               Put (File, "      # " & Type_Name (Param_Type));
               if Param_Type = GType_String then
                  declare
                     This  : constant Param_Spec_String :=
                             Param_Spec_String (Param);
                     Value : constant String :=
                             Style_Get (Widget, Name);
                  begin
                     Put (File, " Default is ");
                     Put_String (Default (This));
                     New_Line (File);
                     Put
                     (  File,
                        "   " & Class_Name & "::" & Name & " = "
                     );
                     Put_String (Value);
                     New_Line (File);
                  end;
               elsif Param_Type = GType_Char then
                  declare
                     This  : constant Param_Spec_Char :=
                             Param_Spec_Char (Param);
                     Value : constant GChar := Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GInt8'Image (Minimum (This))
                        &  " .."
                        &  GInt8'Image (Maximum (This))
                        &  " Default is"
                        &  GInt8'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name & "::" & Name
                        &  " ="
                        &  Integer'Image (GChar'Pos (Value))
                     )  );
                  end;
               elsif Param_Type = GType_Double then
                  declare
                     This  : constant Param_Spec_Double :=
                             Param_Spec_Double (Param);
                     Value : constant GDouble :=
                             Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GDouble'Image (Minimum (This))
                        &  " .."
                        &  GDouble'Image (Maximum (This))
                        &  " Default is"
                        &  GDouble'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  GDouble'Image (Value)
                     )  );
                  end;
               elsif Param_Type = GType_Float then
                  declare
                     This  : constant Param_Spec_Float :=
                             Param_Spec_Float (Param);
                     Value : constant GFloat :=
                             Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GFloat'Image (Minimum (This))
                        &  " .."
                        &  GFloat'Image (Maximum (This))
                        &  " Default is"
                        &  GFloat'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  GFloat'Image (Value)
                     )  );
                  end;
               elsif Param_Type = GType_Long then
                  declare
                     This  : constant Param_Spec_Long :=
                             Param_Spec_Long (Param);
                     Value : constant GLong := Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GLong'Image (Minimum (This))
                        &  " .."
                        &  GLong'Image (Maximum (This))
                        &  " Default is"
                        &  GLong'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  GLong'Image (Value)
                     )  );
                  end;
               elsif Param_Type = GType_Int then
                  declare
                     This  : constant Param_Spec_Int :=
                             Param_Spec_Int (Param);
                     Value : constant GInt := Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GInt'Image (Minimum (This))
                        &  " .."
                        &  GInt'Image (Maximum (This))
                        &  " Default is"
                        &  GInt'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  GInt'Image (Value)
                     )  );
                  end;
               elsif Param_Type = GType_UChar then
                  declare
                     This  : constant Param_Spec_UChar :=
                             Param_Spec_UChar (Param);
                     Value : constant GUChar :=
                             Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GUInt8'Image (Minimum (This))
                        &  " .."
                        &  GUInt8'Image (Maximum (This))
                        &  " Default is"
                        &  GUInt8'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  Integer'Image (GUChar'Pos (Value))
                     )  );
                  end;
               elsif Param_Type = GType_UInt then
                  declare
                     This  : constant Param_Spec_UInt :=
                             Param_Spec_UInt (Param);
                     Value : constant GUInt := Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GUInt'Image (Minimum (This))
                        &  " .."
                        &  GUInt'Image (Maximum (This))
                        &  " Default is"
                        &  GUInt'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  GUInt'Image (Value)
                     )  );
                  end;
               elsif Param_Type = GType_ULong then
                  declare
                     This  : constant Param_Spec_ULong :=
                             Param_Spec_ULong (Param);
                     Value : constant GULong :=
                             Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " range "
                        &  GULong'Image (Minimum (This))
                        &  " .."
                        &  GULong'Image (Maximum (This))
                        &  " Default is"
                        &  GULong'Image (Default (This))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  GULong'Image (Value)
                     )  );
                  end;
               elsif Param_Type = GType_Boolean then
                  declare
                     This  : constant Param_Spec_Boolean :=
                             Param_Spec_Boolean (Param);
                     Value : constant Boolean :=
                             Style_Get (Widget, Name);
                  begin
                     Put_Line
                     (  File,
                        (  " Default is"
                        &  Integer'Image (Boolean'Pos (Default (This)))
                     )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " ="
                        &  Integer'Image (Boolean'Pos (Value))
                     )  );
                  end;
               elsif Param_Type = Gdk_Color_Type then
                  declare
                     Value   : Gdk_Color;
                     Default : Gdk_Color;
                  begin
                     New_Line (File);
                     Set_RGB (Default, 0, 0, 0);
                     Value := Style_Get (Widget, Name, Default);
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " = ""#"
                        &  Figure (Integer (Red   (Value) / (256 * 16)))
                        &  Figure (Integer (Red   (Value) rem 16))
                        &  Figure (Integer (Green (Value) / (256 * 16)))
                        &  Figure (Integer (Green (Value) rem 16))
                        &  Figure (Integer (Blue  (Value) / (256 * 16)))
                        &  Figure (Integer (Blue  (Value) rem 16))
                        &  '"'
                     )  );
                  end;
               elsif Is_A (Param_Type, GType_Enum) then
                  declare
                     This     : constant Param_Spec_Enum :=
                                Param_Spec_Enum (Param);
                     Value    : Enum_Value;
                     First    : Boolean := True;
                     Class_Of : constant Enum_Class :=
                                Enumeration (This);
                  begin
                     Put (File, " (");
                     for Index in 0..GUInt'Last loop
                        Value := Nth_Value (Class_Of, Index);
                        exit when Value = null;
                        if First then
                           First := False;
                        else
                           Put (File, ", ");
                        end if;
                        Put
                        (  File,
                           GLib.Properties.Creation.Name (Value)
                        );
                     end loop;
                     Put_Line
                     (  File,
                        (  ") Default is "
                        &  GLib.Properties.Creation.Name
                           (  Get_Value
                              (  Class_Of,
                                 Default (This)
                     )  )  )  );
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " = "
                        &  GLib.Properties.Creation.Name
                           (  Get_Value
                              (  Class_Of,
                                 Style_Get (Widget, Name)
                     )  )  )  );
                  end;
               elsif Is_A (Param_Type, GType_Flags) then
                  declare
                     This     : constant Param_Spec_Flags :=
                                Param_Spec_Flags (Param);
                     Class_Of : constant Flags_Class :=
                                Flags_Enumeration (This);
                     Value    : Flags_Value;
                     First    : Boolean := True;
                  begin
                     Put (File, " (");
                     for Index in 0..GUInt'Last loop
                        Value := Nth_Value (Class_Of, Index);
                        exit when Value = null;
                        if First then
                           First := False;
                        else
                           Put (File, "or ");
                        end if;
                        Put
                        (  File,
                           GLib.Properties.Creation.Name (Value)
                        );
                     end loop;
                     Put_Line (File, ") Default is ");
                     Put_Flags (Class_Of, Default (This));
                     New_Line (File);
                     Put
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " = "
                     )  );
                     Put_Flags (Class_Of, Style_Get (Widget, Name));
                     New_Line (File);
                  end;
               elsif Param_Type = Border_Get_Type then
                  New_Line (File);
                  declare
                     Border : Gtk_Border;
                  begin
                     Border := Style_Get (Widget, Name);
                     Put_Line
                     (  File,
                        (  "   "
                        &  Class_Name
                        &  "::"
                        &  Name
                        &  " = {"
                        &  GInt16'Image (Border.Left)
                        &  ","
                        &  GInt16'Image (Border.Right)
                        &  ","
                        &  GInt16'Image (Border.Top)
                        &  ","
                        &  GInt16'Image (Border.Bottom)
                        &  "}"
                     )  );
                  exception
                     when Constraint_Error =>
                        Put_Line
                        (  File,
                           (  "   "
                           &  Class_Name
                           &  "::"
                           &  Name
                           &  " = {1,1,1,1} # Undefined"
                        )  );
                  end;
               else
                  New_Line (File);
                  Put_Line
                  (  File,
                     (  "###"
                     &  Name
                     &  " = the type is not supported"
                  )  );
               end if;
            end;
         end loop;
      end;
      Put_Line (File, "}");
   end Put;

   procedure Put_Styles
             (  File    : File_Type;
                Widget  : not null access Gtk_Widget_Record'Class;
                Recurse : Boolean := True
             )  is
   begin
      Put (File, Widget, Recurse, "");
   end Put_Styles;

   procedure Put_Styles
             (  Widget  : not null access Gtk_Widget_Record'Class;
                Recurse : Boolean := True
             )  is
   begin
      Put (Standard_Output, Widget, Recurse, "");
   end Put_Styles;
end Gtk.Widget.Styles.Store;
