--                                                                    --
--  procedure XPM2GtkAda            Copyright (c)  Dmitry A. Kazakov  --
--     XPM to GtkAda converter                     Luebeck            --
--                                                 Summer, 2006       --
--  Implementation                                                    --
--                                Last revision :  07:53 21 Jul 2016  --
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
--
--  XPM  to  GtkAda converter is a small utility which reads an XPM file
--  from  the  standard input and creates Ada packages for embedding the
--  image into a GtkAda application. The generated files are named after
--  the  XPM  image  name  stored in the XPM file. Each XPM file usually
--  starts as:
--
--      /* XPM */
--      static char * <name> [] = {
--
--  The files created will be named as:
--
--     <name>.ads       -- Contains pixels and pixbuf creation function
--     <name>-image.ads -- Contains a child package to get a Gtk_Image
--     <name>-image.adb -- The implementation of
--
--     The package <name>.ads will look like:
--
--     package <name> is
--        X_Size : constant GInt := ...
--        Y_Size : constant GInt := ...
--        type Pixbuf_Image is array (Natural range ...) of GUChar;
--        pragma Convention (C, Pixbuf_Image);
--        Pixels : constant Pixbuf_Image := ...
--        function Get_Pixbuf ...
--           return Gdk_Pixbuf;
--
--     The  function  Pixbuf  can be used to the image in Pixbuf format.
--     The  result  object  references  to the data in the array Pixels.
--     Basically, it is a just a reference.
--
--     The  child  function  <name>-image.ads  provides  an  easy way to
--     create an image object on demand:
--
--        function <name>.Image return Gtk_Image;
--
--     It  creates  and  returns a Gtk_Image object corresponding to the
--     image.
--
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with Parsers.Multiline_Source.XPM;
use  Parsers.Multiline_Source.XPM;

with Parsers.Multiline_Source.Standard_Input;
use  Parsers.Multiline_Source.Standard_Input;

procedure XPM2GtkAda is

   procedure Create_Pixbuf
             (  Header  : Descriptor;
                Picture : Pixel_Buffer
             )  is
      Output  : File_Type;
      Bytes   : Integer := 3;
      Pointer : Integer := 1;
      Pixel   : Natural := 0;
      RGB     : Integer;
      Line    : String (1..72);
   begin
      Create (Output, Out_File, Header.Name & ".ads");
      for Row in Picture'Range (1) loop
         for Column in Picture'Range (2) loop
            if Picture (Row, Column) = Transparent then
               Bytes := 4;
               exit;
            end if;
         end loop;
      end loop;
      Put_Line (Output, "with Gdk.Pixbuf;    use Gdk.Pixbuf;");
      Put_Line (Output, "with GLib;          use GLib;");
      Put_Line (Output, "with Interfaces.C;  use Interfaces.C;");
      Put_Line (Output, "with System;        use System;");
      New_Line (Output);
      Put_Line (Output, "package " & Header.Name & " is");
      Put_Line
      (  Output,
         "   X_Size : constant GInt := "
      &  Image (Header.Width)
      &  ";"
      );
      Put_Line
      (  Output,
         "   Y_Size : constant GInt := "
      &  Image (Header.Height)
      &  ";"
      );
      Put_Line
      (  Output,
         (  "   type Pixbuf_Image is array (Natural range 0.."
         &  Image (Header.Height * Header.Width * Bytes - 1)
         &  ") of GUChar;"
      )  );
      Put_Line (Output, "   pragma Convention (C, Pixbuf_Image);");
      Put_Line (Output, "   Pixels : constant Pixbuf_Image :=");
      Put (Line, Pointer, "   (  ");
      if Bytes = 4 then
         for Row in Picture'Range (1) loop
            for Column in Picture'Range (2) loop
               Pixel := Pixel + 1;
               RGB   := Integer (Picture (Row, Column));
               if RGB > 16#FFFFFF# then
                  Put (Line, Pointer, "255,255,255,  0");
               else
                  Put
                  (  Line,
                     Pointer,
                     RGB / 16#10000#,
                     Field   => 3,
                     Justify => Right
                  );
                  Put (Line, Pointer, ",");
                  Put
                  (  Line,
                     Pointer,
                     (RGB mod 16#10000#) / 16#100#,
                     Field   => 3,
                     Justify => Right
                  );
                  Put (Line, Pointer, ",");
                  Put
                  (  Line,
                     Pointer,
                     RGB mod 16#100#,
                     Field   => 3,
                     Justify => Right
                  );
                  Put (Line, Pointer, ",255");
               end if;
               if (  Row /= Picture'Last (1)
                  or else
                     Column /= Picture'Last (2)
                  )  then
                  Put (Line, Pointer, ",");
               end if;
               if Pixel = 4 then
                  Put_Line (Output, Line (1..Pointer - 1));
                  Pixel := 0;
                  Pointer := 1;
                  Put (Line, Pointer, "      ");
               end if;
            end loop;
         end loop;
      else
         for Row in Picture'Range (1) loop
            for Column in Picture'Range (2) loop
               Pixel := Pixel + 1;
               RGB   := Integer (Picture (Row, Column));
               Put
               (  Line,
                  Pointer,
                  RGB / 16#10000#,
                  Field   => 3,
                  Justify => Right
               );
               Put (Line, Pointer, ",");
               Put
               (  Line,
                  Pointer,
                  (RGB mod 16#10000#) / 16#100#,
                  Field   => 3,
                  Justify => Right
               );
               Put (Line, Pointer, ",");
               Put
               (  Line,
                  Pointer,
                  RGB mod 16#100#,
                  Field   => 3,
                  Justify => Right
               );
               if (  Row /= Picture'Last (1)
                  or else
                     Column /= Picture'Last (2)
                  )  then
                  Put (Line, Pointer, ",");
               end if;
               if Pixel = 5 then
                  Put_Line (Output, Line (1..Pointer - 1));
                  Pixel := 0;
                  Pointer := 1;
                  Put (Line, Pointer, "      ");
               end if;
            end loop;
         end loop;
      end if;
      if Pixel > 0 then
         Put_Line (Output, Line (1..Pointer - 1));
      end if;
      Put_Line (Output, "   );");
      Put_Line (Output, "   function Get_Pixbuf return Gdk_Pixbuf;");
      Put_Line (Output, "end " & Header.Name & ";");
      Close (Output);

      Create (Output, Out_File, Header.Name & ".adb");
      Put_Line (Output, "with Gdk.Pixbuf.Conversions;");
      Put_Line (Output, "package body " & Header.Name & " is");
      Put_Line (Output, "   function Get_Pixbuf return Gdk_Pixbuf is");
      Put_Line (Output, "      function Internal");
      Put_Line (Output, "               (  Data       : Pixbuf_Image;");
      Put_Line
      (  Output,
         "                  Colorspace : Gdk_Colorspace;"
      );
      Put_Line (Output, "                  Has_Alpha  : GBoolean;");
      Put_Line (Output, "                  Bits       : Int;");
      Put_Line (Output, "                  Width      : Int;");
      Put_Line (Output, "                  Height     : Int;");
      Put_Line (Output, "                  Rowstride  : Int;");
      Put_Line (Output, "                  Fn         : Address;");
      Put_Line (Output, "                  Fn_Data    : Address");
      Put_Line (Output, "               )  return Address;");
      Put_Line
      (  Output,
         (  "      pragma Import (C, Internal, "
         &  """gdk_pixbuf_new_from_data"");"
      )  );
      Put_Line (Output, "   begin");
      Put_Line (Output, "      return Gdk.Pixbuf.Conversions."
                                  &  "From_Address");
      Put_Line (Output, "             (  Internal");
      Put_Line (Output, "                (  Pixels,");
      Put_Line (Output, "                   Colorspace_RGB,");
      if Bytes = 4 then
         Put_Line (Output, "                   1,");
      else
         Put_Line (Output, "                   0,");
      end if;
      Put_Line (Output, "                   8,");
      Put_Line (Output, "                   Int (X_Size),");
      Put_Line (Output, "                   Int (Y_Size),");
      Put_Line (Output, "                   "
                                  & Image (Header.Width * Bytes) & ",");
      Put_Line (Output, "                   Null_Address,");
      Put_Line (Output, "                   Null_Address");
      Put_Line (Output, "             )  );");
      Put_Line (Output, "   end Get_Pixbuf;");
      Put_Line (Output, "end " & Header.Name & ";");
      Close (Output);

      Create (Output, Out_File, Header.Name & "-image.ads");
      Put_Line (Output, "with Gtk.Image;  use Gtk.Image;");
      New_Line (Output);
      Put_Line (Output, "function " & Header.Name & ".Image");
      Put_Line (Output, "   return Gtk_Image;");
      Close (Output);

      Create (Output, Out_File, Header.Name & "-image.adb");
      Put_Line (Output, "function " & Header.Name & ".Image");
      Put_Line (Output, "   return Gtk_Image is");
      Put_Line (Output, "   Pic : constant Gdk_Pixbuf := Get_Pixbuf;");
      Put_Line (Output, "   Result : Gtk_Image;");
      Put_Line (Output, "begin");
      Put_Line (Output, "   Gtk_New (Result, Pic);");
      Put_Line (Output, "   Unref (Pic);");
      Put_Line (Output, "   return Result;");
      Put_Line (Output, "end " & Header.Name & ".Image;");
      Close (Output);

   end Create_Pixbuf;

   type Action is (Error, Pixbuf);
   To_Do : Action;
begin
   case Argument_Count is
      when 0 =>
         To_Do := Pixbuf;
      when others =>
         To_Do := Error;
   end case;
   if To_Do = Error then
      Put_Line (Standard_Error, "Use: xpm2ada < <input>");
      Set_Exit_Status (Failure);
      return;
   end if;
   declare
      Input   : aliased Source;
      Header  : Descriptor         := Get (Input'Access);
      Map     : Color_Tables.Table := Get (Input'Access, Header);
      Picture : Pixel_Buffer       := Get (Input'Access, Header, Map);
   begin
      Create_Pixbuf (Header, Picture);
   end;
   Set_Exit_Status (Success);
exception
   when Error : others =>
      Put_Line (Standard_Error, Exception_Message (Error));
      Set_Exit_Status (Failure);
end XPM2GtkAda;
