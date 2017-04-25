--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Widget.Styles.                         Luebeck            --
--         Generic_Enumeration                     Summer, 2006       --
--  Implementation                                                    --
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

package body Gtk.Widget.Styles.Generic_Enumeration is

   function Style_Get
            (  Widget        : access Gtk_Widget_Record'Class;
               Property_Name : String
            )  return Enumeration is
      --
      -- Enumeration   can   be  queried  as  a  number  because  it  is
      -- convertible to. The number is used as the position to  get  the
      -- Ada equivalent.
      --
      Position : constant GUInt := Style_Get (Widget, Property_Name);
   begin
      return Enumeration'Val (Integer (Position));
   end Style_Get;

   function Enum_Property_Parser
            (  PSpec          : Param_Spec;
               RC_String      : GString;
               Property_Value : access GValue
            )  return GBoolean is
   begin
      Set_Enum
      (  Property_Value.all,
         Enumeration'Value (Value (RC_String.Str))
      );
      return 1;
   exception
      when Constraint_Error =>
         return 0;
   end Enum_Property_Parser;

   procedure Install_Style
             (  Class     : GObject_Class;
                Enum_Spec : Param_Spec
             )  is
   begin
       Class_Install_Style_Property_Parser
       (  Class  => Class,
          PSpec  => Enum_Spec,
          Parser => Parser
       );
   end Install_Style;

end Gtk.Widget.Styles.Generic_Enumeration;
