--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Wildcard_Directory_Browser              Luebeck            --
--  Implementation                                 Winter, 2007       --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Glib.Messages;   use Glib.Messages;

with GLib.Object.Checked_Destroy;

package body Gtk.Wildcard_Directory_Browser is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Wildcard_Directory_Browser." & Name;
   end Where;

   procedure Finalize
             (  Widget : not null access
                         Gtk_Wildcard_Directory_Browser_Record
             )  is
   begin
      Free_String_List (Widget.Filter);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   function Filter
            (  Widget    : not null access
                           Gtk_Wildcard_Directory_Browser_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean is
      type Outcome is (Success, Mismatch, Failure);
   begin
      return Match (UTF8_String (Name), Widget.Filter);
   end Filter;

   function Get_Pattern
            (  Widget : not null access
                        Gtk_Wildcard_Directory_Browser_Record
            )  return GList is
   begin
      return Widget.Filter;
   end Get_Pattern;

   procedure Gtk_New
             (  Widget    : out Gtk_Wildcard_Directory_Browser;
                Pattern   : GList       := Any;
                File      : UTF8_String := Get_Current_Dir;
                Columns   : Positive    := 4;
                Vertical  : Boolean     := False;
                Tree_Size : Gtk_Requisition :=
                               (Width => 180, Height => 500);
                List_Size : Gtk_Requisition :=
                               (Width => 600, Height => 500);
                Store     : Gtk_Directory  := null;
                Tracing   : Traced_Actions := Trace_Nothing
             )  is
   begin
      Widget := new Gtk_Wildcard_Directory_Browser_Record;
      begin
         Initialize
         (  Widget    => Widget,
            Pattern   => Pattern,
            File      => File,
            Columns   => Columns,
            Vertical  => Vertical,
            Tree_Size => Tree_Size,
            List_Size => List_Size,
            Store     => Store,
            Tracing   => Tracing
         );
      exception
         when Error : others =>
            Log
            (  GtkAda_Contributions_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget    : out Gtk_Wildcard_Directory_Browser;
                Pattern   : Controlled_String_List;
                File      : UTF8_String := Get_Current_Dir;
                Columns   : Positive    := 4;
                Vertical  : Boolean     := False;
                Tree_Size : Gtk_Requisition :=
                               (Width => 180, Height => 500);
                List_Size : Gtk_Requisition :=
                               (Width => 600, Height => 500);
                Store     : Gtk_Directory  := null;
                Tracing   : Traced_Actions := Trace_Nothing
             )  is
   begin
      Gtk_New
      (  Widget    => Widget,
         Pattern   => +Pattern,
         File      => File,
         Columns   => Columns,
         Vertical  => Vertical,
         Tree_Size => Tree_Size,
         List_Size => List_Size,
         Store     => Store,
         Tracing   => Tracing
      );
   end Gtk_New;

   procedure Initialize
             (  Widget    : not null access
                            Gtk_Wildcard_Directory_Browser_Record'Class;
                Pattern   : GList;
                File      : UTF8_String;
                Columns   : Positive;
                Vertical  : Boolean;
                Tree_Size : Gtk_Requisition;
                List_Size : Gtk_Requisition;
                Store     : Gtk_Directory;
                Tracing   : Traced_Actions
             )  is
      Alternative : GList := Pattern;
   begin
      for Index in 1..Length (Pattern) loop
         Prepend (Widget.Filter, Get_Data (Alternative));
         Alternative := Next (Alternative);
      end loop;
      Initialize
      (  Widget    => Widget,
         File      => File,
         Columns   => Columns,
         Vertical  => Vertical,
         Tree_Size => Tree_Size,
         List_Size => List_Size,
         Store     => Store,
         Tracing   => Tracing
      );
   end Initialize;

   procedure Set_Pattern
             (  Widget  : not null access
                          Gtk_Wildcard_Directory_Browser_Record;
                Pattern : String_List.GList := Any
             )  is
      Alternative : GList := Pattern;
   begin
      Free_String_List (Widget.Filter);
      for Index in 1..Length (Pattern) loop
         Prepend (Widget.Filter, Get_Data (Alternative));
         Alternative := Next (Alternative);
      end loop;
      Refilter (Get_Files_View (Widget));
   end Set_Pattern;

   procedure Set_Pattern
             (  Widget  : not null access
                          Gtk_Wildcard_Directory_Browser_Record;
                Pattern : UTF8_String
             )  is
   begin
      Free_String_List (Widget.Filter);
      if Pattern'Length >0 then
         Prepend (Widget.Filter, Pattern);
      end if;
      Refilter (Get_Files_View (Widget));
   end Set_Pattern;

   procedure Set_Pattern
             (  Widget  : not null access
                          Gtk_Wildcard_Directory_Browser_Record;
                Pattern : Controlled_String_List
             )  is
   begin
      Set_Pattern (Widget, +Pattern);
   end Set_Pattern;

end Gtk.Wildcard_Directory_Browser;
