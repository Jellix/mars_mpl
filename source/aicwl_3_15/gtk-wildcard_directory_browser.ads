--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Wildcard_Directory_Browser              Luebeck            --
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
--
--  This  package  provides  a  spezialized version of directory browser
--  with the files filter built upon a pattern. The pattern is a list of
--  alternative. Each alternative is an UTF8-encoded string. The  string
--  can contain any number of wildcards  *.  The  wildcard  matches  any
--  chain  of  UTF-8  characters. The chain matched can be empty. A file
--  name  matches  if  at  least  one  alternative  does.  When the list
--  contains no alternatives it matches everything.
--
with Gtk.Abstract_Browser;    use Gtk.Abstract_Browser;
with Gtk.Directory_Browser;   use Gtk.Directory_Browser;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Enums.String_Lists;  use Gtk.Enums.String_Lists;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Widget;              use Gtk.Widget;

with Gtk.Enums.String_Lists.Wildcards;
use  Gtk.Enums.String_Lists.Wildcards;

package Gtk.Wildcard_Directory_Browser is
--
-- Gtk_Wildcard_Directory_Browser_Record -- A directory  browser  widget
--                                          with  wildcard files filter.
-- The filter is a string pattern that can contain the * wildcard symbol
-- to match file names which may appear in the files view pane. Wildcard
-- matches  any  sequence  of UTF-8 encoded characters. An empty pattern
-- string patches anything and is equivalent to "*".
--
   type Gtk_Wildcard_Directory_Browser_Record is
      new Gtk_Directory_Browser_Record with private;
   type Gtk_Wildcard_Directory_Browser is
      access all Gtk_Wildcard_Directory_Browser_Record'Class;
--
-- Filter -- Overrides Gtk.Directory_Browser...
--
   overriding
   function Filter
            (  Widget    : not null access
                           Gtk_Wildcard_Directory_Browser_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean;

--
-- Finalize -- To be called by any derived type when overridden
--
--    Widget - The widget
--
   procedure Finalize
             (  Widget : not null access
                         Gtk_Wildcard_Directory_Browser_Record
             );
--
-- Get_Pattern -- Get currently used pattern
--
--    Widget - The widget
--
-- Returns :
--
--    The files pattern
--
   function Get_Pattern
            (  Widget : not null access
                        Gtk_Wildcard_Directory_Browser_Record
            )  return String_List.GList;
--
-- Gtk_New -- Factory
--
--    Widget    - The result
--    Pattern   - Files filtering pattern
--    File      - A file in the file system to browse
--    Columns   - The number of columns to use in the files list
--    Vertical  - The vertically vs. horizontally panned
--    Tree_Size - The maximal size of the tree pane upon start
--    List_Size - The maximal size of the list pane upon start
--    Store     - The cache to use
--    Tracing   - Desired tracing
--
-- When the parameter Store is null a new  Gtk_Directory_Browser  object
-- is created. Otherwise, the specified one is used.
--
   procedure Gtk_New
             (  Widget    : out Gtk_Wildcard_Directory_Browser;
                Pattern   : String_List.GList := Any;
                File      : UTF8_String := Get_Current_Dir;
                Columns   : Positive    := 4;
                Vertical  : Boolean     := False;
                Tree_Size : Gtk_Requisition :=
                               (Width => 180, Height => 500);
                List_Size : Gtk_Requisition :=
                               (Width => 600, Height => 500);
                Store     : Gtk_Directory  := null;
                Tracing   : Traced_Actions := Trace_Nothing
             );
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
             );
--
-- Initialize -- To be called by any derived type
--
--    Widget   - The result
--    Pattern  - Files filtering pattern
--    File     - A file in the file system to browse
--    Columns  - The number of columns to use in the files list
--    Vertical - The vertically vs. horizontally panned
--    Store    - The cache to use
--    Tracing  - Desired tracing
--
   procedure Initialize
             (  Widget    : not null access
                            Gtk_Wildcard_Directory_Browser_Record'Class;
                Pattern   : String_List.GList;
                File      : UTF8_String;
                Columns   : Positive;
                Vertical  : Boolean;
                Tree_Size : Gtk_Requisition;
                List_Size : Gtk_Requisition;
                Store     : Gtk_Directory;
                Tracing   : Traced_Actions
             );
--
-- Set_Pattern -- Set pattern
--
--    Widget  - The widget
--    Pattern - Files filtering pattern
--
-- These procedures changes the filtering by setting a new pattern.  The
-- pattern  can  be  specified  as a list of alternatives or as a single
-- alternative.
--
   procedure Set_Pattern
             (  Widget  : not null access
                          Gtk_Wildcard_Directory_Browser_Record;
                Pattern : String_List.GList := Any
             );
   procedure Set_Pattern
             (  Widget  : not null access
                          Gtk_Wildcard_Directory_Browser_Record;
                Pattern : UTF8_String
             );
   procedure Set_Pattern
             (  Widget  : not null access
                          Gtk_Wildcard_Directory_Browser_Record;
                Pattern : Controlled_String_List
             );
private
   use String_List;

   type Gtk_Wildcard_Directory_Browser_Record is
      new Gtk_Directory_Browser_Record with
   record
      Filter : GList := Null_List;
   end record;
end Gtk.Wildcard_Directory_Browser;
