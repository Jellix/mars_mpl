--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Recent_Manager_Keys                     Luebeck            --
--  Interface                                      Autumn, 2011       --
--                                                                    --
--                                Last revision :  10:35 22 Oct 2011  --
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

with Ada.Finalization;

with Gtk.List_Store;          use Gtk.List_Store;
with Gtk.Recent_Manager_Alt;  use Gtk.Recent_Manager_Alt;

package Gtk.Recent_Manager_Keys is

   --
   -- Restore -- A recently used value
   --
   --    Key     - The key of the value
   --    Default - The default value
   --    Manager - The recent manager to use
   --
   -- Returns :
   --
   --    The value
   --
   function Restore
     (Key     : UTF8_String;
      Default : UTF8_String;
      Manager : Gtk_Recent_Manager := Get_Default) return UTF8_String;

   --
   -- Restore -- A column of the list store
   --
   --    Key     - The key of the model columns
   --    Model   - The list store
   --    Column  - The column 0.. (of the type GType_String)
   --    Max_Row - The maximal row number to restore
   --    Manager - The recent manager to use
   --
   -- This procedure restores Column of the list store model. The values of
   -- the  keys are named as Key_n where n is the number 1..Max_Row are set
   -- into Model. Key_1 corresponds the first row. When the list  does  not
   -- contain a row, it is inserted into the model. The  procedure  can  be
   -- used to fill combo box entries contents with recently entered values.
   --
   procedure Restore
     (Key     : UTF8_String;
      Model   : Gtk_List_Store;
      Column  : Gint;
      Max_Row : Positive := 10;
      Manager : Gtk_Recent_Manager := Get_Default);

   --
   -- Store -- A recently used value
   --
   --    Key     - The key of the value
   --    Value   - The value to store
   --    Manager - The recent manager to use
   --
   procedure Store
     (Key     : UTF8_String;
      Value   : UTF8_String;
      Manager : Gtk_Recent_Manager := Get_Default);

   --
   -- Store -- A column of the list store
   --
   --    Key     - The key of the model columns
   --    Model   - The list store
   --    Column  - The column 0.. (of the type GType_String)
   --    Max_Row - The maximal row number to restore
   --    Manager - The recent manager to use
   --
   -- This procedure stores Column of the list store model. The  values  of
   -- the keys are named as Key_n where n is the row number.  Max_Row  rows
   -- are stored. Stored column can be restored using Restore.
   --
   procedure Store
     (Key     : UTF8_String;
      Model   : Gtk_List_Store;
      Column  : Gint;
      Max_Row : Positive           := 10;
      Manager : Gtk_Recent_Manager := Get_Default);

   --
   -- Key_Enumerator -- Key enumeration object
   --
   type Key_Enumerator is
     abstract new Ada.Finalization.Limited_Controlled with null record;

   --
   -- Enumerate -- Keys
   --
   --    Enumerator - The enumeration object
   --    Prefix     - The prefix of the keys for which Process is called
   --    Manager - The recent manager to use
   --
   procedure Enumerate
     (Enumerator : in out Key_Enumerator'Class;
      Prefix     : UTF8_String;
      Manager    : Gtk_Recent_Manager := Get_Default);

   --
   -- Process -- Called when a key is found
   --
   --    Enumerator - The enumeration object
   --    Key        - The key name
   --    Value      - The value of the key
   --    Info       - The info structure associated with the key
   --
   -- The implementation may raise End_Error to end enumeration.
   --
   procedure Process
     (Enumerator : in out Key_Enumerator;
      Key        : UTF8_String;
      Value      : UTF8_String;
      Info       : Gtk_Recent_Info) is abstract;

private

   type Model_Enumerator is
     new Key_Enumerator with
      record
         Model   : Gtk_List_Store;
         Length  : Natural;
         Column  : Gint;
         Max_Row : Gint;
      end record;

   overriding procedure Process
     (Enumerator : in out Model_Enumerator;
      Key        : UTF8_String;
      Value      : UTF8_String;
      Info       : Gtk_Recent_Info);

end Gtk.Recent_Manager_Keys;
