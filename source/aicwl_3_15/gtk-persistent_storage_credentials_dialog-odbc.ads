--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Persistent_Storage_                    Luebeck            --
--          Credentials_Dialog.ODBC                Winter, 2008       --
--  Interface                                                         --
--                                Last revision :  18:56 10 Feb 2008  --
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
--  This package provides a query object for ODBC persistent storages.
--
package Gtk.Persistent_Storage_Credentials_Dialog.ODBC is
   pragma Elaborate_Body
          (Gtk.Persistent_Storage_Credentials_Dialog.ODBC);
--
-- Create -- A query object
--
   function Create return Query_Handles.Handle;
--
-- ODBC_Dialog_Credentials_Query
--
   type ODBC_Dialog_Credentials_Query is
      new Dialog_Credentials_Query with private;
--
-- Connect -- Overrides Gtk.Persistent_Storage_Credentials_Dialog...
--
   function Connect
            (  Query    : ODBC_Dialog_Credentials_Query;
               Name     : UTF8_String;
               User     : UTF8_String;
               Password : UTF8_String
            )  return Storage_Handle;
private
   type ODBC_Dialog_Credentials_Query is
      new Dialog_Credentials_Query with null record;

end Gtk.Persistent_Storage_Credentials_Dialog.ODBC;
