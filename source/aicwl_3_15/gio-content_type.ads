--                                                                    --
--  package GIO.Content_Type        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Summer, 2010       --
--                                                                    --
--                                Last revision :  19:56 19 Jul 2010  --
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

with GLib;         use GLib;
with GLib.Object;  use GLib.Object;

package GIO.Content_Type is
--
-- Can_Be_Executable -- A guess
--
--    Instance - A content type
--
-- This function checks if a content type can be executable.  Note  that
-- for instance things like text files can be executables (i.e.  scripts
-- and batch files).
--
-- Returns :
--
--    The if the file type corresponds to a type that can be executable
--
   function Can_Be_Executable (Instance : UTF8_String) return Boolean;
--
-- Equals -- Compares two content types for equality
--
--    Type_1 - A content type
--    Type_2 - A content type
--
-- Returns :
--
--    True if Type_1 is equal to Type_2
--
   function Equals (Type_1, Type_2 : UTF8_String) return Boolean;
--
-- From_MIME_Type -- Find a content type based on the MIME type name
--
--    MIME - A MIME type
--
-- This  function  tries  to  find a content type based on the MIME type
-- name.
--
-- Returns :
--
--    The content type or an empty string
--
   function From_MIME_Type (MIME : UTF8_String) return UTF8_String;
--
-- Get_Description -- Gets the human readable description of a type
--
--    Instance - A content type
--
-- Returns :
--
--    The description
--
   function Get_Description (Instance : UTF8_String) return UTF8_String;
--
-- Get_Icon -- Gets an icon for the content type
--
--    Instance - A content type
--
-- Returns :
--
--    The icon object (Unref is required when no more used)
--
   function Get_Icon (Instance : UTF8_String) return GObject;
--
-- Get_MIME_Type -- Gets the MIME-type for the content type
--
--    Instance - A content type
--
-- Returns :
--
--    The type if registered, empty string otherwise
--
   function Get_MIME_Type (Instance : UTF8_String) return UTF8_String;
--
-- Guess -- Content type
--
--    File_Name - The file name
--
-- Returns :
--
--    The content type
--
   function Guess (File_Name : UTF8_String) return UTF8_String;
--
-- Is_A -- Determines if type is a subset of supertype.
--
--    Instance  - A content type
--    Supertype - A content type
--
-- Returns :
--
--    True if Instance is a kind of Supertype
--
   function Is_A (Instance, Supertype : UTF8_String) return Boolean;
--
-- Is_Unknown -- Checks if the content type is the generic "unknown"
--
--    Instance - A content type
--
-- This  function  checks  if  the content type is the generic "unknown"
-- type. On Unix this is the "application/octet-stream" mimetype,  while
-- on win32 it is "*".
--
-- Returns :
--
--    True if Instance is unknown type
--
   function Is_Unknown (Instance : UTF8_String) return Boolean;

end GIO.Content_Type;
