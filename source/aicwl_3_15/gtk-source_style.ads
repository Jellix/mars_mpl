--                                                                    --
--  package Gtk.Source_Style        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2009       --
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

package Gtk.Source_Style is
--
-- Gtk_Source_Style_Record -- Describtion of text attributes  which  are
--                            set when given style is used
--
   type Gtk_Source_Style_Record is new GObject_Record with private;
   type Gtk_Source_Style is access all Gtk_Source_Style_Record'Class;
--
-- Style_Copy -- Copy a style object
--
--    Style - To copy
--
-- Returns :
--
--    A copy object (shall be unref'ed when no more used)
--
   function Style_Copy (Style : not null access Gtk_Source_Style_Record)
      return Gtk_Source_Style;

private
   type Gtk_Source_Style_Record is new GObject_Record with null record;

end Gtk.Source_Style;
