--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Handlers.Generic_Callback               Luebeck            --
--  Interface                                      Autumn, 2011       --
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
--  This  package  provides signal handlers which process the parameters
--  in  an  untyped way, as GValues. Differently to the packages defined
--  in Gtk.Handlers, it also  handles  the  signal  return  value  as  a
--  GValue.
--
with GLib.Values;  use GLib.Values;

generic
   type Object_Type is new GObject_Record with private;
   type User_Type is private;
package Gtk.Handlers.Generic_Callback is
--
-- Handler -- The signal handler
--
--    Object    - The object for which the signal was emitted
--    Arguments - The signal parameters
--    Result    - The signal result value to return
--    Data      - User data
--
-- The  result  value  is initated with the type required by the signal.
-- When the signal has no return value it is GType_None.
--
   type Handler is access procedure
        (  Object    : not null access Object_Type'Class;
           Arguments : GValue_Array;
           Result    : in out GValue;
           Data      : User_Type
        );
--
-- Connect -- To the signal
--
--    Object   - The object
--    Name     - The signal name
--    Callback - The signal handler
--    Data     - The user data to pass to the handler
--    After    - If the handler to be connected after default handlers
--
-- Returns :
--
--    The handler ID
--
-- Exceptions :
--
--    Constraint_Error - Name is not a defined signal of Object
--
   procedure Connect
             (  Object   : not null access Object_Type'Class;
                Name     : GLib.Signal_Name;
                Callback : Handler;
                Data     : User_Type;
                After    : Boolean := False
             );
   function Connect
            (  Object   : not null access Object_Type'Class;
               Name     : GLib.Signal_Name;
               Callback : Handler;
               Data     : User_Type;
               After    : Boolean := False
            )  return Handler_ID;

end Gtk.Handlers.Generic_Callback;
