--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Handlers.References                     Luebeck            --
--  Interface                                      Spring, 2007       --
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
--  This  package  provides a simplified way of disconnecting the signal
--  handlers when the handler callback depends on the life time  of  not
--  only  the object which emits the signal, but also some other object.
--  The parent package provides  Object_Connect  for  this,  but  it  is
--  limited  to  widget  objects. This package allows to disconnect upon
--  finalization  of  any  Ada object. The package declares a controlled
--  type  Handler_Reference.  This type can be bound to a signal handler
--  ID  as  returned  by  the  Connect  functions.  Upon finalization of
--  Handler_Reference the callback will be disconnected. Note that  that
--  differently  to   strong   reference   counting   disconnection   is
--  non-additive.  That is when several Handler_Reference objects  refer
--  to the same callback, finalization of any of  them  disconnects  the
--  callback.
--
with Ada.Finalization;

package Gtk.Handlers.References is
--
-- Handler_Reference -- Reference to a callback handler
--
   type Handler_Reference is
      new Ada.Finalization.Controlled with private;
--
-- Get -- Callback identification as given by Connect
--
--    Reference - To the callback
--
-- Returns :
--
--    The callback identification
--
   function Get (Reference : Handler_Reference) return Handler_ID;
--
-- Set -- Reference to callback
--
--    Reference - To be set
--  [ Handler ] - The callback identification
--
-- The parameter ID is typically the return value of a call to  Connect.
-- When  Reference  is  already  set to a different callback handler the
-- latter is disconnected first.  When Handler  is omitted the reference
-- is reset.
--
   procedure Set
             (  Reference : in out Handler_Reference;
                Handler   : Handler_ID
             );
   procedure Set (Reference : in out Handler_Reference);

private
   type Handler_Reference is
      new Ada.Finalization.Controlled with
   record
      ID      : Gulong := Null_Handler_ID;
      Closure : GClosure;
   end record;

   procedure Adjust (Reference : in out Handler_Reference);
   procedure Finalize (Reference : in out Handler_Reference);

end Gtk.Handlers.References;
