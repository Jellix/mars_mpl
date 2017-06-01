--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Refresh_Engine                  Luebeck            --
--  Interface                                      Winter, 2011       --
--                                                                    --
--                                Last revision :  16:49 28 Feb 2016  --
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
--
--  There  are  two ways to implement a user interface application. One,
--  used in GTK is event-driven. That is when the interface  immediately
--  responds to user action and other input. This  approach  may  become
--  very  resource consuming  when the user interface must render highly
--  dynamic  data.  The  input  events  at millisecond rate would likely
--  block the interface. An alternative approach is time-driven when the
--  user  interface  is  update  at  fixed  rate, usually at about 50Hz,
--  independently on the input events. Both approaches can be mixed.
--     This package provides a refresh engine object, which can be  used
--  in order to periodically refresh the state of layered widgets.
--
with Glib.Object.Weak_References;

package Gtk.Layered.Refresh_Engine is

   pragma Warnings (Off, "declaration hides ""Timer""");
   pragma Warnings (Off, "declaration hides ""Widget""");

   --
   -- Layered_Refresh_Engine -- Refresh engine object
   --
   type Layered_Refresh_Engine is tagged limited private;

   --
   -- Add -- A widget to the engine
   --
   --    Engine - The engine
   --    Widget - The widget to refresh
   --
   -- Nothing happens when the widget is already in the Engine's list.
   --
   -- Exceptions :
   --
   --    Use_Error - Called from a widget's Refresh
   --
   procedure Add
     (Engine : in out Layered_Refresh_Engine;
      Widget : not null access Gtk_Layered_Record'Class);

   --
   -- Delete -- A widget from the engine
   --
   --    Engine - The engine
   --    Widget - The widget to exclude form refresh
   --
   -- Nothing happens when the widget is not in the Engine's list.
   --
   -- Exceptions :
   --
   --    Use_Error - Called from a widget's Refresh
   --
   procedure Delete
     (Engine : in out Layered_Refresh_Engine;
      Widget : not null access Gtk_Layered_Record'Class);

   --
   -- Get_Period -- Get the refresh period
   --
   --    Widget - The widget
   --
   -- Returns :
   --
   --    The period
   --
   function Get_Period (Engine : Layered_Refresh_Engine)
                        return Duration;

   --
   -- Set_Period -- Set the refresh period
   --
   --    Widget - The widget
   --    Period - The period to set
   --
   -- Exceptions :
   --
   --    Constraint_Error - The period is out of range
   --
   procedure Set_Period
     (Engine : in out Layered_Refresh_Engine;
      Period : Duration);

private

   package Layered_Reference is
     new GLib.Object.Weak_References (Gtk_Layered_Record);

   type List_Element is limited
      record
         Widget     : Layered_Reference.Weak_Reference;
         Prev, Next : not null access List_Element :=
                        List_Element'Unchecked_Access;
      end record;
   type List_Element_Ptr is access all List_Element;

   type Layered_Refresh_Engine is
     new Ada.Finalization.Limited_Controlled with
      record
         Timer  : Guint    := 0;
         Period : Duration := Duration'Last;
         Active : Boolean  := False;
         List   : access List_Element;
      end record;

   overriding procedure Finalize (Engine : in out Layered_Refresh_Engine);

   type Layered_Refresh_Engine_Ptr is not null access all
     Layered_Refresh_Engine'Class;

   function Timer (Data : System.Address) return Gboolean;
   pragma Convention (C, Timer);

   type Timer_Ptr is access
     function (Engine : System.Address) return Gboolean;
   pragma Convention (C, Timer_Ptr);

   function Timeout_Add
     (Interval : Guint;
      Timer    : Timer_Ptr;
      Data     : System.Address) return Guint;
   function Remove (Timer : Guint) return Gboolean;

   pragma Import (C, Timeout_Add, "g_timeout_add");
   pragma Import (C, Remove, "g_source_remove");

   pragma Warnings (On, "declaration hides ""Timer""");
   pragma Warnings (On, "declaration hides ""Widget""");

end Gtk.Layered.Refresh_Engine;
