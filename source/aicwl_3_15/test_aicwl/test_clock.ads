--                                                                    --
--  package Test_Clock              Copyright (c)  Dmitry A. Kazakov  --
--  Test real time clock                           Luebeck            --
--                                                 Spring, 2011       --
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

with Cairo;                  use Cairo;
with Gtk.Button;             use Gtk.Button;
with Gdk.Rectangle;          use Gdk.Rectangle;
with Gtk.Wall_Clock.Modern;  use Gtk.Wall_Clock.Modern;

with Gtk.Gauge.LED_Rectangular;
with Gtk.Gauge.LED_Round;
with Gtk.Handlers;

package Test_Clock is
   type Real_Modern_Clock_Record is
      new Gtk_Wall_Clock_Modern_Record with null record;
   type Real_Modern_Clock is
      access all Real_Modern_Clock_Record'Class;
   procedure Gtk_New (Widget : out Real_Modern_Clock);
   overriding
      procedure Refresh
                (  Widget  : not null access Real_Modern_Clock_Record;
                   Context : Cairo_Context
                );

   package LED_Round_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Button_Record,
             Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round
          );
   procedure Connect
             (  Button  : Gtk_Button;
                LED     : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
                Handler : LED_Round_Handlers.Simple_Handler
             );

   package LED_Rectangular_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Button_Record,
             Gtk.Gauge.LED_Rectangular.Gtk_Gauge_LED_Rectangular
          );
   procedure Connect
             (  Button  : Gtk_Button;
                LED     : Gtk.Gauge.LED_Rectangular.
                          Gtk_Gauge_LED_Rectangular;
                Handler : LED_Rectangular_Handlers.Simple_Handler
             );

end Test_Clock;

