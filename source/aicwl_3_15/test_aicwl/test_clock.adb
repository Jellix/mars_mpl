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

with Ada.Calendar;  use Ada.Calendar;

package body Test_Clock is

   procedure Connect
             (  Button  : Gtk_Button;
                LED     : Gtk.Gauge.LED_Rectangular.
                          Gtk_Gauge_LED_Rectangular;
                Handler : LED_Rectangular_Handlers.Simple_Handler
             )  is
   begin
      LED_Rectangular_Handlers.Connect
      (  Button,
         "clicked",
         Handler,
         LED
      );
   end Connect;

   procedure Connect
             (  Button  : Gtk_Button;
                LED     : Gtk.Gauge.LED_Round.Gtk_Gauge_LED_Round;
                Handler : LED_Round_Handlers.Simple_Handler
             )  is
   begin
      LED_Round_Handlers.Connect
      (  Button,
         "clicked",
         Handler,
         LED
      );
   end Connect;

   procedure Gtk_New (Widget : out Real_Modern_Clock) is
   begin
      Widget := new Real_Modern_Clock_Record;
      Initialize
      (  Gtk_Wall_Clock_Modern_Record (Widget.all)'Unchecked_Access,
         null
      );
   end Gtk_New;

   procedure Refresh
             (  Widget  : not null access Real_Modern_Clock_Record;
                Context : Cairo_Context
             )  is
   begin
      Set_Value (Widget, Clock);
      Gtk_Wall_Clock_Modern_Record (Widget.all).Refresh (Context);
   end Refresh;

end Test_Clock;

