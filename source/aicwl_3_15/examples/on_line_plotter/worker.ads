--                                                                    --
--  package Worker   Copyright (c)  Yogeshwarsing Calleecharan, 2010  --
--  Interface                                Dmitry A. Kazakov, 2012  --
--                                                                    --
--                                Last revision :  15:58 22 Jan 2012  --
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
--  The package provides a task that  solves  differential  equation  of
--  describing rotor frequency of a hydropower generator as described in
--  "On  the dynamics of an hydropower generator subjected to unbalanced
--  magnetic  pull:  characterisation  and analysis, Luleå University of
--  Technology, Luleå, ISBN13: 978-91-7439-161-9, ISBN10: 9174391615" by
--  Yogeshwarsing Calleecharan (2010).
--
--  The  code  is  based  on  the  contribution  made  by  Yogeshwarsing
--  Calleecharan.
--
with Gtk.Oscilloscope;  use Gtk.Oscilloscope;
with Gtk.Progress_Bar;  use Gtk.Progress_Bar;

package Worker is
--
-- Parameters -- Of a calculation session
--
   type Parameters is record
      Start     : Long_Float;
      Stop      : Long_Float;
      Stiffness : Long_Float;
      Steps     : Positive;
   end record;
--
-- Process -- Calculation process task
--
   task type Process is
   --
   -- Start -- Computations with the parameters specified
   --
   --    Parameters - To use in the computations
   --    Scope      - The oscilloscope
   --    Channel    - The number of the channel to feed
   --    Progress   - The progress bar to update during computations
   --
      entry Start
            (  Data     : Parameters;
               Scope    : Gtk_Oscilloscope;
               Channel  : Channel_Number;
               Progress : Gtk_Progress_Bar
            );
   --
   -- Stop -- Terminate the task prematurely
   --
      entry Stop;
   end Process;
end Worker;
