--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.Sweeper                Luebeck            --
--  Interface                                      Spring, 2011       --
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
--____________________________________________________________________--

package Gtk.Layered.Waveform.Sweeper is
--
-- Gtk_Waveform_Sweeper_Record -- Waveform sweeper adjustment. This is a
--                                specialized  adjustment object used to
-- sweep one or more waveforms. When  not  frozen  the  sweeper  changes
-- itself  before  the  waveforms are drawn, so that the right margin of
-- all  boxes  of  the waveforms would correspond to the time of drawing
-- minus a fixed offset.  The offset can be changed. When the sweeper is
-- frozen  the right margin does change but the offset is incremented to
-- compensate time.
--
   type Gtk_Waveform_Sweeper_Record is
      new Gtk_Adjustment_Record
      and Waveform_Sweeper with private;
   type Gtk_Waveform_Sweeper is
      access all Gtk_Waveform_Sweeper_Record'Class;
--
-- Get_From -- Get the first time time of the sweeper
--
--    Sweeper - The adjustment object
--
-- This function returns the time corresponding to the  lower  value  of
-- the sweeper.
--
-- Returns :
--
--    The first time
--
   function Get_From
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Time;
   function Get_From
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Ada.Calendar.Time;
--
-- Get_Frozen -- Get frozen state
--
--    Sweeper - The adjustment object
--
-- Returns :
--
--    True if the sweeper is frozen
--
   function Get_Frozen
            (  Sweeper : not null access constant
                         Gtk_Waveform_Sweeper_Record
            )  return Boolean;
--
-- Get_Offset -- The offset between the right margin and upper value
--
--    Sweeper - The adjustment object
--
-- Returns :
--
--    The offset
--
   function Get_Offset
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Duration;
--
-- Get_Page_Span -- Get span of the sweeper page
--
--    Sweeper - The adjustment object
--
-- This  function  is  equivalent  to  Get_Page_Size,  which  result  is
-- converted to Duration.
--
-- Returns :
--
--    The duration of the page
--
   function Get_Page_Span
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Duration;
--
-- Get_Time -- Get current time of the sweeper
--
--    Sweeper - The adjustment object
--
-- Returns :
--
--    The time corresponding to the end (right margin) of the page
--
   function Get_Time
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Time;
   function Get_Time
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Ada.Calendar.Time;
--
-- Get_To -- Get the last time time of the sweeper
--
--    Sweeper - The adjustment object
--
-- This function returns the time corresponding to the  upper  value  of
-- the sweeper.
--
-- Returns :
--
--    The last time
--
   function Get_To
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Time;
   function Get_To
            (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
            )  return Ada.Calendar.Time;
--
-- Get_Type -- The type of the widget
--
-- Returns :
--
--    The GTK type of the widget
--
   function Get_Type return GType;
--
-- Gtk_New -- Factory
--
--    Sweeper - The newly created object
--
   procedure Gtk_New (Sweeper : out Gtk_Waveform_Sweeper);
--
-- Initialize -- Construction
--
--    Sweeper - A newly created object
--
-- This procedure must be called from Initialize of any derived type.
--
   procedure Initialize
             (  Sweeper : not null access
                          Gtk_Waveform_Sweeper_Record'Class
             );
--
-- Set -- Configure sweeper
--
--    Sweeper        - The adjustment object
--    Date           - The time at the end (right margin) of the page
--    From           - The beginning of the time interval
--    To             - The end of the time interval
--    Step_Increment - Small increment
--    Page_Increment - Larget increment
--    Page_Span      - The page span
--
-- This procedure changes all settings of the sweeper.
--
   procedure Set
             (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
                Date           : Time;
                From           : Time;
                To             : Time;
                Step_Increment : Duration;
                Page_Increment : Duration;
                Page_Span      : Duration
             );
   procedure Set
             (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
                Date           : Ada.Calendar.Time;
                From           : Ada.Calendar.Time;
                To             : Ada.Calendar.Time;
                Step_Increment : Duration;
                Page_Increment : Duration;
                Page_Span      : Duration
             );
--
-- Set_Frozen -- Set frozen state
--
--    Sweeper - The adjustment object
--    Frozen  - Desired state
--
-- Frozen sweeper retains its position on the time axis
--
   procedure Set_Frozen
             (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
                Frozen  : Boolean
             );
--
-- Set_Page_Span -- Configure sweeper
--
--    Sweeper   - The adjustment object
--    Page_Span - The page span
--
-- This procedure changes page span of the sweeper.  It is equivalent to
-- Set_Page_Size called with the value numerically equal to Page_Span.
--
   procedure Set_Page_Span
             (  Sweeper   : not null access Gtk_Waveform_Sweeper_Record;
                Page_Span : Duration
              );
--
-- Set_Time -- Set right margin of the sweeper
--
--    Sweeper - The adjustment object
--    Stamp   - The time to set
--
   procedure Set_Time
             (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
                Stamp   : Time
             );
   procedure Set_Time
             (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
                Stamp   : Ada.Calendar.Time
             );

   overriding
      procedure Set_Current_Time
                (  Sweeper : not null access
                             Gtk_Waveform_Sweeper_Record;
                   Stamp   : Time;
                   Active  : Boolean := False
                );
   overriding
      function Is_Active
               (  Sweeper : not null access Gtk_Waveform_Sweeper_Record
               )  return Boolean;

   Class_Name : constant String := "GtkLayeredWaveformSweeper";
private
   pragma Inline (Set_Time);

   type Gtk_Waveform_Sweeper_Record is
      new Gtk_Adjustment_Record
      and Waveform_Sweeper with
   record
      Frozen : Boolean := False;
      Active : Integer := 0;
   end record;

   procedure Set_Time
             (  Sweeper : not null access Gtk_Waveform_Sweeper_Record;
                Stamp   : GDouble
             );
end Gtk.Layered.Waveform.Sweeper;
