--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.Amplifier              Luebeck            --
--  Interface                                      Spring, 2011       --
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

package Gtk.Layered.Waveform.Amplifier is
--
-- Gtk_Waveform_Amplifier_Record -- Waveform amplifier adjustment.  This
--                                  is  a specialized  adjustment object
-- used  to  scale  one or more waveform  vertically.  When in the auto-
-- scaling  mode  the amplifier  changes  itself  to make  all  waveform
-- visible.
--
   type Gtk_Waveform_Amplifier_Record is
      new Gtk_Adjustment_Record
      and Waveform_Amplifier with private;
   type Gtk_Waveform_Amplifier is
      access all Gtk_Waveform_Amplifier_Record'Class;
--
-- Get_Auto_Scaling -- Get scaling mode
--
--    Amplifier - The adjustment object
--
-- Returns :
--
--    True if amplifier automatically scales its page to fit the curve
--
   function Get_Auto_Scaling
            (  Amplifier : not null access constant
                           Gtk_Waveform_Amplifier_Record
            )  return Boolean;
--
-- Get_Raster_Scaling -- Get scaling mode
--
--    Amplifier - The adjustment object
--
-- Returns :
--
--    True if amplifier scales fitting to raster
--
   function Get_Raster_Scaling
            (  Amplifier : not null access constant
                           Gtk_Waveform_Amplifier_Record
            )  return Boolean;
--
-- Get_Scaling -- Get frozen state
--
--    Amplifier - The adjustment object
--
-- Returns :
--
--    The scaling factor
--
   function Get_Scaling
            (  Amplifier : not null access constant
                           Gtk_Waveform_Amplifier_Record
            )  return Waveform_Scaling;
--
-- Get_Tick_Length -- Get approximate length of ticks
--
--    Amplifier - The adjustment object
--
-- Returns :
--
--    The approximate length of ticks in pixels
--
   function Get_Tick_Length
            (  Amplifier : not null access constant
                           Gtk_Waveform_Amplifier_Record
            )  return Positive;
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
--    Amplifier - The newly created object
--
   procedure Gtk_New (Amplifier : out Gtk_Waveform_Amplifier);
--
-- Initialize -- Construction
--
--    Amplifier - A newly created object
--
-- This procedure  must be called from  the Initialize  procedure of any
-- derived type.
--
   procedure Initialize
             (  Amplifier : not null access
                            Gtk_Waveform_Amplifier_Record'Class
             );
--
-- Set_Auto_Scaling -- Set auto scaling mode
--
--    Amplifier - The adjustment object
--    Auto      - Scaling mode
--
-- Amplifier automatically  scales  its  page  to  fit  the  curve  when
-- Auto is set true.
--
   procedure Set_Auto_Scaling
             (  Amplifier : not null access
                            Gtk_Waveform_Amplifier_Record;
                Auto      : Boolean
             );
--
-- Set_Raster_Scaling -- Set raster scaling mode
--
--    Amplifier - The adjustment object
--    Raster    - Scaling mode
--
-- This  procedure  sets  raster fitting mode. When auto-scaling mode is
-- active (see Set_Auto_Scaling), the Amplifier adjusts  its  upper  and
-- lower  bounds  so  that they values would have "good-looking" decimal
-- representations.
--
   procedure Set_Raster_Scaling
             (  Amplifier : not null access
                            Gtk_Waveform_Amplifier_Record;
                Raster    : Boolean
             );
--
-- Set_Scaling -- Set scaling
--
--    Amplifier - The adjustment object
--    Scaling   - The scaling factor
--
-- The  scaling  factor controls the behavior of the amplifier when auto
-- scaling is  on.  The  amplifier  rescales  the  y-axis  to  have  all
-- waveforms  visible.  The  value  of  Scaling  controls  the amount of
-- scaling ahead:
--
--                      ________________Y'
--                     /
--    Y_______________/     new range
--                      ////////////////
--       old range      ////////////////
--                      ////////////////
--     _________________////////////////
--
-- When  the  waveform leaves the range, e.g. exceeds Y the new range is
-- calculated as:
--
--    new range = old range * (1 + Scaling)
--
-- Conversely when all visible waveform is in the range
--
--    Y_______________
--                    \
--       old range     \________________Y'
--     ////////////////
--     ////////////////     new range
--     ////////////////
--     ////////////////_________________
--
--    new range = old range * Scaling / (1 + Scaling)
--
   procedure Set_Scaling
             (  Amplifier : not null access
                            Gtk_Waveform_Amplifier_Record;
                Scaling   : Waveform_Scaling
             );
--
-- Set_Tick_Length -- Set approximate length of major ticks
--
--    Amplifier - The adjustment object
--    Length    - The approximate length in pixels
--
-- This  procedure  sets the approximate length of major ticks used when
-- the scaling fits at the raster. The raster is selected  so  that  the
-- waveform box would contain approximately
--
--    box height / tick length
--
-- ticks corresponding to "good-values".
--
   procedure Set_Tick_Length
             (  Amplifier : not null access
                            Gtk_Waveform_Amplifier_Record;
                Length    : Positive
             );

   overriding
      procedure Add_Range
                (  Amplifier    : not null access
                                  Gtk_Waveform_Amplifier_Record;
                   Layer        : Waveform_Layer'Class;
                   From,  To    : X_Axis;
                   Lower, Upper : Y_Axis
                );
   overriding
      function Get_Lower
               (  Amplifier : access Gtk_Waveform_Amplifier_Record
               )  return GDouble;
   overriding
      function Get_Page_Size
               (  Amplifier : access Gtk_Waveform_Amplifier_Record
               )  return GDouble;
   overriding
      function Get_Upper
               (  Amplifier : access Gtk_Waveform_Amplifier_Record
               )  return GDouble;
   overriding
      function Get_Value
               (  Amplifier : access Gtk_Waveform_Amplifier_Record
               )  return GDouble;

   Class_Name : constant String := "GtkLayeredWaveformAmplifier";
private
   type Gtk_Waveform_Amplifier_Record is
      new Gtk_Adjustment_Record
      and Waveform_Amplifier with
   record
      Scaling : Waveform_Scaling := 0.2;
      Setting : Boolean  := False;
      Auto    : Boolean  := True;
      Raster  : Boolean  := True;
      Tick    : Positive := 50;
      Y1      : Y_Axis   := 0.0;
      Y2      : Y_Axis   := 1.0;
      Width   : GDouble   := 1.0;
   end record;

   procedure Set_Range
             (  Amplifier : in out Gtk_Waveform_Amplifier_Record
             );
   pragma Inline (Set_Range);

end Gtk.Layered.Waveform.Amplifier;
