--                                                                    --
--  package Worker    Copyright (c) Yogeshwarsing Calleecharan, 2010  --
--  Implementation                   Dmitry A. Kazakov, 2012           --
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

with Ada.Calendar;     use Ada.Calendar;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Numerics;     use Ada.Numerics;
with GLib;             use GLib;
with Gtk.Main.Router;  use Gtk.Main.Router;

with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;

package body Worker is
--
-- Update_Request -- Progress bar update refresh
--
-- Since  GTK  is  single  tasking,  drawing may not be performed on the
-- context  of  another task. This data type is used to request the main
-- GTK task to perform update of the progress bar state.
--
   type Update_Request is record
      Progress     : Gtk_Progress_Bar;
      Step_No      : Natural;
      Step_Last    : Positive;
      Substep_No   : Natural;
      Substep_Last : Positive;
   end record;
--
-- Messages -- Update_Request marshaller
--
   package Messages is new Generic_Message (Update_Request);
--
-- Service -- Called on  the GTK context  to service  the request.  This
--            procedure updates the progress bar state.
--
   procedure Service (Data : in out Update_Request) is
   begin
      Data.Progress.Set_Fraction
      (  (  (  GDouble (Data.Substep_No)
            /  GDouble (Data.Substep_Last)
            )
         +  GDouble (Data.Step_No)
         )
      /  GDouble (Data.Step_Last)
      );
      Data.Progress.Set_Text
      (  "step"
      &  Integer'Image (Data.Step_No)
      &  ", index"
      & Integer'Image (Data.Substep_No)
      );
   end Service;
--
-- Vector algebra primitives
--
   type Vector is array (Integer range <>) of Long_Float;

   function "+" (X, Y : Vector) return Vector is
   begin
      return Z : Vector (X'Range) do
         for I in X'Range loop
            Z(I) := X(I) + Y(I);
         end loop;
      end return;
   end "+";

   function "*" (S : Long_Float; X : Vector) return Vector is
   begin
      return Z : Vector (X'Range) do
         for I in X'Range loop
            Z(I) := S * X(I);
         end loop;
      end return;
   end "*";

   function "/" (X : Vector; S : Long_Float) return Vector is
   begin
      return Z : Vector (X'Range) do
         for I in X'Range loop
            Z (I) := X (I) /S;
         end loop;
      end return;
   end "/";

   Young_Modulus   : constant := 200.0E9;
   Second_Moment   : constant := 0.0635;
   Rotor_Length    : constant := 3.6;
   Stiffness_Shaft : constant := 48.0 * Young_Modulus * Second_Moment /
                                 Rotor_Length**3;
   Stiffness_Bearings : constant := 1000.0E6;
   Stiffness_Total    : constant :=
      (  (Stiffness_Shaft * Stiffness_Bearings)
      /  (Stiffness_Shaft + Stiffness_Bearings)
      );
   Rotor_Mass         : constant := 30.0E3;
   Zeta               : constant := 0.1;
   Mean_Airgap_Length : constant := 17.0E-3;

   Two_PI : constant := 2.0 * Pi;

   subtype Actual_Vector is Vector (1..4);
--
-- F -- Calculate the state vector
--
   function F
            (  T                      : Long_Float;
               Y                      : Actual_Vector;
               Force_Z2, Force_Z4     : Long_Float;
               Norm_Driving_Frequency : Long_Float;
               Norm_Radial_Stiffness  : Long_Float;
               Zeta                   : Long_Float
            )  return Actual_Vector is
   begin
      return
      (  1 => Y (2),
         2 => (  Force_Z2 * Cos (Norm_Driving_Frequency * T - Y (3))
              -  2.0 * Zeta * Y (2)
              -  (1.0 - Y (4) * Y (4)) * Y (1)
              +  Norm_Radial_Stiffness * Y (1)
              ),
         3 => Y (4),
         4 => (  Force_Z4 * Sin (Norm_Driving_Frequency * T - Y (3))
              -  2.0 * (Y (2) / Y (1) + Zeta) * Y (4)
      )       );
   end F;
--
-- Process -- The task doing actual computations
--
   task body Process is
      Scope     : Gtk_Oscilloscope;
      Channel   : Channel_Number;
      Progress  : Gtk_Progress_Bar;
      Last_Time : Time := Clock;
      Steps     : Positive;

      -- Initial time, final time, no of steps, step size
      A : constant := 0.0;
      B : constant := 10_000.0;
      N : constant := 2_500_000;
      H : constant := (B - A) / Long_Float (N);

      procedure Show_Progress (Step : Natural; I : Natural) is
      begin
         Messages.Send (Service'Access, (Progress, Step, Steps, I, N));
      end Show_Progress;

      T              : Long_Float;
      K1, K2, K3, K4 : Actual_Vector;
      Y              : Actual_Vector;

      Radial_Ump              : constant := 148724.91491;
      Radial_Stiffness        : Long_Float;
      Stiffness_Ratio         : Long_Float;
      Actual_Stiffness        : Long_Float;
      Norm_Radial_Stiffness   : Long_Float;

      Whirl_Frequency         : Long_Float;
      Norm_Forcing_Frequency  : Long_Float;
      Natural_Frequency       : Long_Float;
      Force_Z2, Force_Z4      : Long_Float;

      -- Driving frequencies over whirling range
      Forcing_Frequency_Start : Long_Float;
      Forcing_Frequency_Limit : Long_Float;
      Forcing_Frequency_Step  : Long_Float;

      -- Minmax algorithm
      Largest      : Long_Float;
      Currentvalue : Long_Float;

      procedure Get_Rk_K is
      begin
         K1 :=
            H * F (  T,
                     Y,
                     Force_Z2,
                     Force_Z4,
                     Norm_Forcing_Frequency,
                     Norm_Radial_Stiffness,
                     Zeta
                  );
         K2 :=
            H * F (  T + H / 2.0,
                     Y + K1 / 2.0,
                     Force_Z2,
                     Force_Z4,
                     Norm_Forcing_Frequency,
                     Norm_Radial_Stiffness,
                     Zeta
                  );
         K3 :=
            H * F (  T + H / 2.0,
                     Y + K2 / 2.0,
                     Force_Z2,
                     Force_Z4,
                     Norm_Forcing_Frequency,
                     Norm_Radial_Stiffness,
                     Zeta
                  );
         K4 :=
            H * F (  T + H,
                     Y + K3,
                     Force_Z2,
                     Force_Z4,
                     Norm_Forcing_Frequency,
                     Norm_Radial_Stiffness,
                     Zeta
                  );
      end Get_Rk_K;
   begin
      select -- Waiting for parameters or exit request
         accept Start
                (  Data     : Parameters;
                   Scope    : Gtk_Oscilloscope;
                   Channel  : Channel_Number;
                   Progress : Gtk_Progress_Bar
                )
         do
           Forcing_Frequency_Start := Data.Start;
           Forcing_Frequency_Limit := Data.Stop;
           Forcing_Frequency_Step :=
              (Data.Stop - Data.Start) / Long_Float (Data.Steps);
           Stiffness_Ratio  := Data.Stiffness;
           Process.Scope    := Scope;
           Process.Channel  := Channel;
           Process.Progress := Progress;
           Steps            := Data.Steps;
         end;
      or accept Stop;
         raise Quit_Error;
      end select;
      -- Starting computations
      Actual_Stiffness := Stiffness_Ratio * Stiffness_Total;
      Natural_Frequency := Sqrt (Actual_Stiffness / Rotor_Mass);

      -- Looping the whirling frequency range
      Whirl_Frequency := Forcing_Frequency_Start;
      for Step in 0..Steps loop
         Largest := Long_Float'First;

         T := A; -- Initial time
         Y := (others => 0.01);

         -- For each whirling frequency
         for I in 1..N loop
            Radial_Stiffness := Radial_Ump / (Mean_Airgap_Length * 0.1);
            Norm_Radial_Stiffness :=
               Radial_Stiffness / Actual_Stiffness;
            Force_Z2 := 0.1; --new force
            Force_Z4 := 0.1 / Y (1); --new force
            Norm_Forcing_Frequency :=
               Whirl_Frequency / Natural_Frequency;
            Get_Rk_K;  -- get the k coefficients
            Y := Y + (K1 + 2.0 * K2 + 2.0 * K3 + K4) / 6.0;
            Y (3) := Long_Float'Remainder (Y (3), Two_Pi);
            if Y (3) < 0.0 then
               Y (3) := Y (3) + Two_PI;
            end if;

            -- Last 25000 values to calculate rmax
            if I >= 2_475_000 then
               Currentvalue := Y (1);
               Largest      := Long_Float'Max (Largest, Currentvalue);
            end if;
            T := T + H;
            --
            -- Updating the progress  bar or exiting.  We don't do it on
            -- each iteration, it would only waste resources. Instead of
            -- that we do it each 200ms, which is short enough to appear
            -- "instant" for the user.
            --
            if Clock - Last_Time > 0.2 then -- Update progress bar
               select
                  accept Stop; -- Check if existing is requested
                  raise Quit_Error;
               else
                  Show_Progress (Step, I);
                  Last_Time := Clock;
               end select;
            end if;
         end loop; -- end for each whirling frequency
         Scope.Feed
         (  Channel => Channel,
            T       => GDouble (Whirl_Frequency),
            V       => GDouble (Largest)
         );
         Whirl_Frequency := Whirl_Frequency + Forcing_Frequency_Step;
      end loop;
      Show_Progress (Steps, N);
      accept Stop;
   exception
      when Quit_Error | Busy_Error => -- Main loop quitted, we follow
         null;
      when Error : others =>
         Say (Exception_Information (Error));
   end Process;

end Worker;
