--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Layered.Waveform.                       Luebeck            --
--        Sample_Lines                             Winter, 2011       --
--  Separate body                                                     --
--                                Last revision :  22:46 07 Apr 2016  --
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

separate (Gtk.Layered.Waveform)
   procedure Sample_Lines
             (  Layer  : in out Waveform_Layer;
                Data   : in out Line_Method_Data;
                X1, X2 : Horizontal_Offset
             )  is
   function To_X (T : X_Axis) return Horizontal_Offset is
      pragma Inline (To_X);
   begin
      return Horizontal_Offset ((T - Layer.T1) / Layer.dT) +  X1;
   end To_X;

   procedure Set_Interval_Bounds
             (  T     : X_Axis;
                X     : out Horizontal_Offset;
                Right : out X_Axis
             )  is
      pragma Inline (Set_Interval_Bounds);
      Offset : constant X_Axis :=
                        X_Axis'Floor ((T - Layer.T1) / Layer.dT);
   begin
      Right := (Offset + 1.0) * Layer.dT + Layer.T1;
      X := Horizontal_Offset (Offset) + X1;
   end Set_Interval_Bounds;

   procedure Add_Point (X : Horizontal_Offset; Y : Y_Axis) is
      pragma Inline (Add_Point);
      Index : constant Natural :=
                 (Data.First + Data.Count) mod Data.Points'Length;
   begin
      Data.Points (Index) := (X, Y);
      if Data.Count + 1 < Data.Points'Length then
         Data.Count := Data.Count + 1;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Dump (Layer, Data.Points.all, Index);
         end if; -------------------------------------------------------
      else
         Data.First := Data.First + 1;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace_Line
            (  Layer'Address,
               (  " Buffer overflow"
               &  Integer'Image (Data.Points'Length)
               &  ", width"
               &  Horizontal_Offset'Image (X2 - X1 + 1)
               &  ", @"
               &  Horizontal_Offset'Image (X)
               &  ", count"
               &  Integer'Image (Data.Count)
            )  );
            Dump (Layer, Data.Points.all, Index);
         end if; -------------------------------------------------------
      end if;
   end Add_Point;

   procedure Add
             (  X           : Horizontal_Offset;
                Left, Right : Y_Axis;
                Min,  Max   : Y_Axis
             )  is
      pragma Inline (Add);
   begin
      if Min < Y_Axis'Min (Left, Right) then
         if Max > Y_Axis'Max (Left, Right) then
            Add_Point (X, Left);
            Add_Point (X, Min);
            Add_Point (X, Max);
            Add_Point (X, Right);
         else
            Add_Point (X, Left);
            Add_Point (X, Min);
            Add_Point (X, Right);
         end if;
      else
         if Max > Y_Axis'Max (Left, Right) then
            Add_Point (X, Left);
            Add_Point (X, Max);
            Add_Point (X, Right);
         elsif Left = Right then
            Add_Point (X, Left);
         else
            Add_Point (X, Left);
            Add_Point (X, Right);
         end if;
      end if;
   end Add;

   Total_Min : Y_Axis := Y_Axis'Last;
   Total_Max : Y_Axis := Y_Axis'First;

   T : X_Axis := X_Axis'Succ (Layer.T1);
   V : Y_Axis;

   Left  : Point;
   Right : Point;
   First : Point;
   Last  : Point;

   Empty : Boolean := True;

   procedure Done is
      pragma Inline (Done);
   begin
      Layer.Sampled := True;
      Layer.Valid   := Data.Last_Count > 0;
      if 0 /= (Tracing_Mode and Trace_Waveform) then -------------------
         if Data.Last_Count > 0 then
            Trace
            (  Layer'Address,
               (  " >"
               &  Integer'Image (Data.Last_Count)
               &  " |"
               &  Integer'Image (Data.Count)
               &  " : "
               &  Edit.Image (GDouble (Data.Last_T))
               &  " L"
               &  Horizontal_Offset'Image (To_X (Layer.Last_T1))
               &  " = "
               &  Edit.Image (GDouble (Layer.Last_T1))
            )  );
         else
            Trace
            (  Layer'Address,
               (  " >"
               &  Integer'Image (Data.Last_Count)
               &  " : "
               &  Edit.Image (GDouble (Data.Last_T))
               &  " |"
               &  Integer'Image (Data.Count)
            )  );
         end if;
      end if; ----------------------------------------------------------
      if Layer.Amplifier_Object /= null then
         Layer.Amplifier_Object.Add_Range
         (  Layer => Layer,
            From  => Layer.T1,
            To    => Layer.T2,
            Lower => Total_Min,
            Upper => Total_Max
         );
      end if;
   end Done;

   Got_It : Boolean;
begin
   if Layer.Data = null then
      Layer.Sampled := True;
      Layer.Valid   := False;
      return;
   end if;
   if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------------
      Trace_Line (Layer'Address, "");
   end if; -------------------------------------------------------------
   -- Setting up conversion x-axis to horizontal coordinates
   --
   declare
      Length : constant Positive := 4 * Positive (X2 - X1 + 2);
   begin
      if Data.Points = null then
         Data.Points := new Points_Array (0..Length);
         Data.Last_T := X_Axis'First;
      elsif Data.Points'Length < Length then
         Free (Data.Points);
         Data.Points := new Points_Array (0..Length);
         Data.Last_T := X_Axis'First;
      end if;
   end;
   --
   -- Finding the leftmost values
   --
   if (  Layer.Valid
      and then
         Layer.Method = Resample_New_And_Stroke
      and then
         Data.Last_T in Layer.T1..Layer.T2 + Layer.dT
      and then
         Layer.Data.Is_In (Data.Last_T)
      )
   then
      Data.Count := Data.Last_Count;
      --
      -- Using already sampled points
      --
      declare
         Width : constant X_Axis := Layer.T2 - Layer.T1;
         Shift : Horizontal_Offset;
         Index : Natural := -- Index of the last sampled point
                 (  (Data.First + Data.Last_Count - 1)
                 mod
                    Data.Points'Length
                 );
      begin
         Layer.T1 := Layer.T1 - X_Axis'Remainder (Layer.T1, Layer.dT);
         Layer.T2 := Layer.T1 + Width;
         Shift :=
            Horizontal_Offset ((Layer.T1 - Layer.Last_T1) / Layer.dT);
         --
         -- Adjusting  the time margins  to make the shift a multiple of
         -- the time step
         --
         T := Data.Last_T;
         V := Data.Points (Index).Y;
         loop
            declare
               This : Point_Data renames Data.Points (Index);
            begin
               Total_Min := Y_Axis'Min (Total_Min, This.Y);
               Total_Max := Y_Axis'Max (Total_Max, This.Y);
               This.X := This.X - Shift;
               Data.Last_Count := Data.Last_Count - 1;
               exit when Data.Last_Count = 0 or else This.X <= X1;
               if Index = 0 then
                  Index := Data.Points'Last;
               else
                  Index := Index - 1;
               end if;
            end;
         end loop;
         Data.Count := Data.Count - Data.Last_Count;
         Data.First :=
            (Data.First + Data.Last_Count) mod Data.Points'Length;
         Data.Last_Count := Data.Count;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace
            (  Layer'Address,
               (  "T1 "
               &  Edit.Image (GDouble (Layer.T1))
               &  " L"
               &  Horizontal_Offset'Image (To_X (T))
               &  " = "
               &  Edit.Image (GDouble (T))
               &  " shift"
               &  Horizontal_Offset'Image (Shift)
               &  " = "
               &  Edit.Image (GDouble (X_Axis (Shift) * Layer.dT))
               &  ", count"
               &  Integer'Image (Data.Count)
               &  " @"
               &  Integer'Image (Data.First)
               &  ">|"
            )  );
            for Item in Index..Index + Data.Count - 1 loop
               Dump
               (  Layer,
                  Data.Points.all,
                  (Item + Data.Points'Length) mod Data.Points'Length
               );
            end loop;
         end if; -------------------------------------------------------
      end;
      Left := (T, V);
      Layer.Data.Forward (T, V, Got_It);
      if not Got_It then -- No new points
         Layer.Sampled := True;
         Layer.Valid   := Data.Last_Count > 0;
         Layer.Last_T1 := Layer.T1;
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            Trace
            (  Layer'Address,
               (  " >"
               &  Integer'Image (Data.Last_Count)
               &  " |"
               &  Integer'Image (Data.Count)
               &  " : "
               &  Edit.Image (GDouble (Data.Last_T))
               &  " L"
               &  Horizontal_Offset'Image (To_X (Layer.Last_T1))
               &  " = "
               &  Edit.Image (GDouble (Layer.Last_T1))
            )  );
         end if; -------------------------------------------------------
         return;
      end if;
      if 0 /= (Tracing_Mode and Trace_Waveform) then -------------------
         Trace
         (  Layer'Address,
            (  " |<"
            &  Integer'Image (Data.First)
            &  " T"
            &  Horizontal_Offset'Image (To_X (T))
            &  " = "
            &  Edit.Image (GDouble (T))
            &  " >|"
         )  );
      end if; ----------------------------------------------------------
      if T >= Layer.T2 then
         --
         -- No new visible points within
         --
         case Layer.Mode is
            when Gtk.Layered.Left =>
               Add_Point (To_X (Layer.T2), Left.V);
            when Linear =>
               Right :=
                  (  T => Layer.T2,
                     V => Interpolate (Layer.T2, Left, (T, V))
                  );
               Add_Point (To_X (Layer.T2), Right.V);
               Total_Min := Y_Axis'Min (Total_Min, Right.V);
               Total_Max := Y_Axis'Max (Total_Max, Right.V);
         end case;
         Layer.Last_T1 := Layer.T1;
         Done;
         return;
      end if;
      --
      -- The first point within the interval
      --
      First := (T, V);
   else
      Data.Count := 0;
      --
      -- Out of page span, resync
      --
      declare
         Width : constant X_Axis := Layer.T2 - Layer.T1;
      begin
         Layer.dT := (Layer.T2 - Layer.T1) / X_Axis (X2 - X1 + 1);
         if 0 /= (Tracing_Mode and Trace_Waveform) then ----------------
            if not Layer.Valid then
               Trace
               (  Layer'Address,
                  (  "sample [not Valid] step "
                  &  Edit.Image (GDouble (Layer.dT))
                  &  ">|"
               )  );
            elsif Data.Last_T not in Layer.T1..Layer.T2 + Layer.dT then
               Trace
               (  Layer'Address,
                  (  "sample [Last_T "
                  &  Edit.Image (GDouble (Data.Last_T))
                  &  " out "
                  &  Edit.Image (GDouble (Layer.T1))
                  &  ".."
                  &  Edit.Image (GDouble (Layer.T2 + Layer.dT))
                  &  "] step "
                  &  Edit.Image (GDouble (Layer.dT))
                  &  ">|"
               )  );
            elsif not Layer.Data.Is_In (Data.Last_T) then
               declare
                  T1, T2 : X_Axis;
                  V1, V2 : Y_Axis;
               begin
                  Layer.Data.First (T1, V1, Got_It);
                  if Got_It then
                     Layer.Data.Last  (T2, V2, Got_It);
                  end if;
                  if Got_It then
                     Trace
                     (  Layer'Address,
                        (  "sample [Last_T "
                        &  Edit.Image (GDouble (Data.Last_T))
                        &  " not in "
                        &  Edit.Image (GDouble (T1))
                        &  ".."
                        &  Edit.Image (GDouble (T2))
                        &  ">|"
                     )  );
                  else
                     Trace
                     (  Layer'Address,
                        (  "sample [Last_T "
                        &  Edit.Image (GDouble (Data.Last_T))
                        &  " not in empty>|"
                     )  );
                  end if;
               end;
            else
               Trace
               (  Layer'Address,
                  (  "sample [Last_T "
                  &  Edit.Image (GDouble (Data.Last_T))
                  &  "] step "
                  &  Edit.Image (GDouble (Layer.dT))
                  &  ">|"
               )  );
            end if;
         end if; -------------------------------------------------------
         Layer.T1 := Layer.T1 - X_Axis'Remainder (Layer.T1, Layer.dT);
         Layer.T2 := Layer.T1 + Width;
      end;
      T := X_Axis'Succ (Layer.T1);
      Data.Last_Count := 0;
      Data.First := 0;
      --
      -- Getting the point left or at Layer.T1
      --
      Layer.Data.Backward (T, V, Got_It);
      if not Got_It then -- No points left of Layer.T1
         Layer.Data.Forward (T, V, Got_It);
         if not Got_It then -- No points at all
            Layer.Sampled := True;
            Layer.Valid   := Data.Last_Count > 0;
            Layer.Last_T1 := Layer.T1;
            if 0 /= (Tracing_Mode and Trace_Waveform) then -------------
               Trace
               (  Layer'Address,
                  (  " >"
                  &  Integer'Image (Data.Last_Count)
                  &  " : "
                  &  Edit.Image (GDouble (Data.Last_T))
                  &  " |"
                  &  Integer'Image (Data.Count)
               )  );
            end if; ----------------------------------------------------
            return;
         end if;
         if T > Layer.T2 then -- No point left of Layer.T2
            Layer.Sampled := True;
            Layer.Valid   := Data.Last_Count > 0;
            Layer.Last_T1 := Layer.T1;
            if 0 /= (Tracing_Mode and Trace_Waveform) then -------------
               Trace
               (  Layer'Address,
                  (  " >"
                  &  Integer'Image (Data.Last_Count)
                  &  " |"
                  &  Integer'Image (Data.Count)
                  &  " : "
                  &  Edit.Image (GDouble (Data.Last_T))
                  &  " L"
                  &  Horizontal_Offset'Image (To_X (Layer.Last_T1))
                  &  " = "
                  &  Edit.Image (GDouble (Layer.Last_T1))
               )  );
            end if; ----------------------------------------------------
            if not Layer.Extrapolate_Left then
               return;
            end if;
         end if;
      end if;
      --
      -- Getting point right of Layer.T1
      --
      if T > Layer.T1 then -- Starting after Layer.T1
         Total_Min := V;
         Total_Max := V;
         Left  := (T, V);
         First := Left;
      elsif T = Layer.T1 then -- Starting at Layer.T1
         Total_Min := V;
         Total_Max := V;
         Left  := (T, V);
         First := Left;
         Empty := False;
         Add_Point (To_X (First.T), First.V);
      else -- Starting before Layer.T1
         Left := (T, V);
         loop
            Layer.Data.Forward (T, V, Got_It);
            if not Got_It then -- No points visible
               if Layer.Extrapolate_Right then
                  Total_Min := Left.V;
                  Total_Max := Left.V;
                  First := (Layer.T1, Left.V);
                  exit;
               end if;
               Layer.Sampled := True;
               Layer.Valid   := Data.Last_Count > 0;
               Layer.Last_T1 := Layer.T1;
               if 0 /= (Tracing_Mode and Trace_Waveform) then ----------
                  Trace
                  (  Layer'Address,
                     (  " >"
                     &  Integer'Image (Data.Last_Count)
                     &  " |"
                     &  Integer'Image (Data.Count)
                     &  " : "
                     &  Edit.Image (GDouble (Data.Last_T))
                     &  " L"
                     &  Horizontal_Offset'Image (To_X(Layer.Last_T1))
                     &  " = "
                     &  Edit.Image (GDouble (Layer.Last_T1))
                  )  );
               end if; -------------------------------------------------
               return;
            end if;
            if T >= Layer.T2 then
               --
               -- No visible points  within  the box, but one point left
               -- of Layer.T1 and one point right of Layer.T2
               --
               case Layer.Mode is
                  when Gtk.Layered.Left =>
                     First := (Layer.T1, Left.V);
                     Last  := (Layer.T2, Left.V);
                  when Linear =>
                     Right := (T, V);
                     First := (  T => Layer.T1,
                                 V => Interpolate (First.T, First, Last)
                              );
                     Last  := (  T => Layer.T2,
                                 V => Interpolate (Last.T, First, Last)
                              );
               end case;
               Add_Point (To_X (First.T), First.V);
               Add_Point (To_X (Last.T),  Last.V);
               Total_Min := Y_Axis'Min (First.V, Last.V);
               Total_Max := Y_Axis'Max (First.V, Last.V);
               Layer.Last_T1 := Layer.T1;
               Done;
               return;
            elsif T <= Layer.T1 then
               --
               -- Before  or  at the  left  boundary  (Layer.T1)  of the
               -- interval
               --
               Left := (T, V);
               if T = Layer.T1 then -- Exactly at it
                  First := Left;
                  Empty := False;
                  Add_Point (To_X (Left.T), Left.V);
                  Total_Min := V;
                  Total_Max := V;
                  exit;
               end if;
            else
               --
               -- The first point within the interval
               --
               First := (T, V);
               Empty := False;
               case Layer.Mode is
                  when Gtk.Layered.Left =>
                     Total_Min := Left.V;
                     Total_Max := Total_Min;
                     Add_Point (To_X (Layer.T1), Total_Min);
                  when Linear =>
                     Total_Min := Interpolate (Layer.T1, Left, First);
                     Total_Max := Total_Min;
                     Add_Point (To_X (Layer.T1), Total_Min);
               end case;
               exit;
            end if;
         end loop;
      end if;
   end if;
   Layer.Last_T1 := Layer.T1;
   --
   -- Sampling missing data
   --
   case Layer.Mode is
      when Gtk.Layered.Left =>
         --
         -- Constant interpolation to the left point
         --
         --      Max | /\    _____ out
         --  in _____|/  \  /|
         --               \/ | Min
         --    pixel <------>
         --
         loop
            --
            -- Left  is the  last point  outside  the interval  or at T1
            -- First is the first point inside the interval
            --
            declare
               Count : Natural := 1;
               Max   : Y_Axis  := First.V;
               Min   : Y_Axis  := First.V;
               X     : Horizontal_Offset;
               T2    : X_Axis;
            begin
               Set_Interval_Bounds (T, X, T2);
               loop
                  --
                  -- Looking for the points within this interval
                  --
                  Layer.Data.Forward (T, V, Got_It);
                  if not Got_It then -- No more points left
                     X := Horizontal_Offset'Max (X, X1);
                     if (  Layer.Extrapolate_Left
                        and then
                           Empty
                        and then
                           Count = 1
                        and then X > X1
                        )
                     then
                        Add_Point (X1, Left.V);
                     end if;
                     Add (X, Left.V, Max, Min, Max);
                     Total_Min := Y_Axis'Min (Total_Min, Min);
                     Total_Max := Y_Axis'Max (Total_Max, Max);
                     if Layer.Extrapolate_Right and then X < X2 then
                        Add_Point (X2, Max);
                     end if;
                     Done;
                     return;
                  end if;
                  exit when T >= T2;
                  Last  := (T, V);
                  Count := Count + 1;
                  Min   := Y_Axis'Min (Min, Last.V);
                  Max   := Y_Axis'Max (Max, Last.V);
               end loop;
               if T = T2 then -- Last point of the pixel
                  Last  := (T, V);
                  Count := Count + 1;
                  Min   := Y_Axis'Min (Min, V);
                  Max   := Y_Axis'Max (Max, V);
                  Right := Last;
               else
                  Right := (T, V);
               end if;
               if Count = 1 then -- Only one point in the interval
                  if (  Layer.Extrapolate_Left
                     and then
                        Empty
                     and then
                        X > X1
                     )
                  then
                     Add_Point (X1, Left.V);
                  end if;
                  Add (X, Left.V, First.V, Min, Max);
                  Left := First;
                  Data.Last_Count := Data.Count;
                  Data.Last_T := First.T;
               else -- Multiple points on the interval
                  Add (X, Left.V, Last.V, Min, Max);
                  Left := Last;
                  Data.Last_Count := Data.Count;
                  Data.Last_T := Last.T;
               end if;
               Total_Min := Y_Axis'Min (Total_Min, Min);
               Total_Max := Y_Axis'Max (Total_Max, Max);
               if T > Layer.T2 then -- No more points visible
                  Add_Point (To_X (Layer.T2), Left.V);
                  Done;
                  return;
               end if;
               First := Right;
               Empty := False;
            end;
         end loop;
      when Linear =>
         --
         -- Linear interpolation between points
         --
         --                   /
         --      Max | /\    / out
         --       in |/  \  /|
         --         /     \/ | Min
         --       \/ <------> pixel
         --
         loop
            --
            -- Left  is the  last point  outside  the interval  or at T1
            -- First is the first point inside the interval
            --
            declare
               Count : Natural := 1;
               Max   : Y_Axis  := First.V;
               Min   : Y_Axis  := First.V;
               X     : Horizontal_Offset;
               T2    : X_Axis;
            begin
               Set_Interval_Bounds (T, X, T2);
               loop
                  --
                  -- Looking for the points within this interval
                  --
                  Layer.Data.Forward (X_Axis (T), Y_Axis (V), Got_It);
                  if not Got_It then -- No more points left
                     if Count = 1 then
                        if (  Layer.Extrapolate_Left
                           and then
                              Empty
                           and then
                              X > X1
                           )
                        then
                           Add_Point (X1, First.V);
                        end if;
                        Add_Point (X, First.V);
                     else
                        Add
                        (  X,
                           Interpolate (T2 - Layer.dT, Left, First),
                           Max,
                           Min,
                           Max
                        );
                     end if;
                     Total_Min := Y_Axis'Min (Total_Min, Min);
                     Total_Max := Y_Axis'Max (Total_Max, Max);
                     if Layer.Extrapolate_Right and then X < X2 then
                        Add_Point (X2, Max);
                     end if;
                     Done;
                     return;
                  end if;
                  exit when T >= T2;
                  Last  := (T, V);
                  Count := Count + 1;
                  Min   := Y_Axis'Min (Min, Last.V);
                  Max   := Y_Axis'Max (Max, Last.V);
               end loop;
               if T = T2 then -- Last point of the pixel
                  Last  := (T, V);
                  Count := Count + 1;
                  Min   := Y_Axis'Min (Min, V);
                  Max   := Y_Axis'Max (Max, V);
                  Right := Last;
               else
                  Right := (T, V);
               end if;
               if Count = 1 then -- Only one point in the interval
                  if (  Layer.Extrapolate_Left
                     and then
                        Empty
                     and then
                        X > X1
                     )
                  then
                     Add_Point (X1, First.V);
                  end if;
                  Add_Point (X, First.V);
                  Left := First;
                  Data.Last_Count := Data.Count;
                  Data.Last_T := First.T;
               else -- Multiple points on the interval
                  Add
                  (  X,
                     Interpolate (T2 - Layer.dT, Left, First),
                     Interpolate (T2, Last, Right),
                     Min, Max
                  );
                  Left := Right;
                  Data.Last_Count := Data.Count;
                  Data.Last_T := Last.T;
               end if;
               Total_Min := Y_Axis'Min (Total_Min, Min);
               Total_Max := Y_Axis'Max (Total_Max, Max);
               if T > Layer.T2 then -- No more points visible
                  if Count = 1 then
                     Right :=
                        (  Layer.T2,
                           Interpolate (Layer.T2, First, Right)
                        );
                  else
                     Right :=
                        (  Layer.T2,
                           Interpolate (Layer.T2, Last, Right)
                        );
                  end if;
                  Add_Point (To_X (Right.T), Right.V);
                  Total_Min := Y_Axis'Min (Total_Min, Right.V);
                  Total_Max := Y_Axis'Max (Total_Max, Right.V);
                  Done;
                  return;
               end if;
               First := Right;
               Empty := False;
            end;
         end loop;
   end case;
exception
   when Error : others =>
      Log
      (  GtkAda_Contributions_Domain,
         Log_Level_Critical,
         (  "Fault: "
         &  Exception_Information (Error)
         &  Where ("Sample_Lines")
      )  );
end Sample_Lines;
