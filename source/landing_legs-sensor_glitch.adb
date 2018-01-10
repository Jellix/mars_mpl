with Ada.Numerics.Discrete_Random;

separate (Landing_Legs)
package body Sensor_Glitch is

   package Random_Delay is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Glitch_Delay);

   package Random_Duration is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Glitch_Duration);

   task type Spurious_Trigger
   is
      entry Execute (For_Leg      : in Legs_Index;
                     At_Time      : in Real_Time.Time;
                     For_Duration : in Real_Time.Time_Span);
   end Spurious_Trigger;

   task body Spurious_Trigger is
      The_Leg      : Legs_Index;
      Activate_At  : Ada.Real_Time.Time;
      Activate_For : Ada.Real_Time.Time_Span;

      use type Ada.Real_Time.Time_Span;
   begin
      accept Execute (For_Leg      : in Legs_Index;
                      At_Time      : in Real_Time.Time;
                      For_Duration : in Real_Time.Time_Span)
      do
         The_Leg      := For_Leg;
         Activate_At  := At_Time;
         Activate_For := For_Duration;
      end Execute;

      delay until Activate_At;
      Legs_State (The_Leg) := Touched_Down;
      Global.Log
        (Message =>
           "Landing leg "
         & Legs_Index'Image (The_Leg)
         & " triggered.");

      delay until Activate_At + Activate_For;
      Legs_State (The_Leg) := In_Flight;
      Global.Log
        (Message =>
           "Landing leg "
         & Legs_Index'Image (The_Leg)
         & " triggered for"
         & Integer'Image (Activate_For / Real_Time.Milliseconds (1))
         & " ms.");
   end Spurious_Trigger;

   type Glitch_Tasks is array (Legs_Index) of Spurious_Trigger;

   Delay_G     : Random_Delay.Generator;
   Duration_G  : Random_Duration.Generator;
   Glitch_Task : Glitch_Tasks;

   procedure Activate_Glitch is
      Now : constant Real_Time.Time := Real_Time.Clock;
   begin
      for The_Leg in Legs_Index'Range loop
         declare
            Trigger_Offset : constant Glitch_Delay    :=
                               Random_Delay.Random (Gen => Delay_G);
            Trigger_Length : constant Glitch_Duration :=
                               Random_Duration.Random (Gen => Duration_G);
         begin
            Glitch_Task (The_Leg).Execute
              (For_Leg      => The_Leg,
               At_Time      => Now + Real_Time.Milliseconds (Trigger_Offset),
               For_Duration => Real_Time.Milliseconds (Trigger_Length));
            Global.Log
              (Message =>
                 "Landing leg "
               & Legs_Index'Image (The_Leg)
               & " scheduled to trigger in"
               & Glitch_Duration'Image (Trigger_Offset)
               & " ms for"
               & Glitch_Duration'Image (Trigger_Length)
               & " ms.");
         end;
      end loop;
   end Activate_Glitch;

   procedure Initialize is
   begin
      Random_Delay.Reset    (Gen => Delay_G);
      Random_Duration.Reset (Gen => Duration_G);
   end Initialize;

end Sensor_Glitch;
