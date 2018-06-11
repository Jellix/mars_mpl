with Ada.Numerics.Discrete_Random;
with Landing_Legs_Iterator;

separate (Landing_Legs)
package body Sensor_Glitch is

   use type Ada.Real_Time.Time_Span;

   package Random_Delay is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Glitch_Delay);

   package Random_Duration is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Glitch_Duration);

   pragma Warnings (Off, "declaration hides ""Task_Control""");
   protected type Task_Control is
      entry Wait_For_Event (At_Time      : out Ada.Real_Time.Time;
                            For_Duration : out Ada.Real_Time.Time_Span);
      procedure Trigger_Glitch (At_Time      : in Ada.Real_Time.Time;
                                For_Duration : in Ada.Real_Time.Time_Span);
   private
      Event_Triggered : Boolean := False;
      At_Time         : Ada.Real_Time.Time;
      For_Duration    : Ada.Real_Time.Time_Span;
   end Task_Control;
   pragma Warnings (On, "declaration hides ""Task_Control""");

   protected body Task_Control is
      procedure Trigger_Glitch (At_Time      : in Ada.Real_Time.Time;
                                For_Duration : in Ada.Real_Time.Time_Span) is
      begin
         Task_Control.At_Time      := At_Time;
         Task_Control.For_Duration := For_Duration;

         Task_Control.Event_Triggered := True;
      end Trigger_Glitch;

      entry Wait_For_Event (At_Time      : out Ada.Real_Time.Time;
                            For_Duration : out Ada.Real_Time.Time_Span)
        when Event_Triggered is
      begin
         At_Time      := Task_Control.At_Time;
         For_Duration := Task_Control.For_Duration;
      end Wait_For_Event;
   end Task_Control;

   type Control_Objects is array (Shared_Types.Legs_Index) of Task_Control;
   Control_Object : Control_Objects;

   task type Spurious_Trigger;

   Assign_Leg : Landing_Legs_Iterator.Leg_Iterator;

   task body Spurious_Trigger is
      The_Leg      : Shared_Types.Legs_Index;
      Activate_At  : Ada.Real_Time.Time;
      Activate_For : Ada.Real_Time.Time_Span;
   begin
      Assign_Leg.Next (The_Leg => The_Leg);

      Handle_Leg_Event :
      declare
         Leg_String : constant String
           := "Landing leg " & Shared_Types.Legs_Index'Image (The_Leg);
      begin
         Control_Object (The_Leg).Wait_For_Event (At_Time      => Activate_At,
                                                  For_Duration => Activate_For);

         delay until Activate_At;
         Legs_State (The_Leg).Set (New_Value => Shared_Types.Touched_Down);
         Log.Trace (Message => Leg_String & " triggered.");

         delay until Activate_At + Activate_For;
         Legs_State (The_Leg).Set (New_Value => Shared_Types.In_Flight);
         Log.Trace
           (Message =>
              Leg_String & " triggered for"
            & Integer'Image (Activate_For / Ada.Real_Time.Milliseconds (1))
            & " ms.");
      end Handle_Leg_Event;
   exception
      when E : others =>
         Log.Trace (E => E);
   end Spurious_Trigger;

   type Glitch_Tasks is array (Shared_Types.Legs_Index) of Spurious_Trigger;

   Delay_G        : Random_Delay.Generator;
   Duration_G     : Random_Duration.Generator;
   Glitch_Task    : Glitch_Tasks;
   pragma Unreferenced (Glitch_Task);

   procedure Activate_Glitch is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      for The_Leg in Shared_Types.Legs_Index'Range loop
         Schedule_Leg_Event :
         declare
            Trigger_Offset   : constant Glitch_Delay    :=
                                 Random_Delay.Random (Gen => Delay_G);
            Trigger_Duration : constant Glitch_Duration :=
                                 Random_Duration.Random (Gen => Duration_G);
            Trigger_At       : constant Ada.Real_Time.Time :=
                                 Now +
                                   Ada.Real_Time.Milliseconds
                                     (MS => Trigger_Offset);
            Trigger_Length   : constant Ada.Real_Time.Time_Span :=
                                 Ada.Real_Time.Milliseconds
                                   (MS => Trigger_Duration);
            Trigger_Until    : constant Ada.Real_Time.Time :=
                                 Trigger_At + Trigger_Length;
         begin
            Control_Object (The_Leg).Trigger_Glitch
              (At_Time      => Trigger_At,
               For_Duration => Trigger_Length);
            Log.Trace
              (Message =>
                 "Landing leg "
               & Shared_Types.Legs_Index'Image (The_Leg)
               & " scheduled to trigger in"
               & Glitch_Duration'Image (Trigger_Offset)
               & " ms (@"
               & Global.Clock_Image (Time => Trigger_At)
               & ") for"
               & Glitch_Duration'Image (Trigger_Duration)
               & " ms (@"
               & Global.Clock_Image (Time => Trigger_Until)
               & ").");
         end Schedule_Leg_Event;
      end loop;
   end Activate_Glitch;

   procedure Initialize is
   begin
      Random_Delay.Reset    (Gen => Delay_G);
      Random_Duration.Reset (Gen => Duration_G);
   end Initialize;

end Sensor_Glitch;
