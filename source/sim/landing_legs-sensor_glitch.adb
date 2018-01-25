with Ada.Numerics.Discrete_Random;

separate (Landing_Legs)
package body Sensor_Glitch is

   use type Ada.Real_Time.Time_Span;

   package Random_Delay is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Glitch_Delay);

   package Random_Duration is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Glitch_Duration);

   pragma Warnings (Off, "declaration hides ""Task_Control""");
   protected type Task_Control is
      procedure Trigger_Glitch (At_Time      : in Ada.Real_Time.Time;
                                For_Duration : in Ada.Real_Time.Time_Span);

      entry Wait_For_Event (At_Time      : out Ada.Real_Time.Time;
                            For_Duration : out Ada.Real_Time.Time_Span);
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

   Assign_Leg : Leg_Iterator;

   task body Spurious_Trigger is
      The_Leg      : Shared_Types.Legs_Index;
      Activate_At  : Ada.Real_Time.Time;
      Activate_For : Ada.Real_Time.Time_Span;
   begin
      Assign_Leg.Next (The_Leg => The_Leg);
      Control_Object (The_Leg).Wait_For_Event (At_Time      => Activate_At,
                                               For_Duration => Activate_For);

      delay until Activate_At;
      Legs_State (The_Leg) := Shared_Types.Touched_Down;
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image
         & "] Landing leg " & Shared_Types.Legs_Index'Image (The_Leg)
         & " triggered.");

      delay until Activate_At + Activate_For;
      Legs_State (The_Leg) := Shared_Types.In_Flight;
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image
         & "] Landing leg "
         & Shared_Types.Legs_Index'Image (The_Leg)
         & " triggered for"
         & Integer'Image (Activate_For / Ada.Real_Time.Milliseconds (1))
         & " ms.");
   exception
      when E : others =>
         Logger.all.Trace (E => E);
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
         declare
            Trigger_Offset : constant Glitch_Delay    :=
                               Random_Delay.Random (Gen => Delay_G);
            Trigger_Length : constant Glitch_Duration :=
                               Random_Duration.Random (Gen => Duration_G);
         begin
            Control_Object (The_Leg).Trigger_Glitch
              (At_Time      => Now + Ada.Real_Time.Milliseconds (Trigger_Offset),
               For_Duration => Ada.Real_Time.Milliseconds (Trigger_Length));
            Logger.all.Trace
              (Message =>
                 "[" & Global.Clock_Image
               & "] Landing leg "
               & Shared_Types.Legs_Index'Image (The_Leg)
               & " scheduled to trigger in"
               & Glitch_Duration'Image (Trigger_Offset)
               & " ms (@"
               & Global.Clock_Image (Now + Ada.Real_Time.Milliseconds (MS => Trigger_Offset))
               & ") for"
               & Glitch_Duration'Image (Trigger_Length)
               & " ms (@"
               & Global.Clock_Image (Now + Ada.Real_Time.Milliseconds (MS => Trigger_Offset + Trigger_Length))
               & ").");
         end;
      end loop;
   end Activate_Glitch;

   procedure Initialize is
   begin
      Random_Delay.Reset    (Gen => Delay_G);
      Random_Duration.Reset (Gen => Duration_G);
   end Initialize;

end Sensor_Glitch;
