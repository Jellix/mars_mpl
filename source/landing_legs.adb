with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Text_IO;

package body Landing_Legs is

   use type Ada.Real_Time.Time;

   Leg : array (Legs_Index) of Boolean with Atomic_Components;

   package Random_Leg is new Ada.Numerics.Discrete_Random (Result_Subtype => Legs_Index);
   package Random_Time is new Ada.Numerics.Discrete_Random (Result_Subtype => Sensor_Glitch);

   task Simulate_Landing_Legs;
   task body Simulate_Landing_Legs is
      Next_Cycle : Ada.Real_Time.Time := Ada.Real_Time.Clock + Ada.Real_Time.Seconds (1);
      Legs_G     : Random_Leg.Generator;
      Time_G     : Random_Time.Generator;
   begin
      Random_Leg.Reset (Gen => Legs_G);
      Random_Time.Reset (Gen => Time_G);

      loop
         delay until Next_Cycle;

         declare
            Selected_Leg : constant Legs_Index    := Random_Leg.Random (Gen => Legs_G);
            MS_Triggered : constant Sensor_Glitch := Random_Time.Random (Time_G);
         begin
            Ada.Text_IO.Put_Line ("Landing leg" & Legs_Index'Image (Selected_Leg) & " triggered for" & Sensor_Glitch'Image (MS_Triggered) & "ms.");
            Leg (Selected_Leg) := True;
            delay until Next_Cycle + Ada.Real_Time.Milliseconds (MS_Triggered);
            Leg (Selected_Leg) := False;
         end;

         Next_Cycle := Next_Cycle + Ada.Real_Time.Seconds (1);
      end loop;
   end Simulate_Landing_Legs;

   function Read_State (Index : in Legs_Index) return Boolean is
   begin
      return Leg (Index);
   end Read_State;

end Landing_Legs;
