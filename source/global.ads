with Ada.Real_Time;

package Global with SPARK_Mode is

   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   procedure Log (Message : String);

end Global;
