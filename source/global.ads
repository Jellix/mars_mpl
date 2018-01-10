-- pragma Profile (Ravenscar);
-- pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
pragma Elaborate_All (Ada.Real_Time);

package Global is

   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   procedure Log (Message : String);

   function Clock_Image
     (Time : Ada.Real_Time.Time := Ada.Real_Time.Clock) return String;
   --  Returns a time stamp relative to the Start_Time, i.e. Time - Start_Time.

end Global;
