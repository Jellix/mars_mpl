with Ada.Exceptions;
with Ada.Real_Time;
pragma Elaborate_All (Ada.Real_Time);

package Global is

   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

   function Clock_Image
     (Time : in Ada.Real_Time.Time := Ada.Real_Time.Clock) return String;
   --  Returns a time stamp relative to the Start_Time, i.e. Time - Start_Time.

   generic
      Unit_Name : String;
   package Log is
      procedure Trace (Message : in String);
      procedure Trace (E       : in Ada.Exceptions.Exception_Occurrence);
   end Log;

end Global;
