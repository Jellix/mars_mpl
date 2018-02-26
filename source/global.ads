with Ada.Exceptions;
with Ada.Real_Time;

--  @summary
--  Provides the Start_Time and some logging facilities.
--
--  @description
--  The start time is a global constant initialized at startup and used in
--  calculating time stamps relative to the start time.
--  Additionally, a generic Log package is provided which can be instantiated
--  with a (preferrably) unique name denoting the unit for which it is
--  instantiated and used within that unit to log messages and exceptions.
--
package Global is

   Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   --  Time at startup of the application.

   function Clock_Image
     (Time : in Ada.Real_Time.Time := Ada.Real_Time.Clock) return String;
   --  Returns a time stamp relative to the global Start_Time.
   --
   --  @param Time The time to be used in calculating the time stamp. Defaults
   --  to the current real time clock.
   --
   --  @return A human readable time stamp relative to the Start_Time, i.e.
   --  Time - Start_Time.

   generic
      Unit_Name : String;
      --  Name of the unit for which the logger is instantiated. This name and
      --  the time stamp is prepended to each log message.
   package Log is
      procedure Trace (Message : in String);
      --  Log a message to the output.
      --
      --  @param Message The message to be logged.

      procedure Trace (E : in Ada.Exceptions.Exception_Occurrence);
      --  Log an exception to the output.
      --
      --  @param E The exception to be logged.
   end Log;

end Global;
