-- @summary
-- Root package for configuration related things.
with Ada.Real_Time;

package Configuration is

   function Microseconds (US : in Integer) return Ada.Real_Time.Time_Span
                          renames Ada.Real_Time.Microseconds;
   --  @param US Number of microseconds.
   --  @return A time span representing a length on US microseconds.

   function Milliseconds (MS : in Integer) return Ada.Real_Time.Time_Span
                          renames Ada.Real_Time.Milliseconds;
   --  @param MS Number of milliseconds.
   --  @return A time span representing a length on MS milliseconds.

end Configuration;
