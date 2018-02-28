-- @summary
-- Root package for configuration related things.
with Ada.Real_Time;

package Configuration is

   function Microseconds (US : in Integer) return Ada.Real_Time.Time_Span
                          renames Ada.Real_Time.Microseconds;

   function Milliseconds (MS : in Integer) return Ada.Real_Time.Time_Span
                          renames Ada.Real_Time.Milliseconds;

end Configuration;
