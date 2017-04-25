with Ada.Text_IO;

package body Global is

   use type Ada.Real_Time.Time;

   protected Logger is
      procedure Write (Msg : in String);
   end Logger;

   protected body Logger is
      procedure Write (Msg : in String) is
      begin
         Ada.Text_IO.Put_Line (Msg);
      end Write;
   end Logger;

   procedure Log (Message : String) is
      Elapsed_Time : constant Duration :=
                       Ada.Real_Time.To_Duration
                         (TS => Ada.Real_Time.Clock - Start_Time);
   begin
      Logger.Write
        (Msg => "[" & Duration'Image (Elapsed_Time) & " ] " & Message);
   end Log;

end Global;
