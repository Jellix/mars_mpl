with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

package body Global is

   use type Ada.Real_Time.Time;

   package Duration_IO is new Ada.Text_IO.Fixed_IO (Num => Duration);

   Space_To_Zero : constant Ada.Strings.Maps.Character_Mapping :=
                     Ada.Strings.Maps.To_Mapping (From => " ",
                                                  To   => "0");

   function Clock_Image
     (Time : in Ada.Real_Time.Time := Ada.Real_Time.Clock) return String
   is
      Duration_Image    : String  := "XXXXX.XXX"; -- Enough digits for a day.
      Exception_Occured : Boolean := False;
   begin
      Handle_Conversion_Exception :
      begin
         Duration_IO.Put
           (Item => Ada.Real_Time.To_Duration (TS => Time - Start_Time),
            To   => Duration_Image,
            Aft  => 3,
            Exp  => 0);
         Ada.Strings.Fixed.Translate (Source  => Duration_Image,
                                      Mapping => Space_To_Zero);
      exception
         when Ada.Text_IO.Layout_Error =>
            --  Most likely overflow in the Duration_IO.Put due to running much
            --  longer than expected. Don't crash, but return something to
            --  indicate this conversion error.
            Exception_Occured := True;
      end Handle_Conversion_Exception;

      return (if Exception_Occured
              then "TIME_OVERFLOW"
              else Duration_Image);
   end Clock_Image;

   protected Logger is
      procedure Write (Msg : in String);
   end Logger;

   protected body Logger is
      procedure Write (Msg : in String) is
      begin
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Output,
                               Item => Msg);
      exception
         when E : others =>
            Handle_IO_Error :
            begin
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Error,
                  Item => Ada.Exceptions.Exception_Information (E));
            exception
               when others =>
                  --  Error logging failed, too. Can't do much about it, but we
                  --  do not want to crash.
                  null;
            end Handle_IO_Error;
      end Write;
   end Logger;

   package body Log is

      procedure Trace (Message : in String) is
      begin
         Logger.Write ("[" & Unit_Name & "] [" & Clock_Image & "] " & Message);
      end Trace;

      procedure Trace (E : in Ada.Exceptions.Exception_Occurrence) is
      begin
         Logger.Write ("[" & Unit_Name & "] [" & Clock_Image & "] "
                       & Ada.Exceptions.Exception_Information (E));
      end Trace;

   end Log;

end Global;
