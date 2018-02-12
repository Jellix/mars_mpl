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
      Duration_Image : String := "XXXX.XXX";
   begin
      Duration_IO.Put
        (Item => Ada.Real_Time.To_Duration (TS => Time - Start_Time),
         To   => Duration_Image,
         Aft  => 3,
         Exp  => 0);

      return Ada.Strings.Fixed.Translate (Source  => Duration_Image,
                                          Mapping => Space_To_Zero);
   end Clock_Image;

   protected Logger is
      procedure Write (Msg : String);
   end Logger;

   protected body Logger is
      procedure Write (Msg : in String) is
      begin
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, Msg);
      end Write;
   end Logger;

   procedure Trace (Message : in String) is
   begin
      Logger.Write ("[" & Unit_Name & "] [" & Clock_Image & "] " & Message);
   end Trace;

end Global;
