with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

package body Global is

   package Duration_IO is new Ada.Text_IO.Fixed_IO (Num => Duration);

   use type Ada.Real_Time.Time;

   Space_To_Zero : constant Ada.Strings.Maps.Character_Mapping :=
                     Ada.Strings.Maps.To_Mapping (From => " ",
                                                  To   => "0");

   protected Logger is
      procedure Write (Msg : in String);
   end Logger;

   protected body Logger is
      procedure Write (Msg : in String) is
      begin
         Ada.Text_IO.Put_Line (Msg);
         --  FIXME: Using a potentially blocking operation within a protected procedure is a bounded error.
      end Write;
   end Logger;

   procedure Log (Message : String) is
      Elapsed_Time : constant Duration :=
                       Ada.Real_Time.To_Duration
                         (TS => Ada.Real_Time.Clock - Start_Time);
      Duration_Image : String := "XXXX.XXX";
   begin
      Duration_IO.Put (Item => Elapsed_Time,
                       To   => Duration_Image,
                       Aft  => 3,
                       Exp  => 0);

      Logger.Write (Msg =>
                      "["
                    & Ada.Strings.Fixed.Translate (Source  => Duration_Image,
                                                   Mapping => Space_To_Zero)
                    & "] "
                    & Message);
   end Log;

end Global;
