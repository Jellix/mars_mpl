with Ada.Text_IO;

package body Global with SPARK_Mode => Off is

   use type Ada.Real_Time.Time;

   protected Semaphore is
      entry Seize;
      entry Release;
   private
      Seized : Boolean := False;
   end Semaphore;

   protected body Semaphore is
      entry Seize when not Seized is
      begin
         Seized := True;
      end Seize;

      entry Release when Seized is
      begin
         Seized := False;
      end Release;
   end Semaphore;

   procedure Log (Message : String) is
      Elapsed_Time : constant Duration :=
                       Ada.Real_Time.To_Duration
                         (Ada.Real_Time.Clock - Start_Time);
   begin
      Semaphore.Seize;
      Ada.Text_IO.Put_Line ("[" & Duration'Image (Elapsed_Time) & " ] " &
                              Message);
      Semaphore.Release;
   end Log;

end Global;
