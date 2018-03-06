separate (Thrusters)
package body Valve_Timing is

   protected Control is
      entry Wait_For_Off (At_Time : out Ada.Real_Time.Time);
      procedure Schedule_Off (At_Time : in Ada.Real_Time.Time);
      procedure Cancel;
      function Is_Cancelled return Boolean;
      procedure Wake_Up;
   private
      Turn_Off_At : Ada.Real_Time.Time := Global.Start_Time;
      Cancelled   : Boolean            := True;
      Scheduled   : Boolean            := False;
   end Control;

   procedure Do_Cancel is
   begin
      Control.Cancel;
   end Do_Cancel;

   procedure Do_Schedule (At_Time : in Ada.Real_Time.Time) is
   begin
      Control.Schedule_Off (At_Time => At_Time);
   end Do_Schedule;

   procedure Do_Wake_Up is
   begin
      Control.Wake_Up;
   end Do_Wake_Up;

   protected body Control is
      entry Wait_For_Off (At_Time : out Ada.Real_Time.Time) when Scheduled is
      begin
         At_Time := Turn_Off_At;
         Scheduled := False;
      end Wait_For_Off;

      procedure Cancel is
      begin
         Cancelled := True;
         Scheduled := False; --  Close barrier.
      end Cancel;

      function Is_Cancelled return Boolean is
        (Cancelled);

      procedure Schedule_Off (At_Time : in Ada.Real_Time.Time) is
      begin
         Turn_Off_At := At_Time;
         Cancelled   := False;
         Scheduled   := True;
         --  trigger waiting task to turn off the valve at given time.
      end Schedule_Off;

      procedure Wake_Up is
      begin
         Turn_Off_At := Global.Start_Time; --  Immediate trigger
         Cancelled   := True; --  Don't do anything.
         Scheduled   := True; --  Open barrier for waiting tasks.
      end Wake_Up;
   end Control;

   task Valve_Control;

   task body Valve_Control is
      At_Time : Ada.Real_Time.Time;
   begin
      while not Aborted loop
         Control.Wait_For_Off (At_Time => At_Time);
         delay until At_Time;

         if not Control.Is_Cancelled then
            --  Off command was not cancelled yet.
            Engine_State.Set (Value => Disabled);
         end if;
      end loop;
   end Valve_Control;

end Valve_Timing;
