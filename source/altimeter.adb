package body Altimeter is

   use type Ada.Real_Time.Time;

   protected Altimeter_Store is
      procedure Set_Height (New_Height : Height_Above_Ground);
      function Get_Height return Height_Above_Ground;
   private
      Height : Height_Above_Ground := Height_Above_Ground'Last;
   end Altimeter_Store;

   protected body Altimeter_Store is
      procedure Set_Height (New_Height : Height_Above_Ground) is
      begin
         Height := New_Height;
      end Set_Height;

      function Get_Height return Height_Above_Ground is
      begin
         return Height;
      end Get_Height;
   end;

   task Radar_Simulator;
   task body Radar_Simulator is
      Base_Height : constant Height_Above_Ground := 1000;
      Start_Time  : constant Ada.Real_Time.Time  := Ada.Real_Time.Clock;

      Next_Cycle  : Ada.Real_Time.Time := Start_Time;
      Measurement : Height_Above_Ground;
   begin
      Measurement := Base_Height;

      while Measurement > 0 loop
         Measurement := Base_Height - Height_Above_Ground (20 * Ada.Real_Time.To_Duration (Next_Cycle - Start_Time));
         Altimeter_Store.Set_Height (Measurement);

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;

      Altimeter_Store.Set_Height (0);
   end Radar_Simulator;

   function Current_Height return Height_Above_Ground is
   begin
      return Altimeter_Store.Get_Height;
   end Current_Height;

   function Image (H : Height_Above_Ground) return String is
   begin
      return Height_Above_Ground'Image (H) & " m";
   end Image;

end Altimeter;
