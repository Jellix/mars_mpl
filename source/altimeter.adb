with Global;

package body Altimeter with SPARK_Mode => Off is

   use type Ada.Real_Time.Time;

   protected Velocity_Store is
      procedure Set (New_Value : Velocity);
      function Get return Velocity;
   private
      Value : Velocity := Velocity'First;
   end Velocity_Store;

   protected body Velocity_Store is
      procedure Set (New_Value : Velocity) is
      begin
         Value := New_Value;
      end Set;

      function Get return Velocity is
      begin
         return Value;
      end Get;
   end Velocity_Store;

   protected Altimeter_Store is
      procedure Set (New_Value : Height);
      function Get return Height;
   private
      Value : Height := Height'Last;
   end Altimeter_Store;

   protected body Altimeter_Store is
      procedure Set (New_Value : Height) is
      begin
         Value := New_Value;
      end Set;

      function Get return Height is
      begin
         return Value;
      end Get;
   end Altimeter_Store;

   task Radar_Simulator;
   task body Radar_Simulator is
      Next_Cycle   : Ada.Real_Time.Time := Global.Start_Time;
      Measurement  : Height;
   begin
      Measurement := Base_Height;

      while Measurement > 0.0 loop
         declare
            T : constant Duration :=
                  Ada.Real_Time.To_Duration (Next_Cycle - Global.Start_Time);
            Distance : constant Height :=
                         Height
                           (0.5 * Float (Acceleration) * Float (T) * Float (T));
            Speed : constant Velocity := Velocity (Acceleration * T);
         begin
            Measurement := Base_Height - Height'Min (Base_Height, Distance);

            Altimeter_Store.Set (Measurement);
            Velocity_Store.Set (Base_Velocity + Speed);
         end;

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;

      Altimeter_Store.Set (0.0);
      Velocity_Store.Set (0.0);
   end Radar_Simulator;

   function Current_Height return Height is
   begin
      return Altimeter_Store.Get;
   end Current_Height;

   function Current_Velocity return Velocity is
   begin
      return Velocity_Store.Get;
   end Current_Velocity;

   function Image (H : Height) return String is
   begin
      return Height'Image (H) & " m";
   end Image;

   function Image (V : Velocity) return String is
   begin
      return Velocity'Image (V) & " m/s (" & Velocity'Image (V * 3.6) & " km/h)";
   end Image;

end Altimeter;
