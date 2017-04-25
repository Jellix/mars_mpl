with Global;

package body Altimeter is

   use type Ada.Real_Time.Time;

   generic
      type Stored_Type is private;
      Initial_Value : Stored_Type;
   package Store is
      procedure Set (New_Value : Stored_Type);
      function Get return Stored_Type;
   end Store;

   package body Store is

      protected Protected_Store is
         procedure Set_Value (New_Value : Stored_Type);
         function Get_Value return Stored_Type;
      private
         Value : Stored_Type := Initial_Value;
      end Protected_Store;

      protected body Protected_Store is
         function Get_Value return Stored_Type is
         begin
            return Value;
         end Get_Value;

         procedure Set_Value (New_Value : Stored_Type) is
         begin
            Value := New_Value;
         end Set_Value;
      end Protected_Store;

      function Get return Stored_Type is
      begin
         return Protected_Store.Get_Value;
      end Get;

      procedure Set (New_Value : Stored_Type) is
      begin
         Protected_Store.Set_Value (New_Value => New_Value);
      end Set;
   end Store;

   package Altimeter_Store is new Store (Stored_Type   => Height,
                                         Initial_Value => Height'Last);
   package Velocity_Store  is new Store (Stored_Type   => Velocity,
                                         Initial_Value => Velocity'First);

   task Radar_Simulator;

   task body Radar_Simulator is
      Next_Cycle   : Ada.Real_Time.Time := Global.Start_Time;
      Measurement  : Height;
   begin
      Measurement := Base_Height;

      while Measurement > 0.0 loop
         declare
            T : constant Duration :=
                  Ada.Real_Time.To_Duration
                    (TS => Next_Cycle - Global.Start_Time);
            Distance : constant Height :=
                         Height
                           (0.5 * Float (Acceleration) * Float (T) * Float (T));
            Speed : constant Velocity := Velocity (Float (Acceleration) * Float (T));
         begin
            Measurement := Base_Height - Height'Min (Base_Height, Distance);

            Altimeter_Store.Set (New_Value => Measurement);
            Velocity_Store.Set (New_Value => Base_Velocity + Speed);
         end;

         delay until Next_Cycle;
         Next_Cycle := Next_Cycle + Cycle;
      end loop;

      Altimeter_Store.Set (New_Value => 0.0);
      Velocity_Store.Set (New_Value => 0.0);
   end Radar_Simulator;

   procedure Current_Height (H : out Height) is
   begin
      H := Altimeter_Store.Get;
   end Current_Height;

   procedure Current_Velocity (V : out Velocity) is
   begin
      V := Velocity_Store.Get;
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
