package body Thrusters is

   use type Shared_Types.State;

   protected Thruster is
      function Get return Shared_Types.State;
      procedure Set (New_State : in     Shared_Types.State;
                     Old_State :    out Shared_Types.State);
      procedure No_More_Fuel;
   private
      Current_Thruster_State : Shared_Types.State := Shared_Types.Disabled;
      Fuel_Tank_Empty        : Boolean            := False;
   end Thruster;

   protected body Thruster is
      function Get return Shared_Types.State is
      begin
         return Current_Thruster_State;
      end Get;

      procedure No_More_Fuel is
      begin
         Fuel_Tank_Empty := True;
         Current_Thruster_State := Shared_Types.Disabled;
      end No_More_Fuel;

      procedure Set (New_State : in     Shared_Types.State;
                     Old_State :    out Shared_Types.State) is
      begin
         Old_State := Current_Thruster_State;

         if not Fuel_Tank_Empty or else New_State = Shared_Types.Disabled then
            Current_Thruster_State := New_State;
         end if;
      end Set;
   end Thruster;

   procedure Disable is
      Old_State : Shared_Types.State;
   begin
      Thruster.Set (New_State => Shared_Types.Disabled,
                    Old_State => Old_State);
      pragma Unreferenced (Old_State);
   end Disable;

   procedure Enable is
      Old_State : Shared_Types.State;
   begin
      Thruster.Set (New_State => Shared_Types.Enabled,
                    Old_State => Old_State);
      pragma Unreferenced (Old_State);
   end Enable;

   procedure Out_Of_Fuel is
   begin
      Thruster.No_More_Fuel;
      Trace (Message => "Thrusters ran out of fuel!");
   end Out_Of_Fuel;

   procedure Shutdown (Source : in Shared_Types.Legs_Index) is
   begin
      Thruster.No_More_Fuel;
      Trace (Message =>
               "Thrusters have been disabled due to signal from leg "
             & Shared_Types.Legs_Index'Image (Source) & ".");
   end Shutdown;

   function Current_State return Shared_Types.State is
     (Thruster.Get);

end Thrusters;
