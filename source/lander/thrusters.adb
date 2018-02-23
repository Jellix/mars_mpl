package body Thrusters is

   type State is (Disabled, Enabled);
   --  State of thruster.
   --  @value Disabled Thruster disabled, no upwards acceleration.
   --  @value Enabled  Thruster enabled, upwards acceleration accordingly.

   protected Thruster_State is
      function Get return State;
      procedure Set (Value : in State);
      procedure No_More_Fuel;
   private
      Current_Thruster_State : State   := Disabled;
      Fuel_Tank_Empty        : Boolean := False;
   end Thruster_State;

   protected body Thruster_State is
      function Get return State is
        (Current_Thruster_State);

      procedure No_More_Fuel is
      begin
         Fuel_Tank_Empty := True;
         Current_Thruster_State := Disabled;
      end No_More_Fuel;

      procedure Set (Value : in State) is
      begin
         if not Fuel_Tank_Empty or else Value = Disabled then
            Current_Thruster_State := Value;
         end if;
      end Set;
   end Thruster_State;

   procedure Disable is
   begin
      Thruster_State.Set (Value => Disabled);
   end Disable;

   procedure Enable is
   begin
      Thruster_State.Set (Value => Enabled);
   end Enable;

   function Is_Disabled return Boolean is
     (Thruster_State.Get = Disabled);

   function Is_Enabled return Boolean is
     (Thruster_State.Get = Enabled);

   procedure Out_Of_Fuel is
   begin
      Thruster_State.No_More_Fuel;
      Log.Trace (Message => "Engine ran out of fuel, thrusters disengaged!");
   end Out_Of_Fuel;

   procedure Shutdown (Source : in Shared_Types.Legs_Index) is
   begin
      Thruster_State.No_More_Fuel;
      Log.Trace (Message =>
                   "Thrusters have been disabled due to signal from leg "
                 & Shared_Types.Legs_Index'Image (Source) & ".");
   end Shutdown;

end Thrusters;
