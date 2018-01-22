with GNATCOLL.Traces;

with Global;

package body Thrusters is

   Logger : constant GNATCOLL.Traces.Trace_Handle
     := GNATCOLL.Traces.Create (Unit_Name => "THR",
                                Default   => GNATCOLL.Traces.On,
                                Stream    => Global.Standard_Error);

   protected Thruster is
      function Get return State;
      procedure Set (New_State : in     State;
                     Old_State :    out State);
      procedure No_More_Fuel;
   private
      Current_Thruster_State : State   := Disabled;
      Fuel_Tank_Empty        : Boolean := False;
   end Thruster;

   protected body Thruster is
      function Get return State is
      begin
         return Current_Thruster_State;
      end Get;

      procedure No_More_Fuel is
      begin
         Fuel_Tank_Empty := True;
         Current_Thruster_State := Disabled;
      end No_More_Fuel;

      procedure Set (New_State : in     State;
                     Old_State :    out State) is
      begin
         Old_State := Current_Thruster_State;

         if not Fuel_Tank_Empty or else New_State = Thrusters.Disabled then
            Current_Thruster_State := New_State;
         end if;
      end Set;
   end Thruster;

   procedure Disable is
      Old_State : State;
   begin
      Thruster.Set (New_State => Disabled,
                    Old_State => Old_State);
      pragma Unreferenced (Old_State);
   end Disable;

   procedure Enable is
      Old_State : State;
   begin
      Thruster.Set (New_State => Enabled,
                    Old_State => Old_State);
      pragma Unreferenced (Old_State);
   end Enable;

   procedure Out_Of_Fuel is
   begin
      Thruster.No_More_Fuel;

      Logger.all.Trace
        (Message => "[" & Global.Clock_Image & "] Thrusters ran out of fuel!");
   end Out_Of_Fuel;

   procedure Shutdown (Source : in Landing_Legs.Legs_Index) is
   begin
      Thruster.No_More_Fuel;
      Logger.all.Trace
        (Message =>
           "[" & Global.Clock_Image
         & "] Thrusters have been disabled due to signal from leg "
         & Landing_Legs.Legs_Index'Image (Source) & ".");
   end Shutdown;

   function Current_State return State is (Thruster.Get);

end Thrusters;
