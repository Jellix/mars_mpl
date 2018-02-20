--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Ada.Real_Time;
with Global;
with Shared_Types;

--  @summary
--  Fuel level tracker.
--
--  @description
--  Depending on thruster state and the parametrized fuel flow rate, the
--  simulated remaining fuel mass in discretely calculated.
--  It uses a cyclic task to sample the current thruster state and estimates the
--  fuel consumption over time. Once the fuel tank is empty, the thrusters are
--  signalled to keep disabled, regardless of thruster commands.
package Engine is

   Cycle : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);
   --  Task cycle time.

   function Current_Fuel_Mass return Shared_Types.Fuel_Mass
     with Volatile_Function;
   --  The remaining fuel.
   --  @return The remaining fuel mass.

   procedure Shutdown;
   --  Terminates the fuel tracking task.

private

   package Log is new Global.Log (Unit_Name => "ENG");

end Engine;
