with Global;

package Engines is

   procedure Start_Descent;
   --  Starts the descent engines and velocity control.

   procedure Shutdown;
   --  Shuts down the engine task.

private

   package Log is new Global.Log (Unit_Name => "ENG");

end Engines;
