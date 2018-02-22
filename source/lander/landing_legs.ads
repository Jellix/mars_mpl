--  pragma Profile (Ravenscar);
--  pragma Partition_Elaboration_Policy (Sequential);

with Global;
with Shared_Types;

--  @summary
--  Simulates the behaviour of the landing legs.
--
--  @description
--  The simulation of the behaviour of the landing legs is done by an internal
--  task, which can be triggered by certain events (which are exposed as
--  callable subroutines).
package Landing_Legs is

   IO_Error : exception;
   --  The exception raised when there is a problem with reading a leg's state.

   procedure Deploy;
   --  Deploy the landing legs. This implies that in the near future a spurious
   --  touchdown signal will be generated by each of the legs. The temporal
   --  distance and length of this event will be chosen randomly.

   procedure Touchdown;
   --  Force all legs into touchdown state.

   procedure Read_State (Index : in     Shared_Types.Legs_Index;
                         State :    out Shared_Types.Leg_State);
   -- Read state of a single leg.
   --
   -- @param Index The value of the leg to be read.
   -- @param State The state of the leg is returned here.
   -- @exception IO_Error if there is a problem reading that particular landing
   -- leg's state.

   procedure Read_State (State : out Shared_Types.All_Legs_State);
   --  Read state of all legs at once.
   --
   --  @param State The state of the legs is returned here.
   --  @exception IO_Error if there was a problem reading any of the landing
   --  leg's state.

   procedure Shutdown;
   --  Terminates the landing leg behavioural simulation task.

   protected type Leg_Iterator is
      entry Next (The_Leg : out Shared_Types.Legs_Index);
      --  Provides the next available leg.
      --
      --  Please note that this subroutine blocks if there is no free landing
      --  leg index available anymore.
      --
      --  @param The_Leg The index of the next available leg.
   private
      Current_Leg    : Shared_Types.Legs_Index := Shared_Types.Legs_Index'First;
      Legs_Available : Boolean                 := True;
   end Leg_Iterator;
   --  Provides a task safe way to assign a certain leg to a task without the
   --  need to use discriminants.

private

   package Log is new Global.Log (Unit_Name => "LLC");
   --  Logger package instance for Landing Legs Control.

end Landing_Legs;