-- @summary
-- Provides planet specific parameters
--
-- @description
-- Child package of Planets, providing various physical parameters of them.
package Planets.Parameters
  with Pure => True
is

   function Gravity (Of_Planet : Planet_Name) return Float;
   --  Return the (average) gravity of the given planet in m/s².
   --  @param Of_Planet The planet which gravity we want to query.
   --  @return The gravity in m/s² of the planet Of_Planet.

end Planets.Parameters;
