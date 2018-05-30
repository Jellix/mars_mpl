-- @summary
-- Provides basic definitions of our solar system.
--
-- @description
-- Provides the planets as an enumeration type.
package Planets
  with Pure => True
is

   type Planet_Name is
     (Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune);
   --  The list of planets in our solar system.
   --  @value Mercury Mercury (hot, small, close to the sun)
   --  @value Venus   Venus (hot, earth-like, but with the run away green house
   --                 effect, Earth still has to experience).
   --  @value Earth   Earth (the planet this simulation is most likely running
   --                 on)
   --  @value Mars    Mars (cold, irony, well researched)
   --  @value Jupiter Jupiter (big, hot, gaseous)
   --  @value Saturn  Saturn (has rings!)
   --  @value Uranus  Uranus (another gas planet)
   --  @value Neptune Neptune (rather large gaseous body)

   Big_G : constant := 6.67408E-11;
   -- gravitational constant

end Planets;
