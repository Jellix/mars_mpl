--  @summary
--  Provide I/O functionality for the shared types.
--
--  @description
--  As Shared_Types needs to be a pure package, any instances of I/O are
--  rejected there, so we provide a child package with such facilities.
package Shared_Types.IO is

   generic
      type T is delta <>;
      --  The fixed point type we want to imag(ine).
      Unit : in String;
      --  The physical unit of the given type.
   function Generic_Image (Value : in T;
                           Include_Unit : in Boolean := True) return String;
   --  Returns a human readable string for the given value, with the choice of
   --  including the physical unit (string).
   --  The returned string is restricted to three digits after the decimal
   --  point.  The Unit given in the instantiation is appended if the parameter
   --  Include_Unit is True, otherwise only the stringified value will be
   --  returned.
   --
   --  @param Value The value to be converted into a human readable string.
   --  @param Include_Unit Choice if the physical unit associated with the
   --                      instance shall be appended to the human readable
   --                      value.

end Shared_Types.IO;
