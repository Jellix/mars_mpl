with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Shared_Types.IO is

   function Generic_Image (Value     : in T;
                           With_Unit : in Boolean := True) return String
   is
      pragma Warnings (Off, "instance does not use primitive operation ""*""");
      package Fixed_IO is new Ada.Text_IO.Fixed_IO (Num => T);
      pragma Warnings (On, "instance does not use primitive operation ""*""");

      Aft    : constant Positive := Positive'Min (T'Aft, 3);
      Result : String (1 .. T'Fore + Aft + 1);
   begin
      Fixed_IO.Put (To   => Result,
                    Item => Value,
                    Aft  => Aft,
                    Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left)
        & (if With_Unit then " " & Unit else "");
   end Generic_Image;

end Shared_Types.IO;
