with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Shared_Types.IO is

   pragma Warnings (Off, "instance does not use primitive operation ""*""");

   package Acceleration_IO is new Ada.Text_IO.Fixed_IO (Num => Acceleration);
   package Altitude_IO     is new Ada.Text_IO.Fixed_IO (Num => Altitude);
   package Fuel_IO         is new Ada.Text_IO.Fixed_IO (Num => Fuel_Mass);
   package Velocity_IO     is new Ada.Text_IO.Fixed_IO (Num => Velocity);

   pragma Warnings (On, "instance does not use primitive operation ""*""");

   function Image (Value        : in Acceleration;
                   Include_Unit : in Boolean := True) return String is
      Result : String := "XXXXXX.XXX";
   begin
      Acceleration_IO.Put (To   => Result,
                           Item => Value,
                           Aft  => 3,
                           Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left)
        & (if Include_Unit then " m/s²" else "");
   end Image;

   function Image (Value        : in Altitude;
                   Include_Unit : in Boolean := True) return String is
      Result : String := "XXXXXXXX.XXX";
   begin
      Altitude_IO.Put (To   => Result,
                       Item => Value,
                       Aft  => 3,
                       Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left)
        & (if Include_Unit then " m" else "");
   end Image;

   function Image (Value        : in Fuel_Mass;
                   Include_Unit : in Boolean := True) return String is
      Result : String := "XXX.XXX";
   begin
      Fuel_IO.Put (To   => Result,
                   Item => Value,
                   Aft  => 3,
                   Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left)
        & (if Include_Unit then " kg" else "");
   end Image;

   function Image (Value        : in Velocity;
                   Include_Unit : in Boolean := True) return String
   is
      Result : String := "-XXXX.XXX";
   begin
      Velocity_IO.Put (To   => Result,
                       Item => Value,
                       Aft  => 3,
                       Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left)
        & (if Include_Unit then " m/s" else "");
   end Image;

end Shared_Types.IO;
