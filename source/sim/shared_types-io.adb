with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Shared_Types.IO is

   package Acceleration_IO is new Ada.Text_IO.Fixed_IO (Num => Acceleration);
   package Altitude_IO     is new Ada.Text_IO.Fixed_IO (Num => Altitude);
   package Fuel_IO         is new Ada.Text_IO.Fixed_IO (Num => Fuel_Mass);
   package Velocity_IO     is new Ada.Text_IO.Fixed_IO (Num => Velocity);

   function Image (Value : in Acceleration) return String is
      Result : String := "XXXXXX.XXX";
   begin
      Acceleration_IO.Put (To   => Result,
                           Item => Value,
                           Aft  => 3,
                           Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left) & " m/s²";
   end Image;

   function Image (Value : in Altitude) return String is
      Result : String := "XXXXXXX.XXX";
   begin
      Altitude_IO.Put (To   => Result,
                       Item => Value,
                       Aft  => 3,
                       Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left) & " m";
   end Image;

   function Image (Value : in Fuel_Mass) return String is
      Result : String := "XXX.XXX";
   begin
      Fuel_IO.Put (To   => Result,
                   Item => Value,
                   Aft  => 3,
                   Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left) & " kg";
   end Image;

   function Image (Value : in Velocity) return String is
      Result     : String := "-XXXX.XXX";
      Result_KMH : String := "-XXXX.XXX";
   begin
      Velocity_IO.Put (To   => Result,
                       Item => Value,
                       Aft  => 3,
                       Exp  => 0);
      Velocity_IO.Put (To   => Result_KMH,
                       Item => Value * 3.6,
                       Aft  => 3,
                       Exp  => 0);
      return Ada.Strings.Fixed.Trim (Source => Result,
                                     Side   => Ada.Strings.Left)
        & " m/s ("
        & Ada.Strings.Fixed.Trim (Source => Result_KMH,
                                  Side   => Ada.Strings.Left)
        & " km/h)";
   end Image;

end Shared_Types.IO;
