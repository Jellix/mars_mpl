package Shared_Types.IO is

   function Image (Value        : in Acceleration;
                   Include_Unit : in Boolean := True) return String;
   function Image (Value        : in Altitude;
                   Include_Unit : in Boolean := True) return String;
   function Image (Value        : in Fuel_Mass;
                   Include_Unit : in Boolean := True) return String;
   function Image (Value        : in Velocity;
                   Include_Unit : in Boolean := True) return String;

end Shared_Types.IO;
