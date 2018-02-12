package Shared_Types.IO is

   generic
      type T is delta <>;
      Unit : in String;
   function Generic_Image (Value : in T;
                           Include_Unit : in Boolean := True) return String;

end Shared_Types.IO;
