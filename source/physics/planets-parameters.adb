package body Planets.Parameters is

   function Gravity (Of_Planet : Planet_Name) return Float is
   begin
      case Of_Planet is
         when Mercury => return  3.700;
         when Venus   => return  8.870;
         when Earth   => return  9.807;
         when Mars    => return  3.711;
         when Jupiter => return 24.790;
         when Saturn  => return 10.440;
         when Uranus  => return  8.690;
         when Neptune => return 11.150;
         when Pluto   => return  0.620;
      end case;
   end Gravity;

end Planets.Parameters;
