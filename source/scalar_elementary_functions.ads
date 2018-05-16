with Ada.Numerics.Generic_Elementary_Functions;
with Shared_Types;

package Scalar_Elementary_Functions is new
  Ada.Numerics.Generic_Elementary_Functions (Float_Type => Shared_Types.Scalar);
pragma Pure (Scalar_Elementary_Functions);
