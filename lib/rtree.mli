
module type BoundableType =
  sig
    type t
    val to_envelope : t -> float * float * float * float
  end

module type R =
  sig
    type elem
    type t
    val empty : t
    val add : t -> elem -> t
    val find : t -> (float * float * float * float) -> elem option
  end


module Make (B : BoundableType) : R with type elem = B.t
