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
    val insert : t -> elem -> t
    val find : t -> (float * float * float * float) -> elem list
  end

(* Functor interface. *)
module Make(B: BoundableType) =
  struct
    type elem = B.t
    type t = elem Rtree.t
    let empty = Rtree.empty

    let insert t elem = Rtree.insert t elem (B.to_envelope elem)
    let find = Rtree.find
  end
