module type BoundableType =
  sig
    type t
    val to_envelope : t -> Envelope.t
  end

module type R =
  sig
    type elem
    type t
    val empty : t
    val insert : t -> elem -> t
    val find : t -> Envelope.t -> elem list
    val size : t -> int
  end

module Make (B : BoundableType) : R with type elem = B.t
