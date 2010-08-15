type 'a t

val empty : 'a t
val insert : 'a t -> 'a -> Envelope.t -> 'a t
val find : 'a t -> Envelope.t -> 'a list
val size : 'a t -> int

(* module type BoundableType = *)
(*   sig *)
(*     type t *)
(*     val to_envelope : t -> float * float * float * float *)
(*   end *)

(* module type R = *)
(*   sig *)
(*     type elem *)
(*     type t *)
(*     val empty : t *)
(*     val add : t -> elem -> t *)
(*     val find : t -> (float * float * float * float) -> elem list *)
(*   end *)


(* module Make (B : BoundableType) : R with type elem = B.t *)
