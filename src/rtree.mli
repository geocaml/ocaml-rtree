type 'a t

val empty : 'a t
val insert : 'a t -> 'a -> Envelope.t -> 'a t
val find : 'a t -> Envelope.t -> 'a list
val size : 'a t -> int
