module type Value = sig
  type t
  (** A type for things stored in the Rtree. *)

  type envelope
  (** A type for envelopes in the Rtree. *)

  val envelope : t -> envelope
  (** Given a value, calculates the envelope for it. *)
end

module type Envelope = sig
  type t
  (** The type for envelopes *)

  val empty : t
  (** The empty envelope. *)

  val intersects : t -> t -> bool
  (** Whether or not two envelopes intersect. *)

  val merge : t -> t -> t
  (** Computing the envelope that contains both the arguments *)

  val merge_many : t list -> t
  (** Like {! merge} only for many envelopes. *)

  val area : t -> float
  (** Compute the area of an envelope. *)

  val contains : t -> t -> bool
  (** [contains a b] asks whether [b] is contained by [a]. *)
end

module type S = sig
  type t
  (** An Rtree. *)

  (** Envelopes for the Rtree. *)
  module Envelope : Envelope

  (** Values stored in the Rtree. *)
  module Value : Value with type envelope = Envelope.t

  val empty : int -> t
  (** The empty tree configured with a maximum load size. This
      is the number of children allowed at a level. *)

  val insert : t -> Value.t -> t
  (** Insert a new element into the tree *)

  val find : t -> Envelope.t -> Value.t list
  (** [find tree env] find all value contained by [env] in [tree]. *)

  val size : t -> int
  (** [size tree] returns the number of element in the tree. *)
end

module type Maker =
  functor (Envelope : Envelope) (Value : Value with type envelope = Envelope.t) ->
  S with module Envelope = Envelope and module Value = Value

module type Intf = sig
  module type Value = Value
  module type Envelope = Envelope
  module type S = S
  module type Maker = Maker

  module Make : Maker

  module Rectangle : sig
    include Envelope with type t = float * float * float * float

    val make : x0:float -> y0:float -> x1:float -> y1:float -> t
  end
end