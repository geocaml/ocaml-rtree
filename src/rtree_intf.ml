module type Value = sig
  (** Values are stored in the Rtree and must provide a means
      of calculating an envelope (minimal bounding box).

      {2 An example}
      A two-dimensional lines that uses {! Rectangle} as a bounding
      box could be implemented as
      {[
        module Line = struct
          type t = { p0 : float * float; p1 : float * float }

          let t =
            let open Repr in
            record "line" (fun p0 p1 -> { p0; p1 })
            |+ field "p0" (pair float float) (fun t -> t.p0)
            |+ field "p1" (pair float float) (fun t -> t.p1)
            |> sealr

          type envelope = Rtree.Rectangle.t

          let envelope { p0 = (x1, y1); p1 = (x2, y2) } =
            let x0 = Float.min x1 x2 in
            let x1 = Float.max x1 x2 in
            let y0 = Float.min y1 y2 in
            let y1 = Float.max y1 y2 in
            Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1
        end
      ]}

      Note that a runtime representation must also be provided.
      *)

  (** {2 Interface} *)

  type t
  (** A type for things stored in the Rtree. *)

  val t : t Repr.t
  (** A runtime representation of values. *)

  type envelope
  (** A type for envelopes in the Rtree. *)

  val envelope : t -> envelope
  (** Given a value, calculates the envelope for it. *)
end

module type Envelope = sig
  type t
  (** The type for envelopes *)

  val dimensions : int
  (** The number of dimensions of this kind of envelope *)

  val compare_dim : int -> t -> t -> int
  (** [compare_dim i a b] compares [a] and [b] along the [i]th dimension.

      Raises [Invalid_arg] if [i >= dimensions]. *)

  val t : t Repr.t
  (** A runtime representation of envelopes. *)

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

  val t : t Repr.t
  (** A runtime representation of the rtree. *)

  module Envelope : Envelope
  (** Envelopes for the Rtree. *)

  module Value : Value with type envelope = Envelope.t
  (** Values stored in the Rtree. *)

  type tree =
    | Node of (Envelope.t * tree) list
    | Leaf of (Envelope.t * Value.t) list
    | Empty  (** The representation of a tree *)

  val tree : t -> tree
  (** Get a representation of the tree. There is no guarantee that this is the
      same representation used internally. *)

  val empty : int -> t
  (** The empty tree configured with a maximum load size. This
      is the number of children allowed at a level. *)

  val insert : t -> Value.t -> t
  (** Insert a new element into the tree *)

  val find : t -> Envelope.t -> Value.t list
  (** [find tree env] find all value contained by [env] in [tree]. *)

  val size : t -> int
  (** [size tree] returns the number of element in the tree. *)

  val values : t -> Value.t list
  (** Returns all the values currently in the index. *)

  val load : ?max_node_load:int -> Value.t list -> t
  (** [load vs] will "bulk" load values into an r-tree. This will produce
      a better tree and is preferred over folding with {! insert}.

      It uses the {{: https://ceur-ws.org/Vol-74/files/FORUM_18.pdf} OMT algorithm}. *)

  val depth : t -> int
  (** [depth tree] returns the depth of the tree. *)
end

module type Maker = functor
  (Envelope : Envelope)
  (Value : Value with type envelope = Envelope.t)
  -> S with module Envelope = Envelope and module Value = Value

module type Intf = sig
  module type Value = Value

  module type Envelope = Envelope
  (** Envelopes are bounding boxes.*)

  module type S = S
  module type Maker = Maker

  module Make : Maker

  module Rectangle : sig
    include Envelope

    val coords : t -> float * float * float * float
    (** Gives you [x0, x1, y0, y1]. *)

    val v : x0:float -> y0:float -> x1:float -> y1:float -> t
  end

  module Cube : sig
    include Envelope

    val coords : t -> float * float * float * float * float * float
    (** Gives you [x0, x1, y0, y1, z0, z1]. *)

    val v :
      x0:float -> y0:float -> z0:float -> x1:float -> y1:float -> z1:float -> t
  end
end
