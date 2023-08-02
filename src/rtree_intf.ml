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
            Rtree.Rectangle.make ~x0 ~y0 ~x1 ~y1
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

  val get_dim : t -> int -> float
  (** [get_dim e i] gets the value for the [i]th dimension of [e].

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

  val values : t -> Value.t list
  (** Returns all the values currently in the index. *)

  val load : ?max_node_load:int -> Value.t list -> t
  (** [load vs] will "bulk" load values into an r-tree. This will produce
      a better tree and is preferred over folding with {! insert}.

      It uses the {{: https://ceur-ws.org/Vol-74/files/FORUM_18.pdf} OMT algorithm}. *)
end

module type Maker =
  functor (Envelope : Envelope) (Value : Value with type envelope = Envelope.t) ->
  S with module Envelope = Envelope and module Value = Value

module type Intf = sig
  module type Value = Value

  (** Envelopes are bounding boxes.*)
  module type Envelope = Envelope

  module type S = S
  module type Maker = Maker

  module Make : Maker

  module Rectangle : sig
    include Envelope with type t = float * float * float * float

    val make : x0:float -> y0:float -> x1:float -> y1:float -> t
  end

  (** {2 A quick example}
There are two key elements to an rtree. The type of envelopes used and the type of the values being store in the tree.
These values must come with a function to calculate an envelope.

The core library comes with an implementation of envelopes as two-dimensional {! Rectangle}.

If you wanted to store lines in your rtree, one possible implementation might be the following.

{@ocaml[
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
    Rtree.Rectangle.make ~x0 ~y0 ~x1 ~y1
end

module R = Rtree.Make(Rtree.Rectangle)(Line)
]}

{2 Insertion}

To insert into an rtree, you simply pass a value into a pre-existing rtree. You can create an empty
rtree where you control the maximum node load size. This is essentially the branching factor in the
tree. The correct value is hard to guess.

{@ocaml[
# let index = R.empty 8;;
val index : R.t = <abstr>
# let index = R.insert index Line.{ p0 = (1., 2.); p1 = (3., 3.) };;
val index : R.t = <abstr>
# let index = R.insert index Line.{ p0 = (4., 4.); p1 = (5., 5.) };;
val index : R.t = <abstr>
]}

{2 Find}

Finding values requires you to pass in a search envelope. A list of result, perhaps empty, will be returned.

{@ocaml[
# R.find index (Rtree.Rectangle.make ~x0:0. ~y0:0. ~x1:3. ~y1:3.);;
- : Line.t list = [{Line.p0 = (1., 2.); p1 = (3., 3.)}]
# R.find index (Rtree.Rectangle.make ~x0:0. ~y0:0. ~x1:5. ~y1:5.);;
- : Line.t list =
[{Line.p0 = (4., 4.); p1 = (5., 5.)}; {Line.p0 = (1., 2.); p1 = (3., 3.)}]
]}
*)
end