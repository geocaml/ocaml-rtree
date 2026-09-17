# ocaml-rtree

[Online Documentation](https://geocaml.github.io/ocaml-rtree/)

_This repository used to live at [mariusae/ocaml-rtree](https://github.com/mariusae/ocaml-rtree)
where most of the core implementation was done_.

This implements a simple rtree library according to [Guttman's original paper](http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf).
Currently node splitting is done through the quadratic algorithm in that paper.

[Some benchmarks are available too](./bench/README.md).

## Usage

There are two key elements to an rtree. The type of envelopes used and the type of the values being store in the tree.
These values must come with a function to calculate an envelope.

The core library comes with an implementation of envelopes as two-dimensional rectangles.

```ocaml
# #show_module Rtree.Rectangle;;
module Rectangle :
  sig
    type t
    val dimensions : int
    val compare_dim : int -> t -> t -> int
    val empty : t
    val intersects : t -> t -> bool
    val merge : t -> t -> t
    val merge_many : t list -> t
    val area : t -> float
    val contains : t -> t -> bool
    val pp : Format.formatter -> t -> unit
    val coords : t -> float * float * float * float
    val v : x0:float -> y0:float -> x1:float -> y1:float -> t
  end
```

If you wanted to store lines in your rtree, one possible implementation might be the following.

```ocaml
module Line = struct
  type t = { p0 : float * float; p1 : float * float }

  (* You could write a more performant [equal] function. *)
  let equal = Stdlib.( = )

  let pp ppf t =
    Format.fprintf ppf "{ p1: (%.2f, %.2f), p2: (%.2f, %.2f) }" (fst t.p0)
      (snd t.p0) (fst t.p1) (snd t.p1)

  type envelope = Rtree.Rectangle.t

  let envelope { p0 = (x1, y1); p1 = (x2, y2) } =
    let x0 = Float.min x1 x2 in
    let x1 = Float.max x1 x2 in
    let y0 = Float.min y1 y2 in
    let y1 = Float.max y1 y2 in
    Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1
end

module R = Rtree.Make(Rtree.Rectangle)(Line)
```

And now we will install the pretty printer.

```ocaml
# #install_printer R.pp;;
```

### Insertion

To insert into an rtree, you simply pass a value into a pre-existing rtree. You can create an empty
rtree where you control the maximum node load size. This is essentially the branching factor in the
tree. The correct value is hard to guess.

```ocaml
# let index = R.empty 8;;
val index : R.t = []
# let index = R.insert index Line.{ p0 = (1., 2.); p1 = (3., 3.) };;
val index : R.t =
  [(((1., 3.), (2., 3.)), { p1: (1.00, 2.00), p2: (3.00, 3.00) })]
# let index = R.insert index Line.{ p0 = (4., 4.); p1 = (5., 5.) };;
val index : R.t =
  [(((4., 5.), (4., 5.)), { p1: (4.00, 4.00), p2: (5.00, 5.00) })
   (((1., 3.), (2., 3.)), { p1: (1.00, 2.00), p2: (3.00, 3.00) })]
```

#### Loading

If you have a list of values to put into an rtree, then you are better off using the `load` function instead
of folding and inserting. This uses the [OMT algorithm](https://ceur-ws.org/Vol-74/files/FORUM_18.pdf) and should give you a more optimised rtree layout.

```ocaml
# let lines =
    Line.[
      { p0 = (0., 0.); p1 = (1., 1.) };
      { p0 = (1., 1.); p1 = (2., 2.) };
      { p0 = (2., 2.); p1 = (3., 3.) };
      { p0 = (3., 3.); p1 = (4., 4.) };
    ]
  in
  R.load ~max_node_load:2 lines
- : R.t =
[|(((0., 2.), (0., 2.)),
   [(((0., 1.), (0., 1.)), { p1: (0.00, 0.00), p2: (1.00, 1.00) })
    (((1., 2.), (1., 2.)), { p1: (1.00, 1.00), p2: (2.00, 2.00) })])
 (((2., 4.), (2., 4.)),
  [(((2., 3.), (2., 3.)), { p1: (2.00, 2.00), p2: (3.00, 3.00) })
   (((3., 4.), (3., 4.)), { p1: (3.00, 3.00), p2: (4.00, 4.00) })])|]
```

Also see [image.ml](./test/image.ml) for rendering an rtree with [vg](https://erratique.ch/software/vg).

<img alt="An rtree rendered with bounding box levels in different colours and the elements are lines" src="./test/rtree.svg" />
<img alt="An rtree rendered with bounding box levels in different colours and the elements are points, this has most points skewed towards the bottom to show how this impacts the shape of the rtree" src="./test/rtree-points.svg" />

### Find

Finding values requires you to pass in a search envelope. A list of result, perhaps empty, will be returned.

```ocaml
# R.find index (Rtree.Rectangle.v ~x0:0. ~y0:0. ~x1:3. ~y1:3.);;
- : Line.t list = [{Line.p0 = (1., 2.); p1 = (3., 3.)}]
# R.find index (Rtree.Rectangle.v ~x0:0. ~y0:0. ~x1:5. ~y1:5.);;
- : Line.t list =
[{Line.p0 = (4., 4.); p1 = (5., 5.)}; {Line.p0 = (1., 2.); p1 = (3., 3.)}]
```

