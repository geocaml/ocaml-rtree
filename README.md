# ocaml-rtree

_This repository used to live at [mariusae/ocaml-rtree](https://github.com/mariusae/ocaml-rtree)
where most of the core implementation was done_.

This implements a simple rtree library according to [Guttman's original paper](http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf).
Currently node splitting is done through the quadratic algorithm in that paper.

## Usage

There are two key elements to an rtree. The type of envelopes used and the type of the values being store in the tree.
These values must come with a function to calculate an envelope.

The core library comes with an implementation of envelopes as two-dimensional rectangles.

```ocaml
# #show_module Rtree.Rectangle;;
module Rectangle :
  sig
    type t = float * float * float * float
    val empty : t
    val intersects : t -> t -> bool
    val merge : t -> t -> t
    val merge_many : t list -> t
    val area : t -> float
    val contains : t -> t -> bool
    val make : x0:float -> y0:float -> x1:float -> y1:float -> t
  end
```

If you wanted to store lines in your rtree, one possible implementation might be the following.

```ocaml
module Line = struct
  type t = { p0 : float * float; p1 : float * float }
  type envelope = Rtree.Rectangle.t

  let envelope { p0 = (x1, y1); p1 = (x2, y2) } =
    let x0 = Float.min x1 x2 in
    let x1 = Float.max x1 x2 in
    let y0 = Float.min y1 y2 in
    let y1 = Float.max y1 y2 in
    Rtree.Rectangle.make ~x0 ~y0 ~x1 ~y1
end

module R = Rtree.Make(Rtree.Rectangle)(Line)
```

### Insert

To insert into an rtree, you simply pass a value into a pre-existing rtree. You can create an empty
rtree where you control the maximum node load size. This is essentially the branching factor in the
tree. The correct value is hard to guess.

```ocaml
# let index = R.empty 8;;
val index : R.t = <abstr>
# let index = R.insert index Line.{ p0 = (1., 2.); p1 = (3., 3.) };;
val index : R.t = <abstr>
# let index = R.insert index Line.{ p0 = (4., 4.); p1 = (5., 5.) };;
val index : R.t = <abstr>
```

### Find

Finding values requires you to pass in a search envelope. A list of result, perhaps empty, will be returned.

```ocaml
# R.find index (Rtree.Rectangle.make ~x0:0. ~y0:0. ~x1:3. ~y1:3.);;
- : Line.t list = [{Line.p0 = (1., 2.); p1 = (3., 3.)}]
# R.find index (Rtree.Rectangle.make ~x0:0. ~y0:0. ~x1:5. ~y1:5.);;
- : Line.t list =
[{Line.p0 = (4., 4.); p1 = (5., 5.)}; {Line.p0 = (1., 2.); p1 = (3., 3.)}]
```

