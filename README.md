# An R-tree implementation for OCaml

This implements a simple R-tree library according to Guttman's
original paper. Currently node splitting is done through the quadratic
algorithm in that paper.

The interface (and implementation) is purely functional.

A functor interface is also provided for providing implicit bounds for
values.

The standard interface is:

    type 'a t

    val empty : 'a t
    val insert : 'a t -> 'a -> Envelope.t -> 'a t
    val find : 'a t -> Envelope.t -> 'a list
    val size : 'a t -> int

and `Envelope.t` is simply `float * float * float * float`
    
To compile, run tests & install, simply:

    $ make
    $ make install

The only external depency is `oUnit` for running unit tests.
