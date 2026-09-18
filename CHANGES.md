# v0.3.0 2026-09-18 Belfast

This release removed the dependency on `repr` (which pulled in at least 6
additional dependencies). This is a breaking change to the interfaces for
constructing rtrees. However, seeing as we are `<1.0.0` it is being released as
a minor bump.

Existing rtree instantiations can easily fulfil the module interface using their
`repr` runtime type representation like so:

```ocaml
module Line = struct
  type t = { p0 : float * float; p1 : float * float }

  let t =
    let open Repr in
    record "line" (fun p0 p1 -> { p0; p1 })
    |+ field "p0" (pair float float) (fun t -> t.p0)
    |+ field "p1" (pair float float) (fun t -> t.p1)
    |> sealr

  (* ADDED: New lines that add [equal] and [pp] functions defined
     using the existing runtime type representation. *)
  let equal = Repr.equal t |> Repr.unstage
  let pp = Repr.pp t

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

This is a quick fix. Users are advised to remove their dependency on `repr` and
hand-write `equal` and `pp` functions.

## Changes

- Remove dependency on `repr`. Users of this library will now have to provide
  their own `equal` and `pp` functions for the values they are storing in
  their rtree (#45, @patricoferris).

# v0.2.0 2026-05-05 Belfast

- Add a set of `remove` functions (#44, @FayCarsons)

## Bugs, Fixes and Optimisations

- Fix `values` not returning entire tree (#40, @mdales)
- Added bounds function to get overall tree dimensions (#42, @mdales)

# v0.1.1 2023-08-17 Cambridge

## Bugs, Fixes and Optimisations

 - Remove extra length calculations (#16, @lindig)
 - Remove some polymorphic comparisons and replace with `Float` functions (#15, @patricoferris)
 - Fix stack overflows in OMT and Rectangle.merge (#14, @patricoferris, reported by @lindig)

# v0.1.0 2023-08-08 Munich

 - Initial release
