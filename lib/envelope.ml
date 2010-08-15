type t = float * float * float * float

let ranges_intersect a b a' b' = a' <= b && a <= b'

let intersects (x0, x1, y0, y1) (x0', x1', y0', y1') =
  (* For two envelopes to intersect, both of their ranges do. *)
  ranges_intersect x0 x1 x0' x1' && ranges_intersect y0 y1 y0' y1'

let add (x0, x1, y0, y1) (x0', x1', y0', y1') =
  min x0 x0', max x1 x1', min y0 y0', max y1 y1'

let rec add_many = function
  | e :: [] -> e
  | e :: es -> add e (add_many es)
  | [] -> raise (Invalid_argument "can't zero envelopes")

let area (x0, x1, y0, y1) =
  (x1 -. x0) *. (y1 -. y0)

let within (x0, x1, y0, y1) (x0', x1', y0', y1') =
  x0 <= x0' && x1 >= x1' && y0 <= y0' && y1 >= y1'

let empty = 0., 0., 0., 0.
