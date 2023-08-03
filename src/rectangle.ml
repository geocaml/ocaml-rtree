type t = float * float * float * float
(* lower left x, upper right x, lower left y, upper right y*)

let dimensions = 2

let compare_dim i (x0, _, y0, _) (x0', _, y0', _) =
  match i with
  | 0 -> Float.compare x0 x0'
  | 1 -> Float.compare y0 y0'
  | n -> invalid_arg ("Only two dimensions and you accessed " ^ string_of_int n)

let t = Repr.(quad float float float float)

let make ~x0 ~y0 ~x1 ~y1 =
  if x0 > x1 then invalid_arg "x0 should be less than or equal to x1";
  if y0 > y1 then invalid_arg "y0 should be less than or equal to y1";
  (x0, x1, y0, y1)

let ranges_intersect a b a' b' = a' <= b && a <= b'

let intersects (x0, x1, y0, y1) (x0', x1', y0', y1') =
  (* For two envelopes to intersect, both of their ranges do. *)
  ranges_intersect x0 x1 x0' x1' && ranges_intersect y0 y1 y0' y1'

let merge (x0, x1, y0, y1) (x0', x1', y0', y1') =
  (min x0 x0', max x1 x1', min y0 y0', max y1 y1')

let rec merge_many = function
  | e :: [] -> e
  | e :: es -> merge e (merge_many es)
  | [] -> raise (Invalid_argument "can't zero envelopes")

let area (x0, x1, y0, y1) = Float.abs (x1 -. x0) *. Float.abs (y1 -. y0)

let contains (x0, x1, y0, y1) (x0', x1', y0', y1') =
  x0 <= x0' && x1 >= x1' && y0 <= y0' && y1 >= y1'

let empty = (0., 0., 0., 0.)
