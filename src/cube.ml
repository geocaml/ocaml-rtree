open! Import

type t = floatarray
(* min x, max x, min y, max y, min z, max z *)

let dimensions = 3
let x0 arr = Array.Floatarray.unsafe_get arr 0
let x1 arr = Array.Floatarray.unsafe_get arr 1
let y0 arr = Array.Floatarray.unsafe_get arr 2
let y1 arr = Array.Floatarray.unsafe_get arr 3
let z0 arr = Array.Floatarray.unsafe_get arr 4
let z1 arr = Array.Floatarray.unsafe_get arr 5

let compare_dim i arr arr' =
  match i with
  | 0 -> Float.compare (x0 arr) (x0 arr')
  | 1 -> Float.compare (y0 arr) (y0 arr')
  | 2 -> Float.compare (z0 arr) (z0 arr')
  | n -> invalid_arg ("Only 3 dimensions and you accessed " ^ string_of_int n)

let t =
  let to_array arr = Float.Array.to_list arr |> Array.of_list in
  let of_array arr = Array.to_list arr |> Float.Array.of_list in
  Repr.(map (array float) of_array to_array)

let coords arr =
  ( Array.Floatarray.unsafe_get arr 0,
    Array.Floatarray.unsafe_get arr 1,
    Array.Floatarray.unsafe_get arr 2,
    Array.Floatarray.unsafe_get arr 3,
    Array.Floatarray.unsafe_get arr 4,
    Array.Floatarray.unsafe_get arr 5 )

let v ~x0 ~y0 ~z0 ~x1 ~y1 ~z1 =
  if x0 > x1 then invalid_arg "x0 should be less than or equal to x1";
  if y0 > y1 then invalid_arg "y0 should be less than or equal to y1";
  if z0 > z1 then invalid_arg "z0 should be less than or equal to z1";
  let arr = Array.Floatarray.create 6 in
  Array.Floatarray.unsafe_set arr 0 x0;
  Array.Floatarray.unsafe_set arr 1 x1;
  Array.Floatarray.unsafe_set arr 2 y0;
  Array.Floatarray.unsafe_set arr 3 y1;
  Array.Floatarray.unsafe_set arr 4 z0;
  Array.Floatarray.unsafe_set arr 5 z1;
  arr

let ranges_intersect a b a' b' =
  Float.compare a' b <= 0 && Float.compare a b' <= 0

let intersects arr arr' =
  let x0, x0' = (x0 arr, x0 arr') in
  let x1, x1' = (x1 arr, x1 arr') in
  let y0, y0' = (y0 arr, y0 arr') in
  let y1, y1' = (y1 arr, y1 arr') in
  let z0, z0' = (z0 arr, z0 arr') in
  let z1, z1' = (z1 arr, z1 arr') in
  (* For two cube envelopes to intersect, both of their ranges do. *)
  ranges_intersect x0 x1 x0' x1'
  && ranges_intersect y0 y1 y0' y1'
  && ranges_intersect z0 z1 z0' z1'

let merge arr arr' =
  let x0, x0' = (x0 arr, x0 arr') in
  let x1, x1' = (x1 arr, x1 arr') in
  let y0, y0' = (y0 arr, y0 arr') in
  let y1, y1' = (y1 arr, y1 arr') in
  let z0, z0' = (z0 arr, z0 arr') in
  let z1, z1' = (z1 arr, z1 arr') in
  let arr = Array.Floatarray.create 6 in
  Array.Floatarray.unsafe_set arr 0 (min x0 x0');
  Array.Floatarray.unsafe_set arr 1 (max x1 x1');
  Array.Floatarray.unsafe_set arr 2 (min y0 y0');
  Array.Floatarray.unsafe_set arr 3 (max y1 y1');
  Array.Floatarray.unsafe_set arr 4 (min z0 z0');
  Array.Floatarray.unsafe_set arr 5 (max z1 z1');
  arr

let merge_many t =
  let rec loop acc = function [] -> acc | e :: es -> loop (merge e acc) es in
  match t with
  | [] -> raise (Invalid_argument "can't zero envelopes")
  | e :: es -> loop e es

let volume arr =
  let x0, x1, y0, y1, z0, z1 =
    (x0 arr, x1 arr, y0 arr, y1 arr, z0 arr, z1 arr)
  in
  Float.abs (x1 -. x0) *. Float.abs (y1 -. y0) *. Float.abs (z1 -. z0)

let contains arr arr' =
  let x0, x0' = (x0 arr, x0 arr') in
  let x1, x1' = (x1 arr, x1 arr') in
  let y0, y0' = (y0 arr, y0 arr') in
  let y1, y1' = (y1 arr, y1 arr') in
  let z0, z0' = (z0 arr, z0 arr') in
  let z1, z1' = (z1 arr, z1 arr') in
  Float.compare x0 x0' <= 0
  && Float.compare x1 x1' >= 0
  && Float.compare y0 y0' <= 0
  && Float.compare y1 y1' >= 0
  && Float.compare z0 z0' <= 0
  && Float.compare z1 z1' >= 0

let empty =
  let arr = Array.Floatarray.create 6 in
  Array.Floatarray.unsafe_set arr 0 0.;
  Array.Floatarray.unsafe_set arr 1 0.;
  Array.Floatarray.unsafe_set arr 2 0.;
  Array.Floatarray.unsafe_set arr 3 0.;
  Array.Floatarray.unsafe_set arr 4 0.;
  Array.Floatarray.unsafe_set arr 5 0.;
  arr
