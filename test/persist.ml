module Line = struct
  type t = float array
  type envelope = Rtree.Rectangle.t

  let t = Repr.(array float)

  let envelope arr =
    let x0 = Float.min arr.(0) arr.(2) in
    let x1 = Float.max arr.(0) arr.(2) in
    let y0 = Float.min arr.(1) arr.(3) in
    let y1 = Float.max arr.(1) arr.(3) in
    Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1
end

module R = Rtree.Make (Rtree.Rectangle) (Line)

let lines =
  [
    [| 0.; 0.; 0.; 1. |];
    [| 0.; 1.; 0.; 2. |];
    [| 1.; 1.; 2.; 2. |];
    [| 2.; 2.; 3.; 3. |];
    [| 0.; 5.; 4.; 4. |];
  ]

module Ic = struct
  let with_open_bin fname fn =
    let ic = open_in_bin fname in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> fn ic)

  let input_all ic =
    let lines = ref [] in
    let rec loop () =
      lines := input_line ic :: !lines;
      loop ()
    in
    try loop () with End_of_file -> List.rev !lines |> String.concat "\n"
end

(* Code used to initial persist the index
   let initial_persist () =
     let idx = R.empty 4 in
     let idx = List.fold_left R.insert idx lines in
     Out_channel.with_open_bin "r.idx" @@ fun oc ->
     Out_channel.output_string oc (Repr.(unstage @@ to_bin_string R.t) idx)
*)

let () =
  Ic.with_open_bin "./r.idx" @@ fun ic ->
  let idx =
    Ic.input_all ic |> Repr.(unstage @@ of_bin_string R.t) |> Result.get_ok
  in
  let all = R.find idx (Rtree.Rectangle.v ~x0:0. ~y0:0. ~x1:5. ~y1:5.) in
  let retrieved =
    List.for_all
      (fun v -> List.exists (Repr.(unstage @@ equal Line.t) v) lines)
      all
  in
  assert retrieved
