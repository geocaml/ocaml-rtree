open OUnit

let pre_made_envelopes = ref []

module V = struct
  type t = int

  let t = Repr.int

  type envelope = Rtree.Rectangle.t

  let envelope i = List.assoc i !pre_made_envelopes
end

module R = Rtree.Make (Rtree.Rectangle) (V)

let make_random_envelope () =
  let x0 = Random.float 100. -. 50.
  and x1 = Random.float 100. -. 50.
  and y0 = Random.float 100. -. 50.
  and y1 = Random.float 100. -. 50. in
  Rtree.Rectangle.v ~x0:(min x0 x1) ~x1:(max x0 x1) ~y0:(min y0 y1)
    ~y1:(max y0 y1)

let rec make_random_envelopes = function
  | 0 -> []
  | n -> (n, make_random_envelope ()) :: make_random_envelopes (n - 1)

let assert_can_find r (i, e) =
  let found = R.find r e in
  assert (List.exists (( = ) i) found)

let assert_meets_bounds r e elems =
  let found = R.find r e in
  List.iter
    (fun elem ->
      let e' = List.assoc elem elems in
      assert (R.Envelope.intersects e e'))
    found

let test_init _ =
  pre_made_envelopes := make_random_envelopes 100;
  let r =
    List.fold_left
      (fun r (i, _envelope) -> R.insert r i)
      (R.empty 8) !pre_made_envelopes
  in

  List.iter (assert_can_find r) !pre_made_envelopes;
  List.iter
    (fun (_, e) -> assert_meets_bounds r e !pre_made_envelopes)
    !pre_made_envelopes

let test_functor _ =
  let elems = make_random_envelopes 100 in
  let module R =
    Rtree.Make
      (Rtree.Rectangle)
      (struct
        type t = int

        let t = Repr.int

        type envelope = Rtree.Rectangle.t

        let envelope i = List.assoc i elems
      end)
  in
  let r =
    List.fold_left (fun r (i, _envelope) -> R.insert r i) (R.empty 8) elems
  in

  List.iter
    (fun (i, e) ->
      let found = R.find r e in
      assert (List.exists (( = ) i) found))
    elems

type line = { p1 : float * float; p2 : float * float }

let lint_t =
  let open Repr in
  record "line" (fun p1 p2 -> { p1; p2 })
  |+ field "p1" (pair float float) (fun t -> t.p1)
  |+ field "p2" (pair float float) (fun t -> t.p2)
  |> sealr

module R1 =
  Rtree.Make
    (Rtree.Rectangle)
    (struct
      type t = line

      let t = lint_t

      type envelope = Rtree.Rectangle.t

      let envelope { p1 = x1, y1; p2 = x2, y2 } =
        let x0 = Float.min x1 x2 in
        let x1 = Float.max x1 x2 in
        let y0 = Float.min y1 y2 in
        let y1 = Float.max y1 y2 in
        Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1
    end)

let test_lines () =
  let module R = R1 in
  let l1 = { p1 = (1., 2.); p2 = (2., 3.) } in
  let index = R.insert (R.empty 8) l1 in
  let l1' = R.find index (Rtree.Rectangle.v ~x0:0. ~y0:0. ~x1:3. ~y1:3.) in
  assert (List.length l1' = 1);
  assert (l1 = List.hd l1');
  let l2 = { p1 = (3., 4.); p2 = (5., 5.) } in
  let index = R.insert index l2 in
  let l1' = R.find index (Rtree.Rectangle.v ~x0:0. ~y0:0. ~x1:3. ~y1:3.) in
  assert (List.length l1' = 1);
  assert (l1 = List.hd l1');
  let l1' = R.find index (Rtree.Rectangle.v ~x0:0. ~y0:0. ~x1:5. ~y1:5.) in
  assert (List.length l1' = 2);
  let vs = R.values index in
  assert (List.length vs = 2)

let omt_loader () =
  let module R = R1 in
  let lines =
    [
      { p1 = (0., 0.); p2 = (1., 1.) };
      { p1 = (1., 1.); p2 = (2., 2.) };
      { p1 = (2., 2.); p2 = (3., 3.) };
      { p1 = (3., 3.); p2 = (4., 4.) };
    ]
  in
  let idx = R.load ~max_node_load:2 lines in
  print_endline (Repr.to_string R.t idx)

let rectangle () =
  let r1 = Rtree.Rectangle.v ~x0:(-1.) ~y0:(-1.) ~x1:1. ~y1:1. in
  assert (r1 = Rtree.Rectangle.(merge r1 empty));
  assert (r1 = Rtree.Rectangle.(merge_many [ r1 ]));
  let r2 = Rtree.Rectangle.v ~x0:(-2.) ~y0:(-2.) ~x1:0. ~y1:0. in
  let r3 = Rtree.Rectangle.v ~x0:(-2.) ~y0:(-2.) ~x1:1. ~y1:1. in
  let r = Rtree.Rectangle.merge_many [ r1; r2 ] in
  assert (r = r3)

(* Testing iter function *)
let test_iter () =
  let module R =
    Rtree.Make
      (Rtree.Rectangle)
      (struct
        type t = line

        let t = lint_t

        type envelope = Rtree.Rectangle.t

        let envelope { p1 = x1, y1; p2 = x2, y2 } =
          let x0 = Float.min x1 x2 in
          let x1 = Float.max x1 x2 in
          let y0 = Float.min y1 y2 in
          let y1 = Float.max y1 y2 in
          Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1
      end)
  in
  let lines =
    [
      { p1 = (0., 0.); p2 = (1., 1.) };
      { p1 = (1., 1.); p2 = (2., 2.) };
      { p1 = (2., 2.); p2 = (3., 3.) };
      { p1 = (3., 3.); p2 = (4., 4.) };
    ]
  in
  let t = R.load ~max_node_load:2 lines in
  let points = ref [] in
  let collect_points = function
    | R.Leaf lst ->
        List.map snd lst |> List.iter (fun f -> points := f :: !points)
    | _ -> ()
  in
  R.iter t collect_points;
  assert (List.length !points = List.length lines)

let cube () =
  let c1 = Rtree.Cube.v ~x0:0. ~y0:0. ~z0:0. ~x1:2. ~y1:2. ~z1:2. in
  let c2 = Rtree.Cube.v ~x0:1. ~y0:1. ~z0:1. ~x1:3. ~y1:3. ~z1:3. in
  let c3 = Rtree.Cube.v ~x0:0. ~y0:0. ~z0:0. ~x1:3. ~y1:3. ~z1:3. in
  assert (Rtree.Cube.(merge c1 empty) = c1);
  let c = Rtree.Cube.merge_many [ c1; c2 ] in
  assert (Rtree.Cube.intersects c1 c2 = true);
  assert (c = c3)

let test_depth () =
  let module R =
    Rtree.Make
      (Rtree.Rectangle)
      (struct
        type t = line

        let t = lint_t

        type envelope = Rtree.Rectangle.t

        let envelope { p1 = x1, y1; p2 = x2, y2 } =
          let x0 = Float.min x1 x2 in
          let x1 = Float.max x1 x2 in
          let y0 = Float.min y1 y2 in
          let y1 = Float.max y1 y2 in
          Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1
      end)
  in
  let lines =
    [
      { p1 = (0., 0.); p2 = (1., 1.) };
      { p1 = (1., 1.); p2 = (2., 2.) };
      { p1 = (2., 2.); p2 = (3., 3.) };
      { p1 = (3., 3.); p2 = (4., 4.) };
    ]
  in
  let t = R.load ~max_node_load:2 lines in
  let calc_depth = R.depth t in
  assert (calc_depth = 2)

let test_size () =
  let module R = R1 in
  let lines =
    [
      { p1 = (0., 0.); p2 = (1., 1.) };
      { p1 = (1., 1.); p2 = (2., 2.) };
      { p1 = (2., 2.); p2 = (3., 3.) };
      { p1 = (3., 3.); p2 = (4., 4.) };
    ]
  in
  let t = R.load ~max_node_load:2 lines in
  let calc_depth = R.size t in
  print_int calc_depth;
  assert (calc_depth = 3)

let suite =
  "R"
  >::: [
         "init" >:: test_init;
         "functor" >:: test_functor;
         "lines" >:: test_lines;
         "omt" >:: omt_loader;
         "rect" >:: rectangle;
         "iter" >:: test_iter;
         "size" >:: test_size;
         "depth" >:: test_depth;
         "cube" >:: cube;
       ]

let _ = run_test_tt_main suite
