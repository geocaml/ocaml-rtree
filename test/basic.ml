open OUnit

let make_random_envelope () =
  let x0 = Random.float 100. -. 50.
  and x1 = Random.float 100. -. 50.
  and y0 = Random.float 100. -. 50.
  and y1 = Random.float 100. -. 50. in
  min x0 x1, max x0 x1, min y0 y1, max y0 y1

let rec make_random_envelopes = function
  | 0 -> []
  | n -> (n, make_random_envelope ()) :: make_random_envelopes (n - 1)

let assert_can_find r (i, e) =
  let found = Rtree.find r e in
  assert (List.exists ((=) i) found)

let assert_meets_bounds r e elems =
  let found = Rtree.find r e in
  List.iter begin fun elem ->
    let e' = List.assoc elem elems in
    assert (Envelope.intersects e e')
  end found

let test_init _ =
  let elems = make_random_envelopes 100 in
  let r =
    List.fold_left
      (fun r (i, envelope) -> Rtree.insert r i envelope) Rtree.empty elems in

  List.iter (assert_can_find r) elems;
  List.iter (fun (_, e) -> assert_meets_bounds r e elems) elems

let test_functor _ =
  let elems = make_random_envelopes 100 in
  let module R = Rtree_f.Make(
    struct
      type t = int
      let to_envelope i = List.assoc i elems
    end) in

  let r =
    List.fold_left
      (fun r (i, _envelope) -> R.insert r i) R.empty elems in

  List.iter begin fun (i, e) ->
    let found = R.find r e in
    assert (List.exists ((=) i) found)
  end elems

let suite = "Rtree" >::: [
  "init"    >:: test_init;
  "functor" >:: test_functor;
]

let _ = run_test_tt_main suite
