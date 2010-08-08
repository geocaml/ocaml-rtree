open OUnit

module R = Rtree.Make(
  struct
    type t = int * (float * float * float * float)
    let to_envelope (_, e) = e
  end
)

let make_random_boundable i =
  let x0 = Random.float 100. -. 50.
  and x1 = Random.float 100. -. 50.
  and y0 = Random.float 100. -. 50.
  and y1 = Random.float 100. -. 50. in
  x0, x1, y0 y1

let test_init _ =
  let _ = R.empty in
  assert_equal true true

let suite = "Rtree" >::: [
  "init" >:: test_init;
]

let _ = run_test_tt_main suite
