open OUnit

let pre_made_envelopes = ref []

module V = struct
  type t = int

  let t = Repr.int

  type envelope = Rtree.Rectangle.t

  let envelope i = List.assoc i !pre_made_envelopes

  let mindist _p _e = 0.
  let minmaxdist _p _e = 0.
end

module R = Rtree.Make (Rtree.Rectangle) (V)

module Vp = struct
  type t = floatarray

  let t =
    let to_array arr = Float.Array.to_list arr |> Array.of_list in
    let of_array arr = Array.to_list arr |> Float.Array.of_list in
    Repr.(map (array float) of_array to_array)
  

  type envelope = Rtree.Rectangle.t

  let envelope fa=
    let x0 =  Array.Floatarray.get fa 0 in
    let x1 =  Array.Floatarray.get fa 0 in
    let y0 =  Array.Floatarray.get fa 1 in
    let y1 =  Array.Floatarray.get fa 1 in
    Rtree.Rectangle.v ~x0 ~y0 ~x1 ~y1

  let get arr i = Array.Floatarray.get arr i 
  
  let mindist (p:floatarray) (e:envelope) = 
    let (x0,y0,x1,y1) = Rtree.Rectangle.coords e in 
    let gete ind = if ind==0 then x0
                else if ind==1 then y0 else if ind==2 then x1 else y1 in
    let dist i = if (get p i)< (gete i) then ( gete i ) -. (get p i)*. (gete i) -. (get p i)
              else if (get p i)>(gete (i+2)) then (get p i) -. (gete (i+2)) *. (get p i)-. (gete (i+2))
              else 0. in
      dist 0 +. dist 1
  
  let minmaxdist p e = 
    let (x0,y0,x1,y1) = Rtree.Rectangle.coords e in 
    let gete ind = if ind==0 then x0
                else if ind==1 then y0 else if ind==2 then x1 else y1 in
    let rm k = if (get p k) <= ((gete k +. gete (k+2))/.2.) then (get p k)
              else gete (k+2) in

    let rM k = if (get p k) >= ((gete k +. gete (k+2))/.2.)  then (get p k)
              else gete (k+2) in
    
    let farthest_distance_axis i= ( get p i -. rm i) *. (get p (i) -. rm i) in 
    let farthest_distance = farthest_distance_axis 0 +. farthest_distance_axis 1 in
    let max_dist_axis i = farthest_distance -. (get p i -. rM i)*.(get p i  -. rM i) +. (get p i -. rM i)*.(get p i -. rM i) in
    min (max_dist_axis 0) (max_dist_axis 1) 

end

module Rp = Rtree.Make (Rtree.Rectangle) (Vp)

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
        let mindist _p _e = 0.
        let minmaxdist _p _e = 0.
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

let test_lines () =
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
        let mindist _p _e = 0.
        let minmaxdist _p _e = 0.
      end)
  in
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
        let mindist _p _e = 0.
        let minmaxdist _p _e = 0.
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

let test_min_dist () = 
  
  let arr = Array.Floatarray.create 2 in
            Array.Floatarray.unsafe_set arr 0 5.;
            Array.Floatarray.unsafe_set arr 1 5.;
  let point = Array.Floatarray.create 2 in
            Array.Floatarray.unsafe_set arr 0 0.;
            Array.Floatarray.unsafe_set arr 1 0.;
  let env = Vp.envelope arr in
  let dist = Vp.mindist point env in
  assert(dist = 25.)


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
        let mindist _p _e = 0.
        let minmaxdist _p _e = 0.
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

let suite =
  "R"
  >::: [
         "init" >:: test_init;
         "functor" >:: test_functor;
         "lines" >:: test_lines;
         "omt" >:: omt_loader;
         "rect" >:: rectangle;
         "depth" >:: test_depth;
         "min_dist" >:: test_min_dist
       ]

let _ = run_test_tt_main suite
