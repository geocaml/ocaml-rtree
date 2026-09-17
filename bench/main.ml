open Bechamel

module List = struct
  include Stdlib.List

  let rec take n = function
    | [] -> []
    | _ when n = 0 -> []
    | x :: rest -> x :: take (n - 1) rest
end

let () = Random.init 42

module Point = struct
  type t = { x : float; y : float }

  let equal a b = Float.equal a.x b.x && Float.equal a.y b.y
  let pp ppf t = Format.fprintf ppf "{ x: %.2f, y: %.2f }" t.x t.y

  type envelope = Rtree.Rectangle.t

  let envelope t : Rtree.Rectangle.t =
    Rtree.Rectangle.v ~x0:t.x ~x1:t.x ~y0:t.y ~y1:t.y
end

module Rtree = Rtree.Make (Rtree.Rectangle) (Point)

let random_points ?(min = -180.) ?(max = 180.) num =
  List.init num (fun _ ->
      let x = Random.float (max -. min) +. min in
      let y = Random.float (max -. min) +. min in
      { Point.x; y })

let bench_insert i =
  let points = random_points i in
  let run () =
    List.fold_left (fun r p -> Rtree.insert r p) (Rtree.empty 8) points
  in
  Staged.stage run

let bench_load i =
  let points = random_points i in
  let run () = Rtree.load ~max_node_load:8 points in
  Staged.stage run

let bench_depth i =
  let points = random_points i in
  let index =
    List.fold_left (fun r p -> Rtree.insert r p) (Rtree.empty 3) points
  in
  let run () = Rtree.depth index in
  Staged.stage run

let bench_find i =
  let points = random_points i in
  let p = List.nth points (i / 2) in
  let index =
    List.fold_left (fun r p -> Rtree.insert r p) (Rtree.empty 8) points
  in
  let run () =
    let ps = Rtree.find index (Point.envelope p) in
    assert (List.exists (fun p' -> p = p') ps)
  in
  Staged.stage run

let bench_find_load i =
  let points = random_points i in
  let p = List.nth points (i / 2) in
  let index = Rtree.load ~max_node_load:8 points in
  let run () =
    let ps = Rtree.find index (Point.envelope p) in
    assert (List.exists (fun p' -> p = p') ps)
  in
  Staged.stage run

let bench_remove i =
  let points = random_points i in
  let p = List.nth points (i / 2) in
  let index = Rtree.load ~max_node_load:8 points in
  let run () =
    let ps, _tree = Rtree.remove index p in
    assert (List.exists (fun p' -> p = p') ps)
  in
  Staged.stage run

let bench_remove_env i =
  let points = random_points i in
  let p = List.take (i / 2) points in
  let e = Rtree.Envelope.merge_many @@ List.map Point.envelope p in
  let index = Rtree.load ~max_node_load:8 points in
  let run () =
    let ps, tree = Rtree.remove_env index e in
    Sys.opaque_identity (ps, tree)
  in
  Staged.stage run

let suite =
  Test.make_grouped ~name:"rtree"
    [
      Test.make_indexed ~name:"insert" ~fmt:"%s %7d"
        ~args:[ 1_000; 3_000; 10_000; 50_000; 100_000 ]
        bench_insert;
      Test.make_indexed ~name:"load" ~fmt:"%s %7d"
        ~args:[ 1_000; 3_000; 10_000; 50_000; 100_000 ]
        bench_load;
      Test.make_indexed ~name:"find" ~fmt:"%s %7d"
        ~args:[ 1_000; 3_000; 10_000; 50_000; 100_000 ]
        bench_find;
      Test.make_indexed ~name:"find_load" ~fmt:"%s %7d"
        ~args:[ 1_000; 3_000; 10_000; 50_000; 100_000 ]
        bench_find_load;
      Test.make_indexed ~name:"depth" ~fmt:"%s %7d"
        ~args:[ 1_000; 3_000; 10_000; 50_000; 100_000 ]
        bench_depth;
      Test.make_indexed ~name:"remove" ~fmt:"%s %7d"
        ~args:[ 1_000; 3_000; 10_000; 50_000; 100_000 ]
        bench_remove;
      Test.make_indexed ~name:"remove_env" ~fmt:"%s %7d"
        ~args:[ 1_000; 3_000; 10_000; 50_000; 100_000 ]
        bench_remove_env;
    ]

let metrics =
  Toolkit.Instance.[ minor_allocated; major_allocated; monotonic_clock ]

let benchmark () =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[| run |]
  in
  let quota = Time.second 0.5 in
  let cfg = Benchmark.cfg ~limit:2000 ~quota ~kde:(Some 1000) () in
  let raw_results = Benchmark.all cfg metrics suite in
  List.map (fun i -> Analyze.all ols i raw_results) metrics
  |> Analyze.merge ols metrics

let () =
  let results = benchmark () in
  Fmt.pr "@[<v>%a@]@." Bechamel_csv.pp results

(* Borrowed from Uring

/*
 * Copyright (C) 2020-2021 Anil Madhavapeddy
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */*)
