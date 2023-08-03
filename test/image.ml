open Vg

module Line = struct
  type t = Gg.P2.t * Gg.P2.t
  type envelope = Rtree.Rectangle.t

  let t =
    let open Repr in
    let point =
      Repr.map (pair float float)
        (fun (x, y) -> Gg.P2.v x y)
        (fun p -> (Gg.P2.x p, Gg.P2.y p))
    in
    Repr.pair point point

  let envelope (p1, p2) =
    let x0 = min (Gg.P2.x p1) (Gg.P2.x p2) in
    let x1 = max (Gg.P2.x p1) (Gg.P2.x p2) in
    let y0 = min (Gg.P2.y p1) (Gg.P2.y p2) in
    let y1 = max (Gg.P2.y p1) (Gg.P2.y p2) in
    Rtree.Rectangle.make ~x0 ~x1 ~y0 ~y1
end

module Point = struct
  type t = Gg.P2.t
  type envelope = Rtree.Rectangle.t

  let t =
    let open Repr in
    let point =
      Repr.map (pair float float)
        (fun (x, y) -> Gg.P2.v x y)
        (fun p -> (Gg.P2.x p, Gg.P2.y p))
    in
    point

  let envelope p1 =
    let x0 = Gg.P2.x p1 in
    let y0 = Gg.P2.y p1 in
    Rtree.Rectangle.make ~x0 ~x1:x0 ~y0 ~y1:y0
end

module R = Rtree.Make (Rtree.Rectangle) (Line)
module Rp = Rtree.Make (Rtree.Rectangle) (Point)

let make_random_line x_max y_max =
  let x0 = Random.float x_max in
  let y0 = Random.float y_max in
  let x1 = Random.float x_max in
  let y1 = Random.float y_max in
  let p0 = Gg.P2.v x0 y0 in
  let p1 = Gg.P2.v x1 y1 in
  (p0, p1)

let line ?(color = Gg.Color.black) ?(width = 0.3) start end' =
  let open Vg in
  let line = P.empty |> P.sub start |> P.line end' |> P.close in
  let area = `O { P.o with P.width } in
  I.const color |> I.cut ~area line

let point ?(color = Gg.Color.black) ?(width = 0.3) pt =
  let open Vg in
  let dot =
    let circle = P.empty |> P.circle Gg.P2.o (0.5 *. width) in
    I.const color |> I.cut circle
  in
  dot |> I.move pt

let rect ~color (e : R.Envelope.t) =
  let open Vg in
  let open Gg in
  let x0, x1, y0, y1 = e in
  let p1 = V2.v x0 y0 in
  let p2 = V2.v x1 y0 in
  let p3 = V2.v x1 y1 in
  let p4 = V2.v x0 y1 in
  let line = line ~color ~width:0.2 in
  List.fold_left I.blend I.void
    [ line p1 p2; line p2 p3; line p3 p4; line p1 p4 ]

let levels opacity =
  Gg.
    [
      Color.v 1. 0. 0. opacity;
      Color.v 0. 1. 0. opacity;
      Color.v 0. 0. 1. opacity;
      Color.v 1. 0. 0. opacity;
      Color.v 0. 1. 0. opacity;
      Color.v 0. 0. 1. opacity;
      Color.v 1. 0. 0. opacity;
      Color.v 0. 1. 0. opacity;
      Color.v 0. 0. 1. opacity;
    ]

let rec render_tree ~size ~level acc (r : Vg.renderer) = function
  | R.Empty -> assert false
  | R.Leaf vs ->
      let render_leaf acc (_env, (s, e)) = I.blend acc (line s e) in
      List.fold_left render_leaf acc vs
  | R.Node vs ->
      let render_node acc (env, e) =
        I.blend
          (rect ~color:(List.nth (levels 1.) level) env)
          (render_tree ~size ~level:(level + 1) acc r e)
      in
      List.fold_left render_node acc vs

let ok = function `Ok -> () | _ -> failwith "Unexpected rendering issue"

let () =
  let width, height = (100., 100.) in
  let size = Gg.Size2.v width height in
  Random.init 42;
  let lines = List.init 8 (fun _ -> make_random_line width height) in
  let idx = R.load ~max_node_load:2 lines in
  let tree = R.tree idx in
  let box = Gg.Box2.v Gg.V2.(v 0. 0.) Gg.V2.(v width height) in
  let oc = open_out "./rtree.svg" in
  let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
  let i = render_tree ~size ~level:0 I.void r tree in
  ( Vgr.render r (`Image (size, box, i)) |> fun v ->
    ok v;
    Vgr.render r `End |> ok );
  close_out oc

let make_random_point width height =
  let x = Random.float width in
  let y = Random.float height in
  Gg.V2.v x y

let rec render_tree ~size ~level acc (r : Vg.renderer) = function
  | Rp.Empty -> assert false
  | Rp.Leaf vs ->
      let render_point acc (_env, p) = I.blend acc (point ~width:2. p) in
      List.fold_left render_point acc vs
  | Rp.Node vs ->
      let render_node acc (env, e) =
        I.blend
          (rect ~color:(List.nth (levels 1.) level) env)
          (render_tree ~size ~level:(level + 1) acc r e)
      in
      List.fold_left render_node acc vs

let () =
  let width, height = (100., 100.) in
  let size = Gg.Size2.v width height in
  Random.init 42;
  let points =
    List.init 108 (fun _ -> make_random_point width (height /. 2.))
  in
  let points2 = List.init 20 (fun _ -> make_random_point width height) in
  let points = points @ points2 in
  let idx = Rp.load ~max_node_load:16 points in
  let tree = Rp.tree idx in
  let box = Gg.Box2.v Gg.V2.(v 0. 0.) Gg.V2.(v width height) in
  let oc = open_out "./rtree-points.svg" in
  let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
  let i = render_tree ~size ~level:0 I.void r tree in
  ( Vgr.render r (`Image (size, box, i)) |> fun v ->
    ok v;
    Vgr.render r `End |> ok );
  close_out oc
