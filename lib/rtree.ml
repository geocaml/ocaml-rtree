type envelope = float * float * float * float

type 'a bounded = envelope * 'a

type 'a t
  = Node of envelope * 'a t list
  | Leaf of envelope * (envelope * 'a) list
  | Empty

let envelope_of_node = function
  | Node (e, _) | Leaf (e, _) -> e
  | Empty -> raise (Invalid_argument "empty nodes have no envelopes")

let unpack_node n = envelope_of_node n, n
let repack_node (_, n) = n

(* XXX!  apply science! *)
let max_node_load = 8

let ranges_intersect a b a' b' = a' <= b && a <= b'

let envelope_intersects (x0, x1, y0, y1) (x0', x1', y0', y1') =
  (* For two envelopes to intersect, both of their ranges do. *)
  ranges_intersect x0 x1 x0' x1' && ranges_intersect y0 y1 y0' y1'

let envelope_add (x0, x1, y0, y1) (x0', x1', y0', y1') =
  min x0 x0', max x1 x1', min y0 y0', max y1 y1'

let rec envelopes_add = function
  | e :: [] -> e
  | e :: es -> envelope_add e (envelopes_add es)
  | [] -> raise (Invalid_argument "can't zero envelopes")

let envelope_area (x0, x1, y0, y1) =
  (x1 -. x0) *. (y1 -. y0)

let rec envelope_of_elems = function
  | (e, _) :: [] -> e
  | (e, _) :: elems -> envelope_add e (envelope_of_elems elems)
  | [] -> raise (Invalid_argument "no envelope for empty elements")

let rec envelope_of_nodes = function
  | n :: [] -> envelope_of_node n
  | n :: ns -> envelope_add (envelope_of_node n) (envelope_of_nodes ns)
  | [] -> raise (Invalid_argument "got empty node list")

let enlargement_needed e e' =
  envelope_area (envelope_add e e') -. envelope_area e

let rec partition_node_by_min_enlargement e = function
  | (Node (e', _) | Leaf (e', _)) as n :: [] -> n, [], enlargement_needed e e'
  | (Node (e', _) | Leaf (e', _)) as n :: ns ->
      let enlargement = enlargement_needed e e' in
      let (min, maxs, enlargement') = partition_node_by_min_enlargement e ns in
      if enlargement < enlargement' then
        n, min :: maxs, enlargement
      else
        min, n :: maxs, enlargement'
  | _ -> raise (Invalid_argument "invalid node for partitioning")

let pairs_of_list xs =  (* (cross product) *)
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) xs) xs)

(* This is Guttman's quadradic splitting algorithm. *)
let split_pick_seeds ns =
  let pairs = pairs_of_list ns in
  let cost (e0, _) (e1, _) =
    (envelope_area (envelope_add e0 e1)) -.
    (envelope_area e0) -. (envelope_area e1) in
  let rec max_cost = function
    | (n, n') :: [] -> cost n n', (n, n')
    | (n, n') as pair :: ns ->
        let max_cost', pair' = max_cost ns in
        let cost = cost n n' in
        if cost > max_cost' then
          cost, pair
        else
          max_cost', pair'
    | [] -> raise (Invalid_argument "can't compute split on empty list") in
  let (_, groups) = max_cost pairs in groups

let split_pick_next e0 e1 ns =
  let diff (e, _) =
    abs_float ((enlargement_needed e0 e) -. (enlargement_needed e1 e)) in
  let rec max_difference = function
    | n :: [] -> diff n, n
    | n :: ns ->
        let diff', n' = max_difference ns in
        let diff = diff n in
        if diff > diff' then
          diff, n
        else
          diff', n'
    | [] -> raise (Invalid_argument "can't compute max diff on empty list") in
  let (_, n) = max_difference ns in n

let split_nodes ns =
  let rec partition xs xs_envelope ys ys_envelope = function
    | [] -> xs, ys
    | rest -> begin
        let (e, _) as n = split_pick_next xs_envelope ys_envelope rest in
        let rest' = List.filter ((!=) n) rest in
        let enlargement_x = enlargement_needed e xs_envelope in
        let enlargement_y = enlargement_needed e ys_envelope in
        if enlargement_x < enlargement_y then
          partition (n :: xs) (envelope_add xs_envelope e) ys ys_envelope rest'
        else
          partition xs xs_envelope (n :: ys) (envelope_add ys_envelope e) rest'
      end in
  let (((e0, _) as n0), ((e1, _) as n1)) = split_pick_seeds ns in
  partition [n0] e0 [n1] e1 (List.filter (fun n -> n != n0 && n != n1) ns)

let rec insert' elem e = function
  | Node (e', ns) -> begin
      let min, maxs, _ = partition_node_by_min_enlargement e ns in
      match insert' elem e min with
        | min', Empty ->
            Node (envelope_add (envelope_of_node min') e', min' :: maxs), Empty
        | min', min'' when (List.length maxs + 2) < max_node_load ->
            let e'' =
              envelopes_add [e; envelope_of_node min'; envelope_of_node min'']
            in Node (e'', min' :: min'' :: maxs), Empty
        | min', min'' ->
            let nodes = min' :: min'' :: maxs in
            let unpacked = List.map unpack_node nodes in
            (* Yuck. Fix. *)
            let a, b = split_nodes unpacked in
            let a' = List.map repack_node a in
            let b' = List.map repack_node b in
            Node (envelope_of_nodes a', a'), Node (envelope_of_nodes b', b')
    end
  | Leaf (e', elems) ->
      let elems' = (e, elem) :: elems in
      if List.length elems >= max_node_load then
        let a, b = split_nodes elems' in
        Leaf (envelope_of_elems a, a), Leaf (envelope_of_elems b, b)
      else
        Leaf (envelope_add e' e , elems'), Empty
  | Empty ->
      Leaf (e, [e, elem]), Empty

let insert t elem envelope =
  match insert' elem envelope t with
    | a, Empty -> a
    | a, b ->  (* Root split. *)
        let nodes = [a; b] in
        Node (envelope_of_nodes nodes, nodes)

let empty = Empty

let rec find t e =
  match t with
    | Node (_, ns) ->
        let intersecting =
          List.filter
            (fun n -> envelope_intersects (envelope_of_node n) e) ns in
        List.concat (List.map (fun n -> find n e) intersecting)
    | Leaf (e', elems) when envelope_intersects e e' ->
        let intersecting =
          List.filter (fun (e'', _) -> envelope_intersects e e'') elems in
        List.map (fun (_, elem) -> elem) intersecting
    | _ -> []

let rec size = function
  | Empty -> 0
  | Leaf (_, elems) ->
      List.length elems
  | Node (_, ns) ->
      let sizes = List.map size ns in
      List.fold_left (+) 0 sizes

(* module type BoundableType = *)
(*   sig *)
(*     type t *)
(*     val to_envelope : t -> float * float * float * float *)
(*   end *)

(* module type R = *)
(*   sig *)
(*     type elem *)
(*     type t *)
(*     val empty : t *)
(*     val add : t -> elem -> t *)
(*     val find : t -> (float * float * float * float) -> elem list *)
(*   end *)

(* module Make(B: BoundableType): (R with type elem = B.t) = *)
(*   struct *)
(*   end *)
