(* XXX todo -- make non-functor version of this that always takes the *)
(* envelope directly, then a functor version that's just implemented on *)
(* top of this. *)

type envelope = float * float * float * float

type 'a t
  = Node of envelope * 'a t list
  | Leaf of envelope * (envelope * 'a) list
  | Empty

let envelope_of_node = function
  | Node (e, _) | Leaf (e, _) -> e
  | Empty -> raise (Invalid_argument "empty nodes have no envelopes")

(* XXX! *)
let max_node_load = 8

let envelope_within (x0, x1, y0, y1) (x0', x1', y0', y1') =
  x0 <= x0' && x1 >= x1' && y0 <= y0' && y1 >= y1'

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

(** Stratify nodes by area enlargement. *)
let rec stratify_nodes_by_enlargement e = function
  | (Node (e', _)) as n :: [] -> n, [], enlargement_needed e e'
  | (Node (e', _)) as n :: ns ->
      let enlargement = enlargement_needed e e' in
      let (min, maxs, enlargement') = stratify_nodes_by_enlargement e ns in
      if enlargement < enlargement' then
        n, min :: maxs, enlargement
      else
        min, n :: maxs, enlargement'
  | _ -> raise (Invalid_argument "can't find enlargement for non-nodes")

(* now do the same, but allow for splits. node splitting can be
 * arbitrary. *)

let split_nodes ns =
  (* My super awesome node splitting algorithm. *)
  List.partition (fun _ -> Random.bool ()) ns

let rec insert' elem e = function
  | Node (e', ns) -> begin
      let min, maxs, _ = stratify_nodes_by_enlargement e ns in
      match insert' elem e min with
        | min', Empty -> Node (envelope_add e e', min' :: maxs), Empty
        | min', min'' when (List.length maxs + 2) < max_node_load ->
            let e'' = envelopes_add [e;
                                     envelope_of_node min';
                                     envelope_of_node min''] in
            Node (e'', min' :: min' :: maxs), Empty
        | min', min'' ->
            let a, b = split_nodes (min' :: min' :: maxs) in
            Node (envelope_of_nodes a, a), Node (envelope_of_nodes b, b)
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
    | Node (e, ns) ->
        let intersecting =
          List.filter (fun n -> envelope_intersects (envelope_of_node n) e) ns in
        List.concat (List.map (fun n -> find n e) intersecting)
    | Leaf (e', elems) when envelope_intersects e e' ->
        List.filter (fun (e', _) -> envelope_intersects e e') elems
    | _ -> []

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
