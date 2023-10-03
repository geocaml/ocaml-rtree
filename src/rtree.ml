open! Import
include Rtree_intf

module Make (E : Envelope) (V : Value with type envelope = E.t) = struct
  module Envelope = E
  module Value = V

  type tree = Node of (E.t * tree) list | Leaf of (E.t * V.t) list | Empty

  let tree_t =
    let open Repr in
    mu (fun tree ->
        variant "tree_t" (fun node leaf empty -> function
          | Node lst -> node lst | Leaf s -> leaf s | Empty -> empty)
        |~ case1 "Node" (list (pair E.t tree)) (fun x -> Node x)
        |~ case1 "Leaf" (list (pair E.t V.t)) (fun x -> Leaf x)
        |~ case0 "Empty" Empty |> sealv)

  type t = { max_node_load : int; tree : tree }

  let t =
    let open Repr in
    record "t" (fun max_node_load tree -> { max_node_load; tree })
    |+ field "max_node_load" int (fun t -> t.max_node_load)
    |+ field "tree" tree_t (fun t -> t.tree)
    |> sealr

  let tree t = t.tree

  let empty max_node_load =
    if max_node_load < 2 then invalid_arg "Max node load must be greater than 1";
    { max_node_load; tree = Empty }

  let empty_node = (E.empty, Empty)
  let enlargement_needed e e' = E.area (E.merge e e') -. E.area e

  let rec partition_by_min_enlargement e = function
    | ((e', _) as n) :: [] -> (n, [], enlargement_needed e e')
    | ((e', _) as n) :: ns ->
        let enlargement = enlargement_needed e e' in
        let min, maxs, enlargement' = partition_by_min_enlargement e ns in
        if Float.compare enlargement enlargement' < 0 then
          (n, min :: maxs, enlargement)
        else (min, n :: maxs, enlargement')
    | [] -> raise (Invalid_argument "cannot partition an empty node")

  let pairs_of_list xs =
    (* (cross product) *)
    List.concat (List.map (fun x -> List.map (fun y -> (x, y)) xs) xs)

  (* This is Guttman's quadradic splitting algorithm. *)
  let split_pick_seeds ns =
    let pairs = pairs_of_list ns in
    let cost (e0, _) (e1, _) =
      E.area (E.merge e0 e1) -. E.area e0 -. E.area e1
    in
    let rec max_cost = function
      | (n, n') :: [] -> (cost n n', (n, n'))
      | ((n, n') as pair) :: ns ->
          let max_cost', pair' = max_cost ns in
          let cost = cost n n' in
          if Float.compare cost max_cost' > 0 then (cost, pair)
          else (max_cost', pair')
      | [] -> raise (Invalid_argument "can't compute split on empty list")
    in
    let _, groups = max_cost pairs in
    groups

  let split_pick_next e0 e1 ns =
    let diff (e, _) =
      abs_float (enlargement_needed e0 e -. enlargement_needed e1 e)
    in
    let rec max_difference = function
      | n :: [] -> (diff n, n)
      | n :: ns ->
          let diff', n' = max_difference ns in
          let diff = diff n in
          if Float.compare diff diff' > 0 then (diff, n) else (diff', n')
      | [] -> raise (Invalid_argument "can't compute max diff on empty list")
    in
    let _, n = max_difference ns in
    n

  let split_nodes ns =
    let rec partition xs xs_envelope ys ys_envelope = function
      | [] -> ((xs, xs_envelope), (ys, ys_envelope))
      | rest ->
          let ((e, _) as n) = split_pick_next xs_envelope ys_envelope rest in
          let rest' = List.filter (( != ) n) rest in
          let enlargement_x = enlargement_needed e xs_envelope in
          let enlargement_y = enlargement_needed e ys_envelope in
          if Float.compare enlargement_x enlargement_y < 0 then
            partition (n :: xs) (E.merge xs_envelope e) ys ys_envelope rest'
          else partition xs xs_envelope (n :: ys) (E.merge ys_envelope e) rest'
    in
    let ((e0, _) as n0), ((e1, _) as n1) = split_pick_seeds ns in
    partition [ n0 ] e0 [ n1 ] e1 (List.filter (fun n -> n != n0 && n != n1) ns)

  let envelope_of_nodes ns = E.merge_many (List.map (fun (e, _) -> e) ns)

  let rec insert' max_node_load elem e = function
    | Node ns -> (
        let (_, min), maxs, _ = partition_by_min_enlargement e ns in
        match insert' max_node_load elem e min with
        | min', (_, Empty) ->
            let ns' = min' :: maxs in
            let e' = envelope_of_nodes ns' in
            ((e', Node ns'), empty_node)
        | min', min'' when List.length maxs + 2 < max_node_load ->
            let ns' = min' :: min'' :: maxs in
            let e' = envelope_of_nodes ns' in
            ((e', Node ns'), empty_node)
        | min', min'' ->
            let (a, envelope_a), (b, envelope_b) =
              split_nodes (min' :: min'' :: maxs)
            in
            ((envelope_a, Node a), (envelope_b, Node b)))
    | Leaf es ->
        let es' = (e, elem) :: es in
        if List.length es' > max_node_load then
          let (a, envelope_a), (b, envelope_b) = split_nodes es' in
          ((envelope_a, Leaf a), (envelope_b, Leaf b))
        else ((envelope_of_nodes es', Leaf es'), empty_node)
    | Empty -> ((e, Leaf [ (e, elem) ]), empty_node)

  let insert t elem =
    match insert' t.max_node_load elem (V.envelope elem) t.tree with
    | (_, tree), (_, Empty) -> { max_node_load = t.max_node_load; tree }
    | a, b -> { max_node_load = t.max_node_load; tree = Node [ a; b ] }
  (* root split *)

  let filter_intersecting e = List.filter (fun (e', _) -> E.intersects e e')

  let rec find' t e =
    match t with
    | Node ns ->
        let intersecting = filter_intersecting e ns in
        let found = List.map (fun (_, n) -> find' n e) intersecting in
        List.concat found
    | Leaf es -> List.map snd (filter_intersecting e es)
    | Empty -> []

  let find t e = find' t.tree e

  let rec size' = function
    | Node ns ->
        let sub_sizes = List.map (fun (_, n) -> size' n) ns in
        List.fold_left ( + ) 0 sub_sizes
    | Leaf es -> List.length es
    | Empty -> 0

  let size t = size' t.tree

  let rec values' acc = function
    | Node lst -> List.fold_left (fun a (_, v) -> values' a v) acc lst
    | Leaf vs -> List.map snd vs
    | Empty -> []

  let values t = values' [] t.tree
  let log_base b n = log n /. log b

  let sort_by_dim entries i =
    List.stable_sort
      (fun v1 v2 -> E.compare_dim i (V.envelope v1) (V.envelope v2))
      entries

  let rec fold_lefti f i accu l =
    match l with [] -> accu | a :: l -> fold_lefti f (i + 1) (f i accu a) l

  let parition_by_n lst n =
    let m = List.length lst / n in
    let partitions = List.init n (fun _ -> []) in
    let add_partition ps idx v =
      List.mapi (fun i p -> if i = min idx (n - 1) then v :: p else p) ps
    in
    fold_lefti
      (fun i partitions v -> add_partition partitions (i / m) v)
      0 partitions lst
    |> List.map List.rev
    |> List.filter_map (fun v -> if v = [] then None else Some v)

  (* A lot of this code is inspired by https://github.com/georust/rstar/blob/master/rstar/src/algorithm/bulk_load/bulk_load_sequential.rs *)
  let number_along_axis ~m n =
    let m = float_of_int m in
    let n = float_of_int n in
    let depth = log_base m n in
    let n_subtree = Float.pow m (depth -. 1.) in
    let n_cluster = Float.ceil @@ (n /. n_subtree) in
    Float.pow n_cluster (1. /. float_of_int E.dimensions)
    |> Float.ceil |> int_of_float

  let rec omt ~m entries =
    let length = List.length entries in
    if length <= m then
      let leaves = List.map (fun v -> (V.envelope v, v)) entries in
      Leaf leaves
    else
      let slices = number_along_axis ~m length in
      let q =
        let q' = Queue.create () in
        Queue.add (entries, E.dimensions - 1) q';
        q'
      in
      let n = partitioning_task ~slices ~m q in
      Node n

  and partitioning_task ~slices ~m q =
    let rec loop acc =
      match Queue.take_opt q with
      | None -> List.rev acc
      | Some (elements, 0) ->
          let env = List.map V.envelope elements |> E.merge_many in
          loop ((env, omt ~m elements) :: acc)
      | Some (elements, n) ->
          let partitions = parition_by_n elements slices in
          let partitions = List.map (fun v -> sort_by_dim v n) partitions in
          List.iter (fun slab -> Queue.add (slab, n - 1) q) partitions;
          loop acc
    in
    loop []

  let load ?(max_node_load = 8) entries =
    let tree = omt ~m:max_node_load entries in
    { max_node_load; tree }

  let rec depth' = function
  | Node ns ->
      let sub_depths = List.map (fun (_, n) -> depth' n) ns in
      List.fold_left max 0 sub_depths    
  | Leaf _ -> 1
  | Empty -> 0

  let depth t = depth' t.tree

end

module Rectangle = Rectangle
