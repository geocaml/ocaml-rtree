module type BoundableType =
  sig
    type t
    val to_envelope : t -> float * float * float * float
  end

module type R =
  sig
    type elem
    type t
    val empty : t
    val add : t -> elem -> t
    val find : t -> (float * float * float * float) -> elem option
  end

module Make(B: BoundableType): (R with type elem = B.t) =
  struct
    type envelope = float * float * float * float
    type elem = B.t
    type t
      = Node of envelope * (t list)
      | Leaf of envelope * (elem list)
      | Empty

    let envelope_of_node = function
      | Node (e, _) | Leaf (e, _) -> e
      | Empty -> raise (Invalid_argument "Empty nodes have no envelopes")

    (* XXX *)
    let max_node_load = 8

    let envelope_within (x0, x1, y0, y1) (x0', x1', y0', y1') =
      x0 <= x0' && x1 >= x1' && y0 <= y0' && y1 >= y1'

    let ranges_intersect a b a' b' =
      

    let envelope_intersects (x0, x1, y0, y1) (x0', x1', y0', y1') =


      (* 
       * For there to be an intersection, both ranges need to
       * intersect.
       *)
      (* x0 -> x1  *)
      (* x0' -> x1' *)
      



    
    let envelope_add (x0, x1, y0, y1) (x0', x1', y0', y1') =
      min x0 x0', max x1 x1', min y0 y0', max y1 y1'

    let envelope_area (x0, x1, y0, y1) =
      (x1 -. x0) *. (y1 -. y0)

    let rec envelope_of_items = function
      | x :: [] -> B.to_envelope x
      | x :: xs -> envelope_add (B.to_envelope x) (envelope_of_items xs)
      | [] -> raise (Invalid_argument "got empty item list")

    let rec envelope_of_nodes = function
      | n :: [] -> envelope_of_node n
      | n :: ns -> envelope_add (envelope_of_node n) (envelope_of_nodes ns)
      | [] -> raise (Invalid_argument "got empty item list")

    let enlargement_needed e e' =
      envelope_area (envelope_add e e') -. envelope_area e

    (** Stratify nodes by area enlargement. *)
    let rec stratify_nodes e = function
      | (Node (e', _)) as n :: [] -> n, [], enlargement_needed e e'
      | (Node (e', _)) as n :: ns ->
          let enlargement = enlargement_needed e e' in
          let (min, maxs, enlargement') = stratify_nodes e ns in
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

    let rec insert' item e = function
      | Node (e', ns) -> begin
          let min, maxs, _ = stratify_nodes e ns in
          match insert' item e min with
            | min', Empty -> Node (envelope_add e e', min' :: maxs), Empty
            | min', min'' when (List.length maxs + 2) < max_node_load ->
                let envelope' =
                  envelope_add
                    (envelope_add e (envelope_of_node min'))
                    (envelope_of_node min'') in
                Node (envelope', min' :: min' :: maxs), Empty
            | min', min'' ->
                let a, b = split_nodes (min' :: min' :: maxs) in
                Node (envelope_of_nodes a, a), Node (envelope_of_nodes b, b)
        end
      | Leaf (e', items) ->
          let items' = item :: items in
          if List.length items >= max_node_load then
            let a, b = split_nodes items' in
            Leaf (envelope_of_items a, a), Leaf (envelope_of_items b, b)
          else
            Leaf (envelope_add e' e , items'), Empty
      | Empty ->
          Leaf (e, [item]), Empty

    let insert item e t =
      match insert' item e t with
        | a, Empty -> a
        | a, b ->
            let nodes = [a; b] in
            Node (envelope_of_nodes nodes, nodes)

    let empty = Empty
    let add t item =
      let e = B.to_envelope item in
      insert item e t

    let rec find t e =
      match t with
        | Node (e, ns) when 
        | Leaf (_, t::_) -> Some t
        | _ -> None
  end
