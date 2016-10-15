type 'a tree =
    | Nil
    | Node of 'a * 'a tree * 'a tree

let rec is_bst = function
    | Nil -> false
    | Node (_, Nil, Nil) -> true
    | Node (n, (Node (x, _, _) as l), Nil) -> x < n && is_bst l
    | Node (n, Nil, (Node (y, _, _) as r)) -> y > n && is_bst r
    | Node (n, (Node (x, _, _) as l), (Node (y, _, _) as r)) ->
            x < n && n < y && is_bst l && is_bst r

let is_perfect t =
    let rec aux h = function
        | Nil -> (-1)
        | Node (_, Nil, Nil) -> h
        | Node (_, Node (_, _, _), Nil) | Node (_, Nil, Node (_, _, _)) -> (-1)
        | Node (_, l, r) ->
                let a = aux (h + 1) l
                and b = aux (h + 1) r in
                if a <> b then -1
                else a
    in
    is_bst t && aux 0 t >= 0

let is_balanced t =
    let rec aux h = function
        | Nil -> (-1)
        | Node (_, Nil, Nil) -> h
        | Node (_, (Node (_, _, _) as l), Nil) -> aux (h + 1) l
        | Node (_, Nil, (Node (_, _, _) as r)) -> aux (h + 1) r
        | Node (_, l, r) ->
                let a = aux (h + 1) l
                and b = aux (h + 1) r in
                if a <> b then -1
                else a
    in
    is_bst t && aux 0 t >= 0

let rec search_bst v = function
    | Nil -> v = Nil
    | Node (_, l, r) as n -> n = v || (search_bst v l) || (search_bst v r)

let rec add_bst nv = function
        | Nil -> Node (nv, Nil, Nil)
        | Node (v, _, _) when v = nv -> failwith "BST can't have same value"
        | Node (v, l, r) ->
                if nv < v then Node (v, (add_bst nv l), r)
                else Node (v, l, (add_bst nv r))

let rec get_val = function
    | Node (v, _, Nil) -> v
    | Node (v, _, r) -> get_val r
    | _ -> failwith "Should never happen"

let rec delete_bst nv = function
    | Nil -> Nil 
(* Node seul, pas de fils *)
    | Node (v, Nil, Nil) when v = nv -> Nil
(* Node avec un seul fils : gauche *)
    | Node (v, (Node (_, _, _) as l), Nil) when v = nv -> l
(* Node avec un seul fils : droit *)
    | Node (v, Nil, (Node (_, _, _) as r)) when v = nv -> r
(* Node avec deux fils *)
    | Node (v, (Node (lv, _, _) as l), (Node (rv, _, _) as r)) when v = nv ->
            let tmp = get_val l in
            Node (tmp, (delete_bst tmp l), r)
(* On parcoure les branches *)
    | Node (v, l, r) ->
            if nv < v then Node (v, (delete_bst nv l), r)
            else Node (v, l, (delete_bst nv r))



let () =
    let t = Node (1, Node(1, Nil, Node(2, Nil, Nil)), Nil) in
    let t = Node (1, Node(0, Nil, Nil), Node(2, Nil, Nil)) in
    let t = Node (1, Node(0, Nil, Nil), Nil) in
    let t = Node (1, Node(0, Node (-1, Nil, Nil), Node (1, Nil, Nil)), Node(2, Nil, Nil)) in
    print_endline (if is_bst t = true then "true" else "false");
    print_endline (if is_perfect t = true then "true" else "false");
    print_endline (if is_balanced t = true then "true" else "false");
    print_endline (if search_bst (Node (2, Nil, Nil)) t = true then "true" else
        "false");
    let a = add_bst 4 t in
    let b = delete_bst 4 a in
    print_endline (if is_bst b = true then "true" else "false")
