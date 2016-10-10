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

(* TODO: a revoir... *)
let rec add_bst add = function
        | Nil -> Nil
        | Node (v, Nil, Nil) ->
                let Node (nv, _, _) = add in
                if v = nv then failwith "BST can't have same value"
                else if v < nv then Node (v, add, Nil)
                else Node (v, Nil, add)
        | Node (v, l, r) ->
                let Node (nv, _, _) = add in
                if v = nv then failwith "BST can't have same value"
                else if v < nv then Node (v, (add_bst add l), r)
                else Node (v, l, (add_bst add r))

let () =
    let t = Node (1, Node(1, Nil, Node(2, Nil, Nil)), Nil) in
    let t = Node (1, Node(0, Nil, Nil), Node(2, Nil, Nil)) in
    let t = Node (1, Node(0, Nil, Nil), Nil) in
    let t = Node (1, Node(0, Node (-1, Nil, Nil), Node (1, Nil, Nil)), Node(2, Nil, Nil)) in
    print_endline (if is_bst t = true then "true" else "false");
    print_endline (if is_perfect t = true then "true" else "false");
    print_endline (if is_balanced t = true then "true" else "false");
    print_endline (if search_bst (Node (2, Nil, Nil)) t = true then "true" else "false")
