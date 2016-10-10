type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size = function
    | Nil -> 0
    | Node (_, l, r) -> 1 + size l + size r

let rec height = function
    | Nil -> 0
    | Node (_, l, r) ->
            let a = 1 + size l
            and b = 1 + size r
            in
            if a > b then a
            else b

let draw_tree t =
    let sz = 25 in
    let draw_square x y s =
        Graphics.moveto x y;
        Graphics.draw_string s;
        Graphics.moveto (x - sz) (y - sz);
        Graphics.lineto (x + sz) (y - sz);
        Graphics.lineto (x + sz) (y + sz);
        Graphics.lineto (x - sz) (y + sz);
        Graphics.lineto (x - sz) (y - sz)
    and draw_connection x y =
        Graphics.moveto (x + sz) y;
        Graphics.lineto (x + sz * 2) (y - 2 * sz);
        Graphics.moveto (x + sz) y;
        Graphics.lineto (x + sz * 2) (y + 2 * sz)
    in
    let rec draw x y = function
        | Nil -> draw_square x y "Nil"
        | Node (s, l, r) ->
                draw_square x y s;
                draw_connection x y;
                draw (x + sz * 3) (y - 2 * sz) l;
                draw (x + sz * 3) (y + 2 * sz) r
    in
    Graphics.open_graph "";
    draw 50 250 t

let () =
    let t = Node (1, Node(1, Nil, Node(2, Nil, Nil)), Nil) in
    let u = Node ("1", Node("1", Nil, Node("2", Nil, Nil)), Nil) in
    Printf.printf "%d\n" (size t);
    Printf.printf "%d\n" (height t);
    draw_tree u;
    while true do
        ()
    done
