type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
    let sz = size / 2 in
    Graphics.moveto (x - sz) (y - sz);
    Graphics.lineto (x + sz) (y - sz);
    Graphics.lineto (x + sz) (y + sz);
    Graphics.lineto (x - sz) (y + sz);
    Graphics.lineto (x - sz) (y - sz)

let draw_tree_node node =
    let size = 50 in
    let draw_aux x y v =
        Graphics.moveto (x - size / 2 + 10) y;
        Graphics.draw_string v;
        draw_square x y size
    in
    match node with
    | Node (v, Nil, Nil) ->
            let sz = size / 2 in
            draw_aux 50 100 v;
            draw_aux 150 50 "Nil";
            draw_aux 150 150 "Nil";
            Graphics.moveto (50 + sz) 100;
            Graphics.lineto (150 - sz) 50;
            Graphics.moveto (50 + sz) 100;
            Graphics.lineto (150 - sz) 150
    | _ -> ()

let () =
    try
    Graphics.open_graph "";
    draw_tree_node (Node ("toto", Nil, Nil));
    while true do
        ()
    done;
    Graphics.clear_graph ()
    with _ -> ()
