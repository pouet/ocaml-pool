let repeat_x x =
    let rec aux x acc =
        if x < 0 then "Error"
        else if x = 0 then acc
        else aux (x - 1) (acc ^ "x")
    in
    aux x ""


let () =
    print_endline (repeat_x (-1));
    print_endline (repeat_x 0);
    print_endline (repeat_x 1);
    print_endline (repeat_x 2);
    print_endline (repeat_x 5)
