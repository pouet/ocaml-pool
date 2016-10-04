let repeat_string ?(str="x") n =
    let rec aux n acc =
        if n < 0 then "Error"
        else if n == 0 then acc
        else aux (n - 1) (acc ^ str)
    in
    aux n ""


let () =
    print_endline (repeat_string (-1));
    print_endline (repeat_string 0);
    print_endline (repeat_string ~str:"Toto" 1);
    print_endline (repeat_string 2);
    print_endline (repeat_string ~str:"a" 5);
    print_endline (repeat_string ~str:"what" 3)
