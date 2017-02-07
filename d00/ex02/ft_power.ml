let ft_power x y =
    let rec aux y acc =
        if y <= 0 then acc
        else aux (y - 1) (acc * x)
    in
    aux y 1

let test x y =
    print_int x;
    print_string " ^ ";
    print_int y;
    print_string ": ";
    print_int (ft_power x y);
    print_endline ""

let () =
    test 2 4;
    test 3 0;
    test 0 5;
    test 4 6;
